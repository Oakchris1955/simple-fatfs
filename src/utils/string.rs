#[cfg(not(feature = "std"))]
use alloc::string::String;

use alloc::string::FromUtf16Error;

use crate::{path::*, Clock, Codepage, FSResult, FileSystem, Sfn, SFN_EXT_LEN, SFN_NAME_LEN};

use crate::fat::BlockWrite;

/// variation of <https://stackoverflow.com/a/42067321/19247098> for processing LFNs
pub(crate) fn string_from_lfn(utf16_src: &[u16]) -> Result<String, FromUtf16Error> {
    let nul_range_end = utf16_src
        .iter()
        .position(|c| *c == 0x0000)
        .unwrap_or(utf16_src.len()); // default to length if no `\0` present

    String::from_utf16(&utf16_src[..nul_range_end])
}

pub(crate) fn as_sfn(string: &str, codepage: Codepage) -> Option<Sfn> {
    // a file can still not have an extension
    let (name, ext) = string.split_once('.').unwrap_or((string, ""));

    // create a sfn with padding
    let mut result = Sfn::default();

    copy_cp_chars(result.name_mut(), name, codepage)?;

    copy_cp_chars(result.ext_mut(), ext, codepage)?;

    Some(result)
}

/// Decodes as many characters as possible using `codepage` from `string` and puts them to `destination`
///
/// Returns [`None`] if not all characters could be decoded
pub(crate) fn copy_cp_chars(
    mut destination: &mut [u8],
    string: &str,
    codepage: Codepage,
) -> Option<()> {
    for c in string.chars() {
        let c = encode_valid_char_checked(c, codepage)?;
        if destination.is_empty() {
            // no space left
            return None;
        }
        destination[0] = c;
        destination = &mut destination[1..];
    }

    Some(())
}

const OTHER_PERMITTED_CHARS: &[u8] = b"$%-_@~`!(){}^#&";

fn encode_valid_char_checked(c: char, codepage: Codepage) -> Option<u8> {
    let c = codepage.encode_char_checked(c)?;

    (c.is_ascii_digit()
        || c.is_ascii_uppercase()
        || OTHER_PERMITTED_CHARS.contains(&c)
        || !c.is_ascii())
    .then_some(c)
}

#[derive(Debug)]
struct SfnGenerator {
    name: [u8; SFN_NAME_LEN],
    ext: [u8; SFN_EXT_LEN],
    position: usize,
}

impl SfnGenerator {
    fn new(string: &str, codepage: Codepage) -> Self {
        let (name, ext) = string.rsplit_once('.').unwrap_or((string, ""));

        let mut result = Self {
            name: [b' '; SFN_NAME_LEN],
            ext: [b' '; SFN_EXT_LEN],
            position: 0,
        };

        Self::_as_sfn_part(&mut result.name, name, codepage);
        Self::_as_sfn_part(&mut result.ext, ext, codepage);

        let len = result
            .name
            .iter()
            .position(|&c| c == b' ')
            .unwrap_or(name.len())
            .min(SFN_NAME_LEN - 2);
        result.name[len] = b'~';
        result.name[len + 1] = b'0';
        result.position = len + 1;

        result
    }

    fn _as_sfn_part(mut destination: &mut [u8], input: &str, codepage: Codepage) {
        for ch in input.chars() {
            if let Some(c) = encode_valid_char_checked(ch.to_ascii_uppercase(), codepage) {
                destination[0] = c;
                destination = &mut destination[1..];
                if destination.is_empty() {
                    break;
                }
            }
        }
    }
}

impl Iterator for SfnGenerator {
    type Item = Sfn;

    // TODO: check beforehands how many similar SFNs exist so that we can increment the index past that number
    fn next(&mut self) -> Option<Self::Item> {
        // increment by one
        let mut pos = self.position;
        loop {
            let c = self.name[pos];
            if c == b'~' {
                // by adding with overflow we reached the front
                if self.position < 7 {
                    // there are still unused spaces at the end, extend there
                    self.position += 1;

                    // move `NAME~9  ` to `NAME~10 `
                    pos += 1;
                    self.name[pos] = b'1';
                    pos += 1;
                    while pos <= self.position {
                        self.name[pos] = b'0';
                        pos += 1;
                    }

                    break;
                } else {
                    // the name needs to be shortened
                    if pos == 1 {
                        // the name has already only one letter -> abort
                        return None;
                    }
                    // move `NAME~000` to `NAM~1000`
                    self.name[pos] = b'1';
                    self.name[pos - 1] = b'~';
                    break;
                }
            } else if c == b'9' {
                // incrementing generates overflow
                self.name[pos] = b'0';
                pos -= 1;
            } else {
                // simply increment number
                self.name[pos] = c + 1;
                break;
            }
        }

        Some(Sfn::new(self.name, self.ext))
    }
}

pub(crate) fn gen_sfn<S, C, P>(
    string: &str,
    fs: &FileSystem<S, C>,
    target_dir: P,
) -> FSResult<Sfn, S::Error>
where
    S: BlockWrite,
    C: Clock,
    P: AsRef<Path>,
{
    // we first check if this string is a valid short filename
    'outer: {
        if let Some(sfn) = as_sfn(string, fs.options.codepage) {
            #[cfg(feature = "bloom")]
            if let Some(filter) = &fs.dir_info.borrow().filter {
                if !filter.check(&sfn.decode(fs.options.codepage)) {
                    return Ok(sfn);
                }
            }

            // don't forget to check if that SFN already exists
            for entry in fs.process_current_dir() {
                let entry = entry?;

                if entry.sfn == sfn {
                    break 'outer;
                }
            }

            return Ok(sfn);
        }
    }

    let generator = SfnGenerator::new(string, fs.options.codepage);

    // FIXME: this is bad, has best-case O(n) time complexity
    'outer: for sfn in generator {
        #[cfg(feature = "bloom")]
        if let Some(filter) = &fs.dir_info.borrow().filter {
            if !filter.check(&sfn.decode(fs.options.codepage)) {
                return Ok(sfn);
            }
        }

        for entry in fs.read_dir(&target_dir)? {
            let entry = entry?;

            if entry.sfn.0 == sfn {
                continue 'outer;
            }
        }

        return Ok(sfn);
    }

    unreachable!(concat!(
        "the FAT32 file limit per directory is 2^16 (~65 hundred) files, and this generator ",
        "can theoretically generate 10^9 - 1 (1 billion minus one) unique short filenames"
    ))
}

#[test]
fn test_sfn_generator_long() {
    let mut generator = SfnGenerator::new("HELLO-WORLD.TXT", Codepage::default());

    assert_eq!(generator.next(), Some(Sfn::new(*b"HELLO-~1", *b"TXT")));
    assert_eq!(generator.next(), Some(Sfn::new(*b"HELLO-~2", *b"TXT")));
    let mut generator = generator.skip(7);
    assert_eq!(generator.next(), Some(Sfn::new(*b"HELLO~10", *b"TXT")));
}

#[test]
fn test_sfn_generator_short() {
    let mut generator = SfnGenerator::new("run.jpeg", Codepage::default());

    assert_eq!(generator.next(), Some(Sfn::new(*b"RUN~1   ", *b"JPE")));
    assert_eq!(generator.next(), Some(Sfn::new(*b"RUN~2   ", *b"JPE")));
    let mut generator = generator.skip(7);
    assert_eq!(generator.next(), Some(Sfn::new(*b"RUN~10  ", *b"JPE")));
}

#[test]
fn test_sfn_generator_cp_chars_cp437() {
    let mut generator = SfnGenerator::new("tëst.txt", Codepage::CP437);

    assert_eq!(generator.next(), Some(Sfn::new(*b"T\x89ST~1  ", *b"TXT")));
}

#[test]
fn test_sfn_generator_unknown_chars() {
    let mut generator = SfnGenerator::new("😇.😈", Codepage::default());

    assert_eq!(generator.next(), Some(Sfn::new(*b"~1      ", *b"   ")));
}

#[cfg(all(test, feature = "std"))]
fn run_gen_sfn(string: &str) -> Option<Sfn> {
    use crate::{FSOptions, FromStd};
    use std::io::Cursor;

    const FAT16: &[u8] = include_bytes!("../../imgs/fat16.img");
    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    gen_sfn(string, &fs, "/").ok()
}

#[cfg(all(test, feature = "std"))]
#[test]
fn test_gen_sfn_match() {
    assert_eq!(
        run_gen_sfn("TEST.TXT"),
        Some(Sfn::new(*b"TEST    ", *b"TXT"))
    )
}

#[cfg(all(test, feature = "std"))]
#[test]
fn test_gen_sfn_mismatch() {
    assert_eq!(
        run_gen_sfn("test.txt"),
        Some(Sfn::new(*b"TEST~1  ", *b"TXT"))
    )
}
