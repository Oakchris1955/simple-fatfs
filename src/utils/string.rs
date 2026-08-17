#[cfg(not(feature = "std"))]
use alloc::string::String;

use alloc::string::FromUtf16Error;

use crate::block_io::prelude::*;
use crate::path::Path;
use crate::serde::sfn::{SFN_EXT_LEN, SFN_NAME_LEN, Sfn};
use crate::time::Clock;
use crate::{Codepage, FSResult, FileSystem};

/// Decode a native endian UTF-16–encoded vector `utf16_src` into a [`String`],
/// returning a [`FromUtf16Error`] [`Err`] if `utf16_src` contains any invalid data.
///
/// The main difference of this function from [`String::from_utf16`] is that this
/// will also handle any vectors that may not be null-terminated, since that is
/// the case with long names with a length which is a multiple of 13.
///
/// # Note
///
/// Code for finding the index of the first zero byte obtained from <https://stackoverflow.com/a/42067321/19247098>
/// Original from [oli_obk](https://stackoverflow.com/users/1103681/oli-obk),
/// checked (safe) version from [Shepmaster](https://stackoverflow.com/users/155423/shepmaster),
/// licensed under [CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/)
/// (can also be found at the crate's root, in `NOTICE.md`).
pub(crate) fn string_from_lfn(utf16_src: &[u16]) -> Result<String, FromUtf16Error> {
    let nul_range_end = utf16_src
        .iter()
        .position(|c| *c == 0x0000)
        .unwrap_or(utf16_src.len()); // default to length if no `\0` present

    String::from_utf16(&utf16_src[..nul_range_end])
}

/// Attempt to decode a provided `string` into a [`Sfn`], using the provided `codepage`.
///
/// # Errors
///
/// Returns [`None`] if decoding fails.
pub(crate) fn as_sfn(string: &str, codepage: Codepage) -> Option<Sfn> {
    // a file can still not have an extension
    let (name, ext) = string.split_once('.').unwrap_or((string, ""));

    // create a sfn with padding
    let mut result = Sfn::default();

    copy_cp_chars(result.name_mut(), name, codepage, true)?;

    copy_cp_chars(result.ext_mut(), ext, codepage, true)?;

    Some(result)
}

/// Decodes as many characters as possible using `codepage` from `string` and puts them to `destination`
///
/// # Errors
///
/// Returns [`None`] if not all characters could be decoded
pub(crate) fn copy_cp_chars(
    mut destination: &mut [u8],
    string: &str,
    codepage: Codepage,
    fail_fast: bool,
) -> Option<()> {
    for c in string.chars() {
        let c = match encode_valid_char_checked(c, codepage) {
            Some(c) => c,
            None => {
                if fail_fast {
                    return None;
                } else {
                    continue;
                }
            }
        };

        if destination.is_empty() {
            // no space left
            return None;
        }
        destination[0] = c;
        destination = &mut destination[1..];
    }

    Some(())
}

/// Other non-letter or numerical ASCII characters that are permitted to reside
/// in a short filename
const OTHER_PERMITTED_CHARS: &[u8] = b"$%'-_@~`!(){}^#&";

/// Decode the given character using `codepage`
///
/// # Note
///
/// The FAT specification says that "the characters comprising a short file name may be any combination
/// of letters, digits, or characters with code point values greater than 127".
/// It is also specified that lowercase characters should be converted to uppercase.
///
/// # Errors
///
/// [`None`] will be returned if the decoded character is not permitted
/// in a short name
fn encode_valid_char_checked(c: char, codepage: Codepage) -> Option<u8> {
    let c = codepage.encode_char_checked(c)?;

    (c.is_ascii_alphanumeric() || OTHER_PERMITTED_CHARS.contains(&c) || !c.is_ascii())
        .then_some(c)
        .map(|c| c.to_ascii_uppercase())
}

#[derive(Debug)]
/// Generate matching [`Sfn`]s for a string
struct SfnGenerator {
    name: [u8; SFN_NAME_LEN],
    ext: [u8; SFN_EXT_LEN],
    position: usize,
}

impl SfnGenerator {
    /// Create a new [`SfnGenerator`] for the provided `string` & `codepage`
    fn new(string: &str, codepage: Codepage) -> Self {
        let (name, ext) = string.rsplit_once('.').unwrap_or((string, ""));

        let mut result = Self {
            name: [b' '; SFN_NAME_LEN],
            ext: [b' '; SFN_EXT_LEN],
            position: 0,
        };

        copy_cp_chars(&mut result.name, name, codepage, false);
        copy_cp_chars(&mut result.ext, ext, codepage, false);

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
}

impl Iterator for SfnGenerator {
    type Item = Sfn;

    // TODO: check beforehand how many similar SFNs exist so that we can increment the index past that number
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

/// Generate a [`Sfn`] for an entry with the provided `name` that will reside in
/// the `target_dir` of the provided `fs`.
///
/// # Errors
///
/// Returns an [`FSError`](crate::FSError) if any IO-related error occurs
///
pub(crate) fn gen_sfn<S, C, P>(
    name: &str,
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
        if let Some(sfn) = as_sfn(name, fs.options.codepage) {
            #[cfg(feature = "bloom")]
            if let Some(filter) = &fs.dir_info.borrow().filter
                && !filter.check(&sfn.decode(fs.options.codepage))
            {
                return Ok(sfn);
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

    let generator = SfnGenerator::new(name, fs.options.codepage);

    // FIXME: this is bad, has best-case O(n) time complexity
    'outer: for sfn in generator {
        #[cfg(feature = "bloom")]
        if let Some(filter) = &fs.dir_info.borrow().filter
            && !filter.check(&sfn.decode(fs.options.codepage))
        {
            return Ok(sfn);
        }

        for entry in fs.read_dir(&target_dir)? {
            let entry = entry?;

            if entry.sfn == sfn {
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

#[cfg(test)]
mod tests {
    use super::*;

    use rstest::*;
    use rstest_reuse::*;

    use test_log::test;

    use crate::test_commons::*;
    use crate::time::DefaultClock;

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

    #[test]
    fn test_sfn_generator_unknown_chars_failfast() {
        let mut generator = SfnGenerator::new("1😇2.T😈XT", Codepage::default());

        assert_eq!(generator.next(), Some(Sfn::new(*b"12~1    ", *b"TXT")));
    }

    fn run_gen_sfn_root<S, C>(string: &str, fs: &FileSystem<S, C>) -> Option<Sfn>
    where
        S: BlockWrite,
        C: Clock,
    {
        gen_sfn(string, fs, "/").ok()
    }

    #[test]
    #[apply(fs)]
    fn test_gen_sfn_match_uppercase(fs: FileSystem<MemoryDevice, DefaultClock>) {
        assert_eq!(
            run_gen_sfn_root("TEST.TXT", &fs),
            Some(Sfn::new(*b"TEST    ", *b"TXT"))
        )
    }

    #[test]
    #[apply(fs)]
    fn test_gen_sfn_match_lowercase(fs: FileSystem<MemoryDevice, DefaultClock>) {
        assert_eq!(
            run_gen_sfn_root("test.txt", &fs),
            Some(Sfn::new(*b"TEST    ", *b"TXT"))
        )
    }

    #[test]
    #[apply(fs)]
    fn test_gen_sfn_match_mixedcase(fs: FileSystem<MemoryDevice, DefaultClock>) {
        assert_eq!(
            run_gen_sfn_root("TesT.tXt", &fs),
            Some(Sfn::new(*b"TEST    ", *b"TXT"))
        )
    }
}
