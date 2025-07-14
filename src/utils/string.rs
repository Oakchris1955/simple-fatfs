#[cfg(not(feature = "std"))]
use alloc::{format, string::String};

use alloc::string::FromUtf16Error;

use crate::{io::prelude::*, path::*, FSResult, FileSystem, Sfn, SFN_EXT_LEN, SFN_NAME_LEN};

/// variation of <https://stackoverflow.com/a/42067321/19247098> for processing LFNs
pub(crate) fn string_from_lfn(utf16_src: &[u16]) -> Result<String, FromUtf16Error> {
    let nul_range_end = utf16_src
        .iter()
        .position(|c| *c == 0x0000)
        .unwrap_or(utf16_src.len()); // default to length if no `\0` present

    String::from_utf16(&utf16_src[..nul_range_end])
}

pub(crate) fn as_sfn(string: &str) -> Option<Sfn> {
    // anything non-ascii should be represented as a LFN
    if !string.is_ascii() {
        return None;
    }

    // since everything is in ascii, 1 byte = 1 char
    if string.len() > SFN_NAME_LEN + 1 + SFN_EXT_LEN {
        return None;
    }

    // a file can still not have an extension
    let (name, ext) = string.split_once('.').unwrap_or((string, ""));

    if name.len() > SFN_NAME_LEN || ext.len() > SFN_EXT_LEN {
        return None;
    }

    // don't forget the padding
    // here, 8 is SFN_NAME_LEN and 3 is SFN_EXT_LEN
    let (name, ext) = (format!("{name:<8}"), format!("{ext:<3}"));

    // this shouldn't panic, as we have checked that both the name
    // and the extension should fit into a SFN
    Some(Sfn {
        name: name.as_bytes().try_into().unwrap(),
        ext: ext.as_bytes().try_into().unwrap(),
    })
}

#[derive(Debug)]
struct SfnGenerator {
    name: String,
    // we won't be messing with this anyways, so we might as well keep it around as a byte array
    ext: [u8; 3],

    index: usize,
    next_pow_of_ten: usize,
}

impl SfnGenerator {
    fn new(string: &str) -> Self {
        let (name, ext) = Self::_sfn_name_ext_from_string(string);

        Self {
            name,
            ext: ext.as_bytes().try_into().unwrap(),

            index: 1,
            next_pow_of_ten: 10,
        }
    }

    fn _sfn_name_ext_from_string(string: &str) -> (String, String) {
        let ascii_string: String = string.chars().filter(|c| c.is_ascii()).collect();

        // a file can still not have an extension
        let (mut name, mut ext) = ascii_string
            .rsplit_once('.')
            .map(|(n, e)| (n.replace(".", ""), String::from(e)))
            .unwrap_or((ascii_string, String::new()));

        (name, ext) = (
            name.chars().take(SFN_NAME_LEN - 2).collect::<String>(),
            ext.chars().take(SFN_EXT_LEN).collect::<String>(),
        );
        (name, ext) = (name.to_ascii_uppercase(), ext.to_ascii_uppercase());

        // here, 6 is (SFN_NAME_LEN - 2) and 3 is SFN_EXT_LEN
        (format!("{name:<6}"), format!("{ext:<3}"))
    }

    /**
     Returns a [`bool`] indicating whether the operation was successfull (that is, the name wasn't already empty)
    */
    fn reduce_name_by_a_char(&mut self) -> bool {
        if self.name.is_empty() {
            return false;
        }

        self.name.truncate(self.name.len() - 1);

        true
    }
}

impl Iterator for SfnGenerator {
    type Item = Sfn;

    // TODO: check beforehands how many similar SFNs exist so that we can increment the index past that number
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.next_pow_of_ten {
            if !self.reduce_name_by_a_char() {
                return None;
            }

            self.next_pow_of_ten *= 10;
        }

        // this shouldn't panic, as we have checked that the name
        // should fit into a SFN
        let sfn = Sfn {
            name: format!("{}~{}", self.name, self.index)
                .as_bytes()
                .try_into()
                .unwrap(),
            ext: self.ext,
        };

        self.index += 1;

        Some(sfn)
    }
}

pub(crate) fn gen_sfn<S, P>(
    string: &str,
    fs: &mut FileSystem<S>,
    target_dir: P,
) -> FSResult<Sfn, S::Error>
where
    S: Read + Write + Seek,
    P: AsRef<Path>,
{
    // we first check if this string is a valid short filename
    if let Some(sfn) = as_sfn(string) {
        return Ok(sfn);
    }

    let generator = SfnGenerator::new(string);

    for sfn in generator {
        if !fs
            .read_dir(&target_dir)?
            .iter()
            .any(|entry| entry.sfn == sfn)
        {
            return Ok(sfn);
        }
    }

    unreachable!(concat!(
        "the FAT32 file limit per directory is 2^16 (~65 hundred) files, and this generator ",
        "can theoretically generate 10^9 - 1 (1 billion minus one) unique short filenames"
    ))
}
