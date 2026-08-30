use core::ops;

#[cfg(not(feature = "std"))]
use alloc::{borrow::ToOwned, string::String};

use zerocopy::{FromBytes, Immutable, IntoBytes};

use crate::Codepage;

pub(crate) const SFN_NAME_LEN: usize = 8;
pub(crate) const SFN_EXT_LEN: usize = 3;
pub(crate) const SFN_LEN: usize = SFN_NAME_LEN + SFN_EXT_LEN;

#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
/// The short filename of an entry
///
/// In FAT, each file has 2 filenames: one long and one short filename.
/// The short filename is retained for backwards-compatibility reasons
/// by the FAT specification and shouldn't concern most users.
pub(crate) struct Sfn([u8; SFN_LEN]);

pub(crate) const CURRENT_DIR_SFN: Sfn = Sfn(*b".          ");

pub(crate) const PARENT_DIR_SFN: Sfn = Sfn(*b"..         ");

impl ops::Deref for Sfn {
    type Target = [u8; SFN_LEN];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for Sfn {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for Sfn {
    fn default() -> Self {
        Sfn(*b"           ")
    }
}

impl Sfn {
    pub(crate) fn new(name: [u8; SFN_NAME_LEN], ext: [u8; SFN_EXT_LEN]) -> Self {
        let mut slice = [0_u8; SFN_LEN];

        slice[..SFN_NAME_LEN].copy_from_slice(&name);
        slice[SFN_NAME_LEN..].copy_from_slice(&ext);

        Sfn(slice)
    }

    pub(crate) fn new_from_slice(slice: [u8; SFN_LEN]) -> Self {
        Sfn(slice)
    }

    pub(crate) fn gen_checksum(&self) -> u8 {
        let mut sum = 0;

        for c in self.iter() {
            sum = (if (sum & 1) != 0 { 0x80_u8 } else { 0_u8 })
                .wrapping_add(sum >> 1)
                .wrapping_add(*c)
        }

        sum
    }

    pub(crate) fn name(&self) -> &[u8; SFN_NAME_LEN] {
        (&self[..SFN_NAME_LEN]).try_into().unwrap()
    }

    pub(crate) fn ext(&self) -> &[u8; SFN_EXT_LEN] {
        (&self[SFN_NAME_LEN..]).try_into().unwrap()
    }

    pub(crate) fn name_mut(&mut self) -> &mut [u8; SFN_NAME_LEN] {
        (&mut self[..SFN_NAME_LEN]).try_into().unwrap()
    }

    pub(crate) fn ext_mut(&mut self) -> &mut [u8; SFN_EXT_LEN] {
        (&mut self[SFN_NAME_LEN..]).try_into().unwrap()
    }

    pub(crate) fn decode(&self, codepage: Codepage) -> String {
        // one more byte for the "." between the name and the file extension
        let mut string = String::with_capacity(SFN_LEN + 1);
        // we begin by writing the name (even if it is padded with spaces, they will be trimmed, so we don't care)
        string.push_str(codepage.decode(self.name()).trim_end());

        // then, if the extension isn't empty (padded with zeroes), we write it too
        let ext = codepage.decode(self.ext()).trim_end().to_owned();
        if !ext.is_empty() {
            string.push_str(&ext);
        };

        string
    }
}
