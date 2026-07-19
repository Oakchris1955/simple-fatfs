use core::iter::FusedIterator;
use core::num;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
use zerocopy::{little_endian::U16, FromBytes, FromZeros, Immutable, IntoBytes};

use crate::{EntryCount, RawAttributes};

const LAST_LFN_ENTRY_MASK: u8 = 0x40;
const LFN_FIRST_CHARS: usize = 5;
const LFN_MID_CHARS: usize = 6;
const LFN_LAST_CHARS: usize = 2;
pub(crate) const LFN_CHAR_LIMIT: usize = 255; // not including the trailing null
pub(crate) const CHARS_PER_LFN_ENTRY: usize = LFN_FIRST_CHARS + LFN_MID_CHARS + LFN_LAST_CHARS;
pub(crate) const LFN_MAX_ENTRIES: usize = LFN_CHAR_LIMIT.div_ceil(CHARS_PER_LFN_ENTRY);

#[derive(Debug, Immutable, FromBytes, IntoBytes)]
#[repr(C)]
pub(crate) struct LFNEntry {
    /// masked with 0x40 if this is the last entry
    pub(crate) order: u8,
    pub(crate) first_chars: [U16; LFN_FIRST_CHARS],
    /// Always equals RawAttributes::LFN
    pub(crate) _lfn_attribute: RawAttributes,
    /// Both OSDev and the FAT specification say this is always 0
    pub(crate) _long_entry_type: u8,
    /// If this doesn't match with the computed checksum, then the set of LFNs is considered corrupt
    ///
    /// A [`LFNEntry`] will be marked as corrupt even if it isn't, if the Sfn is modified by a legacy system,
    /// since the new Sfn's signature and the one on this field won't (probably) match
    pub(crate) checksum: u8,
    pub(crate) mid_chars: [U16; LFN_MID_CHARS],
    pub(crate) _zeroed: [u8; 2],
    pub(crate) last_chars: [U16; LFN_LAST_CHARS],
}

#[derive(Debug, Immutable, FromBytes, IntoBytes)]
#[repr(C)]
pub(crate) struct LFNCharsSlice {
    first: [u16; LFN_FIRST_CHARS],
    mid: [u16; LFN_MID_CHARS],
    last: [u16; LFN_LAST_CHARS],
}

impl LFNEntry {
    pub(crate) fn copy_lfn_name(&self, slice: &mut [u16; CHARS_PER_LFN_ENTRY]) {
        let chars: &mut LFNCharsSlice = zerocopy::transmute_mut!(slice);
        chars.first = self.first_chars.map(U16::into);
        chars.mid = self.mid_chars.map(U16::into);
        chars.last = self.last_chars.map(U16::into);
    }

    #[inline]
    pub(crate) fn verify_signature(&self) -> bool {
        self._long_entry_type == 0 && self._zeroed.iter().all(|v| *v == 0)
    }
}

/// Estimate how many entries a file with the provided file name would take
///
/// This only takes into account the [`DirEntries`](DirEntry) needed,
/// not the contents of the file
pub(crate) fn calc_lfn_entries_needed<S>(file_name: S) -> num::NonZero<EntryCount>
where
    S: AsRef<str>,
{
    let file_name = file_name.as_ref();
    let char_count = file_name.chars().count();
    let lfn_entries_needed = char_count.div_ceil(CHARS_PER_LFN_ENTRY);

    num::NonZero::new(
        EntryCount::try_from(lfn_entries_needed)
            .expect("an LFN can be up to 255 chars, this won't panic"),
    )
    .expect("as seen above, this is >= 1")
}

#[derive(Debug)]
pub(crate) struct LFNEntryGenerator {
    // a necessary evil (lfn entries are stored in reverse (thanks microsoft!))
    chars: Box<[Box<[u8]>]>,
    current_entry: u8,
    checksum: u8,

    exhausted: bool,
}

impl LFNEntryGenerator {
    pub(crate) fn new<S>(filename: S, checksum: u8) -> Self
    where
        S: AsRef<str>,
    {
        let filename = filename.as_ref();
        let chars: Box<[Box<[u8]>]> = filename
            .encode_utf16()
            .collect::<Box<[u16]>>()
            .chunks(CHARS_PER_LFN_ENTRY)
            .map(|s| {
                s.iter()
                    .copied()
                    .flat_map(u16::to_le_bytes)
                    .collect::<Box<[u8]>>()
            })
            .collect();

        Self {
            current_entry: u8::try_from(chars.len())
                .expect("we won't be stored more that 20 entries"),
            chars,
            checksum,

            exhausted: false,
        }
    }
}

impl Iterator for LFNEntryGenerator {
    type Item = LFNEntry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }

        let current_chars = &self.chars[usize::from(self.current_entry - 1)];
        let mut chars = LFNCharsSlice::new_zeroed();
        chars.as_mut_bytes()[..current_chars.len()].copy_from_slice(current_chars);

        let lfn_mask = if self.current_entry
            >= u8::try_from(self.chars.len()).expect("we won't be stored more that 20 entries")
        {
            LAST_LFN_ENTRY_MASK
        } else {
            0
        };

        self.current_entry -= 1;

        if self.current_entry == 0 {
            self.exhausted = true;
        }

        Some(LFNEntry {
            order: lfn_mask | (self.current_entry + 1),
            first_chars: zerocopy::transmute!(chars.first),
            _lfn_attribute: RawAttributes::LFN,
            _long_entry_type: 0,
            checksum: self.checksum,
            mid_chars: zerocopy::transmute!(chars.mid),
            _zeroed: [0, 0],
            last_chars: zerocopy::transmute!(chars.last),
        })
    }
}

impl FusedIterator for LFNEntryGenerator {}
