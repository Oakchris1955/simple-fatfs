// ser_de stands for serialization/deserialization,
// not for the popular serde package
use super::*;

use core::{iter, num};

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
use zerocopy::{little_endian::U16, FromBytes, FromZeros, Immutable, IntoBytes};

use crate::*;

pub(crate) const DIRENTRY_LIMIT: EntryCount = EntryCount::MAX;

const LAST_LFN_ENTRY_MASK: u8 = 0x40;
pub(crate) const LFN_CHAR_LIMIT: usize = 255; // not including the trailing null
const LFN_FIRST_CHARS: usize = 5;
const LFN_MID_CHARS: usize = 6;
const LFN_LAST_CHARS: usize = 2;
pub(crate) const CHARS_PER_LFN_ENTRY: usize = LFN_FIRST_CHARS + LFN_MID_CHARS + LFN_LAST_CHARS;
const LONG_ENTRY_TYPE: u8 = 0;
const LFN_MAX_ENTRIES: usize = LFN_CHAR_LIMIT.div_ceil(CHARS_PER_LFN_ENTRY);

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
            _long_entry_type: LONG_ENTRY_TYPE,
            checksum: self.checksum,
            mid_chars: zerocopy::transmute!(chars.mid),
            _zeroed: [0, 0],
            last_chars: zerocopy::transmute!(chars.last),
        })
    }
}

impl iter::FusedIterator for LFNEntryGenerator {}

/// A special case where due to 0xE5 being a valid
/// byte sequence in the Japanese codepage, 0x05
/// is used instead
pub(crate) const USED_KANJI: u8 = 0x05;
pub(crate) const UNUSED_ENTRY: u8 = 0xE5;
pub(crate) const LAST_AND_UNUSED_ENTRY: u8 = 0x00;

/// Serialize `MinProperties` into bytes
#[derive(Debug)]
pub(crate) struct EntryComposer<'a> {
    entries: &'a [MinProperties],
    entry_index: usize,

    lfn_iter: Option<LFNEntryGenerator>,
}

impl<'a> EntryComposer<'a> {
    pub(crate) fn new(entries: &'a [MinProperties]) -> Self {
        Self {
            entries,
            entry_index: 0,

            lfn_iter: None,
        }
    }
}

impl Iterator for EntryComposer<'_> {
    type Item = [u8; DIRENTRY_SIZE];

    fn next(&mut self) -> Option<Self::Item> {
        let item: Self::Item;

        if self.entry_index >= self.entries.len() {
            return None;
        }

        let current_entry = &self.entries[self.entry_index];

        match &mut self.lfn_iter {
            Some(lfn_iter) => match lfn_iter.next() {
                Some(lfn_entry) => {
                    item = zerocopy::transmute!(lfn_entry);
                }
                None => {
                    // this LFN generator has been exhausted, return the SFN entry
                    self.lfn_iter = None;
                    self.entry_index += 1;

                    item = zerocopy::transmute!(FATDirEntry::from(current_entry.clone()));
                }
            },
            None => {
                // no reason to generate a SFN if the filename is already a valid one
                match &current_entry.name {
                    Some(long_filename) => {
                        self.lfn_iter = Some(LFNEntryGenerator::new(
                            long_filename,
                            current_entry.sfn.gen_checksum(),
                        ));

                        return self.next();
                    }
                    None => {
                        self.entry_index += 1;

                        item = zerocopy::transmute!(FATDirEntry::from(current_entry.clone()));
                    }
                }
            }
        }

        Some(item)
    }
}

impl iter::FusedIterator for EntryComposer<'_> {}

#[derive(Debug)]
pub(crate) struct ReadDirInt<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    lfn_buf: [u16; CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES],
    lfn_buf_pos: usize,
    lfn_checksum: Option<u8>,
    current_chain: Option<DirEntryChain>,

    // if `None`, we have exhausted the iterator
    entry_location: Option<EntryLocation>,

    pub(crate) fs: &'a FileSystem<S, C>,
}

impl<'a, S, C> ReadDirInt<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    pub(crate) fn new(fs: &'a FileSystem<S, C>, chain_start: &EntryLocationUnit) -> Self {
        Self {
            lfn_buf: [0; CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES],
            lfn_buf_pos: CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES,
            lfn_checksum: None,
            current_chain: None,

            entry_location: Some(EntryLocation {
                unit: *chain_start,
                index: 0,
            }),

            fs,
        }
    }

    fn _next(&mut self) -> Result<Option<RawProperties>, S::Error> {
        // if this is `None`, the iterator has been exhausted
        let entry_location = match &mut self.entry_location {
            Some(entry_location) => entry_location,
            None => return Ok(None),
        };

        // load the sector of the current entry
        let mut chunk = entry_location.get_bytes(self.fs)?;

        match chunk[0] {
            LAST_AND_UNUSED_ENTRY => {
                self.entry_location = None;
                // we have exhausted this directory
                return Ok(None);
            }
            UNUSED_ENTRY => {
                self.entry_location = entry_location.next_entry(self.fs)?;
                return Ok(None);
            }
            USED_KANJI => chunk[0] = UNUSED_ENTRY,
            _ => (),
        };

        let entry: FATDirEntry = zerocopy::transmute!(chunk);

        // update current entry chain data
        match &mut self.current_chain {
            Some(current_chain) => current_chain.len += 1,
            None => {
                self.current_chain = Some(DirEntryChain {
                    location: *entry_location,
                    len: 1,
                })
            }
        }

        'outer: {
            if entry.attributes.contains(RawAttributes::LFN) {
                // TODO: perhaps there is a way to utilize the `order` field?
                let lfn_entry: LFNEntry = zerocopy::transmute!(chunk);

                // If the signature verification fails, consider this entry corrupted
                if !lfn_entry.verify_signature() {
                    if let Some(current_chain) = &mut self.current_chain {
                        current_chain.len -= 1
                    }
                    break 'outer;
                }

                match self.lfn_checksum {
                    Some(checksum) => {
                        if checksum != lfn_entry.checksum {
                            self.lfn_checksum = None;
                            self.lfn_buf_pos = CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES;
                            self.current_chain = None;
                            break 'outer;
                        }
                    }
                    None => self.lfn_checksum = Some(lfn_entry.checksum),
                }

                if self.lfn_buf_pos == 0 {
                    // buffer is full (max number of entries already used)
                    self.lfn_checksum = None;
                    self.lfn_buf_pos = CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES;
                    self.current_chain = None;
                    break 'outer;
                }

                self.lfn_buf_pos -= CHARS_PER_LFN_ENTRY;
                lfn_entry.copy_lfn_name(
                    (&mut self.lfn_buf[self.lfn_buf_pos..self.lfn_buf_pos + CHARS_PER_LFN_ENTRY])
                        .try_into()
                        .unwrap(),
                );
            } else {
                let filename = if !self.lfn_buf.is_empty()
                    && self
                        .lfn_checksum
                        .is_some_and(|checksum| checksum == entry.sfn.gen_checksum())
                {
                    let parsed_str =
                        utils::string::string_from_lfn(&self.lfn_buf[self.lfn_buf_pos..]);
                    self.lfn_buf_pos = CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES;
                    self.lfn_checksum = None;
                    Some(parsed_str.unwrap_or(entry.sfn.decode(self.fs.options.codepage)))
                } else {
                    None
                };

                if let (Ok(created), Ok(modified), Ok(accessed)) = (
                    entry.created.try_into(),
                    entry.modified.try_into(),
                    entry.accessed.try_into(),
                ) {
                    self.entry_location = entry_location.next_entry(self.fs)?;

                    return Ok(Some(RawProperties {
                        name: filename,
                        sfn: entry.sfn,
                        is_dir: entry.attributes.contains(RawAttributes::DIRECTORY),
                        attributes: entry.attributes,
                        created,
                        modified,
                        accessed,
                        file_size: entry.file_size.into(),
                        data_cluster: (ClusterIndex::from(entry.cluster_high)
                            << (ClusterIndex::BITS / 2))
                            + ClusterIndex::from(entry.cluster_low),
                        chain: self
                            .current_chain
                            .take()
                            .expect("at this point, this shouldn't be None"),
                    }));
                }
            }
        }

        self.entry_location = entry_location.next_entry(self.fs)?;

        Ok(None)
    }
}

impl<S, C> Iterator for ReadDirInt<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
    type Item = Result<RawProperties, S::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // we want what we are doing here to be clear
            #[expect(clippy::question_mark)]
            if self.entry_location.is_none() {
                return None;
            }

            match self._next().transpose() {
                Some(result) => return Some(result),

                None => continue,
            }
        }
    }
}

impl<S, C> iter::FusedIterator for ReadDirInt<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
}
