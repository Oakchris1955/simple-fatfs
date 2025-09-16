// ser_de stands for serialization/deserialization,
// not for the popular serde package
use super::*;

use core::{iter, num};

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

use crate::*;

use bincode::{Decode, Encode};
use embedded_io::*;

pub(crate) const DIRENTRY_LIMIT: EntryCount = EntryCount::MAX;

const LAST_LFN_ENTRY_MASK: u8 = 0x40;
pub(crate) const LFN_CHAR_LIMIT: usize = 255; // not including the trailing null
const LFN_FIRST_CHARS: usize = 5;
const LFN_MID_CHARS: usize = 6;
const LFN_LAST_CHARS: usize = 2;
pub(crate) const CHARS_PER_LFN_ENTRY: usize = LFN_FIRST_CHARS + LFN_MID_CHARS + LFN_LAST_CHARS;
const LONG_ENTRY_TYPE: u8 = 0;
const LFN_MAX_ENTRIES: usize = LFN_CHAR_LIMIT.div_ceil(CHARS_PER_LFN_ENTRY);

#[derive(Debug, Encode, Decode)]
pub(crate) struct LFNEntry {
    /// masked with 0x40 if this is the last entry
    pub(crate) order: u8,
    pub(crate) first_chars: [u8; LFN_FIRST_CHARS * 2],
    /// Always equals 0x0F
    pub(crate) _lfn_attribute: u8,
    /// Both OSDev and the FAT specification say this is always 0
    pub(crate) _long_entry_type: u8,
    /// If this doesn't match with the computed checksum, then the set of LFNs is considered corrupt
    ///
    /// A [`LFNEntry`] will be marked as corrupt even if it isn't, if the Sfn is modified by a legacy system,
    /// since the new Sfn's signature and the one on this field won't (probably) match
    pub(crate) checksum: u8,
    pub(crate) mid_chars: [u8; LFN_MID_CHARS * 2],
    pub(crate) _zeroed: [u8; 2],
    pub(crate) last_chars: [u8; LFN_LAST_CHARS * 2],
}

impl LFNEntry {
    pub(crate) fn copy_lfn_name(&self, slice: &mut [u16; CHARS_PER_LFN_ENTRY]) {
        {
            // reinterpret the u16 array as an u8 array (which is always sound)
            let slice = unsafe { &mut *(slice.as_mut_ptr() as *mut [u8; CHARS_PER_LFN_ENTRY * 2]) };

            // copy the bytes from the lfn name into it
            slice[..LFN_FIRST_CHARS * 2].copy_from_slice(&self.first_chars);
            slice[LFN_FIRST_CHARS * 2..(LFN_FIRST_CHARS + LFN_MID_CHARS) * 2]
                .copy_from_slice(&self.mid_chars);
            slice[(LFN_FIRST_CHARS + LFN_MID_CHARS) * 2..].copy_from_slice(&self.last_chars);
        }

        // fix endian (if required)
        slice.iter_mut().for_each(|c| *c = c.to_le());
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
pub(crate) fn calc_entries_needed<S>(file_name: S, codepage: Codepage) -> num::NonZero<EntryCount>
where
    S: AsRef<str>,
{
    use crate::utils::string::as_sfn;

    let file_name = file_name.as_ref();
    let char_count = file_name.chars().count();
    let lfn_entries_needed = if as_sfn(file_name, codepage).is_some() {
        0
    } else {
        char_count.div_ceil(CHARS_PER_LFN_ENTRY)
    };
    // let's not forget the first entry
    let calc_entries_needed = 1 + lfn_entries_needed;

    num::NonZero::new(
        EntryCount::try_from(calc_entries_needed)
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
        let mut chars = [0_u8; CHARS_PER_LFN_ENTRY * 2];
        chars[..current_chars.len()].copy_from_slice(current_chars);

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
            first_chars: chars[..10].try_into().unwrap(),
            _lfn_attribute: RawAttributes::LFN.bits(),
            _long_entry_type: LONG_ENTRY_TYPE,
            checksum: self.checksum,
            mid_chars: chars[10..22].try_into().unwrap(),
            _zeroed: [0, 0],
            last_chars: chars[22..].try_into().unwrap(),
        })
    }
}

impl iter::FusedIterator for LFNEntryGenerator {}

pub(crate) const UNUSED_ENTRY: u8 = 0xE5;
pub(crate) const LAST_AND_UNUSED_ENTRY: u8 = 0x00;

/// Serialize `MinProperties` into bytes
#[derive(Debug)]
pub(crate) struct EntryComposer {
    entries: Box<[MinProperties]>,
    entry_index: usize,

    lfn_iter: Option<LFNEntryGenerator>,

    codepage: Codepage,
}

impl EntryComposer {
    pub(crate) fn new(entries: Box<[MinProperties]>, codepage: Codepage) -> Self {
        Self {
            entries,
            entry_index: 0,

            lfn_iter: None,

            codepage,
        }
    }
}

impl Iterator for EntryComposer {
    type Item = [u8; DIRENTRY_SIZE];

    fn next(&mut self) -> Option<Self::Item> {
        use utils::bincode::BINCODE_CONFIG;

        let mut item: Self::Item = [0; DIRENTRY_SIZE];

        if self.entry_index >= self.entries.len() {
            return None;
        }

        let current_entry = &self.entries[self.entry_index];

        match &mut self.lfn_iter {
            Some(lfn_iter) => match lfn_iter.next() {
                Some(lfn_entry) => {
                    bincode::encode_into_slice(lfn_entry, &mut item, BINCODE_CONFIG)
                        .expect("these are completely valid data, this shouldn't panic");
                }
                None => {
                    // this LFN generator has been exhausted, return the SFN entry
                    self.lfn_iter = None;
                    self.entry_index += 1;

                    bincode::encode_into_slice(
                        FATDirEntry::from(current_entry.clone()),
                        &mut item,
                        BINCODE_CONFIG,
                    )
                    .expect("these are completely valid data, this shouldn't panic");
                }
            },
            None => {
                // no reason to generate a SFN if the filename is already a valid one
                if utils::string::as_sfn(&current_entry.name, self.codepage)
                    .is_some_and(|sfn| sfn == current_entry.sfn)
                {
                    self.entry_index += 1;

                    bincode::encode_into_slice(
                        FATDirEntry::from(current_entry.clone()),
                        &mut item,
                        BINCODE_CONFIG,
                    )
                    .expect("these are completely valid data, this shouldn't panic");
                } else {
                    self.lfn_iter = Some(LFNEntryGenerator::new(
                        &current_entry.name,
                        current_entry.sfn.gen_checksum(),
                    ));

                    return self.next();
                }
            }
        }

        Some(item)
    }
}

impl iter::FusedIterator for EntryComposer {}

#[derive(Debug)]
pub(crate) struct ReadDirInt<'a, S, C>
where
    S: Read + Seek,
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
    S: Read + Seek,
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
        use utils::bincode::BINCODE_CONFIG;

        // if this is `None`, the iterator has been exhausted
        let entry_location = match &mut self.entry_location {
            Some(entry_location) => entry_location,
            None => return Ok(None),
        };

        // load the sector of the current entry
        let chunk = entry_location.get_bytes(self.fs)?;

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
            _ => (),
        };

        let Ok(entry) =
            bincode::decode_from_slice::<FATDirEntry, _>(&chunk, BINCODE_CONFIG).map(|(v, _)| v)
        else {
            // FIXME: handle such error cases or panic
            return Ok(None);
        };

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
                let Ok((lfn_entry, _)) =
                    bincode::decode_from_slice::<LFNEntry, _>(&chunk, BINCODE_CONFIG)
                else {
                    if let Some(current_chain) = &mut self.current_chain {
                        current_chain.len -= 1
                    }
                    // FIXME: handle such error cases or panic
                    break 'outer;
                };

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
                    parsed_str.unwrap_or(entry.sfn.decode(self.fs.options.codepage))
                } else {
                    entry.sfn.decode(self.fs.options.codepage)
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
                        file_size: entry.file_size,
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
    S: Read + Seek,
    C: Clock,
{
    type Item = Result<RawProperties, S::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // we want what we are doing here to be clear
            #[allow(clippy::question_mark)]
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
    S: Read + Seek,
    C: Clock,
{
}
