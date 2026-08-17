use core::iter::FusedIterator;

use crate::block_io::BlockWrite;
use crate::time::Clock;
use crate::{FSResult, FileSystem};

use super::lfn::LFNEntryGenerator;
use super::location::{EntryLocation, EntryLocationIter};
use super::{DIRENTRY_SIZE, FATDirEntry, MinProperties};

/// A special case where due to 0xE5 being a valid
/// byte sequence in the Japanese codepage, 0x05
/// is used instead
pub(crate) const USED_KANJI: u8 = 0x05;
pub(crate) const UNUSED_ENTRY: u8 = 0xE5;
pub(crate) const LAST_AND_UNUSED_ENTRY: u8 = 0x00;

/// Serialize [`MinProperties`] into bytes
#[derive(Debug)]
struct EntryComposer<'a> {
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

impl FusedIterator for EntryComposer<'_> {}

/// Convenience function to convert the provided `entries` to bytes and write them
/// at the provided `fs`, starting at `first_loc`
///
/// # Errors
///
/// Returns an [`FSError`](crate::FSError) if any IO-related error occurs
pub(crate) fn write_entries<S, C>(
    entries: &[MinProperties],
    first_loc: EntryLocation,
    fs: &FileSystem<S, C>,
) -> FSResult<(), S::Error>
where
    S: BlockWrite,
    C: Clock,
{
    let entry_composer = EntryComposer::new(entries);
    let entry_loc_iter = EntryLocationIter::new(first_loc, fs);

    entry_composer
        .zip(entry_loc_iter)
        .try_for_each(|(bytes, location_res)| {
            let location = location_res?;
            EntryLocation::set_bytes(&location, fs, bytes)
        })?;

    Ok(())
}
