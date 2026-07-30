use core::iter::FusedIterator;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

use super::attributes::RawAttributes;
use super::entry_composer::{LAST_AND_UNUSED_ENTRY, UNUSED_ENTRY, USED_KANJI};
use super::lfn::{LFNEntry, CHARS_PER_LFN_ENTRY, LFN_MAX_ENTRIES};
use super::location::{DirEntryChain, EntryLocation, EntryLocationUnit};
use super::{DirEntry, FATDirEntry, MinProperties, RawProperties};
use crate::block_io::prelude::*;
use crate::path::{path_consts, Path};
use crate::time::Clock;
use crate::utils;
use crate::{ClusterIndex, FileSystem};

#[derive(Debug)]
pub(crate) struct ReadDirRaw<'a, S, C>
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

impl<'a, S, C> ReadDirRaw<'a, S, C>
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

            entry_location: Some(EntryLocation::from(*chain_start)),

            fs,
        }
    }

    fn next_inner(&mut self) -> Result<Option<RawProperties>, S::Error> {
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
                        props: MinProperties {
                            name: filename.map(|string| string.into_boxed_str()),
                            sfn: entry.sfn,
                            attributes: entry.attributes,
                            created,
                            modified,
                            accessed,
                            file_size: entry.file_size.into(),
                            data_cluster: (ClusterIndex::from(entry.cluster_high)
                                << (ClusterIndex::BITS / 2))
                                + ClusterIndex::from(entry.cluster_low),
                        },
                        is_dir: entry.attributes.contains(RawAttributes::DIRECTORY),
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

impl<S, C> Iterator for ReadDirRaw<'_, S, C>
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

            match self.next_inner().transpose() {
                Some(result) => return Some(result),

                None => continue,
            }
        }
    }
}

impl<S, C> FusedIterator for ReadDirRaw<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
}

/// Iterator over the entries in a directory.
///
/// The order in which this iterator returns entries can vary
/// and shouldn't be relied upon
#[derive(Debug)]
pub struct ReadDir<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    inner: ReadDirRaw<'a, S, C>,
    parent: Box<Path>,
    /// Whether this iterator is intended for internal or public use
    internal: bool,
}

impl<'a, S, C> ReadDir<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    pub(crate) fn new<P>(
        fs: &'a FileSystem<S, C>,
        chain_start: &EntryLocationUnit,
        parent: P,
        internal: bool,
    ) -> Self
    where
        P: AsRef<Path>,
    {
        Self {
            inner: ReadDirRaw::new(fs, chain_start),
            parent: parent.as_ref().into(),
            internal,
        }
    }
}

impl<'a, S, C> Iterator for ReadDir<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    type Item = Result<DirEntry<'a, S, C>, S::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let res = self.inner.next()?;
            match res {
                Ok(value) => {
                    if (self.internal
                        || self.inner.fs.filter.borrow().filter(&value))
                        // we shouldn't expose the special entries to the user
                        && ![path_consts::CURRENT_DIR_STR, path_consts::PARENT_DIR_STR]
                            .contains(&value.name(self.inner.fs.options.codepage).as_str())
                    {
                        return Some(Ok(value.into_dir_entry(&self.parent, self.inner.fs)));
                    } else {
                        continue;
                    }
                }
                Err(err) => return Some(Err(err)),
            }
        }
    }
}

impl<S, C> FusedIterator for ReadDir<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
}
