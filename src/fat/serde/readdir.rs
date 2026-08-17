use core::iter::FusedIterator;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

use super::attributes::RawAttributes;
use super::entry_composer::{LAST_AND_UNUSED_ENTRY, UNUSED_ENTRY, USED_KANJI};
use super::lfn::{LFNEntry, CHARS_PER_LFN_ENTRY, LFN_MAX_ENTRIES};
use super::location::{DirEntryChain, EntryLocation, EntryLocationIter, EntryLocationUnit};
use super::{
    DirEntry, FATDirEntry, MinProperties, RawProperties, CURRENT_DIR_ENTRY_INDEX, CURRENT_DIR_SFN,
    PARENT_DIR_ENTRY_INDEX, PARENT_DIR_SFN,
};
use crate::block_io::prelude::*;
use crate::path::{path_consts, Path};
use crate::time::Clock;
use crate::utils;
use crate::{ClusterIndex, FSError, FSResult, FileSystem, InternalFSError};

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
    entry_location_iter: Option<EntryLocationIter<'a, S, C>>,

    pub(crate) fs: &'a FileSystem<S, C>,
}

impl<'a, S, C> ReadDirRaw<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    pub(crate) fn new(fs: &'a FileSystem<S, C>, chain_start: EntryLocationUnit) -> Self {
        Self {
            lfn_buf: [0; CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES],
            lfn_buf_pos: CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES,
            lfn_checksum: None,
            current_chain: None,

            entry_location_iter: Some(EntryLocationIter::new(EntryLocation::from(chain_start), fs)),

            fs,
        }
    }

    /// Attempt to find the `.` entry of the directory this [`ReadDirRaw`] corresponds to
    ///
    /// # Errors
    ///
    /// Apart from iterator-related [`FSError`]'s, this method will also return
    /// an [`InternalFSError::MalformedEntryChain`] if the `.` entry cannot be found.
    ///
    /// This may occur if this [`ReadDirRaw`] corresponds to the root directory,
    /// the directory's entries are malformed, or if the `.` wasn't found in the
    /// expected position (according to the FAT specification, it must be first
    /// within the entry chain, so if it isn't found there, it is fair on our side to error)
    #[expect(dead_code, reason = "might come in handy later")]
    pub(crate) fn get_current_dir_entry(mut self) -> FSResult<RawProperties, S::Error> {
        self.nth(CURRENT_DIR_ENTRY_INDEX)
            .transpose()?
            .filter(|entry| entry.is_dir && entry.sfn == CURRENT_DIR_SFN)
            .ok_or(FSError::InternalFSError(
                InternalFSError::MalformedEntryChain,
            ))
    }

    /// Attempt to find the `..` entry of the directory this [`ReadDirRaw`] corresponds to
    ///
    /// # Errors
    ///
    /// Apart from iterator-related [`FSError`]'s, this method will also return
    /// an [`InternalFSError::MalformedEntryChain`] if the `..` entry cannot be found.
    ///
    /// This may occur if this [`ReadDirRaw`] corresponds to the root directory,
    /// the directory's entries are malformed, or if the `..` wasn't found in the
    /// expected position (according to the FAT specification, it must be second
    /// within the entry chain, so if it isn't found there, it is fair on our side to error)
    pub(crate) fn get_parent_dir_entry(mut self) -> FSResult<RawProperties, S::Error> {
        self.nth(PARENT_DIR_ENTRY_INDEX)
            .transpose()?
            .filter(|entry| entry.is_dir && entry.sfn == PARENT_DIR_SFN)
            .ok_or(FSError::InternalFSError(
                InternalFSError::MalformedEntryChain,
            ))
    }

    fn next_inner(&mut self) -> Result<Option<RawProperties>, S::Error> {
        let entry_location = match self
            .entry_location_iter
            .as_mut()
            .and_then(|iter| iter.next())
        {
            Some(entry_location) => entry_location?,
            // if this is `None`, the iterator has been exhausted
            None => {
                self.entry_location_iter = None;
                return Ok(None);
            }
        };

        // load the sector of the current entry
        let mut chunk = EntryLocation::get_bytes(&entry_location, self.fs)?;

        match chunk[0] {
            LAST_AND_UNUSED_ENTRY => {
                // we have exhausted this directory
                self.entry_location_iter = None;
                return Ok(None);
            }
            UNUSED_ENTRY => {
                // this entry is unused, advance to the next one
                return self.next_inner();
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
                    location: entry_location,
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
                let filename = (!self.lfn_buf.is_empty()
                    && self
                        .lfn_checksum
                        .is_some_and(|checksum| checksum == entry.sfn.gen_checksum()))
                .then(|| {
                    let parsed_str =
                        utils::string::string_from_lfn(&self.lfn_buf[self.lfn_buf_pos..]);
                    self.lfn_buf_pos = CHARS_PER_LFN_ENTRY * LFN_MAX_ENTRIES;
                    self.lfn_checksum = None;
                    parsed_str.unwrap_or(entry.sfn.decode(self.fs.options.codepage))
                });

                if let (Ok(created), Ok(modified), Ok(accessed)) = (
                    entry.created.try_into(),
                    entry.modified.try_into(),
                    entry.accessed.try_into(),
                ) {
                    return Ok(Some(RawProperties {
                        props: MinProperties {
                            name: filename.map(|string| string.into_boxed_str()),
                            sfn: entry.sfn,
                            codepage: self.fs.options.codepage,
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

        // we have nothing to return yet, advance to the next entry
        self.next_inner()
    }
}

impl<S, C> Iterator for ReadDirRaw<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
    type Item = Result<RawProperties, S::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_inner().transpose()
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
            inner: ReadDirRaw::new(fs, *chain_start),
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
                            .contains(&value.name().as_str())
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
