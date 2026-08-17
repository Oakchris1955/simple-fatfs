use core::ops;

use zerocopy::{
    FromBytes, Immutable, IntoBytes,
    little_endian::{U16, U32},
};

use super::super::file::FileProps;
use super::attributes::RawAttributes;
use super::location::EntryLocationUnit;
use super::readdir::ReadDir;
use super::time::{EntryCreationTime, EntryLastAccessedTime, EntryModificationTime};
use super::{MinProperties, Properties, Sfn};
use crate::block_io::prelude::*;
use crate::time::Clock;
use crate::{EntryCount, FileSystem, ROFile, RWFile};

// a directory entry occupies 32 bytes
pub(crate) const DIRENTRY_SIZE: usize = 32;
/// The maximum number of directory entries in a directory entry chain
pub(crate) const DIRENTRY_LIMIT: EntryCount = EntryCount::MAX;

/// A thin wrapper for [`Properties`] representing a directory entry
#[derive(Debug)]
pub struct DirEntry<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    pub(crate) entry: Properties,
    pub(crate) fs: &'a FileSystem<S, C>,
}

impl<'a, S, C> DirEntry<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    /// Get the corresponding [`ROFile`] object for this [`DirEntry`]
    ///
    /// Will return [`None`] if the entry isn't a file
    pub fn to_ro_file(&self) -> Option<ROFile<'a, S, C>> {
        self.is_file().then(|| ROFile {
            fs: self.fs,
            props: FileProps {
                entry: self.entry.clone(),
                offset: 0,
                current_cluster: self.data_cluster,
            },
        })
    }

    /// Get the corresponding [`ReadDir`] object for this [`DirEntry`]
    ///
    /// Will return [`None`] if the entry isn't a directory
    pub fn to_dir(&self) -> Option<ReadDir<'a, S, C>> {
        self.is_dir().then(|| {
            ReadDir::new(
                self.fs,
                &EntryLocationUnit::DataCluster(self.data_cluster),
                self.path(),
                false,
            )
        })
    }
}

impl<'a, S, C> DirEntry<'a, S, C>
where
    S: BlockWrite,
    C: Clock,
{
    /// Get the corresponding [`RWFile`] object of this [`DirEntry`]
    ///
    /// Will return `None` if the entry is a directory
    pub fn into_rw_file(self) -> Option<RWFile<'a, S, C>> {
        self.to_ro_file().map(|ro_file| ro_file.into())
    }
}

impl<S, C> ops::Deref for DirEntry<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
    type Target = Properties;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.entry
    }
}

#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone, Copy)]
#[repr(C)]
pub(crate) struct FATDirEntry {
    pub(crate) sfn: Sfn,
    pub(crate) attributes: RawAttributes,
    reserved1: [u8; 1],
    pub(crate) created: EntryCreationTime,
    pub(crate) accessed: EntryLastAccessedTime,
    pub(crate) cluster_high: U16,
    pub(crate) modified: EntryModificationTime,
    pub(crate) cluster_low: U16,
    pub(crate) file_size: U32,
}

impl From<MinProperties> for FATDirEntry {
    fn from(value: MinProperties) -> Self {
        let [data_cluster_low, data_cluster_high] = {
            let [lo0, lo1, hi0, hi1] = value.data_cluster.to_le_bytes();
            [[lo0, lo1], [hi0, hi1]].map(u16::from_le_bytes)
        };
        Self {
            sfn: value.sfn,
            attributes: value.attributes,
            reserved1: Default::default(),
            created: value.created.into(),
            accessed: value.accessed.into(),
            cluster_high: data_cluster_high.into(),
            modified: value.modified.into(),
            cluster_low: data_cluster_low.into(),
            file_size: value.file_size.into(),
        }
    }
}
