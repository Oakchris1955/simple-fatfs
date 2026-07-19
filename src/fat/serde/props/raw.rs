use core::ops;

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    string::{String, ToString},
};
use zerocopy::{
    little_endian::{U16, U32},
    FromBytes, Immutable, IntoBytes,
};

use crate::block_io::prelude::*;
use crate::Attributes;

use super::super::location::DirEntryChain;
use super::super::time::{EntryCreationTime, EntryLastAccessedTime, EntryModificationTime};
use crate::path::Path;
use crate::{Clock, ClusterIndex, Codepage, DirEntry, FileSize, FileSystem, Properties, Sfn};

use bitflags::bitflags;
use time::{Date, PrimitiveDateTime};

/// A list of the various (raw) attributes specified for a file/directory
///
/// To check whether a given [`Attributes`] struct contains a flag, use the [`contains()`](Attributes::contains()) method
///
/// Generated using [bitflags](https://docs.rs/bitflags/2.6.0/bitflags/)
#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct RawAttributes(u8);

bitflags! {
    impl RawAttributes: u8 {
        /// This entry is read-only
        const READ_ONLY = 0x01;
        /// This entry is normally hidden
        const HIDDEN = 0x02;
        /// This entry is a system file
        const SYSTEM = 0x04;
        /// This entry represents the volume's ID.
        /// This is used internally and the library will never return such an entry
        const VOLUME_ID = 0x08;
        /// This entry is a directory. You should normally use a [`PathBuf`]s [`is_dir()`](PathBuf::is_dir) method instead
        const DIRECTORY = 0x10;
        /// This entry is marked to be archived. Used by archiving software for backing up files and directories
        const ARCHIVE = 0x20;

        /// This entry is part of a LFN (long filename). Used internally
        const LFN = Self::READ_ONLY.bits() |
                    Self::HIDDEN.bits() |
                    Self::SYSTEM.bits() |
                    Self::VOLUME_ID.bits();
    }
}

impl RawAttributes {
    pub(crate) fn from_attributes(attributes: Attributes, is_dir: bool) -> Self {
        let mut raw = RawAttributes::empty();

        raw.set(RawAttributes::READ_ONLY, attributes.read_only);
        raw.set(RawAttributes::HIDDEN, attributes.hidden);
        raw.set(RawAttributes::SYSTEM, attributes.system);
        raw.set(RawAttributes::ARCHIVE, attributes.archive);
        raw.set(RawAttributes::DIRECTORY, is_dir);

        raw
    }
}

// each directory other than the root directory must have
// at least the `.` and `..` entries
// TODO: actually check this on runtime
pub(crate) const NONROOT_MIN_DIRENTRIES: usize = 2;

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

/// A less-detailed version of [`RawProperties`]
#[derive(Debug, Clone)]
pub(crate) struct MinProperties {
    /// Set to [`None`] to not generate a long filename when encoding
    pub(crate) name: Option<Box<str>>,
    pub(crate) sfn: Sfn,
    pub(crate) attributes: RawAttributes,
    pub(crate) created: Option<PrimitiveDateTime>,
    pub(crate) modified: PrimitiveDateTime,
    pub(crate) accessed: Option<Date>,
    pub(crate) file_size: FileSize,
    pub(crate) data_cluster: ClusterIndex,
}

impl From<RawProperties> for MinProperties {
    fn from(value: RawProperties) -> Self {
        value.props
    }
}

impl From<Properties> for MinProperties {
    fn from(value: Properties) -> Self {
        Self::from(RawProperties::from(value))
    }
}

impl<S, C> From<DirEntry<'_, S, C>> for MinProperties
where
    S: BlockRead,
    C: Clock,
{
    fn from(value: DirEntry<'_, S, C>) -> Self {
        Self::from(value.entry)
    }
}

/// A resolved file/directory entry (for internal usage only)
#[derive(Debug, Clone)]
pub(crate) struct RawProperties {
    pub(crate) props: MinProperties,
    pub(crate) is_dir: bool,
    pub(crate) chain: DirEntryChain,
}

impl RawProperties {
    pub(crate) fn name(&self, codepage: Codepage) -> String {
        self.name
            .clone()
            .map(|boxed_str| boxed_str.to_string())
            .unwrap_or_else(|| self.sfn.decode(codepage))
    }

    pub(crate) fn into_dir_entry<'a, P, S, C>(
        self,
        path: P,
        fs: &'a FileSystem<S, C>,
    ) -> DirEntry<'a, S, C>
    where
        P: AsRef<Path>,
        S: BlockRead,
        C: Clock,
    {
        let entry_path = path.as_ref().join(self.name(fs.options.codepage));

        DirEntry {
            entry: Properties::from_raw(self, entry_path.into(), fs.options.codepage),
            fs,
        }
    }

    pub(crate) fn from_chain(props: MinProperties, chain: DirEntryChain) -> Self {
        Self {
            is_dir: props.attributes.contains(RawAttributes::DIRECTORY),
            props,
            chain,
        }
    }
}

impl ops::Deref for RawProperties {
    type Target = MinProperties;

    fn deref(&self) -> &Self::Target {
        &self.props
    }
}

impl From<Properties> for RawProperties {
    fn from(value: Properties) -> Self {
        Self {
            props: MinProperties {
                name: Some(
                    value
                        .path
                        .file_name()
                        .expect("the path is normalized")
                        .into(),
                ),
                sfn: value.sfn.0,
                attributes: RawAttributes::from_attributes(value.attributes, value.is_dir),
                created: value.created,
                modified: value.modified,
                accessed: value.accessed,
                file_size: value.file_size,
                data_cluster: value.data_cluster,
            },
            is_dir: value.is_dir,
            chain: value.chain,
        }
    }
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
