use core::{cmp, ops};

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    string::{String, ToString},
};

use time::{Date, PrimitiveDateTime};

use super::attributes::{Attributes, RawAttributes};
use super::location::DirEntryChain;
use super::{DirEntry, Sfn};
use crate::block_io::prelude::*;
use crate::path::Path;
use crate::time::Clock;
use crate::{ClusterIndex, Codepage, FileSize, FileSystem};

/// A container for file/directory properties
#[derive(Clone, Debug)]
pub struct Properties {
    pub(crate) path: Box<Path>,
    pub(crate) sfn: (Sfn, Codepage),
    pub(crate) is_dir: bool,
    pub(crate) attributes: Attributes,
    pub(crate) created: Option<PrimitiveDateTime>,
    pub(crate) modified: PrimitiveDateTime,
    pub(crate) accessed: Option<Date>,
    pub(crate) file_size: FileSize,
    pub(crate) data_cluster: ClusterIndex,

    pub(crate) chain: DirEntryChain,
}

impl PartialOrd for Properties {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Properties {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.path().cmp(other.path())
    }
}

impl PartialEq for Properties {
    fn eq(&self, other: &Self) -> bool {
        self.path() == other.path()
    }
}

impl Eq for Properties {}

/// Getter methods
impl Properties {
    #[inline]
    /// Get the corresponding [`Path`] to this entry
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[inline]
    /// Get the corresponding short filename for this entry
    pub fn sfn(&self) -> String {
        self.sfn.0.decode(self.sfn.1)
    }

    #[inline]
    /// Check whether this entry belongs to a directory
    pub fn is_dir(&self) -> bool {
        self.is_dir
    }

    #[inline]
    /// Check whether this entry belongs to a file
    pub fn is_file(&self) -> bool {
        !self.is_dir()
    }

    #[inline]
    /// Get the corresponding [`Attributes`] to this entry
    pub fn attributes(&self) -> &Attributes {
        &self.attributes
    }

    #[inline]
    /// Find out when this entry was created (max resolution: 1ms)
    ///
    /// Returns an [`Option`] containing a [`PrimitiveDateTime`] from the [`time`] crate,
    /// since that field is specified as optional in the FAT32 specification
    pub fn creation_time(&self) -> &Option<PrimitiveDateTime> {
        &self.created
    }

    #[inline]
    /// Find out when this entry was last modified (max resolution: 2 secs)
    ///
    /// Returns a [`PrimitiveDateTime`] from the [`time`] crate
    pub fn modification_time(&self) -> &PrimitiveDateTime {
        &self.modified
    }

    #[inline]
    /// Find out when this entry was last accessed (max resolution: 1 day)
    ///
    /// Returns an [`Option`] containing a [`Date`] from the [`time`] crate,
    /// since that field is specified as optional in the FAT32 specification
    pub fn last_accessed_date(&self) -> &Option<Date> {
        &self.accessed
    }

    #[inline]
    /// Find out the size of this entry
    ///
    /// Always returns `0` for directories
    pub fn file_size(&self) -> u32 {
        self.file_size
    }
}

impl Properties {
    pub(crate) fn from_raw(raw: RawProperties, path: Box<Path>, codepage: Codepage) -> Self {
        Self {
            path,
            sfn: (raw.sfn, codepage),
            is_dir: raw.is_dir,
            attributes: raw.attributes.into(),
            created: raw.created,
            modified: raw.modified,
            accessed: raw.accessed,
            file_size: raw.file_size,
            data_cluster: raw.data_cluster,
            chain: raw.chain,
        }
    }
}

// each directory other than the root directory must have
// at least the `.` and `..` entries
// TODO: actually check this on runtime
pub(crate) const NONROOT_MIN_DIRENTRIES: usize = 2;

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
