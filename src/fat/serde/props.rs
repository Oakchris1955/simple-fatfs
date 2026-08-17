use core::{cmp, ops};

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    string::{String, ToString},
};

use time::{Date, PrimitiveDateTime};

use super::attributes::{Attributes, RawAttributes};
use super::boot_sector::VOLUME_LABEL_BYTES;
use super::location::DirEntryChain;
use super::location::EntryLocationUnit;
use super::{CURRENT_DIR_SFN, PARENT_DIR_SFN};
use super::{DirEntry, Sfn};
use crate::block_io::prelude::*;
use crate::path::Path;
use crate::time::Clock;
use crate::utils;
use crate::{ClusterIndex, Codepage, FSResult, FileSize, FileSystem};

/// A container for file/directory properties
#[derive(Clone, Debug)]
pub struct Properties {
    pub(crate) path: Box<Path>,
    pub(crate) sfn: Sfn,
    pub(crate) codepage: Codepage,
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
        self.sfn.decode(self.codepage)
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
    pub(crate) fn from_raw(raw: RawProperties, path: Box<Path>) -> Self {
        Self {
            path,
            sfn: raw.sfn,
            codepage: raw.codepage,
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
#[expect(
    dead_code,
    reason = "this might be used later to check if a dir is corrupted etc."
)]
pub(crate) const NONROOT_MIN_DIRENTRIES: usize = 2;
// the PARENT DIR entry is always first on a directory
// other than the root directory
pub(crate) const CURRENT_DIR_ENTRY_INDEX: usize = 0;
// the PARENT DIR entry is always second on a directory
// other than the root directory
pub(crate) const PARENT_DIR_ENTRY_INDEX: usize = 1;

/// A less-detailed version of [`RawProperties`]
#[derive(Debug, Clone)]
pub(crate) struct MinProperties {
    /// Set to [`None`] to not generate a long filename when encoding
    pub(crate) name: Option<Box<str>>,
    pub(crate) sfn: Sfn,
    pub(crate) codepage: Codepage,
    pub(crate) attributes: RawAttributes,
    pub(crate) created: Option<PrimitiveDateTime>,
    pub(crate) modified: PrimitiveDateTime,
    pub(crate) accessed: Option<Date>,
    pub(crate) file_size: FileSize,
    pub(crate) data_cluster: ClusterIndex,
}

impl MinProperties {
    pub(crate) fn new(
        name: Option<Box<str>>,
        sfn: Sfn,
        codepage: Codepage,
        attributes: RawAttributes,
        datetime: PrimitiveDateTime,
        data_cluster: ClusterIndex,
    ) -> Self {
        Self {
            name,
            sfn,
            codepage,
            attributes,
            created: Some(datetime),
            modified: datetime,
            accessed: Some(datetime.date()),
            file_size: 0,
            data_cluster,
        }
    }

    pub(crate) fn new_current_dir(datetime: PrimitiveDateTime, dir_cluster: ClusterIndex) -> Self {
        Self::new(
            None,
            CURRENT_DIR_SFN,
            // everything here is normal ASCII, we can just use the default codepage
            Codepage::default(),
            RawAttributes::DIRECTORY,
            datetime,
            dir_cluster,
        )
    }

    pub(crate) fn new_parent_dir(datetime: PrimitiveDateTime, parent: EntryLocationUnit) -> Self {
        Self::new(
            None,
            PARENT_DIR_SFN,
            // everything here is normal ASCII, we can just use the default codepage
            Codepage::default(),
            RawAttributes::DIRECTORY,
            datetime,
            match parent {
                EntryLocationUnit::DataCluster(cluster) => cluster,
                EntryLocationUnit::RootDirSector(_) => 0,
            },
        )
    }

    pub(crate) fn new_file<S, C>(
        datetime: PrimitiveDateTime,
        file_name: &str,
        fs: &FileSystem<S, C>,
        parent_dir: impl AsRef<Path>,
        file_cluster: ClusterIndex,
    ) -> FSResult<Self, S::Error>
    where
        S: BlockWrite,
        C: Clock,
    {
        let sfn = utils::string::gen_sfn(file_name, fs, parent_dir)?;

        Ok(MinProperties::new(
            Some(file_name.into()),
            sfn,
            fs.options.codepage,
            // this needs to be set when creating a file
            RawAttributes::ARCHIVE,
            datetime,
            file_cluster,
        ))
    }

    pub(crate) fn new_dir<S, C>(
        datetime: PrimitiveDateTime,
        dir_name: &str,
        fs: &FileSystem<S, C>,
        parent_dir: impl AsRef<Path>,
        dir_cluster: ClusterIndex,
    ) -> FSResult<Self, S::Error>
    where
        S: BlockWrite,
        C: Clock,
    {
        let sfn = utils::string::gen_sfn(dir_name, fs, parent_dir)?;

        Ok(MinProperties::new(
            Some(dir_name.into()),
            sfn,
            fs.options.codepage,
            RawAttributes::DIRECTORY,
            datetime,
            dir_cluster,
        ))
    }

    pub(crate) fn new_volume_label(
        datetime: PrimitiveDateTime,
        label_bytes: [u8; VOLUME_LABEL_BYTES],
        codepage: Codepage,
    ) -> Self {
        Self::new(
            None,
            Sfn::new_from_slice(label_bytes),
            codepage,
            RawAttributes::VOLUME_ID,
            datetime,
            0,
        )
    }
}

impl MinProperties {
    pub(crate) fn name(&self) -> String {
        self.name
            .clone()
            .map(|boxed_str| boxed_str.to_string())
            .unwrap_or_else(|| self.short_name())
    }

    pub(crate) fn short_name(&self) -> String {
        self.sfn.decode(self.codepage)
    }
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
        let entry_path = path.as_ref().join(self.name());

        DirEntry {
            entry: Properties::from_raw(self, entry_path.into()),
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
                sfn: value.sfn,
                codepage: value.codepage,
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
