use super::*;

use core::{cmp, ops};

use crate::*;

#[cfg(not(feature = "std"))]
use alloc::{borrow::ToOwned, boxed::Box, string::String};

use ::time;
use time::{Date, PrimitiveDateTime};
use zerocopy::{FromBytes, Immutable, IntoBytes};

/// A list of the various attributes specified for a file/directory
#[derive(Debug, Clone, Copy)]
pub struct Attributes {
    /// This is a read-only file
    pub read_only: bool,
    /// This file is to be hidden unless a request is issued
    /// explicitly requesting inclusion of “hidden files”
    pub hidden: bool,
    /// This is a system file and shouldn't be listed unless a request
    /// is issued explicitly requesting inclusion of ”system files”
    pub system: bool,
    /// This file has been modified since last archival
    /// or has never been archived.
    ///
    /// This field should only concern archival software
    pub archive: bool,
}

impl From<RawAttributes> for Attributes {
    fn from(value: RawAttributes) -> Self {
        Attributes {
            read_only: value.contains(RawAttributes::READ_ONLY),
            hidden: value.contains(RawAttributes::HIDDEN),
            system: value.contains(RawAttributes::SYSTEM),
            archive: value.contains(RawAttributes::ARCHIVE),
        }
    }
}

// a directory entry occupies 32 bytes
pub(crate) const DIRENTRY_SIZE: usize = 32;

pub(crate) const SFN_NAME_LEN: usize = 8;
pub(crate) const SFN_EXT_LEN: usize = 3;
pub(crate) const SFN_LEN: usize = SFN_NAME_LEN + SFN_EXT_LEN;

#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
/// The short filename of an entry
///
/// In FAT, each file has 2 filenames: one long and one short filename.
/// The short filename is retained for backwards-compatibility reasons
/// by the FAT specification and shouldn't concern most users.
pub(crate) struct Sfn([u8; SFN_LEN]);

pub(crate) const CURRENT_DIR_SFN: Sfn = Sfn(*b".          ");

pub(crate) const PARENT_DIR_SFN: Sfn = Sfn(*b"..         ");

impl ops::Deref for Sfn {
    type Target = [u8; SFN_LEN];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for Sfn {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for Sfn {
    fn default() -> Self {
        Sfn(*b"           ")
    }
}

impl Sfn {
    pub(crate) fn new(name: [u8; SFN_NAME_LEN], ext: [u8; SFN_EXT_LEN]) -> Self {
        let mut slice = [0_u8; SFN_LEN];

        slice[..SFN_NAME_LEN].copy_from_slice(&name);
        slice[SFN_NAME_LEN..].copy_from_slice(&ext);

        Sfn(slice)
    }

    pub(crate) fn new_from_slice(slice: [u8; SFN_LEN]) -> Self {
        Sfn(slice)
    }

    pub(crate) fn gen_checksum(&self) -> u8 {
        let mut sum = 0;

        for c in self.iter() {
            sum = (if (sum & 1) != 0 { 0x80_u8 } else { 0_u8 })
                .wrapping_add(sum >> 1)
                .wrapping_add(*c)
        }

        sum
    }

    pub(crate) fn name(&self) -> &[u8; SFN_NAME_LEN] {
        (&self[..SFN_NAME_LEN]).try_into().unwrap()
    }

    pub(crate) fn ext(&self) -> &[u8; SFN_EXT_LEN] {
        (&self[SFN_NAME_LEN..]).try_into().unwrap()
    }

    pub(crate) fn name_mut(&mut self) -> &mut [u8; SFN_NAME_LEN] {
        (&mut self[..SFN_NAME_LEN]).try_into().unwrap()
    }

    pub(crate) fn ext_mut(&mut self) -> &mut [u8; SFN_EXT_LEN] {
        (&mut self[SFN_NAME_LEN..]).try_into().unwrap()
    }

    pub(crate) fn decode(&self, codepage: Codepage) -> String {
        // one more byte for the "." between the name and the file extension
        let mut string = String::with_capacity(SFN_LEN + 1);
        // we begin by writing the name (even if it is padded with spaces, they will be trimmed, so we don't care)
        string.push_str(codepage.decode(self.name()).trim_end());

        // then, if the extension isn't empty (padded with zeroes), we write it too
        let ext = codepage.decode(self.ext()).trim_end().to_owned();
        if !ext.is_empty() {
            string.push_str(&ext);
        };

        string
    }
}

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
    pub(crate) file_size: u32,
    pub(crate) data_cluster: u32,

    // internal fields
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
    pub(crate) fn from_raw(raw_props: RawProperties, path: Box<Path>, codepage: Codepage) -> Self {
        Self {
            path,
            sfn: (raw_props.sfn, codepage),
            is_dir: raw_props.is_dir,
            attributes: raw_props.attributes.into(),
            created: raw_props.created,
            modified: raw_props.modified,
            accessed: raw_props.accessed,
            file_size: raw_props.file_size,
            data_cluster: raw_props.data_cluster,
            chain: raw_props.chain,
        }
    }
}

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
    pub fn to_rw_file(self) -> Option<RWFile<'a, S, C>> {
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
    inner: ReadDirInt<'a, S, C>,
    parent: Box<Path>,
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
    ) -> Self
    where
        P: AsRef<Path>,
    {
        Self {
            inner: ReadDirInt::new(fs, chain_start),
            parent: parent.as_ref().into(),
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
            match self.inner.next() {
                Some(res) => match res {
                    Ok(value) => {
                        if self.inner.fs.filter.borrow().filter(&value)
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
                },
                None => return None,
            }
        }
    }
}
