use super::*;

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, string::String};

use crate::*;

use ::time;
use bincode::{impl_borrow_decode, Decode, Encode};
use bitflags::bitflags;
use time::{Date, PrimitiveDateTime};

bitflags! {
    /// A list of the various (raw) attributes specified for a file/directory
    ///
    /// To check whether a given [`Attributes`] struct contains a flag, use the [`contains()`](Attributes::contains()) method
    ///
    /// Generated using [bitflags](https://docs.rs/bitflags/2.6.0/bitflags/)
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub(crate) struct RawAttributes: u8 {
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

impl<Context> bincode::Decode<Context> for RawAttributes {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(RawAttributes::from_bits_truncate(
            <u8 as bincode::Decode<Context>>::decode(decoder)?,
        ))
    }
}

impl_borrow_decode!(RawAttributes);

impl bincode::Encode for RawAttributes {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        bincode::Encode::encode(&self.bits(), encoder)?;
        Ok(())
    }
}

impl From<(Attributes, bool)> for RawAttributes {
    fn from((attributes, is_dir): (Attributes, bool)) -> Self {
        let mut raw_attributes = RawAttributes::empty();

        raw_attributes.set(RawAttributes::READ_ONLY, attributes.read_only);
        raw_attributes.set(RawAttributes::HIDDEN, attributes.hidden);
        raw_attributes.set(RawAttributes::SYSTEM, attributes.system);
        raw_attributes.set(RawAttributes::ARCHIVE, attributes.archive);
        raw_attributes.set(RawAttributes::DIRECTORY, is_dir);

        raw_attributes
    }
}

// each directory other than the root directory must have
// at least the `.` and `..` entries
// TODO: actually check this on runtime
pub(crate) const NONROOT_MIN_DIRENTRIES: usize = 2;

#[derive(Encode, Decode, Debug, Clone, Copy)]
pub(crate) struct FATDirEntry {
    pub(crate) sfn: Sfn,
    pub(crate) attributes: RawAttributes,
    pub(crate) _reserved: [u8; 1],
    pub(crate) created: EntryCreationTime,
    pub(crate) accessed: EntryLastAccessedTime,
    pub(crate) cluster_high: u16,
    pub(crate) modified: EntryModificationTime,
    pub(crate) cluster_low: u16,
    pub(crate) file_size: FileSize,
}

/// A less-detailed version of [`RawProperties`]
#[derive(Debug, Clone)]
pub(crate) struct MinProperties {
    pub(crate) name: Box<str>,
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
        Self {
            name: Box::from(value.name),
            sfn: value.sfn,
            attributes: value.attributes,
            created: value.created,
            modified: value.modified,
            accessed: value.accessed,
            file_size: value.file_size,
            data_cluster: value.data_cluster,
        }
    }
}

impl From<Properties> for MinProperties {
    fn from(value: Properties) -> Self {
        Self::from(RawProperties::from(value))
    }
}

impl From<DirEntry> for MinProperties {
    fn from(value: DirEntry) -> Self {
        Self::from(value.entry)
    }
}

/// A resolved file/directory entry (for internal usage only)
#[derive(Debug, Clone)]
pub(crate) struct RawProperties {
    pub(crate) name: String,
    pub(crate) sfn: Sfn,
    pub(crate) is_dir: bool,
    pub(crate) attributes: RawAttributes,
    pub(crate) created: Option<PrimitiveDateTime>,
    pub(crate) modified: PrimitiveDateTime,
    pub(crate) accessed: Option<Date>,
    pub(crate) file_size: FileSize,
    pub(crate) data_cluster: ClusterIndex,

    pub(crate) chain: DirEntryChain,
}

impl RawProperties {
    pub(crate) fn into_dir_entry<P>(self, path: P) -> DirEntry
    where
        P: AsRef<Path>,
    {
        let entry_path = path.as_ref().join(&self.name);

        DirEntry {
            entry: Properties::from((self, entry_path.into())),
        }
    }

    pub(crate) fn from_chain(props: MinProperties, chain: DirEntryChain) -> Self {
        Self {
            name: String::from(props.name),
            sfn: props.sfn,
            is_dir: props.attributes.contains(RawAttributes::DIRECTORY),
            attributes: props.attributes,
            created: props.created,
            modified: props.modified,
            accessed: props.accessed,
            file_size: props.file_size,
            data_cluster: props.data_cluster,
            chain,
        }
    }
}

impl From<Properties> for RawProperties {
    fn from(value: Properties) -> Self {
        Self {
            name: String::from(value.path.file_name().expect("the path is normalized")),
            sfn: value.sfn,
            is_dir: value.is_dir,
            attributes: (value.attributes, value.is_dir).into(),
            created: value.created,
            modified: value.modified,
            accessed: value.accessed,
            file_size: value.file_size,
            data_cluster: value.data_cluster,
            chain: value.chain,
        }
    }
}

impl From<MinProperties> for FATDirEntry {
    fn from(value: MinProperties) -> Self {
        Self {
            sfn: value.sfn,
            attributes: value.attributes,
            // according to some documents I found, this must be set to zero
            _reserved: [0x00],
            created: value.created.into(),
            accessed: value.accessed.into(),
            cluster_high: (value.data_cluster >> (u32::BITS / 2)) as u16,
            modified: value.modified.into(),
            #[allow(clippy::cast_possible_truncation)] // we are splitting a u32 here
            cluster_low: value.data_cluster as u16,
            file_size: value.file_size,
        }
    }
}
