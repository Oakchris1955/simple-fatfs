use bitflags::bitflags;
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
