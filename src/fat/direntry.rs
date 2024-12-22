use super::*;

use crate::io::prelude::*;
use crate::time::EPOCH;

use core::{fmt, mem, num};

#[cfg(not(feature = "std"))]
use alloc::{borrow::ToOwned, string::String};

use bitfield_struct::bitfield;
use bitflags::bitflags;
use serde::{Deserialize, Serialize};
use time::{Date, PrimitiveDateTime, Time};

bitflags! {
    /// A list of the various (raw) attributes specified for a file/directory
    ///
    /// To check whether a given [`Attributes`] struct contains a flag, use the [`contains()`](Attributes::contains()) method
    ///
    /// Generated using [bitflags](https://docs.rs/bitflags/2.6.0/bitflags/)
    #[derive(Deserialize, Serialize, Debug, Clone, Copy, PartialEq)]
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

/// A list of the various attributes specified for a file/directory
#[derive(Debug, Clone, Copy)]
pub struct Attributes {
    /// This is a read-only file
    pub read_only: bool,
    /// This file is to be hidden unless a request is issued
    /// explicitly requesting inclusion of “hidden files”
    pub hidden: bool,
    /// This is a system file and shouldn't be listed unless a request
    /// is issued explicitly requesting inclusion of system files”
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

#[bitfield(u16)]
#[derive(Serialize, Deserialize)]
pub(crate) struct TimeAttribute {
    /// Multiply by 2
    #[bits(5)]
    seconds: u8,
    #[bits(6)]
    minutes: u8,
    #[bits(5)]
    hour: u8,
}

#[bitfield(u16)]
#[derive(Serialize, Deserialize)]
pub(crate) struct DateAttribute {
    #[bits(5)]
    day: u8,
    #[bits(4)]
    month: u8,
    #[bits(7)]
    year: u8,
}

impl TryFrom<TimeAttribute> for Time {
    type Error = ();

    fn try_from(value: TimeAttribute) -> Result<Self, Self::Error> {
        time::parsing::Parsed::new()
            .with_hour_24(value.hour())
            .and_then(|parsed| parsed.with_minute(value.minutes()))
            .and_then(|parsed| parsed.with_second(value.seconds() * 2))
            .and_then(|parsed| parsed.try_into().ok())
            .ok_or(())
    }
}

impl TryFrom<DateAttribute> for Date {
    type Error = ();

    fn try_from(value: DateAttribute) -> Result<Self, Self::Error> {
        time::parsing::Parsed::new()
            .with_year(i32::from(value.year()) + EPOCH.year())
            .and_then(|parsed| parsed.with_month(value.month().try_into().ok()?))
            .and_then(|parsed| parsed.with_day(num::NonZeroU8::new(value.day())?))
            .and_then(|parsed| parsed.try_into().ok())
            .ok_or(())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub(crate) struct EntryCreationTime {
    pub(crate) hundredths_of_second: u8,
    pub(crate) time: TimeAttribute,
    pub(crate) date: DateAttribute,
}

impl TryFrom<EntryCreationTime> for PrimitiveDateTime {
    type Error = ();

    fn try_from(value: EntryCreationTime) -> Result<Self, Self::Error> {
        let mut time: Time = value.time.try_into()?;

        let new_seconds = time.second() + value.hundredths_of_second / 100;
        let milliseconds = u16::from(value.hundredths_of_second) % 100 * 10;
        time = time
            .replace_second(new_seconds)
            .map_err(|_| ())?
            .replace_millisecond(milliseconds)
            .map_err(|_| ())?;

        let date: Date = value.date.try_into()?;

        Ok(PrimitiveDateTime::new(date, time))
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub(crate) struct EntryModificationTime {
    pub(crate) time: TimeAttribute,
    pub(crate) date: DateAttribute,
}

impl TryFrom<EntryModificationTime> for PrimitiveDateTime {
    type Error = ();

    fn try_from(value: EntryModificationTime) -> Result<Self, Self::Error> {
        Ok(PrimitiveDateTime::new(
            value.date.try_into()?,
            value.time.try_into()?,
        ))
    }
}

// a directory entry occupies 32 bytes
pub(crate) const DIRENTRY_SIZE: usize = 32;

// each directory other than the root directory must have
// at least the `.` and `..` entries
// TODO: actually check this on runtime
pub(crate) const NONROOT_MIN_DIRENTRIES: usize = 2;

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub(crate) struct FATDirEntry {
    pub(crate) sfn: Sfn,
    pub(crate) attributes: RawAttributes,
    pub(crate) _reserved: [u8; 1],
    pub(crate) created: EntryCreationTime,
    pub(crate) accessed: DateAttribute,
    pub(crate) cluster_high: u16,
    pub(crate) modified: EntryModificationTime,
    pub(crate) cluster_low: u16,
    pub(crate) file_size: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub(crate) struct Sfn {
    name: [u8; 8],
    ext: [u8; 3],
}

impl Sfn {
    fn get_byte_slice(&self) -> [u8; 11] {
        let mut slice = [0; 11];

        slice[..8].copy_from_slice(&self.name);
        slice[8..].copy_from_slice(&self.ext);

        slice
    }

    pub(crate) fn gen_checksum(&self) -> u8 {
        let mut sum = 0;

        for c in self.get_byte_slice() {
            sum = (if (sum & 1) != 0 { 0x80_u8 } else { 0_u8 })
                .wrapping_add(sum >> 1)
                .wrapping_add(c)
        }

        log::debug!("Sfn checksum: {:X}", sum);

        sum
    }
}

impl fmt::Display for Sfn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // we begin by writing the name (even if it is padded with spaces, they will be trimmed, so we don't care)
        write!(f, "{}", String::from_utf8_lossy(&self.name).trim())?;

        // then, if the extension isn't empty (padded with zeroes), we write it too
        let ext = String::from_utf8_lossy(&self.ext).trim().to_owned();
        if !ext.is_empty() {
            write!(f, ".{}", ext)?;
        };

        Ok(())
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct LFNEntry {
    /// masked with 0x40 if this is the last entry
    pub(crate) order: u8,
    pub(crate) first_chars: [u8; 10],
    /// Always equals 0x0F
    pub(crate) _lfn_attribute: u8,
    /// Both OSDev and the FAT specification say this is always 0
    pub(crate) _long_entry_type: u8,
    /// If this doesn't match with the computed cksum, then the set of LFNs is considered corrupt
    ///
    /// A [`LFNEntry`] will be marked as corrupt even if it isn't, if the Sfn is modifed by a legacy system,
    /// since the new Sfn's signature and the one on this field won't (probably) match
    pub(crate) checksum: u8,
    pub(crate) mid_chars: [u8; 12],
    pub(crate) _zeroed: [u8; 2],
    pub(crate) last_chars: [u8; 4],
}

impl LFNEntry {
    pub(crate) fn get_byte_slice(&self) -> [u16; 13] {
        let mut slice = [0_u8; 13 * mem::size_of::<u16>()];

        slice[..10].copy_from_slice(&self.first_chars);
        slice[10..22].copy_from_slice(&self.mid_chars);
        slice[22..].copy_from_slice(&self.last_chars);

        let mut out_slice = [0_u16; 13];
        for (i, chunk) in slice.chunks(mem::size_of::<u16>()).enumerate() {
            out_slice[i] = u16::from_le_bytes(chunk.try_into().unwrap());
        }

        out_slice
    }

    #[inline]
    pub(crate) fn verify_signature(&self) -> bool {
        self._long_entry_type == 0 && self._zeroed.iter().all(|v| *v == 0)
    }
}

/// The root directory sector or data cluster a [`FATDirEntry`] belongs too
#[derive(Debug, Clone, Copy)]
pub(crate) enum EntryLocationUnit {
    /// Sector offset from the start of the root directory region (FAT12/16)
    RootDirSector(u16),
    /// Cluster offset from the start of the data region
    DataCluster(u32),
}

impl EntryLocationUnit {
    pub(crate) fn from_partition_sector<S>(sector: u32, fs: &mut FileSystem<S>) -> Self
    where
        S: Read + Write + Seek,
    {
        if sector < fs.first_data_sector() {
            EntryLocationUnit::RootDirSector(
                (sector - fs.props.first_root_dir_sector as u32) as u16,
            )
        } else {
            EntryLocationUnit::DataCluster(fs.partition_sector_to_data_cluster(sector))
        }
    }

    pub(crate) fn get_max_offset<S>(&self, fs: &mut FileSystem<S>) -> u64
    where
        S: Read + Write + Seek,
    {
        match self {
            EntryLocationUnit::DataCluster(_) => fs.props.cluster_size,
            EntryLocationUnit::RootDirSector(_) => fs.props.sector_size.into(),
        }
    }

    pub(crate) fn get_entry_sector<S>(&self, fs: &mut FileSystem<S>) -> u64
    where
        S: Read + Write + Seek,
    {
        match self {
            EntryLocationUnit::RootDirSector(root_dir_sector) => {
                (root_dir_sector + fs.props.first_root_dir_sector).into()
            }
            EntryLocationUnit::DataCluster(data_cluster) => {
                fs.data_cluster_to_partition_sector(*data_cluster).into()
            }
        }
    }

    pub(crate) fn get_next_unit<S>(
        &self,
        fs: &mut FileSystem<S>,
    ) -> Result<Option<EntryLocationUnit>, S::Error>
    where
        S: Read + Write + Seek,
    {
        match self {
            EntryLocationUnit::RootDirSector(sector) => match fs.boot_record {
                BootRecord::Fat(boot_record_fat) => {
                    if boot_record_fat.root_dir_sectors() == 0 {
                        unreachable!(concat!("This should be zero iff the FAT type if FAT32, ",
                    "in which case we won't even be reading root directory sectors, since it doesn't exist"))
                    }

                    if *sector
                        >= fs.props.first_root_dir_sector + boot_record_fat.root_dir_sectors()
                    {
                        Ok(None)
                    } else {
                        Ok(Some(EntryLocationUnit::RootDirSector(sector + 1)))
                    }
                }
                BootRecord::ExFAT(_) => todo!("ExFAT is not implemented yet"),
            },
            EntryLocationUnit::DataCluster(cluster) => Ok(fs
                .get_next_cluster(*cluster)?
                .map(EntryLocationUnit::DataCluster)),
        }
    }
}

/// The location of a [`FATDirEntry`]
#[derive(Clone, Debug)]
pub(crate) struct EntryLocation {
    /// the location of the first corresponding entry's data unit
    pub(crate) unit: EntryLocationUnit,
    /// the first entry's index/offset from the start of the data unit
    pub(crate) index: u32,
}

impl EntryLocation {
    pub(crate) fn free_entry<S>(&self, fs: &mut FileSystem<S>) -> Result<(), S::Error>
    where
        S: Read + Write + Seek,
    {
        let entry_sector = self.unit.get_entry_sector(fs);
        fs.read_nth_sector(entry_sector)?;

        let byte_offset = self.index as usize * DIRENTRY_SIZE;
        fs.sector_buffer[byte_offset] = UNUSED_ENTRY;
        fs.buffer_modified = true;

        Ok(())
    }

    pub(crate) fn next_entry<S>(
        mut self,
        fs: &mut FileSystem<S>,
    ) -> Result<Option<EntryLocation>, S::Error>
    where
        S: Read + Write + Seek,
    {
        self.index += 1;

        // we haven't advanced to a new unit, we return immediately
        if u64::from(self.index) < self.unit.get_max_offset(fs) {
            return Ok(Some(self));
        }

        // we try to advance to the next entry unit (if it exists)
        Ok(self.unit.get_next_unit(fs)?.map(|unit| {
            self.unit = unit;
            self.index = 0;

            self
        }))
    }
}

/// The location of a chain of [`FATDirEntry`]
#[derive(Debug)]
pub(crate) struct DirEntryChain {
    /// the location of the first corresponding entry
    pub(crate) location: EntryLocation,
    /// how many (contiguous) entries this entry chain has
    pub(crate) len: u32,
}
