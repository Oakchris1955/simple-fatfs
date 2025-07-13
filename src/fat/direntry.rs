use super::*;

use crate::io::prelude::*;
use crate::time::EPOCH;

use core::{fmt, iter, mem, num};

#[cfg(not(feature = "std"))]
use alloc::{
    borrow::ToOwned,
    boxed::Box,
    string::{String, ToString},
};

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

impl From<Time> for TimeAttribute {
    fn from(value: Time) -> Self {
        Self::new()
            .with_seconds(value.second() / 2)
            .with_minutes(value.minute())
            .with_hour(value.hour())
    }
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

impl From<Date> for DateAttribute {
    fn from(value: Date) -> Self {
        Self::new()
            .with_day(value.day())
            .with_month(value.month().into())
            .with_year((value.year() - EPOCH.year()) as u8)
    }
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

impl From<PrimitiveDateTime> for EntryCreationTime {
    fn from(value: PrimitiveDateTime) -> Self {
        Self {
            hundredths_of_second: (value.second() % 2) * 100 + (value.millisecond() / 10) as u8,
            time: value.time().into(),
            date: value.date().into(),
        }
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

impl From<PrimitiveDateTime> for EntryModificationTime {
    fn from(value: PrimitiveDateTime) -> Self {
        Self {
            time: value.time().into(),
            date: value.date().into(),
        }
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
            cluster_low: value.data_cluster as u16,
            file_size: value.file_size,
        }
    }
}

pub(crate) const SFN_NAME_LEN: usize = 8;
pub(crate) const SFN_EXT_LEN: usize = 3;
pub(crate) const SFN_LEN: usize = SFN_NAME_LEN + SFN_EXT_LEN;

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
/// The short filename of an entry
pub struct Sfn {
    pub(crate) name: [u8; SFN_NAME_LEN],
    pub(crate) ext: [u8; SFN_EXT_LEN],
}

pub(crate) const CURRENT_DIR_SFN: Sfn = Sfn {
    name: {
        use typed_path::constants::windows::CURRENT_DIR;

        // not pretty, but it works
        let mut s = [b' '; SFN_NAME_LEN];
        // apparently, subslicing a const slice is not const, nice!
        s[0] = CURRENT_DIR[0];
        s
    },
    ext: [b' '; SFN_EXT_LEN],
};

pub(crate) const PARENT_DIR_SFN: Sfn = Sfn {
    name: {
        use typed_path::constants::windows::PARENT_DIR;

        // not pretty, but it works
        let mut s = [b' '; SFN_NAME_LEN];
        // apparently, subslicing a const slice is not const, nice!
        s[0] = PARENT_DIR[0];
        s[1] = PARENT_DIR[1];
        s
    },
    ext: [b' '; SFN_EXT_LEN],
};

impl Sfn {
    fn get_byte_slice(&self) -> [u8; SFN_LEN] {
        let mut slice = [0; SFN_LEN];

        slice[..SFN_NAME_LEN].copy_from_slice(&self.name);
        slice[SFN_NAME_LEN..].copy_from_slice(&self.ext);

        slice
    }

    pub(crate) fn gen_checksum(&self) -> u8 {
        let mut sum = 0;

        for c in self.get_byte_slice() {
            sum = (if (sum & 1) != 0 { 0x80_u8 } else { 0_u8 })
                .wrapping_add(sum >> 1)
                .wrapping_add(c)
        }

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

const LAST_LFN_ENTRY_MASK: u8 = 0x40;
const CHARS_PER_LFN_ENTRY: usize = 13;
const LONG_ENTRY_TYPE: u8 = 0;

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

/// Estimate how many entries a file with the provided file_name would take
///
/// This only takes into account the [`DirEntries`](DirEntry) needed,
/// not the contents of the file
pub(crate) fn calc_entries_needed<S>(file_name: S) -> u32
where
    S: ToString,
{
    use crate::utils::string::as_sfn;

    let file_name = file_name.to_string();
    let char_count = file_name.chars().count();
    let lfn_entries_needed = if as_sfn(&file_name).is_some() {
        0
    } else {
        char_count.div_ceil(CHARS_PER_LFN_ENTRY)
    };
    // let's not forget the first entry
    let calc_entries_needed = 1 + lfn_entries_needed;

    calc_entries_needed as u32
}

#[derive(Debug)]
pub(crate) struct LFNEntryGenerator {
    // a necessary evil (lfn entries are stored in reverse (thanks microsoft!))
    chars: Box<[Box<[u8]>]>,
    current_entry: u8,
    checksum: u8,

    exhausted: bool,
}

impl LFNEntryGenerator {
    pub(crate) fn new<S>(filename: S, checksum: u8) -> Self
    where
        S: ToString,
    {
        let filename = filename.to_string();
        let chars: Box<[Box<[u8]>]> = filename
            .encode_utf16()
            .collect::<Box<[u16]>>()
            .chunks(CHARS_PER_LFN_ENTRY)
            .map(|s| {
                s.iter()
                    .copied()
                    .flat_map(u16::to_le_bytes)
                    .collect::<Box<[u8]>>()
            })
            .collect();

        Self {
            current_entry: chars.len() as u8,
            chars,
            checksum,

            exhausted: false,
        }
    }
}

impl Iterator for LFNEntryGenerator {
    type Item = LFNEntry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }

        let current_chars = &self.chars[(self.current_entry - 1) as usize];
        let mut chars = [0_u8; CHARS_PER_LFN_ENTRY * 2];
        chars[..current_chars.len()].copy_from_slice(current_chars);

        let lfn_mask = if self.current_entry >= self.chars.len() as u8 {
            LAST_LFN_ENTRY_MASK
        } else {
            0
        };

        self.current_entry -= 1;

        if self.current_entry == 0 {
            self.exhausted = true;
        }

        Some(LFNEntry {
            order: lfn_mask | (self.current_entry + 1),
            first_chars: chars[..10].try_into().unwrap(),
            _lfn_attribute: RawAttributes::LFN.bits(),
            _long_entry_type: LONG_ENTRY_TYPE,
            checksum: self.checksum,
            mid_chars: chars[10..22].try_into().unwrap(),
            _zeroed: [0, 0],
            last_chars: chars[22..].try_into().unwrap(),
        })
    }
}

impl iter::FusedIterator for LFNEntryGenerator {}

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
        S: Read + Seek,
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
        S: Read + Seek,
    {
        match self {
            EntryLocationUnit::DataCluster(_) => fs.props.cluster_size,
            EntryLocationUnit::RootDirSector(_) => fs.props.sector_size.into(),
        }
    }

    pub(crate) fn get_entry_sector<S>(&self, fs: &mut FileSystem<S>) -> u64
    where
        S: Read + Seek,
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
        S: Read + Seek,
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

#[derive(Debug, PartialEq)]
pub(crate) enum EntryStatus {
    Unused,
    LastUnused,
    Used,
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
    pub(crate) fn from_partition_sector<S>(sector: u32, fs: &mut FileSystem<S>) -> Self
    where
        S: Read + Seek,
    {
        let unit = if sector < fs.first_data_sector() {
            EntryLocationUnit::RootDirSector(
                (sector - fs.props.first_root_dir_sector as u32) as u16,
            )
        } else {
            EntryLocationUnit::DataCluster(fs.partition_sector_to_data_cluster(sector))
        };

        Self { unit, index: 0 }
    }

    pub(crate) fn entry_status<S>(&self, fs: &mut FileSystem<S>) -> Result<EntryStatus, S::Error>
    where
        S: Read + Seek,
    {
        let entry_sector = self.unit.get_entry_sector(fs);
        fs.load_nth_sector(entry_sector)?;

        let byte_offset = self.get_sector_byte_offset(fs);
        Ok(match fs.sector_buffer[byte_offset] {
            UNUSED_ENTRY => EntryStatus::Unused,
            LAST_AND_UNUSED_ENTRY => EntryStatus::LastUnused,
            _ => EntryStatus::Used,
        })
    }

    #[inline]
    pub(crate) fn get_entry_sector<S>(&self, fs: &mut FileSystem<S>) -> u64
    where
        S: Read + Seek,
    {
        self.unit.get_entry_sector(fs)
    }

    #[inline]
    pub(crate) fn get_sector_byte_offset<S>(&self, fs: &mut FileSystem<S>) -> usize
    where
        S: Read + Seek,
    {
        (self.index as usize * DIRENTRY_SIZE) % fs.props.sector_size as usize
    }

    pub(crate) fn free_entry<S>(&self, fs: &mut FileSystem<S>) -> Result<(), S::Error>
    where
        S: Read + Seek,
    {
        let entry_sector = self.unit.get_entry_sector(fs);
        fs.load_nth_sector(entry_sector)?;

        let byte_offset = self.get_sector_byte_offset(fs);
        fs.sector_buffer[byte_offset] = UNUSED_ENTRY;
        fs.buffer_modified = true;

        Ok(())
    }

    pub(crate) fn next_entry<S>(
        mut self,
        fs: &mut FileSystem<S>,
    ) -> Result<Option<EntryLocation>, S::Error>
    where
        S: Read + Seek,
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
#[derive(Debug, Clone)]
pub(crate) struct DirEntryChain {
    /// the location of the first corresponding entry
    pub(crate) location: EntryLocation,
    /// how many (contiguous) entries this entry chain has
    pub(crate) len: u32,
}
