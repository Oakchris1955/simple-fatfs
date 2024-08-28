#[cfg(not(feature = "std"))]
use core::*;
#[cfg(feature = "std")]
use std::*;

use ::alloc::{
    borrow::ToOwned,
    format,
    string::{FromUtf16Error, String, ToString},
    vec,
    vec::*,
};

use bitfield_struct::bitfield;
use bitflags::bitflags;

use bincode::Options as _;
use serde::{Deserialize, Serialize};
use serde_big_array::BigArray;

use ::time;
use time::{Date, PrimitiveDateTime, Time};

use crate::{error::*, io::prelude::*, path::PathBuf};

/// The minimum size (in bytes) a sector is allowed to have
pub const SECTOR_SIZE_MIN: usize = 512;
/// The maximum size (in bytes) a sector is allowed to have
pub const SECTOR_SIZE_MAX: usize = 4096;

/// Place this in the BPB _jmpboot field to hang if a computer attempts to boot this partition
/// The first two bytes jump to 0 on all bit modes and the third byte is just a NOP
const INFINITE_LOOP: [u8; 3] = [0xEB, 0xFE, 0x90];

const BPBFAT_SIZE: usize = 36;
#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct BPBFAT {
    _jmpboot: [u8; 3],
    _oem_identifier: [u8; 8],
    bytes_per_sector: u16,
    sectors_per_cluster: u8,
    reserved_sector_count: u16,
    table_count: u8,
    root_entry_count: u16,
    // If this is 0, check `total_sectors_32`
    total_sectors_16: u16,
    _media_type: u8,
    table_size_16: u16,
    _sectors_per_track: u16,
    _head_side_count: u16,
    hidden_sector_count: u32,
    total_sectors_32: u32,
}

#[derive(Debug)]
enum BootRecord {
    FAT(BootRecordFAT),
    ExFAT(BootRecordExFAT),
}

impl BootRecord {
    #[inline]
    /// The FAT type of this file system
    pub(crate) fn fat_type(&self) -> FATType {
        match self {
            BootRecord::FAT(boot_record_fat) => {
                let total_clusters = boot_record_fat.total_clusters();
                if total_clusters < 4085 {
                    FATType::FAT12
                } else if total_clusters < 65525 {
                    FATType::FAT16
                } else {
                    FATType::FAT32
                }
            }
            BootRecord::ExFAT(_boot_record_exfat) => {
                todo!("ExFAT not yet implemented");
                FATType::ExFAT
            }
        }
    }

    #[allow(non_snake_case)]
    fn nth_FAT_table_sector(&self, n: u8) -> u32 {
        match self {
            BootRecord::FAT(boot_record_fat) => {
                boot_record_fat.first_fat_sector() as u32
                    + n as u32 * boot_record_fat.fat_sector_size()
            }
            BootRecord::ExFAT(boot_record_exfat) => {
                // this should work, but ExFAT is not yet implemented, so...
                todo!("ExFAT not yet implemented");
                boot_record_exfat.fat_count as u32 + n as u32 * boot_record_exfat.fat_len
            }
        }
    }
}

const BOOT_SIGNATURE: u8 = 0x29;
const FAT_SIGNATURE: u16 = 0x55AA;

#[derive(Debug, Clone, Copy)]
struct BootRecordFAT {
    bpb: BPBFAT,
    ebr: EBR,
}

impl BootRecordFAT {
    #[inline]
    fn verify_signature(&self) -> bool {
        match self.fat_type() {
            FATType::FAT12 | FATType::FAT16 | FATType::FAT32 => match self.ebr {
                EBR::FAT12_16(ebr_fat12_16) => {
                    ebr_fat12_16.boot_signature == BOOT_SIGNATURE
                        && ebr_fat12_16.signature == FAT_SIGNATURE
                }
                EBR::FAT32(ebr_fat32, _) => {
                    ebr_fat32.boot_signature == BOOT_SIGNATURE
                        && ebr_fat32.signature == FAT_SIGNATURE
                }
            },
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        }
    }

    #[inline]
    /// Total sectors in volume (including VBR)s
    pub(crate) fn total_sectors(&self) -> u32 {
        if self.bpb.total_sectors_16 == 0 {
            self.bpb.total_sectors_32
        } else {
            self.bpb.total_sectors_16 as u32
        }
    }

    #[inline]
    /// FAT size in sectors
    pub(crate) fn fat_sector_size(&self) -> u32 {
        match self.ebr {
            EBR::FAT12_16(_ebr_fat12_16) => self.bpb.table_size_16.into(),
            EBR::FAT32(ebr_fat32, _) => ebr_fat32.table_size_32,
        }
    }

    #[inline]
    /// The size of the root directory (unless we have FAT32, in which case the size will be 0)
    /// This calculation will round up
    pub(crate) fn root_dir_sectors(&self) -> u16 {
        // 32 is the size of a directory entry in bytes
        ((self.bpb.root_entry_count * 32) + (self.bpb.bytes_per_sector - 1))
            / self.bpb.bytes_per_sector
    }

    #[inline]
    /// The first sector in the File Allocation Table
    pub(crate) fn first_fat_sector(&self) -> u16 {
        self.bpb.reserved_sector_count
    }

    #[inline]
    /// The first sector of the root directory
    pub(crate) fn first_root_dir_sector(&self) -> u16 {
        self.first_fat_sector() + self.bpb.table_count as u16 * self.fat_sector_size() as u16
    }

    #[inline]
    /// The first data sector (that is, the first sector in which directories and files may be stored)
    pub(crate) fn first_data_sector(&self) -> u16 {
        self.first_root_dir_sector() + self.root_dir_sectors()
    }

    #[inline]
    /// The total number of data sectors
    pub(crate) fn total_data_sectors(&self) -> u32 {
        self.total_sectors() - (self.bpb.table_count as u32 * self.fat_sector_size())
            + self.root_dir_sectors() as u32
    }

    #[inline]
    /// The total number of clusters
    pub(crate) fn total_clusters(&self) -> u32 {
        self.total_data_sectors() / self.bpb.sectors_per_cluster as u32
    }

    #[inline]
    /// The FAT type of this file system
    pub(crate) fn fat_type(&self) -> FATType {
        if self.bpb.bytes_per_sector == 0 {
            todo!("ExFAT not yet implemented");
            FATType::ExFAT
        } else {
            let total_clusters = self.total_clusters();
            if total_clusters < 4085 {
                FATType::FAT12
            } else if total_clusters < 65525 {
                FATType::FAT16
            } else {
                FATType::FAT32
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
// Everything here is naturally aligned (thank god)
struct BootRecordExFAT {
    _dummy_jmp: [u8; 3],
    _oem_identifier: [u8; 8],
    _zeroed: [u8; 53],
    _partition_offset: u64,
    volume_len: u64,
    fat_offset: u32,
    fat_len: u32,
    cluster_heap_offset: u32,
    cluster_count: u32,
    root_dir_cluster: u32,
    partition_serial_num: u32,
    fs_revision: u16,
    flags: u16,
    sector_shift: u8,
    cluster_shift: u8,
    fat_count: u8,
    drive_select: u8,
    used_percentage: u8,
    _reserved: [u8; 7],
}

const EBR_SIZE: usize = 512 - BPBFAT_SIZE;
#[derive(Clone, Copy)]
enum EBR {
    FAT12_16(EBRFAT12_16),
    FAT32(EBRFAT32, FSInfoFAT32),
}

impl fmt::Debug for EBR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: find a good way of printing this
        write!(f, "FAT12-16/32 Extended boot record...")
    }
}

#[derive(Deserialize, Serialize, Clone, Copy)]
struct EBRFAT12_16 {
    _drive_num: u8,
    _windows_nt_flags: u8,
    boot_signature: u8,
    volume_serial_num: u32,
    volume_label: [u8; 11],
    _system_identifier: [u8; 8],
    #[serde(with = "BigArray")]
    _boot_code: [u8; 448],
    signature: u16,
}

// FIXME: these might be the other way around
#[derive(Deserialize, Serialize, Debug, Clone, Copy)]
struct FATVersion {
    minor: u8,
    major: u8,
}

#[derive(Deserialize, Serialize, Clone, Copy)]
struct EBRFAT32 {
    table_size_32: u32,
    _extended_flags: u16,
    fat_version: FATVersion,
    root_cluster: u32,
    fat_info: u16,
    backup_boot_sector: u16,
    _reserved: [u8; 12],
    _drive_num: u8,
    _windows_nt_flags: u8,
    boot_signature: u8,
    volume_serial_num: u32,
    volume_label: [u8; 11],
    _system_ident: [u8; 8],
    #[serde(with = "BigArray")]
    _boot_code: [u8; 420],
    signature: u16,
}

const FSINFO_LEAD_SIGNATURE: u32 = 0x41615252;
const FSINFO_MID_SIGNATURE: u32 = 0x61417272;
const FSINFO_TRAIL_SIGNAUTE: u32 = 0xAA550000;
#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct FSInfoFAT32 {
    lead_signature: u32,
    #[serde(with = "BigArray")]
    _reserved1: [u8; 480],
    mid_signature: u32,
    free_cluster_count: u32,
    first_free_cluster: u32,
    _reserved2: [u8; 12],
    trail_signature: u32,
}

impl FSInfoFAT32 {
    fn verify_signature(&self) -> bool {
        self.lead_signature == FSINFO_LEAD_SIGNATURE
            && self.mid_signature == FSINFO_MID_SIGNATURE
            && self.trail_signature == FSINFO_TRAIL_SIGNAUTE
    }
}

/// An enum representing different versions of the FAT filesystem
#[derive(Debug, Clone, Copy, PartialEq)]
// no need for enum variant documentation here
#[allow(missing_docs)]
pub enum FATType {
    FAT12,
    FAT16,
    FAT32,
    ExFAT,
}

impl FATType {
    #[inline]
    /// How many bits this [`FATType`] uses to address clusters in the disk
    pub fn bits_per_entry(&self) -> u8 {
        match self {
            FATType::FAT12 => 12,
            FATType::FAT16 => 16,
            FATType::FAT32 => 28,
            FATType::ExFAT => 32,
        }
    }

    #[inline]
    /// How many bytes this [`FATType`] spans across
    fn entry_size(&self) -> u32 {
        self.bits_per_entry().next_power_of_two() as u32 / 8
    }
}

#[derive(Debug, Clone, PartialEq)]
enum FATEntry {
    /// This cluster is free
    Free,
    /// This cluster is allocated and the next cluster is the contained value
    Allocated(u32),
    /// This cluster is reserved
    Reserved,
    /// This is a bad (defective) cluster
    Bad,
    /// This cluster is allocated and is the final cluster of the file
    EOF,
}

impl From<FATEntry> for u32 {
    fn from(value: FATEntry) -> Self {
        Self::from(&value)
    }
}

impl From<&FATEntry> for u32 {
    fn from(value: &FATEntry) -> Self {
        match value {
            FATEntry::Free => u32::MIN,
            FATEntry::Allocated(cluster) => *cluster,
            FATEntry::Reserved => 0xFFFFFF6,
            FATEntry::Bad => 0xFFFFFF7,
            FATEntry::EOF => u32::MAX,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct SFN {
    name: [u8; 8],
    ext: [u8; 3],
}

impl SFN {
    fn get_byte_slice(&self) -> [u8; 11] {
        let mut slice = [0; 11];

        slice[..8].copy_from_slice(&self.name);
        slice[8..].copy_from_slice(&self.ext);

        slice
    }

    fn gen_checksum(&self) -> u8 {
        let mut sum = 0;

        for c in self.get_byte_slice() {
            sum = (if (sum & 1) != 0 { 0x80_u8 } else { 0_u8 })
                .wrapping_add(sum >> 1)
                .wrapping_add(c)
        }

        log::debug!("SFN checksum: {:X}", sum);

        sum
    }
}

impl fmt::Display for SFN {
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

bitflags! {
    /// A list of the various (raw) attributes specified for a file/directory
    ///
    /// To check whether a given [`Attributes`] struct contains a flag, use the [`contains()`](Attributes::contains()) method
    ///
    /// Generated using [bitflags](https://docs.rs/bitflags/2.6.0/bitflags/)
    #[derive(Deserialize, Serialize, Debug, Clone, Copy, PartialEq)]
    struct RawAttributes: u8 {
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

const START_YEAR: i32 = 1980;

#[bitfield(u16)]
#[derive(Serialize, Deserialize)]
struct TimeAttribute {
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
struct DateAttribute {
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
            .map(|parsed| parsed.try_into().ok())
            .flatten()
            .ok_or(())
    }
}

impl TryFrom<DateAttribute> for Date {
    type Error = ();

    fn try_from(value: DateAttribute) -> Result<Self, Self::Error> {
        time::parsing::Parsed::new()
            .with_year(i32::from(value.year()) + START_YEAR)
            .and_then(|parsed| parsed.with_month(value.month().try_into().ok()?))
            .and_then(|parsed| parsed.with_day(num::NonZeroU8::new(value.day())?))
            .map(|parsed| parsed.try_into().ok())
            .flatten()
            .ok_or(())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct EntryCreationTime {
    hundredths_of_second: u8,
    time: TimeAttribute,
    date: DateAttribute,
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
struct EntryModificationTime {
    time: TimeAttribute,
    date: DateAttribute,
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

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct FATDirEntry {
    sfn: SFN,
    attributes: RawAttributes,
    _reserved: [u8; 1],
    created: EntryCreationTime,
    accessed: DateAttribute,
    cluster_high: u16,
    modified: EntryModificationTime,
    cluster_low: u16,
    file_size: u32,
}

#[derive(Debug, Deserialize, Serialize)]
struct LFNEntry {
    /// masked with 0x40 if this is the last entry
    order: u8,
    first_chars: [u8; 10],
    /// Always equals 0x0F
    _lfn_attribute: u8,
    /// Both OSDev and the FAT specification say this is always 0
    _long_entry_type: u8,
    /// If this doesn't match with the computed cksum, then the set of LFNs is considered corrupt
    ///
    /// A [`LFNEntry`] will be marked as corrupt even if it isn't, if the SFN is modifed by a legacy system,
    /// since the new SFN's signature and the one on this field won't (probably) match
    checksum: u8,
    mid_chars: [u8; 12],
    _zeroed: [u8; 2],
    last_chars: [u8; 4],
}

impl LFNEntry {
    fn get_byte_slice(&self) -> [u16; 13] {
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
    fn verify_signature(&self) -> bool {
        self._long_entry_type == 0 && self._zeroed.iter().all(|v| *v == 0)
    }
}

/// A resolved file/directory entry (for internal usage only)
#[derive(Debug)]
struct RawProperties {
    name: String,
    is_dir: bool,
    attributes: RawAttributes,
    created: PrimitiveDateTime,
    modified: PrimitiveDateTime,
    accessed: Date,
    file_size: u32,
    data_cluster: u32,
}

/// A container for file/directory properties
#[derive(Debug)]
pub struct Properties {
    path: PathBuf,
    attributes: Attributes,
    created: PrimitiveDateTime,
    modified: PrimitiveDateTime,
    accessed: Date,
    file_size: u32,
    data_cluster: u32,
}

/// Getter methods
impl Properties {
    #[inline]
    /// Get the corresponding [`PathBuf`] to this entry
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    #[inline]
    /// Get the corresponding [`Attributes`] to this entry
    pub fn attributes(&self) -> &Attributes {
        &self.attributes
    }

    #[inline]
    /// Find out when this entry was created (max resolution: 1ms)
    ///
    /// Returns a [`PrimitiveDateTime`] from the [`time`] crate
    pub fn creation_time(&self) -> &PrimitiveDateTime {
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
    /// Returns a [`Date`] from the [`time`] crate
    pub fn last_accessed_date(&self) -> &Date {
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

/// Serialization methods
impl Properties {
    #[inline]
    fn from_raw(raw: RawProperties, path: PathBuf) -> Self {
        Properties {
            path,
            attributes: raw.attributes.into(),
            created: raw.created,
            modified: raw.modified,
            accessed: raw.accessed,
            file_size: raw.file_size,
            data_cluster: raw.data_cluster,
        }
    }
}

/// A thin wrapper for [`Properties`] represing a directory entry
#[derive(Debug)]
pub struct DirEntry {
    entry: Properties,
}

impl ops::Deref for DirEntry {
    type Target = Properties;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.entry
    }
}

#[derive(Debug)]
struct FileProps {
    entry: Properties,
    /// the byte offset of the R/W pointer
    offset: u64,
    current_cluster: u32,
}

/// A read-only file within a FAT filesystem
#[derive(Debug)]
pub struct ROFile<'a, S>
where
    S: Read + Write + Seek,
{
    fs: &'a mut FileSystem<S>,
    props: FileProps,
}

impl<S> ops::Deref for ROFile<'_, S>
where
    S: Read + Write + Seek,
{
    type Target = Properties;

    fn deref(&self) -> &Self::Target {
        &self.props.entry
    }
}

impl<S> ops::DerefMut for ROFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.props.entry
    }
}

impl<S> IOBase for ROFile<'_, S>
where
    S: Read + Write + Seek,
{
    type Error = S::Error;
}

/// A read-write file within a FAT filesystem
///
/// The size of the file will be automatically adjusted
/// if the cursor goes beyond EOF.
///
/// To reduce a file's size, use the [`truncate`](RWFile::truncate) method
#[derive(Debug)]
pub struct RWFile<'a, S>
where
    S: Read + Write + Seek,
{
    ro_file: ROFile<'a, S>,
}

impl<'a, S> ops::Deref for RWFile<'a, S>
where
    S: Read + Write + Seek,
{
    type Target = ROFile<'a, S>;

    fn deref(&self) -> &Self::Target {
        &self.ro_file
    }
}

impl<S> ops::DerefMut for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ro_file
    }
}

impl<S> IOBase for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    type Error = S::Error;
}

// Public functions
impl<S> RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    /// Truncates the file to a given size, deleting everything past the new EOF
    ///
    /// If `size` is greater or equal to the current file size
    /// till the end of the last cluster allocated, this has no effect
    ///
    /// Furthermore, if the cursor point is beyond the new EOF, it will be moved there
    pub fn truncate(&mut self, size: u32) -> Result<(), <Self as IOBase>::Error> {
        // looks like the new truncated size would be smaller than the current one, so we just return
        if size.next_multiple_of(self.fs.props.cluster_size as u32) >= self.file_size {
            return Ok(());
        }

        // we store the current offset for later use
        let previous_offset = cmp::min(self.props.offset, size.into());

        // we seek back to where the EOF will be
        self.seek(SeekFrom::Start(size.into()))?;

        // set what the new filesize will be
        let previous_size = self.file_size;
        self.file_size = size;

        let mut next_cluster_option = self
            .ro_file
            .fs
            .get_next_cluster(self.ro_file.props.current_cluster)?;

        // we set the new last cluster in the chain to be EOF
        self.ro_file
            .fs
            .write_nth_FAT_entry(self.ro_file.props.current_cluster, FATEntry::EOF)?;

        // then, we set each cluster after the current one to EOF
        while let Some(next_cluster) = next_cluster_option {
            next_cluster_option = self.fs.get_next_cluster(next_cluster)?;

            self.fs.write_nth_FAT_entry(next_cluster, FATEntry::Free)?;
        }

        // don't forget to seek back to where we started
        self.seek(SeekFrom::Start(previous_offset))?;

        log::debug!(
            "Successfully truncated file {} from {} to {} bytes",
            self.path,
            previous_size,
            self.file_size
        );

        Ok(())
    }
}

// Internal functions
impl<S> ROFile<'_, S>
where
    S: Read + Write + Seek,
{
    #[inline]
    /// Panics if the current cluser doesn't point to another clluster
    fn next_cluster(&mut self) -> Result<(), <Self as IOBase>::Error> {
        // when a `ROFile` is created, `cluster_chain_is_healthy` is called, if it fails, that ROFile is dropped
        self.props.current_cluster = self
            .fs
            .get_next_cluster(self.props.current_cluster)?
            .unwrap();

        Ok(())
    }

    /// Returns that last cluster in the file's cluster chain
    fn last_cluster_in_chain(&mut self) -> Result<u32, <Self as IOBase>::Error> {
        // we begin from the current cluster to save some time
        let mut current_cluster = self.props.current_cluster;

        loop {
            match self.fs.read_nth_FAT_entry(current_cluster)? {
                FATEntry::Allocated(next_cluster) => current_cluster = next_cluster,
                FATEntry::EOF => break,
                _ => unreachable!(),
            }
        }

        Ok(current_cluster)
    }

    /// Checks whether the cluster chain of this file is healthy or malformed
    fn cluster_chain_is_healthy(&mut self) -> Result<bool, S::Error> {
        let mut current_cluster = self.data_cluster;
        let mut cluster_count = 0;

        loop {
            cluster_count += 1;

            if cluster_count * self.fs.cluster_size() >= self.file_size.into() {
                break;
            }

            match self.fs.read_nth_FAT_entry(current_cluster)? {
                FATEntry::Allocated(next_cluster) => current_cluster = next_cluster,
                _ => return Ok(false),
            };
        }

        Ok(true)
    }

    fn offset_from_seekfrom(&self, seekfrom: SeekFrom) -> u64 {
        match seekfrom {
            SeekFrom::Start(offset) => offset,
            SeekFrom::Current(offset) => {
                let offset = self.props.offset as i64 + offset;
                offset.try_into().unwrap_or(u64::MIN)
            }
            SeekFrom::End(offset) => {
                let offset = self.file_size as i64 + offset;
                offset.try_into().unwrap_or(u64::MIN)
            }
        }
    }
}

impl<S> Read for ROFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        let mut bytes_read = 0;
        // this is the maximum amount of bytes that can be read
        let read_cap = cmp::min(
            buf.len(),
            self.file_size as usize - self.props.offset as usize,
        );

        'outer: loop {
            let sector_init_offset = u32::try_from(self.props.offset % self.fs.cluster_size())
                .unwrap()
                / self.fs.sector_size();
            let first_sector_of_cluster = self
                .fs
                .data_cluster_to_partition_sector(self.props.current_cluster)
                + sector_init_offset;
            let last_sector_of_cluster = first_sector_of_cluster
                + self.fs.sectors_per_cluster() as u32
                - sector_init_offset
                - 1;
            log::debug!(
                "Reading cluster {} from sectors {} to {}",
                self.props.current_cluster,
                first_sector_of_cluster,
                last_sector_of_cluster
            );

            for sector in first_sector_of_cluster..=last_sector_of_cluster {
                self.fs.read_nth_sector(sector.into())?;

                let start_index = self.props.offset as usize % self.fs.sector_size() as usize;
                let bytes_to_read = cmp::min(
                    read_cap - bytes_read,
                    self.fs.sector_size() as usize - start_index,
                );
                log::debug!(
                    "Gonna read {} bytes from sector {} starting at byte {}",
                    bytes_to_read,
                    sector,
                    start_index
                );

                buf[bytes_read..bytes_read + bytes_to_read].copy_from_slice(
                    &self.fs.sector_buffer[start_index..start_index + bytes_to_read],
                );

                bytes_read += bytes_to_read;
                self.props.offset += bytes_to_read as u64;

                // if we have read as many bytes as we want...
                if bytes_read >= read_cap {
                    // ...but we must process get the next cluster for future uses,
                    // we do that before breaking
                    if self.props.offset % self.fs.cluster_size() == 0
                        && self.props.offset < self.file_size.into()
                    {
                        self.next_cluster()?;
                    }

                    break 'outer;
                }
            }

            self.next_cluster()?;
        }

        Ok(bytes_read)
    }

    // the default `read_to_end` implementation isn't efficient enough, so we just do this
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, Self::Error> {
        let bytes_to_read = self.file_size as usize - self.props.offset as usize;
        let init_buf_len = buf.len();

        // resize buffer to fit the file contents exactly
        buf.resize(init_buf_len + bytes_to_read, 0);

        // this is guaranteed not to raise an EOF (although other error kinds might be raised...)
        self.read_exact(&mut buf[init_buf_len..])?;

        Ok(bytes_to_read)
    }
}
impl<S> Read for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        self.ro_file.read(buf)
    }

    #[inline]
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), Self::Error> {
        self.ro_file.read_exact(buf)
    }

    #[inline]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, Self::Error> {
        self.ro_file.read_to_end(buf)
    }

    #[inline]
    fn read_to_string(&mut self, string: &mut String) -> Result<usize, Self::Error> {
        self.ro_file.read_to_string(string)
    }
}

impl<S> Write for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        // allocate clusters
        self.seek(SeekFrom::Current(buf.len() as i64))?;
        // rewind back to where we were
        self.seek(SeekFrom::Current(-(buf.len() as i64)))?;

        let mut bytes_written = 0;

        'outer: loop {
            log::trace!(
                "writing file data to cluster: {}",
                self.props.current_cluster
            );

            let sector_init_offset = u32::try_from(self.props.offset % self.fs.cluster_size())
                .unwrap()
                / self.fs.sector_size();
            let first_sector_of_cluster = self
                .fs
                .data_cluster_to_partition_sector(self.props.current_cluster)
                + sector_init_offset;
            let last_sector_of_cluster = first_sector_of_cluster
                + self.fs.sectors_per_cluster() as u32
                - sector_init_offset
                - 1;
            for sector in first_sector_of_cluster..=last_sector_of_cluster {
                self.fs.read_nth_sector(sector.into())?;

                let start_index = self.props.offset as usize % self.fs.sector_size() as usize;

                let bytes_to_write = cmp::min(
                    buf.len() - bytes_written,
                    self.fs.sector_size() as usize - start_index,
                );

                self.fs.sector_buffer[start_index..start_index + bytes_to_write]
                    .copy_from_slice(&buf[bytes_written..bytes_written + bytes_to_write]);
                self.fs.buffer_modified = true;

                bytes_written += bytes_to_write;
                self.props.offset += bytes_to_write as u64;

                // if we have written as many bytes as we want...
                if bytes_written >= buf.len() {
                    // ...but we must process get the next cluster for future uses,
                    // we do that before breaking
                    if self.props.offset % self.fs.cluster_size() == 0 {
                        self.next_cluster()?;
                    }

                    break 'outer;
                }
            }

            self.next_cluster()?;
        }

        Ok(bytes_written)
    }

    // everything is immediately written to the storage medium
    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<S> Seek for ROFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, Self::Error> {
        let offset = self.offset_from_seekfrom(pos);

        // in case the cursor goes beyond the EOF, allocate more clusters
        if offset > (self.file_size as u64).next_multiple_of(self.fs.cluster_size()) {
            return Err(IOError::new(
                <Self::Error as IOError>::Kind::new_unexpected_eof(),
                "moved past eof in a RO file",
            ));
        }

        log::trace!(
            "Previous cursor offset is {}, new cursor offset is {}",
            self.props.offset,
            offset
        );

        use cmp::Ordering;
        match offset.cmp(&self.props.offset) {
            Ordering::Less => {
                // here, we basically "rewind" back to the start of the file and then seek to where we want
                // this of course has performance issues, so TODO: find a solution that is both memory & time efficient
                // (perhaps we could follow a similar approach to elm-chan's FATFS, by using a cluster link map table, perhaps as an optional feature)
                self.props.offset = 0;
                self.props.current_cluster = self.data_cluster;
                self.seek(SeekFrom::Start(offset))?;
            }
            Ordering::Equal => (),
            Ordering::Greater => {
                for _ in self.props.offset / self.fs.cluster_size()..offset / self.fs.cluster_size()
                {
                    self.next_cluster()?;
                }
                self.props.offset = offset;
            }
        }

        Ok(self.props.offset)
    }
}

impl<S> Seek for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, Self::Error> {
        let offset = self.offset_from_seekfrom(pos);

        // in case the cursor goes beyond the EOF, allocate more clusters
        if offset > (self.file_size as u64).next_multiple_of(self.fs.cluster_size()) {
            let clusters_to_allocate = (offset
                - (self.file_size as u64).next_multiple_of(self.fs.cluster_size()))
            .div_ceil(self.fs.cluster_size())
                + 1;
            log::debug!(
                "Seeking beyond EOF, allocating {} more clusters",
                clusters_to_allocate
            );

            let mut last_cluster_in_chain = self.last_cluster_in_chain()?;

            for clusters_allocated in 0..clusters_to_allocate {
                match self.fs.next_free_cluster()? {
                    Some(next_free_cluster) => {
                        // we set the last allocated cluster to point to the next free one
                        self.fs.write_nth_FAT_entry(
                            last_cluster_in_chain,
                            FATEntry::Allocated(next_free_cluster),
                        )?;
                        // we also set the next free cluster to be EOF
                        self.fs
                            .write_nth_FAT_entry(next_free_cluster, FATEntry::EOF)?;
                        log::trace!(
                            "cluster {} now points to {}",
                            last_cluster_in_chain,
                            next_free_cluster
                        );
                        // now the next free cluster i the last allocated one
                        last_cluster_in_chain = next_free_cluster;
                    }
                    None => {
                        self.file_size = (((self.file_size as u64)
                            .next_multiple_of(self.fs.cluster_size())
                            - offset)
                            + clusters_allocated * self.fs.cluster_size())
                            as u32;
                        self.props.offset = self.file_size.into();

                        log::error!("storage medium full while attempting to allocate more clusters for a ROFile");
                        return Err(IOError::new(
                            <Self::Error as IOError>::Kind::new_unexpected_eof(),
                            "the storage medium is full, can't increase size of file",
                        ));
                    }
                }
            }

            self.file_size = offset as u32;
            log::debug!(
                "New file size after reallocation is {} bytes",
                self.file_size
            );
        }

        self.ro_file.seek(pos)
    }
}

/// variation of https://stackoverflow.com/a/42067321/19247098 for processing LFNs
pub(crate) fn string_from_lfn(utf16_src: &[u16]) -> Result<String, FromUtf16Error> {
    let nul_range_end = utf16_src
        .iter()
        .position(|c| *c == 0x0000)
        .unwrap_or(utf16_src.len()); // default to length if no `\0` present

    String::from_utf16(&utf16_src[0..nul_range_end])
}

trait OffsetConversions {
    fn sector_size(&self) -> u32;
    fn cluster_size(&self) -> u64;
    fn first_data_sector(&self) -> u32;

    #[inline]
    fn cluster_to_sector(&self, cluster: u64) -> u32 {
        (cluster * self.cluster_size() / self.sector_size() as u64)
            .try_into()
            .unwrap()
    }

    #[inline]
    fn sectors_per_cluster(&self) -> u64 {
        self.cluster_size() / self.sector_size() as u64
    }

    // this function assumes that the first sector is also the first sector of the partition
    #[inline]
    fn sector_to_partition_offset(&self, sector: u32) -> u32 {
        sector * self.sector_size()
    }

    // these three functions assume that the first sector (or cluster) is the first sector (or cluster) of the data area
    #[inline]
    fn data_cluster_to_partition_sector(&self, cluster: u32) -> u32 {
        self.cluster_to_sector((cluster - 2).into()) + self.first_data_sector()
    }
}

/// Some generic properties common across all FAT versions, like a sector's size, are cached here
#[derive(Debug)]
struct FSProperties {
    sector_size: u32,
    cluster_size: u64,
    total_sectors: u32,
    total_clusters: u32,
    /// sector offset of the FAT
    fat_table_count: u8,
    first_data_sector: u32,
}

#[inline]
// an easy way to universally use the same bincode (de)serialization options
fn bincode_config() -> impl bincode::Options + Copy {
    // also check https://docs.rs/bincode/1.3.3/bincode/config/index.html#options-struct-vs-bincode-functions
    bincode::DefaultOptions::new()
        .with_fixint_encoding()
        .allow_trailing_bytes()
        .with_little_endian()
}

/// Filter (or not) things like hidden files/directories
/// for FileSystem operations
#[derive(Debug)]
struct FileFilter {
    show_hidden: bool,
    show_systen: bool,
}

impl FileFilter {
    fn filter(&self, item: &RawProperties) -> bool {
        let is_hidden = item.attributes.contains(RawAttributes::HIDDEN);
        let is_system = item.attributes.contains(RawAttributes::SYSTEM);
        let should_filter = !self.show_hidden && is_hidden || !self.show_systen && is_system;

        !should_filter
    }
}

impl Default for FileFilter {
    fn default() -> Self {
        // The FAT spec says to filter everything by default
        FileFilter {
            show_hidden: false,
            show_systen: false,
        }
    }
}

/// An API to process a FAT filesystem
#[derive(Debug)]
pub struct FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// Any struct that implements the [`Read`], [`Write`] & [`Seek`] traits
    storage: S,

    /// The length of this will be the sector size of the FS for all FAT types except FAT12, in that case, it will be double that value
    sector_buffer: Vec<u8>,
    /// ANY CHANGES TO THE SECTOR BUFFER SHOULD ALSO SET THIS TO TRUE
    buffer_modified: bool,
    stored_sector: u64,

    boot_record: BootRecord,
    // since `self.fat_type()` calls like 5 nested functions, we keep this cached and expose it as a public field
    fat_type: FATType,
    props: FSProperties,

    filter: FileFilter,
}

impl<S> OffsetConversions for FileSystem<S>
where
    S: Read + Write + Seek,
{
    #[inline]
    fn sector_size(&self) -> u32 {
        self.props.sector_size
    }

    #[inline]
    fn cluster_size(&self) -> u64 {
        self.props.cluster_size
    }

    #[inline]
    fn first_data_sector(&self) -> u32 {
        self.props.first_data_sector
    }
}

/// Getter functions
impl<S> FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// What is the [`FATType`] of the filesystem
    pub fn fat_type(&self) -> FATType {
        self.fat_type
    }
}

/// Setter functions
impl<S> FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// Whether or not to list hidden files
    ///
    /// Off by default
    #[inline]
    pub fn show_hidden(&mut self, show: bool) {
        self.filter.show_hidden = show;
    }

    /// Whether or not to list system files
    ///
    /// Off by default
    #[inline]
    pub fn show_system(&mut self, show: bool) {
        self.filter.show_systen = show;
    }
}

/// Constructors
impl<S> FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// Create a [`FileSystem`] from a storage object that implements [`Read`], [`Write`] & [`Seek`]
    ///
    /// Fails if the storage is way too small to support a FAT filesystem.
    /// For most use cases, that shouldn't be an issue, you can just call [`.unwrap()`](Result::unwrap)
    pub fn from_storage(mut storage: S) -> FSResult<Self, S::Error> {
        // Begin by reading the boot record
        // We don't know the sector size yet, so we just go with the biggest possible one for now
        let mut buffer = [0u8; SECTOR_SIZE_MAX];

        let bytes_read = storage.read(&mut buffer)?;
        let mut stored_sector = 0;

        if bytes_read < 512 {
            return Err(FSError::InternalFSError(InternalFSError::StorageTooSmall));
        }

        let bpb: BPBFAT = bincode_config().deserialize(&buffer[..BPBFAT_SIZE])?;

        let ebr = if bpb.table_size_16 == 0 {
            let ebr_fat32 = bincode_config()
                .deserialize::<EBRFAT32>(&buffer[BPBFAT_SIZE..BPBFAT_SIZE + EBR_SIZE])?;

            storage.seek(SeekFrom::Start(
                ebr_fat32.fat_info as u64 * bpb.bytes_per_sector as u64,
            ))?;
            stored_sector = ebr_fat32.fat_info.into();
            storage.read_exact(&mut buffer[..bpb.bytes_per_sector as usize])?;
            let fsinfo = bincode_config()
                .deserialize::<FSInfoFAT32>(&buffer[..bpb.bytes_per_sector as usize])?;

            if !fsinfo.verify_signature() {
                log::error!("FAT32 FSInfo has invalid signature(s)");
                return Err(FSError::InternalFSError(InternalFSError::InvalidFSInfoSig));
            }

            EBR::FAT32(ebr_fat32, fsinfo)
        } else {
            EBR::FAT12_16(
                bincode_config()
                    .deserialize::<EBRFAT12_16>(&buffer[BPBFAT_SIZE..BPBFAT_SIZE + EBR_SIZE])?,
            )
        };

        // TODO: see how we will handle this for exfat
        let boot_record = BootRecord::FAT(BootRecordFAT { bpb, ebr });

        // verify boot record signature
        let fat_type = boot_record.fat_type();
        log::info!("The FAT type of the filesystem is {:?}", fat_type);

        match boot_record {
            BootRecord::FAT(boot_record_fat) => {
                if boot_record_fat.verify_signature() {
                    log::error!("FAT boot record has invalid signature(s)");
                    return Err(FSError::InternalFSError(InternalFSError::InvalidBPBSig));
                }
            }
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };

        let sector_size: u32 = match boot_record {
            BootRecord::FAT(boot_record_fat) => boot_record_fat.bpb.bytes_per_sector.into(),
            BootRecord::ExFAT(boot_record_exfat) => 1 << boot_record_exfat.sector_shift,
        };
        let cluster_size: u64 = match boot_record {
            BootRecord::FAT(boot_record_fat) => {
                (boot_record_fat.bpb.sectors_per_cluster as u32 * sector_size).into()
            }
            BootRecord::ExFAT(boot_record_exfat) => {
                1 << (boot_record_exfat.sector_shift + boot_record_exfat.cluster_shift)
            }
        };

        let first_data_sector = match boot_record {
            BootRecord::FAT(boot_record_fat) => boot_record_fat.first_data_sector().into(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };

        let fat_table_count = match boot_record {
            BootRecord::FAT(boot_record_fat) => boot_record_fat.bpb.table_count,
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };

        let total_sectors = match boot_record {
            BootRecord::FAT(boot_record_fat) => boot_record_fat.total_sectors(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };

        let total_clusters = match boot_record {
            BootRecord::FAT(boot_record_fat) => boot_record_fat.total_clusters(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };

        let props = FSProperties {
            sector_size,
            cluster_size,
            fat_table_count,
            total_sectors,
            total_clusters,
            first_data_sector,
        };

        let mut fs = Self {
            storage,
            sector_buffer: buffer[..sector_size as usize].to_vec(),
            buffer_modified: false,
            stored_sector,
            boot_record,
            fat_type,
            props,
            filter: FileFilter::default(),
        };

        if !fs.FAT_tables_are_identical()? {
            return Err(FSError::InternalFSError(
                InternalFSError::MismatchingFATTables,
            ));
        }

        Ok(fs)
    }
}

/// Internal [`Read`]-related low-level functions
impl<S> FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// Unsafe because the sector number must point to an area with directory entries
    ///
    /// Also the sector number starts from the beginning of the partition
    unsafe fn process_entry_sector(
        &mut self,
        sector: u32,
    ) -> FSResult<Vec<RawProperties>, S::Error> {
        let mut entries = Vec::new();
        let mut lfn_buf: Vec<String> = Vec::new();
        let mut lfn_checksum: Option<u8> = None;

        'outer: loop {
            for chunk in self.read_nth_sector(sector.into())?.chunks(32) {
                match chunk[0] {
                    // nothing else to read
                    0 => break 'outer,
                    // unused entry
                    0xE5 => continue,
                    _ => (),
                };

                let Ok(entry) = bincode_config().deserialize::<FATDirEntry>(&chunk) else {
                    continue;
                };

                if entry.attributes.contains(RawAttributes::LFN) {
                    // TODO: perhaps there is a way to utilize the `order` field?
                    let Ok(lfn_entry) = bincode_config().deserialize::<LFNEntry>(&chunk) else {
                        continue;
                    };

                    // If the signature verification fails, consider this entry corrupted
                    if !lfn_entry.verify_signature() {
                        continue;
                    }

                    match lfn_checksum {
                        Some(checksum) => {
                            if checksum != lfn_entry.checksum {
                                lfn_checksum = None;
                                lfn_buf.clear();
                                continue;
                            }
                        }
                        None => lfn_checksum = Some(lfn_entry.checksum),
                    }

                    let char_arr = lfn_entry.get_byte_slice().to_vec();
                    if let Ok(temp_str) = string_from_lfn(&char_arr) {
                        lfn_buf.push(temp_str);
                    }

                    continue;
                }

                let filename = if !lfn_buf.is_empty()
                    && lfn_checksum.is_some_and(|checksum| checksum == entry.sfn.gen_checksum())
                {
                    // for efficiency reasons, we store the LFN string sequences as we read them
                    let parsed_str: String = lfn_buf.iter().cloned().rev().collect();
                    lfn_buf.clear();
                    lfn_checksum = None;
                    parsed_str
                } else {
                    entry.sfn.to_string()
                };

                if let (Ok(created), Ok(modified), Ok(accessed)) = (
                    entry.created.try_into(),
                    entry.modified.try_into(),
                    entry.accessed.try_into(),
                ) {
                    entries.push(RawProperties {
                        name: filename,
                        is_dir: entry.attributes.contains(RawAttributes::DIRECTORY),
                        attributes: entry.attributes,
                        created,
                        modified,
                        accessed,
                        file_size: entry.file_size,
                        data_cluster: ((entry.cluster_high as u32) << 16)
                            + entry.cluster_low as u32,
                    })
                }
            }
        }

        Ok(entries)
    }

    fn process_root_dir(&mut self) -> FSResult<Vec<RawProperties>, S::Error> {
        match self.boot_record {
            BootRecord::FAT(boot_record_fat) => match boot_record_fat.ebr {
                EBR::FAT12_16(_ebr_fat12_16) => {
                    let mut entries = Vec::new();

                    let root_dir_sector = boot_record_fat.first_root_dir_sector();
                    let sector_count = boot_record_fat.root_dir_sectors();

                    for sector in root_dir_sector..(root_dir_sector + sector_count) {
                        let mut new_entries = unsafe { self.process_entry_sector(sector.into())? };
                        entries.append(&mut new_entries);
                    }

                    Ok(entries)
                }
                EBR::FAT32(ebr_fat32, _) => {
                    let cluster = ebr_fat32.root_cluster;
                    unsafe { self.process_normal_dir(cluster) }
                }
            },
            BootRecord::ExFAT(_boot_record_exfat) => todo!(),
        }
    }

    /// Unsafe for the same reason as [`process_entry_sector`]
    unsafe fn process_normal_dir(
        &mut self,
        mut data_cluster: u32,
    ) -> FSResult<Vec<RawProperties>, S::Error> {
        let mut entries = Vec::new();

        loop {
            // FAT specification, section 6.7
            let first_sector_of_cluster = self.data_cluster_to_partition_sector(data_cluster);
            for sector in first_sector_of_cluster
                ..(first_sector_of_cluster + self.sectors_per_cluster() as u32)
            {
                let mut new_entries = unsafe { self.process_entry_sector(sector.into())? };
                entries.append(&mut new_entries);
            }

            // Read corresponding FAT entry
            let current_fat_entry = self.read_nth_FAT_entry(data_cluster)?;

            match current_fat_entry {
                // we are done here, break the loop
                FATEntry::EOF => break,
                // this cluster chain goes on, follow it
                FATEntry::Allocated(next_cluster) => data_cluster = next_cluster,
                // any other case (whether a bad, reserved or free cluster) is invalid, consider this cluster chain malformed
                _ => {
                    log::error!("Cluster chain of directory is malformed");
                    return Err(FSError::InternalFSError(
                        InternalFSError::MalformedClusterChain,
                    ));
                }
            }
        }

        Ok(entries)
    }

    /// Gets the next free cluster. Returns an IO [`Result`]
    /// If the [`Result`] returns [`Ok`] that contains a [`None`], the drive is full
    fn next_free_cluster(&mut self) -> Result<Option<u32>, S::Error> {
        let start_cluster = match self.boot_record {
            BootRecord::FAT(boot_record_fat) => {
                // the first 2 entries are reserved
                let mut first_free_cluster = 2;

                if let EBR::FAT32(_, fsinfo) = boot_record_fat.ebr {
                    // a value of u32::MAX denotes unawareness of the first free cluster
                    // we also do a bit of range checking
                    // TODO: if this is unknown, figure it out and write it to the FSInfo structure
                    if fsinfo.first_free_cluster != u32::MAX
                        && fsinfo.first_free_cluster <= self.props.total_sectors
                    {
                        first_free_cluster = fsinfo.first_free_cluster
                    }
                }

                first_free_cluster
            }
            BootRecord::ExFAT(_) => todo!("ExFAT not yet implemented"),
        };

        let mut current_cluster = start_cluster;

        while current_cluster < self.props.total_clusters {
            match self.read_nth_FAT_entry(current_cluster)? {
                FATEntry::Free => return Ok(Some(current_cluster)),
                _ => (),
            }
            current_cluster += 1;
        }

        Ok(None)
    }

    /// Get the next cluster in a cluster chain, otherwise return [`None`]
    fn get_next_cluster(&mut self, cluster: u32) -> Result<Option<u32>, S::Error> {
        Ok(match self.read_nth_FAT_entry(cluster)? {
            FATEntry::Allocated(next_cluster) => Some(next_cluster),
            // when a `ROFile` is created, `cluster_chain_is_healthy` is called, if it fails, that ROFile is dropped
            _ => None,
        })
    }

    #[allow(non_snake_case)]
    /// Check whether or not the all the FAT tables of the storage medium are identical to each other
    fn FAT_tables_are_identical(&mut self) -> Result<bool, S::Error> {
        // we could make it work, but we are only testing regular FAT filesystems (for now)
        assert_ne!(
            self.fat_type,
            FATType::ExFAT,
            "this function doesn't work with ExFAT"
        );

        /// How many bytes to probe at max for each FAT per iteration (must be a multiple of [`SECTOR_SIZE_MAX`])
        const MAX_PROBE_SIZE: u32 = 1 << 20;

        let fat_byte_size = match self.boot_record {
            BootRecord::FAT(boot_record_fat) => boot_record_fat.fat_sector_size(),
            BootRecord::ExFAT(_) => unreachable!(),
        };

        for nth_iteration in 0..fat_byte_size.div_ceil(MAX_PROBE_SIZE) {
            let mut tables: Vec<Vec<u8>> = Vec::new();

            for i in 0..self.props.fat_table_count {
                let fat_start =
                    self.sector_to_partition_offset(self.boot_record.nth_FAT_table_sector(i));
                let current_offset = fat_start + nth_iteration * MAX_PROBE_SIZE;
                let bytes_left = fat_byte_size - nth_iteration * MAX_PROBE_SIZE;

                self.storage.seek(SeekFrom::Start(current_offset.into()))?;
                let mut buf = vec![0_u8; cmp::min(MAX_PROBE_SIZE, bytes_left) as usize];
                self.storage.read_exact(buf.as_mut_slice())?;
                tables.push(buf);
            }

            // we check each table with the first one (except the first one ofc)
            if !tables.iter().skip(1).all(|buf| buf == &tables[0]) {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Read the nth sector from the partition's beginning and store it in [`self.sector_buffer`](Self::sector_buffer)
    ///
    /// This function also returns an immutable reference to [`self.sector_buffer`](Self::sector_buffer)
    fn read_nth_sector(&mut self, n: u64) -> Result<&Vec<u8>, S::Error> {
        // nothing to do if the sector we wanna read is already cached
        if n != self.stored_sector {
            // let's sync the current sector first
            self.sync_sector_buffer()?;
            self.storage.seek(SeekFrom::Start(
                self.sector_to_partition_offset(n as u32).into(),
            ))?;
            self.storage.read_exact(&mut self.sector_buffer)?;
            self.storage
                .seek(SeekFrom::Current(-i64::from(self.props.sector_size)))?;

            self.stored_sector = n;
        }

        Ok(&self.sector_buffer)
    }

    #[allow(non_snake_case)]
    fn read_nth_FAT_entry(&mut self, n: u32) -> Result<FATEntry, S::Error> {
        // the size of an entry rounded up to bytes
        let entry_size = self.fat_type.entry_size();
        let entry_props = FATEntryProps::new(n, &self);

        self.read_nth_sector(entry_props.fat_sectors[0].into())?;

        let mut value_bytes = [0_u8; 4];
        let bytes_to_read: usize = cmp::min(
            entry_props.sector_offset + entry_size as usize,
            self.sector_size() as usize,
        ) - entry_props.sector_offset;
        value_bytes[..bytes_to_read].copy_from_slice(
            &self.sector_buffer
                [entry_props.sector_offset..entry_props.sector_offset + bytes_to_read],
        ); // this shouldn't panic

        // in FAT12, FAT entries may be split between two different sectors
        if self.fat_type == FATType::FAT12 && (bytes_to_read as u32) < entry_size {
            self.read_nth_sector((entry_props.fat_sectors[0] + 1).into())?;

            value_bytes[bytes_to_read..entry_size as usize]
                .copy_from_slice(&self.sector_buffer[..(entry_size as usize - bytes_to_read)]);
        };

        let mut value = u32::from_le_bytes(value_bytes);
        match self.fat_type {
            // FAT12 entries are split between different bytes
            FATType::FAT12 => {
                if n & 1 != 0 {
                    value >>= 4
                } else {
                    value &= 0xFFF
                }
            }
            // ignore the high 4 bits if this is FAT32
            FATType::FAT32 => value &= 0x0FFFFFFF,
            _ => (),
        }

        /*
        // pad unused bytes with 1s
        let padding: u32 = u32::MAX.to_be() << self.fat_type.bits_per_entry();
        value |= padding.to_le();
        */

        // TODO: perhaps byte padding can replace some redundant code here?
        Ok(match self.fat_type {
            FATType::FAT12 => match value {
                0x000 => FATEntry::Free,
                0xFF7 => FATEntry::Bad,
                0xFF8..=0xFFE | 0xFFF => FATEntry::EOF,
                _ => {
                    if (0x002..(self.props.total_clusters + 1)).contains(&value.into()) {
                        FATEntry::Allocated(value.into())
                    } else {
                        FATEntry::Reserved
                    }
                }
            },
            FATType::FAT16 => match value {
                0x0000 => FATEntry::Free,
                0xFFF7 => FATEntry::Bad,
                0xFFF8..=0xFFFE | 0xFFFF => FATEntry::EOF,
                _ => {
                    if (0x0002..(self.props.total_clusters + 1)).contains(&value.into()) {
                        FATEntry::Allocated(value.into())
                    } else {
                        FATEntry::Reserved
                    }
                }
            },
            FATType::FAT32 => match value {
                0x00000000 => FATEntry::Free,
                0xFFFFFFF7 => FATEntry::Bad,
                0xFFFFFFF8..=0xFFFFFFFE | 0xFFFFFFFF => FATEntry::EOF,
                _ => {
                    if (0x00000002..(self.props.total_clusters + 1)).contains(&value.into()) {
                        FATEntry::Allocated(value.into())
                    } else {
                        FATEntry::Reserved
                    }
                }
            },
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        })
    }
}

/// Internal [`Write`]-related low-level functions
impl<S> FileSystem<S>
where
    S: Read + Write + Seek,
{
    #[allow(non_snake_case)]
    fn write_nth_FAT_entry(&mut self, n: u32, entry: FATEntry) -> Result<(), S::Error> {
        // the size of an entry rounded up to bytes
        let entry_size = self.fat_type.entry_size();
        let entry_props = FATEntryProps::new(n, &self);

        let mask = (1 << self.fat_type.bits_per_entry()) - 1;
        let mut value: u32 = u32::from(entry.clone()) & mask;

        match self.fat_type {
            FATType::FAT12 => {
                let should_shift = n & 1 != 0;
                if should_shift {
                    // FAT12 entries are split between different bytes
                    value <<= 4;
                }

                // we update all the FAT copies
                for fat_sector in entry_props.fat_sectors {
                    self.read_nth_sector(fat_sector.into())?;

                    let value_bytes = value.to_le_bytes();

                    let mut first_byte = value_bytes[0];

                    if should_shift {
                        let mut old_byte = self.sector_buffer[entry_props.sector_offset];
                        // ignore the high 4 bytes of the old entry
                        old_byte &= 0x0F;
                        // OR it with the new value
                        first_byte |= old_byte;
                    }

                    self.sector_buffer[entry_props.sector_offset] = first_byte; // this shouldn't panic
                    self.buffer_modified = true;

                    let bytes_left_on_sector: usize = cmp::min(
                        entry_size as usize,
                        self.sector_size() as usize - entry_props.sector_offset,
                    );

                    if bytes_left_on_sector < entry_size as usize {
                        // looks like this FAT12 entry spans multiple sectors, we must also update the other one
                        self.read_nth_sector((fat_sector + 1).into())?;
                    }

                    let mut second_byte = value_bytes[1];
                    let second_byte_index =
                        (entry_props.sector_offset + 1) % self.sector_size() as usize;
                    if !should_shift {
                        let mut old_byte = self.sector_buffer[second_byte_index];
                        // ignore the low 4 bytes of the old entry
                        old_byte &= 0xF0;
                        // OR it with the new value
                        second_byte |= old_byte;
                    }

                    self.sector_buffer[second_byte_index] = second_byte; // this shouldn't panic
                    self.buffer_modified = true;
                }
            }
            FATType::FAT16 | FATType::FAT32 => {
                // we update all the FAT copies
                for fat_sector in entry_props.fat_sectors {
                    self.read_nth_sector(fat_sector.into())?;

                    let value_bytes = value.to_le_bytes();

                    self.sector_buffer[entry_props.sector_offset
                        ..entry_props.sector_offset + entry_size as usize]
                        .copy_from_slice(&value_bytes[..entry_size as usize]); // this shouldn't panic
                    self.buffer_modified = true;
                }
            }
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        };

        Ok(())
    }

    fn sync_sector_buffer(&mut self) -> Result<(), S::Error> {
        if self.buffer_modified {
            log::trace!("syncing sector {:?}", self.stored_sector);
            self.storage.write_all(&self.sector_buffer)?;
            self.storage
                .seek(SeekFrom::Current(-i64::from(self.props.sector_size)))?;
        }
        self.buffer_modified = false;

        Ok(())
    }
}

/// Public [`Read`]-related functions
impl<S> FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// Read all the entries of a directory ([`PathBuf`]) into [`Vec<DirEntry>`]
    ///
    /// Fails if `path` doesn't represent a directory, or if that directory doesn't exist
    pub fn read_dir(&mut self, path: PathBuf) -> FSResult<Vec<DirEntry>, S::Error> {
        if path.is_malformed() {
            return Err(FSError::MalformedPath);
        }
        if !path.is_dir() {
            log::error!("Not a directory");
            return Err(FSError::NotADirectory);
        }

        let mut entries = self.process_root_dir()?;

        for dir_name in path.clone().into_iter() {
            let dir_cluster = match entries.iter().find(|entry| {
                entry.name == dir_name && entry.attributes.contains(RawAttributes::DIRECTORY)
            }) {
                Some(entry) => entry.data_cluster,
                None => {
                    log::error!("Directory {} not found", path);
                    return Err(FSError::NotFound);
                }
            };

            entries = unsafe { self.process_normal_dir(dir_cluster)? };
        }

        // if we haven't returned by now, that means that the entries vector
        // contains what we want, let's map it to DirEntries and return
        Ok(entries
            .into_iter()
            .filter(|x| self.filter.filter(x))
            .map(|rawentry| {
                let mut entry_path = path.clone();

                entry_path.push(format!(
                    "{}{}",
                    rawentry.name,
                    if rawentry.is_dir { "/" } else { "" }
                ));
                DirEntry {
                    entry: Properties::from_raw(rawentry, entry_path),
                }
            })
            .collect())
    }

    /// Get a corresponding [`ROFile`] object from a [`PathBuf`]
    ///
    /// Borrows `&mut self` until that [`ROFile`] object is dropped, effectively locking `self` until that file closed
    ///
    /// Fails if `path` doesn't represent a file, or if that file doesn't exist
    pub fn get_ro_file(&mut self, path: PathBuf) -> FSResult<ROFile<'_, S>, S::Error> {
        if path.is_malformed() {
            return Err(FSError::MalformedPath);
        }

        if let Some(file_name) = path.file_name() {
            let parent_dir = self.read_dir(path.parent())?;
            match parent_dir.into_iter().find(|direntry| {
                direntry
                    .path()
                    .file_name()
                    .is_some_and(|entry_name| entry_name == file_name)
            }) {
                Some(direntry) => {
                    let mut file = ROFile {
                        fs: self,
                        props: FileProps {
                            offset: 0,
                            current_cluster: direntry.entry.data_cluster,
                            entry: direntry.entry,
                        },
                    };

                    if file.cluster_chain_is_healthy()? {
                        Ok(file)
                    } else {
                        log::error!("The cluster chain of a file is malformed");
                        Err(FSError::InternalFSError(
                            InternalFSError::MalformedClusterChain,
                        ))
                    }
                }
                None => {
                    log::error!("ROFile {} not found", path);
                    Err(FSError::NotFound)
                }
            }
        } else {
            log::error!("Is a directory (not a file)");
            Err(FSError::IsADirectory)
        }
    }
}

/// [`Write`]-related functions
impl<S> FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// Get a corresponding [`RWFile`] object from a [`PathBuf`]
    ///
    /// Borrows `&mut self` until that [`RWFile`] object is dropped, effectively locking `self` until that file closed
    ///
    /// Fails if `path` doesn't represent a file, or if that file doesn't exist
    pub fn get_rw_file(&mut self, path: PathBuf) -> FSResult<RWFile<'_, S>, S::Error> {
        // we first write an empty array to the storage medium
        // if the storage has Write functionality, this shouldn't error,
        // otherwise it should return an error.
        self.storage.write_all(&[])?;

        let ro_file = self.get_ro_file(path)?;
        if ro_file.attributes.read_only {
            return Err(FSError::ReadOnlyFile);
        };

        Ok(RWFile { ro_file })
    }
}

/// Properties about the position of a [`FATEntry`] inside the FAT region
struct FATEntryProps {
    /// Each `n`th element of the vector points at the corrensponding sector at the `n+1`th FAT table
    fat_sectors: Vec<u32>,
    sector_offset: usize,
}

impl FATEntryProps {
    /// Get the [`FATEntryProps`] of the `n`-th [`FATEntry`] of a [`ROFileSystem`] (`fs`)
    pub fn new<S>(n: u32, fs: &FileSystem<S>) -> Self
    where
        S: Read + Write + Seek,
    {
        let fat_byte_offset: u32 = n * fs.fat_type.bits_per_entry() as u32 / 8;
        let mut fat_sectors = Vec::new();
        for nth_table in 0..fs.props.fat_table_count {
            let table_sector_offset = fs.boot_record.nth_FAT_table_sector(nth_table);
            let fat_sector = table_sector_offset + fat_byte_offset / fs.props.sector_size;
            fat_sectors.push(fat_sector);
        }
        let sector_offset: usize = (fat_byte_offset % fs.props.sector_size) as usize;

        FATEntryProps {
            fat_sectors,
            sector_offset,
        }
    }
}

impl<S> ops::Drop for FileSystem<S>
where
    S: Read + Write + Seek,
{
    fn drop(&mut self) {
        // nothing to do if these error out while dropping
        let _ = self.sync_sector_buffer();
        let _ = self.storage.flush();
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;
    use test_log::test;
    use time::macros::*;

    static MINFS: &[u8] = include_bytes!("../imgs/minfs.img");
    static FAT12: &[u8] = include_bytes!("../imgs/fat12.img");
    static FAT16: &[u8] = include_bytes!("../imgs/fat16.img");
    static FAT32: &[u8] = include_bytes!("../imgs/fat32.img");

    #[test]
    #[allow(non_snake_case)]
    fn check_FAT_offset() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let fat_offset = match fs.boot_record {
            BootRecord::FAT(boot_record_fat) => boot_record_fat.first_fat_sector(),
            BootRecord::ExFAT(_boot_record_exfat) => unreachable!(),
        };

        // we manually read the first and second entry of the FAT table
        fs.read_nth_sector(fat_offset.into()).unwrap();

        let first_entry = u16::from_le_bytes(fs.sector_buffer[0..2].try_into().unwrap());
        let media_type = if let BootRecord::FAT(boot_record_fat) = fs.boot_record {
            boot_record_fat.bpb._media_type
        } else {
            unreachable!("this should be a FAT16 filesystem")
        };
        assert_eq!(u16::MAX << 8 | media_type as u16, first_entry);

        let second_entry = u16::from_le_bytes(fs.sector_buffer[2..4].try_into().unwrap());
        assert_eq!(u16::MAX, second_entry);
    }

    #[test]
    fn read_file_in_root_dir() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs.get_ro_file(PathBuf::from("/root.txt")).unwrap();

        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();
        const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    static BEE_MOVIE_SCRIPT: &str = include_str!("../tests/bee movie script.txt");
    fn assert_vec_is_bee_movie_script(buf: &Vec<u8>) {
        let string = str::from_utf8(&buf).unwrap();
        let expected_size = BEE_MOVIE_SCRIPT.len();
        assert_eq!(buf.len(), expected_size);

        assert_eq!(string, BEE_MOVIE_SCRIPT);
    }
    fn assert_file_is_bee_movie_script<S>(file: &mut ROFile<'_, S>)
    where
        S: Read + Write + Seek,
    {
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).unwrap();

        assert_vec_is_bee_movie_script(&buf);
    }

    #[test]
    fn read_huge_file() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs
            .get_ro_file(PathBuf::from("/bee movie script.txt"))
            .unwrap();
        assert_file_is_bee_movie_script(&mut file);
    }

    #[test]
    fn seek_n_read() {
        // this uses the famous "I'd like to interject for a moment" copypasta as a test file
        // you can find it online by just searching this term

        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs
            .get_ro_file(PathBuf::from("/GNU ⁄ Linux copypasta.txt"))
            .unwrap();
        let mut file_bytes = [0_u8; 4096];

        // we first perform a forward seek...
        const EXPECTED_STR1: &str = "Linux is the kernel";
        file.seek(SeekFrom::Start(792)).unwrap();
        let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR1.len()]).unwrap();
        assert_eq!(
            String::from_utf8_lossy(&file_bytes[..bytes_read]),
            EXPECTED_STR1
        );

        // ...then a backward one
        const EXPECTED_STR2: &str = "What you're referring to as Linux, is in fact, GNU/Linux";
        file.seek(SeekFrom::Start(39)).unwrap();
        let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR2.len()]).unwrap();
        assert_eq!(
            String::from_utf8_lossy(&file_bytes[..bytes_read]),
            EXPECTED_STR2
        );
    }

    #[test]
    // this won't actually modify the .img file or the static slices,
    // since we run .to_owned(), which basically clones the data in the static slices,
    // in order to make the Cursor readable/writable
    fn write_to_file() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT12.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs.get_rw_file(PathBuf::from("/root.txt")).unwrap();

        file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
        file.rewind().unwrap();

        assert_file_is_bee_movie_script(&mut file);

        // now let's do something else
        // this write operations will happen between 2 clusters
        const TEXT_OFFSET: u64 = 4598;
        const TEXT: &str = "Hello from the other side";

        file.seek(SeekFrom::Start(TEXT_OFFSET)).unwrap();
        file.write_all(TEXT.as_bytes()).unwrap();

        // seek back to the start of where we wrote our text
        file.seek(SeekFrom::Current(-(TEXT.len() as i64))).unwrap();
        let mut buf = [0_u8; TEXT.len()];
        file.read_exact(&mut buf).unwrap();
        let stored_text = str::from_utf8(&buf).unwrap();

        assert_eq!(TEXT, stored_text);

        // we are also gonna write the bee movie ten more times to see if FAT12 can correctly handle split entries
        for i in 0..10 {
            log::debug!("Writing the bee movie script for the {i} consecutive time",);

            let start_offset = file.seek(SeekFrom::End(0)).unwrap();

            file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
            file.seek(SeekFrom::Start(start_offset)).unwrap();

            let mut buf = vec![0_u8; BEE_MOVIE_SCRIPT.len()];
            file.read_exact(buf.as_mut_slice()).unwrap();

            assert_vec_is_bee_movie_script(&buf);
        }
    }

    #[test]
    #[allow(non_snake_case)]
    fn FAT_tables_after_write_are_identical() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        assert!(
            fs.FAT_tables_are_identical().unwrap(),
            concat!(
                "this should pass. ",
                "if it doesn't, either the corresponding .img file's FAT tables aren't identical",
                "or the tables_are_identical function doesn't work correctly"
            )
        );

        // let's write the bee movie script to root.txt (why not), check, truncate the file, then check again
        let mut file = fs.get_rw_file(PathBuf::from("root.txt")).unwrap();

        file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
        assert!(file.fs.FAT_tables_are_identical().unwrap());

        file.truncate(10_000).unwrap();
        assert!(file.fs.FAT_tables_are_identical().unwrap());
    }

    #[test]
    fn truncate_file() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs
            .get_rw_file(PathBuf::from("/bee movie script.txt"))
            .unwrap();

        // we are gonna truncate the bee movie script down to 20 000 bytes
        const NEW_SIZE: u32 = 20_000;
        file.truncate(NEW_SIZE).unwrap();

        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();
        let mut expected_string = BEE_MOVIE_SCRIPT.to_string();
        expected_string.truncate(NEW_SIZE as usize);

        assert_eq!(file_string, expected_string);
    }

    #[test]
    fn read_only_file() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let file_result = fs.get_rw_file(PathBuf::from("/rootdir/example.txt"));

        match file_result {
            Err(err) => match err {
                FSError::ReadOnlyFile => (),
                _ => panic!("unexpected IOError"),
            },
            _ => panic!("file is marked read-only, yet somehow we got a RWFile for it"),
        }
    }

    #[test]
    fn get_hidden_file() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT12.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let file_path = PathBuf::from("/hidden");
        let file_result = fs.get_ro_file(file_path.clone());
        match file_result {
            Err(err) => match err {
                FSError::NotFound => (),
                _ => panic!("unexpected IOError"),
            },
            _ => panic!("file should be hidden by default"),
        }

        // let's now allow the filesystem to list hidden files
        fs.show_hidden(true);
        let file = fs.get_ro_file(file_path).unwrap();
        assert!(file.attributes.hidden);
    }

    #[test]
    fn read_file_in_subdir() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs
            .get_ro_file(PathBuf::from("/rootdir/example.txt"))
            .unwrap();

        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();
        const EXPECTED_STR: &str = "I am not in the root directory :(\n\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    #[test]
    fn check_file_timestamps() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let file = fs
            .get_ro_file(PathBuf::from("/rootdir/example.txt"))
            .unwrap();

        assert_eq!(datetime!(2024-07-11 13:02:38.15), file.created);
        assert_eq!(datetime!(2024-07-11 13:02:38.0), file.modified);
        assert_eq!(date!(2024 - 07 - 11), file.accessed);
    }

    #[test]
    fn read_file_fat12() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT12.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs.get_ro_file(PathBuf::from("/foo/bar.txt")).unwrap();
        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();
        const EXPECTED_STR: &str = "Hello, World!\n";
        assert_eq!(file_string, EXPECTED_STR);

        // please not that the FAT12 image has been modified so that
        // one FAT entry of the file we are reading is split between different sectors
        // this way, we also test for this case
        let mut file = fs
            .get_ro_file(PathBuf::from("/test/bee movie script.txt"))
            .unwrap();
        assert_file_is_bee_movie_script(&mut file);
    }

    #[test]
    fn assert_img_fat_type() {
        static TEST_CASES: &[(&[u8], FATType)] = &[
            (MINFS, FATType::FAT12),
            (FAT12, FATType::FAT12),
            (FAT16, FATType::FAT16),
            (FAT32, FATType::FAT32),
        ];

        for case in TEST_CASES {
            use std::io::Cursor;

            let mut storage = Cursor::new(case.0.to_owned());
            let fs = FileSystem::from_storage(&mut storage).unwrap();

            assert_eq!(fs.fat_type(), case.1)
        }
    }
}
