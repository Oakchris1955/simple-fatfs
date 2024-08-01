#[cfg(not(feature = "std"))]
use core::*;
#[cfg(feature = "std")]
use std::*;

use ::alloc::{
    borrow::ToOwned,
    format,
    string::{FromUtf16Error, String, ToString},
    vec::*,
};

use bitfield_struct::bitfield;
use bitflags::bitflags;

use ::time;
use ops::Deref;
use time::{Date, PrimitiveDateTime, Time};

use crate::{error::*, io::prelude::*, path::PathBuf};

const SECTOR_SIZE_MIN: usize = 512;
const SECTOR_SIZE_MAX: usize = 4096;

/// Place this in the BPB _jmpboot field to hang if a computer attempts to boot this partition
/// The first two bytes jump to 0 on all bit modes and the third byte is just a NOP
const INFINITE_LOOP: [u8; 3] = [0xEB, 0xFE, 0x90];

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct BootRecordFAT {
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

    // Extended boot record
    ebr: EBR,
}

const BOOT_SIGNATURE: u8 = 0x29;
const FAT_SIGNATURE: u16 = 0x55AA;

impl BootRecordFAT {
    #[inline]
    fn verify_signature(&self) -> bool {
        match self.fat_type() {
            FATType::FAT12 | FATType::FAT16 => unsafe {
                self.ebr.fat12_16.boot_signature == BOOT_SIGNATURE
                    && self.ebr.fat12_16.signature == FAT_SIGNATURE
            },
            FATType::FAT32 => unsafe {
                self.ebr.fat32.boot_signature == BOOT_SIGNATURE
                    && self.ebr.fat32.signature == FAT_SIGNATURE
            },
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        }
    }

    #[inline]
    /// Total sectors in volume (including VBR)s
    pub(crate) fn total_sectors(&self) -> u32 {
        if self.total_sectors_16 == 0 {
            self.total_sectors_32
        } else {
            self.total_sectors_16 as u32
        }
    }

    #[inline]
    /// FAT size in sectors
    pub(crate) fn fat_sector_size(&self) -> u32 {
        if self.table_size_16 == 0 {
            let ebr = self.ebr;
            unsafe { ebr.fat32.table_size_32 }
        } else {
            self.table_size_16 as u32
        }
    }

    #[inline]
    /// The size of the root directory (unless we have FAT32, in which case the size will be 0)
    /// This calculation will round up
    pub(crate) fn root_dir_sectors(&self) -> u16 {
        // 32 is the size of a directory entry in bytes
        ((self.root_entry_count * 32) + (self.bytes_per_sector - 1)) / self.bytes_per_sector
    }

    #[inline]
    /// The first sector in the File Allocation Table
    pub(crate) fn first_fat_sector(&self) -> u16 {
        self.reserved_sector_count
    }

    #[inline]
    /// The first sector of the root directory
    pub(crate) fn first_root_dir_sector(&self) -> u16 {
        self.first_fat_sector() + self.table_count as u16 * self.fat_sector_size() as u16
    }

    #[inline]
    /// The first data sector (that is, the first sector in which directories and files may be stored)
    pub(crate) fn first_data_sector(&self) -> u16 {
        self.first_root_dir_sector() + self.root_dir_sectors()
    }

    #[inline]
    /// The total number of data sectors
    pub(crate) fn total_data_sectors(&self) -> u32 {
        self.total_sectors() - (self.table_count as u32 * self.fat_sector_size())
            + self.root_dir_sectors() as u32
    }

    #[inline]
    /// The total number of clusters
    pub(crate) fn total_clusters(&self) -> u32 {
        self.total_data_sectors() / self.sectors_per_cluster as u32
    }

    #[inline]
    /// The FAT type of this file system
    pub(crate) fn fat_type(&self) -> FATType {
        if self.bytes_per_sector == 0 {
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
#[repr(packed)]
// Everything here is naturally aligned (thank god), so there's no need to make this a packed struct
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

#[derive(Clone, Copy)]
#[repr(packed)]
union BootRecord {
    fat: BootRecordFAT,
    exfat: BootRecordExFAT,
}

#[derive(Clone, Copy)]
#[repr(packed)]
union EBR {
    fat12_16: mem::ManuallyDrop<EBRFAT12_16>,
    fat32: mem::ManuallyDrop<EBRFAT32>,
}

impl fmt::Debug for EBR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: find a good way of printing this
        write!(f, "FAT12-16/32 Extended boot record...")
    }
}

#[derive(Clone, Copy)]
#[repr(packed)]
struct EBRFAT12_16 {
    _drive_num: u8,
    _windows_nt_flags: u8,
    boot_signature: u8,
    volume_serial_num: u32,
    volume_label: [u8; 11],
    _system_identifier: [u8; 8],
    _boot_code: [u8; 448],
    signature: u16,
}

// FIXME: these might be the other way around
#[derive(Debug, Clone, Copy)]
struct FATVersion {
    minor: u8,
    major: u8,
}

#[derive(Clone, Copy)]
#[repr(packed)]
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
    _boot_code: [u8; 420],
    signature: u16,
}

#[derive(Clone, Copy)]
#[repr(packed)]
struct FSInfoFAT32 {
    lead_signature: [u8; 4],
    _reserved1: [u8; 480],
    mid_signature: [u8; 4],
    last_free_cluster: u32,
    cluster_width: u32,
    _reserved2: [u8; 12],
    trail_signature: [u8; 4],
}

/// An enum representing different versions of the FAT filesystem
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FATType {
    FAT12,
    FAT16,
    FAT32,
    ExFAT,
}

impl FATType {
    #[inline]
    pub fn bits_per_entry(&self) -> u8 {
        match self {
            FATType::FAT12 => 12,
            FATType::FAT16 => 16,
            FATType::FAT32 => 28,
            FATType::ExFAT => 32,
        }
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

#[repr(packed)]
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
    /// A list of the various attributes specified for a file/directory
    ///
    /// To check whether a given [`Attributes`] struct contains a flag, use the [`contains()`](Attributes::contains()) method
    ///
    /// Generated using [bitflags](https://docs.rs/bitflags/2.6.0/bitflags/)
    #[derive(Debug, PartialEq)]
    pub struct Attributes: u8 {
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

const START_YEAR: i32 = 1980;

#[bitfield(u16)]
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

#[derive(Debug, Clone)]
#[repr(packed)]
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

#[derive(Debug, Clone)]
#[repr(packed)]
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

#[repr(packed)]
struct FATDirEntry {
    sfn: SFN,
    attributes: Attributes,
    _reserved: [u8; 1],
    created: EntryCreationTime,
    accessed: DateAttribute,
    cluster_high: u16,
    modified: EntryModificationTime,
    cluster_low: u16,
    file_size: u32,
}

#[repr(packed)]
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
        let mut slice = [0_u8; 13 * 2];

        slice[..10].copy_from_slice(&self.first_chars);
        slice[10..22].copy_from_slice(&self.mid_chars);
        slice[22..].copy_from_slice(&self.last_chars);

        // this is safe since u8 is half the size of u16 and the len of the src slice is even
        unsafe { slice.align_to().1.try_into().unwrap() }
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
    attributes: Attributes,
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
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    #[inline]
    pub fn attributes(&self) -> &Attributes {
        &self.attributes
    }

    #[inline]
    pub fn creation_time(&self) -> &PrimitiveDateTime {
        &self.created
    }

    #[inline]
    pub fn modification_time(&self) -> &PrimitiveDateTime {
        &self.modified
    }

    #[inline]
    pub fn last_accessed_date(&self) -> &Date {
        &self.accessed
    }

    #[inline]
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
            attributes: raw.attributes,
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

/// A file within the FAT filesystem
pub struct File<'a, S>
where
    S: Read + Write + Seek,
{
    fs: &'a mut FileSystem<S>,
    entry: Properties,
    /// the byte offset of the R/W pointer
    offset: u64,
    current_cluster: u32,
}

impl<'a, S> Deref for File<'a, S>
where
    S: Read + Write + Seek,
{
    type Target = Properties;

    fn deref(&self) -> &Self::Target {
        &self.entry
    }
}

impl<'a, S> IOBase for File<'a, S>
where
    S: Read + Write + Seek,
{
    type Error = S::Error;
}

impl<'a, S> Read for File<'a, S>
where
    S: Read + Write + Seek,
{
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        let mut current_cluster = self.current_cluster;
        let mut bytes_read = 0;
        // this is the maximum amount of bytes that can be read
        let read_cap = cmp::min(buf.len(), self.file_size as usize - self.offset as usize);

        'outer: loop {
            let sector_init_offset = u32::try_from(self.offset % self.fs.cluster_size()).unwrap()
                / self.fs.sector_size();
            let first_sector_of_cluster =
                self.fs.data_cluster_to_partition_sector(current_cluster) + sector_init_offset;
            for sector in first_sector_of_cluster
                ..first_sector_of_cluster + self.fs.sectors_per_cluster() as u32
            {
                self.fs.read_nth_sector(sector.into())?;

                let start_index = self.offset as usize % self.fs.sector_size() as usize;

                let bytes_to_be_written = cmp::min(
                    read_cap - bytes_read,
                    self.fs.sector_size() as usize - start_index,
                );

                buf[bytes_read..bytes_read + bytes_to_be_written].copy_from_slice(
                    &self.fs.sector_buffer[start_index..start_index + bytes_to_be_written],
                );

                bytes_read += bytes_to_be_written;
                self.offset += bytes_to_be_written as u64;

                if bytes_read >= read_cap || self.offset >= self.file_size.into() {
                    break 'outer;
                }
            }

            match self.fs.read_nth_FAT_entry(current_cluster)? {
                FATEntry::Allocated(next_cluster) => current_cluster = next_cluster,
                // when a `File` is created, `cluster_chain_is_healthy` is called, if it fails, that File is dropped
                _ => unreachable!(
                    "{} {} {} {} {}",
                    current_cluster, self.data_cluster, bytes_read, self.offset, self.file_size,
                ),
            };
        }

        Ok(bytes_read as usize)
    }
}

impl<'a, S> Seek for File<'a, S>
where
    S: Read + Write + Seek,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, Self::Error> {
        let mut offset = match pos {
            SeekFrom::Start(offset) => offset,
            SeekFrom::Current(offset) => {
                let offset = self.offset as i64 + offset;
                offset.try_into().unwrap_or(u64::MIN)
            }
            SeekFrom::End(offset) => {
                let offset = self.file_size as i64 + offset;
                offset.try_into().unwrap_or(u64::MIN)
            }
        };

        if offset > self.file_size.into() {
            offset = self.file_size.into();
        }

        use cmp::Ordering;
        match offset.cmp(&self.offset) {
            Ordering::Less => {
                // here, we basically "rewind" back to the start of the file and then seek to where we want
                // this of course has performance issues, so TODO: find a solution that is both memory & time efficient
                // (perhaps we could follow a similar approach to elm-chan's FATFS, by using a cluster link map table, perhaps as an optional feature)
                self.offset = 0;
                self.current_cluster = self.data_cluster;
                self.seek(pos)?;
            }
            Ordering::Equal => (),
            Ordering::Greater => {
                for _ in (self.offset / self.fs.cluster_size()..offset / self.fs.cluster_size())
                    .step_by(self.fs.cluster_size() as usize)
                {
                    match self.fs.read_nth_FAT_entry(self.current_cluster)? {
                        FATEntry::Allocated(next_cluster) => self.current_cluster = next_cluster,
                        _ => unreachable!(),
                    }
                }
                self.offset = offset;
            }
        }

        Ok(self.offset)
    }
}

impl<'a, S> File<'a, S>
where
    S: Read + Write + Seek,
{
    fn cluster_chain_is_healthy(&mut self) -> Result<bool, S::Error> {
        let mut current_cluster = self.entry.data_cluster;
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
    total_clusters: u32,
    /// sector offset of the FAT
    fat_offset: u32,
    reserved_sectors: u32,
    first_data_sector: u32,
}

/// An API to process a FAT filesystem
pub struct FileSystem<S>
where
    S: Read + Write + Seek,
{
    /// Any struct that implements the [`Read`], [`Write`] & [`Seek`] traits
    storage: S,

    /// The length of this will be the sector size of the FS for all FAT types except FAT12, in that case, it will be double that value
    sector_buffer: Vec<u8>,
    stored_sector: u64,

    boot_record: BootRecord,
    // since `self.fat_type()` calls like 5 nested functions, we keep this cached and expose it as a public field
    fat_type: FATType,
    props: FSProperties,
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

/// Public functions
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

        if bytes_read < mem::size_of::<BootRecord>() {
            return Err(FSError::InternalFSError(InternalFSError::StorageTooSmall));
        }

        let boot_record: BootRecord = unsafe { mem::transmute_copy(&buffer) };

        // verify boot record signature
        let fat_type = unsafe { boot_record.fat.fat_type() };

        match fat_type {
            FATType::FAT12 | FATType::FAT16 | FATType::FAT32 => {
                if unsafe { boot_record.fat.verify_signature() } {
                    return Err(FSError::InternalFSError(InternalFSError::InvalidBPBSig));
                }
            }
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        };

        let sector_size: u32 = unsafe {
            match fat_type {
                FATType::FAT12 | FATType::FAT16 | FATType::FAT32 => {
                    boot_record.fat.bytes_per_sector.into()
                }
                FATType::ExFAT => 1 << boot_record.exfat.sector_shift,
            }
        };
        let cluster_size: u64 = unsafe {
            match fat_type {
                FATType::FAT12 | FATType::FAT16 | FATType::FAT32 => {
                    (boot_record.fat.sectors_per_cluster as u32 * sector_size).into()
                }
                FATType::ExFAT => {
                    1 << (boot_record.exfat.sector_shift + boot_record.exfat.cluster_shift)
                }
            }
        };

        let reserved_sectors: u32 = match fat_type {
            FATType::FAT12 | FATType::FAT16 | FATType::FAT32 => unsafe {
                boot_record.fat.reserved_sector_count as u32
            },
            FATType::ExFAT => todo!("ExFAT is not yet implemented"),
        };

        let first_data_sector = unsafe {
            boot_record.fat.reserved_sector_count as u32
                + (boot_record.fat.table_count as u32 * boot_record.fat.fat_sector_size())
                + boot_record.fat.root_dir_sectors() as u32
        };

        let props = FSProperties {
            sector_size,
            cluster_size,
            fat_offset: unsafe { boot_record.fat.reserved_sector_count as u32 },
            total_clusters: unsafe { boot_record.fat.total_clusters() },
            reserved_sectors,
            first_data_sector,
        };

        Ok(Self {
            storage,
            sector_buffer: buffer[..sector_size as usize].to_vec(),
            stored_sector: 0,
            boot_record,
            fat_type,
            props,
        })
    }

    /// Read all the entries of a directory ([`PathBuf`]) into [`Vec<DirEntry>`]
    ///
    /// Fails if `path` doesn't represent a directory, or if that directory doesn't exist
    pub fn read_dir(&mut self, path: PathBuf) -> FSResult<Vec<DirEntry>, S::Error> {
        if path.is_malformed() {
            return Err(FSError::MalformedPath);
        }
        if !path.is_dir() {
            return Err(FSError::NotADirectory);
        }

        let mut entries = self.process_root_dir()?;

        for dir_name in path.clone().into_iter() {
            let dir_cluster = match entries.iter().find(|entry| {
                entry.name == dir_name && entry.attributes.contains(Attributes::DIRECTORY)
            }) {
                Some(entry) => entry.data_cluster,
                None => {
                    return Err(FSError::NotFound);
                }
            };

            entries = unsafe { self.process_normal_dir(dir_cluster)? };
        }

        // if we haven't returned by now, that means that the entries vector
        // contains what we want, let's map it to DirEntries and return
        Ok(entries
            .into_iter()
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

    /// Get a corresponding [`File`] object from a [`PathBuf`]
    ///
    /// Borrows `&mut self` until that [`File`] object is dropped, effectively locking `self` until that file closed
    ///
    /// Fails if `path` doesn't represent a file, or if that file doesn't exist
    pub fn get_file(&mut self, path: PathBuf) -> FSResult<File<S>, S::Error> {
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
                    let mut file = File {
                        fs: self,
                        offset: 0,
                        current_cluster: direntry.entry.data_cluster,
                        entry: direntry.entry,
                    };

                    if file.cluster_chain_is_healthy()? {
                        Ok(file)
                    } else {
                        Err(FSError::InternalFSError(
                            InternalFSError::MalformedClusterChain,
                        ))
                    }
                }
                None => Err(FSError::NotFound),
            }
        } else {
            Err(FSError::IsADirectory)
        }
    }
}

/// Internal low-level functions
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
            for chunk in self
                .read_nth_sector(sector.into())?
                .chunks(mem::size_of::<FATDirEntry>())
            {
                match chunk[0] {
                    // nothing else to read
                    0 => break 'outer,
                    // unused entry
                    0xE5 => continue,
                    _ => (),
                };

                let entry = mem::transmute::<[u8; mem::size_of::<FATDirEntry>()], FATDirEntry>(
                    // this is guaranteed NOT TO PANIC
                    chunk.try_into().unwrap(),
                );

                if entry.attributes.contains(Attributes::LFN) {
                    // TODO: perhaps there is a way to utilize the `order` field?
                    let lfn_entry: LFNEntry = mem::transmute(entry);

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
                        is_dir: entry.attributes.contains(Attributes::DIRECTORY),
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
        match self.fat_type {
            FATType::FAT12 | FATType::FAT16 => {
                let mut entries = Vec::new();

                let root_dir_sector = unsafe { self.boot_record.fat.first_root_dir_sector() };
                let sector_count = unsafe { self.boot_record.fat.root_dir_sectors() };

                for sector in root_dir_sector..(root_dir_sector + sector_count) {
                    let mut new_entries = unsafe { self.process_entry_sector(sector.into())? };
                    entries.append(&mut new_entries);
                }

                Ok(entries)
            }
            FATType::FAT32 => unsafe {
                let cluster = self.boot_record.exfat.root_dir_cluster;
                self.process_normal_dir(cluster)
            },
            FATType::ExFAT => {
                todo!("ExFAT not implemented yet")
            }
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
                    return Err(FSError::InternalFSError(
                        InternalFSError::MalformedClusterChain,
                    ))
                }
            }
        }

        Ok(entries)
    }

    /// Read the nth sector from the partition's beginning and store it in [`self.sector_buffer`](Self::sector_buffer)
    ///
    /// This function also returns an immutable reference to [`self.sector_buffer`](Self::sector_buffer)
    fn read_nth_sector(&mut self, n: u64) -> Result<&Vec<u8>, S::Error> {
        // nothing to do if the sector we wanna read is already cached
        if n != self.stored_sector {
            self.storage.seek(SeekFrom::Start(
                self.sector_to_partition_offset(n as u32).into(),
            ))?;
            self.storage.read_exact(&mut self.sector_buffer)?;
            self.stored_sector = n;
        }

        Ok(&self.sector_buffer)
    }

    #[allow(non_snake_case)]
    fn read_nth_FAT_entry(&mut self, n: u32) -> Result<FATEntry, S::Error> {
        // the size of an entry rounded up to bytes
        let entry_size = self.fat_type.bits_per_entry().next_power_of_two() as u32 / 8;
        let fat_offset: u32 = n * self.fat_type.bits_per_entry() as u32 / 8;
        let fat_sector_offset = self.props.fat_offset + fat_offset / self.props.sector_size;
        let entry_offset: usize = (fat_offset % self.props.sector_size) as usize;

        self.read_nth_sector(fat_sector_offset.into())?;

        let mut value_bytes = [0_u8; 4];
        let bytes_to_read: usize = cmp::min(
            entry_offset + entry_size as usize,
            self.sector_size() as usize,
        ) - entry_offset;
        value_bytes[..bytes_to_read]
            .copy_from_slice(&self.sector_buffer[entry_offset..entry_offset + bytes_to_read]); // this shouldn't panic

        // in FAT12, FAT entries may be split between two different sectors
        if self.fat_type == FATType::FAT12 && (bytes_to_read as u32) < entry_size {
            self.read_nth_sector((fat_sector_offset + 1).into())?;

            value_bytes[bytes_to_read..entry_size as usize]
                .copy_from_slice(&self.sector_buffer[..(entry_size as usize - bytes_to_read)]);

            /*todo!(
                "read: {} {:?} n: {} offset: {}",
                bytes_to_read,
                value_bytes,
                n,
                fat_offset
            );*/
        }

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
        panic!("{:?} {:?}", value.to_be_bytes(), padding.to_be_bytes());
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

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;
    use time::macros::*;

    static MINFS: &[u8] = include_bytes!("../imgs/minfs.img");
    static FAT12: &[u8] = include_bytes!("../imgs/fat12.img");
    static FAT16: &[u8] = include_bytes!("../imgs/fat16.img");

    #[test]
    #[allow(non_snake_case)]
    fn check_FAT_offset() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        // we manually read the first and second entry of the FAT table
        fs.read_nth_sector(fs.props.fat_offset.into()).unwrap();

        let first_entry = u16::from_le_bytes(fs.sector_buffer[0..2].try_into().unwrap());
        let media_type = unsafe { fs.boot_record.fat._media_type };
        assert_eq!(u16::MAX << 8 | media_type as u16, first_entry);

        let second_entry = u16::from_le_bytes(fs.sector_buffer[2..4].try_into().unwrap());
        assert_eq!(u16::MAX, second_entry);
    }

    #[test]
    fn read_file_in_root_dir() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs.get_file(PathBuf::from("/root.txt")).unwrap();
        let mut file_bytes = [0_u8; 1024];
        let bytes_read = file.read(&mut file_bytes).unwrap();

        let file_string = String::from_utf8_lossy(&file_bytes[..bytes_read]).to_string();
        const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    static BEE_MOVIE_SCRIPT: &str = include_str!("../tests/bee movie script.txt");
    fn assert_file_is_bee_movie_script<S>(file: &mut File<S>)
    where
        S: Read + Write + Seek,
    {
        let mut file_bytes = [0_u8; 65536];
        let bytes_read = file.read(&mut file_bytes).unwrap();

        let expected_filesize = BEE_MOVIE_SCRIPT.len();
        assert_eq!(bytes_read, expected_filesize);

        let utf8_string = str::from_utf8(&file_bytes[..bytes_read]).unwrap();
        assert_eq!(utf8_string, BEE_MOVIE_SCRIPT);
    }

    #[test]
    fn read_huge_file() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs.get_file(PathBuf::from("/bee movie script.txt")).unwrap();
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
            .get_file(PathBuf::from("/GNU ⁄ Linux copypasta.txt"))
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
    fn read_file_in_subdir() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs.get_file(PathBuf::from("/rootdir/example.txt")).unwrap();
        let mut file_bytes = [0_u8; 1024];
        let bytes_read = file.read(&mut file_bytes).unwrap();

        let file_string = String::from_utf8_lossy(&file_bytes[..bytes_read]).to_string();
        const EXPECTED_STR: &str = "I am not in the root directory :(\n\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    #[test]
    fn check_file_timestamps() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT16.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let file = fs.get_file(PathBuf::from("/rootdir/example.txt")).unwrap();

        assert_eq!(datetime!(2024-07-11 13:02:38.15), file.entry.created);
        assert_eq!(datetime!(2024-07-11 13:02:38.0), file.entry.modified);
        assert_eq!(date!(2024 - 07 - 11), file.entry.accessed);
    }

    #[test]
    fn read_file_fat12() {
        use std::io::Cursor;

        let mut storage = Cursor::new(FAT12.to_owned());
        let mut fs = FileSystem::from_storage(&mut storage).unwrap();

        let mut file = fs.get_file(PathBuf::from("/foo/bar.txt")).unwrap();
        let mut file_bytes = [0_u8; 1024];
        let bytes_read = file.read(&mut file_bytes).unwrap();

        let file_string = String::from_utf8_lossy(&file_bytes[..bytes_read]).to_string();
        const EXPECTED_STR: &str = "Hello, World!\n";
        assert_eq!(file_string, EXPECTED_STR);

        // please not that the FAT12 image has been modified so that
        // one FAT entry of the file we are reading is split between different sectors
        // this way, we also test for this case
        let mut file = fs
            .get_file(PathBuf::from("/test/bee movie script.txt"))
            .unwrap();
        assert_file_is_bee_movie_script(&mut file);
    }

    #[test]
    fn assert_img_fat_type() {
        static TEST_CASES: &[(&[u8], FATType)] = &[
            (MINFS, FATType::FAT12),
            (FAT12, FATType::FAT12),
            (FAT16, FATType::FAT16),
        ];

        for case in TEST_CASES {
            use std::io::Cursor;

            let mut storage = Cursor::new(case.0.to_owned());
            let fs = FileSystem::from_storage(&mut storage).unwrap();

            assert_eq!(fs.fat_type, case.1)
        }
    }
}
