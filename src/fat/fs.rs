use super::*;

use crate::{error::*, io::prelude::*, path::PathBuf, time::*, utils};

use core::{cmp, ops};

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use bincode::Options as _;

use ::time;
use time::{Date, PrimitiveDateTime};

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
    fn bits_per_entry(&self) -> u8 {
        match self {
            FATType::FAT12 => 12,
            FATType::FAT16 => 16,
            // the high 4 bits are ignored, but are still part of the entry
            FATType::FAT32 => 32,
            FATType::ExFAT => 32,
        }
    }

    #[inline]
    /// How many bytes this [`FATType`] spans across
    fn entry_size(&self) -> u32 {
        self.bits_per_entry().next_power_of_two() as u32 / 8
    }
}

// the first 2 entries are reserved
const RESERVED_FAT_ENTRIES: u32 = 2;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FATEntry {
    /// This cluster is free
    Free,
    /// This cluster is allocated and the next cluster is the contained value
    Allocated(u32),
    /// This cluster is reserved
    Reserved,
    /// This is a bad (defective) cluster
    Bad,
    /// This cluster is allocated and is the final cluster of the file
    Eof,
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
            FATEntry::Eof => u32::MAX,
        }
    }
}

/// Properties about the position of a [`FATEntry`] inside the FAT region
struct FATEntryProps {
    /// Each `n`th element of the vector points at the corrensponding sector at the (first) active FAT table
    fat_sector: u32,
    sector_offset: usize,
}

impl FATEntryProps {
    /// Get the [`FATEntryProps`] of the `n`-th [`FATEntry`] of a [`ROFileSystem`] (`fs`)
    pub fn new<S>(n: u32, fs: &FileSystem<S>) -> Self
    where
        S: Read + Write + Seek,
    {
        let fat_byte_offset: u32 = n * fs.fat_type.bits_per_entry() as u32 / 8;
        let fat_sector =
            u32::from(fs.props.first_fat_sector) + fat_byte_offset / fs.props.sector_size;
        let sector_offset: usize = (fat_byte_offset % fs.props.sector_size) as usize;

        FATEntryProps {
            fat_sector,
            sector_offset,
        }
    }
}

/// Properties about the position of a sector within the FAT
struct FATSectorProps {
    /// the sector belongs to this FAT copy
    #[allow(unused)]
    fat_offset: u8,
    /// the sector is that many away from the start of the FAT copy
    sector_offset: u32,
}

impl FATSectorProps {
    /// Returns [`None`] if this sector doesn't belong to a FAT table
    pub fn new<S>(sector: u64, fs: &FileSystem<S>) -> Option<Self>
    where
        S: Read + Write + Seek,
    {
        if !fs.sector_belongs_to_FAT(sector) {
            return None;
        }

        let sector_offset_from_first_fat: u64 = sector - u64::from(fs.props.first_fat_sector);
        let fat_offset = (sector_offset_from_first_fat / u64::from(fs.props.fat_sector_size)) as u8;
        let sector_offset =
            (sector_offset_from_first_fat % u64::from(fs.props.fat_sector_size)) as u32;

        Some(FATSectorProps {
            fat_offset,
            sector_offset,
        })
    }

    #[allow(non_snake_case)]
    pub fn get_corresponding_FAT_sectors<S>(&self, fs: &FileSystem<S>) -> Vec<u64>
    where
        S: Read + Write + Seek,
    {
        let mut vec = Vec::new();

        for i in 0..fs.props.fat_table_count {
            vec.push(
                (u32::from(fs.props.first_fat_sector)
                    + u32::from(i) * fs.props.fat_sector_size
                    + self.sector_offset)
                    .into(),
            )
        }

        vec
    }
}

/// A resolved file/directory entry (for internal usage only)
#[derive(Debug)]
pub(crate) struct RawProperties {
    pub(crate) name: String,
    pub(crate) is_dir: bool,
    pub(crate) attributes: RawAttributes,
    pub(crate) created: PrimitiveDateTime,
    pub(crate) modified: PrimitiveDateTime,
    pub(crate) accessed: Date,
    pub(crate) file_size: u32,
    pub(crate) data_cluster: u32,

    pub(crate) chain: DirEntryChain,
}

/// A container for file/directory properties
#[derive(Debug)]
pub struct Properties {
    pub(crate) path: PathBuf,
    pub(crate) attributes: Attributes,
    pub(crate) created: PrimitiveDateTime,
    pub(crate) modified: PrimitiveDateTime,
    pub(crate) accessed: Date,
    pub(crate) file_size: u32,
    pub(crate) data_cluster: u32,

    // internal fields
    pub(crate) chain: DirEntryChain,
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
            chain: raw.chain,
        }
    }
}

/// A thin wrapper for [`Properties`] represing a directory entry
#[derive(Debug)]
pub struct DirEntry {
    pub(crate) entry: Properties,
}

impl ops::Deref for DirEntry {
    type Target = Properties;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.entry
    }
}

pub(crate) const UNUSED_ENTRY: u8 = 0xE5;
pub(crate) const LAST_AND_UNUSED_ENTRY: u8 = 0x00;

#[derive(Debug, Default)]
struct EntryParser {
    entries: Vec<RawProperties>,
    lfn_buf: Vec<String>,
    lfn_checksum: Option<u8>,
    current_chain: Option<DirEntryChain>,
}

impl EntryParser {
    #[inline]
    fn _decrement_parsed_entries_counter(&mut self) {
        if let Some(current_chain) = &mut self.current_chain {
            current_chain.len -= 1
        }
    }

    /// Parses a sector of 8.3 & LFN entries
    ///
    /// Returns a [`Result<bool>`] indicating whether or not
    /// this sector was the last one in the chain containing entries
    fn parse_sector<S>(
        &mut self,
        sector: u32,
        fs: &mut FileSystem<S>,
    ) -> Result<bool, <S as IOBase>::Error>
    where
        S: Read + Write + Seek,
    {
        use utils::bincode::bincode_config;

        let entry_location_unit = EntryLocationUnit::from_partition_sector(sector, fs);

        for (index, chunk) in fs
            .read_nth_sector(sector.into())?
            .chunks(DIRENTRY_SIZE)
            .enumerate()
        {
            match chunk[0] {
                LAST_AND_UNUSED_ENTRY => return Ok(true),
                UNUSED_ENTRY => continue,
                _ => (),
            };

            let Ok(entry) = bincode_config().deserialize::<FATDirEntry>(chunk) else {
                continue;
            };

            // update current entry chain data
            match &mut self.current_chain {
                Some(current_chain) => current_chain.len += 1,
                None => {
                    self.current_chain = Some(DirEntryChain {
                        location: EntryLocation {
                            index: index as u32,
                            unit: entry_location_unit,
                        },
                        len: 1,
                    })
                }
            }

            if entry.attributes.contains(RawAttributes::LFN) {
                // TODO: perhaps there is a way to utilize the `order` field?
                let Ok(lfn_entry) = bincode_config().deserialize::<LFNEntry>(chunk) else {
                    self._decrement_parsed_entries_counter();
                    continue;
                };

                // If the signature verification fails, consider this entry corrupted
                if !lfn_entry.verify_signature() {
                    self._decrement_parsed_entries_counter();
                    continue;
                }

                match self.lfn_checksum {
                    Some(checksum) => {
                        if checksum != lfn_entry.checksum {
                            self.lfn_checksum = None;
                            self.lfn_buf.clear();
                            self.current_chain = None;
                            continue;
                        }
                    }
                    None => self.lfn_checksum = Some(lfn_entry.checksum),
                }

                let char_arr = lfn_entry.get_byte_slice();
                if let Ok(temp_str) = utils::string::string_from_lfn(&char_arr) {
                    self.lfn_buf.push(temp_str);
                }

                continue;
            }

            let filename = if !self.lfn_buf.is_empty()
                && self
                    .lfn_checksum
                    .is_some_and(|checksum| checksum == entry.sfn.gen_checksum())
            {
                // for efficiency reasons, we store the LFN string sequences as we read them
                let parsed_str: String = self.lfn_buf.iter().cloned().rev().collect();
                self.lfn_buf.clear();
                self.lfn_checksum = None;
                parsed_str
            } else {
                entry.sfn.to_string()
            };

            if let (Ok(created), Ok(modified), Ok(accessed)) = (
                entry.created.try_into(),
                entry.modified.try_into(),
                entry.accessed.try_into(),
            ) {
                self.entries.push(RawProperties {
                    name: filename,
                    is_dir: entry.attributes.contains(RawAttributes::DIRECTORY),
                    attributes: entry.attributes,
                    created,
                    modified,
                    accessed,
                    file_size: entry.file_size,
                    data_cluster: ((entry.cluster_high as u32) << 16) + entry.cluster_low as u32,
                    chain: self
                        .current_chain
                        .take()
                        .expect("at this point, this shouldn't be None"),
                })
            }
        }

        Ok(false)
    }

    /// Consumes [`Self`](EntryParser) & returns a `Vec` of [`RawProperties`]
    /// of the parsed entries
    fn finish(self) -> Vec<RawProperties> {
        self.entries
    }
}

pub(crate) trait OffsetConversions {
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

    #[inline]
    fn sector_to_partition_offset(&self, sector: u32) -> u32 {
        sector * self.sector_size()
    }

    #[inline]
    fn data_cluster_to_partition_sector(&self, cluster: u32) -> u32 {
        self.cluster_to_sector((cluster - RESERVED_FAT_ENTRIES).into()) + self.first_data_sector()
    }

    #[inline]
    fn partition_sector_to_data_cluster(&self, sector: u32) -> u32 {
        (sector - self.first_data_sector()) / self.sectors_per_cluster() as u32
            + RESERVED_FAT_ENTRIES
    }
}

impl<S> OffsetConversions for FileSystem<'_, S>
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

/// Some generic properties common across all FAT versions, like a sector's size, are cached here
#[derive(Debug)]
pub(crate) struct FSProperties {
    pub(crate) sector_size: u32,
    pub(crate) cluster_size: u64,
    pub(crate) total_sectors: u32,
    pub(crate) total_clusters: u32,
    /// sector offset of the FAT
    pub(crate) fat_table_count: u8,
    pub(crate) fat_sector_size: u32,
    pub(crate) first_fat_sector: u16,
    pub(crate) first_root_dir_sector: u16,
    pub(crate) first_data_sector: u32,
}

impl FSProperties {
    fn from_boot_record(boot_record: &BootRecord) -> Self {
        let sector_size = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.bpb.bytes_per_sector.into(),
            BootRecord::ExFAT(boot_record_exfat) => 1 << boot_record_exfat.sector_shift,
        };
        let cluster_size = match boot_record {
            BootRecord::Fat(boot_record_fat) => {
                (boot_record_fat.bpb.sectors_per_cluster as u32 * sector_size).into()
            }
            BootRecord::ExFAT(boot_record_exfat) => {
                1 << (boot_record_exfat.sector_shift + boot_record_exfat.cluster_shift)
            }
        };
        let total_sectors = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.total_sectors(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let total_clusters = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.total_clusters(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let fat_table_count = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.bpb.table_count,
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let fat_sector_size = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.fat_sector_size(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };
        let first_fat_sector = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.first_fat_sector(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };
        let first_root_dir_sector = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.first_root_dir_sector(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let first_data_sector = match boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.first_data_sector().into(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };

        FSProperties {
            sector_size,
            cluster_size,
            fat_table_count,
            fat_sector_size,
            first_fat_sector,
            total_sectors,
            total_clusters,
            first_root_dir_sector,
            first_data_sector,
        }
    }
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

#[allow(clippy::derivable_impls)]
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
pub struct FileSystem<'a, S>
where
    S: Read + Write + Seek,
{
    /// Any struct that implements the [`Read`], [`Write`] & [`Seek`] traits
    storage: S,

    /// The length of this will be the sector size of the FS for all FAT types except FAT12, in that case, it will be double that value
    pub(crate) sector_buffer: Vec<u8>,
    /// ANY CHANGES TO THE SECTOR BUFFER SHOULD ALSO SET THIS TO TRUE
    pub(crate) buffer_modified: bool,
    pub(crate) stored_sector: u64,

    clock: &'a dyn Clock,

    pub(crate) boot_record: BootRecord,
    // since `self.boot_record.fat_type()` calls like 5 nested functions, we keep this cached and expose it with a public getter function
    fat_type: FATType,
    pub(crate) props: FSProperties,
    // this doesn't mean that this is the first free cluster, it just means
    // that if we want to figure that out, we should start from this cluster
    first_free_cluster: u32,

    filter: FileFilter,
}

/// Getter functions
impl<S> FileSystem<'_, S>
where
    S: Read + Write + Seek,
{
    /// What is the [`FATType`] of the filesystem
    pub fn fat_type(&self) -> FATType {
        self.fat_type
    }
}

/// Setter functions
impl<'a, S> FileSystem<'a, S>
where
    S: Read + Write + Seek,
{
    /// Replace the internal [`Clock`] with a different one
    ///
    /// Use this in `no-std` contexts to replace the [`DefaultClock`] used
    pub fn with_clock(&mut self, clock: &'a dyn Clock) {
        self.clock = clock;
    }
}

/// Setter functions
impl<S> FileSystem<'_, S>
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
impl<S> FileSystem<'_, S>
where
    S: Read + Write + Seek,
{
    /// Create a [`FileSystem`] from a storage object that implements [`Read`], [`Write`] & [`Seek`]
    ///
    /// Fails if the storage is way too small to support a FAT filesystem.
    /// For most use cases, that shouldn't be an issue, you can just call [`.unwrap()`](Result::unwrap)
    pub fn from_storage(mut storage: S) -> FSResult<Self, S::Error> {
        use utils::bincode::bincode_config;

        // Begin by reading the boot record
        // We don't know the sector size yet, so we just go with the biggest possible one for now
        let mut buffer = [0u8; MAX_SECTOR_SIZE];

        let bytes_read = storage.read(&mut buffer)?;
        let mut stored_sector = 0;

        if bytes_read < MIN_SECTOR_SIZE {
            return Err(FSError::InternalFSError(InternalFSError::StorageTooSmall));
        }

        let bpb: BpbFat = bincode_config().deserialize(&buffer[..BPBFAT_SIZE])?;

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

            Ebr::FAT32(ebr_fat32, fsinfo)
        } else {
            Ebr::FAT12_16(
                bincode_config()
                    .deserialize::<EBRFAT12_16>(&buffer[BPBFAT_SIZE..BPBFAT_SIZE + EBR_SIZE])?,
            )
        };

        // TODO: see how we will handle this for exfat
        let boot_record = BootRecord::Fat(BootRecordFAT { bpb, ebr });

        // verify boot record signature
        let fat_type = boot_record.fat_type();
        log::info!("The FAT type of the filesystem is {:?}", fat_type);

        match boot_record {
            BootRecord::Fat(boot_record_fat) => {
                if boot_record_fat.verify_signature() {
                    log::error!("FAT boot record has invalid signature(s)");
                    return Err(FSError::InternalFSError(InternalFSError::InvalidBPBSig));
                }
            }
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };

        let props = FSProperties::from_boot_record(&boot_record);

        let mut fs = Self {
            storage,
            sector_buffer: buffer[..props.sector_size as usize].to_vec(),
            buffer_modified: false,
            stored_sector,
            clock: &STATIC_DEFAULT_CLOCK,
            boot_record,
            fat_type,
            props,
            first_free_cluster: RESERVED_FAT_ENTRIES,
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
impl<S> FileSystem<'_, S>
where
    S: Read + Write + Seek,
{
    fn process_root_dir(&mut self) -> FSResult<Vec<RawProperties>, S::Error> {
        match self.boot_record {
            BootRecord::Fat(boot_record_fat) => match boot_record_fat.ebr {
                Ebr::FAT12_16(_ebr_fat12_16) => {
                    let mut entry_parser = EntryParser::default();

                    let root_dir_sector = boot_record_fat.first_root_dir_sector();
                    let sector_count = boot_record_fat.root_dir_sectors();

                    for sector in root_dir_sector..(root_dir_sector + sector_count) {
                        if entry_parser.parse_sector(sector.into(), self)? {
                            break;
                        }
                    }

                    Ok(entry_parser.finish())
                }
                Ebr::FAT32(ebr_fat32, _) => {
                    let cluster = ebr_fat32.root_cluster;
                    self.process_normal_dir(cluster)
                }
            },
            BootRecord::ExFAT(_boot_record_exfat) => todo!(),
        }
    }

    fn process_normal_dir(
        &mut self,
        mut data_cluster: u32,
    ) -> FSResult<Vec<RawProperties>, S::Error> {
        let mut entry_parser = EntryParser::default();

        'outer: loop {
            // FAT specification, section 6.7
            let first_sector_of_cluster = self.data_cluster_to_partition_sector(data_cluster);
            for sector in first_sector_of_cluster
                ..(first_sector_of_cluster + self.sectors_per_cluster() as u32)
            {
                if entry_parser.parse_sector(sector, self)? {
                    break 'outer;
                }
            }

            // Read corresponding FAT entry
            let current_fat_entry = self.read_nth_FAT_entry(data_cluster)?;

            match current_fat_entry {
                // we are done here, break the loop
                FATEntry::Eof => break,
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

        Ok(entry_parser.finish())
    }

    /// Gets the next free cluster. Returns an IO [`Result`]
    /// If the [`Result`] returns [`Ok`] that contains a [`None`], the drive is full
    pub(crate) fn next_free_cluster(&mut self) -> Result<Option<u32>, S::Error> {
        let start_cluster = match self.boot_record {
            BootRecord::Fat(boot_record_fat) => {
                let mut first_free_cluster = self.first_free_cluster;

                if let Ebr::FAT32(_, fsinfo) = boot_record_fat.ebr {
                    // a value of u32::MAX denotes unawareness of the first free cluster
                    // we also do a bit of range checking
                    // TODO: if this is unknown, figure it out and write it to the FSInfo structure
                    if fsinfo.first_free_cluster != u32::MAX
                        && fsinfo.first_free_cluster <= self.props.total_sectors
                    {
                        first_free_cluster =
                            cmp::min(self.first_free_cluster, fsinfo.first_free_cluster);
                    }
                }

                first_free_cluster
            }
            BootRecord::ExFAT(_) => todo!("ExFAT not yet implemented"),
        };

        let mut current_cluster = start_cluster;

        while current_cluster < self.props.total_clusters {
            if self.read_nth_FAT_entry(current_cluster)? == FATEntry::Free {
                self.first_free_cluster = current_cluster;

                match &mut self.boot_record {
                    BootRecord::Fat(boot_record_fat) => {
                        if let Ebr::FAT32(_, fsinfo) = &mut boot_record_fat.ebr {
                            fsinfo.first_free_cluster = current_cluster;
                        }
                    }
                    BootRecord::ExFAT(_) => todo!("ExFAT not yet implemented"),
                }

                return Ok(Some(current_cluster));
            }
            current_cluster += 1;
        }

        self.first_free_cluster = self.props.total_clusters - 1;
        Ok(None)
    }

    /// Get the next cluster in a cluster chain, otherwise return [`None`]
    pub(crate) fn get_next_cluster(&mut self, cluster: u32) -> Result<Option<u32>, S::Error> {
        Ok(match self.read_nth_FAT_entry(cluster)? {
            FATEntry::Allocated(next_cluster) => Some(next_cluster),
            // when a `ROFile` is created, `cluster_chain_is_healthy` is called, if it fails, that ROFile is dropped
            _ => None,
        })
    }

    #[allow(non_snake_case)]
    /// Check whether or not the all the FAT tables of the storage medium are identical to each other
    pub(crate) fn FAT_tables_are_identical(&mut self) -> Result<bool, S::Error> {
        // we could make it work, but we are only testing regular FAT filesystems (for now)
        assert_ne!(
            self.fat_type,
            FATType::ExFAT,
            "this function doesn't work with ExFAT"
        );

        /// How many bytes to probe at max for each FAT per iteration (must be a multiple of [`MAX_SECTOR_SIZE`])
        const MAX_PROBE_SIZE: u32 = 1 << 20;

        let fat_byte_size = match self.boot_record {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.fat_sector_size(),
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

    #[allow(non_snake_case)]
    pub(crate) fn sector_belongs_to_FAT(&self, sector: u64) -> bool {
        match self.boot_record {
            BootRecord::Fat(boot_record_fat) => (boot_record_fat.first_fat_sector().into()
                ..boot_record_fat.first_root_dir_sector().into())
                .contains(&sector),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        }
    }

    /// Read the nth sector from the partition's beginning and store it in [`self.sector_buffer`](Self::sector_buffer)
    ///
    /// This function also returns an immutable reference to [`self.sector_buffer`](Self::sector_buffer)
    pub(crate) fn read_nth_sector(&mut self, n: u64) -> Result<&Vec<u8>, S::Error> {
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
    pub(crate) fn read_nth_FAT_entry(&mut self, n: u32) -> Result<FATEntry, S::Error> {
        // the size of an entry rounded up to bytes
        let entry_size = self.fat_type.entry_size();
        let entry_props = FATEntryProps::new(n, self);

        self.read_nth_sector(entry_props.fat_sector.into())?;

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
            self.read_nth_sector((entry_props.fat_sector + 1).into())?;

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
                #[allow(clippy::manual_range_patterns)]
                0xFF8..=0xFFE | 0xFFF => FATEntry::Eof,
                _ => {
                    if (0x002..(self.props.total_clusters + 1)).contains(&value) {
                        FATEntry::Allocated(value)
                    } else {
                        FATEntry::Reserved
                    }
                }
            },
            FATType::FAT16 => match value {
                0x0000 => FATEntry::Free,
                0xFFF7 => FATEntry::Bad,
                #[allow(clippy::manual_range_patterns)]
                0xFFF8..=0xFFFE | 0xFFFF => FATEntry::Eof,
                _ => {
                    if (0x0002..(self.props.total_clusters + 1)).contains(&value) {
                        FATEntry::Allocated(value)
                    } else {
                        FATEntry::Reserved
                    }
                }
            },
            FATType::FAT32 => match value {
                0x00000000 => FATEntry::Free,
                0x0FFFFFF7 => FATEntry::Bad,
                #[allow(clippy::manual_range_patterns)]
                0x0FFFFFF8..=0xFFFFFFE | 0x0FFFFFFF => FATEntry::Eof,
                _ => {
                    if (0x00000002..(self.props.total_clusters + 1)).contains(&value) {
                        FATEntry::Allocated(value)
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
impl<S> FileSystem<'_, S>
where
    S: Read + Write + Seek,
{
    #[allow(non_snake_case)]
    pub(crate) fn write_nth_FAT_entry(&mut self, n: u32, entry: FATEntry) -> Result<(), S::Error> {
        // the size of an entry rounded up to bytes
        let entry_size = self.fat_type.entry_size();
        let entry_props = FATEntryProps::new(n, self);

        // the previous solution would overflow, here's a correct implementation
        let mask = utils::bits::setbits_u32(self.fat_type.bits_per_entry());
        let mut value: u32 = u32::from(entry.clone()) & mask;

        if self.fat_type == FATType::FAT32 {
            // in FAT32, the high 4 bits are unused
            value &= 0x0FFFFFFF;
        }

        match self.fat_type {
            FATType::FAT12 => {
                let should_shift = n & 1 != 0;
                if should_shift {
                    // FAT12 entries are split between different bytes
                    value <<= 4;
                }

                self.read_nth_sector(entry_props.fat_sector.into())?;

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
                    self.read_nth_sector((entry_props.fat_sector + 1).into())?;
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
            FATType::FAT16 | FATType::FAT32 => {
                self.read_nth_sector(entry_props.fat_sector.into())?;

                let value_bytes = value.to_le_bytes();

                self.sector_buffer
                    [entry_props.sector_offset..entry_props.sector_offset + entry_size as usize]
                    .copy_from_slice(&value_bytes[..entry_size as usize]); // this shouldn't panic
                self.buffer_modified = true;
            }
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        };

        if entry == FATEntry::Free && n < self.first_free_cluster {
            self.first_free_cluster = n;
        }

        // lastly, update the FSInfoFAT32 structure is it is available
        if let BootRecord::Fat(boot_record_fat) = &mut self.boot_record {
            if let Ebr::FAT32(_, fsinfo) = &mut boot_record_fat.ebr {
                match entry {
                    FATEntry::Free => {
                        fsinfo.free_cluster_count += 1;
                        if n < fsinfo.first_free_cluster {
                            fsinfo.first_free_cluster = n;
                        }
                    }
                    _ => fsinfo.free_cluster_count -= 1,
                };
            }
        }

        Ok(())
    }

    /// Mark the individual entries of a contiguous FAT entry chain as unused
    ///
    /// Note: No validation is done to check whether or not the chain is valid
    pub(crate) fn remove_entry_chain(&mut self, chain: &DirEntryChain) -> Result<(), S::Error> {
        // we begin by removing the corresponding entries...
        let mut entries_freed = 0;
        let mut current_offset = chain.location.index;

        // current_cluster_option is `None` if we are dealing with a root directory entry
        let (mut current_sector, current_cluster_option): (u32, Option<u32>) =
            match chain.location.unit {
                EntryLocationUnit::RootDirSector(root_dir_sector) => (
                    (root_dir_sector + self.props.first_root_dir_sector).into(),
                    None,
                ),
                EntryLocationUnit::DataCluster(data_cluster) => (
                    self.data_cluster_to_partition_sector(data_cluster),
                    Some(data_cluster),
                ),
            };

        while entries_freed < chain.len {
            if current_sector as u64 != self.stored_sector {
                self.read_nth_sector(current_sector.into())?;
            }

            // we won't even bother zeroing the entire thing, just the first byte
            let byte_offset = current_offset as usize * DIRENTRY_SIZE;
            self.sector_buffer[byte_offset] = UNUSED_ENTRY;
            self.buffer_modified = true;

            log::trace!(
                "freed entry at sector {} with byte offset {}",
                current_sector,
                byte_offset
            );

            if current_offset + 1 >= (self.sector_size() / DIRENTRY_SIZE as u32) {
                // we have moved to a new sector
                current_sector += 1;

                if let Some(mut current_cluster) = current_cluster_option {
                    // data region
                    if self.partition_sector_to_data_cluster(current_sector) != current_cluster {
                        current_cluster = self.get_next_cluster(current_cluster)?.unwrap();
                        current_sector = self.data_cluster_to_partition_sector(current_cluster);
                    }
                }

                current_offset = 0;
            } else {
                current_offset += 1
            }

            entries_freed += 1;
        }

        Ok(())
    }

    /// Frees all the cluster in a cluster chain starting with `first_cluster`
    pub(crate) fn free_cluster_chain(&mut self, first_cluster: u32) -> Result<(), S::Error> {
        let mut current_cluster = first_cluster;

        loop {
            let next_cluster_option = self.get_next_cluster(current_cluster)?;

            // free the current cluster
            self.write_nth_FAT_entry(current_cluster, FATEntry::Free)?;

            // proceed to the next one, otherwise break
            match next_cluster_option {
                Some(next_cluster) => current_cluster = next_cluster,
                None => break,
            }
        }

        Ok(())
    }

    /// Syncs `self.sector_buffer` back to the storage
    fn _sync_current_sector(&mut self) -> Result<(), S::Error> {
        self.storage.write_all(&self.sector_buffer)?;
        self.storage
            .seek(SeekFrom::Current(-i64::from(self.props.sector_size)))?;

        Ok(())
    }

    /// Syncs a FAT sector to ALL OTHER FAT COPIES on the device medium
    #[allow(non_snake_case)]
    fn _sync_FAT_sector(&mut self, fat_sector_props: &FATSectorProps) -> Result<(), S::Error> {
        let current_offset = self.storage.stream_position()?;

        for sector in fat_sector_props.get_corresponding_FAT_sectors(self) {
            self.storage
                .seek(SeekFrom::Start(sector * u64::from(self.props.sector_size)))?;
            self.storage.write_all(&self.sector_buffer)?;
        }

        self.storage.seek(SeekFrom::Start(current_offset))?;

        Ok(())
    }

    pub(crate) fn sync_sector_buffer(&mut self) -> Result<(), S::Error> {
        self._raise_io_rw_result()?;

        if self.buffer_modified {
            if let Some(fat_sector_props) = FATSectorProps::new(self.stored_sector, self) {
                log::trace!("syncing FAT sector {}", fat_sector_props.sector_offset,);
                match self.boot_record {
                    BootRecord::Fat(boot_record_fat) => match boot_record_fat.ebr {
                        Ebr::FAT12_16(_) => {
                            self._sync_FAT_sector(&fat_sector_props)?;
                        }
                        Ebr::FAT32(ebr_fat32, _) => {
                            if ebr_fat32.extended_flags.mirroring_disabled() {
                                self._sync_current_sector()?;
                            } else {
                                self._sync_FAT_sector(&fat_sector_props)?;
                            }
                        }
                    },
                    BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
                }
            } else {
                log::trace!("syncing sector {}", self.stored_sector);
                self._sync_current_sector()?;
            }
        }
        self.buffer_modified = false;

        Ok(())
    }

    /// Returns an `Err` of `Unexpected [`IOErrorKind`]
    /// if the device medium is read-only
    fn _raise_io_rw_result(&mut self) -> Result<(), S::Error> {
        if !utils::io::storage_medium_is_rw(&mut self.storage)? {
            return Err(S::Error::new(
                <S::Error as IOError>::Kind::new_unsupported(),
                "the storage medium is read-only",
            ));
        }

        Ok(())
    }
}

/// Public [`Read`]-related functions
impl<'a, S> FileSystem<'a, S>
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

        for dir_name in path.clone() {
            let dir_cluster = match entries.iter().find(|entry| {
                entry.name == dir_name && entry.attributes.contains(RawAttributes::DIRECTORY)
            }) {
                Some(entry) => entry.data_cluster,
                None => {
                    log::error!("Directory {} not found", path);
                    return Err(FSError::NotFound);
                }
            };

            entries = self.process_normal_dir(dir_cluster)?;
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
    pub fn get_ro_file(&mut self, path: PathBuf) -> FSResult<ROFile<'_, 'a, S>, S::Error> {
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
impl<'a, S> FileSystem<'a, S>
where
    S: Read + Write + Seek,
{
    /// Remove a [`RWFile`] from the filesystem
    ///
    /// This is an alias to `self.get_rw_file(path)?.remove()?`
    #[inline]
    pub fn remove_file(&mut self, path: PathBuf) -> FSResult<(), S::Error> {
        self.get_rw_file(path)?.remove()?;

        Ok(())
    }

    /// Remove an empty directory from the filesystem
    ///
    /// Errors if the path provided points to the root directory
    pub fn remove_empty_dir(&mut self, path: PathBuf) -> FSResult<(), S::Error> {
        if path.is_malformed() {
            return Err(FSError::MalformedPath);
        }

        if !path.is_dir() {
            log::error!("Not a directory");
            return Err(FSError::NotADirectory);
        }

        if path == PathBuf::new() {
            // we are in the root directory, we can't remove it
            return Err(S::Error::new(
                <S::Error as IOError>::Kind::new_unsupported(),
                "We can't remove the root directory",
            )
            .into());
        }

        let dir_entries = self.read_dir(path.clone())?;

        if dir_entries.len() > NONROOT_MIN_DIRENTRIES {
            return Err(FSError::DirectoryNotEmpty);
        }

        let parent_path = path.parent();

        let parent_dir_entries = self.read_dir(parent_path)?;

        let entry = parent_dir_entries
            .iter()
            .find(|entry| entry.path == path)
            .ok_or(FSError::NotFound)?;

        // we first clear the corresponding entry chain in the parent directory
        self.remove_entry_chain(&entry.chain)?;

        // then we remove the allocated cluster chain
        self.free_cluster_chain(entry.data_cluster)?;

        Ok(())
    }

    /// Get a corresponding [`RWFile`] object from a [`PathBuf`]
    ///
    /// Borrows `&mut self` until that [`RWFile`] object is dropped, effectively locking `self` until that file closed
    ///
    /// Fails if `path` doesn't represent a file, or if that file doesn't exist
    pub fn get_rw_file(&mut self, path: PathBuf) -> FSResult<RWFile<'_, 'a, S>, S::Error> {
        self._raise_io_rw_result()?;

        let ro_file = self.get_ro_file(path)?;
        if ro_file.attributes.read_only {
            return Err(FSError::ReadOnlyFile);
        };

        Ok(RWFile { ro_file })
    }
}

impl<S> ops::Drop for FileSystem<'_, S>
where
    S: Read + Write + Seek,
{
    fn drop(&mut self) {
        // nothing to do if these error out while dropping
        let _ = self.sync_sector_buffer();
        let _ = self.storage.flush();
    }
}
