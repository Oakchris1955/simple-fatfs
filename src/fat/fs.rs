use super::*;

use crate::{error::*, path::*, utils, Clock};

use core::{
    cell::{Ref, RefCell, RefMut},
    cmp, iter, num, ops,
};

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use ::time;
use embedded_io::*;
use time::PrimitiveDateTime;
use zerocopy::{FromBytes, IntoBytes};

/// An enum representing different variants of the FAT filesystem
///
/// The logic is essentially the same in all of them, the only thing that
/// changes is the size in bytes of FAT entries, and thus the maximum volume size
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// no need for enum variant documentation here
pub enum FATType {
    /// One of the earliest versions, originally used all the way back to 1980.
    /// This probably won't be encountered anywhere outside ancient MS-DOS versions
    /// or pretty low-size volumes, like microcontrollers
    ///
    /// Max volume size: 8 MB
    FAT12,
    /// Used in many low-size volumes
    ///
    /// Min volume size: 8 MB,
    /// Max volume size: 16 GB
    FAT16,
    /// The most commonly-used variant.
    ///
    /// Min volume size: 256 MB,
    /// Max volume size: 16 TB
    FAT32,
    /// An ex-proprietory filesystem that allows for even larger storage sizes
    /// and its use is currently on the rise
    ///
    /// Not currently supported
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
    fn entry_size(&self) -> u8 {
        self.bits_per_entry().next_power_of_two() / 8
    }
}

// the first 2 entries are reserved
const RESERVED_FAT_ENTRIES: FATEntryCount = 2;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FATEntry {
    /// This cluster is free
    Free,
    /// This cluster is allocated and the next cluster is the contained value
    Allocated(ClusterIndex),
    /// This cluster is reserved
    Reserved,
    /// This is a bad (defective) cluster
    Bad,
    /// This cluster is allocated and is the final cluster of the file
    Eof,
}

impl From<FATEntry> for FATEntryValue {
    fn from(value: FATEntry) -> Self {
        Self::from(&value)
    }
}

impl From<&FATEntry> for FATEntryValue {
    fn from(value: &FATEntry) -> Self {
        match value {
            FATEntry::Free => FATEntryValue::MIN,
            FATEntry::Allocated(cluster) => *cluster,
            FATEntry::Reserved => 0xFFFFFF6,
            FATEntry::Bad => 0xFFFFFF7,
            FATEntry::Eof => FATEntryValue::MAX,
        }
    }
}

/// Properties about the position of a [`FATEntry`] inside the FAT region
struct FATEntryProps {
    /// Each `n`th element of the vector points at the corrensponding sector at the (first) active FAT table
    fat_sector: SectorIndex,
    sector_offset: usize,
}

impl FATEntryProps {
    /// Get the [`FATEntryProps`] of the `n`-th [`FATEntry`] of a [`FileSystem`]
    pub fn new<S, C>(n: FATEntryIndex, fs: &FileSystem<S, C>) -> Self
    where
        S: BlockRead,
        C: Clock,
    {
        let fat_byte_offset: u64 = u64::from(n) * u64::from(fs.fat_type.bits_per_entry()) / 8;
        let fat_sector = SectorIndex::try_from(
            u64::from(fs.props.first_fat_sector)
                + fat_byte_offset / u64::from(fs.props.sector_size),
        )
        .expect("this should fit into a u32");
        let sector_offset: usize =
            usize::try_from(fat_byte_offset % u64::from(fs.props.sector_size))
                .expect("this should fit into a usize");

        FATEntryProps {
            fat_sector,
            sector_offset,
        }
    }
}

/// the BPB_NumFATs field is 1 byte wide (u8)
pub(crate) type FATOffset = u8;

/// Properties about the position of a sector within the FAT
struct FATSectorProps {
    /// the sector belongs to this FAT copy
    #[expect(unused)]
    fat_offset: FATOffset,
    /// the sector is that many away from the start of the FAT copy
    sector_offset: SectorIndex,
}

impl FATSectorProps {
    /// Returns [`None`] if this sector doesn't belong to a FAT table
    pub fn new<S, C>(sector: SectorIndex, fs: &FileSystem<S, C>) -> Option<Self>
    where
        S: BlockRead,
        C: Clock,
    {
        if !fs.sector_belongs_to_FAT(sector) {
            return None;
        }

        let sector_offset_from_first_fat = sector - SectorIndex::from(fs.props.first_fat_sector);
        let fat_offset =
            FATOffset::try_from(sector_offset_from_first_fat / fs.props.fat_sector_size)
                .expect("this should fit in a u89");
        let sector_offset = sector_offset_from_first_fat % fs.props.fat_sector_size;

        Some(FATSectorProps {
            fat_offset,
            sector_offset,
        })
    }

    #[expect(non_snake_case)]
    pub fn get_corresponding_FAT_sectors<S, C>(&self, fs: &FileSystem<S, C>) -> Box<[SectorIndex]>
    where
        S: BlockRead,
        C: Clock,
    {
        let mut vec = Vec::with_capacity(fs.props.fat_table_count.into());

        for i in 0..fs.props.fat_table_count {
            vec.push(
                SectorIndex::from(fs.props.first_fat_sector)
                    + SectorCount::from(i) * fs.props.fat_sector_size
                    + self.sector_offset,
            )
        }

        vec.into_boxed_slice()
    }
}

#[derive(Debug)]
pub(crate) struct DirInfo {
    pub(crate) path: PathBuf,
    pub(crate) chain_start: EntryLocationUnit,
    /// Indicates the [`EntryLocation`] of the last known allocated or removed [`DirEntry`]
    ///
    /// [`None`] if it is not known
    pub(crate) chain_end: Option<EntryLocation>,
    // we box that to save space if it is None (as of writing this,
    // the Bloom struct occupies 184 bytes in-memory)
    #[cfg(feature = "bloom")]
    pub(crate) filter: Option<utils::bloom::Bloom<str>>,
}

impl DirInfo {
    pub(crate) fn at_root_dir(boot_record: &BootRecord) -> Self {
        DirInfo {
            // this is basically the root directory
            path: PathBuf::from(path_consts::SEPARATOR_STR),
            chain_start: match boot_record {
                BootRecord::Fat(boot_record_fat) => match &boot_record_fat.ebr {
                    // it doesn't really matter what value we put in here, since we won't be using it
                    Ebr::FAT12_16(_ebr_fat12_16) => EntryLocationUnit::RootDirSector(0),
                    Ebr::FAT32(ebr_fat32, _) => {
                        EntryLocationUnit::DataCluster(ebr_fat32.root_cluster.get())
                    }
                },
                BootRecord::ExFAT(_boot_record_exfat) => todo!(),
            },
            chain_end: None,
            #[cfg(feature = "bloom")]
            filter: None,
        }
    }
}

impl<S, C> iter::FusedIterator for ReadDir<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
}

pub(crate) trait OffsetConversions {
    fn sector_size(&self) -> u16;
    fn cluster_size(&self) -> u32;
    fn first_data_sector(&self) -> SectorIndex;

    #[inline]
    fn cluster_to_sector(&self, cluster: ClusterIndex) -> SectorIndex {
        cluster * ClusterIndex::from(self.sectors_per_cluster())
    }

    #[inline]
    fn sectors_per_cluster(&self) -> u8 {
        (self.cluster_size() / u32::from(self.sector_size()))
            .try_into()
            .expect("the SecPerClus field is 1 byte long (u8)")
    }

    #[inline]
    fn data_cluster_to_partition_sector(&self, cluster: ClusterIndex) -> SectorIndex {
        self.cluster_to_sector(cluster - RESERVED_FAT_ENTRIES) + self.first_data_sector()
    }

    #[inline]
    fn partition_sector_to_data_cluster(&self, sector: SectorIndex) -> ClusterIndex {
        (sector - self.first_data_sector()) / ClusterIndex::from(self.sectors_per_cluster())
            + RESERVED_FAT_ENTRIES
    }
}

impl<S, C> OffsetConversions for FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    #[inline]
    fn sector_size(&self) -> u16 {
        self.props.sector_size
    }

    #[inline]
    fn cluster_size(&self) -> u32 {
        self.props.cluster_size
    }

    #[inline]
    fn first_data_sector(&self) -> SectorIndex {
        self.props.first_data_sector
    }

    #[inline]
    fn sectors_per_cluster(&self) -> u8 {
        self.props.sec_per_clus
    }
}

/// Some generic properties common across all FAT versions, like a sector's size, are cached here
#[derive(Debug)]
pub(crate) struct FSProperties {
    pub(crate) sector_size: u16,
    pub(crate) cluster_size: u32,
    pub(crate) sec_per_clus: u8,
    pub(crate) total_sectors: SectorCount,
    pub(crate) total_clusters: ClusterCount,
    /// sector offset of the FAT
    pub(crate) fat_table_count: u8,
    pub(crate) fat_sector_size: u32,
    pub(crate) first_fat_sector: u16,
    pub(crate) first_root_dir_sector: SectorIndex,
    pub(crate) first_data_sector: SectorIndex,
}

impl From<&BootRecord> for FSProperties {
    fn from(value: &BootRecord) -> Self {
        let sector_size = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.bpb.bytes_per_sector.get(),
            BootRecord::ExFAT(boot_record_exfat) => 1 << boot_record_exfat.sector_shift,
        };
        let cluster_size = match value {
            BootRecord::Fat(boot_record_fat) => {
                u32::from(boot_record_fat.bpb.sectors_per_cluster) * u32::from(sector_size)
            }
            BootRecord::ExFAT(boot_record_exfat) => {
                1 << (boot_record_exfat.sector_shift + boot_record_exfat.cluster_shift)
            }
        };
        let sec_per_clus = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.bpb.sectors_per_cluster,
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let total_sectors = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.total_sectors(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let total_clusters = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.total_clusters(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let fat_table_count = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.bpb.table_count,
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let fat_sector_size = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.fat_sector_size(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };
        let first_fat_sector = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.first_fat_sector(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };
        let first_root_dir_sector = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.first_root_dir_sector(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };
        let first_data_sector = match value {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.first_data_sector(),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT is not yet implemented"),
        };

        FSProperties {
            sector_size,
            cluster_size,
            sec_per_clus,
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
#[derive(Debug, Clone)]
pub(crate) struct FileFilter {
    show_hidden: bool,
    show_systen: bool,
}

impl FileFilter {
    pub(crate) fn filter(&self, item: &RawProperties) -> bool {
        let is_hidden = item.attributes.contains(RawAttributes::HIDDEN);
        let is_system = item.attributes.contains(RawAttributes::SYSTEM);
        let should_filter = !self.show_hidden && is_hidden || !self.show_systen && is_system;

        !should_filter
    }
}

#[expect(clippy::derivable_impls)]
impl Default for FileFilter {
    fn default() -> Self {
        // The FAT spec says to filter everything by default
        FileFilter {
            show_hidden: false,
            show_systen: false,
        }
    }
}

type SyncSectorBufferFn<S, C> = fn(&FileSystem<S, C>) -> Result<(), <S as ErrorType>::Error>;
type UnmountFn<S, C> = fn(&FileSystem<S, C>) -> FSResult<(), <S as ErrorType>::Error>;

/// Determine the sector size of a FAT filesystem without fully constructing it.
// This is essentially a copy of the beginning of `FileSystem::new`
pub fn determine_fs_sector_size<S>(mut storage: S) -> FSResult<u16, S::Error>
where
    S: BlockRead,
{
    let block_size = storage.block_size();

    if !block_size.is_power_of_two() {
        // block size is 0 or not a power of 2
        return Err(FSError::InternalFSError(InternalFSError::BlockSizeError));
    }
    #[expect(clippy::cast_possible_truncation)]
    if block_size > MAX_SECTOR_SIZE as BlockSize {
        // block size is larger than MAX_SECTOR_SIZE
        return Err(FSError::InternalFSError(InternalFSError::BlockSizeError));
    }

    // Begin by reading the boot record
    let mut buffer = vec![0_u8; block_size.try_into().unwrap()].into_boxed_slice();

    storage.read(0, &mut buffer)?;

    let (bpb, _) = BpbFat::ref_from_prefix(&buffer).unwrap();

    Ok(bpb.bytes_per_sector.into())
}

/// An API to process a FAT filesystem
#[derive(Debug)]
pub struct FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    /// Any struct that implements the [`BlockRead`], and optionally [`BlockWrite`] traits
    storage: RefCell<S>,

    /// The length of this will be the sector size of the FS for all FAT types except FAT12, in that case, it will be double that value
    pub(crate) sector_buffer: RefCell<SectorBuffer<true>>,
    fsinfo_modified: RefCell<bool>,
    boot_sector_modified: RefCell<bool>,

    pub(crate) dir_info: RefCell<DirInfo>,

    sync_f: RefCell<Option<SyncSectorBufferFn<S, C>>>,
    unmount_f: RefCell<Option<UnmountFn<S, C>>>,

    pub(crate) options: FSOptions<C>,

    pub(crate) boot_record: RefCell<BootRecord>,
    // since `self.boot_record.fat_type()` calls like 5 nested functions, we keep this cached and expose it with a public getter function
    fat_type: FATType,
    pub(crate) props: FSProperties,
    // this doesn't mean that this is the first free cluster, it just means
    // that if we want to figure that out, we should start from this cluster
    first_free_cluster: RefCell<ClusterIndex>,

    pub(crate) filter: RefCell<FileFilter>,
}

/// Getter functions
impl<S, C> FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    /// What is the [`FATType`] of the filesystem
    pub fn fat_type(&self) -> FATType {
        self.fat_type
    }
}

/// Setter functions
impl<S, C> FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    /// Whether or not to list hidden files
    ///
    /// Off by default
    #[inline]
    pub fn show_hidden(&self, show: bool) {
        self.filter.borrow_mut().show_hidden = show;
    }

    /// Whether or not to list system files
    ///
    /// Off by default
    #[inline]
    pub fn show_system(&self, show: bool) {
        self.filter.borrow_mut().show_systen = show;
    }
}

/// Constructors for a [`FileSystem`]
impl<S, C> FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    /// Create a [`FileSystem`] from a storage object
    ///
    /// Fails if the storage is way too small to support a FAT filesystem.
    /// For most use cases, that shouldn't be an issue, you can just call [`.unwrap()`](Result::unwrap)
    pub fn new(mut storage: S, options: FSOptions<C>) -> FSResult<Self, S::Error> {
        let block_size = storage.block_size();

        if !block_size.is_power_of_two() {
            // block size is 0 or not a power of 2
            return Err(FSError::InternalFSError(InternalFSError::BlockSizeError));
        }
        #[expect(clippy::cast_possible_truncation)]
        if block_size > MAX_SECTOR_SIZE as BlockSize {
            // block size is larger than MAX_SECTOR_SIZE
            return Err(FSError::InternalFSError(InternalFSError::BlockSizeError));
        }

        // Begin by reading the boot record
        // We don't know the sector size yet, so we just go with the biggest possible one for now
        let buffer = SectorBuffer::new(&mut storage)?;

        let (bpb, _) = BpbFat::read_from_prefix(&buffer).unwrap();

        if block_size > BlockSize::from(bpb.bytes_per_sector) {
            // block size is larger than sector size
            return Err(FSError::InternalFSError(InternalFSError::BlockSizeError));
        }

        let mut buffer = buffer.init(&mut storage, bpb.bytes_per_sector.get())?;
        let storage = RefCell::from(storage);

        let ebr = if bpb.table_size_16 == 0 {
            let (ebr_fat32, _) = EBRFAT32::read_from_prefix(&buffer[BPBFAT_SIZE..]).unwrap();

            buffer.read(&storage, ebr_fat32.fat_info.into())?;

            let fsinfo = FSInfoFAT32::read_from_bytes(&buffer).unwrap();

            if !fsinfo.verify_signature() {
                log::error!("FAT32 FSInfo has invalid signature(s)");
                return Err(FSError::InternalFSError(InternalFSError::InvalidFSInfoSig));
            }

            Ebr::FAT32(ebr_fat32, fsinfo)
        } else {
            Ebr::FAT12_16(
                FromBytes::read_from_prefix(&buffer[BPBFAT_SIZE..])
                    .unwrap()
                    .0,
            )
        };

        // TODO: see how we will handle this for exfat
        let boot_record = BootRecord::Fat(BootRecordFAT { bpb, ebr });

        // verify boot record signature
        let fat_type = boot_record.fat_type();

        if fat_type == FATType::ExFAT {
            log::error!("Filesystem is ExFAT, which is currently unsupported");
            return Err(FSError::UnsupportedFS);
        }

        log::info!("The FAT type of the filesystem is {fat_type:?}");

        match &boot_record {
            BootRecord::Fat(boot_record_fat) => {
                if options.check_boot_signature && boot_record_fat.verify_signature() {
                    log::error!("FAT boot record has invalid signature(s)");
                    return Err(FSError::InternalFSError(InternalFSError::InvalidBPBSig));
                }
            }
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };

        let props = FSProperties::from(&boot_record);

        #[cfg_attr(feature = "lba64", expect(clippy::useless_conversion))]
        if u64::from(props.total_sectors) * u64::from(props.sector_size)
            > u64::from(storage.borrow().block_count()) * u64::from(block_size)
        {
            log::error!("the filesystem seems to be larger than the storage medium");
            return Err(FSError::InternalFSError(InternalFSError::StorageTooSmall));
        }

        let fs = Self {
            storage,
            sector_buffer: buffer.into(),
            fsinfo_modified: false.into(),
            boot_sector_modified: false.into(),
            options,
            dir_info: DirInfo::at_root_dir(&boot_record).into(),
            sync_f: None.into(),
            unmount_f: None.into(),
            boot_record: boot_record.into(),
            fat_type,
            props,
            first_free_cluster: RESERVED_FAT_ENTRIES.into(),
            filter: FileFilter::default().into(),
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
impl<S, C> FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    pub(crate) fn process_current_dir<'a>(&'a self) -> ReadDirInt<'a, S, C> {
        ReadDirInt::new(self, &self.dir_info.borrow().chain_start)
    }

    /// Goes to the parent directory.
    ///
    /// If this is the root directory, it does nothing
    fn _go_to_parent_dir(&self) -> FSResult<(), S::Error> {
        if let Some(parent_path) = self.dir_info.borrow().path.parent() {
            let parent_pathbuf = parent_path.to_path_buf();

            let mut entries = self.process_current_dir();

            // the PARENT DIR entry is always second on a directory
            // other than the root directory
            let parent_entry = entries
                .nth(NONROOT_MIN_DIRENTRIES - 1)
                .transpose()?
                .filter(|entry| entry.is_dir && entry.sfn == PARENT_DIR_SFN)
                .ok_or(FSError::InternalFSError(
                    InternalFSError::MalformedEntryChain,
                ))?;

            self.dir_info.borrow_mut().path = parent_pathbuf;
            self.dir_info.borrow_mut().chain_start =
                EntryLocationUnit::DataCluster(parent_entry.data_cluster);
            self.dir_info.borrow_mut().chain_end = None;
        } else {
            self._go_to_root_directory();
        }

        Ok(())
    }

    /// Goes to the given child directory
    ///
    /// If it doesn't exist, the encapsulated [`Option`] will be `None`
    fn _go_to_child_dir(&self, name: &str) -> FSResult<(), S::Error> {
        let mut entries = self.process_current_dir();

        let child_entry = loop {
            let entry = entries.next().ok_or(FSError::NotFound)??;

            if entry.name(self.options.codepage) == name {
                break entry;
            }
        };

        if !child_entry.is_dir {
            return Err(FSError::NotADirectory);
        }

        self.dir_info
            .borrow_mut()
            .path
            .push(child_entry.name(self.options.codepage));
        self.dir_info.borrow_mut().chain_start =
            EntryLocationUnit::DataCluster(child_entry.data_cluster);
        self.dir_info.borrow_mut().chain_end = None;

        Ok(())
    }

    fn _go_to_root_directory(&self) {
        self.dir_info
            .replace(DirInfo::at_root_dir(&self.boot_record.borrow()));
    }

    // This is a helper function for `go_to_dir`
    fn _go_up_till_target<P>(&self, target: P) -> FSResult<(), S::Error>
    where
        P: AsRef<Path>,
    {
        let target = target.as_ref();

        while self.dir_info.borrow().path != target {
            self._go_to_parent_dir()?;
        }

        Ok(())
    }

    // This is a helper function for `go_to_dir`
    fn _go_down_till_target<P>(&self, target: P) -> FSResult<(), S::Error>
    where
        P: AsRef<Path>,
    {
        let target = target.as_ref();

        let common_path_prefix = find_common_path_prefix(&self.dir_info.borrow().path, target);
        let common_components = common_path_prefix
            .normalize()
            .components()
            .filter(keep_path_normals)
            .count();

        for dir_name in target
            .components()
            .filter(keep_path_normals)
            .skip(common_components)
        {
            self._go_to_child_dir(dir_name.as_str())?;
        }

        Ok(())
    }

    /// Make sure that the sector stored in the sector buffer is the same as
    /// the first sector of cached directory chain
    fn _go_to_cached_dir(&self) -> FSResult<(), S::Error> {
        let dir_chain = self.dir_info.borrow().chain_start;
        let target_sector = dir_chain.get_entry_sector(self);

        if target_sector != self.sector_buffer.borrow().stored_sector() {
            self.load_nth_sector(target_sector)?;
        }

        Ok(())
    }

    // There are many ways this can be achieved. That's how we'll do it:
    // Firstly, we find the common path prefix of the `current_path` and the `target`
    // Then, we check whether it is faster to start from the root directory
    // and get down to the target or whether we should start from where we are
    // now, go up till we find the common prefix path and then go down to the `target`

    /// Navigates to the `target` [`Path`]
    pub(crate) fn go_to_dir<P>(&self, target: P) -> FSResult<(), S::Error>
    where
        P: AsRef<Path>,
    {
        let target = target.as_ref();

        if !target.is_valid() {
            return Err(FSError::MalformedPath);
        }

        if self.dir_info.borrow().path == target {
            // there's a chance that the current loaded sector doesn't belong
            // to the directory we have cached, so we must also navigate to the correct sector
            self._go_to_cached_dir()?;

            return Ok(());
        }

        let common_path_prefix = find_common_path_prefix(&self.dir_info.borrow().path, target);

        // Note: these are the distances to the common prefix, not the target path
        let distance_from_root = common_path_prefix.ancestors().count() - 1;
        let distance_from_current_path =
            (self.dir_info.borrow().path.ancestors().count() - 1) - distance_from_root;

        if distance_from_root <= distance_from_current_path {
            self._go_to_root_directory();

            self._go_down_till_target(target)?;
        } else {
            self._go_up_till_target(common_path_prefix)?;

            self._go_down_till_target(target)?;
        }

        // this should be covered by all the other functions above, but it probably doesn't hurt
        // (if this was the same directory (which could be cached), we would have return long ago)
        #[cfg(feature = "bloom")]
        {
            self.dir_info.borrow_mut().filter = None;
        }

        Ok(())
    }

    /// Gets the next free cluster. Returns an IO [`Result`]
    /// If the [`Result`] returns [`Ok`] that contains a [`None`], the drive is full
    pub(crate) fn next_free_cluster(&self) -> Result<Option<ClusterIndex>, S::Error> {
        let start_cluster = match *self.boot_record.borrow() {
            BootRecord::Fat(ref boot_record_fat) => {
                let mut first_free_cluster = *self.first_free_cluster.borrow();

                if let Ebr::FAT32(_, fsinfo) = &boot_record_fat.ebr {
                    // a value of u32::MAX denotes unawareness of the first free cluster
                    // we also do a bit of range checking
                    // TODO: if this is unknown, figure it out and write it to the FSInfo structure
                    if fsinfo.first_free_cluster != ClusterIndex::MAX
                        && fsinfo.first_free_cluster <= self.props.total_sectors
                    {
                        first_free_cluster =
                            cmp::min(first_free_cluster, fsinfo.first_free_cluster.get());
                    }
                }

                first_free_cluster
            }
            BootRecord::ExFAT(_) => todo!("ExFAT not yet implemented"),
        };

        let mut current_cluster = start_cluster;

        while current_cluster < self.props.total_clusters {
            if self.read_nth_FAT_entry(current_cluster)? == FATEntry::Free {
                self.first_free_cluster.replace(current_cluster);

                match *self.boot_record.borrow_mut() {
                    BootRecord::Fat(ref mut boot_record_fat) => {
                        if let Ebr::FAT32(_, fsinfo) = &mut boot_record_fat.ebr {
                            fsinfo.first_free_cluster = current_cluster.into();
                            self.fsinfo_modified.replace(true);
                        }
                    }
                    BootRecord::ExFAT(_) => todo!("ExFAT not yet implemented"),
                }

                return Ok(Some(current_cluster));
            }
            current_cluster += 1;
        }

        self.first_free_cluster
            .replace(self.props.total_clusters - 1);
        Ok(None)
    }

    /// Get the next cluster in a cluster chain, otherwise return [`None`]
    pub(crate) fn get_next_cluster(
        &self,
        cluster: ClusterIndex,
    ) -> Result<Option<ClusterIndex>, S::Error> {
        Ok(match self.read_nth_FAT_entry(cluster)? {
            FATEntry::Allocated(next_cluster) => Some(next_cluster),
            // when a `ROFile` is created, `cluster_chain_is_healthy` is called, if it fails, that ROFile is dropped
            _ => None,
        })
    }

    #[expect(non_snake_case)]
    /// Check whether or not the all the FAT tables of the storage medium are identical to each other
    pub(crate) fn FAT_tables_are_identical(&self) -> Result<bool, S::Error> {
        // we could make it work, but we are only testing regular FAT filesystems (for now)
        assert_ne!(
            self.fat_type,
            FATType::ExFAT,
            "this function doesn't work with ExFAT"
        );

        /// How many bytes to probe at max for each FAT per iteration (must be a multiple of [`MAX_SECTOR_SIZE`])
        const MAX_PROBE_SIZE: u32 = 1 << 20;
        let max_probe_size_in_sectors: u32 = MAX_PROBE_SIZE / u32::from(self.sector_size());

        let fat_byte_size = match &*self.boot_record.borrow() {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.fat_sector_size(),
            BootRecord::ExFAT(_) => unreachable!(),
        };

        for nth_iteration in 0..fat_byte_size.div_ceil(MAX_PROBE_SIZE) {
            let mut tables: Vec<Vec<u8>> = Vec::new();

            for i in 0..self.props.fat_table_count {
                let current_offset = self.boot_record.borrow().nth_FAT_table_sector(i)
                    + nth_iteration * max_probe_size_in_sectors;
                let bytes_left = fat_byte_size - nth_iteration * max_probe_size_in_sectors;

                let bytes_to_check =
                    usize::try_from(cmp::min(MAX_PROBE_SIZE, bytes_left)).unwrap_or(usize::MAX);

                // ensure it's a multiple of sector size
                let bytes_to_read = bytes_to_check.div_ceil(self.props.sector_size.into())
                    * usize::from(self.props.sector_size);

                let mut buf = vec![0_u8; bytes_to_read];

                self.sector_buffer
                    .borrow()
                    .read_into(&self.storage, current_offset, &mut buf)?;

                // truncate in case less than a sector has to be read
                buf.truncate(bytes_to_check);

                tables.push(buf);
            }

            // we check each table with the first one (except the first one ofc)
            if !tables.iter().skip(1).all(|buf| buf == &tables[0]) {
                return Ok(false);
            }
        }

        Ok(true)
    }

    #[expect(non_snake_case)]
    pub(crate) fn sector_belongs_to_FAT(&self, sector: SectorIndex) -> bool {
        match &*self.boot_record.borrow() {
            BootRecord::Fat(boot_record_fat) => (boot_record_fat.first_fat_sector().into()
                ..boot_record_fat.first_root_dir_sector())
                .contains(&sector),
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        }
    }

    /// Read the nth sector from the partition's beginning and store it in [`self.sector_buffer`](Self::sector_buffer)
    ///
    /// This function also returns an immutable reference to [`self.sector_buffer`](Self::sector_buffer)
    pub(crate) fn load_nth_sector(&self, n: SectorIndex) -> Result<Ref<'_, [u8]>, S::Error> {
        if n >= self.props.total_sectors {
            panic!(concat!(
                "seeked past end of device medium. ",
                "This is most likely an internal error, please report it: ",
                "https://github.com/Oakchris1955/simple-fatfs/issues"
            ));
        }

        // nothing to do if the sector we wanna read is already cached
        let stored_sector = self.sector_buffer.borrow().stored_sector();
        if n != stored_sector {
            // let's sync the current sector first
            let sync_sector_option = *self.sync_f.borrow();
            if let Some(sync_sector_buffer) = sync_sector_option {
                log::debug!("Syncing sector {stored_sector}");

                sync_sector_buffer(self)?;

                // Now that we have synced the sector buffer, there's no reason
                // to sync it again if there have been no changes
                self.sync_f.replace(None);
            }

            self.sector_buffer.borrow_mut().read(&self.storage, n)?;
        }

        Ok(Ref::map(self.sector_buffer.borrow(), |s| &**s))
    }

    #[expect(non_snake_case)]
    pub(crate) fn read_nth_FAT_entry(&self, n: FATEntryIndex) -> Result<FATEntry, S::Error> {
        // the size of an entry rounded up to bytes
        let entry_size = self.fat_type.entry_size();
        let entry_props = FATEntryProps::new(n, self);

        self.load_nth_sector(entry_props.fat_sector)?;

        let mut value_bytes = [0_u8; 4];
        let bytes_to_read: usize = cmp::min(
            entry_props.sector_offset + usize::from(entry_size),
            usize::from(self.sector_size()),
        ) - entry_props.sector_offset;
        value_bytes[..bytes_to_read].copy_from_slice(
            &self.sector_buffer.borrow_mut()
                [entry_props.sector_offset..entry_props.sector_offset + bytes_to_read],
        ); // this shouldn't panic

        // in FAT12, FAT entries may be split between two different sectors
        if self.fat_type == FATType::FAT12 && bytes_to_read < usize::from(entry_size) {
            self.load_nth_sector(entry_props.fat_sector + 1)?;

            value_bytes[bytes_to_read..usize::from(entry_size)].copy_from_slice(
                &self.sector_buffer.borrow_mut()[..(usize::from(entry_size) - bytes_to_read)],
            );
        };

        let mut value = FATEntryValue::from_le_bytes(value_bytes);
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
                #[expect(clippy::manual_range_patterns)]
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
                #[expect(clippy::manual_range_patterns)]
                0xFFF8..=0xFFFE | 0xFFFF => FATEntry::Eof,
                _ => {
                    if (0x0002..(self.props.total_clusters + 1)).contains(&value) {
                        FATEntry::Allocated(value)
                    } else {
                        FATEntry::Reserved
                    }
                }
            },
            FATType::FAT32 => match value & 0x0FFFFFFF {
                0x00000000 => FATEntry::Free,
                0x0FFFFFF7 => FATEntry::Bad,
                #[expect(clippy::manual_range_patterns)]
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
impl<S, C> FileSystem<S, C>
where
    S: BlockWrite,
    C: Clock,
{
    #[expect(non_snake_case)]
    pub(crate) fn write_nth_FAT_entry(
        &self,
        n: FATEntryIndex,
        entry: FATEntry,
    ) -> Result<(), S::Error> {
        // the size of an entry rounded up to bytes
        let entry_size = self.fat_type.entry_size();
        let entry_props = FATEntryProps::new(n, self);

        // the previous solution would overflow, here's a correct implementation
        let mask = utils::bits::setbits_u32(self.fat_type.bits_per_entry());
        let mut value: FATEntryValue = FATEntryValue::from(entry.clone()) & mask;

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

                self.load_nth_sector(entry_props.fat_sector)?;

                let value_bytes = value.to_le_bytes();

                let mut first_byte = value_bytes[0];

                if should_shift {
                    let mut old_byte = self.sector_buffer.borrow()[entry_props.sector_offset];
                    // ignore the high 4 bytes of the old entry
                    old_byte &= 0x0F;
                    // OR it with the new value
                    first_byte |= old_byte;
                }

                self.sector_buffer.borrow_mut()[entry_props.sector_offset] = first_byte; // this shouldn't panic
                self.set_modified();

                let bytes_left_on_sector: usize = cmp::min(
                    usize::from(entry_size),
                    usize::from(self.sector_size()) - entry_props.sector_offset,
                );

                if bytes_left_on_sector < entry_size.into() {
                    // looks like this FAT12 entry spans multiple sectors, we must also update the other one
                    self.load_nth_sector(entry_props.fat_sector + 1)?;
                }

                let mut second_byte = value_bytes[1];
                let second_byte_index =
                    (entry_props.sector_offset + 1) % usize::from(self.sector_size());
                if !should_shift {
                    let mut old_byte = self.sector_buffer.borrow()[second_byte_index];
                    // ignore the low 4 bytes of the old entry
                    old_byte &= 0xF0;
                    // OR it with the new value
                    second_byte |= old_byte;
                }

                self.sector_buffer.borrow_mut()[second_byte_index] = second_byte; // this shouldn't panic
                self.set_modified();
            }
            FATType::FAT16 | FATType::FAT32 => {
                self.load_nth_sector(entry_props.fat_sector)?;

                let mut value_bytes = value.to_le_bytes();

                if self.fat_type == FATType::FAT32 {
                    // the high four bits must be preserved
                    let original_high_byte = self.sector_buffer.borrow()
                        [entry_props.sector_offset + usize::from(entry_size) - 1];
                    value_bytes[3] |= original_high_byte & 0xF0;
                }

                self.sector_buffer.borrow_mut()[entry_props.sector_offset
                    ..entry_props.sector_offset + usize::from(entry_size)]
                    .copy_from_slice(&value_bytes[..usize::from(entry_size)]); // this shouldn't panic
                self.set_modified();
            }
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        };

        if entry == FATEntry::Free && n < *self.first_free_cluster.borrow() {
            self.first_free_cluster.replace(n);
        }

        // lastly, update the FSInfoFAT32 structure is it is available
        if let BootRecord::Fat(boot_record_fat) = &mut *self.boot_record.borrow_mut() {
            if let Ebr::FAT32(_, fsinfo) = &mut boot_record_fat.ebr {
                match entry {
                    FATEntry::Free => {
                        fsinfo.free_cluster_count += 1;
                        if n < fsinfo.first_free_cluster.get() {
                            fsinfo.first_free_cluster = n.into();
                        }
                    }
                    _ => fsinfo.free_cluster_count -= 1,
                };
                self.fsinfo_modified.replace(true);
            }
        }

        Ok(())
    }

    /// Allocate room for at least `n` contiguous [`FATDirEntries`](FATDirEntry)
    /// in the current directory entry chain
    ///
    /// This may or may not allocate new clusters.
    pub(crate) fn allocate_nth_entries(
        &self,
        n: num::NonZero<EntryCount>,
    ) -> FSResult<EntryLocation, S::Error> {
        // navigate to the cached directory, if we aren't already there
        self._go_to_cached_dir()?;

        // we may not even need to allocate new entries.
        // let's check if there is a chain of unused entries big enough to be used
        let mut first_entry = self.dir_info.borrow().chain_end.unwrap_or_else(|| {
            let stored_sector = self.sector_buffer.borrow().stored_sector();
            EntryLocation::from_partition_sector(stored_sector, self)
        });

        let mut last_entry = first_entry;

        let mut chain_len = 0;
        let mut entry_count: EntryCount = 0;

        loop {
            let entry_status = last_entry.entry_status(self)?;
            entry_count += 1;
            match entry_status {
                EntryStatus::Unused | EntryStatus::LastUnused => chain_len += 1,
                EntryStatus::Used => chain_len = 0,
            }

            if chain_len >= n.get() {
                return Ok(first_entry);
            }

            if entry_status == EntryStatus::LastUnused {
                break;
            }

            // we also break if we have reached the end of the cluster chain
            // or else the MalformedEntryChain below will kick in
            if last_entry.unit.get_next_unit(self)?.is_none()
                && last_entry.index + 1 >= last_entry.unit.get_max_offset(self)
            {
                break;
            }

            // what if for whatever reason the data types changes?
            #[expect(clippy::absurd_extreme_comparisons)]
            if entry_count + n.get() >= DIRENTRY_LIMIT {
                // defragment the cluster chain just in case
                // this frees up any space for entries
                let new_entry_count = self.defragment_entry_chain()?;

                if new_entry_count + n.get() >= DIRENTRY_LIMIT {
                    return Err(FSError::DirEntryLimitReached);
                }

                // we have enough entries now, this call should make us
                // enter an infinite recursion
                return self.allocate_nth_entries(n);
            }

            last_entry = last_entry
                .next_entry(self)?
                .ok_or(FSError::InternalFSError(
                    InternalFSError::MalformedEntryChain,
                ))?;

            if entry_status == EntryStatus::Used {
                first_entry = last_entry;
            }
        }

        // let's set the last-known dir entry
        self.dir_info.borrow_mut().chain_end = Some(last_entry);

        // we have broken out of the loop, that means we reached the end of the chain
        // of the already-allocated entries
        match last_entry.unit {
            EntryLocationUnit::RootDirSector(_) => {
                let remaining_sectors: SectorCount = match &*self.boot_record.borrow() {
                    BootRecord::Fat(boot_record_fat) => {
                        boot_record_fat.first_root_dir_sector()
                            + SectorCount::from(boot_record_fat.root_dir_sectors())
                            - last_entry.unit.get_entry_sector(self)
                    }
                    BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
                };

                let entries_per_sector = self.props.sector_size
                    / u16::try_from(DIRENTRY_SIZE).expect("32 can fit in a u16");
                let remaining_entries = EntryCount::try_from(
                    remaining_sectors * SectorCount::from(entries_per_sector)
                        + (SectorCount::from(entries_per_sector)
                            - SectorCount::from(last_entry.index)
                            + 1),
                )
                .unwrap();

                if remaining_entries < n.get() - chain_len {
                    Err(FSError::RootDirectoryFull)
                } else {
                    Ok(first_entry)
                }
            }
            EntryLocationUnit::DataCluster(cluster) => {
                // first, we check how many clusters need to be allocated (if any)
                let entries_per_cluster = u16::try_from(
                    self.props.cluster_size
                        / u32::try_from(DIRENTRY_SIZE).expect("32 can fit into u32"),
                )
                .expect("a cluster can have a max of ~16k entries");

                let entries_left = n.get() - chain_len;
                let free_entries_on_current_cluster = entries_per_cluster - (last_entry.index + 1);

                // if we do in fact have to allocate some clusters, we allocate them
                if free_entries_on_current_cluster < entries_left {
                    let clusters_to_allocate = (entries_left - free_entries_on_current_cluster)
                        .div_ceil(entries_per_cluster);

                    let first_cluster = self.allocate_clusters(
                        num::NonZero::new(clusters_to_allocate.into())
                            .expect("this should be at least 1"),
                        Some(cluster),
                    )?;

                    // the entry chain begins in the newly allocated clusters
                    if chain_len == 0 {
                        first_entry.unit = EntryLocationUnit::DataCluster(first_cluster);
                        first_entry.index = 0;
                    }

                    // before we return, we should zero those sectors according to the FAT spec
                    for cluster in
                        first_cluster..(first_cluster + ClusterCount::from(clusters_to_allocate))
                    {
                        let first_sector = self.data_cluster_to_partition_sector(cluster);

                        for sector in first_sector
                            ..(first_sector + SectorCount::from(self.sectors_per_cluster()))
                        {
                            self.load_nth_sector(sector)?;
                            self.sector_buffer.borrow_mut().fill(0);
                            self.set_modified();
                        }
                    }
                }

                Ok(first_entry)
            }
        }
    }

    /// Creates a new cluster chain with the `.` and `..` entries present,
    // The datetime parameter is there so that we fully comply with the FAT32 spec:
    // ". All date and time fields must be set to the same value as that for
    // the containing directory"
    pub(crate) fn create_entry_chain(
        &self,
        parent: EntryLocationUnit,
        datetime: PrimitiveDateTime,
    ) -> FSResult<u32, S::Error> {
        // we need to allocate a cluster
        let dir_cluster = self.allocate_clusters(num::NonZero::new(1).unwrap(), None)?;

        let entries = [
            MinProperties {
                name: None,
                sfn: CURRENT_DIR_SFN,
                // this needs to be set when creating a file
                attributes: RawAttributes::DIRECTORY,
                created: Some(datetime),
                modified: datetime,
                accessed: Some(datetime.date()),
                file_size: 0,
                data_cluster: dir_cluster,
            },
            MinProperties {
                name: None,
                sfn: PARENT_DIR_SFN,
                // this needs to be set when creating a file
                attributes: RawAttributes::DIRECTORY,
                created: Some(datetime),
                modified: datetime,
                accessed: Some(datetime.date()),
                file_size: 0,
                data_cluster: match parent {
                    EntryLocationUnit::DataCluster(cluster) => cluster,
                    EntryLocationUnit::RootDirSector(_) => 0,
                },
            },
        ];

        // this composer will ALWAYS generate 2 entries
        let entries_iter = EntryComposer::new(&entries);

        self.load_nth_sector(self.data_cluster_to_partition_sector(dir_cluster))?;

        // we zero the current sector
        self.sector_buffer.borrow_mut().fill(0);

        let mut entry_location = EntryLocation {
            unit: EntryLocationUnit::DataCluster(dir_cluster),
            index: 0,
        };

        for (i, bytes) in entries_iter.enumerate() {
            entry_location.set_bytes(self, bytes)?;

            if i < NONROOT_MIN_DIRENTRIES {
                entry_location = entry_location
                    .next_entry(self)?
                    .expect("this will only be called once");
            }
        }

        self.set_modified();

        // we also zero everything else in the cluster
        let stored_sector = self.sector_buffer.borrow().stored_sector();
        for sector in
            (stored_sector + 1)..(stored_sector + SectorCount::from(self.sectors_per_cluster()))
        {
            self.load_nth_sector(sector)?;
            self.sector_buffer.borrow_mut().fill(0);
            self.set_modified();
        }

        Ok(dir_cluster)
    }

    /// Insert the provided `entries` to the cluster chain of the current cached directory
    ///
    /// Returns the corresponding [`DirEntryChain`]
    ///
    /// Panics if the `entries` array is empty
    pub(crate) fn insert_to_entry_chain(
        &self,
        entries: &[MinProperties],
    ) -> FSResult<DirEntryChain, S::Error> {
        let mut entries_needed = 0;

        self._go_to_cached_dir()?;

        for entry in entries {
            // we need at least one entry for the short filename
            entries_needed += 1;

            if let Some(long_filename) = &entry.name {
                entries_needed += calc_lfn_entries_needed(long_filename).get()
            }
        }

        let first_entry = self.allocate_nth_entries(
            num::NonZero::new(entries_needed).expect("The entries array shouldn't be empty"),
        )?;

        let mut entries_iter = EntryComposer::new(entries);

        let mut current_entry = first_entry;
        let mut entry_bytes = entries_iter
            .next()
            .expect("this iterator is guaranteed to return at least once");

        loop {
            current_entry.set_bytes(self, entry_bytes)?;

            match entries_iter.next() {
                Some(bytes) => entry_bytes = bytes,
                None => break,
            };

            current_entry = current_entry
                .next_entry(self)?
                .expect("This entry chain should be valid, we just generated it");
        }

        Ok(DirEntryChain {
            len: entries_needed,
            location: first_entry,
        })
    }

    /// Defragment the entry chain of the current directory
    ///
    /// Returns a [`FSResult`] containing the new number of entries
    pub(crate) fn defragment_entry_chain(&self) -> FSResult<EntryCount, S::Error> {
        let mut current_entry_loc = EntryLocation {
            unit: self.dir_info.borrow().chain_start,
            index: 0,
        };
        let mut new_chain_end = current_entry_loc;
        let mut entry_count: EntryCount = 0;

        loop {
            match current_entry_loc.entry_status(self)? {
                EntryStatus::Used => {
                    // no reason to copy the bytes if both locations are the same
                    if current_entry_loc != new_chain_end {
                        // copy the bytes where they belong
                        let bytes = current_entry_loc.get_bytes(self)?;

                        new_chain_end.set_bytes(self, bytes)?;

                        // what if for whatever reason the data types changes?
                        #[expect(clippy::absurd_extreme_comparisons)]
                        if entry_count >= DIRENTRY_LIMIT {
                            break;
                        }

                        // don't forget to free the entry
                        current_entry_loc.free_entry(self, false)?;
                    }

                    entry_count += 1;

                    // advance the new entry chain
                    new_chain_end = new_chain_end
                        .next_entry(self)?
                        .expect("we just pushed an entry to this chain")
                }
                EntryStatus::LastUnused => break,
                _ => (),
            }

            current_entry_loc = match current_entry_loc.next_entry(self)? {
                Some(entry) => entry,
                None => break,
            }
        }

        // we should also probably mark the entry after the last used one as last and unused
        new_chain_end.free_entry(self, true)?;

        self.dir_info.borrow_mut().chain_end = Some(new_chain_end);

        Ok(entry_count)
    }

    /// Mark the individual entries of a contiguous FAT entry chain as unused
    ///
    /// Note: No validation is done to check whether or not the chain is valid
    pub(crate) fn remove_entry_chain(&self, chain: &DirEntryChain) -> Result<(), S::Error> {
        let mut entries_freed = 0;
        let mut current_entry = chain.location;

        loop {
            current_entry.free_entry(self, false)?;

            entries_freed += 1;

            if entries_freed >= chain.len {
                break;
            }

            current_entry = match current_entry.next_entry(self)? {
                Some(current_entry) => current_entry,
                None => unreachable!(
                    concat!("It is guaranteed that at least as many entries ",
                    "as there are in chain exist, since we counted them when initializing the struct")
                ),
            };
        }

        Ok(())
    }

    /// Frees all the cluster in a cluster chain starting with `first_cluster`
    pub(crate) fn free_cluster_chain(&self, first_cluster: u32) -> Result<(), S::Error> {
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

    /// Allocate `n` clusters and return the index of the first one allocated
    ///
    /// Also has a second [`Option`] argument that if not [`None`] indicates
    /// that this cluster should point to the newly allocated cluster chain
    pub(crate) fn allocate_clusters(
        &self,
        n: num::NonZero<ClusterCount>,
        first_cluster: Option<ClusterIndex>,
    ) -> FSResult<ClusterIndex, S::Error> {
        let mut last_cluster_in_chain = first_cluster;
        let mut first_allocated_cluster = None;

        for i in 0..n.into() {
            match self.next_free_cluster()? {
                Some(next_free_cluster) => {
                    // FIXME: in FAT12 filesystems, this can cause a sector
                    // to be updated up to 4 times for seeminly no reason
                    // Similar behavour is observed in FAT16/32, with 2 sync operations
                    // THis number should be halved for both cases

                    if i == 0 {
                        first_allocated_cluster = Some(next_free_cluster);
                    }

                    // we set the last allocated cluster to point to the next free one
                    if let Some(last_cluster_in_chain) = last_cluster_in_chain {
                        self.write_nth_FAT_entry(
                            last_cluster_in_chain,
                            FATEntry::Allocated(next_free_cluster),
                        )?;
                    }
                    // we also set the next free cluster to be EOF
                    self.write_nth_FAT_entry(next_free_cluster, FATEntry::Eof)?;
                    if let Some(last_cluster_in_chain) = last_cluster_in_chain {
                        log::trace!(
                            "cluster {last_cluster_in_chain} now points to {next_free_cluster}"
                        );
                    }
                    // now the next free cluster i the last allocated one
                    last_cluster_in_chain = Some(next_free_cluster);
                }
                None => {
                    log::error!("storage medium full while attempting to allocate more clusters");
                    return Err(FSError::StorageFull);
                }
            }
        }

        Ok(first_allocated_cluster.expect("This should have Some value by now"))
    }

    /// Syncs `self.sector_buffer` back to the storage
    fn _sync_current_sector(&self) -> Result<(), S::Error> {
        self.sector_buffer.borrow().write(&self.storage)
    }

    /// Syncs a FAT sector to ALL OTHER FAT COPIES on the device medium
    #[expect(non_snake_case)]
    fn _sync_FAT_sector(&self, fat_sector_props: &FATSectorProps) -> Result<(), S::Error> {
        for sector in fat_sector_props.get_corresponding_FAT_sectors(self) {
            self.sector_buffer
                .borrow()
                .write_copy(&self.storage, sector)?;
        }

        Ok(())
    }

    /// Marks that a modification has been made to the storage medium, setting the `sync_f` and `unmount_f` fields
    pub(crate) fn set_modified(&self) {
        self.sync_f.replace(Some(Self::sync_sector_buffer));
        self.unmount_f.replace(Some(Self::unmount));
    }

    pub(crate) fn sync_sector_buffer(&self) -> Result<(), S::Error> {
        // If this is called, we assume the sector buffer has been modified
        let stored_sector = self.sector_buffer.borrow().stored_sector();
        if let Some(fat_sector_props) = FATSectorProps::new(stored_sector, self) {
            log::trace!("syncing FAT sector {}", fat_sector_props.sector_offset,);
            match &*self.boot_record.borrow() {
                BootRecord::Fat(boot_record_fat) => match &boot_record_fat.ebr {
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
            log::trace!(
                "syncing sector {}",
                self.sector_buffer.borrow().stored_sector()
            );
            self._sync_current_sector()?;
        }

        // we don't want to call this again for no reason
        self.sync_f.replace(None);

        Ok(())
    }

    /// Sync the [`FSInfoFAT32`] back to the storage medium
    /// if this is FAT32
    pub(crate) fn sync_fsinfo(&self) -> FSResult<(), S::Error> {
        if *self.fsinfo_modified.borrow() {
            if let BootRecord::Fat(boot_record_fat) = &*self.boot_record.borrow() {
                if let Ebr::FAT32(ebr_fat32, fsinfo) = &boot_record_fat.ebr {
                    self.load_nth_sector(ebr_fat32.fat_info.into())?;

                    fsinfo
                        .write_to_prefix(&mut self.sector_buffer.borrow_mut())
                        .unwrap();
                }
            }

            self.fsinfo_modified.replace(false);
        }

        Ok(())
    }

    pub(crate) fn sync_boot_sector(&self) -> FSResult<(), S::Error> {
        if *self.boot_sector_modified.borrow() {
            self.load_nth_sector(0)?;

            let mut bytes = self.sector_buffer.borrow_mut();

            let boot_record = self.boot_record.borrow();

            match &*boot_record {
                BootRecord::Fat(boot_record_fat) => {
                    boot_record_fat.bpb.write_to_prefix(&mut bytes).unwrap();

                    match &boot_record_fat.ebr {
                        Ebr::FAT12_16(ebr_fat12_16) => {
                            ebr_fat12_16
                                .write_to_prefix(&mut bytes[BPBFAT_SIZE..])
                                .unwrap();
                        }
                        Ebr::FAT32(ebr_fat32, _) => {
                            ebr_fat32
                                .write_to_prefix(&mut bytes[BPBFAT_SIZE..])
                                .unwrap();
                        }
                    }
                }
                BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
            };

            self.boot_sector_modified.replace(false);
            self.set_modified();
        }

        Ok(())
    }

    /// Like [`Self::get_rw_file`], but will ignore the read-only flag (if it is present)
    ///
    /// This is a private function for obvious reasons
    fn get_rw_file_unchecked<P: AsRef<Path>>(
        &self,
        path: P,
    ) -> FSResult<RWFile<'_, S, C>, S::Error> {
        let ro_file = self.get_ro_file(path)?;

        Ok(ro_file.into())
    }
}

/// Public [`Read`]-related functions
impl<S, C> FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    /// Read all the entries of a directory ([`Path`]) into [`ReadDir`]
    ///
    /// Fails if `path` doesn't represent a directory, or if that directory doesn't exist
    pub fn read_dir<P: AsRef<Path>>(&self, path: P) -> FSResult<ReadDir<'_, S, C>, S::Error> {
        // normalize the given path
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        let path = path.normalize();

        self.go_to_dir(&path)?;

        Ok(ReadDir::new(
            self,
            &self.dir_info.borrow().chain_start,
            &self.dir_info.borrow().path,
        ))
    }

    /// Reads the volume label from the BIOS parameter block
    ///
    /// If the volume label is `"NO NAME    "`, it means that it doesn't exists
    /// and [`None`]` will be returned instead
    pub fn volume_label_bpb(&self) -> Option<String> {
        let volume_label = match &*self.boot_record.borrow() {
            BootRecord::Fat(boot_record_fat) => match &boot_record_fat.ebr {
                Ebr::FAT12_16(ebr_fat12_16) => ebr_fat12_16.volume_label,
                Ebr::FAT32(ebr_fat32, _fsinfo) => ebr_fat32.volume_label,
            },
            BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
        };

        (volume_label != EMPTY_VOLUME_LABEL).then(|| {
            self.options
                .codepage
                .decode(&volume_label)
                .trim_end()
                .to_string()
        })
    }

    /// Reads the first volume label entry from the root directory that is found
    pub fn volume_label_root_dir(&self) -> Result<Option<String>, S::Error> {
        let volume_label = 'search: {
            self._go_to_root_directory();

            for entry in self.process_current_dir() {
                let entry = entry?;

                if entry.attributes == RawAttributes::VOLUME_ID {
                    break 'search *entry.sfn;
                }
            }

            // Nothing was found, return [`None`]
            return Ok(None);
        };

        Ok((volume_label != EMPTY_VOLUME_LABEL).then(|| {
            self.options
                .codepage
                .decode(&volume_label)
                .trim_end()
                .to_string()
        }))
    }

    /// Get a corresponding [`ROFile`] object from a [`Path`]
    ///
    /// Fails if `path` doesn't represent a file, or if that file doesn't exist
    pub fn get_ro_file<P: AsRef<Path>>(&self, path: P) -> FSResult<ROFile<'_, S, C>, S::Error> {
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        if let Some(file_name) = path.file_name() {
            // IO operations are expensive, check the bloom filter
            #[cfg(feature = "bloom")]
            if let Some(filter) = &self.dir_info.borrow().filter {
                if !filter.check(file_name) {
                    return Err(FSError::NotFound);
                }
            }

            let parent_dir = self.read_dir(
                path.parent()
                    .expect("we aren't in the root directory, this shouldn't panic"),
            )?;

            // don't ask, I don't know either (https://doc.rust-lang.org/std/boxed/index.html#editions)
            let mut entry = None;

            for dir_entry in parent_dir {
                let dir_entry = dir_entry?;

                if dir_entry
                    .path()
                    .file_name()
                    .is_some_and(|entry_name| entry_name == file_name)
                {
                    entry = Some(dir_entry.entry);

                    break;
                }
            }

            match entry {
                Some(entry) => {
                    let mut file = ROFile::from_props(
                        FileProps {
                            offset: 0,
                            current_cluster: entry.data_cluster,
                            entry,
                        },
                        self,
                    );

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
                    log::error!("ROFile {path} not found");

                    Err(FSError::NotFound)
                }
            }
        } else {
            log::error!("Is a directory (not a file)");
            Err(FSError::IsADirectory)
        }
    }

    /// Cache all of path's entries if it is a directory into a Bloom filter
    ///
    /// Useful if you plan to create lots of files in a directory
    ///
    /// Increases memory usage by `options.query_filter_size()`, where
    /// options is the [`FSOptions`] struct passed to [`new`](Self::new)
    #[cfg(feature = "bloom")]
    pub fn cache_dir<P>(&mut self, path: P) -> FSResult<(), S::Error>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        self.go_to_dir(path)?;

        let mut filter = utils::bloom::Bloom::new(
            self.options.filter_size,
            num::NonZeroUsize::new(DIRENTRY_LIMIT.into()).unwrap(),
        );

        let codepage = self.options.codepage;

        for entry in self.process_current_dir() {
            let entry = entry?;

            let long_name = entry.name;
            let short_name = entry.sfn.decode(codepage);

            filter.set(short_name.as_str());
            if let Some(long_filename) = long_name {
                filter.set(long_filename.as_str());
            }
        }

        self.dir_info.borrow_mut().filter = Some(filter);

        Ok(())
    }
}

/// [`Write`]-related functions
impl<S, C> FileSystem<S, C>
where
    S: BlockWrite,
    C: Clock,
{
    /// Create a new [`RWFile`] and return its handle
    #[inline]
    pub fn create_file<P: AsRef<Path>>(&self, path: P) -> FSResult<RWFile<'_, S, C>, S::Error> {
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        let target = path.normalize();

        let parent_dir = match target.parent() {
            Some(parent) => parent,
            // technically, the path provided is a directory, the root directory
            None => return Err(FSError::IsADirectory),
        };

        let file_name = target
            .file_name()
            .expect("the path is normalized and it isn't the root directory either");

        self.go_to_dir(parent_dir)?;

        // check if there is already a file or directory with the same name
        // this won't actually run unless the file we are creating is in the
        // cached directory
        #[cfg_attr(not(feature = "bloom"), expect(unused_labels))]
        'check: {
            #[cfg(feature = "bloom")]
            if let Some(filter) = &self.dir_info.borrow().filter {
                if !filter.check(file_name) {
                    break 'check;
                }
            }

            for entry in self.process_current_dir() {
                let entry = entry?;

                if entry.name(self.options.codepage) == file_name {
                    return Err(FSError::AlreadyExists);
                }
            }
        }

        let file_cluster = self.allocate_clusters(num::NonZero::new(1).expect("1 != 0"), None)?;

        let sfn = utils::string::gen_sfn(file_name, self, parent_dir)?;

        let now = self.options.clock.now();

        // we got everything to create our first (and only) RawProperties struct
        let raw_properties = MinProperties {
            name: Some(file_name.into()),
            sfn,
            // this needs to be set when creating a file
            attributes: RawAttributes::ARCHIVE,
            created: Some(now),
            modified: now,
            accessed: Some(now.date()),
            file_size: 0,
            data_cluster: file_cluster,
        };

        let entries = [raw_properties.clone()];

        let chain = self.insert_to_entry_chain(&entries)?;

        #[cfg(feature = "bloom")]
        if let Some(filter) = &mut self.dir_info.borrow_mut().filter {
            if let Some(long_filename) = &raw_properties.name {
                filter.set(long_filename);
            }
            filter.set(&Box::from(raw_properties.sfn.decode(self.options.codepage)));
        }

        Ok(RWFile::from_props(
            FileProps {
                current_cluster: raw_properties.data_cluster,
                entry: Properties::from_raw(
                    RawProperties::from_chain(raw_properties, chain),
                    path.into(),
                    self.options.codepage,
                ),
                offset: 0,
            },
            self,
        ))
    }

    /// Create a new directory
    #[inline]
    pub fn create_dir<P: AsRef<Path>>(&self, path: P) -> FSResult<(), S::Error> {
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        let target = path.normalize();

        let parent_dir = match target.parent() {
            Some(parent) => parent,
            // the path provided is the root directory, which already exists
            None => return Err(FSError::AlreadyExists),
        };

        let file_name = target
            .file_name()
            .expect("the path is normalized and it isn't the root directory either");

        // check if there is already a file or directory with the same name
        for entry in self.process_current_dir() {
            let entry = entry?;

            if entry.name(self.options.codepage) == file_name {
                return Err(FSError::AlreadyExists);
            }
        }

        let now = self.options.clock.now();

        let dir_cluster = self.create_entry_chain(self.dir_info.borrow().chain_start, now)?;

        // The cluster chain of the directory has been created,
        // we now need to add it as an entry to the current directory

        let sfn = utils::string::gen_sfn(file_name, self, parent_dir)?;

        // we got everything to create our first (and only) RawProperties struct
        let raw_properties = MinProperties {
            name: Some(file_name.into()),
            sfn,
            attributes: RawAttributes::DIRECTORY,
            created: Some(now),
            modified: now,
            accessed: Some(now.date()),
            file_size: 0,
            data_cluster: dir_cluster,
        };

        let entries = [raw_properties];

        self.go_to_dir(parent_dir)?;

        self.insert_to_entry_chain(&entries)?;

        Ok(())
    }

    /// Rename a file or directory to a new name
    pub fn rename<P: AsRef<Path>, Q: AsRef<Path>>(&self, from: P, to: Q) -> FSResult<(), S::Error> {
        let from = from.as_ref();
        let to = to.as_ref();

        if !from.is_valid() || !to.is_valid() {
            return Err(FSError::MalformedPath);
        }

        let from = from.normalize();
        let to = to.normalize();

        let parent_from = match from.parent() {
            Some(parent) => parent,
            // we can't rename the root directory
            None => return Err(FSError::PermissionDenied),
        };
        let parent_to = match to.parent() {
            Some(parent) => parent,
            // we can't rename the root directory
            None => return Err(FSError::PermissionDenied),
        };

        let entry_from = {
            let mut entry_from = None;

            for entry in self.read_dir(parent_from)? {
                let entry = entry?;

                if *entry.path() == from {
                    entry_from = Some(entry);

                    break;
                }
            }

            match entry_from {
                Some(entry) => entry,
                None => return Err(FSError::NotFound),
            }
        };

        for entry in self.read_dir(parent_to)? {
            let entry = entry?;

            if *entry.path() == to {
                return Err(FSError::AlreadyExists);
            }
        }

        // if the entry is a file, everything is way more simple,
        // we just need to remove this entry a create a new one at
        // the target directory. This can be accomplished in 2 ways:
        // 1. we first remove the old entry and then create the new one, or
        // 2. we first create the new entry and then remove the old one
        // the first method is easier to implement, but has a higher risk of data loss
        // the second method is a bit more difficult and in a worst-case scenario
        // the file won't be lost, althought we will be left with 2 hard links
        // pointing to the same file. Here we use the second method
        self.go_to_dir(parent_to)?;

        let now = self.options.clock.now();

        if entry_from.is_dir() {
            // the process with directories is the same, expect we must modify the ".." entry
            // so that it points to the new parent directory
            // the ".." entry is always the second entry, so we will do something a bit hacky here
            let parent_entry = MinProperties {
                name: None,
                sfn: PARENT_DIR_SFN,
                // this needs to be set when creating a file
                attributes: RawAttributes::DIRECTORY,
                created: Some(now),
                modified: now,
                accessed: Some(now.date()),
                file_size: 0,
                data_cluster: match self.dir_info.borrow().chain_start {
                    EntryLocationUnit::DataCluster(cluster) => cluster,
                    EntryLocationUnit::RootDirSector(_) => 0,
                },
            };

            // we are modifying the 2nd entry
            let entry_location = EntryLocation {
                unit: EntryLocationUnit::DataCluster(entry_from.data_cluster),
                index: 1,
            };

            self._go_to_cached_dir()?;
            let bytes: [u8; DIRENTRY_SIZE] = zerocopy::transmute!(FATDirEntry::from(parent_entry));

            entry_location.set_bytes(self, bytes)?;
        }

        let old_chain = entry_from.chain;
        let old_props: MinProperties = entry_from.into();
        let to_filename = to.file_name().expect("this path is normalized");
        let sfn = utils::string::gen_sfn(to_filename, self, parent_to)?;

        let props = MinProperties {
            name: Some(Box::from(to_filename)),
            sfn,
            attributes: old_props.attributes,
            created: Some(now),
            modified: now,
            accessed: Some(now.date()),
            file_size: old_props.file_size,
            data_cluster: old_props.data_cluster,
        };
        self.insert_to_entry_chain(&[props])?;

        self.remove_entry_chain(&old_chain)?;

        Ok(())
    }

    /// Remove a [`RWFile`] from the filesystem
    ///
    /// This is an alias to `self.get_rw_file(path)?.remove()?`
    #[inline]
    pub fn remove_file<P: AsRef<Path>>(&self, path: P) -> FSResult<(), S::Error> {
        self.get_rw_file(path)?.remove()?;

        Ok(())
    }

    /// Remove a file from the filesystem, even if it is read-only
    ///
    /// **USE WITH EXTREME CAUTION!**
    #[inline]
    pub fn remove_file_unchecked<P: AsRef<Path>>(&self, path: P) -> FSResult<(), S::Error> {
        self.get_rw_file_unchecked(path)?.remove()?;

        Ok(())
    }

    /// Remove an empty directory from the filesystem
    ///
    /// Errors if the path provided points to the root directory
    pub fn remove_empty_dir<P: AsRef<Path>>(&self, path: P) -> FSResult<(), S::Error> {
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        if path
            .components()
            .next_back()
            .expect("this iterator will always yield at least the root directory")
            == WindowsComponent::root()
        {
            // we are in the root directory, we can't remove it
            return Err(FSError::InvalidInput);
        }

        if self.read_dir(path)?.next().is_some() {
            return Err(FSError::DirectoryNotEmpty);
        }

        let parent_path = path
            .parent()
            .expect("we aren't in the root directory, this shouldn't panic");

        let parent_dir_entries = self.read_dir(parent_path)?;

        let entry = {
            let mut entry = None;

            for ent in parent_dir_entries {
                let ent = ent?;

                if ent.path() == path {
                    entry = Some(ent);

                    break;
                }
            }

            entry.ok_or(FSError::NotFound)?
        };

        // we first clear the corresponding entry chain in the parent directory
        self.remove_entry_chain(&entry.chain)?;

        // then we remove the allocated cluster chain
        self.free_cluster_chain(entry.data_cluster)?;

        Ok(())
    }

    /// Removes a directory at this path, after removing all its contents.
    ///
    /// Use with caution!
    ///
    /// This will fail if there is at least 1 (one) read-only file
    /// in this directory or in any subdirectory. To avoid this behavior,
    /// use [`remove_dir_all_unchecked()`](FileSystem::remove_dir_all_unchecked)
    pub fn remove_dir_all<P: AsRef<Path>>(&self, path: P) -> FSResult<(), S::Error> {
        // before we actually start removing stuff,
        // let's make sure there are no read-only files

        if self.check_for_readonly_files(&path)? {
            log::error!(concat!(
                "A read-only file has been found ",
                "in a directory pending deletion."
            ));
            return Err(FSError::ReadOnlyFile);
        }

        // we have checked everything, this is safe to use
        self.remove_dir_all_unchecked(&path)?;

        Ok(())
    }

    /// Like [`remove_dir_all()`](FileSystem::remove_dir_all),
    /// but also removes read-only files.
    ///
    /// **USE WITH EXTREME CAUTION!**
    pub fn remove_dir_all_unchecked<P: AsRef<Path>>(&self, path: P) -> FSResult<(), S::Error> {
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        let mut read_dir = self.read_dir(path)?;
        loop {
            let entry = match read_dir.next() {
                Some(entry) => entry?,
                None => break,
            };

            if entry.is_dir() {
                self.remove_dir_all_unchecked(&entry.path)?;
            } else if entry.is_file() {
                self.remove_file_unchecked(&entry.path)?;
            } else {
                unreachable!()
            }
        }

        self.remove_empty_dir(path)?;

        Ok(())
    }

    /// Check `path` recursively to see if there are any read-only files in it
    ///
    /// If successful, the `bool` returned indicates
    /// whether or not at least 1 (one) read-only file has been found
    pub fn check_for_readonly_files<P: AsRef<Path>>(&self, path: P) -> FSResult<bool, S::Error> {
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        let mut read_dir = self.read_dir(path)?;

        loop {
            let entry = match read_dir.next() {
                Some(entry) => entry?,
                None => break,
            };

            let read_only_found = if entry.is_dir() {
                self.check_for_readonly_files(&entry.path)?
            } else if entry.is_file() {
                entry.attributes.read_only
            } else {
                unreachable!()
            };

            if read_only_found {
                // we have found at least 1 read-only file,
                // no need to search any further
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Get a corresponding [`RWFile`] object from a [`Path`]
    ///
    /// Fails if `path` doesn't represent a file, or if that file doesn't exist
    pub fn get_rw_file<P: AsRef<Path>>(&self, path: P) -> FSResult<RWFile<'_, S, C>, S::Error> {
        let rw_file = self.get_rw_file_unchecked(path)?;

        if rw_file.attributes.read_only {
            return Err(FSError::ReadOnlyFile);
        }

        Ok(rw_file)
    }

    /// Sets the volume label of the BIOS parameter block
    ///
    /// If [`None`] is returned, the label was too big to fit to the volume label field
    /// or the decoded text is `"NO NAME    "`
    pub fn set_volume_label_bpb<L>(&self, label: L) -> Option<()>
    where
        L: AsRef<str>,
    {
        let mut label_bytes = [b' '; VOLUME_LABEL_BYTES];

        utils::string::copy_cp_chars(&mut label_bytes, label.as_ref(), self.options.codepage)?;

        if label_bytes == EMPTY_VOLUME_LABEL {
            return None;
        }

        let mut bpb_volume_label =
            RefMut::map(
                self.boot_record.borrow_mut(),
                |boot_record| match boot_record {
                    BootRecord::Fat(boot_record_fat) => match &mut boot_record_fat.ebr {
                        Ebr::FAT12_16(ref mut ebr_fat12_16) => &mut ebr_fat12_16.volume_label,
                        Ebr::FAT32(ref mut ebr_fat32, _fsinfo) => &mut ebr_fat32.volume_label,
                    },
                    BootRecord::ExFAT(_boot_record_exfat) => todo!("ExFAT not yet implemented"),
                },
            );

        bpb_volume_label.copy_from_slice(&label_bytes);

        self.boot_sector_modified.replace(true);
        self.set_modified();

        Some(())
    }

    /// Sets the volume label of the root directory, removing an already-existing label if one is found
    ///
    /// If [`None`] is returned, the label was too big to fit to the volume label field
    pub fn set_volume_label_root_dir<L>(&self, label: L) -> FSResult<Option<()>, S::Error>
    where
        L: AsRef<str>,
    {
        let mut label_bytes = [b' '; VOLUME_LABEL_BYTES];

        Ok::<_, S::Error>(utils::string::copy_cp_chars(
            &mut label_bytes,
            label.as_ref(),
            self.options.codepage,
        ))?;

        // remove already-existing label if such one is found
        self._go_to_root_directory();

        for entry in self.process_current_dir() {
            let entry = entry?;

            if entry.attributes == RawAttributes::VOLUME_ID {
                self.remove_entry_chain(&entry.chain)?;

                // assume that there aren't any other volume label entries
                break;
            }
        }

        let now = self.options.clock.now();

        let raw_properties = MinProperties {
            name: None,
            sfn: Sfn::new_from_slice(label_bytes),
            attributes: RawAttributes::VOLUME_ID,
            created: Some(now),
            modified: now,
            accessed: Some(now.date()),
            file_size: 0,
            data_cluster: 0,
        };

        let entries = [raw_properties];

        self.insert_to_entry_chain(&entries)?;

        Ok(Some(()))
    }

    /// Sync any pending changes back to the storage medium and drop
    ///
    /// Use this to catch any IO errors that might be rejected silently
    /// while [`Drop`]ping
    pub fn unmount(&self) -> FSResult<(), S::Error> {
        self.sync_boot_sector()?;
        self.sync_fsinfo()?;
        let should_sync_buffer = self.sync_f.borrow().is_some();
        if should_sync_buffer {
            self.sync_sector_buffer()?;
        }
        self.storage.borrow_mut().flush()?;

        Ok(())
    }
}

impl<S, C> ops::Drop for FileSystem<S, C>
where
    S: BlockRead,
    C: Clock,
{
    fn drop(&mut self) {
        if let Some(unmount) = self.unmount_f.replace(None) {
            // nothing to do if this errors out while dropping
            let _ = unmount(self);
        }
    }
}
