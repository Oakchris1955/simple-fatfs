use super::*;

use bitfield_struct::bitfield;
use zerocopy::{
    little_endian::{U16, U32},
    FromBytes, Immutable, IntoBytes, KnownLayout,
};

#[derive(Debug)]
#[expect(clippy::large_enum_variant)]
pub(crate) enum BootRecord {
    Fat(BootRecordFAT),
    #[expect(dead_code)]
    ExFAT(BootRecordExFAT),
}

impl BootRecord {
    #[inline]
    /// The FAT type of this file system
    pub(crate) fn fat_type(&self) -> FATType {
        match self {
            BootRecord::Fat(boot_record_fat) => boot_record_fat.fat_type(),
            BootRecord::ExFAT(_boot_record_exfat) => FATType::ExFAT,
        }
    }

    #[expect(non_snake_case)]
    pub(crate) fn nth_FAT_table_sector(&self, n: u8) -> SectorIndex {
        match self {
            BootRecord::Fat(boot_record_fat) => {
                SectorIndex::from(boot_record_fat.first_fat_sector())
                    + SectorIndex::from(n) * boot_record_fat.fat_sector_size()
            }
            #[expect(unused_variables, unreachable_code)]
            BootRecord::ExFAT(boot_record_exfat) => {
                // this should work, but ExFAT is not yet implemented, so...
                todo!("ExFAT not yet implemented");
                SectorIndex::from(boot_record_exfat.fat_count)
                    + SectorIndex::from(n) * boot_record_exfat.fat_len
            }
        }
    }
}

pub(crate) const BOOT_SIGNATURE: u8 = 0x29;
pub(crate) const FAT_SIGNATURE: u16 = 0x55AA;

#[derive(Debug, Clone)]
pub(crate) struct BootRecordFAT {
    pub bpb: BpbFat,
    pub ebr: Ebr,
}

impl BootRecordFAT {
    #[inline]
    pub(crate) fn verify_signature(&self) -> bool {
        match self.fat_type() {
            FATType::FAT12 | FATType::FAT16 | FATType::FAT32 => match &self.ebr {
                Ebr::FAT12_16(ebr_fat12_16) => {
                    ebr_fat12_16.boot_signature == BOOT_SIGNATURE
                        && ebr_fat12_16.signature == FAT_SIGNATURE
                }
                Ebr::FAT32(ebr_fat32, _) => {
                    ebr_fat32.boot_signature == BOOT_SIGNATURE
                        && ebr_fat32.signature == FAT_SIGNATURE
                }
            },
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        }
    }

    #[inline]
    /// Total sectors in volume (including VBR)s
    pub(crate) fn total_sectors(&self) -> SectorCount {
        if self.bpb.total_sectors_16 == 0 {
            self.bpb.total_sectors_32.get()
        } else {
            self.bpb.total_sectors_16.into()
        }
    }

    #[inline]
    /// FAT size in sectors
    pub(crate) fn fat_sector_size(&self) -> u32 {
        match &self.ebr {
            Ebr::FAT12_16(_ebr_fat12_16) => self.bpb.table_size_16.into(),
            Ebr::FAT32(ebr_fat32, _) => ebr_fat32.table_size_32.get(),
        }
    }

    #[inline]
    /// The size of the root directory (unless we have FAT32, in which case the size will be 0)
    /// This calculation will round up
    pub(crate) fn root_dir_sectors(&self) -> u16 {
        (self.bpb.root_entry_count.get() * u16::try_from(DIRENTRY_SIZE).expect("32 can fit to u16"))
            .div_ceil(self.bpb.bytes_per_sector.get())
    }

    #[inline]
    /// The first sector in the File Allocation Table
    pub(crate) fn first_fat_sector(&self) -> u16 {
        self.bpb.reserved_sector_count.get()
    }

    #[inline]
    /// The first sector of the root directory (returns the first data sector on FAT32)
    pub(crate) fn first_root_dir_sector(&self) -> SectorIndex {
        SectorIndex::from(self.first_fat_sector())
            + SectorIndex::from(self.bpb.table_count) * self.fat_sector_size()
    }

    #[inline]
    /// The first data sector (that is, the first sector in which directories and files may be stored)
    pub(crate) fn first_data_sector(&self) -> SectorIndex {
        self.first_root_dir_sector() + SectorIndex::from(self.root_dir_sectors())
    }

    #[inline]
    /// The total number of data sectors
    pub(crate) fn total_data_sectors(&self) -> SectorCount {
        self.total_sectors() - SectorCount::from(self.first_data_sector()) + 1
    }

    #[inline]
    /// The total number of clusters
    pub(crate) fn total_clusters(&self) -> ClusterCount {
        self.total_data_sectors() / ClusterCount::from(self.bpb.sectors_per_cluster)
    }

    #[inline]
    /// The FAT type of this file system
    pub(crate) fn fat_type(&self) -> FATType {
        #[expect(unreachable_code)]
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

#[derive(Debug, Clone)]
// Everything here is naturally aligned (thank god)
pub(crate) struct BootRecordExFAT {
    pub _dummy_jmp: [u8; 3],
    pub _oem_identifier: [u8; 8],
    pub _zeroed: [u8; 53],
    pub _partition_offset: u64,
    #[expect(dead_code)]
    pub volume_len: u64,
    #[expect(dead_code)]
    pub fat_offset: u32,
    pub fat_len: u32,
    #[expect(dead_code)]
    pub cluster_heap_offset: u32,
    #[expect(dead_code)]
    pub cluster_count: u32,
    #[expect(dead_code)]
    pub root_dir_cluster: u32,
    #[expect(dead_code)]
    pub partition_serial_num: u32,
    #[expect(dead_code)]
    pub fs_revision: u16,
    #[expect(dead_code)]
    pub flags: u16,
    pub sector_shift: u8,
    pub cluster_shift: u8,
    pub fat_count: u8,
    #[expect(dead_code)]
    pub drive_select: u8,
    #[expect(dead_code)]
    pub used_percentage: u8,
    pub _reserved: [u8; 7],
}

pub(crate) const BPBFAT_SIZE: usize = 36;
#[derive(Immutable, KnownLayout, FromBytes, IntoBytes, Debug, Clone)]
#[repr(C)]
pub(crate) struct BpbFat {
    pub _jmpboot: [u8; 3],
    pub _oem_identifier: [u8; 8],
    pub bytes_per_sector: U16,
    pub sectors_per_cluster: u8,
    pub reserved_sector_count: U16,
    pub table_count: u8,
    pub root_entry_count: U16,
    // If this is 0, check `total_sectors_32`
    pub total_sectors_16: U16,
    pub _media_type: u8,
    pub table_size_16: U16,
    pub _sectors_per_track: U16,
    pub _head_side_count: U16,
    pub hidden_sector_count: U32,
    pub total_sectors_32: U32,
}

pub(crate) const VOLUME_LABEL_BYTES: usize = 11;
#[expect(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub(crate) enum Ebr {
    FAT12_16(EBRFAT12_16),
    FAT32(EBRFAT32, FSInfoFAT32),
}

#[derive(Debug, Immutable, FromBytes, IntoBytes, Clone)]
#[repr(C)]
pub(crate) struct EBRFAT12_16 {
    pub _drive_num: u8,
    pub _windows_nt_flags: u8,
    pub boot_signature: u8,
    pub volume_serial_num: U32,
    pub volume_label: [u8; VOLUME_LABEL_BYTES],
    pub _system_identifier: [u8; 8],
    pub _boot_code: [u8; 448],
    pub signature: U16,
}

#[bitfield(u16, order = Lsb, repr = U16, from = U16::new, into = U16::get)]
#[derive(Immutable, FromBytes, IntoBytes)]
pub(crate) struct FAT32ExtendedFlags {
    #[bits(4)]
    #[expect(non_snake_case)]
    pub(crate) active_FAT: u8,
    #[bits(3)]
    reserved1: u8,
    #[bits(1)]
    pub(crate) mirroring_disabled: bool,
    #[bits(8)]
    reserved2: u8,
}

// FIXME: these might be the other way around
#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone)]
#[repr(C)]
pub(crate) struct FATVersion {
    minor: u8,
    major: u8,
}

const EBRFAT32_RESERVED_BYTES: usize = 12;
#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone)]
#[repr(C)]
pub(crate) struct EBRFAT32 {
    pub table_size_32: U32,
    pub extended_flags: FAT32ExtendedFlags,
    pub fat_version: FATVersion,
    pub root_cluster: U32,
    pub fat_info: U16,
    pub backup_boot_sector: U16,
    pub _reserved_bytes: [u8; EBRFAT32_RESERVED_BYTES],
    pub _drive_num: u8,
    pub _windows_nt_flags: u8,
    pub boot_signature: u8,
    pub volume_serial_num: U32,
    pub volume_label: [u8; VOLUME_LABEL_BYTES],
    pub _system_ident: [u8; 8],
    pub _boot_code: [u8; 420],
    pub signature: U16,
}

const FSINFO_LEAD_SIGNATURE: u32 = 0x41615252;
const FSINFO_MID_SIGNATURE: u32 = 0x61417272;
const FSINFO_TRAIL_SIGNATURE: u32 = 0xAA550000;
const FSINFO_RESERVED1_BYTES: usize = 480;
const FSINFO_RESERVED2_BYTES: usize = 12;
#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone)]
#[repr(C)]
pub(crate) struct FSInfoFAT32 {
    pub lead_signature: U32,
    _reserved1: [u8; FSINFO_RESERVED1_BYTES],
    pub mid_signature: U32,
    pub free_cluster_count: U32,
    pub first_free_cluster: U32,
    _reserved2: [u8; FSINFO_RESERVED2_BYTES],
    pub trail_signature: U32,
}

impl FSInfoFAT32 {
    pub(crate) fn verify_signature(&self) -> bool {
        self.lead_signature == FSINFO_LEAD_SIGNATURE
            && self.mid_signature == FSINFO_MID_SIGNATURE
            && self.trail_signature == FSINFO_TRAIL_SIGNATURE
    }
}
