use super::*;

use bincode::{impl_borrow_decode, Decode, Encode};
use bitfield_struct::bitfield;

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
            self.bpb.total_sectors_32
        } else {
            self.bpb.total_sectors_16.into()
        }
    }

    #[inline]
    /// FAT size in sectors
    pub(crate) fn fat_sector_size(&self) -> u32 {
        match &self.ebr {
            Ebr::FAT12_16(_ebr_fat12_16) => self.bpb.table_size_16.into(),
            Ebr::FAT32(ebr_fat32, _) => ebr_fat32.table_size_32,
        }
    }

    #[inline]
    /// The size of the root directory (unless we have FAT32, in which case the size will be 0)
    /// This calculation will round up
    pub(crate) fn root_dir_sectors(&self) -> u16 {
        (self.bpb.root_entry_count * u16::try_from(DIRENTRY_SIZE).expect("32 can fit to u16"))
            .div_ceil(self.bpb.bytes_per_sector)
    }

    #[inline]
    /// The first sector in the File Allocation Table
    pub(crate) fn first_fat_sector(&self) -> u16 {
        self.bpb.reserved_sector_count
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
#[derive(Encode, Decode, Debug, Clone)]
pub(crate) struct BpbFat {
    pub _jmpboot: [u8; 3],
    pub _oem_identifier: [u8; 8],
    pub bytes_per_sector: u16,
    pub sectors_per_cluster: u8,
    pub reserved_sector_count: u16,
    pub table_count: u8,
    pub root_entry_count: u16,
    // If this is 0, check `total_sectors_32`
    pub total_sectors_16: u16,
    pub _media_type: u8,
    pub table_size_16: u16,
    pub _sectors_per_track: u16,
    pub _head_side_count: u16,
    pub hidden_sector_count: u32,
    pub total_sectors_32: u32,
}

pub(crate) const EBR_SIZE: usize = MIN_SECTOR_SIZE - BPBFAT_SIZE;
#[derive(Debug, Clone)]
pub(crate) enum Ebr {
    FAT12_16(EBRFAT12_16),
    FAT32(EBRFAT32, FSInfoFAT32),
}

#[derive(Debug, Encode, Decode, Clone)]
pub(crate) struct EBRFAT12_16 {
    pub _drive_num: u8,
    pub _windows_nt_flags: u8,
    pub boot_signature: u8,
    pub volume_serial_num: u32,
    pub volume_label: [u8; 11],
    pub _system_identifier: [u8; 8],
    pub _boot_code: [u8; 448],
    pub signature: u16,
}

#[bitfield(u16, order = Lsb)]
#[derive(Encode, Decode)]
pub(crate) struct FAT32ExtendedFlags {
    #[bits(4)]
    #[expect(non_snake_case)]
    pub(crate) active_FAT: u8,
    #[bits(3)]
    _reserved: _,
    #[bits(1)]
    pub(crate) mirroring_disabled: bool,
    #[bits(8)]
    _reserved: _,
}

// FIXME: these might be the other way around
#[derive(Encode, Decode, Debug, Clone)]
pub(crate) struct FATVersion {
    minor: u8,
    major: u8,
}

const EBRFAT32_RESERVED_BYTES: usize = 12;
#[derive(Debug, Clone)]
pub(crate) struct EBRFAT32 {
    pub table_size_32: u32,
    pub extended_flags: FAT32ExtendedFlags,
    pub fat_version: FATVersion,
    pub root_cluster: u32,
    pub fat_info: u16,
    pub backup_boot_sector: u16,
    pub _drive_num: u8,
    pub _windows_nt_flags: u8,
    pub boot_signature: u8,
    pub volume_serial_num: u32,
    pub volume_label: [u8; 11],
    pub _system_ident: [u8; 8],
    pub _boot_code: [u8; 420],
    pub signature: u16,
}

impl<__Context> Decode<__Context> for EBRFAT32 {
    fn decode<__D: bincode::de::Decoder<Context = __Context>>(
        decoder: &mut __D,
    ) -> Result<Self, bincode::error::DecodeError> {
        use bincode::de::read::Reader;

        Result::Ok(Self {
            table_size_32: Decode::decode(decoder)?,
            extended_flags: Decode::decode(decoder)?,
            fat_version: Decode::decode(decoder)?,
            root_cluster: Decode::decode(decoder)?,
            fat_info: Decode::decode(decoder)?,
            backup_boot_sector: Decode::decode(decoder)?,
            _drive_num: {
                decoder
                    .reader()
                    .read(&mut [0_u8; EBRFAT32_RESERVED_BYTES])?;
                Decode::decode(decoder)?
            },
            _windows_nt_flags: Decode::decode(decoder)?,
            boot_signature: Decode::decode(decoder)?,
            volume_serial_num: Decode::decode(decoder)?,
            volume_label: Decode::decode(decoder)?,
            _system_ident: Decode::decode(decoder)?,
            _boot_code: Decode::decode(decoder)?,
            signature: Decode::decode(decoder)?,
        })
    }
}

impl_borrow_decode!(EBRFAT32);

impl Encode for EBRFAT32 {
    fn encode<__E: bincode::enc::Encoder>(
        &self,
        encoder: &mut __E,
    ) -> Result<(), bincode::error::EncodeError> {
        use bincode::enc::write::Writer;

        Encode::encode(&self.table_size_32, encoder)?;
        Encode::encode(&self.extended_flags, encoder)?;
        Encode::encode(&self.fat_version, encoder)?;
        Encode::encode(&self.root_cluster, encoder)?;
        Encode::encode(&self.fat_info, encoder)?;
        Encode::encode(&self.backup_boot_sector, encoder)?;
        encoder.writer().write(&[0_u8; EBRFAT32_RESERVED_BYTES])?;
        Encode::encode(&self._drive_num, encoder)?;
        Encode::encode(&self._windows_nt_flags, encoder)?;
        Encode::encode(&self.boot_signature, encoder)?;
        Encode::encode(&self.volume_serial_num, encoder)?;
        Encode::encode(&self.volume_label, encoder)?;
        Encode::encode(&self._system_ident, encoder)?;
        Encode::encode(&self._boot_code, encoder)?;
        Encode::encode(&self.signature, encoder)?;

        Ok(())
    }
}

pub(crate) const FSINFO_SIZE: usize = 512;
const FSINFO_LEAD_SIGNATURE: u32 = 0x41615252;
const FSINFO_MID_SIGNATURE: u32 = 0x61417272;
const FSINFO_TRAIL_SIGNATURE: u32 = 0xAA550000;
const FSINFO_RESERVED1_BYTES: usize = 480;
const FSINFO_RESERVED2_BYTES: usize = 12;
#[derive(Debug, Clone)]
pub(crate) struct FSInfoFAT32 {
    pub lead_signature: u32,
    pub mid_signature: u32,
    pub free_cluster_count: u32,
    pub first_free_cluster: u32,
    pub trail_signature: u32,
}

impl<__Context> Decode<__Context> for FSInfoFAT32 {
    fn decode<__D: bincode::de::Decoder<Context = __Context>>(
        decoder: &mut __D,
    ) -> Result<Self, bincode::error::DecodeError> {
        use bincode::de::read::Reader;

        Ok(Self {
            lead_signature: Decode::decode(decoder)?,
            mid_signature: {
                decoder.reader().read(&mut [0_u8; FSINFO_RESERVED1_BYTES])?;
                Decode::decode(decoder)?
            },
            free_cluster_count: Decode::decode(decoder)?,
            first_free_cluster: Decode::decode(decoder)?,
            trail_signature: {
                decoder.reader().read(&mut [0_u8; FSINFO_RESERVED2_BYTES])?;
                Decode::decode(decoder)?
            },
        })
    }
}

impl_borrow_decode!(FSInfoFAT32);

impl Encode for FSInfoFAT32 {
    fn encode<__E: bincode::enc::Encoder>(
        &self,
        encoder: &mut __E,
    ) -> Result<(), bincode::error::EncodeError> {
        use bincode::enc::write::Writer;

        Encode::encode(&self.lead_signature, encoder)?;
        encoder.writer().write(&[0_u8; FSINFO_RESERVED1_BYTES])?;
        Encode::encode(&self.mid_signature, encoder)?;
        Encode::encode(&self.free_cluster_count, encoder)?;
        Encode::encode(&self.first_free_cluster, encoder)?;
        encoder.writer().write(&[0_u8; FSINFO_RESERVED2_BYTES])?;
        Encode::encode(&self.trail_signature, encoder)?;

        Ok(())
    }
}

impl FSInfoFAT32 {
    pub(crate) fn verify_signature(&self) -> bool {
        self.lead_signature == FSINFO_LEAD_SIGNATURE
            && self.mid_signature == FSINFO_MID_SIGNATURE
            && self.trail_signature == FSINFO_TRAIL_SIGNATURE
    }
}
