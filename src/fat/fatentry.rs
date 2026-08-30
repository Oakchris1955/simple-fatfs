use crate::block_io::prelude::*;
use crate::time::Clock;
use crate::{ClusterCount, FileSystem};
use crate::{ClusterIndex, FATEntryCount, FATEntryIndex, FATEntryValue, SectorIndex};

/// An enum representing different variants of the FAT filesystem
///
/// The logic is essentially the same in all of them, the only thing that
/// changes is the size in bytes of FAT entries, and thus the maximum volume size
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
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
    /// An ex-proprietary filesystem that allows for even larger storage sizes
    /// and its use is currently on the rise
    ///
    /// Not currently supported
    ExFAT,
}

impl FATType {
    #[inline]
    /// How many bits this [`FATType`] uses to address clusters in the disk
    pub(crate) fn bits_per_entry(&self) -> u8 {
        match self {
            FATType::FAT12 => 12,
            FATType::FAT16 => 16,
            // the high 4 bits are reserved, but are still part of the entry
            FATType::FAT32 => 32,
            FATType::ExFAT => 32,
        }
    }

    #[inline]
    // this is currently used only in a test case, but it might be useful in the future
    #[cfg_attr(not(test), expect(dead_code))]
    /// How many bits this [`FATType`] uses to address clusters in the disk,
    /// minus those reserved
    pub(crate) fn actual_bits_per_entry(&self) -> u8 {
        match self {
            FATType::FAT12 => 12,
            FATType::FAT16 => 16,
            // the high 4 bits are reserved
            FATType::FAT32 => 28,
            FATType::ExFAT => 32,
        }
    }

    #[inline]
    /// How many bytes this [`FATType`] spans across
    pub(crate) fn entry_size(&self) -> u8 {
        self.bits_per_entry().next_power_of_two() / 8
    }
}

// the first 2 entries are reserved
pub(crate) const RESERVED_FAT_ENTRIES: FATEntryCount = 2;

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

impl FATEntry {
    /// Parse a [`FATEntry`] of a `fat_type` filesystem with a `cluster_count`
    /// from its raw `value`
    pub(crate) fn from_value(
        value: FATEntryValue,
        fat_type: &FATType,
        cluster_count: ClusterCount,
    ) -> Self {
        /*
        // pad unused bytes with 1s
        let padding: u32 = u32::MAX.to_be() << self.fat_type.bits_per_entry();
        value |= padding.to_le();
        */

        // TODO: perhaps byte padding can replace some redundant code here?
        match fat_type {
            FATType::FAT12 => match value {
                0x000 => FATEntry::Free,
                0xFF7 => FATEntry::Bad,
                #[expect(clippy::manual_range_patterns)]
                0xFF8..=0xFFE | 0xFFF => FATEntry::Eof,
                _ => {
                    if (0x002..=cluster_count).contains(&value) {
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
                    if (0x0002..=cluster_count).contains(&value) {
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
                    if (0x00000002..=cluster_count).contains(&value) {
                        FATEntry::Allocated(value)
                    } else {
                        FATEntry::Reserved
                    }
                }
            },
            FATType::ExFAT => todo!("ExFAT not yet implemented"),
        }
    }
}

impl From<FATEntry> for FATEntryValue {
    fn from(fat_entry: FATEntry) -> Self {
        Self::from(&fat_entry)
    }
}

impl From<&FATEntry> for FATEntryValue {
    fn from(fat_entry: &FATEntry) -> Self {
        match fat_entry {
            FATEntry::Free => FATEntryValue::MIN,
            FATEntry::Allocated(cluster) => *cluster,
            FATEntry::Reserved => 0xFFFFFF6,
            FATEntry::Bad => 0xFFFFFF7,
            FATEntry::Eof => FATEntryValue::MAX,
        }
    }
}

/// Properties about the position of a [`FATEntry`] inside the FAT region
pub(crate) struct FATEntryProps {
    /// Each `n`th element of the vector points at the corresponding sector at the (first) active FAT table
    pub(crate) fat_sector: SectorIndex,
    pub(crate) sector_offset: usize,
}

impl FATEntryProps {
    /// Get the [`FATEntryProps`] of the `n`-th [`FATEntry`] of a [`FileSystem`]
    pub(crate) fn new<S, C>(n: FATEntryIndex, fs: &FileSystem<S, C>) -> Self
    where
        S: BlockRead,
        C: Clock,
    {
        let fat_byte_offset: u64 = u64::from(n) * u64::from(fs.fat_type().bits_per_entry()) / 8;
        let fat_sector = SectorIndex::try_from(
            u64::from(fs.props.first_fat_sector())
                + fat_byte_offset / u64::from(fs.props.sector_size()),
        )
        .expect("this should fit into a u32");
        let sector_offset: usize =
            usize::try_from(fat_byte_offset % u64::from(fs.props.sector_size()))
                .expect("this should fit into a usize");

        FATEntryProps {
            fat_sector,
            sector_offset,
        }
    }
}
