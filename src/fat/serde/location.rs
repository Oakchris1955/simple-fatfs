use core::num;

use super::boot_sector::BootRecord;
use super::entry_composer::{LAST_AND_UNUSED_ENTRY, UNUSED_ENTRY};
use super::DIRENTRY_SIZE;
use crate::block_io::prelude::*;
use crate::time::Clock;
use crate::{ClusterIndex, EntryIndex, FileSystem, SectorCount, SectorIndex};

/// The location of a chain of [`FATDirEntry`](crate::fat::serde::FATDirEntry)
#[derive(Debug, Clone, Copy)]
pub(crate) struct DirEntryChain {
    /// the location of the first corresponding entry
    pub(crate) location: EntryLocation,
    /// how many (contiguous) entries this entry chain has
    pub(crate) len: u16,
}

/*
 * I have opted for using associated functions instead of methods for
 * `EntryLocation` and `EntryLocationUnit`, since they aren't strictly
 * tied to most of them and each function call requires a corresponding
 * `FileSystem` object, otherwise data corruption may happen
 * (this won't happen in practice, since each FileSystem only handles
 * `EntryLocation`s and `EntryLocationUnit`s it generates)
 */

/// The location of a [`FATDirEntry`](crate::fat::serde::FATDirEntry)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct EntryLocation {
    /// the location of the first corresponding entry's data unit
    pub(crate) unit: EntryLocationUnit,
    /// the first entry's index/offset from the start of the data unit
    pub(crate) index: EntryIndex,
}

impl EntryLocation {
    pub(crate) fn entry_status<S, C>(
        this: &Self,
        fs: &FileSystem<S, C>,
    ) -> Result<EntryStatus, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        let entry_sector = Self::get_entry_sector(this, fs);
        fs.load_nth_sector(entry_sector)?;

        let byte_offset = Self::get_sector_byte_offset(this, fs);
        Ok(match fs.sector_buffer.borrow()[byte_offset] {
            UNUSED_ENTRY => EntryStatus::Unused,
            LAST_AND_UNUSED_ENTRY => EntryStatus::LastUnused,
            _ => EntryStatus::Used,
        })
    }

    #[inline]
    pub(crate) fn get_entry_sector<S, C>(this: &Self, fs: &FileSystem<S, C>) -> SectorIndex
    where
        S: BlockRead,
        C: Clock,
    {
        let sector_offset: SectorCount = SectorCount::from(this.index)
            * SectorCount::try_from(DIRENTRY_SIZE).expect("32 can fit into a u32")
            / SectorCount::from(fs.props.sector_size());

        EntryLocationUnit::get_entry_sector(&this.unit, fs) + sector_offset
    }

    #[inline]
    pub(crate) fn get_sector_byte_offset<S, C>(this: &Self, fs: &FileSystem<S, C>) -> usize
    where
        S: BlockRead,
        C: Clock,
    {
        (usize::from(this.index) * DIRENTRY_SIZE) % usize::from(fs.props.sector_size())
    }

    // Note: this could also return a borrowed subslice from fs.sector_buffer,
    // but since it is only 32 bytes, I don't think it is worth the hastle
    pub(crate) fn get_bytes<S, C>(
        this: &Self,
        fs: &FileSystem<S, C>,
    ) -> Result<[u8; DIRENTRY_SIZE], S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        let entry_sector = Self::get_entry_sector(this, fs);
        let entry_offset = Self::get_sector_byte_offset(this, fs);
        let mut bytes = [0u8; DIRENTRY_SIZE];
        bytes.copy_from_slice(
            &fs.load_nth_sector(entry_sector)?[entry_offset..entry_offset + DIRENTRY_SIZE],
        );

        Ok(bytes)
    }

    pub(crate) fn set_bytes<S, C>(
        this: &Self,
        fs: &FileSystem<S, C>,
        bytes: [u8; DIRENTRY_SIZE],
    ) -> Result<(), S::Error>
    where
        S: BlockWrite,
        C: Clock,
    {
        let entry_sector = Self::get_entry_sector(this, fs);
        let entry_offset = Self::get_sector_byte_offset(this, fs);
        fs.load_nth_sector(entry_sector)?;
        fs.sector_buffer.borrow_mut()[entry_offset..entry_offset + DIRENTRY_SIZE]
            .copy_from_slice(&bytes);
        fs.set_modified();

        Ok(())
    }

    pub(crate) fn free_entry<S, C>(
        this: &Self,
        fs: &FileSystem<S, C>,
        is_last: bool,
    ) -> Result<(), S::Error>
    where
        S: BlockWrite,
        C: Clock,
    {
        let entry_sector = EntryLocationUnit::get_entry_sector(&this.unit, fs);
        fs.load_nth_sector(entry_sector)?;

        let byte_offset = Self::get_sector_byte_offset(this, fs);
        fs.sector_buffer.borrow_mut()[byte_offset] = if is_last {
            LAST_AND_UNUSED_ENTRY
        } else {
            UNUSED_ENTRY
        };
        fs.set_modified();

        Ok(())
    }

    pub(crate) fn next_entry<S, C>(
        mut this: Self,
        fs: &FileSystem<S, C>,
    ) -> Result<Option<EntryLocation>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        this.index += 1;

        // we haven't advanced to a new unit, we return immediately
        if this.index < EntryLocationUnit::get_max_offset(&this.unit, fs) {
            return Ok(Some(this));
        }

        // we try to advance to the next entry unit (if it exists)
        Ok(
            EntryLocationUnit::get_next_unit(&this.unit, fs)?.map(|unit| {
                this.unit = unit;
                this.index = 0;

                this
            }),
        )
    }

    // The NonZero here is to ensure that the `0..n` doesn't panic
    pub(crate) fn nth_entry<S, C>(
        this: Self,
        fs: &FileSystem<S, C>,
        n: num::NonZero<EntryIndex>,
    ) -> Result<Option<EntryLocation>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        let mut current_entry = this;

        for _ in 0..n.into() {
            match Self::next_entry(current_entry, fs)? {
                Some(next_entry) => current_entry = next_entry,
                None => return Ok(None),
            }
        }

        Ok(Some(current_entry))
    }
}

impl From<EntryLocationUnit> for EntryLocation {
    fn from(unit: EntryLocationUnit) -> Self {
        Self { unit, index: 0 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryStatus {
    Unused,
    LastUnused,
    Used,
}

/// The root directory sector or data cluster a [`FATDirEntry`](crate::fat::serde::FATDirEntry) belongs too
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryLocationUnit {
    /// Sector offset from the start of the root directory region (FAT12/16)
    RootDirSector(u16),
    /// Cluster offset from the start of the data region
    DataCluster(ClusterIndex),
}

impl EntryLocationUnit {
    pub(crate) fn get_max_offset<S, C>(this: &Self, fs: &FileSystem<S, C>) -> u16
    where
        S: BlockRead,
        C: Clock,
    {
        let unit_size = match this {
            EntryLocationUnit::DataCluster(_) => fs.props.cluster_size(),
            EntryLocationUnit::RootDirSector(_) => fs.props.sector_size().into(),
        };

        u16::try_from(unit_size / u32::try_from(DIRENTRY_SIZE).expect("32 can fit to u32"))
            .expect("a cluster can have a max of ~16k entries")
    }

    pub(crate) fn get_entry_sector<S, C>(this: &Self, fs: &FileSystem<S, C>) -> SectorIndex
    where
        S: BlockRead,
        C: Clock,
    {
        match this {
            EntryLocationUnit::RootDirSector(root_dir_sector) => {
                SectorCount::from(*root_dir_sector) + fs.props.first_root_dir_sector()
            }
            EntryLocationUnit::DataCluster(data_cluster) => {
                fs.props.data_cluster_to_partition_sector(*data_cluster)
            }
        }
    }

    pub(crate) fn get_next_unit<S, C>(
        this: &Self,
        fs: &FileSystem<S, C>,
    ) -> Result<Option<EntryLocationUnit>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        match this {
            EntryLocationUnit::RootDirSector(sector) => match &*fs.boot_record.borrow() {
                BootRecord::Fat(boot_record_fat) => {
                    if boot_record_fat.root_dir_sectors() == 0 {
                        unreachable!(concat!("This should be zero iff the FAT type if FAT32, ",
                    "in which case we won't even be reading root directory sectors, since it doesn't exist"))
                    }

                    if SectorIndex::from(*sector)
                        >= fs.props.first_root_dir_sector()
                            + SectorCount::from(boot_record_fat.root_dir_sectors())
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
                .filter(|cluster| *cluster < fs.props.total_clusters())
                .map(EntryLocationUnit::DataCluster)),
        }
    }
}
