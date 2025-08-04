use super::*;

use core::num;

use crate::io::prelude::*;
use crate::*;

/// The root directory sector or data cluster a [`FATDirEntry`] belongs too
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryLocationUnit {
    /// Sector offset from the start of the root directory region (FAT12/16)
    RootDirSector(u16),
    /// Cluster offset from the start of the data region
    DataCluster(u32),
}

impl EntryLocationUnit {
    // I will leave this here in case it is needed in the future
    #[allow(unused)]
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
        let unit_size = match self {
            EntryLocationUnit::DataCluster(_) => fs.props.cluster_size,
            EntryLocationUnit::RootDirSector(_) => fs.props.sector_size as u64,
        };

        unit_size / DIRENTRY_SIZE as u64
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
                .filter(|cluster| *cluster < fs.props.total_clusters)
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
        let entry_sector = self.get_entry_sector(fs);
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
        let sector_offset = self.index as u64 * DIRENTRY_SIZE as u64 / fs.sector_size() as u64;

        self.unit.get_entry_sector(fs) + sector_offset
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
        S: Read + Write + Seek,
    {
        let entry_sector = self.unit.get_entry_sector(fs);
        fs.load_nth_sector(entry_sector)?;

        let byte_offset = self.get_sector_byte_offset(fs);
        fs.sector_buffer[byte_offset] = UNUSED_ENTRY;
        fs.set_modified();

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

    // The NonZero here is to ensure that the `0..n` doesn't panic
    pub(crate) fn nth_entry<S>(
        self,
        fs: &mut FileSystem<S>,
        n: num::NonZero<u32>,
    ) -> Result<Option<EntryLocation>, S::Error>
    where
        S: Read + Seek,
    {
        let mut current_entry = self;

        for _ in 0..n.into() {
            match current_entry.next_entry(fs)? {
                Some(next_entry) => current_entry = next_entry,
                None => return Ok(None),
            }
        }

        Ok(Some(current_entry))
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
