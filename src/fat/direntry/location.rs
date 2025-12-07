use super::*;

use core::num;

use crate::*;

/// The root directory sector or data cluster a [`FATDirEntry`] belongs too
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryLocationUnit {
    /// Sector offset from the start of the root directory region (FAT12/16)
    RootDirSector(u16),
    /// Cluster offset from the start of the data region
    DataCluster(ClusterIndex),
}

impl EntryLocationUnit {
    pub(crate) fn from_partition_sector<S, C>(sector: SectorIndex, fs: &FileSystem<S, C>) -> Self
    where
        S: BlockRead,
        C: Clock,
    {
        if sector < fs.first_data_sector() {
            EntryLocationUnit::RootDirSector(
                u16::try_from(sector - fs.props.first_root_dir_sector)
                    .expect("this should be a valid root dir sector"),
            )
        } else {
            EntryLocationUnit::DataCluster(fs.partition_sector_to_data_cluster(sector))
        }
    }

    pub(crate) fn get_max_offset<S, C>(&self, fs: &FileSystem<S, C>) -> u16
    where
        S: BlockRead,
        C: Clock,
    {
        let unit_size = match self {
            EntryLocationUnit::DataCluster(_) => fs.props.cluster_size,
            EntryLocationUnit::RootDirSector(_) => fs.props.sector_size.into(),
        };

        u16::try_from(unit_size / u32::try_from(DIRENTRY_SIZE).expect("32 can fit to u32"))
            .expect("a cluster can have a max of ~16k entries")
    }

    pub(crate) fn get_entry_sector<S, C>(&self, fs: &FileSystem<S, C>) -> SectorIndex
    where
        S: BlockRead,
        C: Clock,
    {
        match self {
            EntryLocationUnit::RootDirSector(root_dir_sector) => {
                SectorCount::from(*root_dir_sector) + fs.props.first_root_dir_sector
            }
            EntryLocationUnit::DataCluster(data_cluster) => {
                fs.data_cluster_to_partition_sector(*data_cluster)
            }
        }
    }

    pub(crate) fn get_next_unit<S, C>(
        &self,
        fs: &FileSystem<S, C>,
    ) -> Result<Option<EntryLocationUnit>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        match self {
            EntryLocationUnit::RootDirSector(sector) => match &*fs.boot_record.borrow() {
                BootRecord::Fat(boot_record_fat) => {
                    if boot_record_fat.root_dir_sectors() == 0 {
                        unreachable!(concat!("This should be zero iff the FAT type if FAT32, ",
                    "in which case we won't even be reading root directory sectors, since it doesn't exist"))
                    }

                    if SectorIndex::from(*sector)
                        >= fs.props.first_root_dir_sector
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
                .filter(|cluster| *cluster < fs.props.total_clusters)
                .map(EntryLocationUnit::DataCluster)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryStatus {
    Unused,
    LastUnused,
    Used,
}

/// The location of a [`FATDirEntry`]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct EntryLocation {
    /// the location of the first corresponding entry's data unit
    pub(crate) unit: EntryLocationUnit,
    /// the first entry's index/offset from the start of the data unit
    pub(crate) index: EntryIndex,
}

impl EntryLocation {
    pub(crate) fn from_partition_sector<S, C>(sector: SectorIndex, fs: &FileSystem<S, C>) -> Self
    where
        S: BlockRead,
        C: Clock,
    {
        let unit = EntryLocationUnit::from_partition_sector(sector, fs);

        Self { unit, index: 0 }
    }

    pub(crate) fn entry_status<S, C>(&self, fs: &FileSystem<S, C>) -> Result<EntryStatus, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        let entry_sector = self.get_entry_sector(fs);
        fs.load_nth_sector(entry_sector)?;

        let byte_offset = self.get_sector_byte_offset(fs);
        Ok(match fs.sector_buffer.borrow()[byte_offset] {
            UNUSED_ENTRY => EntryStatus::Unused,
            LAST_AND_UNUSED_ENTRY => EntryStatus::LastUnused,
            _ => EntryStatus::Used,
        })
    }

    #[inline]
    pub(crate) fn get_entry_sector<S, C>(&self, fs: &FileSystem<S, C>) -> SectorIndex
    where
        S: BlockRead,
        C: Clock,
    {
        let sector_offset: SectorCount = SectorCount::from(self.index)
            * SectorCount::try_from(DIRENTRY_SIZE).expect("32 can fit into a u32")
            / SectorCount::from(fs.sector_size());

        self.unit.get_entry_sector(fs) + sector_offset
    }

    #[inline]
    pub(crate) fn get_sector_byte_offset<S, C>(&self, fs: &FileSystem<S, C>) -> usize
    where
        S: BlockRead,
        C: Clock,
    {
        (usize::from(self.index) * DIRENTRY_SIZE) % usize::from(fs.props.sector_size)
    }

    // Note: this could also return a borrowed subslice from fs.sector_buffer,
    // but since it is only 32 bytes, I don't think it is worth the hastle
    pub(crate) fn get_bytes<S, C>(
        &self,
        fs: &FileSystem<S, C>,
    ) -> Result<[u8; DIRENTRY_SIZE], S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        let entry_sector = self.get_entry_sector(fs);
        let entry_offset = self.get_sector_byte_offset(fs);
        let mut bytes = [0u8; DIRENTRY_SIZE];
        bytes.copy_from_slice(
            &fs.load_nth_sector(entry_sector)?[entry_offset..entry_offset + DIRENTRY_SIZE],
        );

        Ok(bytes)
    }

    pub(crate) fn set_bytes<S, C>(
        &self,
        fs: &FileSystem<S, C>,
        bytes: [u8; DIRENTRY_SIZE],
    ) -> Result<(), S::Error>
    where
        S: BlockWrite,
        C: Clock,
    {
        let entry_sector = self.get_entry_sector(fs);
        let entry_offset = self.get_sector_byte_offset(fs);
        fs.load_nth_sector(entry_sector)?;
        fs.sector_buffer.borrow_mut()[entry_offset..entry_offset + DIRENTRY_SIZE]
            .copy_from_slice(&bytes);
        fs.set_modified();

        Ok(())
    }

    pub(crate) fn free_entry<S, C>(
        &self,
        fs: &FileSystem<S, C>,
        is_last: bool,
    ) -> Result<(), S::Error>
    where
        S: BlockWrite,
        C: Clock,
    {
        let entry_sector = self.unit.get_entry_sector(fs);
        fs.load_nth_sector(entry_sector)?;

        let byte_offset = self.get_sector_byte_offset(fs);
        fs.sector_buffer.borrow_mut()[byte_offset] = if is_last {
            LAST_AND_UNUSED_ENTRY
        } else {
            UNUSED_ENTRY
        };
        fs.set_modified();

        Ok(())
    }

    pub(crate) fn next_entry<S, C>(
        mut self,
        fs: &FileSystem<S, C>,
    ) -> Result<Option<EntryLocation>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        self.index += 1;

        // we haven't advanced to a new unit, we return immediately
        if self.index < self.unit.get_max_offset(fs) {
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
    pub(crate) fn nth_entry<S, C>(
        self,
        fs: &FileSystem<S, C>,
        n: num::NonZero<EntryIndex>,
    ) -> Result<Option<EntryLocation>, S::Error>
    where
        S: BlockRead,
        C: Clock,
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
#[derive(Debug, Clone, Copy)]
pub(crate) struct DirEntryChain {
    /// the location of the first corresponding entry
    pub(crate) location: EntryLocation,
    /// how many (contiguous) entries this entry chain has
    pub(crate) len: u16,
}
