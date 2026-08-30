use core::iter;

use super::DIRENTRY_SIZE;
use super::entry_composer::{LAST_AND_UNUSED_ENTRY, UNUSED_ENTRY};
use crate::block_io::prelude::*;
use crate::time::Clock;
use crate::{ClusterIndex, EntryCount, EntryIndex, FileSystem, SectorCount, SectorIndex};

/// The location of a chain of [`FATDirEntry`](crate::fat::serde::FATDirEntry)
#[derive(Debug, Clone, Copy)]
pub(crate) struct DirEntryChain {
    /// the location of the first corresponding entry
    pub(crate) location: EntryLocation,
    /// how many (contiguous) entries this entry chain has
    pub(crate) len: EntryCount,
}

/// An iterator of sequential [`EntryLocation`]s
///
/// # Note
///
/// It is guaranteed that the first element of this iterator will be [`Some`]\([`Ok`]\(`entry_loc`)\)
#[derive(Debug)]
pub(crate) struct EntryLocationIter<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    entry_loc: Option<EntryLocation>,
    is_first_loc: bool,
    fs: &'a FileSystem<S, C>,
}

impl<'a, S, C> EntryLocationIter<'a, S, C>
where
    S: BlockRead,
    C: Clock,
{
    /// Construct a new iterator, starting at `first_loc`
    pub(crate) fn new(first_loc: EntryLocation, fs: &'a FileSystem<S, C>) -> Self {
        Self {
            entry_loc: Some(first_loc),
            is_first_loc: true,
            fs,
        }
    }
}

impl<S, C> Iterator for EntryLocationIter<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
    type Item = Result<EntryLocation, S::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        /* we could avoid the `is_first_loc` field by just using `mem::replace`
         * on the current `entry_loc`, but this could result in unnecessary overhead
         * if the last time we can `next` we need to navigate to a new sector
         */
        if self.is_first_loc {
            self.is_first_loc = false;
            return Ok(self.entry_loc).transpose();
        }

        EntryLocation::next_entry(self.entry_loc?, self.fs)
            .inspect(|next| self.entry_loc = *next)
            .transpose()
    }
}

impl<S, C> iter::FusedIterator for EntryLocationIter<'_, S, C>
where
    S: BlockRead,
    C: Clock,
{
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

    fn next_entry<S, C>(
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
            EntryLocationUnit::RootDirSector(sector) => {
                if fs.props.root_dir_sectors() == 0 {
                    unreachable!(concat!(
                        "This should be zero iff the FAT type if FAT32, ",
                        "in which case we won't even be reading root directory sectors, since it doesn't exist"
                    ))
                }

                Ok((SectorIndex::from(*sector)
                    < fs.props.first_root_dir_sector()
                        + SectorCount::from(fs.props.root_dir_sectors()))
                .then_some(EntryLocationUnit::RootDirSector(sector + 1)))
            }
            EntryLocationUnit::DataCluster(cluster) => Ok(fs
                .get_next_cluster(*cluster)?
                .filter(|cluster| *cluster < fs.props.total_clusters())
                .map(EntryLocationUnit::DataCluster)),
        }
    }
}
