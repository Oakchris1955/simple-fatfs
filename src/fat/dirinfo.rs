use typed_path::Utf8Component;

use super::serde::boot_sector::{BootRecord, Ebr};
use super::serde::location::{EntryLocation, EntryLocationUnit};
use super::serde::readdir::{ReadDir, ReadDirRaw};
use crate::FileSystem;
use crate::block_io::prelude::*;
use crate::error::{FSError, FSResult};
use crate::log::local_log;
use crate::path::{Path, PathBuf, find_common_path_prefix, keep_path_normals, path_consts};
use crate::time::Clock;
#[cfg(feature = "bloom")]
use crate::utils;

/*
 * I have opted for using associated functions instead of methods for
 * `DirInfo`, since each function call requires a corresponding
 * `FileSystem` object, otherwise data corruption may happen
 * (this won't happen in practice, since each FileSystem only handles
 * the `DirInfo` it generates)
 */

#[derive(Debug)]
pub(crate) struct DirInfo {
    path: PathBuf,
    pub(crate) chain_start: EntryLocationUnit,
    /// Indicates the [`EntryLocation`] of the last known allocated or removed [`DirEntry`](crate::fat::DirEntry)
    ///
    /// [`None`] if it is not known
    pub(crate) chain_end: Option<EntryLocation>,
    #[cfg(feature = "bloom")]
    pub(crate) filter: Option<utils::bloom::Bloom<str>>,
}

impl DirInfo {
    pub(crate) fn at_root_dir(boot_record: &BootRecord) -> Self {
        Self {
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

    /// Change the internal directory cache so that it points to the parent directory.
    ///
    /// If this is the root directory, it does nothing
    fn go_to_parent_dir<S, C>(this: &mut Self, fs: &FileSystem<S, C>) -> FSResult<(), S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        if let Some(parent_path) = this.path.parent() {
            let parent_pathbuf = parent_path.to_path_buf();

            let entries = Self::process_current_dir(this, fs);

            let parent_entry = entries.get_parent_dir_entry()?;

            this.path = parent_pathbuf;
            this.chain_start = EntryLocationUnit::DataCluster(parent_entry.data_cluster);
            this.reset();
        } else {
            Self::go_to_root_directory(this, fs);
        }

        Ok(())
    }

    /// Change the internal directory cache so that it points to the given child directory
    fn go_to_child_dir<S, C>(
        this: &mut Self,
        name: &str,
        fs: &FileSystem<S, C>,
    ) -> FSResult<(), S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        let mut entries = Self::process_current_dir(this, fs);

        let child_entry = loop {
            let entry = entries.next().ok_or(FSError::NotFound)??;

            if entry.name() == name {
                break entry;
            }
        };

        if !child_entry.is_dir {
            return Err(FSError::NotADirectory);
        }

        this.path.push(child_entry.name());
        this.chain_start = EntryLocationUnit::DataCluster(child_entry.data_cluster);
        this.reset();

        Ok(())
    }

    /// Change the internal directory cache so that it points to the root directory
    pub(crate) fn go_to_root_directory<S, C>(this: &mut Self, fs: &FileSystem<S, C>)
    where
        S: BlockRead,
        C: Clock,
    {
        *this = Self::at_root_dir(&fs.boot_record.borrow());
    }

    /// Backtrack to each parent directory until the internal directory cache points
    /// to the target directory
    fn go_up_till_target<P, S, C>(
        this: &mut Self,
        target: P,
        fs: &FileSystem<S, C>,
    ) -> FSResult<(), S::Error>
    where
        P: AsRef<Path>,
        S: BlockRead,
        C: Clock,
    {
        let target = target.as_ref();

        while this.path != target {
            Self::go_to_parent_dir(this, fs)?;
        }

        Ok(())
    }

    /// Navigate down child directories until the internal directory cache points
    /// to the target directory
    fn go_down_till_target<P, S, C>(
        this: &mut Self,
        target: P,
        fs: &FileSystem<S, C>,
    ) -> FSResult<(), S::Error>
    where
        P: AsRef<Path>,
        S: BlockRead,
        C: Clock,
    {
        let target = target.as_ref();

        let common_path_prefix = find_common_path_prefix(&this.path, target);
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
            Self::go_to_child_dir(this, dir_name.as_str(), fs)?;
        }

        Ok(())
    }

    // There are many ways this can be achieved. That's how we'll do it:
    // Firstly, we find the common path prefix of the `current_path` and the `target`
    // Then, we check whether it is faster to start from the root directory
    // and get down to the target or whether we should start from where we are
    // now, go up till we find the common prefix path and then go down to the `target`

    /// Change the internal directory cache so that is points to the provided `target`
    pub(crate) fn go_to_dir<P, S, C>(
        this: &mut Self,
        target: P,
        fs: &FileSystem<S, C>,
    ) -> FSResult<(), S::Error>
    where
        P: AsRef<Path>,
        S: BlockRead,
        C: Clock,
    {
        let target = target.as_ref();

        if !target.is_valid() {
            return Err(FSError::MalformedPath);
        }

        if this.path == target {
            // there's a chance that the current loaded sector doesn't belong
            // to the directory we have cached, so we must also navigate to the correct sector
            Self::go_to_cached_dir(this, fs)?;

            return Ok(());
        }

        let common_path_prefix = find_common_path_prefix(&this.path, target);

        // Note: these are the distances to the common prefix, not the target path
        let distance_from_root = common_path_prefix.ancestors().count() - 1;
        let distance_from_current_path = (this.path.ancestors().count() - 1) - distance_from_root;

        if distance_from_root <= distance_from_current_path {
            Self::go_to_root_directory(this, fs);

            Self::go_down_till_target(this, target, fs)?;
        } else {
            Self::go_up_till_target(this, common_path_prefix, fs)?;

            Self::go_down_till_target(this, target, fs)?;
        }

        Ok(())
    }

    /// Load into the sector buffer the first sector of the internal directory cache chain
    pub(crate) fn go_to_cached_dir<S, C>(
        this: &Self,
        fs: &FileSystem<S, C>,
    ) -> FSResult<(), S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        let target_sector = EntryLocationUnit::get_entry_sector(&this.chain_start, fs);

        if target_sector != fs.sector_buffer.borrow().stored_sector() {
            fs.load_nth_sector(target_sector)?;
        }

        Ok(())
    }

    pub(crate) fn process_current_dir<'a, S, C>(
        this: &Self,
        fs: &'a FileSystem<S, C>,
    ) -> ReadDirRaw<'a, S, C>
    where
        S: BlockRead,
        C: Clock,
    {
        ReadDirRaw::new(fs, this.chain_start)
    }

    /// Low-level method to obtain a [`ReadDir`] struct of the provided `path` directory.
    ///
    /// Use the `internal` parameter to configure whether the returned [`ReadDir`]
    /// is intended for internal or public use and whether the `.` and `..` entries
    /// should be filtered or not (only filtered if `internal` is false)
    fn read_dir_internal<'a, P: AsRef<Path>, S, C>(
        this: &mut Self,
        path: P,
        internal: bool,
        fs: &'a FileSystem<S, C>,
    ) -> FSResult<ReadDir<'a, S, C>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        // normalize the given path
        let path = path.as_ref();

        if !path.is_valid() {
            return Err(FSError::MalformedPath);
        }

        let path = path.normalize();

        local_log::debug!("Reading directory {path}");

        Self::go_to_dir(this, &path, fs)?;

        Ok(ReadDir::new(fs, &this.chain_start, &this.path, internal))
    }

    /// Like [`read_dir`](Self::read_dir), but doesn't filter files based on whether
    /// they are hidden or system files.
    ///
    /// Can come in handy if you don't want to skip hidden or system files, for
    /// example during a directory deletion.
    #[inline]
    pub(crate) fn read_dir_raw<'a, P: AsRef<Path>, S, C>(
        this: &mut Self,
        path: P,
        fs: &'a FileSystem<S, C>,
    ) -> FSResult<ReadDir<'a, S, C>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        Self::read_dir_internal(this, path, true, fs)
    }

    /// Return a [`ReadDir`] iterator of all the entries of the provided directory
    ///
    /// # Errors
    ///
    /// Fails if `path` doesn't represent a directory, or if that directory doesn't exist
    #[inline]
    pub(crate) fn read_dir<'a, P: AsRef<Path>, S, C>(
        this: &mut Self,
        path: P,
        fs: &'a FileSystem<S, C>,
    ) -> FSResult<ReadDir<'a, S, C>, S::Error>
    where
        S: BlockRead,
        C: Clock,
    {
        Self::read_dir_internal(this, path, false, fs)
    }

    /// Reset `chain_end` and `filter` fields
    ///
    /// Use this when internally changing directories so that e.g. the `filter`
    /// field will also be set to [`None`] when the `bloom` feature is enabled
    fn reset(&mut self) {
        self.chain_end = None;
        #[cfg(feature = "bloom")]
        {
            self.filter = None
        }
    }
}
