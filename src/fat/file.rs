use super::*;

use core::{cmp, num, ops};

#[cfg(not(feature = "std"))]
use alloc::{borrow::ToOwned, string::String, vec::Vec};
use time::{Date, PrimitiveDateTime};

use crate::utils::{self, bincode::BINCODE_CONFIG};
use crate::{FSError, FSResult, InternalFSError};

use embedded_io::*;

#[derive(Debug)]
pub(crate) struct FileProps {
    pub(crate) entry: Properties,
    /// the byte offset of the R/W pointer
    ///
    /// this can't exceed the file size, so they share the same data type
    pub(crate) offset: FileSize,
    pub(crate) current_cluster: ClusterIndex,
}

/// A read-only file within a FAT filesystem
///
/// Note: whether or not your FileSystem is RO or R/W, this won't update
/// the [`ROFile::last_accessed_date()`](Properties::last_accessed_date())
/// If you want to avoid this behaviour in a R/W filesystem, use [`RWFile`]
#[derive(Debug)]
pub struct ROFile<'a, S>
where
    S: Read + Seek,
{
    pub(crate) fs: &'a FileSystem<S>,
    pub(crate) props: FileProps,
}

impl<S> ops::Deref for ROFile<'_, S>
where
    S: Read + Seek,
{
    type Target = Properties;

    fn deref(&self) -> &Self::Target {
        &self.props.entry
    }
}

impl<S> ops::DerefMut for ROFile<'_, S>
where
    S: Read + Seek,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.props.entry
    }
}

// Constructors
impl<'a, S> ROFile<'a, S>
where
    S: Read + Seek,
{
    pub(crate) fn from_props(props: FileProps, fs: &'a FileSystem<S>) -> Self {
        Self { fs, props }
    }
}

// Internal functions
impl<S> ROFile<'_, S>
where
    S: Read + Seek,
{
    #[inline]
    /// Panics if the current cluser doesn't point to another clluster
    fn next_cluster(&mut self) -> Result<(), <Self as ErrorType>::Error> {
        // when a `ROFile` is created, `cluster_chain_is_healthy` is called, if it fails, that ROFile is dropped
        self.props.current_cluster = self.get_next_cluster()?.unwrap();

        Ok(())
    }

    #[inline]
    /// Non-[`panic`]king version of [`next_cluster()`](ROFile::next_cluster)
    fn get_next_cluster(&mut self) -> Result<Option<ClusterIndex>, <Self as ErrorType>::Error> {
        self.fs.get_next_cluster(self.props.current_cluster)
    }

    /// Returns that last cluster in the file's cluster chain
    fn last_cluster_in_chain(&mut self) -> Result<ClusterIndex, <Self as ErrorType>::Error> {
        // we begin from the current cluster to save some time
        let mut current_cluster = self.props.current_cluster;

        loop {
            match self.fs.read_nth_FAT_entry(current_cluster)? {
                FATEntry::Allocated(next_cluster) => current_cluster = next_cluster,
                FATEntry::Eof => break,
                _ => unreachable!(),
            }
        }

        Ok(current_cluster)
    }

    /// Checks whether the cluster chain of this file is healthy or malformed
    pub(crate) fn cluster_chain_is_healthy(&mut self) -> Result<bool, S::Error> {
        let mut current_cluster = self.data_cluster;
        let mut cluster_count = 0;

        loop {
            cluster_count += 1;

            if cluster_count * self.fs.cluster_size() >= self.file_size {
                break;
            }

            match self.fs.read_nth_FAT_entry(current_cluster)? {
                FATEntry::Allocated(next_cluster) => current_cluster = next_cluster,
                _ => return Ok(false),
            };
        }

        Ok(true)
    }

    fn offset_from_seekfrom(&self, seekfrom: SeekFrom) -> u64 {
        match seekfrom {
            SeekFrom::Start(offset) => offset,
            SeekFrom::Current(offset) => {
                let offset = i64::from(self.props.offset) + offset;
                offset.try_into().unwrap_or(u64::MIN)
            }
            SeekFrom::End(offset) => {
                let offset = i64::from(self.file_size) + offset;
                offset.try_into().unwrap_or(u64::MIN)
            }
        }
    }
}

impl<S> ErrorType for ROFile<'_, S>
where
    S: Read + Seek,
{
    type Error = S::Error;
}

impl<S> Read for ROFile<'_, S>
where
    S: Read + Seek,
{
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        let mut bytes_read = 0;
        // this is the maximum amount of bytes that can be read
        let read_cap = cmp::min(
            buf.len(),
            // we better not panic here (this could be an issue only on 16-bit targets tho)
            usize::try_from(self.file_size - self.props.offset).unwrap_or(usize::MAX),
        );

        'outer: loop {
            let sector_init_offset =
                self.props.offset % self.fs.cluster_size() / u32::from(self.fs.sector_size());
            let first_sector_of_cluster = self
                .fs
                .data_cluster_to_partition_sector(self.props.current_cluster)
                + sector_init_offset;
            let last_sector_of_cluster = first_sector_of_cluster
                + SectorCount::from(self.fs.sectors_per_cluster())
                - sector_init_offset
                - 1;
            log::debug!(
                "Reading cluster {} from sectors {} to {}",
                self.props.current_cluster,
                first_sector_of_cluster,
                last_sector_of_cluster
            );

            for sector in first_sector_of_cluster..=last_sector_of_cluster {
                self.fs.load_nth_sector(sector)?;

                let start_index = usize::try_from(self.props.offset % u32::from(self.fs.sector_size()))
                    .expect("sector_size's upper limit is 2^16, within Rust's usize (Rust support 16, 32 and 64-bit archs)");
                let bytes_to_read = cmp::min(
                    read_cap - bytes_read,
                    usize::from(self.fs.sector_size()) - start_index,
                );
                log::debug!(
                    "Gonna read {bytes_to_read} bytes from sector {sector} starting at byte {start_index}"
                );

                buf[bytes_read..bytes_read + bytes_to_read].copy_from_slice(
                    &self.fs.sector_buffer.borrow()[start_index..start_index + bytes_to_read],
                );

                bytes_read += bytes_to_read;
                self.props.offset += FileSize::try_from(bytes_to_read).unwrap();

                // if we have read as many bytes as we want...
                if bytes_read >= read_cap {
                    // ...but we must process get the next cluster for future uses,
                    // we do that before breaking
                    if self.props.offset % self.fs.cluster_size() == 0
                        && self.props.offset < self.file_size
                    {
                        self.next_cluster()?;
                    }

                    break 'outer;
                }
            }

            self.next_cluster()?;
        }

        Ok(bytes_read)
    }
}

impl<S> Seek for ROFile<'_, S>
where
    S: Read + Seek,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, Self::Error> {
        let mut offset = self.offset_from_seekfrom(pos);

        // seek beyond EOF behaviour is implementation-defined,
        // so we just move to EOF
        offset = cmp::min(offset, self.file_size.into());

        let offset = FileSize::try_from(offset)
            .expect("file_size is u32, so offset must be able to fit in a u32 too");

        log::trace!(
            "Previous cursor offset is {}, new cursor offset is {}",
            self.props.offset,
            offset
        );

        use cmp::Ordering;
        match offset.cmp(&self.props.offset) {
            Ordering::Less => {
                // here, we basically "rewind" back to the start of the file and then seek to where we want
                // this of course has performance issues, so TODO: find a solution that is both memory & time efficient
                // (perhaps we could follow a similar approach to elm-chan's FATFS, by using a cluster link map table, perhaps as an optional feature)
                self.props.offset = 0;
                self.props.current_cluster = self.data_cluster;
                self.seek(SeekFrom::Start(offset.into()))?;
            }
            Ordering::Equal => (),
            Ordering::Greater => {
                for _ in self.props.offset / self.fs.cluster_size()..offset / self.fs.cluster_size()
                {
                    self.next_cluster()?;
                }
                self.props.offset = offset;
            }
        }

        Ok(self.props.offset.into())
    }
}

/// A read-write file within a FAT filesystem
///
/// The size of the file will be automatically adjusted
/// if the cursor goes beyond EOF.
///
/// To reduce a file's size, use the [`truncate`](RWFile::truncate) method
#[derive(Debug)]
pub struct RWFile<'a, S>
where
    S: Read + Write + Seek,
{
    pub(crate) ro_file: ROFile<'a, S>,
    /// Represents whether or not the file has been written to
    pub(crate) entry_modified: bool,
}

impl<'a, S> From<ROFile<'a, S>> for RWFile<'a, S>
where
    S: Read + Write + Seek,
{
    fn from(value: ROFile<'a, S>) -> Self {
        Self {
            ro_file: value,
            entry_modified: false,
        }
    }
}

impl<'a, S> ops::Deref for RWFile<'a, S>
where
    S: Read + Write + Seek,
{
    type Target = ROFile<'a, S>;

    fn deref(&self) -> &Self::Target {
        &self.ro_file
    }
}

impl<S> ops::DerefMut for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ro_file
    }
}

// Constructors
impl<'a, S> RWFile<'a, S>
where
    S: Read + Write + Seek,
{
    pub(crate) fn from_props(props: FileProps, fs: &'a FileSystem<S>) -> Self {
        ROFile::from_props(props, fs).into()
    }
}

// Public functions
impl<S> RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    /// Set the last accessed [`Date`] attribute of this file
    pub fn set_accessed(&mut self, accessed: Date) {
        self.accessed = Some(accessed);

        self.entry_modified = true;
    }

    /// Set the creation [`DateTime`](PrimitiveDateTime) attributes of this file
    pub fn set_created(&mut self, created: PrimitiveDateTime) {
        self.created = Some(created);

        self.entry_modified = true;
    }

    /// Set the last modified [`DateTime`](PrimitiveDateTime) attributes of this file
    pub fn set_modified(&mut self, modified: PrimitiveDateTime) {
        self.modified = modified;

        self.entry_modified = true;
    }

    /// Truncates the file to a given size, deleting everything past the new EOF
    ///
    /// If `size` is greater or equal to the current file size
    /// till the end of the last cluster allocated, this has no effect
    /// to the file contents
    ///
    /// Furthermore, if the cursor point is beyond the new EOF, it will be moved there
    pub fn truncate(&mut self, size: FileSize) -> Result<(), <Self as ErrorType>::Error> {
        // looks like the new truncated size would be smaller than the current one, so we just return
        if size.next_multiple_of(self.fs.props.cluster_size) >= self.file_size {
            if size < self.file_size {
                self.file_size = size;
            }

            return Ok(());
        }

        // we store the current offset for later use
        let previous_offset = cmp::min(self.props.offset, size);

        // we seek back to where the EOF will be
        self.seek(SeekFrom::Start(size.into()))?;

        // set what the new filesize will be
        let previous_size = self.file_size;
        self.file_size = size;

        let mut next_cluster_option = self.get_next_cluster()?;

        // we set the new last cluster in the chain to be EOF
        self.ro_file
            .fs
            .write_nth_FAT_entry(self.ro_file.props.current_cluster, FATEntry::Eof)?;

        // then, we set each cluster after the current one to EOF
        while let Some(next_cluster) = next_cluster_option {
            next_cluster_option = self.fs.get_next_cluster(next_cluster)?;

            self.fs.write_nth_FAT_entry(next_cluster, FATEntry::Free)?;
        }

        // don't forget to seek back to where we started
        self.seek(SeekFrom::Start(previous_offset.into()))?;

        log::debug!(
            "Successfully truncated file {} from {} to {} bytes",
            self.path,
            previous_size,
            self.file_size
        );

        self.entry_modified = true;

        Ok(())
    }

    /// Remove the current file from the [`FileSystem`]
    pub fn remove(mut self) -> Result<(), <Self as ErrorType>::Error> {
        // we begin by removing the corresponding entries...
        self.ro_file
            .fs
            .remove_entry_chain(&self.ro_file.props.entry.chain)?;

        // ... and then we free the data clusters

        // rewind back to the start of the file
        self.rewind()?;

        let current_cluster = self.ro_file.props.current_cluster;
        self.ro_file.fs.free_cluster_chain(current_cluster)?;

        // we are removing the file, no reason to sync it back to the filesystem
        // (apart from that, we also won't overwrite the UNUSED_ENTRY flag
        // on our dir entry assigned by the remove_entry_chain call above
        self.entry_modified = false;

        Ok(())
    }
}

// Private functions
impl<S> RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn sync_entry(&mut self) -> FSResult<(), S::Error> {
        if self.entry_modified {
            let direntry = FATDirEntry::from(MinProperties::from(self.props.entry.clone()));
            let mut bytes = [0; DIRENTRY_SIZE];
            bincode::encode_into_slice(direntry, &mut bytes, BINCODE_CONFIG)
                .map_err(utils::bincode::map_err_enc)?;

            let chain_start = self.props.entry.chain.location;
            let file_name = self
                .path()
                .file_name()
                .expect("This file name should be valid")
                .to_owned();
            // the first entry of the dirchain could belong to a LFNEntry, so we must handle that
            let direntry_location = match num::NonZero::new(
                EntryCount::from(calc_entries_needed(file_name, &self.fs.options.codepage)) - 1,
            ) {
                Some(nonzero) => {
                    chain_start
                        .nth_entry(self.fs, nonzero)?
                        .ok_or(FSError::InternalFSError(
                            InternalFSError::MalformedEntryChain,
                        ))?
                }
                None => chain_start,
            };

            direntry_location.set_bytes(self.fs, bytes)?;

            self.entry_modified = false;
        }

        Ok(())
    }

    #[inline]
    fn _set_accessed(&mut self) {
        if self.fs.options.update_file_fields {
            let now = self.fs.options.clock.now();

            if let Some(accessed) = &mut self.accessed {
                *accessed = now.date();
            }

            self.entry_modified = true;
        }
    }

    #[inline]
    fn _set_modified(&mut self) {
        if self.fs.options.update_file_fields {
            let now = self.fs.options.clock.now();

            if let Some(accessed) = &mut self.accessed {
                *accessed = now.date();
            }
            self.modified = now;

            self.entry_modified = true;
        }
    }
}

#[derive(Debug)]
#[non_exhaustive] // TODO: see whether or not to keep this marked as non-exhaustive
/// A [`RWFile`]-exclusive IO error struct
pub enum RWFileError<I>
where
    I: Error,
{
    /// The underlying storage is full.
    StorageFull,
    /// An IO error occured
    IOError(I),
}

impl<I> Error for RWFileError<I>
where
    I: Error,
{
    #[inline]
    fn kind(&self) -> ErrorKind {
        match self {
            // TODO: when embedded-io adds a StorageFull variant, use that instead
            Self::StorageFull => ErrorKind::OutOfMemory,
            Self::IOError(err) => err.kind(),
        }
    }
}

impl<I> From<I> for RWFileError<I>
where
    I: Error,
{
    #[inline]
    fn from(value: I) -> Self {
        Self::IOError(value)
    }
}

impl<S> ErrorType for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    type Error = RWFileError<S::Error>;
}

impl<S> Read for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        let res = self.ro_file.read(buf);

        if res.is_ok() {
            self._set_accessed()
        };

        res.map_err(|e| e.into())
    }

    #[inline]
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), ReadExactError<Self::Error>> {
        let res = self.ro_file.read_exact(buf);

        if res.is_ok() {
            self._set_accessed()
        };

        res.map_err(|e| match e {
            ReadExactError::UnexpectedEof => ReadExactError::UnexpectedEof,
            ReadExactError::Other(err) => ReadExactError::Other(err.into()),
        })
    }
}

impl<S> Write for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn write(&mut self, mut buf: &[u8]) -> Result<usize, Self::Error> {
        let cur_offset = self.props.offset;

        // seek beyond EOF behaviour is implementation-defined,
        // so we just allocate the maximum possible space
        if u64::try_from(buf.len()).unwrap_or(u64::MAX) > FileSize::MAX.into() {
            log::warn!("a file can be up to 2^32 bytes long, can't have a file larger than that");

            buf = &buf[..FileSize::MAX as usize];
        };

        // allocate clusters
        self.seek(SeekFrom::Start(u64::from(cur_offset) + buf.len() as u64))?;
        // rewind back to where we were
        self.seek(SeekFrom::Start(cur_offset.into()))?;

        let mut bytes_written = 0;

        'outer: loop {
            log::trace!(
                "writing file data to cluster: {}",
                self.props.current_cluster
            );

            let sector_init_offset =
                self.props.offset % self.fs.cluster_size() / u32::from(self.fs.sector_size());
            let first_sector_of_cluster = self
                .fs
                .data_cluster_to_partition_sector(self.props.current_cluster)
                + sector_init_offset;
            let last_sector_of_cluster = first_sector_of_cluster
                + SectorCount::from(self.fs.sectors_per_cluster())
                - sector_init_offset
                - 1;
            for sector in first_sector_of_cluster..=last_sector_of_cluster {
                self.fs.load_nth_sector(sector)?;

                let start_index = usize::try_from(self.props.offset % u32::from(self.fs.sector_size()))
                    .expect("sector_size's upper limit is 2^16, within Rust's usize (Rust support 16, 32 and 64-bit archs)");

                let bytes_to_write = cmp::min(
                    buf.len() - bytes_written,
                    usize::from(self.fs.sector_size()) - start_index,
                );

                self.fs.sector_buffer.borrow_mut()[start_index..start_index + bytes_to_write]
                    .copy_from_slice(&buf[bytes_written..bytes_written + bytes_to_write]);
                self.fs.set_modified();

                bytes_written += bytes_to_write;
                self.props.offset += FileSize::try_from(bytes_to_write).unwrap();

                // if we have written as many bytes as we want...
                if bytes_written >= buf.len() {
                    // ...but we must process get the next cluster for future uses,
                    // we do that before breaking
                    if self.props.offset % self.fs.cluster_size() == 0 {
                        self.next_cluster()?;
                    }

                    break 'outer;
                }
            }

            self.next_cluster()?;
        }

        // we've written something at this point
        self._set_modified();

        Ok(bytes_written)
    }

    // everything is immediately written to the storage medium
    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<S> Seek for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, Self::Error> {
        let offset = self.offset_from_seekfrom(pos);

        let offset = match FileSize::try_from(offset) {
            Ok(offset) => offset,
            Err(_) => {
                log::warn!(
                    "a file can be up to 2^32 bytes long, can't have a file larger than that"
                );

                FileSize::MAX
            }
        };

        // in case the cursor goes beyond the EOF, allocate more clusters
        if offset > self.file_size.next_multiple_of(self.fs.cluster_size()) {
            let bytes_allocated = if self.file_size == 0 {
                // even if the file size is zero, a file has a cluster already allocated
                self.fs.props.cluster_size
            } else {
                self.file_size.next_multiple_of(self.fs.cluster_size())
            };
            let clusters_to_allocate = (offset - bytes_allocated).div_ceil(self.fs.cluster_size());
            log::debug!("Seeking beyond EOF, allocating {clusters_to_allocate} more clusters");

            let last_cluster_in_chain = self.last_cluster_in_chain()?;

            // TODO: if possible, find how many clusters we successfully allocated
            // and modify the file length accordingly
            match self.fs.allocate_clusters(
                num::NonZero::new(clusters_to_allocate).expect("This is greater than 1"),
                Some(last_cluster_in_chain),
            ) {
                Ok(_) => (),
                Err(_) => return Err(RWFileError::StorageFull),
            };

            self.file_size = offset;
            log::debug!(
                "New file size after reallocation is {} bytes",
                self.file_size
            );
        }

        self._set_accessed();
        self.entry_modified = true;

        self.ro_file.seek(pos).map_err(|e| e.into())
    }
}

impl<S> Drop for RWFile<'_, S>
where
    S: Read + Write + Seek,
{
    fn drop(&mut self) {
        // nothing to do if this errors out while dropping
        let _ = self.sync_entry();
    }
}
