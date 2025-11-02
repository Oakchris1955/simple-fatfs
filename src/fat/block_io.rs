//! Block device-related IO traits and adapters
//!
//! Low-level storage devices, like flash memories, operate in units of data
//! which will hereby after be referred as blocks. This library assumes that
//! both for read and write operations the block size will be the same.

use super::*;

use embedded_io::ErrorType;

/// The `BlockRead` traits allows to read data from a source in units of blocks.
pub trait BlockRead: ErrorType {
    /// Size of a block, must be a power of two. A panic may occur if this isn't
    /// a power of two other than zero.
    const SIZE: usize;

    /// Read one or multiple blocks from the device medium, starting at `block`
    ///
    /// The underlying implementation should expect a `buf` with a length multiple
    /// of `SIZE`. If that isn't the case, a panic may occur.
    fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error>;
}

/// The `BlockRead` traits allows to write data to a sink in units of blocks.
pub trait BlockWrite: BlockRead {
    /// Write one or multiple blocks to the device medium, starting at `block`
    ///
    /// The underlying implementation should expect a `buf` with a length multiple
    /// of `SIZE`. If that isn't the case, a panic may occur.
    fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error>;

    /// Flushes this output stream, ensuring that all intermediately buffered contents reach their destination.
    fn flush(&mut self) -> Result<(), Self::Error>;
}

impl<T: BlockRead> BlockRead for &mut T {
    const SIZE: usize = T::SIZE;

    #[inline]
    fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
        T::read(self, block, buf)
    }
}
impl<T: BlockWrite> BlockWrite for &mut T {
    #[inline]
    fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
        T::write(self, block, buf)
    }

    #[inline]
    fn flush(&mut self) -> Result<(), Self::Error> {
        T::flush(self)
    }
}

#[cfg(feature = "std")]
pub(crate) mod from_std {
    use crate::{BlockIndex, BlockRead, BlockWrite, MIN_SECTOR_SIZE};
    use std::io::{Error, Read, Seek, SeekFrom, Write};

    /// Adapter from `std::io` traits.
    #[derive(Clone, Debug)]
    pub struct FromStd<T: ?Sized, const SIZE: usize = MIN_SECTOR_SIZE> {
        inner: T,
    }

    impl<T> FromStd<T> {
        /// Create a new adapter with the default block size.
        pub fn new(inner: T) -> Self {
            Self { inner }
        }
    }

    impl<T, const SIZE: usize> FromStd<T, SIZE> {
        /// Create a new adapter with the default block size.
        pub fn with_block_size(inner: T) -> Self {
            Self { inner }
        }
    }

    impl<T, const SIZE: usize> FromStd<T, SIZE> {
        /// Consume the adapter, returning the inner object.
        pub fn into_inner(self) -> T {
            self.inner
        }
    }

    impl<T: ?Sized, const SIZE: usize> FromStd<T, SIZE> {
        /// Borrow the inner object.
        pub fn inner(&self) -> &T {
            &self.inner
        }

        /// Mutably borrow the inner object.
        pub fn inner_mut(&mut self) -> &mut T {
            &mut self.inner
        }
    }

    impl<T: ?Sized, const SIZE: usize> embedded_io::ErrorType for FromStd<T, SIZE> {
        type Error = Error;
    }

    impl<T: Read + Seek + ?Sized, const SIZE: usize> BlockRead for FromStd<T, SIZE> {
        const SIZE: usize = SIZE;

        fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
            assert!(
                buf.len().is_multiple_of(Self::SIZE),
                "expected the buffer size ({}) to be a multiple of the medium's block size ({})",
                buf.len(),
                Self::SIZE
            );

            self.inner
                .seek(SeekFrom::Start(u64::from(block) * (Self::SIZE as u64)))?;

            self.inner.read_exact(buf)?;

            Ok(())
        }
    }

    impl<T: Read + Write + Seek + ?Sized, const SIZE: usize> BlockWrite for FromStd<T, SIZE> {
        fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
            assert!(
                buf.len().is_multiple_of(Self::SIZE),
                "expected the buffer size ({}) to be a multiple of the medium's block size ({})",
                buf.len(),
                Self::SIZE
            );

            self.inner
                .seek(SeekFrom::Start(u64::from(block) * (Self::SIZE as u64)))?;
            self.inner.write_all(buf)?;

            Ok(())
        }

        #[inline]
        fn flush(&mut self) -> Result<(), Self::Error> {
            self.inner.flush()
        }
    }
}

#[cfg(feature = "std")]
pub use from_std::FromStd;

/// TODO
#[derive(Debug)]
pub struct BlockTranslator<'a, const RBS: usize, const VBS: usize, S: BlockWrite> {
    storage: S,
    buffer: &'a mut [u8; RBS],
    stored_sector: SectorIndex,
    status: BlockTranslatorStatus,
}

#[derive(Debug, Eq, PartialEq)]
enum BlockTranslatorStatus {
    Unknown,
    Read,
    Modified,
}

impl<'a, const RBS: usize, const VBS: usize, S: BlockWrite> BlockTranslator<'a, RBS, VBS, S> {
    const fn check() {
        if S::SIZE != RBS {
            panic!("block size mismatch");
        }
        if RBS <= VBS {
            panic!("real block size must be bigger than virtual block size");
        }
        if !RBS.is_power_of_two() || !VBS.is_power_of_two() {
            panic!("real and virtual block size must be a power of two");
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    const VBS_PER_RBS: u16 = (RBS / VBS) as u16;

    /// TODO
    pub fn new(storage: S, buffer: &'a mut [u8; RBS]) -> Self {
        Self::check();

        Self {
            storage,
            buffer,
            stored_sector: 0,
            status: BlockTranslatorStatus::Unknown,
        }
    }

    fn go_to_sector(&mut self, sector_in_vbs: SectorIndex) -> Result<usize, S::Error> {
        let real_sector = sector_in_vbs / SectorIndex::from(Self::VBS_PER_RBS);
        if self.stored_sector != real_sector || self.status == BlockTranslatorStatus::Unknown {
            if self.status == BlockTranslatorStatus::Modified {
                self.storage
                    .write(BlockIndex::from(self.stored_sector), self.buffer)?;
            }
            self.stored_sector = real_sector;
            self.storage
                .read(BlockIndex::from(self.stored_sector), self.buffer)?;
            self.status = BlockTranslatorStatus::Read;
        }
        Ok((sector_in_vbs % SectorIndex::from(Self::VBS_PER_RBS)) as usize)
    }
}

impl<const RBS: usize, const VBS: usize, S> ErrorType for BlockTranslator<'_, RBS, VBS, S>
where
    S: BlockWrite,
{
    type Error = S::Error;
}

impl<const RBS: usize, const VBS: usize, S> BlockRead for BlockTranslator<'_, RBS, VBS, S>
where
    S: BlockWrite,
{
    const SIZE: usize = VBS;

    fn read(&mut self, block: BlockIndex, mut buf: &mut [u8]) -> Result<(), Self::Error> {
        let mut sector_in_vbs = block * BlockCount::from(Self::VBS_PER_RBS);
        while !buf.is_empty() {
            let (this, next) = buf.split_at_mut(VBS);
            let offset = self.go_to_sector(sector_in_vbs as SectorIndex)?; // FIXME
            this.copy_from_slice(&self.buffer[offset..offset + VBS]);
            // advance
            buf = next;
            sector_in_vbs += 1;
        }

        Ok(())
    }
}

impl<const RBS: usize, const VBS: usize, S> BlockWrite for BlockTranslator<'_, RBS, VBS, S>
where
    S: BlockWrite,
{
    fn write(&mut self, block: BlockIndex, mut buf: &[u8]) -> Result<(), Self::Error> {
        let mut sector_in_vbs = block * BlockCount::from(Self::VBS_PER_RBS);
        while !buf.is_empty() {
            let (this, next) = buf.split_at(VBS);
            let offset = self.go_to_sector(sector_in_vbs as SectorIndex)?; // FIXME
            self.buffer[offset..offset + VBS].copy_from_slice(this);
            self.status = BlockTranslatorStatus::Modified;

            // advance
            buf = next;
            sector_in_vbs += 1;
        }

        Ok(())
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        if self.status == BlockTranslatorStatus::Modified {
            self.storage
                .write(BlockIndex::from(self.stored_sector), self.buffer)?;
            self.status = BlockTranslatorStatus::Read;
        }

        self.storage.flush()
    }
}
