//! Block device-related IO traits and adapters
//!
//! Low-level storage devices, like flash memories, operate in units of data
//! which will hereby after be referred as blocks. This library assumes that
//! both for read and write operations the block size will be the same.

pub use crate::fat::block_translator::{BlockTranslator, BlockTranslatorError};
pub use crate::fat::types::{BlockCount, BlockIndex, BlockSize};

use embedded_io::ErrorType;

/// The base trait for all block devices. Used to query information like
/// block size and block count
pub trait BlockBase: ErrorType {
    /// Size of a block, must be a power of two. A panic may occur if this isn't
    /// a power of two other than zero.
    fn block_size(&self) -> BlockSize;

    /// Retrieve the number of available blocks in the storage medium.
    fn block_count(&self) -> BlockCount;
}

/// The `BlockRead` traits allows to read data from a source in units of blocks.
pub trait BlockRead: BlockBase {
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

impl<T: BlockBase> BlockBase for &mut T {
    fn block_size(&self) -> BlockSize {
        T::block_size(self)
    }

    fn block_count(&self) -> BlockCount {
        T::block_count(self)
    }
}

impl<T: BlockRead> BlockRead for &mut T {
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
    use crate::{
        BlockBase, BlockCount, BlockIndex, BlockRead, BlockSize, BlockWrite, MIN_SECTOR_SIZE,
    };
    use std::io::{Error, Read, Seek, SeekFrom, Write};

    /// Determine the block count of a storage medium
    ///
    /// This function may fail (return [`None`]) if the underlying [`seek`](std::io::Seek)
    /// operation fails or if the storage medium's size isn't a multiple of `T::SIZE`
    fn determine_block_count<T: ?Sized + Seek>(
        block_size: BlockSize,
        storage: &mut T,
    ) -> Option<BlockCount> {
        let offset = storage.seek(SeekFrom::End(0)).ok()?;

        if !offset.is_multiple_of(u64::from(block_size)) {
            return None;
        }

        let count = offset / u64::from(block_size);

        BlockCount::try_from(count).ok()
    }

    /// The default block size for the [`FromStd`] adapter
    #[expect(clippy::cast_possible_truncation)]
    pub const DEFAULT_BLOCK_SIZE: BlockSize = MIN_SECTOR_SIZE as BlockSize;

    /// Adapter from [`std::io`] traits.
    #[derive(Clone, Debug)]
    pub struct FromStd<T: ?Sized> {
        block_count: BlockCount,
        block_size: BlockSize,
        inner: T,
    }

    impl<T: Seek> FromStd<T> {
        /// Create a new adapter with the [default block size](DEFAULT_BLOCK_SIZE).
        pub fn new(mut inner: T) -> Option<Self> {
            let block_count = determine_block_count(DEFAULT_BLOCK_SIZE, &mut inner)?;

            Some(Self {
                inner,
                block_count,
                block_size: DEFAULT_BLOCK_SIZE,
            })
        }

        /// Create a new adapter with a custom block size.
        pub fn with_block_size(mut inner: T, block_size: BlockSize) -> Option<Self> {
            let block_count = determine_block_count(block_size, &mut inner)?;

            Some(Self {
                inner,
                block_count,
                block_size,
            })
        }
    }

    impl<T> FromStd<T> {
        /// Consume the adapter, returning the inner object.
        pub fn into_inner(self) -> T {
            self.inner
        }
    }

    impl<T: ?Sized> FromStd<T> {
        /// Borrow the inner object.
        pub fn inner(&self) -> &T {
            &self.inner
        }

        /// Mutably borrow the inner object.
        pub fn inner_mut(&mut self) -> &mut T {
            &mut self.inner
        }
    }

    impl<T: ?Sized> embedded_io::ErrorType for FromStd<T> {
        type Error = Error;
    }

    impl<T: ?Sized> BlockBase for FromStd<T> {
        fn block_size(&self) -> BlockSize {
            self.block_size
        }

        fn block_count(&self) -> BlockCount {
            self.block_count
        }
    }

    impl<T: Read + Seek + ?Sized> BlockRead for FromStd<T> {
        fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
            assert!(
                buf.len()
                    .is_multiple_of(self.block_size.try_into().unwrap()),
                "expected the buffer size ({}) to be a multiple of the medium's block size ({})",
                buf.len(),
                self.block_size
            );

            #[cfg_attr(feature = "lba64", expect(clippy::useless_conversion))]
            // silence warning on u64->u64 conversion with feature `lba64` (it's u32->u64 without the feature)
            self.inner.seek(SeekFrom::Start(
                u64::from(block) * u64::from(self.block_size),
            ))?;

            self.inner.read_exact(buf)?;

            Ok(())
        }
    }

    impl<T: Read + Write + Seek + ?Sized> BlockWrite for FromStd<T> {
        fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
            assert!(
                buf.len()
                    .is_multiple_of(self.block_size.try_into().unwrap()),
                "expected the buffer size ({}) to be a multiple of the medium's block size ({})",
                buf.len(),
                self.block_size
            );

            #[cfg_attr(feature = "lba64", expect(clippy::useless_conversion))]
            // silence warning on u64->u64 conversion with feature `lba64` (it's u32->u64 without the feature)
            self.inner.seek(SeekFrom::Start(
                u64::from(block) * u64::from(self.block_size),
            ))?;
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
pub use from_std::*;

#[cfg(feature = "embedded_storage_translator")]
pub(crate) mod embedded_storage_translators {
    use super::*;
    use core::ops::{Deref, DerefMut};

    use embedded_storage::nor_flash::{NorFlash, ReadNorFlash};

    #[inline]
    fn calc_offset(block: BlockIndex, block_size: u32) -> u32 {
        u32::try_from(block).unwrap() * block_size
    }

    /// Adapter from [`ReadNorFlash`]
    #[derive(Debug)]
    pub struct ReadNorFlashTranslator<S>
    where
        S: ReadNorFlash,
        S::Error: embedded_io::Error,
    {
        storage: S,
    }

    impl<S> ReadNorFlashTranslator<S>
    where
        S: ReadNorFlash,
        S::Error: embedded_io::Error,
    {
        /// Create a new [`ReadNorFlashTranslator`] from a flash storage that
        /// implements [`ReadNorFlash`] and [`embedded_io::Error`]
        pub fn new(storage: S) -> Self {
            Self { storage }
        }
    }

    impl<S> Deref for ReadNorFlashTranslator<S>
    where
        S: ReadNorFlash,
        S::Error: embedded_io::Error,
    {
        type Target = S;

        #[inline]
        fn deref(&self) -> &Self::Target {
            &self.storage
        }
    }

    impl<S> DerefMut for ReadNorFlashTranslator<S>
    where
        S: ReadNorFlash,
        S::Error: embedded_io::Error,
    {
        #[inline]
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.storage
        }
    }

    impl<S> ErrorType for ReadNorFlashTranslator<S>
    where
        S: ReadNorFlash,
        S::Error: embedded_io::Error,
    {
        type Error = S::Error;
    }

    impl<S> BlockBase for ReadNorFlashTranslator<S>
    where
        S: ReadNorFlash,
        S::Error: embedded_io::Error,
    {
        #[inline]
        fn block_size(&self) -> BlockSize {
            S::READ_SIZE.try_into().unwrap()
        }

        fn block_count(&self) -> BlockCount {
            (self.storage.capacity() / S::READ_SIZE).try_into().unwrap()
        }
    }

    impl<S> BlockRead for ReadNorFlashTranslator<S>
    where
        S: ReadNorFlash,
        S::Error: embedded_io::Error,
    {
        fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
            self.storage
                .read(calc_offset(block, S::READ_SIZE.try_into().unwrap()), buf)
        }
    }

    /// Adapter from [`NorFlash`]
    ///
    /// It is assumed that [`S::ERASE_SIZE`] is a multiple
    /// of both [`S::READ_SIZE`] and [`S::WRITE_SIZE`] and that
    /// it is bigger than both of them.
    ///
    /// Each block with be [`S::ERASE_SIZE`] bytes
    ///
    /// [`S::READ_SIZE`]: ReadNorFlash::READ_SIZE
    /// [`S::WRITE_SIZE`]: NorFlash::WRITE_SIZE
    /// [`S::ERASE_SIZE`]: NorFlash::ERASE_SIZE
    #[derive(Debug)]
    pub struct MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        read_translator: ReadNorFlashTranslator<S>,
    }

    impl<S> MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        /// Create a new [`MultiWriteNorFlashTranslator`] from a flash storage that
        /// implements [`NorFlash`] and [`embedded_io::Error`]
        pub fn new(storage: S) -> Self {
            Self {
                read_translator: ReadNorFlashTranslator::new(storage),
            }
        }
    }

    impl<S> Deref for MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        type Target = ReadNorFlashTranslator<S>;

        #[inline]
        fn deref(&self) -> &Self::Target {
            &self.read_translator
        }
    }

    impl<S> DerefMut for MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        #[inline]
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.read_translator
        }
    }

    impl<S> ErrorType for MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        type Error = S::Error;
    }

    impl<S> BlockBase for MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        #[inline]
        fn block_size(&self) -> BlockSize {
            S::ERASE_SIZE.try_into().unwrap()
        }

        fn block_count(&self) -> BlockCount {
            (self.storage.capacity() / S::ERASE_SIZE)
                .try_into()
                .unwrap()
        }
    }

    impl<S> BlockRead for MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        #[inline]
        fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
            self.read_translator.read(block, buf)
        }
    }

    impl<S> BlockWrite for MultiWriteNorFlashTranslator<S>
    where
        S: NorFlash,
        S::Error: embedded_io::Error,
    {
        fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
            let from = calc_offset(block, S::ERASE_SIZE.try_into().unwrap());

            self.storage
                .erase(from, from + u32::try_from(buf.len()).unwrap())?;
            self.storage.write(from, buf)?;

            Ok(())
        }

        fn flush(&mut self) -> Result<(), Self::Error> {
            // everything is immediately flushed
            Ok(())
        }
    }
}

#[cfg(feature = "embedded_storage_translator")]
pub use embedded_storage_translators::*;
