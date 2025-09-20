use super::*;

use embedded_io::ErrorType;

/// how to read blocks
pub trait BlockRead: ErrorType {
    /// size of a block, must be a power of two
    const SIZE: usize;

    /// read a block (or multiple)
    fn read(
        &mut self,
        sector: SectorIndex,
        blocks_per_sector: u16,
        buf: &mut [u8],
    ) -> Result<(), Self::Error>;
}

/// how to write blocks
pub trait BlockWrite: BlockRead {
    /// write a block (or multiple)
    fn write(
        &mut self,
        sector: SectorIndex,
        blocks_per_sector: u16,
        buf: &[u8],
    ) -> Result<(), Self::Error>;

    /// flush
    fn flush(&mut self) -> Result<(), Self::Error>;
}

impl<T: BlockRead> BlockRead for &mut T {
    const SIZE: usize = T::SIZE;

    #[inline]
    fn read(
        &mut self,
        sector: SectorIndex,
        blocks_per_sector: u16,
        buf: &mut [u8],
    ) -> Result<(), Self::Error> {
        T::read(self, sector, blocks_per_sector, buf)
    }
}
impl<T: BlockWrite> BlockWrite for &mut T {
    #[inline]
    fn write(
        &mut self,
        sector: SectorIndex,
        blocks_per_sector: u16,
        buf: &[u8],
    ) -> Result<(), Self::Error> {
        T::write(self, sector, blocks_per_sector, buf)
    }

    #[inline]
    fn flush(&mut self) -> Result<(), Self::Error> {
        T::flush(self)
    }
}

#[cfg(feature = "std")]
pub(crate) mod from_std {
    use crate::{BlockRead, BlockWrite, SectorIndex};
    use std::io::{Error, ErrorKind, Read, Seek, SeekFrom, Write};

    /// Adapter from `std::io` traits.
    #[derive(Clone, Debug)]
    pub struct FromStd<T: ?Sized> {
        inner: T,
    }

    impl<T> FromStd<T> {
        /// Create a new adapter.
        pub fn new(inner: T) -> Self {
            Self { inner }
        }

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

    impl<T: Read + Seek + ?Sized> BlockRead for FromStd<T> {
        const SIZE: usize = 512;

        fn read(
            &mut self,
            sector: SectorIndex,
            blocks_per_sector: u16,
            mut buf: &mut [u8],
        ) -> Result<(), Self::Error> {
            self.inner.seek(SeekFrom::Start(
                u64::from(sector) * u64::from(blocks_per_sector) * (Self::SIZE as u64),
            ))?;
            while !buf.is_empty() {
                let n = self.inner.read(buf)?;
                if n == 0 {
                    return Err(Error::new(
                        ErrorKind::UnexpectedEof,
                        "failed to fill whole buffer",
                    ));
                }
                buf = &mut buf[n..];
            }
            Ok(())
        }
    }

    impl<T: Read + Write + Seek + ?Sized> BlockWrite for FromStd<T> {
        fn write(
            &mut self,
            sector: SectorIndex,
            blocks_per_sector: u16,
            buf: &[u8],
        ) -> Result<(), Self::Error> {
            self.inner.seek(SeekFrom::Start(
                u64::from(sector) * u64::from(blocks_per_sector) * (Self::SIZE as u64),
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
pub use from_std::FromStd;
