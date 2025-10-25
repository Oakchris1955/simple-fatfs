use super::*;

use embedded_io::ErrorType;

/// how to read blocks
pub trait BlockRead: ErrorType {
    /// size of a block, must be a power of two
    const SIZE: usize;

    /// read a block (or multiple)
    fn read(&mut self, sector: SectorIndex, count: u16, buf: &mut [u8]) -> Result<(), Self::Error>;
}

/// how to write blocks
pub trait BlockWrite: BlockRead {
    /// write a block (or multiple)
    fn write(&mut self, sector: SectorIndex, count: u16, buf: &[u8]) -> Result<(), Self::Error>;

    /// flush
    fn flush(&mut self) -> Result<(), Self::Error>;
}

impl<T: BlockRead> BlockRead for &mut T {
    const SIZE: usize = T::SIZE;

    #[inline]
    fn read(&mut self, sector: SectorIndex, count: u16, buf: &mut [u8]) -> Result<(), Self::Error> {
        T::read(self, sector, count, buf)
    }
}
impl<T: BlockWrite> BlockWrite for &mut T {
    #[inline]
    fn write(&mut self, sector: SectorIndex, count: u16, buf: &[u8]) -> Result<(), Self::Error> {
        T::write(self, sector, count, buf)
    }

    #[inline]
    fn flush(&mut self) -> Result<(), Self::Error> {
        T::flush(self)
    }
}

#[cfg(feature = "std")]
pub(crate) mod from_std {
    use crate::{BlockRead, BlockWrite, SectorIndex, MIN_SECTOR_SIZE};
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
        const SIZE: usize = MIN_SECTOR_SIZE;

        fn read(
            &mut self,
            sector: SectorIndex,
            count: u16,
            mut buf: &mut [u8],
        ) -> Result<(), Self::Error> {
            let bytes_to_read = Self::SIZE * usize::from(count);

            assert!(
                buf.len() >= bytes_to_read,
                "expected buf to fit at least {} bytes, actual length is {} bytes",
                bytes_to_read,
                buf.len()
            );

            buf = &mut buf[..Self::SIZE * usize::from(count)];

            self.inner
                .seek(SeekFrom::Start(u64::from(sector) * (Self::SIZE as u64)))?;

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
            count: u16,
            mut buf: &[u8],
        ) -> Result<(), Self::Error> {
            let bytes_to_read = Self::SIZE * usize::from(count);

            assert!(
                buf.len() >= bytes_to_read,
                "expected buf to fit at least {} bytes, actual length is {} bytes",
                bytes_to_read,
                buf.len()
            );

            buf = &buf[..Self::SIZE * usize::from(count)];

            self.inner
                .seek(SeekFrom::Start(u64::from(sector) * (Self::SIZE as u64)))?;
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
    stored_sector: u32,
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
        let _check: () = Self::check();

        Self {
            storage,
            buffer,
            stored_sector: 0,
            status: BlockTranslatorStatus::Unknown,
        }
    }

    fn go_to_sector(&mut self, sector_in_vbs: u32) -> Result<usize, S::Error> {
        let real_sector = sector_in_vbs / u32::from(Self::VBS_PER_RBS);
        if self.stored_sector != real_sector || self.status == BlockTranslatorStatus::Unknown {
            if self.status == BlockTranslatorStatus::Modified {
                self.storage.write(self.stored_sector, 1, self.buffer)?;
            }
            self.stored_sector = real_sector;
            self.storage.read(self.stored_sector, 1, self.buffer)?;
            self.status = BlockTranslatorStatus::Read;
        }
        Ok((sector_in_vbs % u32::from(Self::VBS_PER_RBS)) as usize)
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

    fn read(
        &mut self,
        sector: SectorIndex,
        count: u16,
        mut buf: &mut [u8],
    ) -> Result<(), Self::Error> {
        let mut sector_in_vbs = sector * u32::from(count);
        while !buf.is_empty() {
            let (this, next) = buf.split_at_mut(VBS);
            let offset = self.go_to_sector(sector_in_vbs)?;
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
    fn write(
        &mut self,
        sector: SectorIndex,
        count: u16,
        mut buf: &[u8],
    ) -> Result<(), Self::Error> {
        let mut sector_in_vbs = sector * u32::from(count);
        while !buf.is_empty() {
            let (this, next) = buf.split_at(VBS);
            let offset = self.go_to_sector(sector_in_vbs)?;
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
            self.storage.write(self.stored_sector, 1, self.buffer)?;
            self.status = BlockTranslatorStatus::Read;
        }

        self.storage.flush()
    }
}
