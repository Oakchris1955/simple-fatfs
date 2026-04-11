use core::ops::{Deref, DerefMut};
use embedded_io::ErrorType;
#[cfg(not(test))]
use simple_fatfs::block_io::*;

#[cfg(test)]
#[cfg_attr(test, allow(unused))]
use crate::block_io::*;

#[cfg(not(feature = "std"))]
extern crate alloc;
#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

pub struct MemoryDevice<A>(A)
where
    A: Deref<Target = [u8]>;

impl From<&[u8]> for MemoryDevice<Box<[u8]>> {
    fn from(value: &[u8]) -> Self {
        Self(Box::from(value))
    }
}

#[derive(Debug)]
pub struct WrapperError;

impl embedded_io::Error for WrapperError {
    fn kind(&self) -> embedded_io::ErrorKind {
        unreachable!()
    }
}

impl<A> ErrorType for MemoryDevice<A>
where
    A: Deref<Target = [u8]>,
{
    type Error = WrapperError;
}

impl<A> BlockBase for MemoryDevice<A>
where
    A: Deref<Target = [u8]>,
{
    fn block_size(&self) -> BlockSize {
        1
    }

    fn block_count(&self) -> BlockCount {
        self.0.len() as BlockCount
    }
}

impl<A> BlockRead for MemoryDevice<A>
where
    A: Deref<Target = [u8]>,
{
    fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
        // this is fine for testing
        #![expect(clippy::cast_possible_truncation)]
        let start = block as usize;
        let end = start + buf.len();

        buf.copy_from_slice(&self.0[start..end]);

        Ok(())
    }
}

impl<A> BlockWrite for MemoryDevice<A>
where
    A: Deref<Target = [u8]> + DerefMut,
{
    fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
        // this is fine for testing
        #![expect(clippy::cast_possible_truncation)]
        let start = block as usize;
        let end = start + buf.len();

        self.0[start..end].copy_from_slice(buf);

        Ok(())
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}
