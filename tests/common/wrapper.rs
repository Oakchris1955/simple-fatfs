use core::ops::{Deref, DerefMut};
use embedded_io::ErrorType;

#[derive(Debug)]
pub struct MemoryDevice(Box<[u8]>);

impl From<&[u8]> for MemoryDevice {
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

impl ErrorType for MemoryDevice {
    type Error = WrapperError;
}

impl BlockBase for MemoryDevice {
    fn block_size(&self) -> BlockSize {
        1
    }

    fn block_count(&self) -> BlockCount {
        // this is fine for testing
        #[allow(clippy::cast_possible_truncation)]
        {
            self.0.len() as BlockCount
        }
    }
}

impl BlockRead for MemoryDevice {
    fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
        // this is fine for testing
        #![allow(clippy::cast_possible_truncation)]
        let start = block as usize;
        let end = start + buf.len();

        buf.copy_from_slice(&self.0[start..end]);

        Ok(())
    }
}

impl BlockWrite for MemoryDevice {
    fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
        // this is fine for testing
        #![allow(clippy::cast_possible_truncation)]
        let start = block as usize;
        let end = start + buf.len();

        self.0[start..end].copy_from_slice(buf);

        Ok(())
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}
