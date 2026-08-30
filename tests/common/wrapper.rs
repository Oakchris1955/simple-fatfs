use core::error::Error;
use core::ops::{Deref, DerefMut};

use displaydoc::Display;
use embedded_io::{Error as IOError, ErrorKind as IOErrorKind, ErrorType as IOErrorType};

use crate::block_io::prelude::*;

#[derive(Debug)]
pub struct MemoryDevice(Box<[u8]>);

impl From<&[u8]> for MemoryDevice {
    fn from(value: &[u8]) -> Self {
        Self(Box::from(value))
    }
}

/// Device wrapper error
#[derive(Display, Debug, PartialEq, Eq)]
pub struct WrapperError;

impl Error for WrapperError {}

impl IOError for WrapperError {
    fn kind(&self) -> IOErrorKind {
        unreachable!()
    }
}

impl IOErrorType for MemoryDevice {
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
