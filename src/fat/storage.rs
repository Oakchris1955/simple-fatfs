use core::ops::{Deref, DerefMut};

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

#[derive(Debug)]
pub(crate) struct SectorBuffer {
    pub(crate) slice: Box<[u8]>,
    pub(crate) stored_sector: u64,
}

impl From<(&[u8], u64)> for SectorBuffer {
    fn from(value: (&[u8], u64)) -> Self {
        Self {
            slice: Box::from(value.0),
            stored_sector: value.1,
        }
    }
}

impl Deref for SectorBuffer {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.slice
    }
}

impl DerefMut for SectorBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.slice
    }
}
