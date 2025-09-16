use super::*;

use core::ops::{Deref, DerefMut};

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

#[derive(Debug)]
pub(crate) struct SectorBuffer {
    slice: Box<[u8]>,
    pub(crate) stored_sector: SectorIndex,
}

impl SectorBuffer {
    pub(crate) fn new(sector_buffer: Box<[u8]>, stored_sector: SectorIndex) -> Self {
        Self {
            slice: sector_buffer,
            stored_sector,
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
