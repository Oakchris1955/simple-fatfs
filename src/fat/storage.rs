use super::*;

use core::ops::{Deref, DerefMut};

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
use core::cell::RefCell;

#[derive(Debug)]
pub(crate) struct SectorBuffer<const INIT: bool> {
    slice: Box<[u8]>,
    stored_sector: SectorIndex,
    blocks_per_sector: u16,
}

impl SectorBuffer<false> {
    /// Create a new buffer and fill it with the first 512 bytes
    pub(crate) fn new<S: BlockRead>(storage: &mut S) -> Result<Self, S::Error> {
        let mut slf = Self {
            slice: [0u8; MAX_SECTOR_SIZE].into(),
            stored_sector: 0,
            blocks_per_sector: 1,
        };

        storage.read(0, &mut slf[0..MIN_SECTOR_SIZE.max(S::SIZE)])?;

        Ok(slf)
    }

    /// Initialize the buffer with the correct sector size, read sector 0
    pub(crate) fn init<S: BlockRead>(
        self,
        storage: &mut S,
        sector_size: u16,
    ) -> Result<SectorBuffer<true>, S::Error> {
        let mut slf = SectorBuffer {
            slice: self.slice[..usize::from(sector_size)].into(),
            stored_sector: 0,
            #[allow(clippy::cast_possible_truncation)]
            // Safety: S::SIZE is guaranteed to be <= 4096
            blocks_per_sector: sector_size / (S::SIZE as u16),
        };

        if usize::from(sector_size) > S::SIZE {
            storage.read(0, &mut slf)?;
        }

        Ok(slf)
    }
}

impl SectorBuffer<true> {
    pub(crate) fn stored_sector(&self) -> SectorIndex {
        self.stored_sector
    }

    pub(crate) fn read<S: BlockRead>(
        &mut self,
        storage: &RefCell<S>,
        sector: SectorIndex,
    ) -> Result<(), S::Error> {
        if self.stored_sector != sector {
            storage
                .borrow_mut()
                .read(sector * u32::from(self.blocks_per_sector), &mut self.slice)?;
            self.stored_sector = sector;
        }

        Ok(())
    }

    pub(crate) fn read_into<S: BlockRead>(
        &self,
        storage: &RefCell<S>,
        sector: SectorIndex,
        buf: &mut [u8],
    ) -> Result<(), S::Error> {
        assert_eq!(
            buf.len() & (usize::from(self.blocks_per_sector) * S::SIZE - 1),
            0
        );
        storage
            .borrow_mut()
            .read(sector * u32::from(self.blocks_per_sector), buf)?;

        Ok(())
    }

    pub(crate) fn write<S: BlockWrite>(&self, storage: &RefCell<S>) -> Result<(), S::Error> {
        storage.borrow_mut().write(
            self.stored_sector * u32::from(self.blocks_per_sector),
            &self.slice,
        )
    }

    pub(crate) fn write_copy<S: BlockWrite>(
        &self,
        storage: &RefCell<S>,
        sector: SectorIndex,
    ) -> Result<(), S::Error> {
        storage
            .borrow_mut()
            .write(sector * u32::from(self.blocks_per_sector), &self.slice)
    }
}

impl<const INIT: bool> Deref for SectorBuffer<INIT> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.slice
    }
}

impl<const INIT: bool> DerefMut for SectorBuffer<INIT> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.slice
    }
}
