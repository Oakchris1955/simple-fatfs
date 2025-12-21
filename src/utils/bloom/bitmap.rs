use core::fmt::Debug;
use core::num;

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

#[derive(Clone, Debug)]
pub(crate) struct BitMap {
    bits: Box<[u8]>,
}

impl BitMap {
    pub fn new(len_bytes: num::NonZeroUsize) -> Self {
        let bits = vec![0; len_bytes.get()].into_boxed_slice();

        Self { bits }
    }

    #[inline]
    fn bits(&self) -> &[u8] {
        &self.bits
    }

    #[inline]
    fn bits_mut(&mut self) -> &mut [u8] {
        &mut self.bits
    }

    pub fn get(&self, bit_offset: usize) -> bool {
        let byte_offset = bit_offset / 8;
        let bit_shift = bit_offset % 8;
        (self.bits()[byte_offset] & (1 << bit_shift)) != 0
    }

    pub fn set(&mut self, bit_offset: usize) {
        let byte_offset = bit_offset / 8;
        let bit_shift = bit_offset % 8;
        self.bits_mut()[byte_offset] |= 1 << bit_shift;
    }

    pub fn clear(&mut self) {
        for byte in self.bits_mut().iter_mut() {
            *byte = 0;
        }
    }

    #[expect(unused)]
    pub fn set_all(&mut self) {
        for byte in self.bits_mut().iter_mut() {
            *byte = !0;
        }
    }

    pub fn any(&self) -> bool {
        self.bits().iter().any(|&byte| byte != 0)
    }

    pub fn len_bits(&self) -> num::NonZeroU64 {
        num::NonZeroU64::new(
            u64::try_from(self.bits().len())
                .unwrap()
                .checked_mul(8)
                .unwrap(),
        )
        .unwrap()
    }
}
