// Code obtained from https://github.com/jedisct1/rust-bloom-filter/blob/6b93b922be474998514b696dc84333d6c04ed991/src/lib.rs

// (C)opyleft 2013-2024 Frank Denis
// Licensed under the ICS license (https://opensource.org/licenses/ISC)

mod bitmap;
use bitmap::*;

use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::num;

use siphasher::sip::SipHasher13;

const LARGEST_U64_PRIME: u64 = 0xFFFF_FFFF_FFFF_FFC5u64;

/// Bloom filter structure
#[derive(Clone)]
pub struct Bloom<T: ?Sized> {
    bitmap: BitMap,
    k_num: num::NonZeroU32,
    sips: [SipHasher13; 2],

    _phantom: PhantomData<T>,
}

impl<T: ?Sized> Debug for Bloom<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Bloom filter with {} bits, {} hash functions and seed: {:?} ",
            self.bitmap.len_bits(),
            self.k_num,
            self.seed()
        )
    }
}

impl<T: ?Sized> Bloom<T> {
    /// Create a new bloom filter structure.
    /// bitmap_size is the size in bytes (not bits) that will be allocated in
    /// memory items_count is an estimation of the maximum number of items
    /// to store.
    ///
    /// Will panic if `bitmap_size` is bigger than 2^61
    pub fn new(bitmap_size: num::NonZeroUsize, items_count: num::NonZeroUsize) -> Self {
        let bitmap_bits = num::NonZeroU64::try_from(bitmap_size)
            .expect("There's no reason to make a bloom filter more than (2^64)-1 bytes")
            .checked_mul(num::NonZeroU64::new(8).unwrap())
            .expect("There's no reason to make a bloom filter more than 2^61 bytes");
        let k_num = Self::optimal_k_num(bitmap_bits, items_count);
        let bitmap = BitMap::new(bitmap_size);
        let sips = [Self::sip_new(), Self::sip_new()];

        Self {
            bitmap,
            k_num,
            sips,
            _phantom: PhantomData,
        }
    }

    #[expect(unused)]
    /// Create a new bloom filter structure.
    /// items_count is an estimation of the maximum number of items to store.
    /// fp_p is the wanted rate of false positives, in ]0.0, 1.0[
    pub fn new_for_fp_rate(items_count: num::NonZeroUsize, fp_p: f64) -> Self {
        let bitmap_size = Self::compute_bitmap_size(items_count, fp_p);
        Bloom::new(bitmap_size, items_count)
    }

    /// Compute a recommended bitmap size for items_count items
    /// and a fp_p rate of false positives.
    /// fp_p obviously has to be within the ]0.0, 1.0[ range.
    #[inline]
    pub fn compute_bitmap_size(items_count: num::NonZeroUsize, fp_p: f64) -> num::NonZeroUsize {
        crate::bloom::compute_bitmap_size(items_count, fp_p)
    }

    #[expect(unused)]
    /// Return the number of bits in the filter.
    pub fn len(&self) -> num::NonZeroU64 {
        self.bitmap.len_bits()
    }

    /// Record the presence of an item.
    pub fn set(&mut self, item: &T)
    where
        T: Hash,
    {
        let mut hashes = [0u64, 0u64];
        for k_i in 0..self.k_num.get() {
            // TODO: need to check whether this could actually truncate and cause problems
            #[expect(clippy::cast_possible_truncation)]
            let bit_offset =
                (self.bloom_hash(&mut hashes, item, k_i) % self.bitmap.len_bits()) as usize;
            self.bitmap.set(bit_offset);
        }
    }

    /// Check if an item is present in the set.
    /// There can be false positives, but no false negatives.
    pub fn check(&self, item: &T) -> bool
    where
        T: Hash,
    {
        let mut hashes = [0u64, 0u64];
        for k_i in 0..self.k_num.get() {
            // TODO: need to check whether this could actually truncate and cause problems
            #[expect(clippy::cast_possible_truncation)]
            let bit_offset =
                (self.bloom_hash(&mut hashes, item, k_i) % self.bitmap.len_bits()) as usize;
            if !self.bitmap.get(bit_offset) {
                return false;
            }
        }
        true
    }

    #[expect(unused)]
    /// Record the presence of an item in the set, and return the previous state of this item.
    pub fn check_and_set(&mut self, item: &T) -> bool
    where
        T: Hash,
    {
        let mut hashes = [0u64, 0u64];
        let mut found = true;
        for k_i in 0..self.k_num.get() {
            // TODO: need to check whether this could actually truncate and cause problems
            #[expect(clippy::cast_possible_truncation)]
            let bit_offset =
                (self.bloom_hash(&mut hashes, item, k_i) % self.bitmap.len_bits()) as usize;
            if !self.bitmap.get(bit_offset) {
                found = false;
                self.bitmap.set(bit_offset);
            }
        }
        found
    }

    #[expect(unused)]
    /// Clear all of the bits in the filter, removing all keys from the set
    pub fn clear(&mut self) {
        self.bitmap.clear()
    }

    #[expect(unused)]
    /// Test if there are no elements in the set
    pub fn is_empty(&self) -> bool {
        !self.bitmap.any()
    }

    /// Return the seed used to generate the hash functions
    pub fn seed(&self) -> [u8; 32] {
        let mut seed = [0u8; 32];
        seed[0..16].copy_from_slice(&self.sips[0].key());
        seed[16..32].copy_from_slice(&self.sips[1].key());
        seed
    }

    #[inline]
    fn sip_new() -> SipHasher13 {
        SipHasher13::new()
    }

    #[inline]
    fn optimal_k_num(
        bitmap_size: num::NonZeroU64,
        items_count: num::NonZeroUsize,
    ) -> num::NonZeroU32 {
        crate::bloom::compute_hash_count(bitmap_size, items_count)
    }

    fn bloom_hash(&self, hashes: &mut [u64; 2], item: &T, k_i: u32) -> u64
    where
        T: Hash,
    {
        if k_i < 2 {
            let sip = &mut self.sips[k_i as usize].clone();
            item.hash(sip);
            let hash = sip.finish();
            hashes[k_i as usize] = hash;
            hash
        } else {
            (hashes[0]).wrapping_add(u64::from(k_i).wrapping_mul(hashes[1])) % LARGEST_U64_PRIME
        }
    }
}
