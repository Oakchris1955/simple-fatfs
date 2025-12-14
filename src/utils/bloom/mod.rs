// Code obtained from https://github.com/jedisct1/rust-bloom-filter/blob/6b93b922be474998514b696dc84333d6c04ed991/src/lib.rs

// (C)opyleft 2013-2024 Frank Denis
// Licensed under the ICS license (https://opensource.org/licenses/ISC)

#![warn(non_camel_case_types, non_upper_case_globals, unused_qualifications)]
#![forbid(unsafe_code)]
#![expect(clippy::bool_comparison)]

mod bitmap;
use bitmap::*;

use core::cmp;
use core::convert::TryFrom;
use core::f64;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::num;

use siphasher::sip::SipHasher13;

/// Bloom filter structure
#[derive(Clone)]
pub struct Bloom<T: ?Sized> {
    bitmap: BitMap,
    bitmap_bits: u64,
    k_num: u32,
    sips: [SipHasher13; 2],

    _phantom: PhantomData<T>,
}

impl<T: ?Sized> Debug for Bloom<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Bloom filter with {} bits, {} hash functions and seed: {:?} ",
            self.bitmap_bits,
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
    pub fn new(bitmap_size: num::NonZeroUsize, items_count: num::NonZeroUsize) -> Self {
        let bitmap_bits = u64::try_from(bitmap_size.get())
            .unwrap()
            .checked_mul(8u64)
            .unwrap();
        let k_num = Self::optimal_k_num(bitmap_bits, items_count.get());
        let bitmap = BitMap::new(bitmap_size.get());
        let sips = [Self::sip_new(), Self::sip_new()];
        let mut res = Self {
            bitmap,
            bitmap_bits,
            k_num,
            sips,
            _phantom: PhantomData,
        };
        res.sync();
        res
    }

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
        crate::compute_bitmap_size(items_count, fp_p)
    }

    /// Return the number of bits in the filter.
    pub fn len(&self) -> u64 {
        self.bitmap.len_bits()
    }

    /// Record the presence of an item.
    pub fn set(&mut self, item: &T)
    where
        T: Hash,
    {
        let mut hashes = [0u64, 0u64];
        for k_i in 0..self.k_num {
            // TODO: need to check whether this could actually truncate and cause problems
            #[expect(clippy::cast_possible_truncation)]
            let bit_offset = (self.bloom_hash(&mut hashes, item, k_i) % self.bitmap_bits) as usize;
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
        for k_i in 0..self.k_num {
            // TODO: need to check whether this could actually truncate and cause problems
            #[expect(clippy::cast_possible_truncation)]
            let bit_offset = (self.bloom_hash(&mut hashes, item, k_i) % self.bitmap_bits) as usize;
            if self.bitmap.get(bit_offset) == false {
                return false;
            }
        }
        true
    }

    /// Record the presence of an item in the set, and return the previous state of this item.
    pub fn check_and_set(&mut self, item: &T) -> bool
    where
        T: Hash,
    {
        let mut hashes = [0u64, 0u64];
        let mut found = true;
        for k_i in 0..self.k_num {
            // TODO: need to check whether this could actually truncate and cause problems
            #[expect(clippy::cast_possible_truncation)]
            let bit_offset = (self.bloom_hash(&mut hashes, item, k_i) % self.bitmap_bits) as usize;
            if self.bitmap.get(bit_offset) == false {
                found = false;
                self.bitmap.set(bit_offset);
            }
        }
        found
    }

    /// Return the number of hash functions used for `check` and `set`
    pub fn number_of_hash_functions(&self) -> u32 {
        self.k_num
    }

    /// Clear all of the bits in the filter, removing all keys from the set
    pub fn clear(&mut self) {
        self.bitmap.clear()
    }

    /// Set all of the bits in the filter, making it appear like every key is in the set
    pub fn fill(&mut self) {
        self.bitmap.set_all()
    }

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

    fn sync(&mut self) {
        let seed = self.seed();
        let header = self.bitmap.header_mut();
        BitMap::set_k_num(header, self.k_num);
        BitMap::set_seed(header, &seed);
    }

    #[expect(
        clippy::cast_precision_loss,
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation
    )]
    fn optimal_k_num(bitmap_bits: u64, items_count: usize) -> u32 {
        let m = bitmap_bits as f64;
        let n = items_count as f64;
        let k_num = (m / n * f64::ln(2.0f64)).round() as u32;
        cmp::max(k_num, 1)
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
            (hashes[0]).wrapping_add(u64::from(k_i).wrapping_mul(hashes[1]))
                % 0xFFFF_FFFF_FFFF_FFC5u64 //largest u64 prime
        }
    }
}
