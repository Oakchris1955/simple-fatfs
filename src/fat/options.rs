#[cfg(feature = "bloom")]
use core::num;

use crate::{Clock, Codepage, DefaultClock};

#[derive(Debug)]
/// FileSystem mount options
pub struct FSOptions<C: Clock> {
    pub(crate) clock: C,
    pub(crate) codepage: Codepage,
    pub(crate) update_file_fields: bool,
    pub(crate) check_boot_signature: bool,
    #[cfg(feature = "bloom")]
    pub(crate) filter_size: core::num::NonZeroUsize,
}

impl FSOptions<DefaultClock> {
    #[inline]
    /// Create a new options struct with the default options
    ///
    /// This is just an alias to [`Self::default`]
    pub fn new() -> Self {
        Self::default()
    }
}

impl<C> FSOptions<C>
where
    C: Clock,
{
    #[inline]
    /// Create a new options struct with the default options
    /// and a provided clock
    pub fn new_with_clock(clock: C) -> Self {
        Self {
            clock,
            codepage: Codepage::default(),
            update_file_fields: false,
            check_boot_signature: true,
            #[cfg(feature = "bloom")]
            filter_size: bloom::compute_bitmap_size(num::NonZero::new(1_000).unwrap(), 0.01),
        }
    }
}

impl<C: Clock> FSOptions<C> {
    /// Set the codepage to be used by the filesystem
    pub fn set_codepage(&mut self, codepage: Codepage) {
        self.codepage = codepage
    }

    /// Set the codepage to be used by the filesystem (chainable)
    pub fn with_codepage(mut self, codepage: Codepage) -> Self {
        self.set_codepage(codepage);

        self
    }

    /// Whether to update the last accessed/modified file fields
    pub fn set_update_file_fields(&mut self, update: bool) {
        self.update_file_fields = update
    }

    /// Whether to update the last accessed/modified file fields (chainable)
    pub fn with_update_file_fields(mut self, update: bool) -> Self {
        self.update_file_fields = update;

        self
    }

    /// Whether to check for the `[0x55, 0xAA]` boot signature
    pub fn set_boot_signature_check(&mut self, check: bool) {
        self.check_boot_signature = check
    }

    /// Whether to check for the `[0x55, 0xAA]` boot signature (chainable)
    pub fn with_boot_signature_check(mut self, check: bool) -> Self {
        self.check_boot_signature = check;

        self
    }

    #[cfg(feature = "bloom")]
    /// Set the bloom filter size to be that many `bits` long
    ///
    /// # Note
    /// The internal bloom filter might panic if `bitmap_size` is bigger than 2^61
    pub fn set_filter_size(&mut self, bits: num::NonZeroUsize) {
        self.filter_size = bits
    }

    #[cfg(feature = "bloom")]
    /// Set the bloom filter size to be that many `bits` long (chainable)
    ///
    /// # Note
    /// The internal bloom filter might panic if `bitmap_size` is bigger than 2^61
    pub fn with_filter_size(mut self, bits: num::NonZeroUsize) -> Self {
        self.filter_size = bits;

        self
    }

    #[cfg(feature = "bloom")]
    /// Query the directory cache / Bloom filter's size in bytes
    pub fn query_filter_size(&self) -> num::NonZeroUsize {
        self.filter_size
    }
}

// taken from utils::bloom::Bloom
/// Contains bloom filter-related functions
/// (mainly to compute the desired filter size from certain parameters)
#[cfg(feature = "bloom")]
pub mod bloom {
    use core::f64::consts::LN_2;
    use core::{cmp, num};

    use crate::BloomFloat;

    type FloatMethod = fn(BloomFloat) -> BloomFloat;

    const LN2_2: BloomFloat = LN_2 * LN_2;

    /// Compute a recommended bitmap size for items_count items
    /// and a fp_p rate of false positives.
    /// fp_p obviously has to be within the ]0.0, 1.0[ range
    /// or this will panic
    #[expect(clippy::cast_precision_loss)]
    #[inline]
    pub fn compute_bitmap_size(
        items_count: num::NonZeroUsize,
        fp_p: BloomFloat,
    ) -> num::NonZeroUsize {
        assert!(fp_p > 0.0 && fp_p < 1.0);

        #[cfg(feature = "std")]
        const LN: FloatMethod = BloomFloat::ln;
        #[cfg(not(feature = "std"))]
        const LN: FloatMethod = libm::Libm::<BloomFloat>::log;

        #[cfg(feature = "std")]
        const CEIL: FloatMethod = BloomFloat::ceil;
        #[cfg(not(feature = "std"))]
        const CEIL: FloatMethod = libm::Libm::<BloomFloat>::ceil;

        #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        num::NonZero::new(
            CEIL((items_count.get() as BloomFloat) * LN(fp_p) / (-8.0 * LN2_2)) as usize,
        )
        .unwrap()
    }

    /// Compute the max expected false positive rate for a bitmap
    /// of size bitmap_size (in bytes) which is expected to hold up to items_count items
    #[expect(clippy::cast_precision_loss)]
    #[inline]
    pub fn compute_false_positive_rate(
        bitmap_size: num::NonZeroUsize,
        items_count: num::NonZeroUsize,
    ) -> BloomFloat {
        #[cfg(feature = "std")]
        const EXP: FloatMethod = BloomFloat::exp;
        #[cfg(not(feature = "std"))]
        const EXP: FloatMethod = libm::Libm::<BloomFloat>::exp;

        let m = (bitmap_size.get() * 8) as BloomFloat;
        let n = items_count.get() as BloomFloat;
        EXP(-m * LN2_2 / n)
    }

    #[expect(
        clippy::cast_precision_loss,
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss
    )]
    #[inline]
    pub(crate) fn compute_hash_count(
        bitmap_size: num::NonZeroU64,
        items_count: num::NonZeroUsize,
    ) -> num::NonZeroU32 {
        #[cfg(feature = "std")]
        const ROUND: FloatMethod = BloomFloat::round;
        #[cfg(not(feature = "std"))]
        const ROUND: FloatMethod = libm::Libm::<BloomFloat>::round;

        let m = (bitmap_size.get() * 8) as BloomFloat;
        let n = items_count.get() as BloomFloat;

        num::NonZero::new(cmp::max(ROUND(m * LN_2 / n) as u32, 1)).expect("1 > 0")
    }
}

impl Default for FSOptions<DefaultClock> {
    fn default() -> Self {
        Self::new_with_clock(DefaultClock)
    }
}

#[cfg(feature = "bloom")]
#[cfg(test)]
mod bloom_compute_tests {
    use super::*;

    use core::num::NonZeroUsize;

    #[cfg(not(feature = "std"))]
    use alloc::{boxed::Box, format, vec};

    #[cfg(feature = "bloom")]
    use crate::BloomFloat;

    struct Params {
        items_count: NonZeroUsize,
        fp_p: BloomFloat,
        bitmap_size: NonZeroUsize,
    }

    const PARAMS_LIST: &[Params] = &[
        Params {
            items_count: NonZeroUsize::new(4_000).unwrap(),
            fp_p: 1e-7,
            bitmap_size: NonZeroUsize::new(134_191_usize.div_ceil(8)).unwrap(),
        },
        Params {
            items_count: NonZeroUsize::new(500_000).unwrap(),
            fp_p: 2.7e-3,
            bitmap_size: NonZeroUsize::new(6_155_133_usize.div_ceil(8)).unwrap(),
        },
        Params {
            items_count: NonZeroUsize::new(43_695).unwrap(),
            fp_p: 2.35e-3,
            bitmap_size: NonZeroUsize::new(550_524_usize.div_ceil(8)).unwrap(),
        },
        Params {
            items_count: NonZeroUsize::new(1_000_000_000).unwrap(),
            fp_p: 4.2e-15,
            bitmap_size: NonZeroUsize::new(68_900_997_415_usize.div_ceil(8)).unwrap(),
        },
    ];

    #[test]
    fn correct_bloom_filter_size() {
        let mut predicted_bitmap_sizes = vec![(NonZeroUsize::MAX, true); PARAMS_LIST.len()];

        for (i, param) in PARAMS_LIST.iter().enumerate() {
            let predicted_size = bloom::compute_bitmap_size(param.items_count, param.fp_p);
            predicted_bitmap_sizes[i] = (predicted_size, predicted_size != param.bitmap_size);
        }

        let failures = predicted_bitmap_sizes
            .iter()
            .enumerate()
            .filter_map(|(i, entry)| (entry.1).then_some((i, entry.0)))
            .collect::<Box<_>>();

        if !failures.is_empty() {
            panic!(
                "Incorrectly predicted filter size for the following cases:\n{}",
                failures
                    .iter()
                    .map(|entry| format!(
                        "index {}: expected {}, found {}",
                        entry.0 + 1,
                        PARAMS_LIST[entry.0].bitmap_size,
                        entry.1
                    ))
                    .reduce(|acc, s| format!("{acc}\n{s}"))
                    .unwrap_or_default()
            )
        }
    }

    #[test]
    fn correct_bloom_false_positive_probability() {
        let mut predicted_fp_ps = vec![(0.0, true); PARAMS_LIST.len()];

        // TODO: find a proper way to deal with floating-points errors
        // till then, this is good enough
        const FP_P_ERROR_FACTOR: BloomFloat = 1e-3;

        for (i, param) in PARAMS_LIST.iter().enumerate() {
            let predicted_fp_p =
                bloom::compute_false_positive_rate(param.bitmap_size, param.items_count);
            predicted_fp_ps[i] = (
                predicted_fp_p,
                (predicted_fp_p - param.fp_p).abs()
                    > predicted_fp_p.min(param.fp_p) * FP_P_ERROR_FACTOR,
            );
        }

        let failures = predicted_fp_ps
            .iter()
            .enumerate()
            .filter_map(|(i, entry)| (entry.1).then_some((i, entry.0)))
            .collect::<Box<_>>();

        if !failures.is_empty() {
            panic!(
                "Incorrectly predicted false positive probability for the following cases:\n{}",
                failures
                    .iter()
                    .map(|entry| format!(
                        "index {}: expected {}, found {} ",
                        entry.0 + 1,
                        PARAMS_LIST[entry.0].fp_p,
                        entry.1,
                    ))
                    .reduce(|acc, s| format!("{acc}\n{s}"))
                    .unwrap_or_default()
            )
        }
    }
}
