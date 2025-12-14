use crate::*;

use core::{cmp, num};

#[derive(Debug)]
/// FileSystem mount options
pub struct FSOptions<C: Clock> {
    pub(crate) clock: C,
    pub(crate) codepage: codepage::Codepage,
    pub(crate) update_file_fields: bool,
    pub(crate) check_boot_signature: bool,
    pub(crate) filter_size: num::NonZeroUsize,
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
            codepage: codepage::Codepage::default(),
            update_file_fields: false,
            check_boot_signature: true,
            filter_size: compute_bitmap_size(num::NonZero::new(1_000).unwrap(), 0.01),
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

    /// Set the bloom filter size to be that many `bits` long
    pub fn set_filter_size(&mut self, bits: num::NonZeroUsize) {
        self.filter_size = bits
    }

    /// Set the bloom filter size to be that many `bits` long (chainable)
    pub fn with_filter_size(mut self, bits: num::NonZeroUsize) -> Self {
        self.filter_size = bits;

        self
    }

    /// Query the directory cache / Bloom filter's size in bytes
    pub fn query_filter_size(&self) -> num::NonZeroUsize {
        self.filter_size
    }
}

// taken from utils::bloom::Bloom
/// Compute a recommended bitmap size for items_count items
/// and a fp_p rate of false positives.
/// fp_p obviously has to be within the ]0.0, 1.0[ range
/// or this will panic
#[inline]
pub fn compute_bitmap_size(items_count: num::NonZeroUsize, fp_p: f64) -> num::NonZeroUsize {
    assert!(fp_p > 0.0 && fp_p < 1.0);
    let log2 = core::f64::consts::LN_2;
    let log2_2 = log2 * log2;

    #[expect(
        clippy::cast_precision_loss,
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss
    )]
    {
        num::NonZero::new(
            ((items_count.get() as f64) * f64::ln(fp_p) / (-8.0 * log2_2)).ceil() as usize,
        )
        .unwrap()
    }
}

/// Compute the max expected false positive rate for a bitmap
/// of size bitmap_size which is expected to hold up to items_count items
#[expect(clippy::cast_precision_loss)]
#[inline]
pub fn compute_false_positive_rate(
    bitmap_size: num::NonZeroUsize,
    items_count: num::NonZeroUsize,
) -> f64 {
    let m = (bitmap_size.get() * 8) as f64;
    let n = items_count.get() as f64;
    let log2 = core::f64::consts::LN_2;
    let log2_2 = log2.powi(2);
    let e = core::f64::consts::E;
    e.powf(-m * log2_2 / n)
}

#[expect(
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
)]
#[inline]
pub(crate) fn compute_hash_count(
    bitmap_size: num::NonZeroUsize,
    items_count: num::NonZeroUsize,
) -> num::NonZeroUsize {
    let m = (bitmap_size.get() * 8) as f64;
    let n = items_count.get() as f64;

    num::NonZero::new(cmp::max(
        (m * core::f64::consts::LN_2 / n).round() as usize,
        1,
    ))
    .expect("1 > 0")
}

impl Default for FSOptions<DefaultClock> {
    fn default() -> Self {
        Self::new_with_clock(DefaultClock)
    }
}
