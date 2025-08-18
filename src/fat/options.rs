use crate::*;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

#[derive(Debug)]
/// Filesystem mount options
pub struct FSOptions {
    pub(crate) clock: Box<dyn Clock>,
}

impl FSOptions {
    #[inline]
    /// Create a new options struct with the default options
    ///
    /// This is just an alias to [`Self::default`]
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for FSOptions {
    fn default() -> Self {
        Self {
            clock: Box::new(DefaultClock),
        }
    }
}
