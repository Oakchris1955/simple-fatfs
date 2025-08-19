use crate::*;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

#[derive(Debug)]
/// Filesystem mount options
pub struct FSOptions {
    pub(crate) clock: Box<dyn Clock>,
    pub(crate) codepage: codepage::Codepage,
}

impl FSOptions {
    #[inline]
    /// Create a new options struct with the default options
    ///
    /// This is just an alias to [`Self::default`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the codepage to be used by the filesystem
    pub fn set_codepage(&mut self, codepage: Codepage) {
        self.codepage = codepage
    }

    /// Set the codepage to be used by the filesystem (chainable)
    pub fn with_codepage(mut self, codepage: Codepage) -> Self {
        self.set_codepage(codepage);

        self
    }
}

impl Default for FSOptions {
    fn default() -> Self {
        Self {
            clock: Box::new(DefaultClock),
            codepage: codepage::Codepage::CP437,
        }
    }
}
