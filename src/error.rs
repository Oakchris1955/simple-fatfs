#[cfg(not(feature = "std"))]
use core::*;
#[cfg(feature = "std")]
use std::*;

/// Base error type
///
/// To be replaced with [`core::error`] when feature [`error_in_core`](https://github.com/rust-lang/rust/issues/103765) gets pushed to `stable`
pub trait Error: fmt::Debug + fmt::Display {}

#[cfg(feature = "std")]
impl Error for std::io::Error {}

/// Base IO error type
pub trait IOError: Error {
    /// The type of the kind of this [`IOError`]
    type Kind: IOErrorKind;

    /// Construct a new [`IOError`] from an [`IOErrorKind`] and a `msg`
    fn new<M>(kind: Self::Kind, msg: M) -> Self
    where
        M: fmt::Display;

    /// Get the kind of this [`IOError`]
    fn kind(&self) -> Self::Kind;
}

#[cfg(feature = "std")]
impl IOError for std::io::Error {
    type Kind = std::io::ErrorKind;

    #[inline]
    fn new<M>(kind: Self::Kind, msg: M) -> Self
    where
        M: fmt::Display,
    {
        std::io::Error::new(kind, msg.to_string())
    }

    #[inline]
    fn kind(&self) -> Self::Kind {
        self.kind()
    }
}

/// The kind of an [`IOError`]
pub trait IOErrorKind: PartialEq + Sized {
    /// Create a new `UnexpectedEOF` [`IOErrorKind`]
    fn new_unexpected_eof() -> Self;
    /// Create a new `Interrupted` [`IOErrorKind`]
    fn new_interrupted() -> Self;

    #[inline]
    /// Check whether this [`IOErrorKind`] is of kind `UnexpectedEOF`
    fn is_unexpected_eof(&self) -> bool {
        self == &Self::new_unexpected_eof()
    }
    /// Check whether this [`IOErrorKind`] is of kind `Interrupted`
    #[inline]
    fn is_interrupted(&self) -> bool {
        self == &Self::new_interrupted()
    }
}

#[cfg(feature = "std")]
impl IOErrorKind for std::io::ErrorKind {
    #[inline]
    fn new_unexpected_eof() -> Self {
        std::io::ErrorKind::UnexpectedEof
    }
    #[inline]
    fn new_interrupted() -> Self {
        std::io::ErrorKind::Interrupted
    }
}

/// An error indicating that a filesystem-related operation has failed
#[derive(Debug, displaydoc::Display)]
pub enum FSError<I>
where
    I: IOError,
{
    /// The storage medium isn't large enough to accompany a FAT filesystem
    StorageTooSmall,
    /// Invalid boot sector signature. Perharps this isn't a FAT filesystem?
    InvalidBPBSig,
    /// Encountered a malformed cluster chain
    MalformedClusterChain,
    /**
     The [PathBuf](`crate::path::PathBuf`) provided is malformed.

     This is mostly an error variant used for internal testing.
     If you get this error, open an issue: <https://github.com/Oakchris1955/simple-fatfs/issues>
    */
    MalformedPath,
    /// Expected a directory
    NotADirectory,
    /// Found a directory when we expected a file
    IsADirectory,
    /// A file or directory wasn't found
    NotFound,
    /// Found an unexpected EOF while reading a file
    UnexpectedEOF,
    /// An IO error occured
    #[displaydoc("An IO error occured: {0}")]
    IOError(I),
}

impl<I> From<I> for FSError<I>
where
    I: IOError,
{
    #[inline]
    fn from(value: I) -> Self {
        FSError::IOError(value)
    }
}

/// An alias for a [`Result`] with a [`FSError`] error type
pub type FSResult<T, E> = Result<T, FSError<E>>;
