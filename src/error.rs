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
    /// Create a new `InvalidData` [`IOErrorKind`]
    fn new_invalid_data() -> Self;

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
    /// Check whether this [`IOErrorKind`] is of kind `InvalidData`
    #[inline]
    fn is_invalid_data(&self) -> bool {
        self == &Self::new_invalid_data()
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
    #[inline]
    fn new_invalid_data() -> Self {
        std::io::ErrorKind::InvalidData
    }
}

/// An error type that denotes that there is something wrong
/// with the filesystem's structure itself (perhaps the FS itself is malformed/corrupted)
#[derive(Debug, Clone, Copy, displaydoc::Display)]
pub enum InternalFSError {
    /// The storage medium isn't large enough to accompany a FAT filesystem
    StorageTooSmall,
    /// Invalid boot sector signature. Perhaps this isn't a FAT filesystem?
    InvalidBPBSig,
    /**
     Invalid FAT32 FSInfo signature.
     Perhaps the FSInfo structure or the FAT32 EBR's fat_info field is malformed?
    */
    InvalidFSInfoSig,
    /// Encountered a malformed cluster chain
    MalformedClusterChain,
}

/// An error indicating that a filesystem-related operation has failed
#[derive(Debug, displaydoc::Display)]
pub enum FSError<I>
where
    I: IOError,
{
    /// An internal FS error occured
    #[displaydoc("An internal FS error occured: {0}")]
    InternalFSError(InternalFSError),
    /**
     The [PathBuf](`crate::path::PathBuf`) provided is malformed.

     This is mostly an error variant used for internal testing.
     If you get this error, open an issue: <https://github.com/Oakchris1955/simple-fatfs/issues>
    */
    MalformedPath,
    /**
     [`bincode`] errored out while (de)serializing

     This error variant should NEVER be raised.
     If you get this error, open an issue: <https://github.com/Oakchris1955/simple-fatfs/issues>
    */
    BincodeError(bincode::Error),
    /// Expected a directory
    NotADirectory,
    /// Found a directory when we expected a file
    IsADirectory,
    /// A file or directory wasn't found
    NotFound,
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

impl<I> From<bincode::Error> for FSError<I>
where
    I: IOError,
{
    #[inline]
    fn from(value: bincode::Error) -> Self {
        FSError::BincodeError(value)
    }
}

/// An alias for a [`Result`] with a [`FSError`] error type
pub type FSResult<T, E> = Result<T, FSError<E>>;
