use core::fmt;

use bincode::error::{DecodeError, EncodeError};

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
    /// Create a new `Unsupported` [`IOErrorKind`]
    fn new_unsupported() -> Self;

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
    /// Check whether this [`IOErrorKind`] is of kind `Unsupported`
    #[inline]
    fn is_unsupported(&self) -> bool {
        self == &Self::new_unsupported()
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
    #[inline]
    fn new_unsupported() -> Self {
        std::io::ErrorKind::Unsupported
    }
}

/// An error type that denotes that there is something wrong
/// with the filesystem's structure itself (perhaps the FS itself is malformed/corrupted)
#[non_exhaustive]
#[derive(Debug, Clone, Copy, displaydoc::Display)]
pub enum InternalFSError {
    /// The storage medium isn't large enough to accompany a FAT filesystem
    StorageTooSmall,
    /// Invalid boot sector signature. Perhaps this isn't a FAT filesystem?
    InvalidBPBSig,
    /**
     Invalid FAT32 FSInfo signature.
     Perhaps the FSInfo structure or the FAT32 Ebr's fat_info field is malformed?
    */
    InvalidFSInfoSig,
    /**
     The FAT and it's copies do not much.
     This is either the result of some bad FAT library that chose to ignore the FAT copies
     or perhaps the storage medium has been corrupted (most likely).
     Either way, we are not handling this FileSystem
    */
    MismatchingFATTables,
    /// Encountered a malformed cluster chain
    MalformedClusterChain,
    /// Encountered a malformed directory entry chain
    MalformedEntryChain,
}

/// An error indicating that a filesystem-related operation has failed
#[non_exhaustive]
#[derive(Debug, displaydoc::Display)]
pub enum FSError<I>
where
    I: IOError,
{
    /// An internal FS error occured
    #[displaydoc("An internal FS error occured: {0}")]
    InternalFSError(InternalFSError),
    /**
     The [Path](`crate::Path`) provided is malformed.

     This usually means that a path you provided isn't a valid [`Utf8WindowsPath`](typed_path::Utf8WindowsPath)

     If you are 100% that your path is valid (`path.is_valid()`), then perhaps you have encountered a bug.
     File a bug report here: <https://github.com/Oakchris1955/simple-fatfs/issues>
    */
    MalformedPath,
    /**
     [`bincode`] errored out while (de)serializing

     This error variant should NEVER be raised.
     If you get this error, open an issue: <https://github.com/Oakchris1955/simple-fatfs/issues>
    */
    BincodeError(BincodeError),
    /// Expected a directory
    NotADirectory,
    /// Found a directory when we expected a file
    IsADirectory,
    /// Expected an empty directory
    DirectoryNotEmpty,
    /// This file cannot be modified, as it is read-only
    ReadOnlyFile,
    /// A file or directory wasn't found
    NotFound,
    /// An entity already exists
    AlreadyExists,
    /// The operation lacked the necessary privileges to complete.
    PermissionDenied,
    /// A parameter was incorrect.
    InvalidInput,
    /// The underlying sotrage is full.
    StorageFull,
    /**
     There aren't enough free entries on the root directory to perform
     this operation. Consider performing this operation on a subdirectory instead
    */
    RootDirectoryFull,
    /// An IO error occured
    #[displaydoc("An IO error occured: {0}")]
    IOError(I),
}

/// An encode/decode-related error
///
/// This error enum should NEVER be raised.
/// If you get it, file an issue: <https://github.com/Oakchris1955/simple-fatfs/issues>
#[derive(Debug)]
pub enum BincodeError {
    /// A decode-related error
    DecodeError(DecodeError),
    /// An encode-related error
    EncodeError(EncodeError),
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

impl<I> From<DecodeError> for FSError<I>
where
    I: IOError,
{
    #[inline]
    fn from(value: DecodeError) -> Self {
        FSError::BincodeError(BincodeError::DecodeError(value))
    }
}

impl<I> From<EncodeError> for FSError<I>
where
    I: IOError,
{
    #[inline]
    fn from(value: EncodeError) -> Self {
        FSError::BincodeError(BincodeError::EncodeError(value))
    }
}

/// An alias for a [`Result`] with a [`FSError`] error type
pub type FSResult<T, E> = Result<T, FSError<E>>;
