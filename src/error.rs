use embedded_io::*;

use crate::*;

/// An error type that denotes that there is something wrong
/// with the filesystem's structure itself (perhaps the FS itself is malformed/corrupted)
#[non_exhaustive]
#[derive(Debug)]
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
    /// The Hardware block size is not valid
    BlockSizeError,
}

/// An error indicating that a filesystem-related operation has failed
#[non_exhaustive]
#[derive(Debug)]
pub enum FSError<I>
where
    I: Error,
{
    /// An internal FS error occured
    InternalFSError(InternalFSError),
    /**
     The [Path](`crate::Path`) provided is malformed.

     This usually means that a path you provided isn't a valid [`Utf8WindowsPath`](typed_path::Utf8WindowsPath)

     If you are 100% that your path is valid (`path.is_valid()`), then perhaps you have encountered a bug.
     File a bug report here: <https://github.com/Oakchris1955/simple-fatfs/issues>
    */
    MalformedPath,
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
    /// The underlying storage is full.
    StorageFull,
    /**
     There aren't enough free entries on the root directory to perform
     this operation. Consider performing this operation on a subdirectory instead
    */
    RootDirectoryFull,
    /**
     The entry limit for this directory (2^16 - 1) has been reached.
     Consider performing this operation on a subdirectory instead
    */
    DirEntryLimitReached,
    /**
     The filesystem provided is not supported (e.g. ExFAT).
    */
    UnsupportedFS,
    /// Unexpected EOF
    UnexpectedEof,
    /// An IO error occured
    IOError(I),
}

impl<I> From<I> for FSError<I>
where
    I: Error,
{
    #[inline]
    fn from(value: I) -> Self {
        FSError::IOError(value)
    }
}

impl<I> From<ReadExactError<I>> for FSError<I>
where
    I: Error,
{
    #[inline]
    fn from(value: ReadExactError<I>) -> Self {
        match value {
            ReadExactError::UnexpectedEof => FSError::UnexpectedEof,
            ReadExactError::Other(e) => FSError::IOError(e),
        }
    }
}

impl<I> From<RWFileError<I>> for FSError<I>
where
    I: Error,
{
    fn from(value: RWFileError<I>) -> Self {
        match value {
            RWFileError::StorageFull => FSError::StorageFull,
            RWFileError::IOError(e) => FSError::IOError(e),
        }
    }
}

/// An alias for a [`Result`] with a [`FSError`] error type
pub type FSResult<T, E> = Result<T, FSError<E>>;
