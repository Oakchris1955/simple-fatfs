#[cfg(not(feature = "std"))]
use core::*;
#[cfg(feature = "std")]
use std::*;

pub trait Error: fmt::Debug + fmt::Display {}
impl<T> Error for T where T: fmt::Debug + fmt::Display {}

pub trait IOError: Error {
    fn is_unexpected_eof(&self) -> bool;
}

#[cfg(feature = "std")]
impl IOError for std::io::Error {
    fn is_unexpected_eof(&self) -> bool {
        self.kind() == std::io::ErrorKind::UnexpectedEof
    }
}

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
    /// The [PathBuf](`crate::path::PathBuf`) provided is malformed
    MalformedPath,
    /// Expected a directory
    NotADirectory,
    /// Found a directory when we expected a file
    IsADirectory,
    /// A file or directory wasn't found
    NotFound,
    /// Found an unexpected EOF while reading a file
    UnexpectedEOF,
    /// {0}
    IOError(I),
}

impl<I> From<I> for FSError<I>
where
    I: IOError,
{
    fn from(value: I) -> Self {
        FSError::IOError(value)
    }
}

pub type FSResult<T, E> = Result<T, FSError<E>>;
