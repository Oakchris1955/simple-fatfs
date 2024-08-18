//! This module contains all the IO-related objects of the crate
//!
//! If you want to implement IO functionality in a `std` environment,
//! it is advised to implement the relevant [`std::io`] traits, which will
//! auto-`impl` the corresponding IO traits here. In a `no-std` environment,
//! implement the traits directly
//!
//! # Traits
//!
//! - [`IOBase`] provides a common error type for [`Read`], [`Write`] & [`Seek`]
//! - [`Read`] allows for reading bytes from a source.
//! - [`Write`] allows for writing bytes to a sink.
//! - [`Seek`] provides a cursor which can be moved within a stream of bytes

#[cfg(not(feature = "std"))]
use core::*;
#[cfg(feature = "std")]
use std::*;

use ::alloc::{string::String, vec::Vec};

use crate::error::{IOError, IOErrorKind};

/// With `use prelude::*`, all IO-related traits are automatically imported
pub mod prelude {
    pub use super::{IOBase, Read, Seek, SeekFrom, Write};
}

// Some things here were borrowed from the Rust Standard Library and the source code of the `ciborium-io` crate

/// All other IO traits ([`Read`], [`Write`] and [`Seek`]) are supertraits of this
///
/// Used to define a shared error type for these traits
pub trait IOBase {
    /// The error type
    type Error: IOError;
}

/// A simplified version of [`std::io::Read`] for use within a `no_std` context
pub trait Read: IOBase {
    /// Pull some bytes from this source into the specified buffer,
    /// returning how many bytes were read.
    ///
    /// If the return value of this method is [`Ok(n)`], then implementations must
    /// guarantee that `0 <= n <= buf.len()`. A nonzero `n` value indicates
    /// that the buffer `buf` has been filled in with `n` bytes of data from this
    /// source. If `n` is `0`, then it can indicate one of two scenarios:
    ///
    /// 1. This reader has reached its "end of file" and will likely no longer
    ///    be able to produce bytes. Note that this does not mean that the
    ///    reader will *always* no longer be able to produce bytes.
    /// 2. The buffer specified was 0 bytes in length.
    ///
    /// It is not an error if the returned value `n` is smaller than the buffer size,
    /// even when the reader is not at the end of the stream yet.
    /// This may happen for example because fewer bytes are actually available right now
    /// (e. g. being close to end-of-file) or because read() was interrupted by a signal,
    /// although for the later, an [`IOErrorKind`] of kind `Interrupted` should preferably be raised
    ///
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error>;

    /// Read all bytes until EOF in this source, placing them into `buf`.
    ///
    /// All bytes read from this source will be appended to the specified buffer
    /// `buf`. This function will continuously call [`read()`] to append more data to
    /// `buf` until [`read()`] returns either [`Ok(0)`] or an [`IOErrorKind`] of
    /// non-`Interrupted` kind.
    ///
    /// [`read()`]: Read::read
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, Self::Error> {
        let mut bytes_read = 0;

        const PROBE_SIZE: usize = 32;
        let mut probe = [0_u8; PROBE_SIZE];

        loop {
            match self.read(&mut probe) {
                Ok(0) => break,
                Ok(n) => {
                    buf.extend_from_slice(&probe[..n]);
                    bytes_read += n;
                }
                Err(ref e) if e.kind().is_interrupted() => continue,
                Err(e) => return Err(e),
            }
        }

        Ok(bytes_read)
    }

    /// Read all bytes until EOF in this source, appending them to `string`.
    ///
    /// If successful, this function returns the number of bytes which were read and appended to buf.
    ///
    /// # Errors
    ///
    /// If the data in this stream is *not* valid UTF-8 then an error is returned and `string` is unchanged.
    ///
    /// See [`read_to_end`](Read::read_to_end) for other error semantics.
    fn read_to_string(&mut self, string: &mut String) -> Result<usize, Self::Error> {
        let mut buf = Vec::new();
        let bytes_read = self.read_to_end(&mut buf)?;

        string.push_str(str::from_utf8(&buf).map_err(|_| {
            IOError::new(
                <Self::Error as IOError>::Kind::new_invalid_data(),
                "found invalid utf-8",
            )
        })?);

        Ok(bytes_read)
    }

    /// Read the exact number of bytes required to fill `buf`.
    ///
    /// This function reads as many bytes as necessary to completely fill the
    /// specified buffer `buf`.
    ///
    /// *Implementations* of this method can make no assumptions about the contents of `buf` when
    /// this function is called. It is recommended that implementations only write data to `buf`
    /// instead of reading its contents. The documentation on [`read`](Read::read) has a more detailed
    /// explanation of this subject.
    fn read_exact(&mut self, mut buf: &mut [u8]) -> Result<(), Self::Error> {
        while !buf.is_empty() {
            match self.read(buf) {
                Ok(0) => break,
                Ok(n) => buf = &mut buf[n..],
                Err(ref e) if e.kind().is_interrupted() => {}
                Err(e) => return Err(e),
            }
        }
        if !buf.is_empty() {
            Err(Self::Error::new(
                <<Self::Error as IOError>::Kind as IOErrorKind>::new_unexpected_eof(),
                "failed to fill whole buffer",
            ))
        } else {
            Ok(())
        }
    }
}

/// A simplified version of [`std::io::Write`] for use within a `no_std` context
pub trait Write: IOBase {
    /// Write a buffer into this writer, returning how many bytes were written.
    ///
    /// This function will attempt to write the entire contents of `buf`, but
    /// the entire write might not succeed, or the write may also generate an
    /// error. Typically, a call to `write` represents one attempt to write to
    /// any wrapped object.
    ///
    /// If this method consumed `n > 0` bytes of `buf` it must return [`Ok(n)`].
    /// If the return value is `Ok(n)` then `n` must satisfy `n <= buf.len()`.
    /// A return value of `Ok(0)` typically means that the underlying object is
    /// no longer able to accept bytes and will likely not be able to in the
    /// future as well, or that the buffer provided is empty.
    ///
    /// # Errors
    ///
    /// Each call to `write` may generate an I/O error indicating that the
    /// operation could not be completed. If an error is returned then no bytes
    /// in the buffer were written to this writer.
    ///
    /// It is **not** considered an error if the entire buffer could not be
    /// written to this writer.
    ///
    /// An error of the `Interrupted` [`IOErrorKind`] is non-fatal and the
    /// write operation should be retried if there is nothing else to do.
    fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error>;

    /// Attempts to write an entire buffer into this writer.
    ///
    /// This method will continuously call [`write`] until there is no more data
    /// to be written or an error of non-`Interrupted` [`IOErrorKind`] is
    /// returned. This method will not return until the entire buffer has been
    /// successfully written or such an error occurs. The first error that is
    /// not of `Interrupted` [`IOErrorKind`] generated from this method will be
    /// returned.
    ///
    /// If the buffer contains no data, this will never call [`write`].
    ///
    /// # Errors
    ///
    /// This function will return the first error of
    /// non-`Interrupted` [`IOErrorKind`] that [`write`] returns.
    ///
    /// [`write`]: Write::write
    fn write_all(&mut self, mut buf: &[u8]) -> Result<(), Self::Error> {
        while !buf.is_empty() {
            match self.write(buf) {
                Ok(0) => {
                    return Err(IOError::new(
                        IOErrorKind::new_unexpected_eof(),
                        "writer returned EOF before all data could be written",
                    ));
                }
                Ok(n) => buf = &buf[n..],
                Err(ref e) if e.kind().is_interrupted() => {}
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    /// Flush this output stream, ensuring that all intermediately buffered
    /// contents reach their destination.
    ///
    /// # Errors
    ///
    /// It is considered an error if not all bytes could be written due to
    /// I/O errors or EOF being reached.
    fn flush(&mut self) -> Result<(), Self::Error>;
}

/// A copy of [`std::io::SeekFrom`] for use within a `no_std` context
#[derive(Debug, Clone, Copy)]
pub enum SeekFrom {
    /// Sets the offset to the provided number of bytes.
    Start(u64),

    /// Sets the offset to the size of this object plus the specified number of
    /// bytes.
    ///
    /// It is possible to seek beyond the end of an object, but it's an error to
    /// seek before byte 0.
    End(i64),

    /// Sets the offset to the current position plus the specified number of
    /// bytes.
    ///
    /// It is possible to seek beyond the end of an object, but it's an error to
    /// seek before byte 0.
    Current(i64),
}

#[cfg(feature = "std")]
impl From<SeekFrom> for std::io::SeekFrom {
    fn from(value: SeekFrom) -> Self {
        match value {
            SeekFrom::Start(offset) => std::io::SeekFrom::Start(offset),
            SeekFrom::Current(offset) => std::io::SeekFrom::Current(offset),
            SeekFrom::End(offset) => std::io::SeekFrom::End(offset),
        }
    }
}

#[cfg(feature = "std")]
impl From<std::io::SeekFrom> for SeekFrom {
    fn from(value: std::io::SeekFrom) -> Self {
        match value {
            std::io::SeekFrom::Start(offset) => SeekFrom::Start(offset),
            std::io::SeekFrom::Current(offset) => SeekFrom::Current(offset),
            std::io::SeekFrom::End(offset) => SeekFrom::End(offset),
        }
    }
}

/// A simplified version of [`std::io::Seek`] for use within a `no_std` context
pub trait Seek: IOBase {
    /// Seek to an offset, in bytes, in a stream.
    ///
    /// A seek beyond the end of a stream **IS NOT ALLOWED**.
    ///
    /// If the seek operation completed successfully, this method returns the new position from the start of the stream. That position can be used later with `SeekFrom::Start`.
    ///
    /// # Errors
    /// Seeking can fail, for example because it might involve flushing a buffer.
    ///
    /// Seeking to a negative offset is considered an error.
    ///
    /// Seeking beyond the end of the stream behaviour depends on the implementation.
    /// If [`self`] can be extended (because it's a [`File`](fs::File) for example), this shouldn't error out.
    /// In the case that a stream is being extended or if the stream can't be extended,
    /// this should return an [`IOError`] with an [`IOErrorKind`] of `UnexpectedEOF`.
    /// The [`seek`] operation should be considered partially successfull, since some clusters were allocated.
    /// In general, for suchs cases, it is recommended that the user first checks if there's enough storage and then perform the action
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, Self::Error>;

    /// Rewind to the beginning of a stream.
    ///
    /// This is a convenience method, equivalent to `seek(SeekFrom::Start(0))`.
    ///
    /// # Errors
    /// Rewinding can fail, for example because it might involve flushing a buffer.
    fn rewind(&mut self) -> Result<(), Self::Error> {
        self.seek(SeekFrom::Start(0))?;

        Ok(())
    }

    /// Returns the current seek position from the start of the stream.
    ///
    /// This is equivalent to `self.seek(SeekFrom::Current(0))`.
    fn stream_position(&mut self) -> Result<u64, Self::Error> {
        self.seek(SeekFrom::Current(0))
    }
}

#[cfg(feature = "std")]
impl<T> IOBase for T
where
    T: std::io::Read + std::io::Write + std::io::Seek,
{
    type Error = std::io::Error;
}

#[cfg(feature = "std")]
impl<T> Read for T
where
    T: std::io::Read + IOBase<Error = std::io::Error>,
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        self.read(buf)
    }

    #[inline]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, Self::Error> {
        self.read_to_end(buf)
    }

    #[inline]
    fn read_to_string(&mut self, string: &mut String) -> Result<usize, Self::Error> {
        self.read_to_string(string)
    }

    #[inline]
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), Self::Error> {
        self.read_exact(buf)
    }
}

#[cfg(feature = "std")]
impl<T> Write for T
where
    T: std::io::Write + IOBase<Error = std::io::Error>,
{
    #[inline]
    fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        self.write(buf)
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<(), Self::Error> {
        self.write_all(buf)
    }

    #[inline]
    fn flush(&mut self) -> Result<(), Self::Error> {
        self.flush()
    }
}

#[cfg(feature = "std")]
impl<T> Seek for T
where
    T: std::io::Seek + IOBase<Error = std::io::Error>,
{
    #[inline]
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, Self::Error> {
        self.seek(pos.into())
    }

    #[inline]
    fn rewind(&mut self) -> Result<(), Self::Error> {
        self.rewind()
    }

    #[inline]
    fn stream_position(&mut self) -> Result<u64, Self::Error> {
        self.stream_position()
    }
}

// https://github.com/rust-lang/rust/issues/31844 open for 8 years, not yet stabilized (as of 2024)
#[cfg(not(feature = "std"))]
impl<T> IOBase for &mut T
where
    T: IOBase,
{
    type Error = T::Error;
}

#[cfg(not(feature = "std"))]
impl<R: Read + IOBase> Read for &mut R {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, R::Error> {
        (**self).read(buf)
    }

    #[inline]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, R::Error> {
        (**self).read_to_end(buf)
    }

    #[inline]
    fn read_to_string(&mut self, buf: &mut String) -> Result<usize, R::Error> {
        (**self).read_to_string(buf)
    }

    #[inline]
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), R::Error> {
        (**self).read_exact(buf)
    }
}

#[cfg(not(feature = "std"))]
impl<W: Write + IOBase> Write for &mut W {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> Result<usize, W::Error> {
        (**self).write(buf)
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<(), W::Error> {
        (**self).write_all(buf)
    }

    #[inline]
    fn flush(&mut self) -> Result<(), W::Error> {
        (**self).flush()
    }
}

#[cfg(not(feature = "std"))]
impl<S: Seek + IOBase> Seek for &mut S {
    #[inline]
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, S::Error> {
        (**self).seek(pos.into())
    }

    #[inline]
    fn rewind(&mut self) -> Result<(), S::Error> {
        (**self).rewind()
    }

    #[inline]
    fn stream_position(&mut self) -> Result<u64, S::Error> {
        (**self).stream_position()
    }
}
