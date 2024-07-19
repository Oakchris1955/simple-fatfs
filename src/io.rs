use crate::error::IOError;

pub mod prelude {
    pub use super::{IOBase, Read, Seek, SeekFrom, Write};
}

// Some things here were borrowed from the Rust Standard Library and the source code of the `ciborium-io` crate

/// The base trait on which [`Read`], [`Write`] and [`Seek`] build
/// Mainly used to provide a shared error type
pub trait IOBase {
    /// The error type
    type Error: IOError;
}

/// A simplified version of [`std::io::Read`] for use within a `no_std` context
pub trait Read: IOBase {
    /// Pull some bytes from this source into the specified buffer, returning how many bytes were read.
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error>;

    /// Read the exact number of bytes required to fill `buf`.
    ///
    /// Blocks until enough bytes could be read
    ///
    /// Returns an error if EOF is met.
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), Self::Error>;
}

/// A simplified version of `std::io::Write` for use within a `no_std` context
pub trait Write: IOBase {
    /// Attempts to write an entire buffer into this writer.
    ///
    /// Blocks until the entire buffer has been written
    fn write_all(&mut self, data: &[u8]) -> Result<(), Self::Error>;

    /// Flush this output stream, ensuring that all intermediately buffered contents reach their destination.
    ///
    /// # Errors
    /// It is considered an error if not all bytes could be written due to I/O errors or EOF being reached.
    fn flush(&mut self) -> Result<(), Self::Error>;
}

/// A literal copy of the `std::io::Seekfrom` enum for use within a `no_std` context
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

/// A simplified version of `std::io::Seek` for use within a `no_std` context
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
    /// Seeking beyond the end of the stream should also be considered an error.
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
    fn read(&mut self, data: &mut [u8]) -> Result<usize, Self::Error> {
        self.read(data)
    }

    #[inline]
    fn read_exact(&mut self, data: &mut [u8]) -> Result<(), Self::Error> {
        self.read_exact(data)
    }
}

#[cfg(feature = "std")]
impl<T> Write for T
where
    T: std::io::Write + IOBase<Error = std::io::Error>,
{
    #[inline]
    fn write_all(&mut self, data: &[u8]) -> Result<(), Self::Error> {
        self.write_all(data)
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
    fn read(&mut self, data: &mut [u8]) -> Result<usize, R::Error> {
        (**self).read(data)
    }

    #[inline]
    fn read_exact(&mut self, data: &mut [u8]) -> Result<(), R::Error> {
        (**self).read_exact(data)
    }
}

#[cfg(not(feature = "std"))]
impl<W: Write + IOBase> Write for &mut W {
    #[inline]
    fn write_all(&mut self, data: &[u8]) -> Result<(), W::Error> {
        (**self).write_all(data)
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
