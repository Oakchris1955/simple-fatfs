use crate::error::{IOError, IOErrorKind};
use crate::io::prelude::*;

/// Returns a `Result<bool>`, where the `bool` indicates whether
/// the storage medium support write operations or not
pub(crate) fn storage_medium_is_rw<S>(storage: &mut S) -> Result<bool, <S as IOBase>::Error>
where
    S: Read + Write + Seek,
{
    // this zero-length write should return an error
    // of `Unexpected` IOErrorKind is the storage medium is read-only
    match storage.write_all(&[]) {
        Ok(_) => Ok(true),
        // in case this filesystem doesn't support
        // write operations, don't error out
        Err(ref e) if e.kind().is_unsupported() => Ok(false),
        Err(e) => Err(e),
    }
}
