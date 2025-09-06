use bincode::config::{self, standard as StandardOptions, Configuration};
use bincode::error::{DecodeError, EncodeError};

use crate::*;

// an easy way to universally use the same bincode (de)serialization options
pub(crate) const BINCODE_CONFIG: Configuration<
    config::LittleEndian,
    config::Fixint,
    config::NoLimit,
> = StandardOptions()
    .with_fixed_int_encoding()
    .with_no_limit()
    .with_little_endian();

pub(crate) fn map_err_enc<I>(err: EncodeError) -> FSError<I>
where
    I: embedded_io::Error,
{
    FSError::BincodeError(crate::BincodeError::EncodeError(err))
}

pub(crate) fn map_err_dec<I>(err: DecodeError) -> FSError<I>
where
    I: embedded_io::Error,
{
    FSError::BincodeError(crate::BincodeError::DecodeError(err))
}
