use bincode::config::{self, standard as StandardOptions, Configuration};

// an easy way to universally use the same bincode (de)serialization options
pub(crate) const BINCODE_CONFIG: Configuration<
    config::LittleEndian,
    config::Fixint,
    config::NoLimit,
> = StandardOptions()
    .with_fixed_int_encoding()
    .with_no_limit()
    .with_little_endian();
