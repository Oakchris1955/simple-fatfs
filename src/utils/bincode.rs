use bincode::config::{standard as StandardOptions, Config};

#[inline]
// an easy way to universally use the same bincode (de)serialization options
pub(crate) const fn bincode_config() -> impl Config {
    // also check https://docs.rs/bincode/1.3.3/bincode/config/index.html#options-struct-vs-bincode-functions
    StandardOptions()
        .with_fixed_int_encoding()
        .with_no_limit()
        .with_little_endian()
}
