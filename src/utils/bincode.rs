use bincode::{DefaultOptions, Options};

#[inline]
// an easy way to universally use the same bincode (de)serialization options
pub(crate) fn bincode_config() -> impl Options + Copy {
    // also check https://docs.rs/bincode/1.3.3/bincode/config/index.html#options-struct-vs-bincode-functions
    DefaultOptions::new()
        .with_fixint_encoding()
        .allow_trailing_bytes()
        .with_little_endian()
}
