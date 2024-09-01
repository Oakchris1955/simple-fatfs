#[cfg(not(feature = "std"))]
use alloc::string::String;

use alloc::string::FromUtf16Error;

/// variation of https://stackoverflow.com/a/42067321/19247098 for processing LFNs
pub(crate) fn string_from_lfn(utf16_src: &[u16]) -> Result<String, FromUtf16Error> {
    let nul_range_end = utf16_src
        .iter()
        .position(|c| *c == 0x0000)
        .unwrap_or(utf16_src.len()); // default to length if no `\0` present

    String::from_utf16(&utf16_src[0..nul_range_end])
}
