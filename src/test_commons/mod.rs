#![allow(unused)]
use super::*;

#[cfg(not(feature = "std"))]
extern crate alloc;
#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec};

mod wrapper {
    use super::*;

    include! {concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/common/wrapper.rs"
    )}
}

mod fixtures {
    use super::*;

    use var::*;

    include! {concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/common/fixtures.rs"
    )}
}

mod var {
    use super::*;

    include! {concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/common/var.rs"
    )}
}

pub(crate) use fixtures::*;
pub(crate) use var::*;
pub(crate) use wrapper::*;
