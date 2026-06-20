#![allow(
    unused,
    reason = "due to the `include!` statements below, unused code here might be used somewhere else"
)]

pub use simple_fatfs::{block_io::*, *};

#[cfg(not(feature = "std"))]
extern crate alloc;
#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

include! {"var.rs"}
include! {"fixtures.rs"}
include! {"wrapper.rs"}
