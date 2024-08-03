#![cfg_attr(not(feature = "std"), no_std)]
// Even inside unsafe functions, we must acknowlegde the usage of unsafe code
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;

mod error;
mod fs;
pub mod io;
mod path;

pub use error::*;
pub use fs::*;
pub use path::*;
