//! # simple-fatfs
//!
//! An easy-to-use FAT filesystem library designed for usage in embedded systems
//!
//! ## Features
//!
//! - `no_std` support
//! - FAT12/16/32 support
//! - VFAT/LFN (long filenames) support
//! - Auto-`impl`s for [`std::io`] traits and structs
//! - Easy-to-implement [`io`] traits
//!
//! ## Examples
//! ```
//! # // this test fails on a no_std environment, don't run it in such a case
//! extern crate simple_fatfs;
//! use simple_fatfs::*;
//! use simple_fatfs::io::prelude::*;
//!
//! const FAT_IMG: &[u8] = include_bytes!("../imgs/fat12.img");
//!
//! fn main() {
//!     let mut cursor = std::io::Cursor::new(FAT_IMG.to_owned());
//!
//!     // We can either pass by value of by (mutable) reference
//!     let mut fs = FileSystem::from_ro_storage(&mut cursor).unwrap();
//!
//!     // Let's see what entries there are in the root directory
//!     for entry in fs.read_dir(PathBuf::from("/")).unwrap() {
//!         if entry.is_dir() {
//!             println!("Directory: {}", entry.path())
//!         } else if entry.is_file() {
//!             println!("File: {}", entry.path())
//!         } else {
//!             unreachable!()
//!         }
//!     }
//!
//!     // the image we currently use has a file named "root.txt"
//!     // in the root directory. Let's read it
//!
//!     // please keep in mind that opening a `ROFile` or `RWFile` borrows
//!     // the parent `FileSystem` until that `ROFile` or `RWFile` is dropped
//!     let mut file = fs.get_ro_file(PathBuf::from("/root.txt")).unwrap();
//!     let mut string = String::new();
//!     file.read_to_string(&mut string);
//!     println!("root.txt contents:\n{}", string);
//! }
//! ```

#![cfg_attr(not(feature = "std"), no_std)]
// Even inside unsafe functions, we must acknowlegde the usage of unsafe code
#![deny(deprecated)]
#![deny(macro_use_extern_crate)]
#![deny(private_bounds)]
#![deny(private_interfaces)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(non_ascii_idents)]
#![warn(trivial_numeric_casts)]
#![warn(single_use_lifetimes)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
// clippy attributes
#![warn(clippy::redundant_clone)]

extern crate alloc;

mod error;
mod fat;
pub mod io;
mod path;
mod time;
mod utils;

pub use error::*;
pub use fat::*;
pub use path::*;
pub use time::*;
