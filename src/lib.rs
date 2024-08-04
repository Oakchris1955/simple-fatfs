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
//! # Examples
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
//!     let mut fs = FileSystem::from_storage(&mut cursor).unwrap();
//!
//!     // Let's see what entries are in the root directory
//!     for entry in fs.read_dir(PathBuf::from("/")).unwrap() {
//!         if entry.path().is_dir() {
//!             println!("Directory: {}", entry.path())
//!         } else if entry.path().is_file() {
//!             println!("File: {}", entry.path())
//!         } else {
//!             unreachable!()
//!         }
//!     }
//!
//!     // the image we currently use has a file named "root.txt"
//!     // in the root directory. Let's read it
//!
//!     // please keep in mind that opening a `File` borrows
//!     // the parent `FileSystem` until that `File` is dropped
//!     let mut file = fs.get_file(PathBuf::from("/root.txt")).unwrap();
//!     let mut string = String::new();
//!     file.read_to_string(&mut string);
//!     println!("root.txt contents:\n{}", string);
//! }
//! ```

#![cfg_attr(not(feature = "std"), no_std)]
// Even inside unsafe functions, we must acknowlegde the usage of unsafe code
#![deny(deprecated)]
#![deny(elided_lifetimes_in_paths)]
#![deny(macro_use_extern_crate)]
#![deny(missing_copy_implementations)]
#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(non_ascii_idents)]
#![deny(trivial_numeric_casts)]
#![deny(single_use_lifetimes)]
#![deny(unsafe_op_in_unsafe_fn)]
#![deny(unused_crate_dependencies)]
#![deny(unused_extern_crates)]
#![deny(unused_import_braces)]
#![deny(unused_lifetimes)]

extern crate alloc;

mod error;
mod fs;
pub mod io;
mod path;

pub use error::*;
pub use fs::*;
pub use path::*;
