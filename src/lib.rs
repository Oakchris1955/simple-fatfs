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
//!     // please keep in mind that opening a `ROFile` or `RWFile` borrows
//!     // the parent `FileSystem` until that `ROFile` or `RWFile` is dropped
//!     let mut file = fs.get_ro_file(PathBuf::from("/root.txt")).unwrap();
//!     let mut string = String::new();
//!     file.read_to_string(&mut string);
//!     println!("root.txt contents:\n{}", string);
//! }
//! ```
//!
//! ## **FAQ**
//! ### We have [`ROFile`] and [`RWFile`], why can't we also have `ROFileSystem` and `RWFileSystem`?
//!
//! *TL;DR:
//! this is not possible until [RFC 1210 \(specialization\)][`RFC 1210`]
//! [gets implemented](https://github.com/rust-lang/rust/issues/31844)
//! \(it doesn't look like it's going to happen anytime soon\)*
//!
//! One way to implement this would be to have `RWFileSystem`
//! `impl Deref(Mut) for ROFileSystem`.
//! However, we would soon stumble across a major issue.
//!
//! Internally, we use a `read_nth_sector` method that does exactly what it says,
//! but also sync any changes made to the previous sector back to the filesystem.
//! Even if we were to have two separate such methods, one for the R/O filesystem
//! that just reads a sector and another one for the R/W filesystem that also
//! syncs the previous sector, all methods within `ROFileSystem` would use the
//! one that doesn't sync any changes back to the storage medium. Thus, using
//! this method, [`Write`] functionality would become impossible.
//!
//! One solution would to have a different implementation of the `read_nth_sector`
//! method for `ROFileSystem`s for storage mediums that do and don't impl [`Write`].
//! That's only possible using [specialization][`RFC 1210`].
//!
//! If you happen to know a way to bypass this restriction,
//! [please open a new issue](https://github.com/Oakchris1955/simple-fatfs/issues)
//!
//! [`RFC 1210`]: https://github.com/rust-lang/rfcs/blob/master/text/1210-impl-specialization.md
//! [`Write`]: crate::io::Write

#![cfg_attr(not(feature = "std"), no_std)]
// Even inside unsafe functions, we must acknowlegde the usage of unsafe code
#![deny(deprecated)]
#![deny(macro_use_extern_crate)]
#![deny(missing_copy_implementations)]
#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(non_ascii_idents)]
#![deny(private_bounds)]
#![deny(private_interfaces)]
#![deny(trivial_numeric_casts)]
#![deny(single_use_lifetimes)]
#![deny(unsafe_op_in_unsafe_fn)]
#![deny(unused_import_braces)]
#![deny(unused_lifetimes)]
// clippy attributes
#![deny(clippy::redundant_clone)]

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
