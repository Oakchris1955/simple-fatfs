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
//! ## Usage
//!
//! The library uses [`embedded-io`](embedded_io) for IO operations.
//! Most notably, the storage medium is expected to implement at least the
//! [`Read`] and [`Seek`] traits (RO storage), while [`Write`] is optional
//! (R/W storage). Furthermore, [`ROFile`] & [`RWFile`] both implement [`Read`]
//! & [`Seek`], while [`RWFile`] also implements [`Write`]
//!
//! To use [`std::io`]'s respective traits, use the
//! [`embedded-io-adapters`](https://crates.io/crates/embedded-io-adapters) crate.
//!
//! ## Examples
//! ```
//! # // this test fails on a no_std environment, don't run it in such a case
//! extern crate simple_fatfs;
//! use simple_fatfs::*;
//! use simple_fatfs::io::*;
//! use simple_fatfs::block_io::FromStd;
//!
//! const FAT_IMG: &[u8] = include_bytes!("../imgs/fat12.img");
//!
//! fn main() {
//!     let mut cursor = FromStd::new(std::io::Cursor::new(FAT_IMG.to_owned())).unwrap();
//!
//!     // We can either pass by value or by (mutable) reference
//!     // (Yes, the storage medium might be Read-Only, but reading is a mutable action)
//!     let mut fs = FileSystem::new(&mut cursor, FSOptions::new()).unwrap();
//!
//!     // Let's see what entries there are in the root directory
//!     for entry in fs.read_dir("/").unwrap() {
//!         // in a real world example, you probably don't wanna unwrap this
//!         let entry = entry.unwrap();
//!
//!         if entry.is_dir() {
//!             println!("Directory: {}", entry.path())
//!         } else if entry.is_file() {
//!             println!("File: {}", entry.path())
//!         } else {
//!             unreachable!()
//!         }
//!     }
//!
//!     // the disk image we currently use has a file named "root.txt"
//!     // in the root directory. Let's read it
//!     let mut file = fs.get_ro_file("/root.txt").unwrap();
//!     let mut file_buf = vec![0; file.file_size() as usize];
//!     file.read_exact(&mut file_buf).unwrap();
//!     let string = str::from_utf8(&file_buf).unwrap();
//!     println!("root.txt contents:\n{}", string);
//! }
//! ```
//!
//! ## Notes
//!
//! - **Regarding volume labels**
//!
//!   For historic reasons FAT label is stored in two different
//!   locations: in the boot sector and as a special volume label entry
//!   in the root directory. Windows read the FAT label only from the root
//!   directory (and only update that). Keep that in mind when using the
//!   various volume label functions
//!
//!   For more info, please check <https://man7.org/linux/man-pages/man8/fatlabel.8.html>
//!
//! [`Read`]: io::Read
//! [`Seek`]: io::Seek
//! [`Write`]: io::Write

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
#![warn(clippy::absurd_extreme_comparisons)] // who thought this was a good idea for a deny lint?
#![warn(clippy::derive_partial_eq_without_eq)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::cast_possible_truncation)]
#![warn(clippy::cast_possible_wrap)]
#![warn(clippy::cast_precision_loss)]
#![warn(clippy::cast_sign_loss)]
#![warn(clippy::redundant_clone)]

#[cfg(target_pointer_width = "16")]
compile_error!(
    concat!(
        "For various reasons, this project has been designed ",
        "around architectures with at least 32-bit pointer widths\n",
        "If you believe this isn't right, file an issue at https://github.com/Oakchris1955/simple-fatfs"
    )
);

extern crate alloc;

mod codepage;
mod error;
mod fat;
mod path;
mod time;
mod utils;

pub use codepage::*;
pub use embedded_io as io;
pub use error::*;
pub use fat::*;
pub use path::*;
pub use time::*;
