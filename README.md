# simple-fatfs

[![CI Status](https://github.com/Oakchris1955/simple-fatfs/actions/workflows/test.yml/badge.svg)](https://github.com/Oakchris1955/simple-fatfs/actions/workflows/test.yml)
![GitHub License](https://img.shields.io/github/license/Oakchris1955/simple-fatfs?color=blue)
[![Crates.io Version](https://img.shields.io/crates/v/simple-fatfs)](https://crates.io/crates/simple-fatfs)
[![docs.rs](https://docs.rs/simple-fatfs/badge.svg)](https://docs.rs/simple-fatfs)
![Crates.io MSRV](https://img.shields.io/crates/msrv/simple-fatfs)

A simple-to-use filesystem driver for the File Allocation Table (FAT)

## Motive

Apart from [rafalh's rust-fatfs] library, there aren't actually any other FAT filesystem drivers in [crates.io]. All the other libraries either support only FAT16/32, aren't being actively developed or are just bindings to some C library.

Another thing I found somewhat frustrating about [rafalh's rust-fatfs] (which ultimately led to my decision of creating this project) is the fact that his library isn't suitable for embedded Rust, since it requires implementing [some weird kind of buffered Read/Write](https://github.com/rafalh/rust-fatfs/issues/94), while it is also worth mentioning that the [crates.io] version of his library is somewhat outdated (there have been 144 [additional commits](https://github.com/rafalh/rust-fatfs/compare/v0.3.6...master) as of the time I'm writing this).

## Intent

A fully-working FAT driver that covers the following criteria:

- An easy-to-use public API for developers
- Avoids unnecessary/overbloated dependencies (I am talking about [leftpad](https://www.npmjs.com/package/left-pad)-like dependencies)
- `#[no_std]` support
- Uses [`embedded-io`](https://crates.io/crates/embedded-io) for IO operations, making it suitable for embedded devices
- FAT12/16/32/ExFAT support
- VFAT/LFN (long filename) support

It also aims to be able to do the following in the future:

- Allow low-level manipulation of a FAT filesystem (e.g. for checking if a file is continuous)
- Features enabling/disabling perhaps unnecessary features for certain use cases,
  allowing for usage in devices with limited flash memory / RAM

## TODO

- [x] FAT12 support (just handle entries between 2 sectors)
- [x] Distinguish between directories and files in paths (this must also be verified by the filesystem, just like in the `std`)
- [x] Check whether system endianness matters (FAT is little-endian)
    PS: it does in fact matter. [bincode](https://crates.io/crates/bincode), which we use for (de)serialization allows us to configure the default endianess
- [ ] Handle non-printable characters in names of files and directories
- [ ] ExFAT support
- [x] replace custom `io` implementation with the [embedded-io] crate
- [ ] use `from_utf16be` for decoding LFNs (`str_from_utf16_endian` [#116258](https://github.com/rust-lang/rust/issues/116258))
- [ ] handle duplicate file open, either by blocking or more preferably, by not allowing such behavior.
- [ ] the majority of codepages will end up being dead code for most users, use features for enabling/disabling them.

## Known issues

- While the library can support both little and big-endian systems,
  due to the `str_from_utf16_endian` feature being unstable, long filenames
  won't be properly decoded.

- Duplicate file opens or in general any write operation involving a file that
  is open either as R/W or RO could cause data corruption (see [#14](https://github.com/Oakchris1955/simple-fatfs/issues/14))

- Multi-byte codepages, such as the Japanese one (932) are currently unsupported.

## Acknowledgments

I'd like to thank [@alexkazik](https://github.com/alexkazik) for his contributions, most importantly his [short filename generation algorithm](https://github.com/Oakchris1955/simple-fatfs/pull/17) and his [Block-based device traits](https://github.com/Oakchris1955/simple-fatfs/pull/22).

This project adheres to [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) (since commit `21c7d6b`, that is excluding the first two commits which don't actually contain any code). It also uses [git-cliff](https://github.com/orhun/git-cliff) to parse commit messages into a `CHANGELOG`

## License

[MIT](LICENSE)

[crates.io]: https://crates.io
[rafalh's rust-fatfs]: https://github.com/rafalh/rust-fatfs
[embedded-io]: https://crates.io/crates/embedded-io
