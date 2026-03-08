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
- FAT12/16/32/ExFAT support
- VFAT/LFN (long filename) support

It also aims to be able to do the following in the future:

- Allow low-level manipulation of a FAT filesystem (e.g. for checking if a file is continuous)
- Features enabling/disabling perhaps unnecessary features for certain use cases,
  allowing for usage in devices with limited flash memory / RAM

## TODO

- [ ] Handle non-printable characters in names of files and directories
- [ ] ExFAT support
- [ ] handle duplicate file open, either by blocking or more preferably, by not allowing such behavior.
- [ ] proper codepage logic (support for multi-byte codepages, allow the user to use their own if it isn't already supported, etc.)
- [ ] proper testing suite (the current `tests.rs` file is a mess)

## Known issues

- Duplicate file opens or in general any write operation involving a file that
  is open either as R/W or RO could cause data corruption (see [#14](https://github.com/Oakchris1955/simple-fatfs/issues/14))

- Multi-byte codepages, such as the Japanese one (932) are currently unsupported.

## Acknowledgments

I'd like to thank [@alexkazik](https://github.com/alexkazik) for his contributions, most importantly his [short filename generation algorithm](https://github.com/Oakchris1955/simple-fatfs/pull/17) and his [Block-based device traits](https://github.com/Oakchris1955/simple-fatfs/pull/22).

I'd also like to thank [@lukaslihotzki] for [migrating](https://github.com/Oakchris1955/simple-fatfs/pull/38) the library away from the now-abandonded bincode to [zerocopy] and [making](https://github.com/Oakchris1955/simple-fatfs/pull/39) simple-fatfs endian-agnostic.

This project adheres to [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) (since commit `21c7d6b`, that is excluding the first two commits which don't actually contain any code). It also uses [git-cliff](https://github.com/orhun/git-cliff) to parse commit messages into a `CHANGELOG`

## License

[MIT](LICENSE)

[crates.io]: https://crates.io
[rafalh's rust-fatfs]: https://github.com/rafalh/rust-fatfs
[@lukaslihotzki]: https://github.com/@lukaslihotzki
[zerocopy]: https://crates.io/crates/zerocopy
