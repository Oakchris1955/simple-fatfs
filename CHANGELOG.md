# Changelog

All notable changes to this project will be documented in this file.

This changelog is automatically updated weekly by a cron job

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0-alpha.2] - 2025-09-14

### Added

- Implement Write for File & Seek beyond EOF allocates more clusters ([f79db1f](f79db1f54ea91f0a070659ea492a85792c400aa3))
- Add `remove()` method to RWFile struct ([f88942b](f88942b988d2b0d8dcffa913b28828a6a8db40ca))
- Create alias method `remove_file` ([dbeaf4b](dbeaf4bdb1e4af1d8fe04597635f5612c3db3a07))
- Add a proper unmount method ([7803ab2](7803ab239a5ce7c731b17e6d70cc88d919b06ddd))
- Add the ability to remove non-empty directories ([faf1be2](faf1be26143cedc2507edb1f1052b4e84e1bd5d4))
- Implement the ability to create files ([58b236e](58b236e47044bacd2ae22b1d122a4525083eed46))
- Implement the ability to create directories ([d515358](d5153585aa952131b3517bded57cc66cd38ab957))
- Implement the ability to rename files and directories ([96d30e7](96d30e7b14342e97b28b5a67fad49c5cfc57c2de))
- Proper RO and R/W storage support ([d2490ec](d2490ec65b2d93848699ad94e4c9599e93165ece))
- Codepage support ([f9d2338](f9d23384b143ceaf93539be778627970313a81bb))

### Changed

- Implement basic logging ([c849a6b](c849a6b256ef4848be0b09fe805b0ec262371b5f))
- Public function to truncate a File down to a given size ([6bd3534](6bd35347ab887c679ec410933ababa0d3222be56))
- Split File struct into separate ROFile & RWFile ([20adcf0](20adcf0d8f55f0250ebc2849b622de5e9071c51c))
- Add a Clock trait that will be used for generating file timestamps ([61c83ed](61c83eda8ecdbebc780e78228db521ebaf816f34))
- Add the ability to remove empty directories ([63b0f9b](63b0f9b08f3071abcd9f56f978a075bfc930abfb))
- Directories will now be defragmented when filled with entries ([cb3e2dc](cb3e2dc1b62bb7be12440c4eb0b9656408a5fd29))
- Mount options struct ([6fac63f](6fac63f24b1fbce637f65022279faca33384e6d0))
- Option field to update file time fields ([1396a4c](1396a4c063665aca9ad106793500ca604ca23878))

### Fixed

- Fix potential bug when File read stops at the end of a sector ([6eb9226](6eb922610fa5b1c76c61f7e20ff56039e310991b))
- Seeking on a File wouldn't always happen correctly ([9775f2a](9775f2a4ad3315e31d9ccd8fd1a63441114e0dd9))
- Also write FAT entries to the FAT table copies ([a8ced4c](a8ced4c0fff7fc76b7ba5334208cce81179cdf7b))
- Correctly implement file attributes ([f427f9e](f427f9ef0aa4acece9790680240c3ef4741ea038))
- Correctly parse 8.3 & LFN entries that span multiple sectors ([21d83d6](21d83d667ebf3f1b4753a14e74495089c6458d96))
- `truncate()` now works if the new size is close to the old one ([a60e55b](a60e55b933881b23be0876dde587298efed0975f))
- Various fat32-related bug fixes ([d8a5cfd](d8a5cfd6c703a170bd5cdb41d7f89a8a7cac72bc))
- Properly handle FATs ([b891cc0](b891cc03dc2dfbf3bec4571b47a8cc00f78286ed))
- Correctly handle calling RW methods on RO storage mediums ([66b1f78](66b1f780f18ffe803b322e6b20817ce29b0b7c1f))
- PathBuf's `.parent()` method wouldn't behave as expected ([7a2bc0c](7a2bc0ce4f0c218f70c4d4d9f9b409bb20f1fedf))
- Don't expose the `.` & `..` entries to the end user ([02eb213](02eb2132aca8d7814fdaacbddc8b8373e110f0b7))
- Sync the FSInfo struct on FAT32 filesystems ([d19a04e](d19a04e2e1a8bbded115efe14aa40a58b77aee71))
- Correctly navigate directories ([f21088e](f21088e66f277f9992cac4293f87718f43caf2c7))
- Prevent unnecessary LFN creation ([cb59a2e](cb59a2ed51a33b3737fedbc4aea54b38800b8961))
- Correctly handle time and dates ([3d89c0d](3d89c0dc69b2553fcadb6b78ee27f8f5c99b5c06))
- Proper RO-R/W method separation and handling ([0828f0d](0828f0d64e8badd2250cbeb47b48593aee3c68e2))
- `EntryCreationTime` was being parsed incorrectly ([d1c4f05](d1c4f057d26d1836ba9a68bdbde72a6f9e335188))
- Incorrect `DirEntry` cluster allocation ([5da6ccf](5da6ccfb148163df76f13454f656bc164edfbd47))
- The entire CreationTime field is optional, not just it's subfields ([a1644d7](a1644d73820db6a9b4411e5ac854da82564f3db5))
- Directory entries spanning multiple sectors now work correctly ([30ed5ac](30ed5ac440d0fa30bc3942ced1988b22ca053934))
- Short filenames no longer contain capitalized letters ([77207b0](77207b0779354b2d4fa2e52eb08b9e6b1f811843))
- Directory-related operations are now memory-friendly ([a78cdf5](a78cdf5b8e90405b2dd653a550e174dbd1f96697))
- Properly error out if the storage medium is full ([471283d](471283de907469f5980b20c289f682807c5d5263))
- Prevent creation of files/dirs with duplicate names ([71b3d2b](71b3d2b8399d02ab95f8d53381ac42491183d543))
- Correctly check if a short filename already exists ([56a16ce](56a16ce1872e19e38e9075a2dc3b44e26b83c13b))
- Incorrect recursion call ([eff23ea](eff23ea9ab4b814ca8ca103e09874b6d7a5f6ffa))
- Properly error out for ExFAT ([5a0756f](5a0756fe527763abb370d27d7cd08278889e93c8))
- Internal functions `allocate_clusters` & `allocate_nth_entries` didn't work as expected ([bfaa13d](bfaa13d3908156665b9f99790c4c3efbb9e4fde7))

## [0.1.0-alpha.1] - 2024-08-04

### Added

- Add basic filesystem functionality ([a565da4](a565da4af6e11571bd2e2cd6f1072085630f9c63))
- Implement checksum validation for LFNs ([236db1b](236db1b97af7c4f8a4555263d6477f2de918e33d))
- Implement sector caching ([7a5a618](7a5a618218ba8a03076ce92332c77865ce2f9c72))
- FAT12 support!!! ([6460079](646007928cacac6dd8112e0d8896fcd708673d23))
- Create new InternalFSError enum ([88a99a3](88a99a32281726c27fb027bf425b102741473c2c))

### Changed

- Use "time" crate for date & time handling ([b934c7b](b934c7b1db974cc07c730e1f508842918a3a9138))
- Pushing an absolute path replaces destination pathbuf ([278e60f](278e60f73977cdfd28fe263b7720508f98bd762d))
- IOError now have an IOErrorKind ([4ac6a95](4ac6a95424884e8a775a36590788a0897cfbba8d))
- In the Read trait, read_exact is now auto-implemented ([f9ca087](f9ca0873d58696c8244e9df6874d784922b1ab04))
- Correctly implement Read + Seek for File ([dd2823d](dd2823deff32a78a62f20bebc7c135ff42eb1502))
- Add a bunch of default implementations & make documentation more clear ([72cd1bd](72cd1bd6d38ebc20861bd078ab9115cf5545d4a0))

### Fixed

- Correctly handle forbidden/reserved filenames ([16b14d6](16b14d6ea4429c28d180cbf8eff0cc6ca7eb60b1))
- Due to a bug in the code, files larger than 1 cluster wouldn't be read properly ([3116e9d](3116e9d9d8bc53acdd7eab720a1a9f6bc74ebfd7))
- Calling Read on a File would sometimes "loop" the same cluster over and over again ([49a67d1](49a67d11b84a233b6f53d86715b2454198d39459))
- Fix potential endianess issue when transmuting an array ([54962a1](54962a1d13f746a5194234ae89f3c3c2194b168a))

[0.1.0-alpha.2]: https://github.com/Oakchris1955/simple-fatfs/compare/v0.1.0-alpha.1..v0.1.0-alpha.2

[0.1.0-alpha.1]: https://github.com/Oakchris1955/simple-fatfs/tree/v0.1.0-alpha.1
<!-- generated by git-cliff -->
