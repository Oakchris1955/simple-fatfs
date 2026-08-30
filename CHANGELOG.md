# Changelog

All notable changes to this project will be documented in this file.

This changelog is automatically updated weekly by a cron job

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Error enums now derive PartialEq & Eq ([136e5d0](136e5d060ec4a3c708bba550177a37e1de6d9aa0))
- `defmt` logging support & slight rework of internal logging structure ([b42671e](b42671e8b7d7edccb16d40380c5550cd9b5edb3b))
- Function to determine filesystem sector size + test cases ([0616422](0616422e781e8c29b3e994ced1dfce768d2e1168))
- (optional) `embedded-storage` translators ([6c879e2](6c879e2ece0511b321c4a06e4bcc3d38b89724ba))
- Add note regarding volume labels ([5e7b321](5e7b3217e9f651cc6bd73bd80ee301c38165e5e2))
- Set volume label for root directory ([5438c1a](5438c1ab0550e73ae830d46d4d25579f6ee08194))
- Set volume label for boot record ([2b85a16](2b85a16e3b99a76d9795658edebcc27edf951ce4))
- Obtain volume label from root directory ([973e993](973e9932d6f08a3c9135e092694879bf06304429))
- Obtain volume label from BIOS parameter block ([132b5d8](132b5d893b24957e805bb3ef316fde4b63cbad99))
- Implement equivalence and orderng traits for `Properties` struct ([82b3c8a](82b3c8a814729271489924934855c1d3d8fd177e))
- Make bloom filter support optional (controlled by a feature flag) ([d729eb5](d729eb564590d7be276bb9a3b28d02d91821befb))
- Basic directory caching ([5e6e14d](5e6e14dd8452b977f5cb45d2c02b87afdebe5e9c))
- Expose BlockIndex ([335facf](335facfb1f31d16cf508b8a70fa87871c2183146))
- Block_size method, prevents unexpected EOF on malformed filesystems ([91c7751](91c77516f34d80475de6d18a1e3fcbe2390122ad))
- 64-bit logical block addressing ([eb65b22](eb65b22880347cb935680ac4b0f10bb359a7b22d))
- Block translator, will allow to write 512-4096 byte sectors (via e.g. 512 byte blocks) to e.g. flash with 64KiB blocks ([81478a9](81478a98b5e8f3b6a9e1da98cd00579043ac9adf))
- Access the disk though blocks ([30eca24](30eca24e7e64aed420487d45cdfa6b9b201e5fff))
- Build on platforms not supporting atomic loads ([bd92135](bd92135393326473fcd43867808d98663f373eb5))
- Provide method for custom clock in FSOptions ([e4603a6](e4603a6b87ab5fb3aa48eb2365272bcb50091401))
- Lfn buffer-related changes ([dc4fdda](dc4fdda34f67490180879c5caefb5a6e0ea56af5))
- Change clock from Boxed trait object to generic ([23e7bb0](23e7bb07422f8e2d5385d6690d26e4c402b743d1))
- Always enable cp437 and use it as the default ([ab5f46c](ab5f46c97ac35c3d97179c3a48ad871a50400324))
- Allow for selecting which codepage is available ([cb20cea](cb20cea21d892565e035c998a97fbf1096cfea89))
- Impl Default for Codepage ([cb08d8d](cb08d8d8b5c71b1ad70531eb8fcc4794c65fb82f))
- Prevent redundant copying of strings ([819e551](819e551ef9c4ef9ec36246f1b6e465d37da48fdd))
- Skip boot record signature check ([e309cbf](e309cbf55e4053cb4748e1b6ea978cd187319c54))

### Changed

- Do not `expect` `non_snake_case` lints & remove no-longer relevant comment ([c418019](c4180192045cd66a62a7bb9d4d8e6e7d286956e8))
- Use `.expect` instead of match statements, `assert!` instead of `unreachable!` and `.then` methods where possible ([e250e15](e250e15282b8ffe14e4028ef961406a0776a1609))
- Document all crate features ([7e1daf3](7e1daf34f86cf0f4a99b97a4f81d084d2b373a1d))

### Fixed

- Return `None` instead of continuing if root directory label was too big ([c228414](c2284142ac4e43231e66e3b5f7fbfecec8254e03))
- Hidden files are now taken into account when removing a directory ([80e8b71](80e8b71048557d65b16772edc017fe349bab464a))
- `bloom` feature now correctly compiles on `no-std` targets ([91f5ea7](91f5ea71b6488853d1c835a1dc4c5969f2f7aad5))
- Fix lib.rs example ([86c8c45](86c8c458f19ccc9e90b697c051828c0a9d4ebb39))
- Directories can no longer be fetched as files ([3368a21](3368a211c51f8a0c077762975d1651d25ba6c91a))
- R/W file wouldn't sometimes allocate space correctly or update the file size field ([1d957ba](1d957ba9077f3eeef4ba089e47961ee5dbf0bec1))
- `determine_fs_sector_size` for storage mediums smaller than 4k bytes ([a5b9ac4](a5b9ac4d43995dbed1f1e641af771d9606ea9a2b))
- Little endian bitfields ([38ab7fb](38ab7fb52bc0696e3c12dfdf9bbac2599afb6dca))
- Little endian conversions for file_size ([015d7e0](015d7e096ca2b5b7f589f1865443f7df7a8ddaec))
- Use ERASE_SIZE instead of READ_SIZE as block size for the MultiWriteNorFlashTranslator ([4092d16](4092d16dc70819e1c30493bd97f78995ee5b35e0))

### Chore

- Bump embedded_io to v0.7.1 & implement `core::error::Error` for Error structs/enums (**breaking**) ([76e14f4](76e14f4d6f2dfbbeed367b4df562bfe0df49ffcb))
- Only codepage 437 (OEM United States) is now enabled by default out of all codepages (**breaking**) ([7eb585f](7eb585f4ca7326b1f77c65ee45928407561d71d5))
- Trigger a compilation error on 16-bit targets (**breaking**) ([2ef49c3](2ef49c335b84e1c36d6b3338c391ecc79210184a))
- Block size is now a method, not a const generic (**breaking**) ([39c1a8a](39c1a8af89bf172f32545038b48ac79f4e1f97c9))

## [0.1.0-alpha.2] - 2025-09-14

### Added

- Use `embedded-io` for IO operations ([4799387](4799387b2beca680fb0c63a010ea217c540fb749))
- Option field to update file time fields ([1396a4c](1396a4c063665aca9ad106793500ca604ca23878))
- Codepage support ([f9d2338](f9d23384b143ceaf93539be778627970313a81bb))
- Mount options struct ([6fac63f](6fac63f24b1fbce637f65022279faca33384e6d0))
- Directories will now be defragmented when filled with entries ([cb3e2dc](cb3e2dc1b62bb7be12440c4eb0b9656408a5fd29))
- Proper RO and R/W storage support ([d2490ec](d2490ec65b2d93848699ad94e4c9599e93165ece))
- Implement the ability to rename files and directories ([96d30e7](96d30e7b14342e97b28b5a67fad49c5cfc57c2de))
- Implement the ability to create directories ([d515358](d5153585aa952131b3517bded57cc66cd38ab957))
- Implement the ability to create files ([58b236e](58b236e47044bacd2ae22b1d122a4525083eed46))
- Add the ability to remove non-empty directories ([faf1be2](faf1be26143cedc2507edb1f1052b4e84e1bd5d4))
- Add a proper unmount method ([7803ab2](7803ab239a5ce7c731b17e6d70cc88d919b06ddd))
- Add the ability to remove empty directories ([63b0f9b](63b0f9b08f3071abcd9f56f978a075bfc930abfb))
- Create alias method `remove_file` ([dbeaf4b](dbeaf4bdb1e4af1d8fe04597635f5612c3db3a07))
- Add a Clock trait that will be used for generating file timestamps ([61c83ed](61c83eda8ecdbebc780e78228db521ebaf816f34))
- Add `remove()` method to RWFile struct ([f88942b](f88942b988d2b0d8dcffa913b28828a6a8db40ca))
- Split File struct into separate ROFile & RWFile ([20adcf0](20adcf0d8f55f0250ebc2849b622de5e9071c51c))
- Public function to truncate a File down to a given size ([6bd3534](6bd35347ab887c679ec410933ababa0d3222be56))
- Implement Write for File & Seek beyond EOF allocates more clusters ([f79db1f](f79db1f54ea91f0a070659ea492a85792c400aa3))
- Implement basic logging ([c849a6b](c849a6b256ef4848be0b09fe805b0ec262371b5f))

### Fixed

- Truncate now truncates to the current cursor position ([2882445](28824451e27238798ed896a8e4fc05dfacdecab3))
- Internal functions `allocate_clusters` & `allocate_nth_entries` didn't work as expected ([bfaa13d](bfaa13d3908156665b9f99790c4c3efbb9e4fde7))
- Properly error out for ExFAT ([5a0756f](5a0756fe527763abb370d27d7cd08278889e93c8))
- Incorrect recursion call ([eff23ea](eff23ea9ab4b814ca8ca103e09874b6d7a5f6ffa))
- Correctly check if a short filename already exists ([56a16ce](56a16ce1872e19e38e9075a2dc3b44e26b83c13b))
- Prevent creation of files/dirs with duplicate names ([71b3d2b](71b3d2b8399d02ab95f8d53381ac42491183d543))
- Properly error out if the storage medium is full ([471283d](471283de907469f5980b20c289f682807c5d5263))
- Directory-related operations are now memory-friendly ([a78cdf5](a78cdf5b8e90405b2dd653a550e174dbd1f96697))
- Short filenames no longer contain capitalized letters ([77207b0](77207b0779354b2d4fa2e52eb08b9e6b1f811843))
- Directory entries spanning multiple sectors now work correctly ([30ed5ac](30ed5ac440d0fa30bc3942ced1988b22ca053934))
- The entire CreationTime field is optional, not just it's subfields ([a1644d7](a1644d73820db6a9b4411e5ac854da82564f3db5))
- Incorrect `DirEntry` cluster allocation ([5da6ccf](5da6ccfb148163df76f13454f656bc164edfbd47))
- `EntryCreationTime` was being parsed incorrectly ([d1c4f05](d1c4f057d26d1836ba9a68bdbde72a6f9e335188))
- Proper RO-R/W method separation and handling ([0828f0d](0828f0d64e8badd2250cbeb47b48593aee3c68e2))
- Correctly handle time and dates ([3d89c0d](3d89c0dc69b2553fcadb6b78ee27f8f5c99b5c06))
- Prevent unnecessary LFN creation ([cb59a2e](cb59a2ed51a33b3737fedbc4aea54b38800b8961))
- Correctly navigate directories ([f21088e](f21088e66f277f9992cac4293f87718f43caf2c7))
- Sync the FSInfo struct on FAT32 filesystems ([d19a04e](d19a04e2e1a8bbded115efe14aa40a58b77aee71))
- Don't expose the `.` & `..` entries to the end user ([02eb213](02eb2132aca8d7814fdaacbddc8b8373e110f0b7))
- PathBuf's `.parent()` method wouldn't behave as expected ([7a2bc0c](7a2bc0ce4f0c218f70c4d4d9f9b409bb20f1fedf))
- Correctly handle calling RW methods on RO storage mediums ([66b1f78](66b1f780f18ffe803b322e6b20817ce29b0b7c1f))
- Properly handle FATs ([b891cc0](b891cc03dc2dfbf3bec4571b47a8cc00f78286ed))
- Various fat32-related bug fixes ([d8a5cfd](d8a5cfd6c703a170bd5cdb41d7f89a8a7cac72bc))
- `truncate()` now works if the new size is close to the old one ([a60e55b](a60e55b933881b23be0876dde587298efed0975f))
- Correctly parse 8.3 & LFN entries that span multiple sectors ([21d83d6](21d83d667ebf3f1b4753a14e74495089c6458d96))
- Correctly implement file attributes ([f427f9e](f427f9ef0aa4acece9790680240c3ef4741ea038))
- Also write FAT entries to the FAT table copies ([a8ced4c](a8ced4c0fff7fc76b7ba5334208cce81179cdf7b))
- Seeking on a File wouldn't always happen correctly ([9775f2a](9775f2a4ad3315e31d9ccd8fd1a63441114e0dd9))
- Fix potential bug when File read stops at the end of a sector ([6eb9226](6eb922610fa5b1c76c61f7e20ff56039e310991b))

## [0.1.0-alpha.1] - 2024-08-04

### Added

- Add a bunch of default implementations & make documentation more clear ([72cd1bd](72cd1bd6d38ebc20861bd078ab9115cf5545d4a0))
- Create new InternalFSError enum ([88a99a3](88a99a32281726c27fb027bf425b102741473c2c))
- FAT12 support!!! ([6460079](646007928cacac6dd8112e0d8896fcd708673d23))
- Correctly implement Read + Seek for File ([dd2823d](dd2823deff32a78a62f20bebc7c135ff42eb1502))
- In the Read trait, read_exact is now auto-implemented ([f9ca087](f9ca0873d58696c8244e9df6874d784922b1ab04))
- IOError now have an IOErrorKind ([4ac6a95](4ac6a95424884e8a775a36590788a0897cfbba8d))
- Pushing an absolute path replaces destination pathbuf ([278e60f](278e60f73977cdfd28fe263b7720508f98bd762d))
- Use "time" crate for date & time handling ([b934c7b](b934c7b1db974cc07c730e1f508842918a3a9138))
- Implement sector caching ([7a5a618](7a5a618218ba8a03076ce92332c77865ce2f9c72))
- Implement checksum validation for LFNs ([236db1b](236db1b97af7c4f8a4555263d6477f2de918e33d))
- Add basic filesystem functionality ([a565da4](a565da4af6e11571bd2e2cd6f1072085630f9c63))

### Fixed

- Fix potential endianess issue when transmuting an array ([54962a1](54962a1d13f746a5194234ae89f3c3c2194b168a))
- Calling Read on a File would sometimes "loop" the same cluster over and over again ([49a67d1](49a67d11b84a233b6f53d86715b2454198d39459))
- Due to a bug in the code, files larger than 1 cluster wouldn't be read properly ([3116e9d](3116e9d9d8bc53acdd7eab720a1a9f6bc74ebfd7))
- Correctly handle forbidden/reserved filenames ([16b14d6](16b14d6ea4429c28d180cbf8eff0cc6ca7eb60b1))

[Unreleased]: https://github.com/Oakchris1955/simple-fatfs/compare/v0.1.0-alpha.2..HEAD
[0.1.0-alpha.2]: https://github.com/Oakchris1955/simple-fatfs/compare/v0.1.0-alpha.1..v0.1.0-alpha.2

[0.1.0-alpha.1]: https://github.com/Oakchris1955/simple-fatfs/tree/v0.1.0-alpha.1
<!-- generated by git-cliff -->
