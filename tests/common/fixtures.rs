use rstest_reuse::*;

#[template]
#[rstest]
#[case(device(FAT12))]
#[case(device(FAT16))]
#[case(device(FAT32))]
pub fn device(#[case] device: MemoryDevice) {}

#[allow(unused)]
#[template]
#[rstest]
#[case(fat12_fs())]
#[case(fat16_fs())]
#[case(fat32_fs())]
pub fn fs(#[case] fs: FS) {}
