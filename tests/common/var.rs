use embedded_io::Read;

use crate::block_io::prelude::*;

pub fn device(storage: &[u8]) -> MemoryDevice {
    MemoryDevice::from(storage)
}

pub fn new_fs(storage: &[u8]) -> FileSystem<MemoryDevice, DefaultClock> {
    FileSystem::new(device(storage), FSOptions::new()).unwrap()
}

pub fn minfs() -> FileSystem<MemoryDevice, DefaultClock> {
    new_fs(MINFS)
}

pub fn fat12_fs() -> FileSystem<MemoryDevice, DefaultClock> {
    new_fs(FAT12)
}

pub fn fat16_fs() -> FileSystem<MemoryDevice, DefaultClock> {
    new_fs(FAT16)
}

pub fn fat32_fs() -> FileSystem<MemoryDevice, DefaultClock> {
    new_fs(FAT32)
}

pub static MINFS: &[u8] = include_bytes!("imgs/minfs.img");
pub static FAT12: &[u8] = include_bytes!("imgs/fat12.img");
pub static FAT16: &[u8] = include_bytes!("imgs/fat16.img");
pub static FAT32: &[u8] = include_bytes!("imgs/fat32.img");

pub static BEE_MOVIE_SCRIPT: &str = include_str!("imgs/structure/subdir/bee movie script.txt");
pub static I_DONT_NEED_A_BADGE: &str = include_str!("imgs/structure/I don't need a badge.txt");

pub fn assert_vec_is_bee_movie_script(buf: &[u8]) {
    assert_vec_is_string(buf, BEE_MOVIE_SCRIPT)
}

pub fn assert_file_is_bee_movie_script<S, C>(file: &mut ROFile<'_, S, C>)
where
    S: BlockWrite,
    C: Clock,
{
    assert_file_against_string(file, BEE_MOVIE_SCRIPT);
}

pub fn assert_file_is_i_dont_need_a_badge<S, C>(file: &mut ROFile<'_, S, C>)
where
    S: BlockWrite,
    C: Clock,
{
    assert_file_against_string(file, I_DONT_NEED_A_BADGE);
}

pub fn assert_vec_is_string(buf: &[u8], expected_string: &str) {
    let string = core::str::from_utf8(buf).unwrap();
    let expected_size = expected_string.len();
    assert_eq!(buf.len(), expected_size);

    assert_eq!(string, expected_string);
}
pub fn assert_file_against_string<S, C>(file: &mut ROFile<'_, S, C>, expected_string: &str)
where
    S: BlockWrite,
    C: Clock,
{
    let mut buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut buf).unwrap();

    assert_vec_is_string(&buf, expected_string);
}
