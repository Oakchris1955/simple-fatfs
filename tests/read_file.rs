mod common;
use common::*;

use embedded_io::*;

use rstest::*;
use rstest_reuse::{self, *};
use test_log::test;

#[test]
#[apply(fs)]
fn read_file_in_root_dir(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    let mut file = fs.get_ro_file("/root.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\nbottom text\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
#[apply(fs)]
fn read_huge_file(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    let mut file = fs.get_ro_file("/subdir/bee movie script.txt").unwrap();
    assert_file_is_bee_movie_script(&mut file);
}

#[test]
#[apply(fs)]
fn seek_n_read(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    let mut file = fs.get_ro_file("/copypasta.txt").unwrap();
    let mut file_bytes = [0_u8; 4096];

    // we first perform a forward seek...
    const EXPECTED_STR1: &str = "Linux is the kernel";
    file.seek(SeekFrom::Start(848)).unwrap();
    let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR1.len()]).unwrap();
    assert_eq!(
        String::from_utf8_lossy(&file_bytes[..bytes_read]),
        EXPECTED_STR1
    );

    // ...then a backward one
    const EXPECTED_STR2: &str = "What you're refering to as Linux, is in fact, GNU/Linux";
    file.seek(SeekFrom::Start(96)).unwrap();
    let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR2.len()]).unwrap();
    assert_eq!(
        String::from_utf8_lossy(&file_bytes[..bytes_read]),
        EXPECTED_STR2
    );
}

#[test]
#[apply(fs)]
fn read_file_in_subdir(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    let mut file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n";
    assert_eq!(file_string, EXPECTED_STR);
}
