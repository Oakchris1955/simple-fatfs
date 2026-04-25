mod common;
use common::*;

use embedded_io::*;

use rstest::*;
use rstest_reuse::*;
use test_log::test;

#[test]
#[apply(fs)]
fn rename_root_directory(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    fs.rename("/rootdir", "/rootdir2").unwrap();

    let mut file = fs.get_ro_file("/rootdir2/example.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
#[apply(fs)]
fn rename_root_directory_fat32(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    fs.rename("/subdir", "/emptydir/secret").unwrap();

    let mut file = fs
        .get_ro_file("/emptydir/secret/bee movie script.txt")
        .unwrap();

    assert_file_is_bee_movie_script(&mut file);
}
