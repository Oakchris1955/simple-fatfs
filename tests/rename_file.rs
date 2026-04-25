mod common;
use common::*;

use embedded_io::*;

use rstest::*;
use rstest_reuse::*;
use test_log::test;

#[test]
#[apply(fs)]
fn rename_root_file(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    fs.rename("/root.txt", "/rootdir/not root.txt").unwrap();

    let mut file = fs.get_ro_file("/rootdir/not root.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\nbottom text\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
#[apply(fs)]
fn rename_nonroot_file(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    fs.rename("/rootdir/example.txt", "/subdir/hello.txt")
        .unwrap();

    let mut file = fs.get_ro_file("/subdir/hello.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n";
    assert_eq!(file_string, EXPECTED_STR);
}
