mod common;
use common::*;

use test_log::test;

#[test]
fn rename_root_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/root.txt", "/rootdir/not root.txt").unwrap();

    let mut file = fs.get_ro_file("/rootdir/not root.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn rename_nonroot_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/rootdir/example.txt", "/another root directory/hello.txt")
        .unwrap();

    let mut file = fs.get_ro_file("/another root directory/hello.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn rename_root_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/hello.txt", "/emptydir/bye.txt").unwrap();

    let mut file = fs.get_ro_file("/emptydir/bye.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "Hello from a FAT32 filesystem!!!\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn rename_nonroot_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/secret/bee movie script.txt", "/BEES.txt")
        .unwrap();

    let mut file = fs.get_ro_file("/BEES.txt").unwrap();

    assert_file_is_bee_movie_script(&mut file);
}
