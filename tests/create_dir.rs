mod common;
use common::*;

use test_log::test;

#[test]
fn create_directory_in_root_and_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/unbelievable").unwrap();
    let mut file = fs.create_file("/unbelievable/baby i am free.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
fn create_directory_in_subdir_and_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/another root directory2").unwrap();
    let mut file = fs
        .create_file(PathBuf::from(
            "/another root directory/bee movie script.txt",
        ))
        .unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn create_directory_in_root_and_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/unbelievable").unwrap();
    let mut file = fs.create_file("/unbelievable/baby i am free.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
fn create_directory_in_subdir_and_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/another root directory").unwrap();
    let mut file = fs
        .create_file(PathBuf::from(
            "/another root directory/bee movie script.txt",
        ))
        .unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_bee_movie_script(&mut file);
}
