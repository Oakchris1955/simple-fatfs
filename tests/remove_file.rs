mod common;
use common::*;

use test_log::test;

#[test]
fn remove_root_dir_file() {
    let mut storage = MemoryDevice::from(FAT16);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    // the bee movie script (here) is in the root directory region
    let file_path = "/bee movie script.txt";
    let file = fs.get_rw_file(file_path).unwrap();
    file.remove().unwrap();

    // the file should now be gone
    let file_result = fs.get_ro_file(file_path);
    match file_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("file should have been deleted by now"),
    }
}

#[test]
fn remove_data_region_file() {
    let mut storage = MemoryDevice::from(FAT12);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    // the bee movie script (here) is in the data region
    let file_path = "/test/bee movie script.txt";
    let file = fs.get_rw_file(file_path).unwrap();
    file.remove().unwrap();

    // the file should now be gone
    let file_result = fs.get_ro_file(file_path);
    match file_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("file should have been deleted by now"),
    }
}

#[test]
fn attempt_to_remove_file_as_directory() {
    let mut storage = MemoryDevice::from(FAT32);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/hello.txt";

    let fs_result = fs.remove_dir_all(dir_path);

    match fs_result {
        Err(err) => match err {
            FSError::NotADirectory => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("the filesystem struct should have detected that this isn't a directory"),
    }
}
