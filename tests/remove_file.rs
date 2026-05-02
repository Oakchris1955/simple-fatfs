mod common;
use common::*;

use rstest::*;
use rstest_reuse::*;
use test_log::test;

#[test]
#[apply(fs)]

fn remove_root_dir_file(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    // the "I don't need a bagde" file is in the root directory region
    let file_path = "/I don't need a badge.txt";
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
#[apply(fs)]
fn remove_data_region_file(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    // the bee movie script  is in the data region
    let file_path = "/subdir/bee movie script.txt";
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
#[apply(fs)]
fn attempt_to_remove_file_as_directory(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    let target_path = "/hello 🗺️.txt";

    let fs_result = fs.remove_dir_all(target_path);

    match fs_result {
        Err(err) => match err {
            FSError::NotADirectory => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("the filesystem struct should have detected that this isn't a directory"),
    }
}
