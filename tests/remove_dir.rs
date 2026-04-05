mod common;
use common::*;

use test_log::test;

#[test]
fn remove_empty_dir() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/another root directory/";

    fs.remove_empty_dir(dir_path).unwrap();

    // the directory should now be gone
    let dir_result = fs.read_dir(dir_path);
    match dir_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("the directory should have been deleted by now"),
    }
}

#[test]
fn remove_nonempty_dir_with_readonly_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/rootdir/";

    // the directory should contain a read-only file (example.txt)
    let del_result = fs.remove_dir_all(dir_path);
    match del_result {
        Err(err) => match err {
            FSError::ReadOnlyFile => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("the directory shouldn't have been removed already"),
    }

    // this should now remove the directory
    fs.remove_dir_all_unchecked(dir_path).unwrap();

    // the directory should now be gone
    let dir_result = fs.read_dir(dir_path);
    match dir_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("the directory should have been deleted by now"),
    }
}
