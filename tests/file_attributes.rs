mod common;
use common::*;

pub use test_log::test;

#[test]
fn read_only_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let file_result = fs.get_rw_file("/rootdir/example.txt");

    match file_result {
        Err(err) => match err {
            FSError::ReadOnlyFile => (),
            _ => panic!("unexpected IOError"),
        },
        _ => panic!("file is marked read-only, yet somehow we got a RWFile for it"),
    }
}

#[test]
fn get_hidden_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT12.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let file_path = "/hidden";
    {
        let file_result = fs.get_ro_file(file_path);
        match file_result {
            Err(err) => match err {
                FSError::NotFound => (),
                _ => panic!("unexpected IOError"),
            },
            _ => panic!("file should be hidden by default"),
        }
    }

    {
        // let's now allow the filesystem to list hidden files
        fs.show_hidden(true);
        let file = fs.get_ro_file(file_path).unwrap();
        assert!(file.attributes().hidden);
    }
}
