mod common;
use common::*;

use rstest::*;
use rstest_reuse::*;
pub use test_log::test;

#[test]
#[apply(fs)]
fn read_only_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
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
#[apply(fs)]
fn get_hidden_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
    let file_path = "/hidden/hidden.txt";
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
