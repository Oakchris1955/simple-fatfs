mod common;
use common::*;

use test_log::test;

#[test]
fn check_last_accessed_ro() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    // read some data
    let mut target = [0; 42];
    file.read(&mut target).unwrap();

    drop(file);

    let file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    assert_ne!(&Some(DefaultClock.now().date()), file.last_accessed_date());
}

#[test]
fn check_last_accessed_rw() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new().with_update_file_fields(true)).unwrap();

    let mut file = fs.get_rw_file("/bee movie script.txt").unwrap();

    // read some data
    let mut target = [0; 42];
    file.read(&mut target).unwrap();

    drop(file);

    let file = fs.get_ro_file("/bee movie script.txt").unwrap();

    assert_eq!(&Some(DefaultClock.now().date()), file.last_accessed_date());
}

#[test]
fn check_last_modified() {
    use ::time::Duration;

    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new().with_update_file_fields(true)).unwrap();

    let mut file = fs.get_rw_file("/bee movie script.txt").unwrap();

    // just some random data
    file.write(&[49, 65, 47]).unwrap();

    drop(file);

    let file = fs.get_ro_file("/bee movie script.txt").unwrap();

    assert_eq!(&Some(DefaultClock.now().date()), file.last_accessed_date());
    // I find it highly unlikely that this test won't have been completed within 15 seconds
    assert!(DefaultClock.now() - *file.modification_time() < Duration::seconds(15));
}

#[test]
fn check_file_timestamps() {
    use ::time::macros::*;

    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    assert_eq!(
        Some(datetime!(2024-07-11 13:02:38.15)),
        *file.creation_time()
    );
    assert_eq!(datetime!(2024-07-11 13:02:38.0), *file.modification_time());
    assert_eq!(Some(date!(2024 - 07 - 11)), *file.last_accessed_date());
}

#[test]
fn modify_file_timestamps() {
    use ::time::macros::*;

    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_rw_file("/bee movie script.txt").unwrap();

    // back to the future we go
    file.set_accessed(date!(1985 - 07 - 3));

    drop(file);

    let file = fs.get_ro_file("/bee movie script.txt").unwrap();

    assert_eq!(&Some(date!(1985 - 07 - 3)), file.last_accessed_date());
}
