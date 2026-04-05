mod common;
use common::*;

use test_log::test;

#[test]
fn volume_label_bpb_correct1() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), Some(String::from("SIMPLEFATFS")))
}

#[test]
fn volume_label_bpb_correct2() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(MINFS.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), Some(String::from("TEST FS")))
}

#[test]
fn volume_label_bpb_none() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), None)
}

#[test]
fn volume_label_root_none() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert!(fs
        .volume_label_root_dir()
        .is_ok_and(|label| label.is_none()))
}

#[test]
fn volume_label_root_correct() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(MINFS.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(
        fs.volume_label_root_dir().unwrap(),
        Some(String::from("TEST FS"))
    )
}

#[test]
fn set_volume_label_bpb() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.set_volume_label_bpb("DEADBEEF");

    drop(fs);

    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), Some(String::from("DEADBEEF")));
}

#[test]
fn set_volume_label_root_dir() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.set_volume_label_root_dir("DEADBEEF").unwrap();

    drop(fs);

    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(
        fs.volume_label_root_dir().unwrap(),
        Some(String::from("DEADBEEF"))
    );
}
