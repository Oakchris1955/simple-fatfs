mod common;
use common::*;

use test_log::test;

#[test]
fn volume_label_bpb_correct1() {
    let mut storage = MemoryDevice::from(FAT16);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), Some(String::from("SIMPLEFATFS")))
}

#[test]
fn volume_label_bpb_correct2() {
    let mut storage = MemoryDevice::from(MINFS);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), Some(String::from("TEST FS")))
}

#[test]
fn volume_label_bpb_none() {
    let mut storage = MemoryDevice::from(FAT32);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), None)
}

#[test]
fn volume_label_root_none() {
    let mut storage = MemoryDevice::from(FAT32);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert!(fs
        .volume_label_root_dir()
        .is_ok_and(|label| label.is_none()))
}

#[test]
fn volume_label_root_correct() {
    let mut storage = MemoryDevice::from(MINFS);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(
        fs.volume_label_root_dir().unwrap(),
        Some(String::from("TEST FS"))
    )
}

#[test]
fn set_volume_label_bpb() {
    let mut storage = MemoryDevice::from(FAT32);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.set_volume_label_bpb("DEADBEEF");

    drop(fs);

    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), Some(String::from("DEADBEEF")));
}

#[test]
fn set_volume_label_root_dir() {
    let mut storage = MemoryDevice::from(FAT32);
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.set_volume_label_root_dir("DEADBEEF").unwrap();

    drop(fs);

    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert_eq!(
        fs.volume_label_root_dir().unwrap(),
        Some(String::from("DEADBEEF"))
    );
}
