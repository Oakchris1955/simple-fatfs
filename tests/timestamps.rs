mod common;
use common::*;

use embedded_io::*;

use rstest::*;
use rstest_reuse::*;
use test_log::test;

#[test]
#[apply(fs)]
fn check_last_accessed_ro(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    let mut file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    // read some data
    let mut target = [0; 42];
    file.read(&mut target).unwrap();

    drop(file);

    let file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    assert_ne!(&Some(DefaultClock.now().date()), file.last_accessed_date());
}

#[test]
#[apply(device)]
fn check_last_accessed_rw(#[case] mut storage: MemoryDevice<Box<[u8]>>) {
    let fs = FileSystem::new(&mut storage, FSOptions::new().with_update_file_fields(true)).unwrap();

    let mut file = fs.get_rw_file("/I don't need a badge.txt").unwrap();

    // read some data
    let mut target = [0; 42];
    file.read(&mut target).unwrap();

    drop(file);

    let file = fs.get_ro_file("/I don't need a badge.txt").unwrap();

    assert_eq!(&Some(DefaultClock.now().date()), file.last_accessed_date());
}

#[test]
#[apply(device)]
fn check_last_modified(#[case] mut storage: MemoryDevice<Box<[u8]>>) {
    use ::time::Duration;

    let fs = FileSystem::new(&mut storage, FSOptions::new().with_update_file_fields(true)).unwrap();

    let mut file = fs.get_rw_file("/I don't need a badge.txt").unwrap();

    // just some random data
    file.write(&[49, 65, 47]).unwrap();

    drop(file);

    let file = fs.get_ro_file("/I don't need a badge.txt").unwrap();

    assert_eq!(&Some(DefaultClock.now().date()), file.last_accessed_date());
    // I find it highly unlikely that this test won't have been completed within 15 seconds
    assert!(DefaultClock.now() - *file.modification_time() < Duration::seconds(15));
}

use time::macros::*;

#[test]
#[rstest]
#[case(fat12_fs(), Some(datetime!(2026-04-12 14:19:12.32)), datetime!(2026-04-12 13:43:52.0),Some(date!(2026 - 04 - 12)))]
#[case(fat16_fs(), Some(datetime!(2026-04-15 16:48:20.29)), datetime!(2026-04-12 13:43:52.0),Some(date!(2026 - 04 - 12)))]
#[case(fat32_fs(), Some(datetime!(2026-04-12 14:19:35.38)), datetime!(2026-04-12 13:43:52.0),Some(date!(2026 - 04 - 12)))]
fn check_file_timestamps(
    #[case] fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>,
    #[case] creation_time: Option<time::PrimitiveDateTime>,
    #[case] modification_time: time::PrimitiveDateTime,
    #[case] last_access_date: Option<time::Date>,
) {
    let file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    assert_eq!(creation_time, *file.creation_time());
    assert_eq!(modification_time, *file.modification_time());
    assert_eq!(last_access_date, *file.last_accessed_date());
}

#[test]
#[apply(fs)]
fn modify_file_timestamps(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    use ::time::macros::*;

    let mut file = fs.get_rw_file("/I don't need a badge.txt").unwrap();

    // back to the future we go
    file.set_accessed(date!(1985 - 07 - 3));

    drop(file);

    let file = fs.get_ro_file("/I don't need a badge.txt").unwrap();

    assert_eq!(&Some(date!(1985 - 07 - 3)), file.last_accessed_date());
}
