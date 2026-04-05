mod common;
use common::*;

pub use test_log::test;

/// The cases below exist to ensure that we handle FAT32 filesystems correctly
/// (some differences between FAT16 and FAT32 used to cause uncatched error before these)

#[test]
fn seek_n_read_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_ro_file("/hello.txt").unwrap();
    file.seek(SeekFrom::Start(13)).unwrap();

    #[expect(clippy::cast_possible_truncation)]
    let mut file_buf =
        vec![0; (file.file_size() - file.stream_position().unwrap() as u32) as usize];
    file.read_exact(&mut file_buf).unwrap();
    let string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "FAT32 filesystem!!!\n";

    assert_eq!(string, EXPECTED_STR);
}

#[test]
fn write_to_fat32_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_rw_file("/hello.txt").unwrap();
    // an arbitrary offset to seek to
    const START_OFFSET: u64 = 1436;
    file.seek(SeekFrom::Start(START_OFFSET)).unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();

    // seek back
    file.seek(SeekFrom::Current(
        -i64::try_from(BEE_MOVIE_SCRIPT.len()).unwrap(),
    ))
    .unwrap();

    // read back what we wrote
    #[expect(clippy::cast_possible_truncation)]
    let mut file_buf =
        vec![0; (file.file_size() - file.stream_position().unwrap() as u32) as usize];
    file.read_exact(&mut file_buf).unwrap();
    let string = str::from_utf8(&file_buf).unwrap();
    assert_eq!(string, BEE_MOVIE_SCRIPT);

    // let's also read back what was (and hopefully still is)
    // at the start of the file
    const EXPECTED_STR: &str = "Hello from a FAT32 filesystem!!!\n";
    file.rewind().unwrap();
    let mut buf = [0_u8; EXPECTED_STR.len()];
    file.read_exact(&mut buf).unwrap();

    let stored_text = std::str::from_utf8(&buf).unwrap();
    assert_eq!(stored_text, EXPECTED_STR)
}

#[test]
fn truncate_fat32_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    const EXPECTED_STR: &str = "Hello fr";

    let mut file = fs.get_rw_file("/hello.txt").unwrap();
    file.seek(SeekFrom::Start(EXPECTED_STR.len() as u64))
        .unwrap();
    file.truncate().unwrap();

    file.rewind().unwrap();
    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let string = str::from_utf8(&file_buf).unwrap();
    assert_eq!(string, EXPECTED_STR);
}

#[test]
fn remove_fat32_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let file_path = "/secret/bee movie script.txt";

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
fn remove_empty_fat32_dir() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/emptydir/";

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
fn remove_nonempty_fat32_dir() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/secret/";

    fs.remove_dir_all(dir_path).unwrap();

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
