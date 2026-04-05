mod common;
use common::*;

pub use test_log::test;

#[cfg_attr(miri, ignore)]
#[test]
// this won't actually modify the .img file or the static slices,
// since we run .to_owned(), which basically clones the data in the static slices,
// in order to make the Cursor readable/writable
fn write_to_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT12.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_rw_file("/root.txt").unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_bee_movie_script(&mut file);

    // now let's do something else
    // this write operations will happen between 2 clusters
    const TEXT_OFFSET: u64 = 4598;
    const TEXT: &str = "Hello from the other side";

    file.seek(SeekFrom::Start(TEXT_OFFSET)).unwrap();
    file.write_all(TEXT.as_bytes()).unwrap();

    // seek back to the start of where we wrote our text
    file.seek(SeekFrom::Current(-i64::try_from(TEXT.len()).unwrap()))
        .unwrap();
    let mut buf = [0_u8; TEXT.len()];
    file.read_exact(&mut buf).unwrap();
    let stored_text = std::str::from_utf8(&buf).unwrap();

    assert_eq!(TEXT, stored_text);

    // we are also gonna write the bee movie ten more times to see if FAT12 can correctly handle split entries
    for i in 0..10 {
        log::debug!("Writing the bee movie script for the {i} consecutive time",);

        let start_offset = file.seek(SeekFrom::End(0)).unwrap();

        file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
        file.seek(SeekFrom::Start(start_offset)).unwrap();

        let mut buf = vec![0_u8; BEE_MOVIE_SCRIPT.len()];
        file.read_exact(buf.as_mut_slice()).unwrap();

        assert_vec_is_bee_movie_script(&buf);
    }
}

#[test]
fn truncate_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_rw_file("/bee movie script.txt").unwrap();

    // we are gonna truncate the bee movie script down to 20 000 bytes
    const NEW_SIZE: usize = 20_000;
    file.seek(SeekFrom::Start(20_000)).unwrap();
    file.truncate().unwrap();

    file.rewind().unwrap();
    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    let mut expected_string = BEE_MOVIE_SCRIPT.to_string();
    expected_string.truncate(NEW_SIZE);

    assert_eq!(file_string, expected_string);
}
