mod common;
use common::*;

use test_log::test;

#[test]
fn read_file_in_root_dir() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_ro_file("/root.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn read_huge_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_ro_file("/bee movie script.txt").unwrap();
    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn seek_n_read() {
    use std::io::Cursor;

    // this uses the famous "I'd like to interject for a moment" copypasta as a test file
    // you can find it online by just searching this term

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_ro_file("/GNU ⁄ Linux copypasta.txt").unwrap();
    let mut file_bytes = [0_u8; 4096];

    // we first perform a forward seek...
    const EXPECTED_STR1: &str = "Linux is the kernel";
    file.seek(SeekFrom::Start(792)).unwrap();
    let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR1.len()]).unwrap();
    assert_eq!(
        String::from_utf8_lossy(&file_bytes[..bytes_read]),
        EXPECTED_STR1
    );

    // ...then a backward one
    const EXPECTED_STR2: &str = "What you're referring to as Linux, is in fact, GNU/Linux";
    file.seek(SeekFrom::Start(39)).unwrap();
    let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR2.len()]).unwrap();
    assert_eq!(
        String::from_utf8_lossy(&file_bytes[..bytes_read]),
        EXPECTED_STR2
    );
}

#[test]
fn read_file_in_subdir() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn read_file_fat12() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT12.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    {
        let mut file = fs.get_ro_file("/foo/bar.txt").unwrap();
        let mut file_buf = vec![0; file.file_size() as usize];
        file.read_exact(&mut file_buf).unwrap();
        let file_string = str::from_utf8(&file_buf).unwrap();
        const EXPECTED_STR: &str = "Hello, World!\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    {
        // please not that the FAT12 image has been modified so that
        // one FAT entry of the file we are reading is split between different sectors
        // this way, we also test for this case
        let mut file = fs.get_ro_file("/test/bee movie script.txt").unwrap();
        assert_file_is_bee_movie_script(&mut file);
    }
}

#[test]
fn read_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.get_ro_file("/secret/bee movie script.txt").unwrap();

    assert_file_is_bee_movie_script(&mut file);
}
