use crate::io::prelude::*;
use crate::*;

use test_log::test;

static MINFS: &[u8] = include_bytes!("../../imgs/minfs.img");
static FAT12: &[u8] = include_bytes!("../../imgs/fat12.img");
static FAT16: &[u8] = include_bytes!("../../imgs/fat16.img");
static FAT32: &[u8] = include_bytes!("../../imgs/fat32.img");

#[test]
#[allow(non_snake_case)]
fn check_FAT_offset() {
    use crate::fat::BootRecord;

    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let fat_offset = match fs.boot_record {
        BootRecord::FAT(boot_record_fat) => boot_record_fat.first_fat_sector(),
        BootRecord::ExFAT(_boot_record_exfat) => unreachable!(),
    };

    // we manually read the first and second entry of the FAT table
    fs.read_nth_sector(fat_offset.into()).unwrap();

    let first_entry = u16::from_le_bytes(fs.sector_buffer[..2].try_into().unwrap());
    let media_type = if let BootRecord::FAT(boot_record_fat) = fs.boot_record {
        boot_record_fat.bpb._media_type
    } else {
        unreachable!("this should be a FAT16 filesystem")
    };
    assert_eq!(u16::MAX << 8 | media_type as u16, first_entry);

    let second_entry = u16::from_le_bytes(fs.sector_buffer[2..4].try_into().unwrap());
    assert_eq!(u16::MAX, second_entry);
}

#[test]
fn read_file_in_root_dir() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs.get_ro_file(PathBuf::from("/root.txt")).unwrap();

    let mut file_string = String::new();
    file.read_to_string(&mut file_string).unwrap();
    const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

static BEE_MOVIE_SCRIPT: &str = include_str!("../../tests/bee movie script.txt");
fn assert_vec_is_bee_movie_script(buf: &Vec<u8>) {
    let string = std::str::from_utf8(&buf).unwrap();
    let expected_size = BEE_MOVIE_SCRIPT.len();
    assert_eq!(buf.len(), expected_size);

    assert_eq!(string, BEE_MOVIE_SCRIPT);
}
fn assert_file_is_bee_movie_script<S>(file: &mut ROFile<'_, '_, S>)
where
    S: Read + Write + Seek,
{
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    assert_vec_is_bee_movie_script(&buf);
}

#[test]
fn read_huge_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs
        .get_ro_file(PathBuf::from("/bee movie script.txt"))
        .unwrap();
    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn seek_n_read() {
    // this uses the famous "I'd like to interject for a moment" copypasta as a test file
    // you can find it online by just searching this term

    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs
        .get_ro_file(PathBuf::from("/GNU ⁄ Linux copypasta.txt"))
        .unwrap();
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
// this won't actually modify the .img file or the static slices,
// since we run .to_owned(), which basically clones the data in the static slices,
// in order to make the Cursor readable/writable
fn write_to_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT12.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs.get_rw_file(PathBuf::from("/root.txt")).unwrap();

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
    file.seek(SeekFrom::Current(-(TEXT.len() as i64))).unwrap();
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
fn remove_root_dir_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    // the bee movie script (here) is in the root directory region
    let file_path = PathBuf::from("/bee movie script.txt");
    let file = fs.get_rw_file(file_path.clone()).unwrap();
    file.remove().unwrap();

    // the file should now be gone
    let file_result = fs.get_ro_file(file_path);
    match file_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {:?}", err),
        },
        _ => panic!("file should have been deleted by now"),
    }
}

#[test]
fn remove_data_region_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT12.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    // the bee movie script (here) is in the data region
    let file_path = PathBuf::from("/test/bee movie script.txt");
    let file = fs.get_rw_file(file_path.clone()).unwrap();
    file.remove().unwrap();

    // the file should now be gone
    let file_result = fs.get_ro_file(file_path);
    match file_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {:?}", err),
        },
        _ => panic!("file should have been deleted by now"),
    }
}

#[test]
fn remove_empty_dir() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let dir_path = PathBuf::from("/another root directory/");

    fs.remove_empty_dir(dir_path.clone()).unwrap();

    // the directory should now be gone
    let dir_result = fs.read_dir(dir_path);
    match dir_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {:?}", err),
        },
        _ => panic!("the directory should have been deleted by now"),
    }
}

#[test]
#[allow(non_snake_case)]
fn FAT_tables_after_write_are_identical() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    assert!(
        fs.FAT_tables_are_identical().unwrap(),
        concat!(
            "this should pass. ",
            "if it doesn't, either the corresponding .img file's FAT tables aren't identical",
            "or the tables_are_identical function doesn't work correctly"
        )
    );

    // let's write the bee movie script to root.txt (why not), check, truncate the file, then check again
    let mut file = fs.get_rw_file(PathBuf::from("root.txt")).unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());

    file.truncate(10_000).unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());
}

#[test]
fn truncate_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs
        .get_rw_file(PathBuf::from("/bee movie script.txt"))
        .unwrap();

    // we are gonna truncate the bee movie script down to 20 000 bytes
    const NEW_SIZE: u32 = 20_000;
    file.truncate(NEW_SIZE).unwrap();

    let mut file_string = String::new();
    file.read_to_string(&mut file_string).unwrap();
    let mut expected_string = BEE_MOVIE_SCRIPT.to_string();
    expected_string.truncate(NEW_SIZE as usize);

    assert_eq!(file_string, expected_string);
}

#[test]
fn read_only_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let file_result = fs.get_rw_file(PathBuf::from("/rootdir/example.txt"));

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

    let mut storage = Cursor::new(FAT12.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let file_path = PathBuf::from("/hidden");
    let file_result = fs.get_ro_file(file_path.clone());
    match file_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError"),
        },
        _ => panic!("file should be hidden by default"),
    }

    // let's now allow the filesystem to list hidden files
    fs.show_hidden(true);
    let file = fs.get_ro_file(file_path).unwrap();
    assert!(file.attributes.hidden);
}

#[test]
fn read_file_in_subdir() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs
        .get_ro_file(PathBuf::from("/rootdir/example.txt"))
        .unwrap();

    let mut file_string = String::new();
    file.read_to_string(&mut file_string).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn check_file_timestamps() {
    use ::time::macros::*;

    use std::io::Cursor;

    let mut storage = Cursor::new(FAT16.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let file = fs
        .get_ro_file(PathBuf::from("/rootdir/example.txt"))
        .unwrap();

    assert_eq!(datetime!(2024-07-11 13:02:38.15), file.created);
    assert_eq!(datetime!(2024-07-11 13:02:38.0), file.modified);
    assert_eq!(date!(2024 - 07 - 11), file.accessed);
}

#[test]
fn read_file_fat12() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT12.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs.get_ro_file(PathBuf::from("/foo/bar.txt")).unwrap();
    let mut file_string = String::new();
    file.read_to_string(&mut file_string).unwrap();
    const EXPECTED_STR: &str = "Hello, World!\n";
    assert_eq!(file_string, EXPECTED_STR);

    // please not that the FAT12 image has been modified so that
    // one FAT entry of the file we are reading is split between different sectors
    // this way, we also test for this case
    let mut file = fs
        .get_ro_file(PathBuf::from("/test/bee movie script.txt"))
        .unwrap();
    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn read_file_fat32() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT32.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs
        .get_ro_file(PathBuf::from("/secret/bee movie script.txt"))
        .unwrap();

    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn seek_n_read_fat32() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT32.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs.get_ro_file(PathBuf::from("/hello.txt")).unwrap();
    file.seek(SeekFrom::Start(13)).unwrap();

    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();
    const EXPECTED_STR: &str = "FAT32 filesystem!!!\n";

    assert_eq!(string, EXPECTED_STR);
}

#[test]
fn write_to_fat32_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT32.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let mut file = fs.get_rw_file(PathBuf::from("/hello.txt")).unwrap();
    // an arbitrary offset to seek to
    const START_OFFSET: u64 = 1436;
    file.seek(SeekFrom::Start(START_OFFSET)).unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();

    // seek back
    file.seek(SeekFrom::Current(-(BEE_MOVIE_SCRIPT.len() as i64)))
        .unwrap();

    // read back what we wrote
    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();
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

    let mut storage = Cursor::new(FAT32.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    const EXPECTED_STR: &str = "Hello fr";

    let mut file = fs.get_rw_file(PathBuf::from("/hello.txt")).unwrap();
    file.truncate(EXPECTED_STR.len() as u32).unwrap();

    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();
    assert_eq!(string, EXPECTED_STR);
}

#[test]
fn remove_fat32_file() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT32.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let file_path = PathBuf::from("/secret/bee movie script.txt");

    let file = fs.get_rw_file(file_path.clone()).unwrap();
    file.remove().unwrap();

    // the file should now be gone
    let file_result = fs.get_ro_file(file_path);
    match file_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {:?}", err),
        },
        _ => panic!("file should have been deleted by now"),
    }
}

#[test]
fn remove_fat32_dir() {
    use std::io::Cursor;

    let mut storage = Cursor::new(FAT32.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    let dir_path = PathBuf::from("/emptydir/");

    fs.remove_empty_dir(dir_path.clone()).unwrap();

    // the directory should now be gone
    let dir_result = fs.read_dir(dir_path);
    match dir_result {
        Err(err) => match err {
            FSError::NotFound => (),
            _ => panic!("unexpected IOError: {:?}", err),
        },
        _ => panic!("the directory should have been deleted by now"),
    }
}

#[test]
#[allow(non_snake_case)]
fn FAT_tables_after_fat32_write_are_identical() {
    use crate::fat::{BootRecord, EBR};

    use std::io::Cursor;

    let mut storage = Cursor::new(FAT32.to_owned());
    let mut fs = FileSystem::from_storage(&mut storage).unwrap();

    match fs.boot_record {
        BootRecord::FAT(boot_record_fat) => match boot_record_fat.ebr {
            EBR::FAT32(ebr_fat32, _) => assert!(
                !ebr_fat32.extended_flags.mirroring_disabled(),
                "mirroring should be enabled for this .img file"
            ),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }

    assert!(
        fs.FAT_tables_are_identical().unwrap(),
        concat!(
            "this should pass. ",
            "if it doesn't, either the corresponding .img file's FAT tables aren't identical",
            "or the tables_are_identical function doesn't work correctly"
        )
    );

    // let's write the bee movie script to root.txt (why not), check, truncate the file, then check again
    let mut file = fs.get_rw_file(PathBuf::from("hello.txt")).unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());

    file.truncate(10_000).unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());
}

#[test]
fn assert_img_fat_type() {
    static TEST_CASES: &[(&[u8], FATType)] = &[
        (MINFS, FATType::FAT12),
        (FAT12, FATType::FAT12),
        (FAT16, FATType::FAT16),
        (FAT32, FATType::FAT32),
    ];

    for case in TEST_CASES {
        use std::io::Cursor;

        let mut storage = Cursor::new(case.0.to_owned());
        let fs = FileSystem::from_storage(&mut storage).unwrap();

        assert_eq!(fs.fat_type(), case.1)
    }
}
