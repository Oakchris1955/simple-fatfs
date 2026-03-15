use crate::*;
use embedded_io::*;

use akin::akin;
use test_log::test;

static MINFS: &[u8] = include_bytes!("../../imgs/minfs.img");
static FAT12: &[u8] = include_bytes!("../../imgs/fat12.img");
static FAT16: &[u8] = include_bytes!("../../imgs/fat16.img");
static FAT32: &[u8] = include_bytes!("../../imgs/fat32.img");

#[test]
#[expect(non_snake_case)]
fn check_FAT_offset() {
    use crate::fat::BootRecord;

    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let fat_offset = match &*fs.boot_record.borrow() {
        BootRecord::Fat(boot_record_fat) => boot_record_fat.first_fat_sector(),
        BootRecord::ExFAT(_boot_record_exfat) => unreachable!(),
    };

    // we manually read the first and second entry of the FAT table
    fs.load_nth_sector(fat_offset.into()).unwrap();

    let first_entry = u16::from_le_bytes(fs.sector_buffer.borrow()[..2].try_into().unwrap());
    let media_type = if let BootRecord::Fat(boot_record_fat) = &*fs.boot_record.borrow() {
        boot_record_fat.bpb._media_type
    } else {
        unreachable!("this should be a FAT16 filesystem")
    };
    assert_eq!(u16::MAX << 8 | u16::from(media_type), first_entry);

    let second_entry = u16::from_le_bytes(fs.sector_buffer.borrow()[2..4].try_into().unwrap());
    assert_eq!(u16::MAX, second_entry);
}

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

fn assert_vec_is_string(buf: &[u8], expected_string: &str) {
    let string = std::str::from_utf8(buf).unwrap();
    let expected_size = expected_string.len();
    assert_eq!(buf.len(), expected_size);

    assert_eq!(string, expected_string);
}
fn assert_file_against_string<S, C>(file: &mut ROFile<'_, S, C>, expected_string: &str)
where
    S: BlockWrite,
    C: Clock,
{
    let mut buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut buf).unwrap();

    assert_vec_is_string(&buf, expected_string);
}

static BEE_MOVIE_SCRIPT: &str = include_str!("../../tests/bee movie script.txt");
fn assert_vec_is_bee_movie_script(buf: &[u8]) {
    assert_vec_is_string(buf, BEE_MOVIE_SCRIPT)
}
fn assert_file_is_bee_movie_script<S, C>(file: &mut ROFile<'_, S, C>)
where
    S: BlockWrite,
    C: Clock,
{
    assert_file_against_string(file, BEE_MOVIE_SCRIPT);
}
static I_DONT_NEED_A_BADGE: &str = include_str!("../../tests/I don't need a badge.txt");
fn assert_file_is_i_dont_need_a_badge<S, C>(file: &mut ROFile<'_, S, C>)
where
    S: BlockWrite,
    C: Clock,
{
    assert_file_against_string(file, I_DONT_NEED_A_BADGE);
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
fn create_root_dir_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.create_file("/new.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
fn create_subdir_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs
        .create_file("/another root directory/baby i am free.txt")
        .unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[cfg_attr(miri, ignore)]
#[test]
fn create_lots_of_files() {
    use regex::Regex;
    use std::io::Cursor;

    #[cfg(not(miri))]
    const FILE_COUNT: usize = 1000;

    #[cfg(miri)]
    const FILE_COUNT: usize = 10;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    #[cfg(not(feature = "bloom"))]
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();
    #[cfg(feature = "bloom")]
    let mut fs = FileSystem::new(
        &mut storage,
        FSOptions::new().with_filter_size(bloom::compute_bitmap_size(
            std::num::NonZero::new(FILE_COUNT * 2).unwrap(),
            0.001,
        )),
    )
    .unwrap();

    #[cfg(feature = "bloom")]
    fs.cache_dir("/another root directory").unwrap();

    for i in 1..=FILE_COUNT {
        let name = PathBuf::from(&format!("/another root directory/{i}.txt"));
        let mut file = fs.create_file(&name).unwrap();

        file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
        file.rewind().unwrap();

        drop(file);
    }

    let dir = fs.read_dir("/another root directory/").unwrap();
    let mut found = [false; FILE_COUNT];
    let re = Regex::new(r"([0-9]*).txt").unwrap();
    for entry in dir {
        let entry = entry.unwrap();
        if entry.is_file() {
            let file_name = entry.path().file_name().unwrap();
            if let Some(c_id) = re.captures(file_name) {
                let id: usize = c_id[1].parse().unwrap();
                if (1..=FILE_COUNT).contains(&id) {
                    found[id - 1] = true;
                    let mut file = entry.to_ro_file().unwrap();
                    assert_file_is_i_dont_need_a_badge(&mut file);
                } else {
                    log::error!("Found unexpected file with name \"{id}\"")
                }
            }
        }
    }

    let mut all_found = true;
    for (id, id_found) in found.iter().enumerate() {
        if !id_found {
            all_found = false;
            log::error!("File /another root directory/{id}.txt not found")
        }
    }

    assert!(
        all_found,
        "Some files that were created weren't found during directory iteration"
    )
}

#[test]
fn create_directory_in_root_and_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/unbelievable").unwrap();
    let mut file = fs.create_file("/unbelievable/baby i am free.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
fn create_directory_in_subdir_and_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/another root directory2").unwrap();
    let mut file = fs
        .create_file(PathBuf::from(
            "/another root directory/bee movie script.txt",
        ))
        .unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn rename_root_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/root.txt", "/rootdir/not root.txt").unwrap();

    let mut file = fs.get_ro_file("/rootdir/not root.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn rename_nonroot_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/rootdir/example.txt", "/another root directory/hello.txt")
        .unwrap();

    let mut file = fs.get_ro_file("/another root directory/hello.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn rename_root_directory() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/rootdir", "/rootdir2").unwrap();

    let mut file = fs.get_ro_file("/rootdir2/example.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "I am not in the root directory :(\n\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn rename_root_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/hello.txt", "/emptydir/bye.txt").unwrap();

    let mut file = fs.get_ro_file("/emptydir/bye.txt").unwrap();

    let mut file_buf = vec![0; file.file_size() as usize];
    file.read_exact(&mut file_buf).unwrap();
    let file_string = str::from_utf8(&file_buf).unwrap();
    const EXPECTED_STR: &str = "Hello from a FAT32 filesystem!!!\n";
    assert_eq!(file_string, EXPECTED_STR);
}

#[test]
fn rename_nonroot_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/secret/bee movie script.txt", "/BEES.txt")
        .unwrap();

    let mut file = fs.get_ro_file("/BEES.txt").unwrap();

    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn rename_root_directory_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.rename("/secret", "/emptydir/secret").unwrap();

    let mut file = fs
        .get_ro_file("/emptydir/secret/bee movie script.txt")
        .unwrap();

    assert_file_is_bee_movie_script(&mut file);
}

#[test]
fn remove_root_dir_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    // the bee movie script (here) is in the root directory region
    let file_path = "/bee movie script.txt";
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
fn remove_data_region_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT12.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    // the bee movie script (here) is in the data region
    let file_path = "/test/bee movie script.txt";
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
fn remove_empty_dir() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/another root directory/";

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
fn remove_nonempty_dir_with_readonly_file() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/rootdir/";

    // the directory should contain a read-only file (example.txt)
    let del_result = fs.remove_dir_all(dir_path);
    match del_result {
        Err(err) => match err {
            FSError::ReadOnlyFile => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("the directory shouldn't have been removed already"),
    }

    // this should now remove the directory
    fs.remove_dir_all_unchecked(dir_path).unwrap();

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
akin! {
    let &fat_type = [FAT12, FAT16, FAT32];
    let &unused_entries = [5, 2, 1];

    #[test]
    #[expect(non_snake_case)]
    fn entry_defragment_~*fat_type() {
        const UNUSED_ENTRY_COUNT: EntryCount = *unused_entries;

        use std::io::Cursor;

        let mut storage = FromStd::new(Cursor::new(~*fat_type.to_owned())).unwrap();
        let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();
        fs.show_hidden(true);

        // ik, this is dirty
        let old_entry_count = {
            let mut i: EntryCount = 0;

            fs.go_to_dir("/").unwrap();

            let mut current_entry = EntryLocation {
                unit: fs.dir_info.borrow().chain_start,
                index: 0,
            };

            while let Some(next_entry) = current_entry
                .next_entry(&fs)
                .unwrap()
                .filter(|entry| entry.entry_status(&fs).unwrap() != EntryStatus::LastUnused)
            {
                current_entry = next_entry;
                i += 1
            }

            // we miss the last entry because of the .filter
            i + 1
        };

        log::info!("Old entry count: {old_entry_count}");

        let old_names: Box<[Box<str>]> = fs
            .read_dir("/")
            .unwrap()
            .map(|entry| entry.unwrap())
            .map(|entry| entry.path().file_name().unwrap().to_owned())
            .map(Box::from)
            .collect();

        let new_entry_count = fs.defragment_entry_chain().unwrap();

        log::info!("New entry count: {new_entry_count}");

        let new_names: Box<[Box<str>]> = fs
            .read_dir("/")
            .unwrap()
            .map(|entry| entry.unwrap())
            .map(|entry| entry.path().file_name().unwrap().to_owned())
            .map(Box::from)
            .collect();

        assert_eq!(old_names, new_names);
        assert_eq!(old_entry_count - UNUSED_ENTRY_COUNT, new_entry_count);
    }
}

#[test]
#[expect(non_snake_case)]
fn FAT_tables_after_write_are_identical() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    assert!(
        fs.FAT_tables_are_identical().unwrap(),
        concat!(
            "this should pass. ",
            "if it doesn't, either the corresponding .img file's FAT tables aren't identical",
            "or the tables_are_identical function doesn't work correctly"
        )
    );

    // let's write the bee movie script to root.txt (why not), check, truncate the file, then check again
    let mut file = fs.get_rw_file("root.txt").unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());

    file.seek(SeekFrom::Start(10_000)).unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());
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
        assert!(file.attributes.hidden);
    }
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
fn check_file_timestamps() {
    use ::time::macros::*;

    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT16.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let file = fs.get_ro_file("/rootdir/example.txt").unwrap();

    assert_eq!(Some(datetime!(2024-07-11 13:02:38.15)), file.created);
    assert_eq!(datetime!(2024-07-11 13:02:38.0), file.modified);
    assert_eq!(Some(date!(2024 - 07 - 11)), file.accessed);
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

#[test]
fn create_file_root_dir_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs
        .create_file("/bee movie script or something ig.txt")
        .unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
fn create_file_subdir_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let mut file = fs.create_file("/secret/baby i am free.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
fn create_directory_in_root_and_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/unbelievable").unwrap();
    let mut file = fs.create_file("/unbelievable/baby i am free.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
fn create_directory_in_subdir_and_file_fat32() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    fs.create_dir("/another root directory").unwrap();
    let mut file = fs
        .create_file(PathBuf::from(
            "/another root directory/bee movie script.txt",
        ))
        .unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_bee_movie_script(&mut file);
}

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

#[test]
fn attempt_to_remove_file_as_directory() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    let dir_path = "/hello.txt";

    let fs_result = fs.remove_dir_all(dir_path);

    match fs_result {
        Err(err) => match err {
            FSError::NotADirectory => (),
            _ => panic!("unexpected IOError: {err:?}"),
        },
        _ => panic!("the filesystem struct should have detected that this isn't a directory"),
    }
}

#[test]
fn read_dir_and_go_back() {
    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    for entry in fs.read_dir("/").unwrap() {
        let entry = entry.unwrap();

        if entry.path() == "/secret/" {
            let mut secret_dir = entry.to_dir().unwrap();

            let bee_movie_script_found = secret_dir.any(|res| {
                if let Ok(entry) = res {
                    entry.is_file() && entry.path() == "/secret/bee movie script.txt"
                } else {
                    false
                }
            });

            assert!(
                bee_movie_script_found,
                "couldn't find \"/secret/bee movie script.txt\""
            )
        }
    }
}

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

#[test]
#[expect(non_snake_case)]
fn FAT_tables_after_fat32_write_are_identical() {
    use crate::fat::{BootRecord, Ebr};

    use std::io::Cursor;

    let mut storage = FromStd::new(Cursor::new(FAT32.to_owned())).unwrap();
    let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

    match &*fs.boot_record.borrow() {
        BootRecord::Fat(boot_record_fat) => match &boot_record_fat.ebr {
            Ebr::FAT32(ebr_fat32, _) => assert!(
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
    let mut file = fs.get_rw_file("hello.txt").unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());

    file.seek(SeekFrom::Start(10_000)).unwrap();
    file.truncate().unwrap();
    assert!(file.fs.FAT_tables_are_identical().unwrap());
}

#[test]
fn assert_fat_sector_size() {
    static TEST_CASES: &[(&[u8], u16)] = &[(MINFS, 512), (FAT12, 512), (FAT16, 512), (FAT32, 512)];

    for case in TEST_CASES {
        use std::io::Cursor;

        let mut storage = FromStd::new(Cursor::new(case.0)).unwrap();
        let sector_size = determine_fs_sector_size(&mut storage).unwrap();

        assert_eq!(sector_size, case.1)
    }
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

        let mut storage = FromStd::new(Cursor::new(case.0)).unwrap();
        let fs = FileSystem::new(&mut storage, FSOptions::new()).unwrap();

        assert_eq!(fs.fat_type(), case.1)
    }
}
