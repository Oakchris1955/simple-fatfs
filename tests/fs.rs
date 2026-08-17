mod common;
use common::*;

#[cfg(feature = "bloom")]
use simple_fatfs::options::bloom;
use simple_fatfs::options::FSOptions;
use simple_fatfs::time::{Clock, DefaultClock};

use ::time;
use embedded_io::*;

use test_log::test as test_log;

use rstest::*;
use rstest_reuse::*;

#[cfg(test)]
mod create_dir {
    use super::*;

    #[test_log]
    #[apply(fs)]
    fn create_directory_in_root_and_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        fs.create_dir("/unbelievable").unwrap();
        let mut file = fs.create_file("/bee movie script.txt").unwrap();

        file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
        file.rewind().unwrap();

        assert_file_is_bee_movie_script(&mut file);
    }

    #[test_log]
    #[apply(fs)]
    fn create_directory_in_subdir_and_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        fs.create_dir("/subdir/another dir ig").unwrap();
        let mut file = fs.create_file("/subdir/i don't need a badge.txt").unwrap();

        file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
        file.rewind().unwrap();

        assert_file_is_i_dont_need_a_badge(&mut file);
    }
}

#[cfg(test)]
mod create_file {
    use super::*;

    #[test_log]
    #[apply(fs)]
    fn create_root_dir_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.create_file("/new.txt").unwrap();

        file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
        file.rewind().unwrap();

        assert_file_is_i_dont_need_a_badge(&mut file);
    }

    #[test_log]
    #[apply(fs)]
    fn create_subdir_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.create_file("/subdir/baby i am free.txt").unwrap();

        file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
        file.rewind().unwrap();

        assert_file_is_i_dont_need_a_badge(&mut file);
    }

    #[cfg_attr(miri, ignore)]
    #[test_log]
    #[rstest]
    #[case(device(FAT16))]
    #[case(device(FAT32))]
    fn create_lots_of_files(#[case] mut storage: MemoryDevice) {
        use regex::Regex;

        #[cfg(not(miri))]
        const FILE_COUNT: usize = 1000;

        #[cfg(miri)]
        const FILE_COUNT: usize = 10;

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
        fs.cache_dir("/subdir").unwrap();

        for i in 1..=FILE_COUNT {
            let name = format!("/subdir/{i}.txt");
            let mut file = fs.create_file(name).unwrap();

            file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
            file.rewind().unwrap();

            drop(file);
        }

        let dir = fs.read_dir("/subdir/").unwrap();
        let mut found = [false; FILE_COUNT];
        let re = Regex::new(r"([0-9]+).txt").unwrap();
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
}

#[cfg(test)]
mod file_attributes {
    use super::*;

    #[test_log]
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

    #[test_log]
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
}

#[cfg(test)]
mod modify_file {
    use super::*;

    #[cfg_attr(miri, ignore)]
    #[test_log]
    #[apply(fs)]
    fn write_to_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
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

    #[test_log]
    #[apply(fs)]
    fn truncate_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.get_rw_file("/subdir/bee movie script.txt").unwrap();

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
}

#[cfg(test)]
mod read_dir {
    use super::*;

    #[test_log]
    #[apply(fs)]
    fn read_dir_and_go_back(fs: FileSystem<MemoryDevice, DefaultClock>) {
        for entry in fs.read_dir("/").unwrap() {
            let entry = entry.unwrap();

            if entry.path() == "/subdir/" {
                let mut secret_dir = entry.to_dir().unwrap();

                let bee_movie_script_found = secret_dir.any(|res| {
                    if let Ok(entry) = res {
                        entry.is_file() && entry.path() == "/subdir/bee movie script.txt"
                    } else {
                        false
                    }
                });

                assert!(
                    bee_movie_script_found,
                    "couldn't find \"/subdir/bee movie script.txt\""
                )
            }
        }
    }
}

#[cfg(test)]
mod read_file {
    use super::*;

    #[test_log]
    #[apply(fs)]
    fn read_file_in_root_dir(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.get_ro_file("/root.txt").unwrap();

        let mut file_buf = vec![0; file.file_size() as usize];
        file.read_exact(&mut file_buf).unwrap();
        let file_string = str::from_utf8(&file_buf).unwrap();
        const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\nbottom text\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    #[test_log]
    #[apply(fs)]
    fn read_huge_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.get_ro_file("/subdir/bee movie script.txt").unwrap();
        assert_file_is_bee_movie_script(&mut file);
    }

    #[test_log]
    #[apply(fs)]
    fn seek_n_read(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.get_ro_file("/copypasta.txt").unwrap();
        let mut file_bytes = [0_u8; 4096];

        // we first perform a forward seek...
        const EXPECTED_STR1: &str = "Linux is the kernel";
        file.seek(SeekFrom::Start(848)).unwrap();
        let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR1.len()]).unwrap();
        assert_eq!(
            String::from_utf8_lossy(&file_bytes[..bytes_read]),
            EXPECTED_STR1
        );

        // ...then a backward one
        const EXPECTED_STR2: &str = "What you're refering to as Linux, is in fact, GNU/Linux";
        file.seek(SeekFrom::Start(96)).unwrap();
        let bytes_read = file.read(&mut file_bytes[..EXPECTED_STR2.len()]).unwrap();
        assert_eq!(
            String::from_utf8_lossy(&file_bytes[..bytes_read]),
            EXPECTED_STR2
        );
    }

    #[test_log]
    #[apply(fs)]
    fn read_file_in_subdir(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.get_ro_file("/rootdir/example.txt").unwrap();

        let mut file_buf = vec![0; file.file_size() as usize];
        file.read_exact(&mut file_buf).unwrap();
        let file_string = str::from_utf8(&file_buf).unwrap();
        const EXPECTED_STR: &str = "I am not in the root directory :(\n";
        assert_eq!(file_string, EXPECTED_STR);
    }
}

#[cfg(test)]
mod remove_dir {
    use super::*;

    #[test_log]
    #[apply(fs)]

    fn remove_empty_dir(fs: FileSystem<MemoryDevice, DefaultClock>) {
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

    #[test_log]
    #[apply(fs)]

    fn try_remove_nonempty_dir(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let dir_path = "/rootdir/";

        // the directory shouldn't be gone
        assert_eq!(
            fs.remove_empty_dir(dir_path),
            Err(FSError::DirectoryNotEmpty)
        );

        assert!(fs.read_dir(dir_path).is_ok());
    }

    #[test_log]
    #[apply(fs)]

    fn try_remove_dir_with_hidden_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let dir_path = "/hidden/";

        // manually remove the only public file in the directory
        fs.remove_file("/hidden/public.txt").unwrap();

        let rm_result = fs.remove_empty_dir(dir_path);

        match rm_result {
            Err(err) => match err {
                FSError::DirectoryNotEmpty => (),
                _ => panic!("unexpected IOError: {err:?}"),
            },
            Ok(()) => panic!("the directory isn't completely empty (has \"hidden.txt\""),
        }
    }

    #[test_log]
    #[apply(fs)]

    fn remove_nonempty_dir_with_readonly_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        fs.show_hidden(true);

        let dir_path = "/rootdir/";

        // the directory should contain a read-only file (hidden.txt)
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
}

#[cfg(test)]
mod remove_file {
    use super::*;

    #[test_log]
    #[apply(fs)]

    fn remove_root_dir_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        // the "I don't need a bagde" file is in the root directory region
        let file_path = "/I don't need a badge.txt";
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

    #[test_log]
    #[apply(fs)]
    fn remove_data_region_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        // the bee movie script  is in the data region
        let file_path = "/subdir/bee movie script.txt";
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

    #[test_log]
    #[apply(fs)]
    fn attempt_to_remove_file_as_directory(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let target_path = "/hello 🗺️.txt";

        let fs_result = fs.remove_dir_all(target_path);

        match fs_result {
            Err(err) => match err {
                FSError::NotADirectory => (),
                _ => panic!("unexpected IOError: {err:?}"),
            },
            _ => panic!("the filesystem struct should have detected that this isn't a directory"),
        }
    }
}

#[cfg(test)]
mod rename_dir {
    use super::*;

    #[test_log]
    #[apply(fs)]
    fn rename_root_directory(fs: FileSystem<MemoryDevice, DefaultClock>) {
        fs.rename("/rootdir", "/rootdir2").unwrap();

        let mut file = fs.get_ro_file("/rootdir2/example.txt").unwrap();

        let mut file_buf = vec![0; file.file_size() as usize];
        file.read_exact(&mut file_buf).unwrap();
        let file_string = str::from_utf8(&file_buf).unwrap();
        const EXPECTED_STR: &str = "I am not in the root directory :(\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    #[test_log]
    #[apply(fs)]
    fn rename_root_directory_fat32(fs: FileSystem<MemoryDevice, DefaultClock>) {
        fs.rename("/subdir", "/emptydir/secret").unwrap();

        let mut file = fs
            .get_ro_file("/emptydir/secret/bee movie script.txt")
            .unwrap();

        assert_file_is_bee_movie_script(&mut file);
    }
}

#[cfg(test)]
mod rename_file {
    use super::*;

    #[test_log]
    #[apply(fs)]
    fn rename_root_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        fs.rename("/root.txt", "/rootdir/not root.txt").unwrap();

        let mut file = fs.get_ro_file("/rootdir/not root.txt").unwrap();

        let mut file_buf = vec![0; file.file_size() as usize];
        file.read_exact(&mut file_buf).unwrap();
        let file_string = str::from_utf8(&file_buf).unwrap();
        const EXPECTED_STR: &str = "I am in the filesystem's root!!!\n\nbottom text\n";
        assert_eq!(file_string, EXPECTED_STR);
    }

    #[test_log]
    #[apply(fs)]
    fn rename_nonroot_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
        fs.rename("/rootdir/example.txt", "/subdir/hello.txt")
            .unwrap();

        let mut file = fs.get_ro_file("/subdir/hello.txt").unwrap();

        let mut file_buf = vec![0; file.file_size() as usize];
        file.read_exact(&mut file_buf).unwrap();
        let file_string = str::from_utf8(&file_buf).unwrap();
        const EXPECTED_STR: &str = "I am not in the root directory :(\n";
        assert_eq!(file_string, EXPECTED_STR);
    }
}

#[cfg(test)]
mod timestamps {
    use super::*;

    #[test_log]
    #[apply(fs)]
    fn check_last_accessed_ro(fs: FileSystem<MemoryDevice, DefaultClock>) {
        let mut file = fs.get_ro_file("/rootdir/example.txt").unwrap();

        // read some data
        let mut target = [0; 42];
        file.read(&mut target).unwrap();

        drop(file);

        let file = fs.get_ro_file("/rootdir/example.txt").unwrap();

        assert_ne!(&Some(DefaultClock.now().date()), file.last_accessed_date());
    }

    #[test_log]
    #[apply(device)]
    fn check_last_accessed_rw(#[case] mut storage: MemoryDevice) {
        let fs =
            FileSystem::new(&mut storage, FSOptions::new().with_update_file_fields(true)).unwrap();

        let mut file = fs.get_rw_file("/I don't need a badge.txt").unwrap();

        // read some data
        let mut target = [0; 42];
        file.read(&mut target).unwrap();

        drop(file);

        let file = fs.get_ro_file("/I don't need a badge.txt").unwrap();

        assert_eq!(&Some(DefaultClock.now().date()), file.last_accessed_date());
    }

    #[test_log]
    #[apply(device)]
    fn check_last_modified(#[case] mut storage: MemoryDevice) {
        use time::Duration;

        let fs =
            FileSystem::new(&mut storage, FSOptions::new().with_update_file_fields(true)).unwrap();

        let mut file = fs.get_rw_file("/I don't need a badge.txt").unwrap();

        // just some random data
        file.write(&[49, 65, 47]).unwrap();

        drop(file);

        let file = fs.get_ro_file("/I don't need a badge.txt").unwrap();

        assert_eq!(&Some(DefaultClock.now().date()), file.last_accessed_date());
        // I find it highly unlikely that this test won't have been completed within 15 seconds
        assert!(DefaultClock.now() - *file.modification_time() < Duration::seconds(15));
    }

    use time::macros::{date, datetime};

    #[test_log]
    #[rstest]
    #[case(fat12_fs(), Some(datetime!(2026-04-12 14:19:12.32)), datetime!(2026-04-12 13:43:52.0),Some(date!(2026 - 04 - 12)))]
    #[case(fat16_fs(), Some(datetime!(2026-04-15 16:48:20.29)), datetime!(2026-04-12 13:43:52.0),Some(date!(2026 - 04 - 12)))]
    #[case(fat32_fs(), Some(datetime!(2026-04-12 14:19:35.38)), datetime!(2026-04-12 13:43:52.0),Some(date!(2026 - 04 - 12)))]
    fn check_file_timestamps(
        #[case] fs: FileSystem<MemoryDevice, DefaultClock>,
        #[case] creation_time: Option<time::PrimitiveDateTime>,
        #[case] modification_time: time::PrimitiveDateTime,
        #[case] last_access_date: Option<time::Date>,
    ) {
        let file = fs.get_ro_file("/rootdir/example.txt").unwrap();

        assert_eq!(creation_time, *file.creation_time());
        assert_eq!(modification_time, *file.modification_time());
        assert_eq!(last_access_date, *file.last_accessed_date());
    }

    #[test_log]
    #[apply(fs)]
    fn modify_file_timestamps(fs: FileSystem<MemoryDevice, DefaultClock>) {
        use time::macros::date;

        let mut file = fs.get_rw_file("/I don't need a badge.txt").unwrap();

        // back to the future we go
        file.set_accessed(date!(1985 - 07 - 3));

        drop(file);

        let file = fs.get_ro_file("/I don't need a badge.txt").unwrap();

        assert_eq!(&Some(date!(1985 - 07 - 3)), file.last_accessed_date());
    }
}

#[cfg(test)]
mod volume_label {
    use super::*;

    #[test_log]
    #[rstest]
    #[case(fat12_fs(), None)]
    #[case(fat16_fs(), Some("SOMETHINGEL".into()))]
    #[case(fat32_fs(), Some("SIMPLEFATFS".into()))]
    fn volume_label_bpb(
        #[case] fs: FileSystem<MemoryDevice, DefaultClock>,
        #[case] bpb_volume_label: Option<String>,
    ) {
        assert_eq!(fs.volume_label_bpb(), bpb_volume_label)
    }

    #[test_log]
    #[rstest]
    #[case(fat12_fs(), Some("HELLOWORLD".into()))]
    #[case(fat16_fs(), Some("SOMETHINGEL".into()))]
    #[case(fat32_fs(), None)]
    fn volume_label_root(
        #[case] fs: FileSystem<MemoryDevice, DefaultClock>,
        #[case] root_volume_label: Option<String>,
    ) {
        assert_eq!(fs.volume_label_root_dir().unwrap(), root_volume_label)
    }

    #[test_log]
    #[apply(device)]
    fn set_volume_label_bpb(mut device: MemoryDevice) {
        let fs = FileSystem::new(&mut device, FSOptions::new()).unwrap();

        fs.set_volume_label_bpb("DEADBEEF");

        drop(fs);

        let fs = FileSystem::new(&mut device, FSOptions::new()).unwrap();

        assert_eq!(fs.volume_label_bpb(), Some(String::from("DEADBEEF")));
    }

    #[test_log]
    #[apply(device)]
    fn set_volume_label_root_dir(mut device: MemoryDevice) {
        let fs = FileSystem::new(&mut device, FSOptions::new()).unwrap();

        fs.set_volume_label_root_dir("DEADBEEF").unwrap();

        drop(fs);

        let fs = FileSystem::new(&mut device, FSOptions::new()).unwrap();

        assert_eq!(
            fs.volume_label_root_dir().unwrap(),
            Some(String::from("DEADBEEF"))
        );
    }
}
