mod common;
use common::*;

pub use test_log::test;

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
