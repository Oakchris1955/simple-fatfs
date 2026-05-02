mod common;
use common::*;

use embedded_io::*;

use rstest::*;
use rstest_reuse::*;
pub use test_log::test;

#[test]
#[apply(fs)]
fn create_root_dir_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
    let mut file = fs.create_file("/new.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[test]
#[apply(fs)]
fn create_subdir_file(fs: FileSystem<MemoryDevice, DefaultClock>) {
    let mut file = fs.create_file("/subdir/baby i am free.txt").unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}

#[cfg_attr(miri, ignore)]
#[test]
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
        let name = PathBuf::from(&format!("/subdir/{i}.txt"));
        let mut file = fs.create_file(&name).unwrap();

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
