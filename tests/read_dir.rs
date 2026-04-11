mod common;
use common::*;

pub use test_log::test;

#[test]
fn read_dir_and_go_back() {
    let mut storage = MemoryDevice::from(FAT32);
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
