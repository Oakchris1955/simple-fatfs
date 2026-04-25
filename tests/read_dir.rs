mod common;
use common::*;

use rstest::*;
use rstest_reuse::*;
use test_log::test;

#[test]
#[apply(fs)]
fn read_dir_and_go_back(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
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
