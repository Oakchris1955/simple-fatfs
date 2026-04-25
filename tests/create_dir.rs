mod common;
use common::*;

use embedded_io::*;

use rstest::*;
use rstest_reuse::*;
use test_log::test;

#[test]
#[apply(fs)]
fn create_directory_in_root_and_file(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    fs.create_dir("/unbelievable").unwrap();
    let mut file = fs.create_file("/bee movie script.txt").unwrap();

    file.write_all(BEE_MOVIE_SCRIPT.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_bee_movie_script(&mut file);
}

#[test]
#[apply(fs)]
fn create_directory_in_subdir_and_file(fs: FileSystem<MemoryDevice<Box<[u8]>>, DefaultClock>) {
    fs.create_dir("/subdir/another dir ig").unwrap();
    let mut file = fs
        .create_file(PathBuf::from("/subdir/i don't need a badge.txt"))
        .unwrap();

    file.write_all(I_DONT_NEED_A_BADGE.as_bytes()).unwrap();
    file.rewind().unwrap();

    assert_file_is_i_dont_need_a_badge(&mut file);
}
