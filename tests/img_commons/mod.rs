use std::env;
use std::path::PathBuf;

pub const IMGS_SUBDIR: &str = "imgs/";

/// get the pathbuf to the subdir where the /img files are located
pub fn get_imgs_path() -> PathBuf {
    let mut path_buf = env::current_dir().unwrap();
    path_buf.push(IMGS_SUBDIR);
    path_buf
}

#[test]
fn img_dir_exists() {
    assert!(get_imgs_path().is_dir());
}
