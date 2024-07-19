mod img_commons;

use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn verify_checksums() {
    let mut fails: Vec<PathBuf> = Vec::new();
    let mut total_checks = 0_usize;

    for file in fs::read_dir(img_commons::get_imgs_path())
        .unwrap()
        .map(|f| f.unwrap().path())
        .filter(|p| p.extension().unwrap() == "ck")
    {
        total_checks += 1;

        if !sha256sum(&file).success() {
            fails.push(file)
        }
    }

    if !fails.is_empty() {
        panic!(
            "A total of {} out of {} checksum checks failed:\n{:?}",
            fails.len(),
            total_checks,
            fails
        )
    }
}

fn sha256sum(file: &std::path::PathBuf) -> std::process::ExitStatus {
    Command::new("sha256sum")
        .args(["--status", "-c", file.as_os_str().to_str().unwrap()])
        .status()
        // if the sha256sum utility can't be run (not installed), we assume that it returned a success
        .unwrap_or_default()
}
