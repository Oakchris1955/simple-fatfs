mod common;
use common::*;

use rstest::*;
use rstest_reuse::*;
use test_log::test;

#[test]
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

#[test]
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

#[test]
#[apply(device)]
fn set_volume_label_bpb(mut device: MemoryDevice) {
    let fs = FileSystem::new(&mut device, FSOptions::new()).unwrap();

    fs.set_volume_label_bpb("DEADBEEF");

    drop(fs);

    let fs = FileSystem::new(&mut device, FSOptions::new()).unwrap();

    assert_eq!(fs.volume_label_bpb(), Some(String::from("DEADBEEF")));
}

#[test]
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
