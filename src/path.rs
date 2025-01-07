//! Re-exports for [`typed_path`]

pub(crate) use typed_path::{
    constants::windows as path_consts, Utf8Component as _, Utf8WindowsComponent as WindowsComponent,
};

pub use typed_path::{Utf8WindowsPath as Path, Utf8WindowsPathBuf as PathBuf};

// if the path is normalized, so this essentially only filters prefixes and the root directory
pub(crate) fn keep_path_normals(component: &WindowsComponent) -> bool {
    matches!(component, WindowsComponent::Normal(_))
}

pub(crate) fn find_common_path_prefix<P1, P2>(path1: P1, path2: P2) -> PathBuf
where
    P1: AsRef<Path>,
    P2: AsRef<Path>,
{
    let path1 = path1.as_ref().normalize();
    let path2 = path2.as_ref().normalize();

    let mut common_prefix_pathbuf = PathBuf::new();

    for (component1, component2) in path1
        .components()
        .filter(keep_path_normals)
        .zip(path2.components().filter(keep_path_normals))
    {
        if component1 != component2 {
            break;
        }

        // it doesn't matter whether we push component1 or component2
        common_prefix_pathbuf.push(component1);
    }

    common_prefix_pathbuf
}
