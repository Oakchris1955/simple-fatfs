//! Re-exports for [`typed_path`]

pub(crate) use typed_path::{
    constants::windows as path_consts, Utf8Component as _, Utf8WindowsComponent as WindowsComponent,
};

pub use typed_path::{Utf8WindowsPath as Path, Utf8WindowsPathBuf as PathBuf};
