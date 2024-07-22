//! Note: Paths here are Unix-like when converted to [`String`]s (the root directory is `/`, and the directory separator is `/`),
//! but DOS-like paths are also accepted and converted to Unix-like when pushed
//!
//! It is possible to push forbidden characters to a [`PathBuf`], doing so however will make most functions
//! return a [`MalformedPath`](crate::error::FSError::MalformedPath) error
//!

#[cfg(not(feature = "std"))]
use core::*;
#[cfg(feature = "std")]
use std::*;

#[cfg(not(feature = "std"))]
use ::alloc::{
    borrow::ToOwned,
    collections::vec_deque::VecDeque,
    string::{String, ToString},
    vec::Vec,
};

#[cfg(feature = "std")]
use std::collections::VecDeque;

// A (incomplete) list of all the forbidden filename/directory name characters
// See https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file?redirectedfrom=MSDN

/// A list of all the characters that are forbidden to exist in a filename
pub const FORBIDDEN_CHARS: &[char] = &['<', '>', ':', '"', '/', '\\', ',', '?', '*'];
/// A list of all filenames that are reserved in Windows (and should probably also not be used by FAT)
pub const RESERVED_FILENAMES: &[&str] = &[
    "CON", "PRN", "AUX", "NUL", "COM0", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7",
    "COM8", "COM9", "COM¹", "COM²", "COM³", "LPT0", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6",
    "LPT7", "LPT8", "LPT9", "LPT¹", "LPT²", "LPT³",
];

/// Check whether a [`PathBuf`] is forbidden for use in filenames or directory names
pub fn is_forbidden(pathbuf: &PathBuf) -> bool {
    for subpath in pathbuf.clone() {
        if subpath
            .chars()
            .any(|c| c.is_control() || FORBIDDEN_CHARS.contains(&c))
        {
            return true;
        }
    }

    if let Some(file_name) = pathbuf.file_name() {
        if RESERVED_FILENAMES
            .iter()
            // remove extension
            .map(|file_name| file_name.split_once(".").map(|s| s.0).unwrap_or(file_name))
            .any(|reserved| file_name == reserved)
        {
            return true;
        }
    }

    false
}

#[derive(Debug, Clone)]
pub struct PathBuf {
    inner: VecDeque<String>,
}

impl PathBuf {
    pub fn new() -> Self {
        Self::default()
    }

    // Seperate by "\" or "/" and push into inner vector
    pub fn push<P>(&mut self, subpath: P)
    where
        P: ToString,
    {
        let subpath = subpath.to_string();

        let mut split_subpath: VecDeque<&str> =
            subpath.split(|c| ['\\', '/'].contains(&c)).collect();

        // remove trailing slash in the beginning of split_subpath..
        if split_subpath.front().map(|s| s.is_empty()).unwrap_or(false) {
            split_subpath.pop_front();
        };
        // ...as well in the beginning of the inner deque
        if self.inner.back().map(|s| s.is_empty()).unwrap_or(false) {
            self.inner.pop_back();
        };

        for p in split_subpath {
            match p {
                "." => continue,
                ".." => {
                    self.inner.pop_back();
                    continue;
                }
                _ => self.inner.push_back(p.to_string()),
            }
        }
    }

    // Assumes that the [`PathBuf`] isn't malformed
    pub fn file_name(&self) -> Option<String> {
        if let Some(file_name) = self.inner.back() {
            if !file_name.is_empty() {
                return Some(file_name.to_owned());
            }
        }

        None
    }

    /// Returns the parent directory of the current [`PathBuf`]
    pub fn parent(&self) -> PathBuf {
        if self.inner.len() > 1 {
            let mut pathbuf = self.clone();
            pathbuf.inner.back_mut().unwrap().clear();
            pathbuf
        } else {
            PathBuf::new()
        }
    }

    pub fn is_dir(&self) -> bool {
        match self.inner.len() {
            // root directory
            0 => true,
            n => self.inner[n - 1].is_empty(),
        }
    }

    pub fn is_file(&self) -> bool {
        !self.is_dir()
    }

    /// Resets the [`PathBuf`]
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    /// Checks whether this [`PathBuf`] is malformed (as if someone pushed a string with many consecutive slashes)
    pub fn is_malformed(&self) -> bool {
        is_forbidden(&self)
    }
}

impl From<String> for PathBuf {
    fn from(value: String) -> Self {
        let mut pathbuf = PathBuf::new();
        pathbuf.push(value);

        pathbuf
    }
}

impl From<&str> for PathBuf {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

impl fmt::Display for PathBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "/{}",
            self.inner.iter().cloned().collect::<Vec<_>>().join("/")
        )
    }
}

impl Default for PathBuf {
    fn default() -> Self {
        Self {
            inner: VecDeque::new(),
        }
    }
}

impl iter::Iterator for PathBuf {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        // for this to work correctly, the PathBuf mustn't be malformed
        self.inner.pop_front().filter(|s| !s.is_empty())
    }
}

#[test]
fn empty_pathbuf_tostring() {
    let pathbuf = PathBuf::new();

    assert_eq!(pathbuf.to_string(), "/");
    assert!(!pathbuf.is_malformed());
}

#[test]
fn catch_invalid_path() {
    #[cfg(not(feature = "std"))]
    use ::alloc::format;

    let mut pathbuf = PathBuf::new();

    // ignore path separators
    for filename in RESERVED_FILENAMES {
        pathbuf.push(format!("/{filename}"));

        assert!(
            pathbuf.is_malformed(),
            "unable to detect invalid filename {}",
            pathbuf
        );

        pathbuf.clear();
    }
}

#[test]
fn catch_non_control_forbidden_chars() {
    #[cfg(not(feature = "std"))]
    use ::alloc::format;

    let mut pathbuf = PathBuf::new();

    // ignore path separators
    const PATH_SEPARATORS: &[char] = &['/', '\\'];
    for c in FORBIDDEN_CHARS
        .iter()
        .filter(|c| !PATH_SEPARATORS.contains(c))
    {
        pathbuf.push(format!("/{c}"));

        assert!(
            pathbuf.is_malformed(),
            "unable to detect character {} (hex: {:#02x}) as forbidden {:?}",
            c,
            (*c as usize),
            pathbuf
        );

        pathbuf.clear();
    }
}

#[test]
fn ignore_multiple_separators() {
    let mut pathbuf = PathBuf::new();

    pathbuf.push("first/");
    pathbuf.push("second\\");
    pathbuf.push("/third\\");
    pathbuf.push("\\last/");

    assert_eq!(pathbuf.to_string(), "/first/second/third/last/")
}

#[test]
fn push_to_pathbuf() {
    let mut pathbuf = PathBuf::new();

    pathbuf.push("foo");
    pathbuf.push("bar/test");
    pathbuf.push("bar2\\test2");
    pathbuf.push("ignored\\../.");
    pathbuf.push("\\fintest1");
    pathbuf.push("fintest2/");
    pathbuf.push("last");

    assert_eq!(
        pathbuf.to_string(),
        "/foo/bar/test/bar2/test2/fintest1/fintest2/last"
    )
}
