//! Embedded standard library source files.

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
#[cfg(not(debug_assertions))]
use std::sync::OnceLock;
use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{sort_fs_paths, FileSystem, FileSystemEntryKind};

const STDLIB_SOURCE_FINGERPRINT_NAMESPACE: &str = "vo-stdlib-source-v1";
#[cfg(not(debug_assertions))]
static STDLIB_SOURCE_FINGERPRINT: OnceLock<String> = OnceLock::new();

/// Embedded stdlib filesystem containing the canonical source assets.
#[derive(Debug, Clone)]
pub struct EmbeddedStdlib {
    files: HashMap<PathBuf, String>,
}

impl Default for EmbeddedStdlib {
    fn default() -> Self {
        Self::new()
    }
}

impl EmbeddedStdlib {
    pub fn new() -> Self {
        let mut files = HashMap::new();
        for file_path in vo_stdlib_source::iter() {
            if let Some(content) = vo_stdlib_source::get(file_path.as_ref()) {
                if let Ok(s) = std::str::from_utf8(content.as_ref()) {
                    files.insert(PathBuf::from(file_path.to_string()), s.to_string());
                }
            }
        }
        Self { files }
    }

    /// Returns the content identity of this exact standard-library snapshot.
    ///
    /// Computing the identity from `self.files` lets the compiler use the same
    /// snapshot for cache lookup and, after a miss, package resolution. This is
    /// important in debug builds where `vo-stdlib-source` observes source-tree
    /// edits made while a Studio or daemon process remains alive.
    pub fn source_fingerprint(&self) -> String {
        fingerprint_assets(self.files.iter().map(|(path, source)| {
            (
                path.to_str()
                    .expect("standard-library asset paths are valid UTF-8"),
                source.as_bytes(),
            )
        }))
    }

    /// Returns the cached identity of the immutable release asset view.
    ///
    /// Release builds embed all standard-library bytes in the executable, so
    /// their identity cannot change during the process lifetime. Keeping this
    /// cache release-only avoids rebuilding a full source map on every compile
    /// cache hit while preserving live-edit behavior in debug builds.
    #[cfg(not(debug_assertions))]
    pub fn immutable_source_fingerprint() -> &'static str {
        STDLIB_SOURCE_FINGERPRINT
            .get_or_init(|| Self::new().source_fingerprint())
            .as_str()
    }
}

fn fingerprint_assets<I, P, B>(assets: I) -> String
where
    I: IntoIterator<Item = (P, B)>,
    P: AsRef<str>,
    B: AsRef<[u8]>,
{
    let mut assets: Vec<_> = assets.into_iter().collect();
    assets.sort_by(|left, right| left.0.as_ref().cmp(right.0.as_ref()));

    let mut hasher = StableHasher::new(STDLIB_SOURCE_FINGERPRINT_NAMESPACE);
    hasher.update_str("asset_count", &assets.len().to_string());
    for (path, bytes) in assets {
        hasher.update_str("asset_path", path.as_ref());
        hasher.update_bytes("asset_bytes", bytes.as_ref());
    }
    hasher.finish()
}

impl FileSystem for EmbeddedStdlib {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        self.files.get(path).cloned().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("file not found: {:?}", path),
            )
        })
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.files
            .get(path)
            .map(|s| s.as_bytes().to_vec())
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("file not found: {:?}", path),
                )
            })
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        let source = self.files.get(path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, format!("file not found: {path:?}"))
        })?;
        if source.len() > max_bytes {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "embedded file size {} exceeds the {max_bytes}-byte limit",
                    source.len()
                ),
            ));
        }
        let mut bytes = Vec::new();
        bytes
            .try_reserve_exact(source.len())
            .map_err(|_| io::Error::other("failed to allocate embedded file buffer"))?;
        bytes.extend_from_slice(source.as_bytes());
        Ok(bytes)
    }

    fn read_text_limited(&self, path: &Path, max_bytes: usize) -> io::Result<String> {
        let source = self.files.get(path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, format!("file not found: {path:?}"))
        })?;
        if source.len() > max_bytes {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "embedded text file size {} exceeds the {max_bytes}-byte limit",
                    source.len()
                ),
            ));
        }
        let mut text = String::new();
        text.try_reserve_exact(source.len())
            .map_err(|_| io::Error::other("failed to allocate embedded text buffer"))?;
        text.push_str(source);
        Ok(text)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut seen = HashSet::new();
        let is_root =
            path == Path::new(".") || path == Path::new("") || path.as_os_str().is_empty();

        for file_path in self.files.keys() {
            let entry = if is_root {
                file_path
                    .components()
                    .next()
                    .map(|c| PathBuf::from(c.as_os_str()))
            } else {
                let path_str = format!("{}/", path.to_string_lossy());
                let file_str = file_path.to_string_lossy();
                if file_str.starts_with(&path_str) {
                    let rest = &file_str[path_str.len()..];
                    if !rest.is_empty() {
                        Some(if let Some(idx) = rest.find('/') {
                            path.join(&rest[..idx])
                        } else {
                            path.join(rest)
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
            };
            if let Some(e) = entry {
                seen.insert(e);
            }
        }
        let mut entries: Vec<_> = seen.into_iter().collect();
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        if self.files.contains_key(path) {
            return true;
        }
        if path == Path::new(".") || path == Path::new("") {
            return !self.files.is_empty();
        }
        let path_str = path.to_string_lossy();
        self.files.keys().any(|p| {
            let p_str = p.to_string_lossy();
            p_str.starts_with(&*path_str)
                && p_str.len() > path_str.len()
                && p_str.chars().nth(path_str.len()) == Some('/')
        })
    }

    fn is_dir(&self, path: &Path) -> bool {
        !self.files.contains_key(path) && self.exists(path)
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        Ok(if self.files.contains_key(path) {
            FileSystemEntryKind::RegularFile
        } else if self.exists(path) {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::Missing
        })
    }
}

impl vo_common_core::SourceProvider for EmbeddedStdlib {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

/// Type alias for clarity.
pub type StdlibFs = EmbeddedStdlib;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_fingerprint_matches_the_resolver_snapshot() {
        let stdlib = EmbeddedStdlib::new();
        let resolver_assets: Vec<_> = stdlib
            .files
            .iter()
            .map(|(path, source)| {
                (
                    path.to_str().expect("embedded paths are UTF-8").to_string(),
                    source.as_bytes().to_vec(),
                )
            })
            .collect();
        assert_eq!(
            stdlib.source_fingerprint(),
            fingerprint_assets(resolver_assets)
        );
    }

    #[test]
    fn source_fingerprint_observes_fixture_changes_in_the_same_process() {
        let mut stdlib = EmbeddedStdlib {
            files: HashMap::from([
                (PathBuf::from("a/a.vo"), "package a".to_string()),
                (PathBuf::from("b/b.vo"), "package b".to_string()),
            ]),
        };
        let original = stdlib.source_fingerprint();
        assert_eq!(original, stdlib.source_fingerprint());

        stdlib.files.insert(
            PathBuf::from("b/b.vo"),
            "package b\nconst Changed = true".to_string(),
        );
        let changed = stdlib.source_fingerprint();

        assert_ne!(original, changed);
    }

    #[test]
    fn limited_reads_reject_before_copying_embedded_content() {
        let stdlib = EmbeddedStdlib {
            files: HashMap::from([(PathBuf::from("demo.vo"), "package demo".to_string())]),
        };

        let bytes_error = stdlib
            .read_bytes_limited(Path::new("demo.vo"), 4)
            .expect_err("byte limit must be enforced");
        assert_eq!(bytes_error.kind(), io::ErrorKind::InvalidData);

        let text_error = stdlib
            .read_text_limited(Path::new("demo.vo"), 4)
            .expect_err("text limit must be enforced");
        assert_eq!(text_error.kind(), io::ErrorKind::InvalidData);
        assert_eq!(
            stdlib
                .read_text_limited(Path::new("demo.vo"), "package demo".len())
                .unwrap(),
            "package demo"
        );
    }

    #[test]
    fn source_fingerprint_is_order_independent_and_path_sensitive() {
        let left = fingerprint_assets(vec![
            ("a/a.vo".to_string(), b"package a".to_vec()),
            ("b/b.vo".to_string(), b"package b".to_vec()),
        ]);
        let reordered = fingerprint_assets(vec![
            ("b/b.vo".to_string(), b"package b".to_vec()),
            ("a/a.vo".to_string(), b"package a".to_vec()),
        ]);
        let renamed = fingerprint_assets(vec![
            ("a/renamed.vo".to_string(), b"package a".to_vec()),
            ("b/b.vo".to_string(), b"package b".to_vec()),
        ]);
        let changed = fingerprint_assets(vec![
            ("a/a.vo".to_string(), b"package changed".to_vec()),
            ("b/b.vo".to_string(), b"package b".to_vec()),
        ]);

        assert_eq!(left, reordered);
        assert_ne!(left, renamed);
        assert_ne!(left, changed);
    }
}
