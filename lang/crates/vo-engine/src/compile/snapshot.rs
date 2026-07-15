//! Immutable filesystem snapshots used by cache-aware compilation.

use std::collections::{BTreeMap, BTreeSet};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use vo_common::vfs::{
    normalize_fs_path, sort_fs_paths, FileSystem, FileSystemEntryKind, RealFs, ScopedFs,
    MAX_DIRECTORY_ENTRIES, MAX_TEXT_FILE_BYTES,
};

const MAX_NATIVE_BUILD_INPUT_BYTES: usize = 64 * 1024 * 1024;
const MAX_NATIVE_ARTIFACT_BYTES: usize = 256 * 1024 * 1024;
const MAX_OTHER_COMPILE_INPUT_BYTES: usize = 64 * 1024 * 1024;
const MAX_COMPILE_SNAPSHOT_BYTES: usize = 512 * 1024 * 1024;
const MAX_COMPILE_SNAPSHOT_FILES: usize = 100_000;

/// A byte-preserving, immutable view of every file that participates in one
/// cache-aware compilation. Paths are normalized before insertion and lookup.
#[derive(Debug, Default)]
pub(super) struct CompileInputSnapshot {
    files: BTreeMap<PathBuf, Vec<u8>>,
    total_bytes: usize,
}

impl CompileInputSnapshot {
    pub(super) fn insert(&mut self, path: PathBuf, bytes: Vec<u8>) -> io::Result<()> {
        let path = normalize_fs_path(&path);
        validate_compile_input_size(&path, bytes.len())?;
        if !self.files.contains_key(&path) && self.files.len() >= MAX_COMPILE_SNAPSHOT_FILES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile input snapshot exceeds the {}-file limit",
                    MAX_COMPILE_SNAPSHOT_FILES,
                ),
            ));
        }
        let previous_len = self.files.get(&path).map_or(0, Vec::len);
        let total_bytes = self
            .total_bytes
            .checked_sub(previous_len)
            .and_then(|total| total.checked_add(bytes.len()))
            .ok_or_else(|| compile_snapshot_size_error(usize::MAX, MAX_COMPILE_SNAPSHOT_BYTES))?;
        if total_bytes > MAX_COMPILE_SNAPSHOT_BYTES {
            return Err(compile_snapshot_size_error(
                total_bytes,
                MAX_COMPILE_SNAPSHOT_BYTES,
            ));
        }
        self.files.insert(path, bytes);
        self.total_bytes = total_bytes;
        Ok(())
    }

    /// Reads a path at most once for this compilation and returns the retained
    /// bytes on every later encounter, including overlapping project/replace
    /// roots and an ancestor workspace file.
    pub(super) fn capture_file(&mut self, path: &Path) -> io::Result<&[u8]> {
        let path = normalize_fs_path(path);
        if !self.files.contains_key(&path) {
            let bytes = read_bounded_compile_input(&path)?;
            self.insert(path.clone(), bytes)?;
        }
        Ok(self
            .files
            .get(&path)
            .expect("captured path must be present")
            .as_slice())
    }
}

fn read_bounded_compile_input(path: &Path) -> io::Result<Vec<u8>> {
    let max_bytes = compile_input_limit(path);
    let max_bytes_u64 = u64::try_from(max_bytes).unwrap_or(u64::MAX);
    let linked_metadata = std::fs::symlink_metadata(path)?;
    if linked_metadata.file_type().is_symlink() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "compile input at {} must not be a symbolic link",
                path.display(),
            ),
        ));
    }
    if !linked_metadata.file_type().is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("compile input at {} is not a regular file", path.display()),
        ));
    }
    let file = std::fs::File::open(path)?;
    let opened_metadata = file.metadata()?;
    if !opened_metadata.file_type().is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("compile input at {} is not a regular file", path.display()),
        ));
    }
    let current_metadata = std::fs::symlink_metadata(path)?;
    if current_metadata.file_type().is_symlink() || !current_metadata.file_type().is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "compile input at {} changed file type while it was opened",
                path.display(),
            ),
        ));
    }
    if !same_file_identity(&opened_metadata, &current_metadata) {
        return Err(compile_input_identity_error(path));
    }
    if opened_metadata.len() > max_bytes_u64 {
        return Err(compile_input_size_error(
            path,
            max_bytes.saturating_add(1),
            max_bytes,
        ));
    }
    let mut bytes = Vec::new();
    (&file)
        .take(max_bytes_u64.saturating_add(1))
        .read_to_end(&mut bytes)?;
    validate_compile_input_size(path, bytes.len())?;
    let final_opened_metadata = file.metadata()?;
    let final_path_metadata = std::fs::symlink_metadata(path)?;
    if final_path_metadata.file_type().is_symlink()
        || !final_path_metadata.file_type().is_file()
        || !same_file_identity(&final_opened_metadata, &final_path_metadata)
        || !same_file_identity(&opened_metadata, &final_opened_metadata)
        || final_opened_metadata.len() != u64::try_from(bytes.len()).unwrap_or(u64::MAX)
    {
        return Err(compile_input_identity_error(path));
    }
    Ok(bytes)
}

fn compile_input_identity_error(path: &Path) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input at {} changed identity or length while it was read",
            path.display(),
        ),
    )
}

#[cfg(unix)]
fn same_file_identity(left: &std::fs::Metadata, right: &std::fs::Metadata) -> bool {
    use std::os::unix::fs::MetadataExt;

    left.dev() == right.dev() && left.ino() == right.ino()
}

#[cfg(windows)]
fn same_file_identity(left: &std::fs::Metadata, right: &std::fs::Metadata) -> bool {
    use std::os::windows::fs::MetadataExt;

    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    left.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT == 0
        && right.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT == 0
        && left.volume_serial_number() == right.volume_serial_number()
        && left.file_index() == right.file_index()
}

#[cfg(not(any(unix, windows)))]
fn same_file_identity(left: &std::fs::Metadata, right: &std::fs::Metadata) -> bool {
    left.file_type() == right.file_type()
        && left.len() == right.len()
        && left.modified().ok() == right.modified().ok()
}

fn validate_compile_input_size(path: &Path, len: usize) -> io::Result<()> {
    let max_bytes = compile_input_limit(path);
    if len > max_bytes {
        return Err(compile_input_size_error(path, len, max_bytes));
    }
    Ok(())
}

fn compile_input_limit(path: &Path) -> usize {
    if path.file_name() == Some(std::ffi::OsStr::new("vo.lock")) {
        return vo_module::MAX_LOCK_FILE_BYTES;
    }
    if is_text_compile_input(path) {
        return MAX_TEXT_FILE_BYTES;
    }
    if path.components().any(|component| {
        matches!(
            component,
            std::path::Component::Normal(name) if name == "artifacts"
        )
    }) || matches!(
        path.extension().and_then(|extension| extension.to_str()),
        Some("so") | Some("dylib") | Some("dll") | Some("wasm")
    ) {
        return MAX_NATIVE_ARTIFACT_BYTES;
    }
    if path.components().any(|component| {
        matches!(
            component,
            std::path::Component::Normal(name) if name == "rust"
        )
    }) {
        return MAX_NATIVE_BUILD_INPUT_BYTES;
    }
    MAX_OTHER_COMPILE_INPUT_BYTES
}

fn is_text_compile_input(path: &Path) -> bool {
    if matches!(
        path.extension().and_then(|extension| extension.to_str()),
        Some("vo") | Some("rs") | Some("toml") | Some("json")
    ) {
        return true;
    }
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("vo.mod")
            | Some("vo.lock")
            | Some("vo.work")
            | Some("vo.web.json")
            | Some("Cargo.lock")
            | Some("build.rs")
            | Some("rust-toolchain")
            | Some("config")
    )
}

fn compile_input_size_error(path: &Path, actual: usize, max: usize) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input at {} has size {} and exceeds the {}-byte limit",
            path.display(),
            actual,
            max,
        ),
    )
}

fn compile_snapshot_size_error(actual: usize, max: usize) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input snapshot has size {} and exceeds the {}-byte limit",
            actual, max,
        ),
    )
}

impl FileSystem for CompileInputSnapshot {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let path = normalize_fs_path(path);
        let bytes = self.files.get(&path).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("file not found in compile snapshot: {path:?}"),
            )
        })?;
        let max_bytes = compile_input_limit(&path);
        if bytes.len() > max_bytes {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile snapshot text file at {} has size {} and exceeds the {}-byte limit",
                    path.display(),
                    bytes.len(),
                    max_bytes,
                ),
            ));
        }
        std::str::from_utf8(bytes)
            .map(str::to_owned)
            .map_err(|error| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("source file is not valid UTF-8: {path:?}: {error}"),
                )
            })
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        let path = normalize_fs_path(path);
        self.files.get(&path).cloned().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("file not found in compile snapshot: {path:?}"),
            )
        })
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        let path = normalize_fs_path(path);
        let bytes = self.files.get(&path).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("file not found in compile snapshot: {path:?}"),
            )
        })?;
        if bytes.len() > max_bytes {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile snapshot file at {} has size {} and exceeds the {}-byte read limit",
                    path.display(),
                    bytes.len(),
                    max_bytes,
                ),
            ));
        }
        Ok(bytes.clone())
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let path = normalize_fs_path(path);
        if self.files.contains_key(&path) {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("path is not a directory in compile snapshot: {path:?}"),
            ));
        }
        let mut entries = BTreeSet::new();
        for file in self.files.keys() {
            let Ok(relative) = file.strip_prefix(&path) else {
                continue;
            };
            let Some(first) = relative.components().next() else {
                continue;
            };
            let entry = normalize_fs_path(&path.join(first.as_os_str()));
            entries.insert(entry);
        }
        if entries.is_empty() && !self.is_dir(&path) {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("directory not found in compile snapshot: {path:?}"),
            ));
        }
        if entries.len() > MAX_DIRECTORY_ENTRIES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile snapshot directory contains more than {MAX_DIRECTORY_ENTRIES} entries"
                ),
            ));
        }
        let mut entries = entries.into_iter().collect::<Vec<_>>();
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        let path = normalize_fs_path(path);
        self.files.contains_key(&path)
            || self
                .files
                .keys()
                .any(|file| file != &path && file.starts_with(&path))
    }

    fn is_dir(&self, path: &Path) -> bool {
        let path = normalize_fs_path(path);
        !self.files.contains_key(&path)
            && self
                .files
                .keys()
                .any(|file| file != &path && file.starts_with(&path))
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        let path = normalize_fs_path(path);
        Ok(if self.files.contains_key(&path) {
            FileSystemEntryKind::RegularFile
        } else if self
            .files
            .keys()
            .any(|file| file != &path && file.starts_with(&path))
        {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::Missing
        })
    }
}

/// Filesystem used for dependency and replacement resolution. Both variants
/// have the same concrete type so the pipeline can select live or captured
/// inputs without duplicating its generic analysis path.
#[derive(Clone)]
pub(super) enum ResolverFs {
    Live(RealFs),
    Snapshot(ScopedFs<Arc<CompileInputSnapshot>>),
    SnapshotGlobal(Arc<CompileInputSnapshot>),
}

impl ResolverFs {
    pub(super) fn live(root: impl Into<PathBuf>) -> Self {
        Self::Live(RealFs::new(root))
    }

    pub(super) fn snapshot(snapshot: Arc<CompileInputSnapshot>, root: impl Into<PathBuf>) -> Self {
        Self::Snapshot(ScopedFs::new(snapshot, root))
    }

    /// Exposes captured paths in their original absolute namespace.
    ///
    /// Workspace replacement roots are canonical absolute paths. Scoping a
    /// snapshot at `.` would reject those paths before lookup, even though all
    /// replacement bytes were captured. The global view remains immutable and
    /// can only expose files present in the snapshot.
    pub(super) fn snapshot_global(snapshot: Arc<CompileInputSnapshot>) -> Self {
        Self::SnapshotGlobal(snapshot)
    }
}

impl FileSystem for ResolverFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        match self {
            Self::Live(fs) => fs.read_file(path),
            Self::Snapshot(fs) => fs.read_file(path),
            Self::SnapshotGlobal(fs) => fs.read_file(path),
        }
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        match self {
            Self::Live(fs) => fs.read_bytes(path),
            Self::Snapshot(fs) => fs.read_bytes(path),
            Self::SnapshotGlobal(fs) => fs.read_bytes(path),
        }
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        match self {
            Self::Live(fs) => fs.read_bytes_limited(path, max_bytes),
            Self::Snapshot(fs) => fs.read_bytes_limited(path, max_bytes),
            Self::SnapshotGlobal(fs) => fs.read_bytes_limited(path, max_bytes),
        }
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        match self {
            Self::Live(fs) => fs.read_dir(path),
            Self::Snapshot(fs) => fs.read_dir(path),
            Self::SnapshotGlobal(fs) => fs.read_dir(path),
        }
    }

    fn exists(&self, path: &Path) -> bool {
        match self {
            Self::Live(fs) => fs.exists(path),
            Self::Snapshot(fs) => fs.exists(path),
            Self::SnapshotGlobal(fs) => fs.exists(path),
        }
    }

    fn is_dir(&self, path: &Path) -> bool {
        match self {
            Self::Live(fs) => fs.is_dir(path),
            Self::Snapshot(fs) => fs.is_dir(path),
            Self::SnapshotGlobal(fs) => fs.is_dir(path),
        }
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        match self {
            Self::Live(fs) => fs.entry_kind(path),
            Self::Snapshot(fs) => fs.entry_kind(path),
            Self::SnapshotGlobal(fs) => fs.entry_kind(path),
        }
    }

    fn root(&self) -> Option<&Path> {
        match self {
            Self::Live(fs) => FileSystem::root(fs),
            Self::Snapshot(fs) => FileSystem::root(fs),
            Self::SnapshotGlobal(fs) => FileSystem::root(fs),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_preserves_binary_files_and_directory_shape() {
        let root = std::env::temp_dir().join("vo-engine-snapshot-test");
        let mut snapshot = CompileInputSnapshot::default();
        snapshot
            .insert(root.join("pkg/main.vo"), b"package pkg\n".to_vec())
            .unwrap();
        snapshot
            .insert(root.join("artifacts/lib.bin"), vec![0, 255, 1])
            .unwrap();

        assert_eq!(
            snapshot.read_file(&root.join("pkg/main.vo")).unwrap(),
            "package pkg\n"
        );
        assert_eq!(
            snapshot
                .read_bytes(&root.join("artifacts/lib.bin"))
                .unwrap(),
            vec![0, 255, 1]
        );
        assert_eq!(
            snapshot
                .read_bytes_limited(&root.join("artifacts/lib.bin"), 2)
                .unwrap_err()
                .kind(),
            io::ErrorKind::InvalidData,
        );
        assert!(snapshot.is_dir(&root.join("pkg")));
        assert_eq!(
            snapshot.read_dir(&root).unwrap(),
            vec![root.join("artifacts"), root.join("pkg")]
        );
    }

    #[test]
    fn capture_file_retains_the_first_observed_bytes() {
        let path =
            std::env::temp_dir().join(format!("vo-engine-capture-once-{}", std::process::id()));
        std::fs::write(&path, b"first").unwrap();
        let mut snapshot = CompileInputSnapshot::default();
        assert_eq!(snapshot.capture_file(&path).unwrap(), b"first");

        std::fs::write(&path, b"second").unwrap();
        assert_eq!(snapshot.capture_file(&path).unwrap(), b"first");

        std::fs::remove_file(path).unwrap();
    }

    #[test]
    fn capture_file_rejects_oversized_text_before_reading_it() {
        let root =
            std::env::temp_dir().join(format!("vo-engine-oversized-source-{}", std::process::id()));
        std::fs::create_dir_all(&root).unwrap();
        let path = root.join("main.vo");
        let file = std::fs::File::create(&path).unwrap();
        file.set_len(u64::try_from(MAX_TEXT_FILE_BYTES).unwrap() + 1)
            .unwrap();

        let error = CompileInputSnapshot::default()
            .capture_file(&path)
            .unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn capture_file_rejects_oversized_native_input_before_reading_it() {
        let root = std::env::temp_dir().join(format!(
            "vo-engine-oversized-native-input-{}",
            std::process::id()
        ));
        let rust_dir = root.join("rust/assets");
        std::fs::create_dir_all(&rust_dir).unwrap();
        let path = rust_dir.join("input.bin");
        let file = std::fs::File::create(&path).unwrap();
        file.set_len(u64::try_from(MAX_NATIVE_BUILD_INPUT_BYTES).unwrap() + 1)
            .unwrap();

        let error = CompileInputSnapshot::default()
            .capture_file(&path)
            .unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);

        std::fs::remove_dir_all(root).unwrap();
    }
}
