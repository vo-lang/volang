//! Immutable filesystem snapshots used by cache-aware compilation.

use std::collections::BTreeMap;
use std::io;
use std::ops::Bound::{Excluded, Unbounded};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use vo_common::vfs::{
    normalize_fs_path, sort_fs_paths, FileSystem, FileSystemEntryKind, ScopedFs,
    MAX_DIRECTORY_ENTRIES, MAX_TEXT_FILE_BYTES,
};

const MAX_NATIVE_BUILD_INPUT_BYTES: usize = 64 * 1024 * 1024;
const MAX_NATIVE_ARTIFACT_BYTES: usize = vo_module::MAX_MODULE_ARTIFACT_BYTES_USIZE;
const MAX_OTHER_COMPILE_INPUT_BYTES: usize = 64 * 1024 * 1024;
pub(super) const MAX_COMPILE_SNAPSHOT_BYTES: usize = 512 * 1024 * 1024;
pub(super) const MAX_COMPILE_SNAPSHOT_FILES: usize = 100_000;

/// A byte-preserving, immutable view of every file that participates in one
/// cache-aware compilation. Paths are normalized before insertion and lookup.
#[derive(Debug, Default)]
pub(super) struct CompileInputSnapshot {
    files: BTreeMap<PathBuf, Vec<u8>>,
    directory_identities: BTreeMap<PathBuf, [u8; 24]>,
    total_bytes: usize,
}

impl CompileInputSnapshot {
    /// Path ordering compares components, so strict descendants form the
    /// contiguous range immediately after a directory. The existing file map
    /// supplies the index; this view owns no paths or additional metadata.
    fn descendant_files<'a>(&'a self, directory: &'a Path) -> impl Iterator<Item = &'a Path> {
        self.files
            .range::<Path, _>((Excluded(directory), Unbounded))
            .map(|(file, _)| file.as_path())
            .take_while(move |file| file.starts_with(directory))
    }

    pub(super) fn contains_file(&self, path: &Path) -> bool {
        self.files.contains_key(&normalize_fs_path(path))
    }

    pub(super) fn remaining_bytes(&self) -> usize {
        MAX_COMPILE_SNAPSHOT_BYTES.saturating_sub(self.total_bytes)
    }

    pub(super) fn remaining_files(&self) -> usize {
        MAX_COMPILE_SNAPSHOT_FILES.saturating_sub(self.files.len())
    }

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

    pub(super) fn record_host_parent_directory_identity(&mut self, path: &Path) -> io::Result<()> {
        let Some(parent) = path.parent() else {
            return Ok(());
        };
        let parent = normalize_fs_path(parent);
        let generation = super::host_input::validate_stable_directory_path(&parent)?;
        self.record_directory_identity(&parent, &generation.identity)
    }

    /// Retain the identity of the directory capability used for a tree read.
    /// The collector validates that capability's generation against its live
    /// path before returning the completed tree to the compiler.
    pub(super) fn record_directory_identity(
        &mut self,
        parent: &Path,
        observed: &super::host_input::HostEntryIdentity,
    ) -> io::Result<()> {
        let parent = normalize_fs_path(parent);
        let mut identity = [0; 24];
        identity[..8].copy_from_slice(&observed.volume.to_le_bytes());
        identity[8..].copy_from_slice(&observed.file);
        if let Some(existing) = self.directory_identities.get(&parent) {
            if existing != &identity {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "compile input directory {} changed identity while capturing aliases",
                        parent.display()
                    ),
                ));
            }
            return Ok(());
        }
        self.directory_identities.insert(parent, identity);
        Ok(())
    }

    /// Reads a path at most once for this compilation and returns the retained
    /// bytes on every later encounter, including overlapping project/workspace-source
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

    pub(super) fn insert_consistent(&mut self, path: PathBuf, bytes: Vec<u8>) -> io::Result<&[u8]> {
        let path = normalize_fs_path(&path);
        if self.files.contains_key(&path) {
            if self
                .files
                .get(&path)
                .is_some_and(|existing| existing != &bytes)
            {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "compile input at {} changed between overlapping capability walks",
                        path.display()
                    ),
                ));
            }
            return Ok(self
                .files
                .get(&path)
                .expect("existing compile input must be present"));
        }
        self.insert(path.clone(), bytes)?;
        Ok(self
            .files
            .get(&path)
            .expect("inserted compile input must be present"))
    }
}

fn read_bounded_compile_input(path: &Path) -> io::Result<Vec<u8>> {
    let max_bytes = compile_input_limit(path);
    let bytes = super::host_input::read_stable_regular_file(path, max_bytes)?;
    validate_compile_input_size(path, bytes.len())?;
    Ok(bytes)
}

pub(super) fn validate_compile_input_size(path: &Path, len: usize) -> io::Result<()> {
    let max_bytes = compile_input_limit(path);
    if len > max_bytes {
        return Err(compile_input_size_error(path, len, max_bytes));
    }
    Ok(())
}

pub(super) fn compile_input_limit(path: &Path) -> usize {
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
        let mut entries: Vec<PathBuf> = Vec::new();
        for file in self.descendant_files(&path) {
            // All files below one immediate child are adjacent in the map.
            // Allocate its output path once, even for a large nested package.
            if entries.last().is_some_and(|entry| file.starts_with(entry)) {
                continue;
            }
            let relative = file.strip_prefix(&path).expect("descendant prefix");
            let first = relative.components().next().expect("strict descendant");
            entries.push(normalize_fs_path(&path.join(first.as_os_str())));
            if entries.len() > MAX_DIRECTORY_ENTRIES {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "compile snapshot directory contains more than {MAX_DIRECTORY_ENTRIES} entries"
                    ),
                ));
            }
        }
        if entries.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("directory not found in compile snapshot: {path:?}"),
            ));
        }
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        let path = normalize_fs_path(path);
        self.files.contains_key(&path) || self.descendant_files(&path).next().is_some()
    }

    fn is_dir(&self, path: &Path) -> bool {
        let path = normalize_fs_path(path);
        !self.files.contains_key(&path) && self.descendant_files(&path).next().is_some()
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        let path = normalize_fs_path(path);
        Ok(if self.files.contains_key(&path) {
            FileSystemEntryKind::RegularFile
        } else if self.descendant_files(&path).next().is_some() {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::Missing
        })
    }

    fn opaque_directory_identity(&self, path: &Path) -> io::Result<Option<Vec<u8>>> {
        let path = normalize_fs_path(path);
        Ok(self
            .directory_identities
            .get(&path)
            .map(|identity| identity.to_vec()))
    }
}

/// Filesystem used for dependency and workspace-source resolution. Both variants
/// have the same concrete type so the pipeline can select live or captured
/// inputs without duplicating its generic analysis path.
#[derive(Clone)]
pub(super) enum ResolverFs {
    Snapshot(ScopedFs<Arc<CompileInputSnapshot>>),
    SnapshotGlobal(Arc<CompileInputSnapshot>),
}

impl ResolverFs {
    pub(super) fn snapshot(snapshot: Arc<CompileInputSnapshot>, root: impl Into<PathBuf>) -> Self {
        Self::Snapshot(ScopedFs::new(snapshot, root))
    }

    /// Exposes captured paths in their original absolute namespace.
    ///
    /// Workspace source roots are canonical absolute paths. Scoping a
    /// snapshot at `.` would reject those paths before lookup, even though all
    /// workspace source bytes were captured. The global view remains immutable and
    /// can only expose files present in the snapshot.
    pub(super) fn snapshot_global(snapshot: Arc<CompileInputSnapshot>) -> Self {
        Self::SnapshotGlobal(snapshot)
    }
}

impl FileSystem for ResolverFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        match self {
            Self::Snapshot(fs) => fs.read_file(path),
            Self::SnapshotGlobal(fs) => fs.read_file(path),
        }
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        match self {
            Self::Snapshot(fs) => fs.read_bytes(path),
            Self::SnapshotGlobal(fs) => fs.read_bytes(path),
        }
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        match self {
            Self::Snapshot(fs) => fs.read_bytes_limited(path, max_bytes),
            Self::SnapshotGlobal(fs) => fs.read_bytes_limited(path, max_bytes),
        }
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        match self {
            Self::Snapshot(fs) => fs.read_dir(path),
            Self::SnapshotGlobal(fs) => fs.read_dir(path),
        }
    }

    fn exists(&self, path: &Path) -> bool {
        match self {
            Self::Snapshot(fs) => fs.exists(path),
            Self::SnapshotGlobal(fs) => fs.exists(path),
        }
    }

    fn is_dir(&self, path: &Path) -> bool {
        match self {
            Self::Snapshot(fs) => fs.is_dir(path),
            Self::SnapshotGlobal(fs) => fs.is_dir(path),
        }
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        match self {
            Self::Snapshot(fs) => fs.entry_kind(path),
            Self::SnapshotGlobal(fs) => fs.entry_kind(path),
        }
    }

    fn opaque_directory_identity(&self, path: &Path) -> io::Result<Option<Vec<u8>>> {
        match self {
            Self::Snapshot(fs) => fs.opaque_directory_identity(path),
            Self::SnapshotGlobal(fs) => fs.opaque_directory_identity(path),
        }
    }

    fn root(&self) -> Option<&Path> {
        match self {
            Self::Snapshot(fs) => FileSystem::root(fs),
            Self::SnapshotGlobal(fs) => FileSystem::root(fs),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_directory(
        snapshot: &CompileInputSnapshot,
        path: &Path,
    ) -> Result<Vec<PathBuf>, io::ErrorKind> {
        use std::collections::BTreeSet;
        let path = normalize_fs_path(path);
        if snapshot.files.contains_key(&path) {
            return Err(io::ErrorKind::NotADirectory);
        }
        let mut entries = BTreeSet::new();
        for file in snapshot.files.keys() {
            if let Ok(relative) = file.strip_prefix(&path) {
                if let Some(first) = relative.components().next() {
                    entries.insert(normalize_fs_path(&path.join(first.as_os_str())));
                }
            }
        }
        if entries.is_empty() {
            return Err(io::ErrorKind::NotFound);
        }
        if entries.len() > MAX_DIRECTORY_ENTRIES {
            return Err(io::ErrorKind::InvalidData);
        }
        let mut entries: Vec<_> = entries.into_iter().collect();
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn assert_scan_equivalent(snapshot: &CompileInputSnapshot, paths: &[PathBuf]) {
        for path in paths {
            let normalized = normalize_fs_path(path);
            let is_file = snapshot.files.contains_key(&normalized);
            let has_children = snapshot
                .files
                .keys()
                .any(|file| file != &normalized && file.starts_with(&normalized));
            assert_eq!(snapshot.exists(path), is_file || has_children, "{path:?}");
            assert_eq!(snapshot.is_dir(path), !is_file && has_children, "{path:?}");
            let expected_kind = if is_file {
                FileSystemEntryKind::RegularFile
            } else if has_children {
                FileSystemEntryKind::Directory
            } else {
                FileSystemEntryKind::Missing
            };
            assert_eq!(
                snapshot.entry_kind(path).unwrap(),
                expected_kind,
                "{path:?}"
            );
            assert_eq!(
                snapshot.read_dir(path).map_err(|error| error.kind()),
                scan_directory(snapshot, path),
                "{path:?}"
            );
        }
    }

    #[test]
    fn directory_ranges_preserve_component_boundaries_and_normalization() {
        let mut snapshot = CompileInputSnapshot::default();
        let roots = [
            "/project",
            "/project-2",
            "project",
            "project-2",
            "..",
            "../..",
        ];
        let suffixes = [
            "a.vo",
            "a.vo/child.vo",
            "a/b.vo",
            "a/nested/c.vo",
            "a-2/d.vo",
            "a0/e.vo",
            "z.vo",
            "中文/源码.vo",
            "space name/one.vo",
        ];
        let mut probes = vec![
            PathBuf::from(""),
            PathBuf::from("."),
            PathBuf::from("/"),
            PathBuf::from("missing"),
        ];
        for root in roots {
            for suffix in suffixes {
                let path = Path::new(root).join(suffix);
                snapshot.insert(path.clone(), vec![0, 255]).unwrap();
                probes.extend(path.ancestors().map(Path::to_path_buf));
                probes.push(path.with_extension("missing"));
            }
            probes.push(Path::new(root).join("a/../a/./"));
            probes.push(Path::new(root).join("a"));
            probes.push(Path::new(root).join("a-"));
        }
        snapshot
            .insert(PathBuf::from("project/a/../z.vo"), vec![1])
            .unwrap();
        // Identity records do not synthesize filesystem entries or override a file.
        snapshot
            .directory_identities
            .insert(PathBuf::from("empty"), [7; 24]);
        snapshot
            .directory_identities
            .insert(PathBuf::from("project/a.vo"), [9; 24]);
        probes.push(PathBuf::from("empty"));
        assert_scan_equivalent(&snapshot, &probes);
        assert_eq!(
            snapshot
                .opaque_directory_identity(Path::new("empty"))
                .unwrap(),
            Some(vec![7; 24])
        );
        assert_eq!(
            snapshot.read_bytes(Path::new("project/z.vo")).unwrap(),
            vec![1]
        );
    }

    #[test]
    fn directory_ranges_match_full_scan_across_generated_package_trees() {
        let mut snapshot = CompileInputSnapshot::default();
        let mut probes = Vec::new();
        for package in 0..97 {
            let root = PathBuf::from(format!("/workspace/pkg{package:03}"));
            probes.push(root.clone());
            probes.push(root.with_extension("missing"));
            for file in 0..17 {
                let path = root.join(format!("dir{}/nested{}/file{file}.vo", file % 7, file % 3));
                snapshot.insert(path.clone(), Vec::new()).unwrap();
                probes.extend(path.ancestors().take(3).map(Path::to_path_buf));
            }
        }
        probes.sort();
        probes.dedup();
        assert_scan_equivalent(&snapshot, &probes);
    }

    #[test]
    fn directory_range_keeps_entry_limit_and_deduplicates_nested_files() {
        let mut snapshot = CompileInputSnapshot::default();
        let root = Path::new("/directory-limit");
        // Construct an oversized internal image to exercise the defensive
        // directory limit independently of the normal snapshot file limit.
        for n in 0..MAX_DIRECTORY_ENTRIES {
            snapshot
                .files
                .insert(root.join(format!("child{n}/first.vo")), Vec::new());
        }
        snapshot
            .files
            .insert(root.join("child0/second.vo"), Vec::new());
        assert_eq!(
            snapshot.read_dir(root).unwrap().len(),
            MAX_DIRECTORY_ENTRIES
        );
        snapshot.files.insert(root.join("one-more.vo"), Vec::new());
        assert_eq!(
            snapshot.read_dir(root).unwrap_err().kind(),
            io::ErrorKind::InvalidData
        );
        assert_eq!(
            snapshot.read_dir(Path::new("/absent")).unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
    }

    #[cfg(unix)]
    #[test]
    fn directory_ranges_preserve_non_utf8_names() {
        use std::ffi::OsString;
        use std::os::unix::ffi::OsStringExt;
        let mut snapshot = CompileInputSnapshot::default();
        let mut probes = vec![PathBuf::from("/binary")];
        for byte in [0x7f, 0x80, 0xfe, 0xff] {
            let directory = Path::new("/binary").join(OsString::from_vec(vec![b'p', byte]));
            snapshot.insert(directory.join("a.vo"), Vec::new()).unwrap();
            snapshot
                .insert(directory.join("b/c.vo"), Vec::new())
                .unwrap();
            probes.push(directory);
        }
        assert_scan_equivalent(&snapshot, &probes);
    }

    fn canonical_temp_dir() -> PathBuf {
        std::env::temp_dir()
            .canonicalize()
            .unwrap_or_else(|_| std::env::temp_dir())
    }

    #[test]
    fn snapshot_preserves_binary_files_and_directory_shape() {
        let root = canonical_temp_dir().join("vo-engine-snapshot-test");
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
    fn snapshot_preserves_authenticated_parent_directory_identity() {
        let root = canonical_temp_dir().join(format!(
            "vo-engine-snapshot-directory-identity-{}",
            std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        let first = root.join("first.vo");
        let second = root.join("second.vo");
        std::fs::write(&first, "package sample\n").unwrap();
        std::fs::write(&second, "package sample\n").unwrap();
        let mut snapshot = CompileInputSnapshot::default();

        snapshot
            .record_host_parent_directory_identity(&first)
            .unwrap();
        snapshot
            .record_host_parent_directory_identity(&second)
            .unwrap();

        let identity = snapshot
            .opaque_directory_identity(&root)
            .unwrap()
            .expect("captured parent identity");
        assert_eq!(identity.len(), 24);
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn capture_file_retains_the_first_observed_bytes() {
        let path =
            canonical_temp_dir().join(format!("vo-engine-capture-once-{}", std::process::id()));
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
            canonical_temp_dir().join(format!("vo-engine-oversized-source-{}", std::process::id()));
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
        let root = canonical_temp_dir().join(format!(
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
