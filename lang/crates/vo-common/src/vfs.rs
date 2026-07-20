//! Virtual file system abstraction.

use std::collections::{BTreeMap, BTreeSet, HashMap};
#[cfg(feature = "zip")]
use std::io::Seek;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use vo_common_core::SourceProvider;

pub const MAX_TEXT_FILE_BYTES: usize = 16 * 1024 * 1024;
pub const MAX_PACKAGE_SOURCE_BYTES: usize = 64 * 1024 * 1024;
pub const MAX_PACKAGE_SOURCE_FILES: usize = 10_000;
pub const MAX_DIRECTORY_ENTRIES: usize = 100_000;
#[cfg(feature = "zip")]
const MAX_ZIP_ENTRY_BYTES: usize = 64 * 1024 * 1024;
#[cfg(feature = "zip")]
const MAX_ZIP_CONTENT_BYTES: usize = 128 * 1024 * 1024;
#[cfg(feature = "zip")]
pub const MAX_ZIP_ARCHIVE_BYTES: usize = 256 * 1024 * 1024;
#[cfg(feature = "zip")]
const MAX_ZIP_ENTRIES: usize = 100_000;

/// Exact kind of one filesystem entry at a validation boundary.
///
/// The default [`FileSystem`] implementation reports [`Unknown`](Self::Unknown)
/// so security- and integrity-sensitive callers can fail closed. Filesystems
/// that can prove entry kinds should override [`FileSystem::entry_kind`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileSystemEntryKind {
    RegularFile,
    Directory,
    Symlink,
    Special,
    Missing,
    Unknown,
}

fn size_limit_error(kind: &str, actual: usize, max: usize) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("{kind} size {actual} exceeds the {max}-byte limit"),
    )
}

fn validate_text_size(len: usize) -> io::Result<()> {
    if len > MAX_TEXT_FILE_BYTES {
        return Err(size_limit_error("text file", len, MAX_TEXT_FILE_BYTES));
    }
    Ok(())
}

fn validate_binary_size(len: usize, max: usize) -> io::Result<()> {
    if len > max {
        return Err(size_limit_error("file", len, max));
    }
    Ok(())
}

fn allocation_error(kind: &str, additional: usize) -> io::Error {
    io::Error::other(format!(
        "failed to reserve {additional} bytes while reading {kind}"
    ))
}

fn read_to_end_limited(mut reader: impl Read, max_bytes: usize, kind: &str) -> io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    let mut buffer = [0u8; 8 * 1024];
    loop {
        let remaining = max_bytes.saturating_sub(bytes.len());
        let read_limit = buffer.len().min(remaining.saturating_add(1));
        let count = match reader.read(&mut buffer[..read_limit]) {
            Ok(count) => count,
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error),
        };
        if count == 0 {
            return Ok(bytes);
        }
        let new_len = bytes
            .len()
            .checked_add(count)
            .ok_or_else(|| size_limit_error(kind, usize::MAX, max_bytes))?;
        if new_len > max_bytes {
            return Err(size_limit_error(kind, new_len, max_bytes));
        }
        bytes
            .try_reserve(count)
            .map_err(|_| allocation_error(kind, count))?;
        bytes.extend_from_slice(&buffer[..count]);
    }
}

pub fn read_binary_file(path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
    let file = std::fs::File::open(path)?;
    let max_len = u64::try_from(max_bytes).unwrap_or(u64::MAX);
    let metadata_len = file.metadata()?.len();
    if metadata_len > max_len {
        return Err(size_limit_error(
            "file",
            usize::try_from(metadata_len).unwrap_or(usize::MAX),
            max_bytes,
        ));
    }
    // The streaming bound remains authoritative if the file grows after the
    // metadata check or if it is a virtual file whose metadata reports zero.
    read_to_end_limited(file, max_bytes, "file")
}

pub fn read_text_file(path: &Path) -> io::Result<String> {
    let bytes = read_binary_file(path, MAX_TEXT_FILE_BYTES)?;
    validate_text_size(bytes.len())?;
    String::from_utf8(bytes)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.utf8_error()))
}

/// A virtual file system trait for abstracting file operations.
pub trait FileSystem: Send + Sync {
    /// Read file contents as a string.
    fn read_file(&self, path: &Path) -> io::Result<String>;

    /// Read file contents as raw bytes.
    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>>;

    /// Read raw bytes while enforcing an allocation bound.
    ///
    /// Implementations backed by external or lazily loaded storage should
    /// override this method so the bound is checked before materialization.
    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        let bytes = self.read_bytes(path)?;
        validate_binary_size(bytes.len(), max_bytes)?;
        Ok(bytes)
    }

    /// Read UTF-8 text while enforcing a byte allocation bound.
    fn read_text_limited(&self, path: &Path, max_bytes: usize) -> io::Result<String> {
        let bytes = self.read_bytes_limited(path, max_bytes)?;
        String::from_utf8(bytes)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.utf8_error()))
    }

    /// List entries in a directory in normalized relative-path order.
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;

    /// Check if a path exists.
    fn exists(&self, path: &Path) -> bool;

    /// Check if a path is a directory.
    fn is_dir(&self, path: &Path) -> bool;

    /// Return the exact kind of an entry without following symbolic links.
    ///
    /// Implementations that cannot make that guarantee retain the fail-closed
    /// default. Callers must not infer a regular file from `exists`/`is_dir`.
    fn entry_kind(&self, _path: &Path) -> io::Result<FileSystemEntryKind> {
        Ok(FileSystemEntryKind::Unknown)
    }

    /// Return the host executable-bit state when this filesystem can express
    /// POSIX source modes securely. Virtual filesystems and Windows return
    /// `None` while still authenticating mode through package metadata.
    fn executable_mode(&self, _path: &Path) -> io::Result<Option<bool>> {
        Ok(None)
    }

    /// Root path of the file system (if any).
    fn root(&self) -> Option<&Path> {
        None
    }

    /// Resolve an entry to the exact host-filesystem path backing it.
    ///
    /// Implementations must return `None` when entries are virtual, layered,
    /// or otherwise lack a one-to-one host path. The returned path carries no
    /// symlink-safety guarantee by itself; callers that cross a trust boundary
    /// must still open it component by component without following links.
    fn resolve_host_path(&self, _path: &Path) -> io::Result<Option<PathBuf>> {
        Ok(None)
    }
}

pub fn normalize_fs_path(path: &Path) -> PathBuf {
    use std::path::Component;

    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                let tail = normalized.components().next_back();
                match tail {
                    Some(Component::Normal(_)) => {
                        normalized.pop();
                    }
                    Some(Component::RootDir | Component::Prefix(_)) => {}
                    _ => normalized.push(".."),
                }
            }
            _ => normalized.push(component.as_os_str()),
        }
    }

    if normalized.as_os_str().is_empty() {
        PathBuf::from(".")
    } else {
        normalized
    }
}

/// Normalize the starting point for ancestor discovery without allowing a
/// leading `..` to collapse into the process working directory by accident.
///
/// A global host filesystem can bind a relative parent path to an absolute
/// host path first. Virtual and scoped filesystems fail closed when the path
/// would escape their logical namespace.
pub fn normalize_ancestor_discovery_start<F: FileSystem + ?Sized>(
    fs: &F,
    path: &Path,
) -> io::Result<Option<PathBuf>> {
    use std::path::Component;

    let normalized = normalize_fs_path(path);
    if !normalized
        .components()
        .any(|component| matches!(component, Component::ParentDir))
    {
        return Ok(Some(normalized));
    }
    let Some(root) = fs.root() else {
        return Ok(None);
    };
    if root != Path::new(".") && !root.as_os_str().is_empty() {
        return Ok(None);
    }
    let Some(host_path) = fs.resolve_host_path(&normalized)? else {
        return Ok(None);
    };
    let absolute = if host_path.is_absolute() {
        host_path
    } else {
        std::env::current_dir()?.join(host_path)
    };
    Ok(Some(normalize_fs_path(&absolute)))
}

/// Sort paths by their normalized, forward-slash relative representation.
///
/// Filesystem enumeration order varies by platform and HashMap seed. Source
/// consumers use this order as the package presentation order, so every VFS
/// implementation shares this comparator.
pub fn sort_fs_paths(paths: &mut [PathBuf]) {
    paths.sort_by_cached_key(|path| {
        (
            normalize_fs_path(path).to_string_lossy().replace('\\', "/"),
            path.clone(),
        )
    });
}

fn join_scoped_path(base: &Path, child: &Path) -> Option<PathBuf> {
    let base = normalize_fs_path(base);
    let candidate = if child.is_absolute() {
        normalize_fs_path(child)
    } else if child == Path::new(".") || child.as_os_str().is_empty() {
        base.clone()
    } else if base == Path::new(".") || base.as_os_str().is_empty() {
        normalize_fs_path(child)
    } else {
        normalize_fs_path(&base.join(child))
    };
    if base == Path::new(".") {
        let escapes = candidate.is_absolute()
            || matches!(
                candidate.components().next(),
                Some(std::path::Component::ParentDir | std::path::Component::Prefix(_))
            );
        return (!escapes).then_some(candidate);
    }
    (candidate == base || candidate.starts_with(&base)).then_some(candidate)
}

fn strip_scoped_prefix(path: &Path, scope: &Path) -> Option<PathBuf> {
    if scope == Path::new(".") || scope.as_os_str().is_empty() {
        return Some(normalize_fs_path(path));
    }
    let relative = path.strip_prefix(scope).ok()?;
    if relative.as_os_str().is_empty() {
        Some(PathBuf::from("."))
    } else {
        Some(normalize_fs_path(relative))
    }
}

fn normalized_parent(path: &Path) -> Option<PathBuf> {
    path.parent().map(normalize_fs_path)
}

fn insert_parent_directories(directories: &mut BTreeSet<PathBuf>, path: &Path) {
    let mut current = path;
    while let Some(parent) = current.parent() {
        directories.insert(normalize_fs_path(parent));
        if parent.as_os_str().is_empty() || parent == current {
            break;
        }
        current = parent;
    }
}

impl<T: FileSystem + ?Sized> FileSystem for Arc<T> {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        (**self).read_file(path)
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        (**self).read_bytes(path)
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        (**self).read_bytes_limited(path, max_bytes)
    }

    fn read_text_limited(&self, path: &Path, max_bytes: usize) -> io::Result<String> {
        (**self).read_text_limited(path, max_bytes)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        (**self).read_dir(path)
    }

    fn exists(&self, path: &Path) -> bool {
        (**self).exists(path)
    }

    fn is_dir(&self, path: &Path) -> bool {
        (**self).is_dir(path)
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        (**self).entry_kind(path)
    }

    fn executable_mode(&self, path: &Path) -> io::Result<Option<bool>> {
        (**self).executable_mode(path)
    }

    fn root(&self) -> Option<&Path> {
        (**self).root()
    }

    fn resolve_host_path(&self, path: &Path) -> io::Result<Option<PathBuf>> {
        (**self).resolve_host_path(path)
    }
}

/// Real file system implementation.
///
/// All operations are relative to `root`. Both `FileSystem` and `SourceProvider`
/// traits use the same path resolution: `root.join(path)`.
#[derive(Debug, Clone)]
pub struct RealFs {
    /// Root directory for all path resolution.
    root: PathBuf,
}

impl RealFs {
    /// Create a new RealFs with a root directory.
    /// All paths will be resolved relative to this root.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self {
            root: normalize_fs_path(&root.into()),
        }
    }

    /// Get the root directory.
    pub fn root(&self) -> &Path {
        &self.root
    }

    fn is_global_root(&self) -> bool {
        self.root == Path::new(".") || self.root.as_os_str().is_empty()
    }

    fn resolve(&self, path: &Path) -> Option<PathBuf> {
        if self.is_global_root() {
            Some(normalize_fs_path(path))
        } else {
            join_scoped_path(&self.root, path)
        }
    }

    fn resolve_for_read(&self, path: &Path) -> io::Result<PathBuf> {
        self.resolve(path).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!("path escapes filesystem root: {path:?}"),
            )
        })
    }
}

impl FileSystem for RealFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        read_text_file(&self.resolve_for_read(path)?)
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        read_binary_file(&self.resolve_for_read(path)?, usize::MAX)
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        read_binary_file(&self.resolve_for_read(path)?, max_bytes)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let full_path = self.resolve_for_read(path)?;
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(&full_path)? {
            if entries.len() >= MAX_DIRECTORY_ENTRIES {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
                ));
            }
            // Return paths relative to root
            let entry_path = entry?.path();
            if !self.is_global_root() {
                let rel = entry_path.strip_prefix(&self.root).map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "directory entry {} escapes filesystem root {}",
                            entry_path.display(),
                            self.root.display()
                        ),
                    )
                })?;
                entries.push(rel.to_path_buf());
            } else {
                entries.push(entry_path);
            }
        }
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        self.resolve(path).is_some_and(|path| path.exists())
    }

    fn is_dir(&self, path: &Path) -> bool {
        self.resolve(path).is_some_and(|path| path.is_dir())
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        let path = self.resolve_for_read(path)?;
        let metadata = match std::fs::symlink_metadata(path) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                return Ok(FileSystemEntryKind::Missing);
            }
            Err(error) => return Err(error),
        };
        let file_type = metadata.file_type();
        Ok(if file_type.is_symlink() {
            FileSystemEntryKind::Symlink
        } else if file_type.is_file() {
            FileSystemEntryKind::RegularFile
        } else if file_type.is_dir() {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::Special
        })
    }

    fn executable_mode(&self, path: &Path) -> io::Result<Option<bool>> {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt as _;

            let path = self.resolve_for_read(path)?;
            let metadata = std::fs::symlink_metadata(path)?;
            if !metadata.is_file() || metadata.file_type().is_symlink() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "executable mode requires a regular file without symbolic links",
                ));
            }
            Ok(Some(metadata.permissions().mode() & 0o100 != 0))
        }
        #[cfg(not(unix))]
        {
            let _ = path;
            Ok(None)
        }
    }

    fn root(&self) -> Option<&Path> {
        Some(&self.root)
    }

    fn resolve_host_path(&self, path: &Path) -> io::Result<Option<PathBuf>> {
        self.resolve_for_read(path).map(Some)
    }
}

impl SourceProvider for RealFs {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

/// In-memory file system for testing.
#[derive(Debug, Clone)]
pub struct MemoryFs {
    files: HashMap<PathBuf, Vec<u8>>,
    directories: BTreeSet<PathBuf>,
}

impl Default for MemoryFs {
    fn default() -> Self {
        Self::new()
    }
}

impl MemoryFs {
    pub fn new() -> Self {
        let mut directories = BTreeSet::new();
        directories.insert(PathBuf::from("."));
        Self {
            files: HashMap::new(),
            directories,
        }
    }

    pub fn add_file(&mut self, path: impl Into<PathBuf>, content: impl Into<String>) {
        self.add_bytes(path, content.into().into_bytes());
    }

    pub fn add_bytes(&mut self, path: impl Into<PathBuf>, content: impl Into<Vec<u8>>) {
        let path = normalize_fs_path(&path.into());
        insert_parent_directories(&mut self.directories, &path);
        self.directories.remove(&path);
        self.files.insert(path, content.into());
    }

    pub fn add_dir(&mut self, path: impl Into<PathBuf>) {
        let path = normalize_fs_path(&path.into());
        insert_parent_directories(&mut self.directories, &path);
        self.directories.insert(path);
    }

    pub fn with_file(mut self, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        self.add_file(path, content);
        self
    }

    pub fn with_bytes(mut self, path: impl Into<PathBuf>, content: impl Into<Vec<u8>>) -> Self {
        self.add_bytes(path, content);
        self
    }

    pub fn with_dir(mut self, path: impl Into<PathBuf>) -> Self {
        self.add_dir(path);
        self
    }

    pub fn with_files(
        mut self,
        files: impl IntoIterator<Item = (impl Into<PathBuf>, impl Into<String>)>,
    ) -> Self {
        for (path, content) in files {
            self.add_file(path, content);
        }
        self
    }
}

impl FileSystem for MemoryFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let path = normalize_fs_path(path);
        let content = self.files.get(&path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, format!("file not found: {path:?}"))
        })?;
        validate_text_size(content.len())?;
        String::from_utf8(content.clone())
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.utf8_error()))
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        let path = normalize_fs_path(path);
        self.files.get(&path).cloned().ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, format!("file not found: {path:?}"))
        })
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        let path = normalize_fs_path(path);
        let content = self.files.get(&path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, format!("file not found: {path:?}"))
        })?;
        validate_binary_size(content.len(), max_bytes)?;
        Ok(content.clone())
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let path = normalize_fs_path(path);
        if self.files.contains_key(&path) {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("path is not a directory: {path:?}"),
            ));
        }
        if !self.directories.contains(&path) {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("directory not found: {path:?}"),
            ));
        }

        let mut entries = BTreeSet::new();
        for candidate in self.files.keys().chain(self.directories.iter()) {
            if candidate != &path && normalized_parent(candidate).as_deref() == Some(path.as_path())
            {
                entries.insert(candidate.clone());
                if entries.len() > MAX_DIRECTORY_ENTRIES {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
                    ));
                }
            }
        }
        let mut entries: Vec<_> = entries.into_iter().collect();
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        let path = normalize_fs_path(path);
        self.files.contains_key(&path) || self.directories.contains(&path)
    }

    fn is_dir(&self, path: &Path) -> bool {
        let path = normalize_fs_path(path);
        !self.files.contains_key(&path) && self.directories.contains(&path)
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        let path = normalize_fs_path(path);
        Ok(if self.files.contains_key(&path) {
            FileSystemEntryKind::RegularFile
        } else if self.directories.contains(&path) {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::Missing
        })
    }

    fn root(&self) -> Option<&Path> {
        None
    }
}

impl SourceProvider for MemoryFs {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

#[derive(Clone)]
pub struct ScopedFs<F: FileSystem> {
    base: F,
    root: PathBuf,
}

impl<F: FileSystem> ScopedFs<F> {
    pub fn new(base: F, root: impl Into<PathBuf>) -> Self {
        Self {
            base,
            root: normalize_fs_path(&root.into()),
        }
    }

    fn resolve(&self, path: &Path) -> Option<PathBuf> {
        join_scoped_path(&self.root, path)
    }

    fn resolve_for_read(&self, path: &Path) -> io::Result<PathBuf> {
        self.resolve(path).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!("path escapes scoped filesystem root: {path:?}"),
            )
        })
    }
}

impl<F: FileSystem> FileSystem for ScopedFs<F> {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        self.base.read_file(&self.resolve_for_read(path)?)
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.base.read_bytes(&self.resolve_for_read(path)?)
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        self.base
            .read_bytes_limited(&self.resolve_for_read(path)?, max_bytes)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let resolved_dir = self.resolve_for_read(path)?;
        let entries = self.base.read_dir(&resolved_dir)?;
        let mut scoped_entries = BTreeSet::new();
        for entry in entries {
            let entry = strip_scoped_prefix(&entry, &self.root).ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "underlying filesystem returned entry outside scope {}: {}",
                        self.root.display(),
                        entry.display()
                    ),
                )
            })?;
            scoped_entries.insert(entry);
            if scoped_entries.len() > MAX_DIRECTORY_ENTRIES {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
                ));
            }
        }
        let mut entries: Vec<_> = scoped_entries.into_iter().collect();
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        self.resolve(path)
            .is_some_and(|resolved| self.base.exists(&resolved))
    }

    fn is_dir(&self, path: &Path) -> bool {
        self.resolve(path)
            .is_some_and(|resolved| self.base.is_dir(&resolved))
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        self.base.entry_kind(&self.resolve_for_read(path)?)
    }

    fn root(&self) -> Option<&Path> {
        Some(&self.root)
    }

    fn resolve_host_path(&self, path: &Path) -> io::Result<Option<PathBuf>> {
        self.base.resolve_host_path(&self.resolve_for_read(path)?)
    }
}

impl<F: FileSystem> SourceProvider for ScopedFs<F> {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

/// Zip file system implementation.
///
/// All files are loaded into memory on creation. Paths are relative to `root`.
/// Both `FileSystem` and `SourceProvider` traits use the same path resolution.
#[cfg(feature = "zip")]
#[derive(Debug, Clone)]
pub struct ZipFs {
    /// All files in the zip: path relative to zip root -> content
    files: HashMap<PathBuf, Vec<u8>>,
    /// Explicit and inferred directories, including the archive root.
    directories: BTreeSet<PathBuf>,
    /// Root directory within the zip (empty = zip root)
    root: PathBuf,
}

#[cfg(feature = "zip")]
impl ZipFs {
    /// Create a ZipFs from a zip file path, using zip root as project root.
    pub fn from_path(path: &Path) -> io::Result<Self> {
        let file = std::fs::File::open(path)?;
        Self::from_reader(file)
    }

    /// Create a ZipFs with a specific root directory within the zip.
    ///
    /// Example: `ZipFs::from_path_with_root("project.zip", "src/")`
    /// will treat `src/` as the project root.
    pub fn from_path_with_root(path: &Path, root: impl Into<PathBuf>) -> io::Result<Self> {
        let file = std::fs::File::open(path)?;
        Self::from_reader_with_root(file, root)
    }

    /// Create a ZipFs from any Read + Seek source (file, bytes, etc).
    pub fn from_reader<R: Read + Seek>(reader: R) -> io::Result<Self> {
        Self::from_reader_with_root(reader, PathBuf::new())
    }

    /// Create a ZipFs from reader with a specific root directory.
    pub fn from_reader_with_root<R: Read + Seek>(
        mut reader: R,
        root: impl Into<PathBuf>,
    ) -> io::Result<Self> {
        use std::io::SeekFrom;

        let original_position = reader.stream_position()?;
        let archive_size = reader.seek(SeekFrom::End(0))?;
        if archive_size > u64::try_from(MAX_ZIP_ARCHIVE_BYTES).unwrap_or(u64::MAX) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "zip archive size {archive_size} exceeds the {MAX_ZIP_ARCHIVE_BYTES}-byte limit"
                ),
            ));
        }
        reader.seek(SeekFrom::Start(original_position))?;
        let mut archive = zip::ZipArchive::new(reader)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        if archive.len() > MAX_ZIP_ENTRIES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("zip contains more than {MAX_ZIP_ENTRIES} entries"),
            ));
        }

        let mut files = HashMap::new();
        files
            .try_reserve(archive.len())
            .map_err(|_| io::Error::other("failed to reserve zip file index"))?;
        let mut directories = BTreeSet::new();
        directories.insert(PathBuf::from("."));
        let mut explicit_entries = BTreeSet::new();
        let mut total_bytes = 0usize;

        for i in 0..archive.len() {
            let mut file = archive
                .by_index(i)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

            let raw_name = std::str::from_utf8(file.name_raw()).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "zip entry path is not valid UTF-8",
                )
            })?;
            let path = safe_zip_path(raw_name)?;
            if file.is_symlink() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("zip symbolic links are not supported: {path:?}"),
                ));
            }
            if !explicit_entries.insert(path.clone()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("zip contains duplicate normalized path {path:?}"),
                ));
            }
            validate_zip_parent_chain(&files, &path)?;
            insert_parent_directories(&mut directories, &path);
            if directories.len() > MAX_ZIP_ENTRIES {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("zip contains more than {MAX_ZIP_ENTRIES} directories"),
                ));
            }

            if file.is_dir() {
                if files.contains_key(&path) {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("zip path is both a file and directory: {path:?}"),
                    ));
                }
                directories.insert(path);
                continue;
            }

            if directories.contains(&path) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("zip path is both a file and directory: {path:?}"),
                ));
            }
            let advertised_size = usize::try_from(file.size()).unwrap_or(usize::MAX);
            validate_binary_size(advertised_size, MAX_ZIP_ENTRY_BYTES)?;
            let bytes = read_to_end_limited(&mut file, MAX_ZIP_ENTRY_BYTES, "zip entry")?;
            total_bytes = total_bytes.checked_add(bytes.len()).ok_or_else(|| {
                size_limit_error("zip content", usize::MAX, MAX_ZIP_CONTENT_BYTES)
            })?;
            if total_bytes > MAX_ZIP_CONTENT_BYTES {
                return Err(size_limit_error(
                    "zip content",
                    total_bytes,
                    MAX_ZIP_CONTENT_BYTES,
                ));
            }
            if files.insert(path.clone(), bytes).is_some() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("zip contains duplicate path {path:?}"),
                ));
            }
        }
        let root = safe_zip_root(&root.into())?;
        if files.contains_key(&root) {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("zip filesystem root is a file: {root:?}"),
            ));
        }
        if !directories.contains(&root) {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("zip filesystem root does not exist: {root:?}"),
            ));
        }

        Ok(Self {
            files,
            directories,
            root,
        })
    }

    /// Create a ZipFs from in-memory bytes.
    pub fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        Self::from_reader(std::io::Cursor::new(bytes))
    }

    /// Resolve a path relative to the root.
    fn resolve(&self, path: &Path) -> Option<PathBuf> {
        join_scoped_path(&self.root, path)
    }

    fn resolve_for_read(&self, path: &Path) -> io::Result<PathBuf> {
        self.resolve(path).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!("path escapes zip filesystem root: {path:?}"),
            )
        })
    }
}

#[cfg(feature = "zip")]
fn safe_zip_path(name: &str) -> io::Result<PathBuf> {
    if name.contains('\\') || name.contains('\0') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "zip path contains a non-portable separator or NUL",
        ));
    }
    let without_trailing_slash = name.strip_suffix('/').unwrap_or(name);
    if without_trailing_slash.is_empty()
        || without_trailing_slash
            .split('/')
            .any(|component| component.is_empty() || component == "..")
        || without_trailing_slash
            .split('/')
            .next()
            .is_some_and(|component| component.ends_with(':'))
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("zip path is not normalized: {name:?}"),
        ));
    }
    let path = safe_zip_root(Path::new(without_trailing_slash))?;
    if path.as_os_str().is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "zip entry path is empty",
        ));
    }
    Ok(path)
}

#[cfg(feature = "zip")]
fn safe_zip_root(path: &Path) -> io::Result<PathBuf> {
    use std::path::Component;

    let portable = path.to_str().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "zip root path is not valid UTF-8",
        )
    })?;
    if portable.contains('\\') || portable.contains('\0') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "zip root contains a non-portable separator or NUL",
        ));
    }
    if portable
        .split('/')
        .next()
        .is_some_and(|component| component.ends_with(':'))
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "zip root contains a Windows drive prefix",
        ));
    }

    let mut safe = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(value) => safe.push(value),
            Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("zip path escapes its archive root: {path:?}"),
                ));
            }
        }
    }
    Ok(normalize_fs_path(&safe))
}

#[cfg(feature = "zip")]
fn validate_zip_parent_chain(files: &HashMap<PathBuf, Vec<u8>>, path: &Path) -> io::Result<()> {
    let mut current = path.to_path_buf();
    while let Some(parent) = current.parent() {
        let parent = normalize_fs_path(parent);
        if files.contains_key(&parent) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("zip file path is used as a parent directory: {parent:?}"),
            ));
        }
        if parent == Path::new(".") || parent == current {
            break;
        }
        current = parent;
    }
    Ok(())
}

#[cfg(feature = "zip")]
impl FileSystem for ZipFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let full_path = self.resolve_for_read(path)?;
        let bytes = self.files.get(&full_path).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("file not found: {:?}", full_path),
            )
        })?;
        validate_text_size(bytes.len())?;
        String::from_utf8(bytes.clone())
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.utf8_error()))
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        let full_path = self.resolve_for_read(path)?;
        self.files.get(&full_path).cloned().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("file not found: {full_path:?}"),
            )
        })
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        let full_path = self.resolve_for_read(path)?;
        let content = self.files.get(&full_path).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("file not found: {full_path:?}"),
            )
        })?;
        validate_binary_size(content.len(), max_bytes)?;
        Ok(content.clone())
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let full_path = self.resolve_for_read(path)?;
        if self.files.contains_key(&full_path) {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("path is not a directory: {path:?}"),
            ));
        }
        if !self.directories.contains(&full_path) {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("directory not found: {path:?}"),
            ));
        }

        let mut entries = BTreeSet::new();
        for candidate in self.files.keys().chain(self.directories.iter()) {
            if candidate == &full_path
                || normalized_parent(candidate).as_deref() != Some(full_path.as_path())
            {
                continue;
            }
            let relative = strip_scoped_prefix(candidate, &self.root).ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("zip directory entry escapes configured root: {candidate:?}"),
                )
            })?;
            entries.insert(relative);
            if entries.len() > MAX_DIRECTORY_ENTRIES {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
                ));
            }
        }
        let mut entries: Vec<_> = entries.into_iter().collect();
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        let Some(full_path) = self.resolve(path) else {
            return false;
        };
        self.files.contains_key(&full_path) || self.directories.contains(&full_path)
    }

    fn is_dir(&self, path: &Path) -> bool {
        let Some(full_path) = self.resolve(path) else {
            return false;
        };
        !self.files.contains_key(&full_path) && self.directories.contains(&full_path)
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        let full_path = self.resolve_for_read(path)?;
        Ok(if self.files.contains_key(&full_path) {
            FileSystemEntryKind::RegularFile
        } else if self.directories.contains(&full_path) {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::Missing
        })
    }

    fn root(&self) -> Option<&Path> {
        None
    }
}

#[cfg(feature = "zip")]
impl SourceProvider for ZipFs {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

/// A collection of source files for a project.
#[derive(Debug, Clone)]
pub struct FileSet {
    /// File path -> file content
    pub files: BTreeMap<PathBuf, String>,
    /// Project root directory
    pub root: PathBuf,
}

impl FileSet {
    pub fn new(root: PathBuf) -> Self {
        Self {
            files: BTreeMap::new(),
            root,
        }
    }

    /// Collect .vo files from a directory (non-recursive).
    /// Subdirectories are treated as separate packages and loaded via import.
    ///
    /// `fs` - FileSystem to read from (paths are relative to fs.root)
    /// `dir` - Directory to collect from (relative to fs.root, use "." for root)
    /// `abs_root` - Absolute root path for the FileSet (used for source map)
    pub fn collect<F: FileSystem>(fs: &F, dir: &Path, abs_root: PathBuf) -> io::Result<Self> {
        let mut file_set = Self::new(abs_root);
        let mut total_bytes = 0usize;
        let dir = normalize_fs_path(dir);

        // Only collect .vo files in the directory (not subdirectories)
        let entries = fs.read_dir(&dir)?;
        let mut entries: Vec<_> = entries
            .into_iter()
            .map(|entry| normalize_fs_path(&entry))
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        if entries.len() > MAX_DIRECTORY_ENTRIES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
            ));
        }
        sort_fs_paths(&mut entries);
        for entry in entries {
            if normalized_parent(&entry).as_deref() != Some(dir.as_path()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "filesystem returned non-child entry {} for directory {}",
                        entry.display(),
                        dir.display()
                    ),
                ));
            }
            if !fs.is_dir(&entry) && entry.extension().is_some_and(|e| e == "vo") {
                if file_set.files.len() >= MAX_PACKAGE_SOURCE_FILES {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "package contains more than {MAX_PACKAGE_SOURCE_FILES} source files"
                        ),
                    ));
                }
                let remaining = MAX_PACKAGE_SOURCE_BYTES.saturating_sub(total_bytes);
                let content = fs.read_text_limited(&entry, MAX_TEXT_FILE_BYTES.min(remaining))?;
                total_bytes = total_bytes.checked_add(content.len()).ok_or_else(|| {
                    size_limit_error("package source", usize::MAX, MAX_PACKAGE_SOURCE_BYTES)
                })?;
                if total_bytes > MAX_PACKAGE_SOURCE_BYTES {
                    return Err(size_limit_error(
                        "package source",
                        total_bytes,
                        MAX_PACKAGE_SOURCE_BYTES,
                    ));
                }
                file_set.files.insert(entry, content);
            }
        }

        Ok(file_set)
    }

    /// Create a FileSet from a single source file.
    ///
    /// `fs` - FileSystem to read from
    /// `file_path` - Path to the file (relative to fs.root)
    /// `abs_root` - Absolute root path for the FileSet
    pub fn from_file<F: FileSystem>(
        fs: &F,
        file_path: &Path,
        abs_root: PathBuf,
    ) -> io::Result<Self> {
        let mut file_set = Self::new(abs_root);
        let file_path = normalize_fs_path(file_path);
        let content = fs.read_text_limited(&file_path, MAX_TEXT_FILE_BYTES)?;
        file_set.files.insert(file_path, content);
        Ok(file_set)
    }

    /// Get files grouped by their parent directory (package).
    pub fn files_by_package(&self) -> BTreeMap<PathBuf, Vec<(&PathBuf, &String)>> {
        let mut packages: BTreeMap<PathBuf, Vec<(&PathBuf, &String)>> = BTreeMap::new();

        for (path, content) in &self.files {
            let pkg_dir = path.parent().unwrap_or(Path::new("")).to_path_buf();
            packages.entry(pkg_dir).or_default().push((path, content));
        }

        for files in packages.values_mut() {
            files.sort_by_cached_key(|(path, _)| {
                normalize_fs_path(path).to_string_lossy().replace('\\', "/")
            });
        }

        packages
    }
}

/// Overlay file system that combines two file systems.
///
/// Tries the primary file system first, falls back to secondary if not found.
/// This is useful for combining a zip file (project files) with real fs (stdlib).
#[derive(Debug, Clone)]
pub struct OverlayFs<P: FileSystem, S: FileSystem> {
    /// Primary file system (checked first)
    primary: P,
    /// Secondary file system (fallback)
    secondary: S,
}

impl<P: FileSystem, S: FileSystem> OverlayFs<P, S> {
    /// Create an overlay file system with primary and secondary layers.
    pub fn new(primary: P, secondary: S) -> Self {
        Self { primary, secondary }
    }
}

impl<P: FileSystem, S: FileSystem> FileSystem for OverlayFs<P, S> {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        match self.primary.read_file(path) {
            Ok(content) => Ok(content),
            Err(error) if error.kind() == io::ErrorKind::NotFound && !self.primary.exists(path) => {
                self.secondary.read_file(path)
            }
            Err(error) => Err(error),
        }
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        match self.primary.read_bytes(path) {
            Ok(bytes) => Ok(bytes),
            Err(error) if error.kind() == io::ErrorKind::NotFound && !self.primary.exists(path) => {
                self.secondary.read_bytes(path)
            }
            Err(error) => Err(error),
        }
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        match self.primary.read_bytes_limited(path, max_bytes) {
            Ok(bytes) => Ok(bytes),
            Err(error) if error.kind() == io::ErrorKind::NotFound && !self.primary.exists(path) => {
                self.secondary.read_bytes_limited(path, max_bytes)
            }
            Err(error) => Err(error),
        }
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        // Merge entries from both file systems
        let primary_entries = match self.primary.read_dir(path) {
            Ok(entries) => Some(entries),
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                if self.primary.exists(path) {
                    return if self.primary.is_dir(path) {
                        Err(error)
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::NotADirectory,
                            format!("path is not a directory: {path:?}"),
                        ))
                    };
                }
                None
            }
            Err(error) => return Err(error),
        };
        let primary_is_directory = primary_entries.is_some();
        let secondary_entries = match self.secondary.read_dir(path) {
            Ok(entries) => Some(entries),
            Err(error) if error.kind() == io::ErrorKind::NotFound => None,
            Err(error) if primary_is_directory && error.kind() == io::ErrorKind::NotADirectory => {
                None
            }
            Err(error) => return Err(error),
        };
        if primary_entries.is_none() && secondary_entries.is_none() {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                "directory not found",
            ))
        } else {
            let mut unique = BTreeSet::new();
            for entry in primary_entries
                .into_iter()
                .flatten()
                .chain(secondary_entries.into_iter().flatten())
            {
                unique.insert(normalize_fs_path(&entry));
                if unique.len() > MAX_DIRECTORY_ENTRIES {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
                    ));
                }
            }
            let mut entries: Vec<_> = unique.into_iter().collect();
            sort_fs_paths(&mut entries);
            Ok(entries)
        }
    }

    fn exists(&self, path: &Path) -> bool {
        self.primary.exists(path) || self.secondary.exists(path)
    }

    fn is_dir(&self, path: &Path) -> bool {
        if self.primary.exists(path) {
            self.primary.is_dir(path)
        } else {
            self.secondary.is_dir(path)
        }
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        match self.primary.entry_kind(path)? {
            FileSystemEntryKind::Missing => self.secondary.entry_kind(path),
            kind => Ok(kind),
        }
    }

    fn root(&self) -> Option<&Path> {
        self.primary.root().or_else(|| self.secondary.root())
    }

    fn resolve_host_path(&self, path: &Path) -> io::Result<Option<PathBuf>> {
        if self.primary.exists(path) {
            self.primary.resolve_host_path(path)
        } else {
            self.secondary.resolve_host_path(path)
        }
    }
}

impl<P: FileSystem, S: FileSystem> SourceProvider for OverlayFs<P, S> {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct FailingDiscoveryFs;

    impl FileSystem for FailingDiscoveryFs {
        fn read_file(&self, _path: &Path) -> io::Result<String> {
            unreachable!("ancestor start normalization must not read files")
        }

        fn read_bytes(&self, _path: &Path) -> io::Result<Vec<u8>> {
            unreachable!("ancestor start normalization must not read files")
        }

        fn read_dir(&self, _path: &Path) -> io::Result<Vec<PathBuf>> {
            unreachable!("ancestor start normalization must not enumerate directories")
        }

        fn exists(&self, _path: &Path) -> bool {
            false
        }

        fn is_dir(&self, _path: &Path) -> bool {
            false
        }

        fn root(&self) -> Option<&Path> {
            Some(Path::new("."))
        }

        fn resolve_host_path(&self, _path: &Path) -> io::Result<Option<PathBuf>> {
            Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "discovery path denied",
            ))
        }
    }

    #[test]
    fn test_memory_fs() {
        let fs = MemoryFs::new()
            .with_file("/project/main.vo", "package main")
            .with_file("/project/lib/util.vo", "package lib");

        assert!(fs.exists(Path::new("/project/main.vo")));
        assert!(fs.exists(Path::new("/project/lib")));
        assert!(fs.is_dir(Path::new("/project/lib")));
        assert!(!fs.is_dir(Path::new("/project/main.vo")));

        let content = fs.read_file(Path::new("/project/main.vo")).unwrap();
        assert_eq!(content, "package main");
    }

    #[test]
    fn test_normalize_fs_path_collapses_parent_segments() {
        assert_eq!(
            normalize_fs_path(Path::new("workspace/app/../voplay")),
            PathBuf::from("workspace/voplay")
        );
        assert_eq!(
            normalize_fs_path(Path::new("./workspace/./voplay")),
            PathBuf::from("workspace/voplay")
        );
    }

    #[test]
    fn ancestor_discovery_start_propagates_host_path_resolution_errors() {
        let error = normalize_ancestor_discovery_start(
            &FailingDiscoveryFs,
            Path::new("../outside/project"),
        )
        .expect_err("host-path resolution failures must remain observable");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
        assert!(error.to_string().contains("discovery path denied"));
    }

    #[test]
    fn test_memory_fs_read_dir_uses_normalized_source_order() {
        let fs = MemoryFs::new()
            .with_file("z_last.vo", "package main")
            .with_file("nested/b.vo", "package nested")
            .with_file("a_first.vo", "package main")
            .with_file("nested/a.vo", "package nested");

        assert_eq!(
            fs.read_dir(Path::new(".")).unwrap(),
            vec![
                PathBuf::from("a_first.vo"),
                PathBuf::from("nested"),
                PathBuf::from("z_last.vo"),
            ]
        );
        assert_eq!(
            fs.read_dir(Path::new("nested")).unwrap(),
            vec![PathBuf::from("nested/a.vo"), PathBuf::from("nested/b.vo"),]
        );
    }

    #[test]
    fn memory_fs_preserves_binary_files_and_empty_directories() {
        let fs = MemoryFs::new()
            .with_bytes("assets/data.bin", [0xff, 0x00, 0x7f])
            .with_dir("empty/nested");

        assert_eq!(
            fs.read_bytes(Path::new("assets/data.bin")).unwrap(),
            [0xff, 0x00, 0x7f]
        );
        assert_eq!(
            fs.read_file(Path::new("assets/data.bin"))
                .unwrap_err()
                .kind(),
            io::ErrorKind::InvalidData
        );
        assert!(fs.is_dir(Path::new("empty/nested")));
        assert!(fs.read_dir(Path::new("empty/nested")).unwrap().is_empty());
        assert!(MemoryFs::new().read_dir(Path::new(".")).unwrap().is_empty());
    }

    #[test]
    fn real_fs_non_global_roots_reject_lexical_escape_before_io() {
        let fs = RealFs::new("workspace/root");
        for path in ["../secret", "sub/../../secret", "/absolute"] {
            assert_eq!(
                fs.read_bytes(Path::new(path)).unwrap_err().kind(),
                io::ErrorKind::PermissionDenied,
                "{path}"
            );
            assert!(!fs.exists(Path::new(path)));
        }
    }

    #[test]
    fn test_scoped_fs_reads_relative_to_scope_root() {
        let fs = MemoryFs::new()
            .with_file(
                "workspace/voplay/vo.mod",
                "format = 1\nmodule = \"github.com/vo-lang/voplay\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
            )
            .with_file("workspace/voplay/codec/codec.vo", "package codec\n");
        let scoped = ScopedFs::new(fs, "workspace/app/../voplay");

        let mod_file = scoped.read_file(Path::new("vo.mod")).unwrap();
        assert!(mod_file.contains("module = \"github.com/vo-lang/voplay\""));

        let entries = scoped.read_dir(Path::new(".")).unwrap();
        assert!(entries.contains(&PathBuf::from("vo.mod")));
        assert!(entries.contains(&PathBuf::from("codec")));

        let codec = scoped.read_file(Path::new("codec/codec.vo")).unwrap();
        assert_eq!(codec, "package codec\n");
        assert_eq!(scoped.root(), Some(Path::new("workspace/voplay")));
    }

    #[test]
    fn scoped_fs_rejects_lexical_escapes_and_overlay_preserves_the_error() {
        let primary = MemoryFs::new()
            .with_file("workspace/root/main.vo", "package main")
            .with_file("workspace/secret.vo", "package secret");
        let scoped = ScopedFs::new(primary, "workspace/root");
        for path in ["../secret.vo", "sub/../../secret.vo", "/absolute.vo"] {
            let error = scoped
                .read_file(Path::new(path))
                .expect_err("scope escape must be rejected");
            assert_eq!(error.kind(), io::ErrorKind::PermissionDenied, "{path}");
            assert!(!scoped.exists(Path::new(path)));
            assert!(!scoped.is_dir(Path::new(path)));
        }

        let secondary = MemoryFs::new().with_file("../secret.vo", "leaked");
        let overlay = OverlayFs::new(scoped, secondary);
        let error = overlay
            .read_file(Path::new("../secret.vo"))
            .expect_err("overlay must not mask scope errors with fallback content");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
    }

    #[test]
    fn overlay_respects_primary_file_and_directory_shadowing() {
        let primary_file = MemoryFs::new().with_file("node", "primary");
        let secondary_dir = MemoryFs::new().with_file("node/child", "secondary");
        let overlay = OverlayFs::new(primary_file, secondary_dir);
        assert_eq!(overlay.read_file(Path::new("node")).unwrap(), "primary");
        assert!(!overlay.is_dir(Path::new("node")));
        assert_eq!(
            overlay.read_dir(Path::new("node")).unwrap_err().kind(),
            io::ErrorKind::NotADirectory
        );

        let primary_dir = MemoryFs::new().with_dir("node");
        let secondary_file = MemoryFs::new().with_file("node", "secondary");
        let overlay = OverlayFs::new(primary_dir, secondary_file);
        assert!(overlay.is_dir(Path::new("node")));
        assert!(overlay.read_dir(Path::new("node")).unwrap().is_empty());
        assert!(overlay.read_file(Path::new("node")).is_err());
    }

    #[test]
    fn test_file_set_collect() {
        let fs = MemoryFs::new()
            .with_file("main.vo", "package main")
            .with_file("lib/util.vo", "package lib")
            .with_file("readme.md", "# Readme");

        let file_set = FileSet::collect(&fs, Path::new("."), PathBuf::from(".")).unwrap();

        // Should only collect .vo files in root (not subdirectories)
        assert_eq!(file_set.files.len(), 1);
        assert!(file_set.files.contains_key(Path::new("main.vo")));
        assert!(!file_set.files.contains_key(Path::new("lib/util.vo"))); // subdirectory
        assert!(!file_set.files.contains_key(Path::new("readme.md")));
    }

    #[test]
    fn source_size_limits_reject_oversized_lengths_without_allocating_them() {
        assert!(validate_text_size(MAX_TEXT_FILE_BYTES).is_ok());
        let error = validate_text_size(MAX_TEXT_FILE_BYTES + 1)
            .expect_err("oversized source must be rejected");
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    #[cfg(feature = "zip")]
    fn test_zip_fs_read_dir() {
        // Create a simple in-memory zip for testing
        let mut zip_buffer = std::io::Cursor::new(Vec::new());
        {
            let mut zip = zip::ZipWriter::new(&mut zip_buffer);
            let options = zip::write::SimpleFileOptions::default();
            zip.start_file("main.vo", options).unwrap();
            std::io::Write::write_all(&mut zip, b"package main").unwrap();
            zip.start_file("math/math.vo", options).unwrap();
            std::io::Write::write_all(&mut zip, b"package math").unwrap();
            zip.finish().unwrap();
        }

        zip_buffer.set_position(0);
        let zip_fs = ZipFs::from_reader(zip_buffer).unwrap();

        // Test read_dir at root
        let entries = zip_fs.read_dir(Path::new(".")).unwrap();
        assert!(entries.contains(&PathBuf::from("main.vo")));
        assert!(entries.contains(&PathBuf::from("math")));

        // Test is_dir
        assert!(!zip_fs.is_dir(Path::new("main.vo")));
        assert!(zip_fs.is_dir(Path::new("math")));

        // Test read_file
        let content = zip_fs.read_file(Path::new("main.vo")).unwrap();
        assert_eq!(content, "package main");
    }

    #[test]
    #[cfg(feature = "zip")]
    fn zip_fs_rejects_unsafe_and_duplicate_entries_and_preserves_binary_data() {
        fn archive(entries: &[(&str, &[u8])]) -> std::io::Cursor<Vec<u8>> {
            let mut buffer = std::io::Cursor::new(Vec::new());
            {
                let mut zip = zip::ZipWriter::new(&mut buffer);
                let options = zip::write::SimpleFileOptions::default();
                for (name, bytes) in entries {
                    zip.start_file(*name, options).unwrap();
                    std::io::Write::write_all(&mut zip, bytes).unwrap();
                }
                zip.finish().unwrap();
            }
            buffer.set_position(0);
            buffer
        }

        for name in ["../escape.vo", "/absolute.vo", "C:/drive.vo", "a\\b.vo"] {
            let error = ZipFs::from_reader(archive(&[(name, b"package bad")]))
                .expect_err("unsafe zip path must fail");
            assert_eq!(error.kind(), io::ErrorKind::InvalidData, "{name}");
        }

        let duplicate = archive(&[("pkg/./main.vo", b"first"), ("pkg/main.vo", b"second")]);
        assert_eq!(
            ZipFs::from_reader(duplicate).unwrap_err().kind(),
            io::ErrorKind::InvalidData
        );

        let invalid_utf8 = archive(&[("main.vo", &[0xff, 0xfe])]);
        let binary_fs = ZipFs::from_reader(invalid_utf8).unwrap();
        assert_eq!(
            binary_fs.read_bytes(Path::new("main.vo")).unwrap(),
            [0xff, 0xfe]
        );
        assert_eq!(
            binary_fs
                .read_file(Path::new("main.vo"))
                .unwrap_err()
                .kind(),
            io::ErrorKind::InvalidData
        );

        let rooted_archive = archive(&[
            ("root/main.vo", b"package main"),
            ("secret.vo", b"package secret"),
        ]);
        let rooted = ZipFs::from_reader_with_root(rooted_archive, "root").unwrap();
        assert_eq!(
            rooted.read_file(Path::new("main.vo")).unwrap(),
            "package main"
        );
        assert_eq!(
            rooted
                .read_file(Path::new("../secret.vo"))
                .unwrap_err()
                .kind(),
            io::ErrorKind::PermissionDenied
        );
    }

    #[test]
    #[cfg(feature = "zip")]
    fn zip_fs_preserves_explicit_empty_directories() {
        let mut buffer = std::io::Cursor::new(Vec::new());
        {
            let mut zip = zip::ZipWriter::new(&mut buffer);
            zip.add_directory("empty/", zip::write::SimpleFileOptions::default())
                .unwrap();
            zip.finish().unwrap();
        }
        buffer.set_position(0);

        let fs = ZipFs::from_reader(buffer).unwrap();
        assert!(fs.is_dir(Path::new("empty")));
        assert!(fs.read_dir(Path::new("empty")).unwrap().is_empty());
        assert_eq!(
            fs.read_dir(Path::new("missing")).unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
    }
}
