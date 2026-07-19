//! Race-resistant host filesystem reads for immutable compile snapshots.
//!
//! Every component is opened without following links. Regular files are read
//! twice through one descriptor, while directories are enumerated twice from
//! one pinned directory descriptor. Callers still capture the complete tree
//! twice; these local checks prevent one scan from accepting torn bytes or a
//! path that crosses a symlink/reparse boundary.

use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom};
use std::path::{Component, Path};
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum HostEntryKind {
    RegularFile,
    Directory,
    Symlink,
    Special,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct HostDirectoryEntry {
    pub(super) name: OsString,
    pub(super) kind: HostEntryKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct HostEntryIdentity {
    pub(super) volume: u64,
    pub(super) file: [u8; 16],
}

#[derive(Debug, Clone)]
pub(super) struct HostDirectorySnapshot {
    pub(super) identity: HostEntryIdentity,
    pub(super) generation: HostMetadataGeneration,
    pub(super) entries: Vec<HostDirectoryEntry>,
    pub(super) capability: HostDirectoryCapability,
}

#[derive(Debug, Clone)]
pub(super) struct HostFileSnapshot {
    pub(super) generation: HostMetadataGeneration,
    pub(super) bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
pub(super) struct HostDirectoryCapability {
    file: Arc<File>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct HostMetadataGeneration {
    pub(super) identity: HostEntryIdentity,
    pub(super) kind: HostEntryKind,
    pub(super) len: u64,
    pub(super) modified_seconds: i64,
    pub(super) modified_subseconds: i64,
    pub(super) changed_seconds: i64,
    pub(super) changed_subseconds: i64,
    pub(super) created_seconds: i64,
    pub(super) created_subseconds: i64,
    pub(super) attributes: u64,
    pub(super) links: u64,
    pub(super) delete_pending: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpenedKind {
    RegularFile,
    Directory,
}

const CACHE_DIRECTORY_TAG_SIGNATURE: &[u8] = b"Signature: 8a477f597d28d172789f06886806bc55";
const CACHE_DIRECTORY_TAG_MAX_BYTES: usize = 1024;

#[cfg(test)]
static DIRECTORY_ENUMERATION_COUNTS: std::sync::OnceLock<
    std::sync::Mutex<std::collections::BTreeMap<std::path::PathBuf, usize>>,
> = std::sync::OnceLock::new();

/// Returns the platform-independent case key used by the module protocol for
/// a host directory entry. Host names that cannot participate in the portable
/// UTF-8 namespace are rejected so callers never fall back to host-specific
/// byte or case semantics.
pub(super) fn portable_host_name_key(name: &OsStr, display_path: &Path) -> io::Result<String> {
    let name = name
        .to_str()
        .ok_or_else(|| non_portable_host_path(display_path))?;
    Ok(vo_module::schema::portable_case_key(name))
}

pub(super) fn portable_host_name_eq(
    name: &OsStr,
    expected: &str,
    display_path: &Path,
) -> io::Result<bool> {
    Ok(portable_host_name_key(name, display_path)?
        == vo_module::schema::portable_case_key(expected))
}

/// Compare paths within a portable namespace rooted at an opaque host path.
/// The root and paths outside it retain exact host spelling; only relative
/// descendants use the module protocol's case key and UTF-8 requirements.
pub(super) fn portable_host_path_eq_within(
    root: &Path,
    left: &Path,
    right: &Path,
) -> io::Result<bool> {
    let left_components = portable_host_path_components_within(root, left)?;
    let right_components = portable_host_path_components_within(root, right)?;
    match (left_components, right_components) {
        (Some(left), Some(right)) => Ok(left == right),
        _ => Ok(left == right),
    }
}

pub(super) fn portable_host_path_starts_with_within(
    root: &Path,
    path: &Path,
    base: &Path,
) -> io::Result<bool> {
    let path_components = portable_host_path_components_within(root, path)?;
    let base_components = portable_host_path_components_within(root, base)?;
    match (path_components, base_components) {
        (Some(path), Some(base)) => Ok(path.starts_with(&base)),
        _ => Ok(path.starts_with(base)),
    }
}

pub(super) fn validate_portable_host_path_within(root: &Path, path: &Path) -> io::Result<()> {
    portable_host_path_components_within(root, path)?
        .map_or_else(|| Err(non_portable_host_path_structure(path)), |_| Ok(()))
}

fn portable_host_path_components_within(
    root: &Path,
    path: &Path,
) -> io::Result<Option<Vec<String>>> {
    let Ok(relative) = path.strip_prefix(root) else {
        return Ok(None);
    };
    portable_relative_path_components(relative, path).map(Some)
}

fn portable_relative_path_components(
    relative: &Path,
    display_path: &Path,
) -> io::Result<Vec<String>> {
    relative
        .components()
        .map(|component| match component {
            Component::Normal(name) => portable_host_name_key(name, display_path),
            Component::Prefix(_)
            | Component::RootDir
            | Component::CurDir
            | Component::ParentDir => Err(non_portable_host_path_structure(display_path)),
        })
        .collect()
}

fn non_portable_host_path_structure(path: &Path) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input path escapes or uses a structural component inside the portable module namespace: {}",
            path.display(),
        ),
    )
}

fn non_portable_host_path(path: &Path) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input path contains a non-UTF-8 name in the portable module namespace: {}",
            path.display(),
        ),
    )
}

pub(super) fn read_stable_regular_file(path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
    read_stable_regular_file_snapshot(path, max_bytes).map(|snapshot| snapshot.bytes)
}

pub(super) fn read_stable_regular_file_snapshot(
    path: &Path,
    max_bytes: usize,
) -> io::Result<HostFileSnapshot> {
    let file = open_path_no_follow(path, OpenedKind::RegularFile)?;
    read_stable_regular_descriptor(file, None, path, max_bytes)
}

pub(super) fn read_stable_regular_child(
    parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    display_path: &Path,
    max_bytes: usize,
) -> io::Result<Vec<u8>> {
    read_stable_regular_child_snapshot(parent, name, display_path, max_bytes)
        .map(|snapshot| snapshot.bytes)
}

pub(super) fn read_stable_regular_child_snapshot(
    parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    display_path: &Path,
    max_bytes: usize,
) -> io::Result<HostFileSnapshot> {
    validate_child_name(name, display_path)?;
    let file = open_child_no_follow(parent, name, OpenedKind::RegularFile)?;
    read_stable_regular_descriptor(file, Some((parent, name)), display_path, max_bytes)
}

fn read_stable_regular_descriptor(
    mut file: File,
    parent: Option<(&HostDirectoryCapability, &std::ffi::OsStr)>,
    path: &Path,
    max_bytes: usize,
) -> io::Result<HostFileSnapshot> {
    let before = metadata_generation(&file)?;
    if before.kind != HostEntryKind::RegularFile {
        return Err(unsupported_path_type(path, "a regular file"));
    }
    let max_bytes_u64 = u64::try_from(max_bytes).unwrap_or(u64::MAX);
    if before.len > max_bytes_u64 {
        return Err(size_limit_error(
            path,
            max_bytes.saturating_add(1),
            max_bytes,
        ));
    }

    let first = read_descriptor_from_start(&mut file, path, max_bytes)?;
    let between = metadata_generation(&file)?;
    let second_matches = descriptor_matches_bytes(&mut file, path, &first, max_bytes)?;
    let after = metadata_generation(&file)?;
    if before != between
        || between != after
        || !second_matches
        || after.len != u64::try_from(first.len()).unwrap_or(u64::MAX)
    {
        return Err(generation_changed(path));
    }

    let current = match parent {
        Some((parent, name)) => open_child_no_follow(parent, name, OpenedKind::RegularFile)?,
        None => open_path_no_follow(path, OpenedKind::RegularFile)?,
    };
    if metadata_generation(&current)? != after {
        return Err(generation_changed(path));
    }
    Ok(HostFileSnapshot {
        generation: after,
        bytes: first,
    })
}

fn descriptor_matches_bytes(
    file: &mut File,
    path: &Path,
    expected: &[u8],
    max_bytes: usize,
) -> io::Result<bool> {
    file.seek(SeekFrom::Start(0))?;
    let mut offset = 0usize;
    let mut buffer = [0u8; 8 * 1024];
    loop {
        let count = match file.read(&mut buffer) {
            Ok(count) => count,
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error),
        };
        if count == 0 {
            return Ok(offset == expected.len());
        }
        let next = offset
            .checked_add(count)
            .ok_or_else(|| size_limit_error(path, usize::MAX, max_bytes))?;
        if next > max_bytes {
            return Err(size_limit_error(path, next, max_bytes));
        }
        if next > expected.len() || expected[offset..next] != buffer[..count] {
            return Ok(false);
        }
        offset = next;
    }
}

pub(super) fn read_stable_directory(
    path: &Path,
    max_entries: usize,
) -> io::Result<HostDirectorySnapshot> {
    let directory = open_path_no_follow(path, OpenedKind::Directory)?;
    read_stable_directory_descriptor(directory, None, path, max_entries)
}

pub(super) fn validate_stable_directory_path(path: &Path) -> io::Result<HostMetadataGeneration> {
    let directory = open_path_no_follow(path, OpenedKind::Directory)?;
    let generation = metadata_generation(&directory)?;
    if generation.kind != HostEntryKind::Directory {
        return Err(unsupported_path_type(path, "a directory"));
    }
    let current = open_path_no_follow(path, OpenedKind::Directory)?;
    if metadata_generation(&current)? != generation {
        return Err(generation_changed(path));
    }
    Ok(generation)
}

/// Confirm that an already-open regular-file handle still names the exact
/// object reachable through `path` without following links or reparse points.
/// Lock users call this after acquisition and again before destructive work,
/// closing the open-then-replace split-lock window.
pub(super) fn validate_open_regular_file_path_identity(
    file: &File,
    path: &Path,
) -> io::Result<HostEntryIdentity> {
    let opened = metadata_generation(file)?;
    if opened.kind != HostEntryKind::RegularFile || opened.delete_pending {
        return Err(unsupported_path_type(path, "a live regular file"));
    }
    let current = open_path_no_follow(path, OpenedKind::RegularFile)?;
    let current = metadata_generation(&current)?;
    if current.kind != HostEntryKind::RegularFile
        || current.delete_pending
        || current.identity != opened.identity
    {
        return Err(generation_changed(path));
    }
    Ok(opened.identity)
}

pub(super) fn read_stable_directory_child(
    parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    display_path: &Path,
    max_entries: usize,
) -> io::Result<HostDirectorySnapshot> {
    validate_child_name(name, display_path)?;
    let directory = open_child_no_follow(parent, name, OpenedKind::Directory)?;
    read_stable_directory_descriptor(directory, Some((parent, name)), display_path, max_entries)
}

pub(super) fn child_is_declared_cache_directory(
    parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    display_path: &Path,
) -> io::Result<bool> {
    validate_child_name(name, display_path)?;
    let directory = open_child_no_follow(parent, name, OpenedKind::Directory)?;
    let opened = metadata_generation(&directory)?;
    if opened.kind != HostEntryKind::Directory {
        return Err(unsupported_path_type(display_path, "a directory"));
    }
    let capability = HostDirectoryCapability {
        file: Arc::new(directory),
    };
    let tag_name = std::ffi::OsStr::new("CACHEDIR.TAG");
    let tag_path = display_path.join(tag_name);
    let Ok(tag) = read_stable_regular_child(
        &capability,
        tag_name,
        &tag_path,
        CACHE_DIRECTORY_TAG_MAX_BYTES,
    ) else {
        // A missing, linked, special, or unreadable tag does not authorize an
        // opaque cache. The caller will perform the ordinary strict walk and
        // report the underlying invalid entry if it remains observable.
        return Ok(false);
    };
    if !cache_directory_tag_has_signature(&tag) {
        return Ok(false);
    }

    // Cache contents may be changing concurrently. Bind the stable tag read
    // to the same directory object through identity only; directory metadata
    // generations intentionally change when cache entries are created or
    // removed and therefore cannot participate in this check.
    let current = open_child_no_follow(parent, name, OpenedKind::Directory)?;
    let current = metadata_generation(&current)?;
    if current.kind != HostEntryKind::Directory || current.identity != opened.identity {
        return Err(generation_changed(display_path));
    }
    Ok(true)
}

pub(super) fn cache_directory_tag_has_signature(bytes: &[u8]) -> bool {
    let Some(suffix) = bytes.strip_prefix(CACHE_DIRECTORY_TAG_SIGNATURE) else {
        return false;
    };
    suffix.starts_with(b"\n") || suffix.starts_with(b"\r\n")
}

fn open_stable_directory_child_capability(
    parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    display_path: &Path,
) -> io::Result<HostDirectoryCapability> {
    validate_child_name(name, display_path)?;
    let directory = open_child_no_follow(parent, name, OpenedKind::Directory)?;
    let generation = metadata_generation(&directory)?;
    if generation.kind != HostEntryKind::Directory {
        return Err(unsupported_path_type(display_path, "a directory"));
    }
    let current = open_child_no_follow(parent, name, OpenedKind::Directory)?;
    if metadata_generation(&current)? != generation {
        return Err(generation_changed(display_path));
    }
    Ok(HostDirectoryCapability {
        file: Arc::new(directory),
    })
}

#[cfg(test)]
pub(super) fn read_stable_descendant_directory(
    root: &HostDirectoryCapability,
    relative: &Path,
    display_path: &Path,
    max_entries: usize,
) -> io::Result<HostDirectorySnapshot> {
    let components = normalized_descendant_components(relative, display_path)?;
    if components.is_empty() {
        return read_stable_directory_descriptor(
            root.file.try_clone()?,
            None,
            display_path,
            max_entries,
        );
    }
    let mut parent = root.clone();
    let mut current_display = display_path.to_path_buf();
    for _ in 0..components.len() {
        current_display.pop();
    }
    for (index, name) in components.iter().enumerate() {
        current_display.push(name);
        if index + 1 == components.len() {
            return read_stable_directory_child(&parent, name, &current_display, max_entries);
        }
        parent = open_stable_directory_child_capability(&parent, name, &current_display)?;
    }
    unreachable!("non-empty descendant traversal must return its leaf")
}

pub(super) fn read_stable_descendant_file(
    root: &HostDirectoryCapability,
    relative: &Path,
    display_path: &Path,
    max_bytes: usize,
) -> io::Result<Vec<u8>> {
    let mut components = normalized_descendant_components(relative, display_path)?;
    let leaf = components.pop().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "compile input file path is empty: {}",
                display_path.display()
            ),
        )
    })?;
    let mut parent = root.clone();
    let mut current_display = display_path.to_path_buf();
    for _ in 0..components.len().saturating_add(1) {
        current_display.pop();
    }
    for name in components {
        current_display.push(&name);
        parent = open_stable_directory_child_capability(&parent, &name, &current_display)?;
    }
    read_stable_regular_child(&parent, &leaf, display_path, max_bytes)
}

fn normalized_descendant_components(
    relative: &Path,
    display_path: &Path,
) -> io::Result<Vec<OsString>> {
    let mut components = Vec::new();
    for component in relative.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::Normal(name) => components.push(name.to_os_string()),
            std::path::Component::Prefix(_)
            | std::path::Component::RootDir
            | std::path::Component::ParentDir => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "compile input descendant path must stay normalized beneath its root: {}",
                        display_path.display()
                    ),
                ));
            }
        }
    }
    Ok(components)
}

fn read_stable_directory_descriptor(
    directory: File,
    parent: Option<(&HostDirectoryCapability, &std::ffi::OsStr)>,
    path: &Path,
    max_entries: usize,
) -> io::Result<HostDirectorySnapshot> {
    let before = metadata_generation(&directory)?;
    if before.kind != HostEntryKind::Directory {
        return Err(unsupported_path_type(path, "a directory"));
    }
    record_directory_enumeration(path);
    let first = enumerate_directory(&directory, path, max_entries)?;
    let between = metadata_generation(&directory)?;
    record_directory_enumeration(path);
    let second = enumerate_directory(&directory, path, max_entries)?;
    let after = metadata_generation(&directory)?;
    if before != between || between != after || first != second {
        return Err(generation_changed(path));
    }

    let current = match parent {
        Some((parent, name)) => open_child_no_follow(parent, name, OpenedKind::Directory)?,
        None => open_path_no_follow(path, OpenedKind::Directory)?,
    };
    if metadata_generation(&current)? != after {
        return Err(generation_changed(path));
    }
    Ok(HostDirectorySnapshot {
        identity: after.identity.clone(),
        generation: after,
        entries: first,
        capability: HostDirectoryCapability {
            file: Arc::new(directory),
        },
    })
}

#[cfg(test)]
fn record_directory_enumeration(path: &Path) {
    let mut counts = DIRECTORY_ENUMERATION_COUNTS
        .get_or_init(Default::default)
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *counts.entry(path.to_path_buf()).or_default() += 1;
}

#[cfg(not(test))]
fn record_directory_enumeration(_path: &Path) {}

#[cfg(test)]
pub(super) fn directory_enumeration_count(path: &Path) -> usize {
    DIRECTORY_ENUMERATION_COUNTS
        .get_or_init(Default::default)
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(path)
        .copied()
        .unwrap_or_default()
}

fn validate_child_name(name: &std::ffi::OsStr, path: &Path) -> io::Result<()> {
    let mut components = Path::new(name).components();
    let valid = matches!(components.next(), Some(std::path::Component::Normal(found)) if found == name)
        && components.next().is_none();
    if valid {
        return Ok(());
    }
    Err(io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input directory returned an invalid child name for {}",
            path.display()
        ),
    ))
}

fn read_descriptor_from_start(
    file: &mut File,
    path: &Path,
    max_bytes: usize,
) -> io::Result<Vec<u8>> {
    file.seek(SeekFrom::Start(0))?;
    let mut bytes = Vec::new();
    file.take(
        u64::try_from(max_bytes)
            .unwrap_or(u64::MAX)
            .saturating_add(1),
    )
    .read_to_end(&mut bytes)?;
    if bytes.len() > max_bytes {
        return Err(size_limit_error(path, bytes.len(), max_bytes));
    }
    Ok(bytes)
}

fn size_limit_error(path: &Path, actual: usize, max: usize) -> io::Error {
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

fn unsupported_path_type(path: &Path, expected: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input path {} must resolve to {expected} without symbolic links, reparse points, or special entries",
            path.display(),
        ),
    )
}

fn generation_changed(path: &Path) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input at {} changed identity, metadata, or content while it was captured",
            path.display(),
        ),
    )
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn open_path_no_follow(path: &Path, expected: OpenedKind) -> io::Result<File> {
    use std::ffi::CString;
    use std::os::fd::{AsRawFd, FromRawFd};
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::OpenOptionsExt;
    use std::path::Component;

    let anchor = if path.is_absolute() {
        Path::new("/")
    } else {
        Path::new(".")
    };
    let mut directory = std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW)
        .open(anchor)?;
    let components = path
        .components()
        .filter_map(|component| match component {
            Component::Prefix(_) => Some(Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("unsupported compile input path prefix: {}", path.display()),
            ))),
            Component::RootDir | Component::CurDir => None,
            // Retain lexical parent traversal for sibling workspace members.
            // `..` is opened from the pinned directory descriptor, so every
            // traversed level keeps the same no-follow guarantee.
            Component::ParentDir => Some(Ok(OsString::from(".."))),
            Component::Normal(name) => Some(Ok(name.to_os_string())),
        })
        .collect::<io::Result<Vec<_>>>()?;
    if components.is_empty() {
        if expected == OpenedKind::Directory {
            return Ok(directory);
        }
        return Err(unsupported_path_type(path, "a regular file"));
    }

    for (index, name) in components.iter().enumerate() {
        let leaf = index + 1 == components.len();
        let component_expected = if leaf {
            expected
        } else {
            OpenedKind::Directory
        };
        let c_name = CString::new(name.as_os_str().as_bytes()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("compile input path contains a NUL byte: {}", path.display()),
            )
        })?;
        let mut flags = libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW;
        match component_expected {
            OpenedKind::Directory => flags |= libc::O_DIRECTORY,
            OpenedKind::RegularFile => flags |= libc::O_NONBLOCK,
        }
        let descriptor = loop {
            let descriptor = unsafe { libc::openat(directory.as_raw_fd(), c_name.as_ptr(), flags) };
            if descriptor >= 0 {
                break descriptor;
            }
            let error = io::Error::last_os_error();
            if error.kind() != io::ErrorKind::Interrupted {
                return Err(io::Error::new(
                    if matches!(error.raw_os_error(), Some(code) if code == libc::ELOOP || code == libc::ENOTDIR) {
                        io::ErrorKind::InvalidData
                    } else {
                        error.kind()
                    },
                    format!(
                        "cannot open compile input component {} in {} without following links: {error}",
                        name.to_string_lossy(),
                        path.display(),
                    ),
                ));
            }
        };
        directory = unsafe { File::from_raw_fd(descriptor) };
        let metadata = directory.metadata()?;
        let valid = match component_expected {
            OpenedKind::RegularFile => metadata.is_file(),
            OpenedKind::Directory => metadata.is_dir(),
        };
        if !valid {
            return Err(unsupported_path_type(
                path,
                match component_expected {
                    OpenedKind::RegularFile => "a regular file",
                    OpenedKind::Directory => "a directory",
                },
            ));
        }
    }
    Ok(directory)
}

#[cfg(windows)]
fn open_path_no_follow(path: &Path, expected: OpenedKind) -> io::Result<File> {
    use std::path::{Component, PathBuf};

    // `absolute` only qualifies the path. Every replaceable component is
    // still opened relative to the preceding pinned handle.
    let absolute = std::path::absolute(path)?;
    let mut anchor = PathBuf::new();
    let mut components = Vec::new();
    let mut saw_root = false;
    for component in absolute.components() {
        match component {
            Component::Prefix(prefix) => {
                if !anchor.as_os_str().is_empty() {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!(
                            "compile input path has multiple prefixes: {}",
                            path.display()
                        ),
                    ));
                }
                anchor.push(prefix.as_os_str());
            }
            Component::RootDir => {
                anchor.push(Path::new(r"\"));
                saw_root = true;
            }
            Component::CurDir => {}
            // Parent traversal is capability-relative. Sibling workspace
            // members need this path form, and each level keeps no-follow
            // enforcement.
            Component::ParentDir => components.push(OsString::from("..")),
            Component::Normal(name) => components.push(name.to_os_string()),
        }
    }
    if !saw_root || anchor.as_os_str().is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "compile input path could not be anchored to a Windows volume or share: {}",
                path.display(),
            ),
        ));
    }

    let mut current = open_windows_root_anchor(&anchor)?;
    validate_windows_opened_kind(&current, &anchor, OpenedKind::Directory)?;
    if components.is_empty() {
        if expected == OpenedKind::Directory {
            return Ok(current);
        }
        return Err(unsupported_path_type(path, "a regular file"));
    }

    for (index, name) in components.iter().enumerate() {
        let component_expected = if index + 1 == components.len() {
            expected
        } else {
            OpenedKind::Directory
        };
        current = open_windows_relative_handle(&current, name, component_expected, path)?;
    }
    Ok(current)
}

#[cfg(windows)]
fn open_windows_root_anchor(anchor: &Path) -> io::Result<File> {
    use std::os::windows::fs::OpenOptionsExt;
    use windows_sys::Win32::Storage::FileSystem::{
        FILE_FLAG_BACKUP_SEMANTICS, FILE_FLAG_OPEN_REPARSE_POINT, FILE_SHARE_DELETE,
        FILE_SHARE_READ, FILE_SHARE_WRITE,
    };

    let mut options = std::fs::OpenOptions::new();
    options
        .read(true)
        .share_mode(FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)
        .custom_flags(FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT);
    options.open(anchor)
}

#[cfg(windows)]
fn open_windows_relative_handle(
    parent: &File,
    name: &std::ffi::OsStr,
    expected: OpenedKind,
    display_path: &Path,
) -> io::Result<File> {
    use std::os::windows::ffi::OsStrExt;
    use std::os::windows::io::{AsRawHandle, FromRawHandle};
    use windows_sys::Wdk::Foundation::OBJECT_ATTRIBUTES;
    use windows_sys::Wdk::Storage::FileSystem::{
        NtCreateFile, FILE_DIRECTORY_FILE, FILE_NON_DIRECTORY_FILE, FILE_OPEN,
        FILE_OPEN_REPARSE_POINT, FILE_SYNCHRONOUS_IO_NONALERT,
    };
    use windows_sys::Win32::Foundation::{
        RtlNtStatusToDosError, HANDLE, OBJ_CASE_INSENSITIVE, UNICODE_STRING,
    };
    use windows_sys::Win32::Storage::FileSystem::{
        FILE_ATTRIBUTE_NORMAL, FILE_LIST_DIRECTORY, FILE_READ_ATTRIBUTES, FILE_READ_DATA,
        FILE_SHARE_DELETE, FILE_SHARE_READ, FILE_SHARE_WRITE, SYNCHRONIZE,
    };
    use windows_sys::Win32::System::IO::IO_STATUS_BLOCK;

    let mut wide_name = name.encode_wide().collect::<Vec<_>>();
    let byte_len = wide_name
        .len()
        .checked_mul(std::mem::size_of::<u16>())
        .and_then(|len| u16::try_from(len).ok())
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "compile input component is too long for a Windows capability open: {}",
                    display_path.display(),
                ),
            )
        })?;
    let object_name = UNICODE_STRING {
        Length: byte_len,
        MaximumLength: byte_len,
        Buffer: wide_name.as_mut_ptr(),
    };
    let object_attributes = OBJECT_ATTRIBUTES {
        Length: u32::try_from(std::mem::size_of::<OBJECT_ATTRIBUTES>())
            .expect("OBJECT_ATTRIBUTES size fits u32"),
        RootDirectory: parent.as_raw_handle() as HANDLE,
        ObjectName: &object_name,
        Attributes: OBJ_CASE_INSENSITIVE,
        SecurityDescriptor: std::ptr::null(),
        SecurityQualityOfService: std::ptr::null(),
    };
    let desired_access = FILE_READ_ATTRIBUTES
        | SYNCHRONIZE
        | match expected {
            OpenedKind::RegularFile => FILE_READ_DATA,
            OpenedKind::Directory => FILE_LIST_DIRECTORY,
        };
    let create_options = FILE_OPEN_REPARSE_POINT
        | FILE_SYNCHRONOUS_IO_NONALERT
        | match expected {
            OpenedKind::RegularFile => FILE_NON_DIRECTORY_FILE,
            OpenedKind::Directory => FILE_DIRECTORY_FILE,
        };
    let mut handle: HANDLE = std::ptr::null_mut();
    let mut io_status = IO_STATUS_BLOCK::default();
    // SAFETY: `wide_name`, `object_name`, `object_attributes`, `handle`, and
    // `io_status` remain alive and immovable for the call. Every length is in
    // bytes and was checked to fit the Windows ABI field. `RootDirectory`
    // borrows the live parent `File`; a successful call returns one newly
    // owned handle through `handle`.
    let status = unsafe {
        NtCreateFile(
            &mut handle,
            desired_access,
            &object_attributes,
            &mut io_status,
            std::ptr::null(),
            FILE_ATTRIBUTE_NORMAL,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            FILE_OPEN,
            create_options,
            std::ptr::null(),
            0,
        )
    };
    if status < 0 {
        // SAFETY: the failed NTSTATUS value is a plain integer accepted by
        // `RtlNtStatusToDosError` and carries no borrowed pointer.
        let code = unsafe { RtlNtStatusToDosError(status) };
        let raw_code = i32::try_from(code).unwrap_or(i32::MAX);
        let source = io::Error::from_raw_os_error(raw_code);
        return Err(io::Error::new(
            source.kind(),
            format!(
                "cannot open compile input component {} in {} through its parent handle: {source}",
                name.to_string_lossy(),
                display_path.display(),
            ),
        ));
    }
    if handle.is_null() {
        return Err(io::Error::other(format!(
            "Windows returned an empty handle for compile input {}",
            display_path.display(),
        )));
    }
    // SAFETY: `NtCreateFile` succeeded and returned a non-null, newly owned
    // handle. Transferring it to `File` gives Rust its sole closing owner.
    let file = unsafe { File::from_raw_handle(handle) };
    validate_windows_opened_kind(&file, display_path, expected)?;
    Ok(file)
}

#[cfg(windows)]
fn validate_windows_opened_kind(file: &File, path: &Path, expected: OpenedKind) -> io::Result<()> {
    let metadata = metadata_generation(file)?;
    let valid = matches!(
        (metadata.kind, expected),
        (HostEntryKind::RegularFile, OpenedKind::RegularFile)
            | (HostEntryKind::Directory, OpenedKind::Directory)
    );
    if valid {
        return Ok(());
    }
    Err(unsupported_path_type(
        path,
        match expected {
            OpenedKind::RegularFile => "a regular file",
            OpenedKind::Directory => "a directory",
        },
    ))
}

#[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
fn open_path_no_follow(path: &Path, _expected: OpenedKind) -> io::Result<File> {
    Err(unsupported_host_capability(path))
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn open_child_no_follow(
    parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    expected: OpenedKind,
) -> io::Result<File> {
    use std::ffi::CString;
    use std::os::fd::{AsRawFd, FromRawFd};
    use std::os::unix::ffi::OsStrExt;

    let c_name = CString::new(name.as_bytes()).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "compile input child name contains a NUL byte",
        )
    })?;
    let mut flags = libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW;
    match expected {
        OpenedKind::Directory => flags |= libc::O_DIRECTORY,
        OpenedKind::RegularFile => flags |= libc::O_NONBLOCK,
    }
    let descriptor = loop {
        let descriptor = unsafe { libc::openat(parent.file.as_raw_fd(), c_name.as_ptr(), flags) };
        if descriptor >= 0 {
            break descriptor;
        }
        let error = io::Error::last_os_error();
        if error.kind() != io::ErrorKind::Interrupted {
            return Err(io::Error::new(
                if matches!(error.raw_os_error(), Some(code) if code == libc::ELOOP || code == libc::ENOTDIR)
                {
                    io::ErrorKind::InvalidData
                } else {
                    error.kind()
                },
                format!(
                    "cannot open compile input child {} without following links: {error}",
                    name.to_string_lossy()
                ),
            ));
        }
    };
    let file = unsafe { File::from_raw_fd(descriptor) };
    let metadata = file.metadata()?;
    let valid = match expected {
        OpenedKind::RegularFile => metadata.is_file(),
        OpenedKind::Directory => metadata.is_dir(),
    };
    if !valid {
        return Err(unsupported_path_type(
            Path::new(name),
            match expected {
                OpenedKind::RegularFile => "a regular file",
                OpenedKind::Directory => "a directory",
            },
        ));
    }
    Ok(file)
}

#[cfg(windows)]
fn open_child_no_follow(
    parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    expected: OpenedKind,
) -> io::Result<File> {
    open_windows_relative_handle(&parent.file, name, expected, Path::new(name))
}

#[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
fn open_child_no_follow(
    _parent: &HostDirectoryCapability,
    name: &std::ffi::OsStr,
    _expected: OpenedKind,
) -> io::Result<File> {
    Err(unsupported_host_capability(Path::new(name)))
}

#[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
fn unsupported_host_capability(path: &Path) -> io::Error {
    io::Error::new(
        io::ErrorKind::Unsupported,
        format!(
            "host compilation at {} requires descriptor-relative, no-follow filesystem capabilities that are unavailable on this platform",
            path.display()
        ),
    )
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn enumerate_directory(
    directory: &File,
    path: &Path,
    max_entries: usize,
) -> io::Result<Vec<HostDirectoryEntry>> {
    use std::ffi::{CStr, CString};
    use std::os::fd::AsRawFd;
    use std::os::unix::ffi::OsStringExt;

    let dot = CString::new(".").expect("literal has no NUL byte");
    let descriptor = loop {
        let descriptor = unsafe {
            libc::openat(
                directory.as_raw_fd(),
                dot.as_ptr(),
                libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW,
            )
        };
        if descriptor >= 0 {
            break descriptor;
        }
        let error = io::Error::last_os_error();
        if error.kind() != io::ErrorKind::Interrupted {
            return Err(error);
        }
    };
    let stream = unsafe { libc::fdopendir(descriptor) };
    if stream.is_null() {
        let error = io::Error::last_os_error();
        unsafe { libc::close(descriptor) };
        return Err(error);
    }
    let stream = DirectoryStream(stream);
    let mut entries = Vec::new();
    loop {
        clear_directory_errno()?;
        let raw = unsafe { libc::readdir(stream.0) };
        if raw.is_null() {
            classify_null_readdir(directory_errno_code())?;
            break;
        }
        let name = unsafe { CStr::from_ptr((*raw).d_name.as_ptr()) }.to_bytes();
        if name == b"." || name == b".." {
            continue;
        }
        if entries.len() >= max_entries {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile input directory at {} exceeds the {}-entry limit",
                    path.display(),
                    max_entries,
                ),
            ));
        }
        let c_name = CString::new(name).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile input directory at {} contains a NUL name",
                    path.display()
                ),
            )
        })?;
        let mut stat = std::mem::MaybeUninit::<libc::stat>::uninit();
        let result = unsafe {
            libc::fstatat(
                directory.as_raw_fd(),
                c_name.as_ptr(),
                stat.as_mut_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if result != 0 {
            return Err(io::Error::last_os_error());
        }
        let mode = unsafe { stat.assume_init() }.st_mode & libc::S_IFMT;
        let kind = match mode {
            libc::S_IFREG => HostEntryKind::RegularFile,
            libc::S_IFDIR => HostEntryKind::Directory,
            libc::S_IFLNK => HostEntryKind::Symlink,
            _ => HostEntryKind::Special,
        };
        entries.push(HostDirectoryEntry {
            name: OsString::from_vec(name.to_vec()),
            kind,
        });
    }
    entries.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(entries)
}

#[cfg(windows)]
fn enumerate_directory(
    directory: &File,
    path: &Path,
    max_entries: usize,
) -> io::Result<Vec<HostDirectoryEntry>> {
    use std::os::windows::ffi::OsStringExt;
    use std::os::windows::io::AsRawHandle;
    use windows_sys::Win32::Foundation::{ERROR_NO_MORE_FILES, HANDLE};
    use windows_sys::Win32::Storage::FileSystem::{
        FileIdBothDirectoryInfo, FileIdBothDirectoryRestartInfo, GetFileInformationByHandleEx,
        FILE_ATTRIBUTE_DEVICE, FILE_ATTRIBUTE_DIRECTORY, FILE_ATTRIBUTE_REPARSE_POINT,
        FILE_ID_BOTH_DIR_INFO,
    };

    const BUFFER_BYTES: usize = 64 * 1024;
    const WORD_BYTES: usize = std::mem::size_of::<u64>();
    const FILE_NAME_OFFSET: usize = std::mem::offset_of!(FILE_ID_BOTH_DIR_INFO, FileName);

    let mut entries = Vec::new();
    let mut restart = true;
    loop {
        // A `u64` backing allocation satisfies the alignment required by
        // FILE_ID_BOTH_DIR_INFO while exposing a fixed-size byte buffer.
        let mut buffer = vec![0_u64; BUFFER_BYTES / WORD_BYTES];
        let class = if restart {
            FileIdBothDirectoryRestartInfo
        } else {
            FileIdBothDirectoryInfo
        };
        // SAFETY: the directory handle stays live for the call. `buffer` is
        // writable, aligned to `u64` (and thus FILE_ID_BOTH_DIR_INFO), and its
        // allocation is exactly BUFFER_BYTES long.
        let succeeded = unsafe {
            GetFileInformationByHandleEx(
                directory.as_raw_handle() as HANDLE,
                class,
                buffer.as_mut_ptr().cast(),
                u32::try_from(BUFFER_BYTES).expect("directory buffer size fits u32"),
            )
        };
        if succeeded == 0 {
            let error = io::Error::last_os_error();
            if error.raw_os_error() == i32::try_from(ERROR_NO_MORE_FILES).ok() {
                break;
            }
            return Err(error);
        }
        restart = false;

        let bytes = buffer.as_ptr().cast::<u8>();
        let mut offset = 0_usize;
        loop {
            let header_end = offset.checked_add(FILE_NAME_OFFSET).ok_or_else(|| {
                invalid_windows_directory_response(path, "entry header offset overflowed")
            })?;
            if header_end > BUFFER_BYTES {
                return Err(invalid_windows_directory_response(
                    path,
                    "entry header extends past the query buffer",
                ));
            }
            if offset % std::mem::align_of::<FILE_ID_BOTH_DIR_INFO>() != 0 {
                return Err(invalid_windows_directory_response(
                    path,
                    "entry header is not correctly aligned",
                ));
            }
            // SAFETY: the checked offset is aligned and leaves a complete
            // fixed header inside the still-live backing buffer.
            let info = unsafe { &*bytes.add(offset).cast::<FILE_ID_BOTH_DIR_INFO>() };
            let name_bytes = usize::try_from(info.FileNameLength).map_err(|_| {
                invalid_windows_directory_response(path, "file-name length does not fit usize")
            })?;
            if name_bytes % std::mem::size_of::<u16>() != 0 {
                return Err(invalid_windows_directory_response(
                    path,
                    "file-name byte length is not UTF-16 aligned",
                ));
            }
            let record_end = header_end.checked_add(name_bytes).ok_or_else(|| {
                invalid_windows_directory_response(path, "entry length overflowed")
            })?;
            if record_end > BUFFER_BYTES {
                return Err(invalid_windows_directory_response(
                    path,
                    "entry name extends past the query buffer",
                ));
            }
            if header_end % std::mem::align_of::<u16>() != 0 {
                return Err(invalid_windows_directory_response(
                    path,
                    "entry file name is not UTF-16 aligned",
                ));
            }
            // SAFETY: `header_end..record_end` was bounds-checked, the start
            // is u16-aligned, and the buffer remains alive for the slice use.
            let name = unsafe {
                std::slice::from_raw_parts(
                    bytes.add(header_end).cast::<u16>(),
                    name_bytes / std::mem::size_of::<u16>(),
                )
            };
            if name != [b'.' as u16] && name != [b'.' as u16, b'.' as u16] {
                if entries.len() >= max_entries {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "compile input directory at {} exceeds the {}-entry limit",
                            path.display(),
                            max_entries,
                        ),
                    ));
                }
                let attributes = info.FileAttributes;
                let kind = if attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
                    HostEntryKind::Symlink
                } else if attributes & FILE_ATTRIBUTE_DIRECTORY != 0 {
                    HostEntryKind::Directory
                } else if attributes & FILE_ATTRIBUTE_DEVICE != 0 {
                    HostEntryKind::Special
                } else {
                    HostEntryKind::RegularFile
                };
                entries.push(HostDirectoryEntry {
                    name: OsString::from_wide(name),
                    kind,
                });
            }

            let next = usize::try_from(info.NextEntryOffset).map_err(|_| {
                invalid_windows_directory_response(path, "next-entry offset does not fit usize")
            })?;
            if next == 0 {
                break;
            }
            if next < FILE_NAME_OFFSET {
                return Err(invalid_windows_directory_response(
                    path,
                    "next-entry offset overlaps the current header",
                ));
            }
            offset = offset.checked_add(next).ok_or_else(|| {
                invalid_windows_directory_response(path, "next-entry offset overflowed")
            })?;
            if offset >= BUFFER_BYTES {
                return Err(invalid_windows_directory_response(
                    path,
                    "next-entry offset extends past the query buffer",
                ));
            }
        }
    }
    entries.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(entries)
}

#[cfg(windows)]
fn invalid_windows_directory_response(path: &Path, detail: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "Windows returned a malformed handle-relative directory listing for {}: {detail}",
            path.display(),
        ),
    )
}

#[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
fn enumerate_directory(
    _directory: &File,
    path: &Path,
    _max_entries: usize,
) -> io::Result<Vec<HostDirectoryEntry>> {
    Err(unsupported_host_capability(path))
}

#[cfg(unix)]
fn metadata_generation(file: &File) -> io::Result<HostMetadataGeneration> {
    use std::os::unix::fs::MetadataExt;

    let metadata = file.metadata()?;
    Ok(HostMetadataGeneration {
        identity: HostEntryIdentity {
            volume: metadata.dev(),
            file: host_file_id_from_u64(metadata.ino()),
        },
        kind: if metadata.is_file() {
            HostEntryKind::RegularFile
        } else if metadata.is_dir() {
            HostEntryKind::Directory
        } else if metadata.file_type().is_symlink() {
            HostEntryKind::Symlink
        } else {
            HostEntryKind::Special
        },
        len: metadata.len(),
        modified_seconds: metadata.mtime(),
        modified_subseconds: metadata.mtime_nsec(),
        changed_seconds: metadata.ctime(),
        changed_subseconds: metadata.ctime_nsec(),
        created_seconds: 0,
        created_subseconds: 0,
        attributes: u64::from(metadata.mode()),
        links: metadata.nlink(),
        delete_pending: false,
    })
}

#[cfg(windows)]
fn metadata_generation(file: &File) -> io::Result<HostMetadataGeneration> {
    use windows_sys::Win32::Storage::FileSystem::{
        FILE_ATTRIBUTE_DEVICE, FILE_ATTRIBUTE_DIRECTORY, FILE_ATTRIBUTE_REPARSE_POINT,
    };

    let before = query_windows_metadata(file)?;
    let after = query_windows_metadata(file)?;
    if before != after {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Windows file metadata changed during its handle query",
        ));
    }
    let attributes = after.attributes;
    Ok(HostMetadataGeneration {
        identity: HostEntryIdentity {
            volume: after.volume,
            file: after.file,
        },
        kind: if attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
            HostEntryKind::Symlink
        } else if attributes & FILE_ATTRIBUTE_DIRECTORY != 0 {
            HostEntryKind::Directory
        } else if attributes & FILE_ATTRIBUTE_DEVICE != 0 {
            HostEntryKind::Special
        } else {
            HostEntryKind::RegularFile
        },
        len: after.len,
        modified_seconds: after.last_write_time,
        modified_subseconds: 0,
        changed_seconds: after.change_time,
        changed_subseconds: 0,
        created_seconds: after.creation_time,
        created_subseconds: 0,
        attributes: u64::from(attributes),
        links: u64::from(after.links),
        delete_pending: after.delete_pending,
    })
}

#[cfg(windows)]
#[derive(Debug, Clone, PartialEq, Eq)]
struct WindowsMetadataQuery {
    volume: u64,
    file: [u8; 16],
    creation_time: i64,
    last_write_time: i64,
    change_time: i64,
    attributes: u32,
    len: u64,
    links: u32,
    delete_pending: bool,
}

#[cfg(windows)]
fn query_windows_metadata(file: &File) -> io::Result<WindowsMetadataQuery> {
    use std::os::windows::io::AsRawHandle;
    use windows_sys::Win32::Foundation::HANDLE;
    use windows_sys::Win32::Storage::FileSystem::{
        FileBasicInfo, FileIdInfo, FileStandardInfo, GetFileInformationByHandleEx,
        FILE_ATTRIBUTE_DIRECTORY, FILE_BASIC_INFO, FILE_ID_INFO, FILE_STANDARD_INFO,
    };

    let handle = file.as_raw_handle() as HANDLE;
    let mut id = FILE_ID_INFO::default();
    // SAFETY: `file` owns a live handle for the duration of the call and `id`
    // is a writable FILE_ID_INFO with its exact ABI size.
    let id_succeeded = unsafe {
        GetFileInformationByHandleEx(
            handle,
            FileIdInfo,
            (&mut id as *mut FILE_ID_INFO).cast(),
            u32::try_from(std::mem::size_of::<FILE_ID_INFO>()).expect("FILE_ID_INFO size fits u32"),
        )
    };
    if id_succeeded == 0 {
        return Err(io::Error::last_os_error());
    }

    let mut basic = FILE_BASIC_INFO::default();
    // SAFETY: `file` owns a live handle for the duration of the call and
    // `basic` is a writable FILE_BASIC_INFO with its exact ABI size.
    let basic_succeeded = unsafe {
        GetFileInformationByHandleEx(
            handle,
            FileBasicInfo,
            (&mut basic as *mut FILE_BASIC_INFO).cast(),
            u32::try_from(std::mem::size_of::<FILE_BASIC_INFO>())
                .expect("FILE_BASIC_INFO size fits u32"),
        )
    };
    if basic_succeeded == 0 {
        return Err(io::Error::last_os_error());
    }

    let mut standard = FILE_STANDARD_INFO::default();
    // SAFETY: `file` owns a live handle for the duration of the call and
    // `standard` is a writable FILE_STANDARD_INFO with its exact ABI size.
    let standard_succeeded = unsafe {
        GetFileInformationByHandleEx(
            handle,
            FileStandardInfo,
            (&mut standard as *mut FILE_STANDARD_INFO).cast(),
            u32::try_from(std::mem::size_of::<FILE_STANDARD_INFO>())
                .expect("FILE_STANDARD_INFO size fits u32"),
        )
    };
    if standard_succeeded == 0 {
        return Err(io::Error::last_os_error());
    }

    let directory_from_attributes = basic.FileAttributes & FILE_ATTRIBUTE_DIRECTORY != 0;
    let directory_from_standard = standard.Directory;
    if directory_from_attributes != directory_from_standard {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Windows file metadata disagrees about whether the handle names a directory",
        ));
    }
    let len = u64::try_from(standard.EndOfFile).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "Windows returned a negative end-of-file position for a compile input",
        )
    })?;
    Ok(WindowsMetadataQuery {
        volume: id.VolumeSerialNumber,
        file: id.FileId.Identifier,
        creation_time: basic.CreationTime,
        last_write_time: basic.LastWriteTime,
        change_time: basic.ChangeTime,
        attributes: basic.FileAttributes,
        len,
        links: standard.NumberOfLinks,
        delete_pending: standard.DeletePending,
    })
}

#[cfg(not(any(unix, windows)))]
fn metadata_generation(file: &File) -> io::Result<HostMetadataGeneration> {
    use std::time::UNIX_EPOCH;

    let metadata = file.metadata()?;
    let modified = metadata
        .modified()
        .ok()
        .and_then(|time| time.duration_since(UNIX_EPOCH).ok());
    Ok(HostMetadataGeneration {
        identity: HostEntryIdentity {
            volume: 0,
            file: host_file_id_from_u64(metadata.len()),
        },
        kind: if metadata.is_file() {
            HostEntryKind::RegularFile
        } else if metadata.is_dir() {
            HostEntryKind::Directory
        } else if metadata.file_type().is_symlink() {
            HostEntryKind::Symlink
        } else {
            HostEntryKind::Special
        },
        len: metadata.len(),
        modified_seconds: modified
            .and_then(|duration| i64::try_from(duration.as_secs()).ok())
            .unwrap_or_default(),
        modified_subseconds: modified
            .map(|duration| i64::from(duration.subsec_nanos()))
            .unwrap_or_default(),
        changed_seconds: 0,
        changed_subseconds: 0,
        created_seconds: 0,
        created_subseconds: 0,
        attributes: 0,
        links: 0,
        delete_pending: false,
    })
}

#[cfg(not(windows))]
fn host_file_id_from_u64(file: u64) -> [u8; 16] {
    let mut encoded = [0; 16];
    encoded[..std::mem::size_of::<u64>()].copy_from_slice(&file.to_le_bytes());
    encoded
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
struct DirectoryStream(*mut libc::DIR);

#[cfg(all(unix, not(target_arch = "wasm32")))]
impl Drop for DirectoryStream {
    fn drop(&mut self) {
        unsafe { libc::closedir(self.0) };
    }
}

#[cfg(all(
    unix,
    not(target_arch = "wasm32"),
    any(target_os = "linux", target_os = "android")
))]
fn errno_location() -> *mut libc::c_int {
    unsafe { libc::__errno_location() }
}

#[cfg(all(
    unix,
    not(target_arch = "wasm32"),
    any(
        target_os = "macos",
        target_os = "ios",
        target_os = "freebsd",
        target_os = "dragonfly",
        target_os = "openbsd",
        target_os = "netbsd"
    )
))]
fn errno_location() -> *mut libc::c_int {
    unsafe { libc::__error() }
}

#[cfg(all(
    unix,
    not(target_arch = "wasm32"),
    not(any(
        target_os = "linux",
        target_os = "android",
        target_os = "macos",
        target_os = "ios",
        target_os = "freebsd",
        target_os = "dragonfly",
        target_os = "openbsd",
        target_os = "netbsd"
    ))
))]
fn errno_location() -> *mut libc::c_int {
    std::ptr::null_mut()
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn clear_directory_errno() -> io::Result<()> {
    let location = errno_location();
    if location.is_null() {
        return Err(readdir_errno_unavailable());
    }
    unsafe { *location = 0 };
    Ok(())
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn directory_errno_code() -> Option<libc::c_int> {
    let location = errno_location();
    if location.is_null() {
        return None;
    }
    Some(unsafe { *location })
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn classify_null_readdir(errno: Option<libc::c_int>) -> io::Result<()> {
    match errno {
        Some(0) => Ok(()),
        Some(code) => Err(io::Error::from_raw_os_error(code)),
        None => Err(readdir_errno_unavailable()),
    }
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn readdir_errno_unavailable() -> io::Error {
    io::Error::new(
        io::ErrorKind::Unsupported,
        "cannot distinguish readdir end-of-directory from failure because this Unix target does not expose thread-local errno",
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time")
            .as_nanos();
        std::env::temp_dir()
            .canonicalize()
            .unwrap_or_else(|_| std::env::temp_dir())
            .join(format!("vo-engine-host-input-{label}-{nonce}"))
    }

    #[cfg(all(unix, not(target_arch = "wasm32")))]
    #[test]
    fn null_readdir_requires_a_proven_zero_errno() {
        classify_null_readdir(Some(0)).expect("zero errno is an authenticated end-of-directory");

        let error = classify_null_readdir(Some(libc::EIO))
            .expect_err("non-zero errno must remain a directory read failure");
        assert_eq!(error.raw_os_error(), Some(libc::EIO));

        let unavailable = classify_null_readdir(None)
            .expect_err("missing errno storage must fail closed instead of signaling EOF");
        assert_eq!(unavailable.kind(), io::ErrorKind::Unsupported);
        assert!(unavailable.to_string().contains("thread-local errno"));
    }

    #[test]
    fn portable_host_paths_use_the_module_protocol_case_key() {
        assert!(
            portable_host_name_eq(OsStr::new(".GIT"), ".git", Path::new("module/.GIT"),)
                .expect("portable name comparison")
        );
        assert!(
            portable_host_name_eq(OsStr::new("Straße"), "STRASSE", Path::new("module/Straße"),)
                .expect("full Unicode case fold")
        );
        assert!(portable_host_path_eq_within(
            Path::new("module"),
            Path::new("module/Native/Target"),
            Path::new("module/native/target"),
        )
        .expect("portable path comparison"));
        assert!(portable_host_path_starts_with_within(
            Path::new("module"),
            Path::new("module/NATIVE/src/lib.rs"),
            Path::new("module/native"),
        )
        .expect("portable path prefix comparison"));
        assert!(!portable_host_path_starts_with_within(
            Path::new("module"),
            Path::new("module/nativeish/src/lib.rs"),
            Path::new("module/native"),
        )
        .expect("portable prefix comparison respects component boundaries"));
    }

    #[test]
    fn portable_host_paths_reject_namespace_escape() {
        let root = Path::new("module");
        let escaped = Path::new("module/../outside");
        let external = Path::new("outside");

        let equality_error = portable_host_path_eq_within(root, escaped, escaped)
            .expect_err("parent components must not escape a portable namespace");
        assert_eq!(equality_error.kind(), io::ErrorKind::InvalidData);

        let prefix_error = portable_host_path_starts_with_within(root, escaped, root)
            .expect_err("prefix comparisons must reject namespace escapes");
        assert_eq!(prefix_error.kind(), io::ErrorKind::InvalidData);

        for result in [
            portable_host_path_eq_within(root, escaped, external),
            portable_host_path_eq_within(root, external, escaped),
            portable_host_path_starts_with_within(root, escaped, external),
            portable_host_path_starts_with_within(root, external, escaped),
        ] {
            let error = result.expect_err("mixed comparisons must validate escaped operands");
            assert_eq!(error.kind(), io::ErrorKind::InvalidData);
        }
    }

    #[cfg(unix)]
    #[test]
    fn non_utf8_host_names_fail_portable_comparison_closed() {
        use std::os::unix::ffi::OsStrExt;

        let name = OsStr::from_bytes(b"target\xff");
        let path = Path::new("module").join(name);
        let error = portable_host_name_eq(name, "target", &path)
            .expect_err("non-UTF-8 host name must not receive host-specific semantics");
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
        assert!(error.to_string().contains("non-UTF-8"));
    }

    #[cfg(unix)]
    #[test]
    fn portable_host_paths_allow_identical_opaque_outer_components() {
        use std::os::unix::ffi::OsStringExt;

        let opaque_name = OsString::from_vec(b"project-\xff".to_vec());
        let opaque_root = Path::new("/host").join(&opaque_name);
        let left = opaque_root.join("Native/Target");
        let equivalent = opaque_root.join("native/target");
        assert!(
            portable_host_path_eq_within(&opaque_root, &left, &equivalent)
                .expect("portable descendants may live under an opaque host root")
        );

        let descendant = left.join("src/lib.rs");
        let base = opaque_root.join("native");
        assert!(
            portable_host_path_starts_with_within(&opaque_root, &descendant, &base)
                .expect("portable prefix comparison below an opaque container")
        );

        assert!(
            !portable_host_path_eq_within(&opaque_root, &left, Path::new("/other/cache"))
                .expect("paths outside the namespace retain exact host spelling")
        );

        let case_distinct_host_root = Path::new("/HOST").join(&opaque_name);
        let case_distinct_path = case_distinct_host_root.join("native/target");
        assert!(
            !portable_host_path_eq_within(&opaque_root, &left, &case_distinct_path,)
                .expect("case-distinct outer host paths must not alias")
        );
    }

    #[cfg(unix)]
    #[test]
    fn portable_host_paths_reject_non_utf8_descendants() {
        use std::os::unix::ffi::OsStringExt;

        let left = Path::new("/host")
            .join(OsString::from_vec(b"project-\xff".to_vec()))
            .join("main.vo");
        let right = Path::new("/host")
            .join(OsString::from_vec(b"project-\xfe".to_vec()))
            .join("main.vo");
        let error = portable_host_path_eq_within(Path::new("/host"), &left, &right)
            .expect_err("different opaque components must fail closed");
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
        assert!(error.to_string().contains("non-UTF-8"));

        let identical_error = portable_host_path_eq_within(Path::new("/host"), &left, &left)
            .expect_err("identical non-UTF-8 descendants must still fail closed");
        assert_eq!(identical_error.kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    fn stable_file_read_returns_exact_bytes() {
        let root = temp_dir("exact");
        std::fs::create_dir_all(&root).expect("create root");
        let path = root.join("main.vo");
        std::fs::write(&path, b"package main\n").expect("write source");

        assert_eq!(
            read_stable_regular_file(&path, 1024).expect("read source"),
            b"package main\n",
        );
        std::fs::remove_dir_all(root).expect("remove root");
    }

    #[test]
    fn directory_capability_enumerates_and_reads_descendants() {
        let root = temp_dir("descendant");
        let package = root.join("package");
        std::fs::create_dir_all(&package).expect("create package");
        std::fs::write(package.join("main.vo"), b"package main\n").expect("write source");

        let root_snapshot = read_stable_directory(&root, 8).expect("open root capability");
        assert_eq!(
            root_snapshot.entries,
            vec![HostDirectoryEntry {
                name: OsString::from("package"),
                kind: HostEntryKind::Directory,
            }],
        );
        let package_snapshot = read_stable_directory_child(
            &root_snapshot.capability,
            std::ffi::OsStr::new("package"),
            &package,
            8,
        )
        .expect("open child capability");
        assert_eq!(
            read_stable_regular_child(
                &package_snapshot.capability,
                std::ffi::OsStr::new("main.vo"),
                &package.join("main.vo"),
                1024,
            )
            .expect("read capability-relative source"),
            b"package main\n",
        );

        std::fs::remove_dir_all(root).expect("remove root");
    }

    #[test]
    fn descendant_traversal_does_not_enumerate_large_intermediate_directories() {
        let root = temp_dir("descendant-intermediate-budget");
        let intermediate = root.join("wide");
        let leaf = intermediate.join("leaf");
        std::fs::create_dir_all(&leaf).expect("create descendant tree");
        for index in 0..512 {
            std::fs::write(intermediate.join(format!("ignored-{index}")), b"x")
                .expect("populate wide intermediate directory");
        }
        let source = leaf.join("main.vo");
        std::fs::write(&source, b"package main\n").expect("write source");

        let root_snapshot = read_stable_directory(&root, 1).expect("open root capability");
        assert_eq!(
            read_stable_descendant_file(
                &root_snapshot.capability,
                Path::new("wide/leaf/main.vo"),
                &source,
                1024,
            )
            .expect("read descendant through non-enumerating intermediate capabilities"),
            b"package main\n",
        );
        let leaf_snapshot = read_stable_descendant_directory(
            &root_snapshot.capability,
            Path::new("wide/leaf"),
            &leaf,
            1,
        )
        .expect("enumerate only the bounded leaf directory");
        assert_eq!(leaf_snapshot.entries.len(), 1);
        assert_eq!(directory_enumeration_count(&intermediate), 0);
        assert_eq!(directory_enumeration_count(&leaf), 2);

        std::fs::remove_dir_all(root).expect("remove root");
    }

    #[test]
    fn stable_path_open_allows_capability_checked_parent_traversal() {
        let root = temp_dir("parent-traversal");
        let left = root.join("left");
        let right = root.join("right");
        std::fs::create_dir_all(&left).expect("create left sibling");
        std::fs::create_dir_all(&right).expect("create right sibling");
        std::fs::write(right.join("main.vo"), b"package sibling\n").expect("write sibling");

        assert_eq!(
            read_stable_regular_file(&left.join("../right/main.vo"), 1024)
                .expect("read sibling through checked parent traversal"),
            b"package sibling\n",
        );

        std::fs::remove_dir_all(root).expect("remove root");
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn stable_file_snapshot_retains_same_size_a_b_a_generation_changes() {
        let root = temp_dir("aba-generation");
        std::fs::create_dir_all(&root).expect("create root");
        let path = root.join("input.vo");
        let source_a = b"A\n";
        let source_b = b"B\n";
        assert_eq!(source_a.len(), source_b.len());
        std::fs::write(&path, source_a).expect("write original source");
        let original =
            read_stable_regular_file_snapshot(&path, 1024).expect("capture original generation");
        let original_modified = std::fs::metadata(&path)
            .expect("source metadata")
            .modified()
            .expect("source modification time");

        std::fs::write(&path, source_b).expect("write same-size intermediate source");
        std::fs::write(&path, source_a).expect("restore source bytes");
        std::fs::OpenOptions::new()
            .write(true)
            .open(&path)
            .expect("open restored source")
            .set_times(std::fs::FileTimes::new().set_modified(original_modified))
            .expect("restore modification time");
        let restored =
            read_stable_regular_file_snapshot(&path, 1024).expect("capture restored generation");

        assert_eq!(original.bytes, restored.bytes);
        assert_ne!(original.generation, restored.generation);
        std::fs::remove_dir_all(root).expect("remove root");
    }

    #[cfg(unix)]
    #[test]
    fn stable_file_read_rejects_a_symlinked_parent_component() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("parent-link");
        let outside = temp_dir("parent-link-outside");
        std::fs::create_dir_all(&root).expect("create root");
        std::fs::create_dir_all(&outside).expect("create outside");
        std::fs::write(outside.join("main.vo"), b"package outside\n").expect("write outside");
        symlink(&outside, root.join("linked")).expect("create parent link");

        let error = read_stable_regular_file(&root.join("linked/main.vo"), 1024)
            .expect_err("parent link must be rejected");
        assert_eq!(error.kind(), io::ErrorKind::InvalidData, "{error}");

        std::fs::remove_dir_all(root).expect("remove root");
        std::fs::remove_dir_all(outside).expect("remove outside");
    }

    #[cfg(unix)]
    #[test]
    fn stable_file_read_rejects_fifo_without_blocking() {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt;

        let root = temp_dir("fifo");
        std::fs::create_dir_all(&root).expect("create root");
        let fifo = root.join("input.vo");
        let fifo_c = CString::new(fifo.as_os_str().as_bytes()).expect("FIFO path");
        assert_eq!(unsafe { libc::mkfifo(fifo_c.as_ptr(), 0o600) }, 0);

        let error = read_stable_regular_file(&fifo, 1024).expect_err("FIFO must be rejected");
        assert_eq!(error.kind(), io::ErrorKind::InvalidData, "{error}");

        std::fs::remove_dir_all(root).expect("remove root");
    }
}
