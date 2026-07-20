use std::collections::BTreeSet;
use std::path::{Component, Path, PathBuf};

use vo_common::vfs::{
    normalize_fs_path, sort_fs_paths, FileSystem, FileSystemEntryKind, MAX_DIRECTORY_ENTRIES,
};

use crate::cache::validate::{
    InstalledModuleError, InstalledModuleErrorKind, InstalledModuleField,
};
use crate::digest::Digest;
use crate::schema::lockfile::LockedModule;
use crate::schema::{PortablePathSet, SourceFileEntry, SourceFileMode};

const MAX_INSTALLED_SOURCE_SCAN_DEPTH: usize = crate::schema::MAX_PORTABLE_PATH_COMPONENTS;
const MAX_INSTALLED_SOURCE_SCAN_ENTRIES: usize = crate::MAX_SOURCE_ARCHIVE_ENTRIES
    .saturating_add(crate::schema::source_files::CACHE_OWNED_ROOT_ENTRY_BUDGET);
pub(super) const MAX_CACHE_MARKER_BYTES: usize = 512;

/// One authenticated view of the installed package source tree.
///
/// Integrity-sensitive metadata consumers must parse `mod_file_bytes` directly:
/// reopening `vo.mod` after this scan would let a concurrent replacement supply
/// bytes that were never checked against `vo.tree.json`.
#[derive(Debug, PartialEq, Eq)]
pub(super) struct InstalledSourceSnapshot {
    pub(super) entries: Vec<SourceFileEntry>,
    pub(super) mod_file_bytes: Option<Vec<u8>>,
    pub(super) version_marker_bytes: Option<Vec<u8>>,
    pub(super) source_digest_marker_bytes: Option<Vec<u8>>,
}

fn error(
    locked: &LockedModule,
    field: InstalledModuleField,
    kind: InstalledModuleErrorKind,
) -> InstalledModuleError {
    InstalledModuleError {
        module: locked.path.to_string(),
        version: locked.version.to_string(),
        field,
        kind: Box::new(kind),
    }
}

pub(super) fn entry_kind<F: FileSystem>(
    fs: &F,
    path: &Path,
    locked: &LockedModule,
    field: InstalledModuleField,
) -> Result<FileSystemEntryKind, InstalledModuleError> {
    fs.entry_kind(path).map_err(|source| {
        error(
            locked,
            field,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "cannot inspect {} without following symbolic links: {source}",
                    path.display(),
                ),
            },
        )
    })
}

pub(super) fn require_regular_file(
    kind: FileSystemEntryKind,
    path: &Path,
    locked: &LockedModule,
    field: InstalledModuleField,
) -> Result<(), InstalledModuleError> {
    match kind {
        FileSystemEntryKind::RegularFile => Ok(()),
        FileSystemEntryKind::Missing => Err(error(
            locked,
            field,
            InstalledModuleErrorKind::Missing {
                detail: format!("{} is missing", path.display()),
            },
        )),
        other => Err(error(
            locked,
            field,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "{} must be a regular file without symbolic links; found {other:?}",
                    path.display(),
                ),
            },
        )),
    }
}

pub(super) fn require_directory(
    kind: FileSystemEntryKind,
    path: &Path,
    locked: &LockedModule,
    field: InstalledModuleField,
) -> Result<(), InstalledModuleError> {
    match kind {
        FileSystemEntryKind::Directory => Ok(()),
        FileSystemEntryKind::Missing => Err(error(
            locked,
            field,
            InstalledModuleErrorKind::Missing {
                detail: format!("directory {} is missing", path.display()),
            },
        )),
        other => Err(error(
            locked,
            field,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "{} must be a directory without symbolic links; found {other:?}",
                    path.display(),
                ),
            },
        )),
    }
}

/// Validate the canonical cache location and every directory component below
/// the filesystem root without following symbolic links.
///
/// `FileSystem::root()` is the caller-authorized trust boundary.  The cache
/// key and exact-version directory below it are both integrity-sensitive and
/// therefore must independently be real directories.
pub(super) fn validate_module_directory_chain<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
    field: InstalledModuleField,
) -> Result<PathBuf, InstalledModuleError> {
    let expected = super::layout::relative_module_dir(&locked.path, &locked.version);
    if module_dir != expected {
        return Err(error(
            locked,
            field,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "module cache directory must be {}, found {}",
                    expected.display(),
                    module_dir.display(),
                ),
            },
        ));
    }

    let mut current = PathBuf::new();
    for component in expected.components() {
        let Component::Normal(component) = component else {
            return Err(error(
                locked,
                field,
                InstalledModuleErrorKind::ValidationFailed {
                    detail: format!(
                        "module cache directory contains a non-canonical component: {}",
                        expected.display(),
                    ),
                },
            ));
        };
        current.push(component);
        require_exact_child_spelling(fs, &current, locked, field)?;
        let kind = entry_kind(fs, &current, locked, field)?;
        require_directory(kind, &current, locked, field)?;
    }
    Ok(expected)
}

pub(super) fn require_exact_child_spelling<F: FileSystem>(
    fs: &F,
    path: &Path,
    locked: &LockedModule,
    field: InstalledModuleField,
) -> Result<(), InstalledModuleError> {
    let Some(expected_name) = path.file_name() else {
        return Ok(());
    };
    let parent = path.parent().unwrap_or_else(|| Path::new(""));
    let children = fs.read_dir(parent).map_err(|source| {
        if source.kind() == std::io::ErrorKind::NotFound {
            return error(
                locked,
                field,
                InstalledModuleErrorKind::Missing {
                    detail: format!("directory {} is missing", parent.display()),
                },
            );
        }
        error(
            locked,
            field,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "cannot enumerate {} to verify exact cache spelling: {source}",
                    parent.display(),
                ),
            },
        )
    })?;
    if children.len() > MAX_DIRECTORY_ENTRIES {
        return Err(error(
            locked,
            field,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "directory {} contains more than {MAX_DIRECTORY_ENTRIES} entries while verifying exact cache spelling",
                    parent.display(),
                ),
            },
        ));
    }
    let expected_text = expected_name.to_str();
    let expected_key = expected_text.map(crate::schema::portable_case_key);
    for child in children {
        let Some(actual_name) = child.file_name() else {
            continue;
        };
        if actual_name == expected_name {
            continue;
        }
        if let (Some(expected_key), Some(actual_text)) = (&expected_key, actual_name.to_str()) {
            if crate::schema::portable_case_key(actual_text) == *expected_key {
                return Err(error(
                    locked,
                    field,
                    InstalledModuleErrorKind::ValidationFailed {
                        detail: format!(
                            "cache path {} conflicts with portable spelling {:?}",
                            path.display(),
                            actual_text,
                        ),
                    },
                ));
            }
        }
    }
    // Absence is classified by the following typed entry check. This pass is
    // responsible only for rejecting ambiguous portable aliases.
    Ok(())
}

fn cache_owned_root_entry(path: &str) -> Option<&'static str> {
    let first = path.split('/').next()?;
    let first_key = crate::schema::portable_case_key(first);
    [
        "artifacts",
        super::layout::VERSION_MARKER,
        super::layout::SOURCE_DIGEST_MARKER,
        "vo.release.json",
        "vo.tree.json",
    ]
    .into_iter()
    .find(|reserved| first_key == crate::schema::portable_case_key(reserved))
}

/// Reconstruct the published package source set from an installed module tree.
/// The walk is iterative and every resource dimension is bounded.
pub(super) fn scan<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
    expected_files: &[SourceFileEntry],
) -> Result<InstalledSourceSnapshot, InstalledModuleError> {
    scan_with_path_set(
        fs,
        module_dir,
        locked,
        expected_files,
        PortablePathSet::default(),
    )
}

fn scan_with_path_set<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
    expected_files: &[SourceFileEntry],
    mut seen_paths: PortablePathSet,
) -> Result<InstalledSourceSnapshot, InstalledModuleError> {
    let canonical_module_dir =
        validate_module_directory_chain(fs, module_dir, locked, InstalledModuleField::Directory)?;

    let mut pending = vec![(canonical_module_dir.clone(), 0usize)];
    let mut source_entries = Vec::new();
    let mut installed_directories = BTreeSet::new();
    let mut visited_entries = 0usize;
    let mut total_read_bytes = 0usize;
    let mut mod_file_bytes = None;
    let mut version_marker_bytes = None;
    let mut source_digest_marker_bytes = None;

    while let Some((directory, depth)) = pending.pop() {
        let mut children = fs.read_dir(&directory).map_err(|source| {
            error(
                locked,
                InstalledModuleField::SourceFiles,
                InstalledModuleErrorKind::ValidationFailed {
                    detail: format!("cannot enumerate {}: {source}", directory.display()),
                },
            )
        })?;
        sort_fs_paths(&mut children);
        visited_entries = visited_entries.checked_add(children.len()).ok_or_else(|| {
            error(
                locked,
                InstalledModuleField::SourceFiles,
                InstalledModuleErrorKind::ValidationFailed {
                    detail: "installed source entry count overflows usize".to_string(),
                },
            )
        })?;
        if visited_entries > MAX_INSTALLED_SOURCE_SCAN_ENTRIES {
            return Err(error(
                locked,
                InstalledModuleField::SourceFiles,
                InstalledModuleErrorKind::ValidationFailed {
                    detail: format!(
                        "installed source tree contains more than {MAX_INSTALLED_SOURCE_SCAN_ENTRIES} materialized source and cache-owned entries"
                    ),
                },
            ));
        }

        let mut next_directories = Vec::new();
        for child in children {
            validate_child_path(locked, &directory, &child)?;
            let relative = child.strip_prefix(&canonical_module_dir).map_err(|_| {
                error(
                    locked,
                    InstalledModuleField::SourceFiles,
                    InstalledModuleErrorKind::ValidationFailed {
                        detail: format!(
                            "filesystem entry {} escapes module cache directory {}",
                            child.display(),
                            canonical_module_dir.display(),
                        ),
                    },
                )
            })?;
            let portable =
                crate::schema::portable_relative_path_from_path(relative).map_err(|source| {
                    error(
                        locked,
                        InstalledModuleField::SourceFiles,
                        InstalledModuleErrorKind::ValidationFailed {
                            detail: format!(
                                "installed source path {} is not canonical and portable: {source}",
                                relative.display(),
                            ),
                        },
                    )
                })?;
            let kind = entry_kind(fs, &child, locked, InstalledModuleField::SourceFiles)?;

            if !crate::schema::is_package_file_candidate(&portable).map_err(|detail| {
                error(
                    locked,
                    InstalledModuleField::SourceFiles,
                    InstalledModuleErrorKind::ValidationFailed { detail },
                )
            })? {
                if let Some(canonical_reserved) = cache_owned_root_entry(&portable) {
                    validate_cache_owned_entry(
                        locked,
                        &child,
                        &portable,
                        canonical_reserved,
                        kind,
                    )?;
                    let marker_capture = match canonical_reserved {
                        super::layout::VERSION_MARKER => Some((
                            InstalledModuleField::VersionMarker,
                            &mut version_marker_bytes,
                        )),
                        super::layout::SOURCE_DIGEST_MARKER => Some((
                            InstalledModuleField::SourceDigestMarker,
                            &mut source_digest_marker_bytes,
                        )),
                        _ => None,
                    };
                    if let Some((field, capture)) = marker_capture {
                        if capture.is_some() {
                            return Err(error(
                                locked,
                                field,
                                InstalledModuleErrorKind::ValidationFailed {
                                    detail: format!(
                                        "filesystem returned duplicate cache marker {canonical_reserved}"
                                    ),
                                },
                            ));
                        }
                        *capture = Some(read_cache_marker_bytes(fs, locked, &child, field)?);
                    }
                } else {
                    return Err(error(
                        locked,
                        InstalledModuleField::SourceFiles,
                        InstalledModuleErrorKind::ValidationFailed {
                            detail: format!(
                                "installed source contains obsolete or reserved protocol path {portable:?}"
                            ),
                        },
                    ));
                }
                continue;
            }

            match kind {
                FileSystemEntryKind::Directory => {
                    let child_depth = depth.checked_add(1).ok_or_else(|| {
                        error(
                            locked,
                            InstalledModuleField::SourceFiles,
                            InstalledModuleErrorKind::ValidationFailed {
                                detail: "installed source depth overflows usize".to_string(),
                            },
                        )
                    })?;
                    if child_depth > MAX_INSTALLED_SOURCE_SCAN_DEPTH {
                        return Err(error(
                            locked,
                            InstalledModuleField::SourceFiles,
                            InstalledModuleErrorKind::ValidationFailed {
                                detail: format!(
                                    "installed source tree exceeds the {MAX_INSTALLED_SOURCE_SCAN_DEPTH}-level depth limit at {}",
                                    relative.display(),
                                ),
                            },
                        ));
                    }
                    insert_directory(locked, &mut seen_paths, &portable)?;
                    installed_directories.insert(portable);
                    next_directories.push((child, child_depth));
                }
                FileSystemEntryKind::RegularFile => {
                    insert_file(locked, &mut seen_paths, &portable)?;
                    let bytes =
                        read_bounded_entry(fs, locked, &child, relative, &mut total_read_bytes)?;
                    if source_entries.len() >= crate::MAX_SOURCE_ARCHIVE_ENTRIES.saturating_sub(1) {
                        return Err(error(
                            locked,
                            InstalledModuleField::SourceFiles,
                            InstalledModuleErrorKind::ValidationFailed {
                                detail: format!(
                                    "installed source tree contains more than {} files",
                                    crate::MAX_SOURCE_ARCHIVE_ENTRIES.saturating_sub(1),
                                ),
                            },
                        ));
                    }
                    let is_mod_file = portable == "vo.mod";
                    let expected_mode = expected_files
                        .binary_search_by(|entry| entry.path.as_str().cmp(&portable))
                        .ok()
                        .map(|index| expected_files[index].mode)
                        .unwrap_or(SourceFileMode::Regular);
                    if let Some(executable) = fs.executable_mode(&child).map_err(|source| {
                        error(
                            locked,
                            InstalledModuleField::SourceFiles,
                            InstalledModuleErrorKind::ValidationFailed {
                                detail: format!(
                                    "cannot inspect executable mode for {}: {source}",
                                    relative.display(),
                                ),
                            },
                        )
                    })? {
                        if executable != expected_mode.is_executable() {
                            return Err(error(
                                locked,
                                InstalledModuleField::SourceFiles,
                                InstalledModuleErrorKind::ValidationFailed {
                                    detail: format!(
                                        "installed source file {} executable mode drifted from authenticated {:?}",
                                        relative.display(),
                                        expected_mode,
                                    ),
                                },
                            ));
                        }
                    }
                    source_entries.push(SourceFileEntry {
                        path: portable,
                        mode: expected_mode,
                        size: u64::try_from(bytes.len()).map_err(|_| {
                            error(
                                locked,
                                InstalledModuleField::SourceFiles,
                                InstalledModuleErrorKind::ValidationFailed {
                                    detail: "installed source file size exceeds u64".to_string(),
                                },
                            )
                        })?,
                        digest: Digest::from_sha256(&bytes),
                    });
                    if is_mod_file {
                        mod_file_bytes = Some(bytes);
                    }
                }
                other => {
                    return Err(error(
                        locked,
                        InstalledModuleField::SourceFiles,
                        InstalledModuleErrorKind::ValidationFailed {
                            detail: format!(
                                "installed source entry {} must be a regular file or directory without symbolic links; found {other:?}",
                                relative.display(),
                            ),
                        },
                    ));
                }
            }
        }
        // `pending` is LIFO, so reverse the already-sorted children to visit
        // directories in ascending canonical path order.
        pending.extend(next_directories.into_iter().rev());
    }

    source_entries.sort_by(|left, right| left.path.cmp(&right.path));
    let mut required_directories = BTreeSet::new();
    for entry in &source_entries {
        let mut path = entry.path.as_str();
        while let Some((parent, _)) = path.rsplit_once('/') {
            required_directories.insert(parent.to_string());
            path = parent;
        }
    }
    if let Some(extra) = installed_directories
        .difference(&required_directories)
        .next()
    {
        return Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "installed source tree contains unauthenticated empty directory {extra:?}"
                ),
            },
        ));
    }
    Ok(InstalledSourceSnapshot {
        entries: source_entries,
        mod_file_bytes,
        version_marker_bytes,
        source_digest_marker_bytes,
    })
}

fn read_cache_marker_bytes<F: FileSystem>(
    fs: &F,
    locked: &LockedModule,
    path: &Path,
    field: InstalledModuleField,
) -> Result<Vec<u8>, InstalledModuleError> {
    fs.read_bytes_limited(path, MAX_CACHE_MARKER_BYTES)
        .map_err(|source| {
            error(
                locked,
                field,
                InstalledModuleErrorKind::ValidationFailed {
                    detail: format!(
                        "cannot read cache marker {} within the {MAX_CACHE_MARKER_BYTES}-byte limit: {source}",
                        path.display(),
                    ),
                },
            )
        })
}

fn validate_child_path(
    locked: &LockedModule,
    directory: &Path,
    child: &Path,
) -> Result<(), InstalledModuleError> {
    if normalize_fs_path(child) == child && child.parent() == Some(directory) {
        return Ok(());
    }
    Err(error(
        locked,
        InstalledModuleField::SourceFiles,
        InstalledModuleErrorKind::ValidationFailed {
            detail: format!(
                "filesystem returned non-canonical or non-child entry {} for {}",
                child.display(),
                directory.display(),
            ),
        },
    ))
}

fn validate_cache_owned_entry(
    locked: &LockedModule,
    path: &Path,
    portable: &str,
    canonical: &str,
    kind: FileSystemEntryKind,
) -> Result<(), InstalledModuleError> {
    if portable != canonical {
        return Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "installed source path {portable:?} aliases cache-owned path {canonical:?}"
                ),
            },
        ));
    }
    if canonical == "artifacts" {
        if kind == FileSystemEntryKind::Directory {
            return Ok(());
        }
        return Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed {
                detail: "cache-owned artifacts path must be a directory".to_string(),
            },
        ));
    }
    let field = match canonical {
        super::layout::VERSION_MARKER => InstalledModuleField::VersionMarker,
        super::layout::SOURCE_DIGEST_MARKER => InstalledModuleField::SourceDigestMarker,
        "vo.release.json" => InstalledModuleField::ReleaseManifest,
        "vo.tree.json" => InstalledModuleField::TreeManifest,
        _ => InstalledModuleField::SourceFiles,
    };
    require_regular_file(kind, path, locked, field)
}

fn insert_directory(
    locked: &LockedModule,
    paths: &mut PortablePathSet,
    portable: &str,
) -> Result<(), InstalledModuleError> {
    match paths.insert_directory(portable) {
        Ok(true) => Ok(()),
        Ok(false) => Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!("filesystem returned duplicate directory {portable:?}"),
            },
        )),
        Err(detail) => Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed { detail },
        )),
    }
}

fn insert_file(
    locked: &LockedModule,
    paths: &mut PortablePathSet,
    portable: &str,
) -> Result<(), InstalledModuleError> {
    match paths.insert_file(portable) {
        Ok(true) => Ok(()),
        Ok(false) => Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!("filesystem returned duplicate file {portable:?}"),
            },
        )),
        Err(detail) => Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed { detail },
        )),
    }
}

fn read_bounded_entry<F: FileSystem>(
    fs: &F,
    locked: &LockedModule,
    path: &Path,
    relative: &Path,
    total_read_bytes: &mut usize,
) -> Result<Vec<u8>, InstalledModuleError> {
    let bytes = fs
        .read_bytes_limited(path, crate::MAX_SOURCE_ARCHIVE_ENTRY_BYTES)
        .map_err(|source| {
            error(
                locked,
                InstalledModuleField::SourceFiles,
                InstalledModuleErrorKind::ValidationFailed {
                    detail: format!(
                        "cannot read installed source {} within the {}-byte entry limit: {source}",
                        relative.display(),
                        crate::MAX_SOURCE_ARCHIVE_ENTRY_BYTES,
                    ),
                },
            )
        })?;
    *total_read_bytes = total_read_bytes.checked_add(bytes.len()).ok_or_else(|| {
        error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed {
                detail: "installed source read size overflows usize".to_string(),
            },
        )
    })?;
    if *total_read_bytes > crate::MAX_EXTRACTED_SOURCE_BYTES {
        return Err(error(
            locked,
            InstalledModuleField::SourceFiles,
            InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "installed source tree exceeds the {}-byte scan limit",
                    crate::MAX_EXTRACTED_SOURCE_BYTES,
                ),
            },
        ));
    }
    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::identity::ModulePath;
    use crate::schema::lockfile::LockedModule;
    use crate::version::ExactVersion;
    use std::path::PathBuf;
    use vo_common::vfs::MemoryFs;

    struct UnknownKindFs {
        inner: MemoryFs,
        unknown_path: PathBuf,
    }

    struct ReverseReadDirFs(MemoryFs);

    struct OversizedReadDirFs;

    impl FileSystem for ReverseReadDirFs {
        fn read_file(&self, path: &Path) -> std::io::Result<String> {
            self.0.read_file(path)
        }

        fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            self.0.read_bytes(path)
        }

        fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> std::io::Result<Vec<u8>> {
            self.0.read_bytes_limited(path, max_bytes)
        }

        fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
            let mut entries = self.0.read_dir(path)?;
            entries.reverse();
            Ok(entries)
        }

        fn exists(&self, path: &Path) -> bool {
            self.0.exists(path)
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.0.is_dir(path)
        }

        fn entry_kind(&self, path: &Path) -> std::io::Result<FileSystemEntryKind> {
            self.0.entry_kind(path)
        }
    }

    impl FileSystem for UnknownKindFs {
        fn read_file(&self, path: &Path) -> std::io::Result<String> {
            self.inner.read_file(path)
        }

        fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            self.inner.read_bytes(path)
        }

        fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> std::io::Result<Vec<u8>> {
            self.inner.read_bytes_limited(path, max_bytes)
        }

        fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
            self.inner.read_dir(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.inner.exists(path)
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.inner.is_dir(path)
        }

        fn entry_kind(&self, path: &Path) -> std::io::Result<FileSystemEntryKind> {
            if path == self.unknown_path {
                return Ok(FileSystemEntryKind::Unknown);
            }
            self.inner.entry_kind(path)
        }
    }

    impl FileSystem for OversizedReadDirFs {
        fn read_file(&self, _path: &Path) -> std::io::Result<String> {
            unreachable!()
        }

        fn read_bytes(&self, _path: &Path) -> std::io::Result<Vec<u8>> {
            unreachable!()
        }

        fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
            Ok((0..=MAX_DIRECTORY_ENTRIES)
                .map(|index| path.join(format!("entry-{index}")))
                .collect())
        }

        fn exists(&self, _path: &Path) -> bool {
            true
        }

        fn is_dir(&self, _path: &Path) -> bool {
            true
        }
    }

    fn locked() -> LockedModule {
        LockedModule {
            path: ModulePath::parse("github.com/acme/lib").unwrap(),
            version: ExactVersion::parse("1.2.3").unwrap(),
            origin: crate::schema::lockfile::LockOrigin::Registry,
            release: Some(Digest::from_sha256(b"manifest")),
            intent: None,
        }
    }

    #[test]
    fn cache_owned_entries_use_portable_full_case_folding() {
        assert_eq!(
            cache_owned_root_entry("artifactſ/demo.wasm"),
            Some("artifacts")
        );
        assert_eq!(
            cache_owned_root_entry("vo.releaſe.json"),
            Some("vo.release.json"),
        );
        assert_eq!(cache_owned_root_entry("vo.tree.jſon"), Some("vo.tree.json"),);
        assert_eq!(cache_owned_root_entry("docs/vo.tree.jſon"), None);
    }

    #[test]
    fn unknown_entry_kinds_fail_closed() {
        let locked = locked();
        let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let unknown_path = module_dir.join("unknown.vo");
        let fs = UnknownKindFs {
            inner: MemoryFs::new().with_file(&unknown_path, "package unknown\n"),
            unknown_path,
        };
        let failure = scan(&fs, &module_dir, &locked, &[]).unwrap_err();
        assert_eq!(failure.field, InstalledModuleField::SourceFiles);
        assert!(failure.to_string().contains("Unknown"), "{failure}");
    }

    #[test]
    fn exact_spelling_checks_reject_oversized_directories() {
        let failure = require_exact_child_spelling(
            &OversizedReadDirFs,
            Path::new("cache/expected"),
            &locked(),
            InstalledModuleField::Directory,
        )
        .unwrap_err();
        assert!(failure.to_string().contains("more than"), "{failure}");
    }

    #[test]
    fn enumeration_order_cannot_change_results_or_first_failure() {
        let locked = locked();
        let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let ordered = MemoryFs::new()
            .with_file(module_dir.join("z/z.vo"), "package z\n")
            .with_file(module_dir.join("a/a.vo"), "package a\n");
        let reversed = ReverseReadDirFs(ordered.clone());
        assert_eq!(
            scan(&ordered, &module_dir, &locked, &[]).unwrap(),
            scan(&reversed, &module_dir, &locked, &[]).unwrap(),
        );

        let colliding = MemoryFs::new()
            .with_file(module_dir.join("Source/a.vo"), "package source\n")
            .with_file(module_dir.join("source/b.vo"), "package source\n");
        let reversed = ReverseReadDirFs(colliding.clone());
        assert_eq!(
            scan(&colliding, &module_dir, &locked, &[])
                .unwrap_err()
                .to_string(),
            scan(&reversed, &module_dir, &locked, &[])
                .unwrap_err()
                .to_string(),
        );
    }

    #[test]
    fn installed_source_scan_enforces_aggregate_complete_path_key_bytes() {
        let locked = locked();
        let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let mut fs = MemoryFs::new();
        fs.add_file(
            module_dir.join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        for branch in ["root-a", "root-b"] {
            fs.add_file(
                module_dir.join(format!("{branch}/component000000/file.vo")),
                "",
            );
        }

        let failure = scan_with_path_set(
            &fs,
            &module_dir,
            &locked,
            &[],
            PortablePathSet::with_max_path_key_bytes(100),
        )
        .unwrap_err();
        assert!(failure.to_string().contains("path-key limit"), "{failure}");
    }

    #[test]
    fn empty_directories_are_not_part_of_the_authenticated_source_tree() {
        let locked = locked();
        let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let mut fs = MemoryFs::new();
        fs.add_file(
            module_dir.join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        fs.add_dir(module_dir.join("empty/nested"));

        let failure = scan(&fs, &module_dir, &locked, &[]).unwrap_err();

        assert_eq!(failure.field, InstalledModuleField::SourceFiles);
        assert!(failure.to_string().contains("empty directory"), "{failure}");
        assert!(failure.to_string().contains("empty"), "{failure}");
    }
}
