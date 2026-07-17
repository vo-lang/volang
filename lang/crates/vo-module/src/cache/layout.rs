//! Cache directory layout helpers (platform-agnostic).
//!
//! Layout computation and installed-version discovery for the module cache.
//! All helpers are generic over the `FileSystem` trait so they work
//! identically on native `RealFs`, in-memory `MemoryFs`, and browser `WasmVfs`.

use std::path::{Path, PathBuf};

use vo_common::vfs::{
    normalize_fs_path, sort_fs_paths, FileSystem, FileSystemEntryKind, MAX_DIRECTORY_ENTRIES,
};

use crate::identity::ModulePath;
use crate::version::ExactVersion;

/// Metadata file name for the cached source digest marker.
pub const SOURCE_DIGEST_MARKER: &str = ".vo-source-digest";

/// Metadata file name for the cached version marker.
pub const VERSION_MARKER: &str = ".vo-version";

/// Cache-root-owned transaction workspace. Installers stage only beneath this
/// real directory so crashes cannot create fake module keys or versions.
pub const STAGING_DIR: &str = ".vo-staging";
pub const STAGING_LOCK_FILE: &str = ".lock";

/// Decode one cache marker written by the installer.
///
/// Marker files have exactly one non-empty line and always end in one LF.
/// Keeping this byte-level contract strict prevents cache acceptance from
/// depending on the host toolchain's Unicode whitespace tables.
pub(crate) fn canonical_marker_value(content: &str) -> Option<&str> {
    let value = content.strip_suffix('\n')?;
    (!value.is_empty() && !value.bytes().any(|byte| matches!(byte, b'\r' | b'\n'))).then_some(value)
}

/// Encode a module path as a flat directory name.
///
/// Replaces `/` with `@` (forbidden in module-path segments) so that
/// different module paths never collide in a flat directory listing.
///
/// Example: `github.com/acme/lib` → `github.com@acme@lib`
pub fn cache_key(module: &ModulePath) -> String {
    module.as_str().replace('/', "@")
}

/// Relative cache directory path for a module version (no root prefix).
///
/// Returns `"<encoded_module>/<version>"`, suitable for joining with an
/// OS cache root or prepending with `/` for a VFS root.
pub fn relative_module_dir(module: &ModulePath, version: &ExactVersion) -> PathBuf {
    PathBuf::from(cache_key(module)).join(version.to_string())
}

/// Cache directory layout helper.
/// Cache key: `<cache_root>/<module_path_encoded>/<version>/`
///
/// The module path is encoded by replacing `/` with `@` (which is forbidden
/// in module-path segments) so that different paths never collide.
/// For example `github.com/acme/lib` → `github.com@acme@lib`.
pub fn cache_dir(cache_root: &Path, module: &ModulePath, version: &ExactVersion) -> PathBuf {
    cache_root.join(relative_module_dir(module, version))
}

pub fn module_identity_from_cache_dir(
    cache_root: &Path,
    module_dir: &Path,
) -> Option<(ModulePath, ExactVersion)> {
    let rel = module_dir.strip_prefix(cache_root).ok()?;
    let components = rel
        .components()
        .map(|component| component.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()?;
    if components.len() != 2 {
        return None;
    }
    let module_path = ModulePath::parse(&components[0].replace('@', "/")).ok()?;
    let version = ExactVersion::parse(components[1]).ok()?;
    let canonical = relative_module_dir(&module_path, &version);
    if rel.as_os_str() != canonical.as_os_str() {
        return None;
    }
    Some((module_path, version))
}

/// Discover the unique installed version for a module under `cache_root`.
///
/// Scans `<cache_root>/<cache_key(module)>/` and accepts only canonical exact
/// version directories whose `.vo-version` marker parses to the same version.
/// Unexpected files, invalid directory names, missing or invalid markers, and
/// directory/marker mismatches are treated as corrupt cache state.
///
/// Returns:
/// - `Ok(Some(version))` if exactly one installed version is found.
/// - `Ok(None)` if no installed version is found.
/// - `Err(...)` if the cache state is corrupt or contains multiple versions.
pub fn discover_installed_version<F: FileSystem>(
    fs: &F,
    cache_root: &Path,
    module: &ModulePath,
) -> Result<Option<ExactVersion>, String> {
    let module_key = cache_key(module);
    match fs.entry_kind(cache_root).map_err(|error| {
        format!(
            "failed to inspect module cache root {} without following symbolic links: {error}",
            cache_root.display(),
        )
    })? {
        FileSystemEntryKind::Missing => return Ok(None),
        FileSystemEntryKind::Directory => {}
        other => {
            return Err(format!(
                "module cache root {} must be a directory without symbolic links; found {other:?}",
                cache_root.display(),
            ));
        }
    }
    if !exact_child_exists(fs, cache_root, &module_key)? {
        return Ok(None);
    }
    let module_dir = cache_root.join(&module_key);
    match fs.entry_kind(&module_dir).map_err(|error| {
        format!(
            "failed to inspect module cache directory {} for {} without following symbolic links: {}",
            module_dir.display(),
            module,
            error,
        )
    })? {
        FileSystemEntryKind::Missing => return Ok(None),
        FileSystemEntryKind::Directory => {}
        other => {
            return Err(format!(
                "module cache path {} for {} must be a directory without symbolic links; found {other:?}",
                module_dir.display(),
                module,
            ));
        }
    }
    let mut entries = fs.read_dir(&module_dir).map_err(|error| {
        format!(
            "failed to read module cache directory {} for {}: {}",
            module_dir.display(),
            module,
            error,
        )
    })?;
    if entries.len() > MAX_DIRECTORY_ENTRIES {
        return Err(format!(
            "module {} cache contains more than {MAX_DIRECTORY_ENTRIES} version entries",
            module,
        ));
    }
    sort_fs_paths(&mut entries);
    let mut found: Option<ExactVersion> = None;
    for entry in entries {
        if normalize_fs_path(&entry) != entry || entry.parent() != Some(module_dir.as_path()) {
            return Err(format!(
                "module {} cache returned non-canonical or non-child entry {} for {}",
                module,
                entry.display(),
                module_dir.display(),
            ));
        }
        let name = entry
            .file_name()
            .and_then(|name| name.to_str())
            .ok_or_else(|| {
                format!(
                    "module {} cache contains a non-UTF-8 entry: {}",
                    module,
                    entry.display(),
                )
            })?;
        match fs.entry_kind(&entry).map_err(|error| {
            format!(
                "failed to inspect module {} cache entry {} without following symbolic links: {}",
                module,
                entry.display(),
                error,
            )
        })? {
            FileSystemEntryKind::Directory => {}
            other => {
                return Err(format!(
                    "module {} cache contains an unexpected non-directory entry {} ({other:?})",
                    module,
                    entry.display(),
                ));
            }
        }

        let directory_version = ExactVersion::parse(name).map_err(|error| {
            format!(
                "module {} cache contains invalid version directory {}: {}",
                module,
                entry.display(),
                error,
            )
        })?;
        let canonical_entry = module_dir.join(directory_version.to_string());
        if entry != canonical_entry {
            return Err(format!(
                "module {} cache version directory must be {}, found {}",
                module,
                canonical_entry.display(),
                entry.display(),
            ));
        }
        let marker = entry.join(VERSION_MARKER);
        if !exact_child_exists(fs, &entry, VERSION_MARKER)? {
            return Err(format!(
                "module {} cache version directory {} has no exact {} marker",
                module,
                entry.display(),
                VERSION_MARKER,
            ));
        }
        match fs.entry_kind(&marker).map_err(|error| {
            format!(
                "failed to inspect module {} cache marker {} without following symbolic links: {}",
                module,
                marker.display(),
                error,
            )
        })? {
            FileSystemEntryKind::RegularFile => {}
            FileSystemEntryKind::Missing => {
                return Err(format!(
                    "module {} cache version directory {} has no readable {} marker",
                    module,
                    entry.display(),
                    VERSION_MARKER,
                ));
            }
            other => {
                return Err(format!(
                    "module {} cache marker {} must be a regular file without symbolic links; found {other:?}",
                    module,
                    marker.display(),
                ));
            }
        }
        let content = fs.read_text_limited(&marker, 1024).map_err(|error| {
            format!(
                "module {} cache version directory {} has no readable {} marker: {}",
                module,
                entry.display(),
                VERSION_MARKER,
                error,
            )
        })?;
        let marker_value = canonical_marker_value(&content).ok_or_else(|| {
            format!(
                "module {} cache marker {} must contain exactly one non-empty LF-terminated line",
                module,
                marker.display(),
            )
        })?;
        let marker_version = ExactVersion::parse(marker_value).map_err(|error| {
            format!(
                "module {} cache marker {} contains an invalid exact version: {}",
                module,
                marker.display(),
                error,
            )
        })?;
        if marker_version != directory_version {
            return Err(format!(
                "module {} cache version mismatch: directory {} contains marker {}",
                module, directory_version, marker_version,
            ));
        }
        if let Some(ref existing) = found {
            return Err(format!(
                "module {} has multiple installed versions in cache: {} and {}",
                module, existing, directory_version,
            ));
        }
        found = Some(directory_version);
    }
    Ok(found)
}

fn exact_child_exists<F: FileSystem>(
    fs: &F,
    parent: &Path,
    expected: &str,
) -> Result<bool, String> {
    let expected_key = crate::schema::portable_case_key(expected);
    let mut exact = false;
    let entries = fs.read_dir(parent).map_err(|error| {
        format!(
            "failed to enumerate {} while verifying exact cache spelling: {error}",
            parent.display(),
        )
    })?;
    if entries.len() > MAX_DIRECTORY_ENTRIES {
        return Err(format!(
            "cache directory {} contains more than {MAX_DIRECTORY_ENTRIES} entries",
            parent.display(),
        ));
    }
    for entry in entries {
        let Some(name) = entry.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if name == expected {
            exact = true;
        } else if crate::schema::portable_case_key(name) == expected_key {
            return Err(format!(
                "cache entry {} conflicts with required portable spelling {:?}",
                entry.display(),
                expected,
            ));
        }
    }
    Ok(exact)
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::vfs::{FileSystemEntryKind, MemoryFs};

    struct ReverseReadDirFs(MemoryFs);

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

    fn module() -> ModulePath {
        ModulePath::parse("github.com/acme/lib").unwrap()
    }

    fn exact_version(value: &str) -> ExactVersion {
        ExactVersion::parse(value).unwrap()
    }

    fn make_cache_fs(module: &ModulePath, versions: &[ExactVersion]) -> MemoryFs {
        let mut fs = MemoryFs::new();
        for version in versions {
            let marker_path = relative_module_dir(module, version).join(VERSION_MARKER);
            fs.add_file(marker_path, format!("{}\n", version));
        }
        fs
    }

    #[test]
    fn discover_installed_version_none() {
        let fs = MemoryFs::new();
        let result = discover_installed_version(&fs, Path::new(""), &module()).unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn discover_installed_version_single() {
        let module = module();
        let version = exact_version("1.2.0");
        let fs = make_cache_fs(&module, std::slice::from_ref(&version));
        let result = discover_installed_version(&fs, Path::new(""), &module).unwrap();
        assert_eq!(result, Some(version));
    }

    #[test]
    fn discover_installed_version_rejects_multiple() {
        let module = module();
        let fs = make_cache_fs(&module, &[exact_version("1.2.0"), exact_version("1.3.0")]);
        let result = discover_installed_version(&fs, Path::new(""), &module);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.contains("multiple installed versions"),
            "error was: {}",
            err
        );
        assert!(err.contains("github.com/acme/lib"), "error was: {}", err);
    }

    #[test]
    fn discover_installed_version_rejects_invalid_directory_names() {
        let module = module();
        let mut fs = MemoryFs::new();
        fs.add_dir(PathBuf::from(cache_key(&module)).join("latest"));

        let error = discover_installed_version(&fs, Path::new(""), &module).unwrap_err();

        assert!(error.contains("invalid version directory"), "{error}");
    }

    #[test]
    fn discover_installed_version_rejects_invalid_marker_versions() {
        let module = module();
        let version = exact_version("1.0.0");
        let mut fs = MemoryFs::new();
        let marker = relative_module_dir(&module, &version).join(VERSION_MARKER);
        fs.add_file(marker, "latest\n".to_string());

        let error = discover_installed_version(&fs, Path::new(""), &module).unwrap_err();

        assert!(error.contains("invalid exact version"), "{error}");
    }

    #[test]
    fn discover_installed_version_rejects_non_canonical_marker_encoding() {
        let module = module();
        let version = exact_version("1.0.0");
        for (case, content) in [
            ("missing newline", "1.0.0"),
            ("CRLF", "1.0.0\r\n"),
            ("extra newline", "1.0.0\n\n"),
            ("Unicode padding", "\u{00a0}1.0.0\u{00a0}\n"),
        ] {
            let mut fs = MemoryFs::new();
            let marker = relative_module_dir(&module, &version).join(VERSION_MARKER);
            fs.add_file(marker, content.to_string());

            let error = discover_installed_version(&fs, Path::new(""), &module).unwrap_err();

            assert!(
                error.contains("LF-terminated line") || error.contains("invalid exact version"),
                "{case}: {error}"
            );
        }
    }

    #[test]
    fn discover_installed_version_rejects_directory_marker_mismatches() {
        let module = module();
        let directory_version = exact_version("1.0.0");
        let marker_version = exact_version("1.0.1");
        let mut fs = MemoryFs::new();
        let marker = relative_module_dir(&module, &directory_version).join(VERSION_MARKER);
        fs.add_file(marker, format!("{marker_version}\n"));

        let error = discover_installed_version(&fs, Path::new(""), &module).unwrap_err();

        assert!(error.contains("version mismatch"), "{error}");
        assert!(error.contains("1.0.0"), "{error}");
        assert!(error.contains("1.0.1"), "{error}");
    }

    #[test]
    fn discover_installed_version_rejects_phantom_module_markers() {
        let module = module();
        let mut fs = MemoryFs::new();
        let marker = PathBuf::from(cache_key(&module)).join(VERSION_MARKER);
        fs.add_file(marker, "1.0.0\n".to_string());

        let error = discover_installed_version(&fs, Path::new(""), &module).unwrap_err();

        assert!(error.contains("unexpected non-directory entry"), "{error}");
        assert!(error.contains(VERSION_MARKER), "{error}");
    }

    #[test]
    fn discover_installed_version_rejects_version_directories_without_markers() {
        let module = module();
        let version = exact_version("1.0.0");
        let mut fs = MemoryFs::new();
        fs.add_dir(relative_module_dir(&module, &version));

        let error = discover_installed_version(&fs, Path::new(""), &module).unwrap_err();

        assert!(error.contains("has no exact .vo-version marker"), "{error}");
    }

    #[test]
    fn discover_installed_version_is_independent_of_enumeration_order() {
        let module = module();
        let mut fs = MemoryFs::new();
        fs.add_dir(PathBuf::from(cache_key(&module)).join("z-invalid"));
        fs.add_dir(PathBuf::from(cache_key(&module)).join("a-invalid"));
        let reversed = ReverseReadDirFs(fs.clone());

        let ordered_error = discover_installed_version(&fs, Path::new(""), &module).unwrap_err();
        let reversed_error =
            discover_installed_version(&reversed, Path::new(""), &module).unwrap_err();

        assert_eq!(ordered_error, reversed_error);
        assert!(ordered_error.contains("a-invalid"), "{ordered_error}");
    }

    #[cfg(unix)]
    #[test]
    fn discover_installed_version_rejects_symlinked_cache_chain() {
        use std::os::unix::fs::symlink;
        use vo_common::vfs::RealFs;

        let module = module();
        let version = exact_version("1.2.3");

        let root = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        symlink(outside.path(), root.path().join(cache_key(&module))).unwrap();
        let error = discover_installed_version(&RealFs::new(root.path()), Path::new(""), &module)
            .unwrap_err();
        assert!(error.contains("Symlink"), "{error}");

        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir(root.path().join(cache_key(&module))).unwrap();
        symlink(
            outside.path(),
            root.path()
                .join(cache_key(&module))
                .join(version.to_string()),
        )
        .unwrap();
        let error = discover_installed_version(&RealFs::new(root.path()), Path::new(""), &module)
            .unwrap_err();
        assert!(error.contains("Symlink"), "{error}");

        let root = tempfile::tempdir().unwrap();
        let version_dir = root
            .path()
            .join(cache_key(&module))
            .join(version.to_string());
        std::fs::create_dir_all(&version_dir).unwrap();
        let marker_target = outside.path().join("marker");
        std::fs::write(&marker_target, format!("{version}\n")).unwrap();
        symlink(&marker_target, version_dir.join(VERSION_MARKER)).unwrap();
        let error = discover_installed_version(&RealFs::new(root.path()), Path::new(""), &module)
            .unwrap_err();
        assert!(error.contains("Symlink"), "{error}");
    }

    #[test]
    fn module_identity_from_cache_dir_round_trips_canonical_layout() {
        let cache_root = Path::new("cache");
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.2.3").unwrap();
        let module_dir = cache_dir(cache_root, &module, &version);

        assert_eq!(cache_key(&module), "github.com@acme@lib");
        assert_eq!(
            relative_module_dir(&module, &version),
            Path::new("github.com@acme@lib/1.2.3")
        );

        let resolved = module_identity_from_cache_dir(cache_root, &module_dir).unwrap();

        assert_eq!(resolved.0, module);
        assert_eq!(resolved.1, version);
    }

    #[test]
    fn module_identity_from_cache_dir_rejects_non_canonical_layout() {
        let cache_root = Path::new("cache");
        let invalid_dir = cache_root.join("github.com@acme@lib").join("not-a-version");

        let resolved = module_identity_from_cache_dir(cache_root, &invalid_dir);

        assert!(resolved.is_none());
    }

    #[test]
    fn module_identity_from_cache_dir_rejects_non_canonical_path_spelling() {
        let separator = std::path::MAIN_SEPARATOR;
        let cache_root = Path::new("cache");
        let non_canonical_dir = PathBuf::from(format!(
            "cache{separator}github.com@acme@lib{separator}{separator}1.2.3"
        ));

        let resolved = module_identity_from_cache_dir(cache_root, &non_canonical_dir);

        assert!(resolved.is_none());
    }
}
