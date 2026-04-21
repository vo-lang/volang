//! Cache directory layout helpers (platform-agnostic).
//!
//! Layout computation and installed-version discovery for the module cache.
//! All helpers are generic over the `FileSystem` trait so they work
//! identically on native `RealFs`, in-memory `MemoryFs`, and browser `WasmVfs`.

use std::path::{Path, PathBuf};

use vo_common::vfs::FileSystem;

use crate::identity::ModulePath;
use crate::version::ExactVersion;

/// Metadata file name for the cached source digest marker.
pub const SOURCE_DIGEST_MARKER: &str = ".vo-source-digest";

/// Metadata file name for the cached version marker.
pub const VERSION_MARKER: &str = ".vo-version";

/// Encode a module path as a flat directory name.
///
/// Replaces `/` with `@` (forbidden in module-path segments) so that
/// different module paths never collide in a flat directory listing.
///
/// Example: `github.com/acme/lib` → `github.com@acme@lib`
pub fn cache_key(module: &str) -> String {
    module.replace('/', "@")
}

/// Relative cache directory path for a module version (no root prefix).
///
/// Returns `"<encoded_module>/<version>"`, suitable for joining with an
/// OS cache root or prepending with `/` for a VFS root.
pub fn relative_module_dir(module: &str, version: impl std::fmt::Display) -> PathBuf {
    PathBuf::from(cache_key(module)).join(version.to_string())
}

/// Cache directory layout helper.
/// Cache key: `<cache_root>/<module_path_encoded>/<version>/`
///
/// The module path is encoded by replacing `/` with `@` (which is forbidden
/// in module-path segments) so that different paths never collide.
/// For example `github.com/acme/lib` → `github.com@acme@lib`.
pub fn cache_dir(cache_root: &Path, module: &ModulePath, version: &ExactVersion) -> PathBuf {
    cache_root.join(relative_module_dir(module.as_str(), version))
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
    Some((module_path, version))
}

/// Discover the unique installed version for a module under `cache_root`.
///
/// Scans `<cache_root>/<cache_key(module)>/` for version directories (names
/// starting with `v`) that contain a valid `.vo-version` marker file.
///
/// Returns:
/// - `Ok(Some(version))` if exactly one installed version is found.
/// - `Ok(None)` if no installed version is found.
/// - `Err(...)` if multiple installed versions are found (ambiguous state).
pub fn discover_installed_version<F: FileSystem>(
    fs: &F,
    cache_root: &Path,
    module: &str,
) -> Result<Option<String>, String> {
    let module_dir = cache_root.join(cache_key(module));
    let entries = match fs.read_dir(&module_dir) {
        Ok(entries) => entries,
        Err(_) => return Ok(None),
    };
    let mut found: Option<String> = None;
    for entry in entries {
        let name = match entry.file_name() {
            Some(n) => n.to_string_lossy().to_string(),
            None => continue,
        };
        if !name.starts_with('v') || !fs.is_dir(&entry) {
            continue;
        }
        let marker = entry.join(VERSION_MARKER);
        if let Ok(content) = fs.read_file(&marker) {
            let version = content.trim().to_string();
            if version.is_empty() {
                continue;
            }
            if let Some(ref existing) = found {
                return Err(format!(
                    "module {} has multiple installed versions in cache: {} and {}",
                    module, existing, version
                ));
            }
            found = Some(version);
        }
    }
    Ok(found)
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::vfs::MemoryFs;

    fn make_cache_fs(module: &str, versions: &[&str]) -> MemoryFs {
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
        let result = discover_installed_version(&fs, Path::new(""), "github.com/acme/lib").unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn discover_installed_version_single() {
        let fs = make_cache_fs("github.com/acme/lib", &["v1.2.0"]);
        let result = discover_installed_version(&fs, Path::new(""), "github.com/acme/lib").unwrap();
        assert_eq!(result, Some("v1.2.0".to_string()));
    }

    #[test]
    fn discover_installed_version_rejects_multiple() {
        let fs = make_cache_fs("github.com/acme/lib", &["v1.2.0", "v1.3.0"]);
        let result = discover_installed_version(&fs, Path::new(""), "github.com/acme/lib");
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
    fn discover_installed_version_ignores_non_version_dirs() {
        let mut fs = make_cache_fs("github.com/acme/lib", &["v1.0.0"]);
        let key = cache_key("github.com/acme/lib");
        fs.add_file(format!("{}/metadata/some_file", key), "data".to_string());
        let result = discover_installed_version(&fs, Path::new(""), "github.com/acme/lib").unwrap();
        assert_eq!(result, Some("v1.0.0".to_string()));
    }

    #[test]
    fn discover_installed_version_ignores_empty_marker() {
        let mut fs = MemoryFs::new();
        let marker = relative_module_dir("github.com/acme/lib", "v1.0.0").join(VERSION_MARKER);
        fs.add_file(marker, "  \n".to_string());
        let result = discover_installed_version(&fs, Path::new(""), "github.com/acme/lib").unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn module_identity_from_cache_dir_round_trips_canonical_layout() {
        let cache_root = Path::new("cache");
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("v1.2.3").unwrap();
        let module_dir = cache_dir(cache_root, &module, &version);

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
}
