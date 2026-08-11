//! Cache directory layout helpers (platform-agnostic).
//!
//! Layout computation for the exact module versions selected by a lock file.

use std::path::{Path, PathBuf};

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

#[cfg(test)]
mod tests {
    use super::*;

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
