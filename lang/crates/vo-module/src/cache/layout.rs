//! Cache directory layout helpers (platform-agnostic).
//!
//! These are pure functions that compute cache paths and keys without
//! performing any I/O.  They are used by `materialize` (native download),
//! `vo-engine` (compile-time validation), and `vo-web` (VFS caching).

use std::path::{Path, PathBuf};

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
