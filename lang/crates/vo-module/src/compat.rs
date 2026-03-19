//! Convenience helpers for callers that work with raw `&str` module/import paths
//! and need quick validation or field extraction without constructing typed objects.
//!
//! Consumers that already have `ModulePath`, `ImportClass`, etc. should use those
//! types directly rather than going through these wrappers.

use crate::identity::{self as core, ModulePath};
use crate::registry::RepositoryId;

/// Validate an import path per spec §4.
/// Returns Ok(()) on valid path, Err(String) with a human-readable message on invalid.
pub fn validate_import_path(path: &str) -> Result<(), String> {
    core::classify_import(path)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

/// Validate internal package access per spec §9.4.
/// Returns Ok(()) if `importer` is allowed to import `target`, Err(String) otherwise.
pub fn validate_internal_access(importer: &str, target: &str) -> Result<(), String> {
    if core::check_internal_visibility(importer, target) {
        Ok(())
    } else {
        Err(format!(
            "use of internal package not allowed: {} cannot import {}",
            importer, target
        ))
    }
}

/// Check if a string is a valid canonical module path.
pub fn is_valid_module_path(path: &str) -> bool {
    ModulePath::parse(path).is_ok()
}

/// Extract repository identity from a canonical module path string.
/// Returns None if the path is not a valid module path.
pub fn module_repository(path: &str) -> Option<RepositoryId> {
    let mp = ModulePath::parse(path).ok()?;
    Some(RepositoryId {
        owner: mp.owner().to_string(),
        repo: mp.repo().to_string(),
    })
}

/// Extract the module root directory relative to the repository root.
/// Returns None if the path is not a valid module path.
pub fn module_root(path: &str) -> Option<String> {
    let mp = ModulePath::parse(path).ok()?;
    Some(mp.module_root().to_string())
}
