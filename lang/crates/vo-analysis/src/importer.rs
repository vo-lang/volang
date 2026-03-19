//! Package importer.
//!
//! This module provides the interface for importing packages during type checking.


use std::path::{Path, PathBuf};

use crate::objects::PackageKey;

/// Configuration for tracing/debugging.
#[derive(Debug, Default, Clone)]
pub struct TraceConfig {
    pub trace_parser: bool,
    pub trace_checker: bool,
}

/// Import key identifying a package by canonical import path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportKey {
    pub path: String,
}

impl ImportKey {
    pub fn new(path: &str) -> Self {
        ImportKey {
            path: path.to_string(),
        }
    }
}

pub fn validate_import_path<'a>(path: &'a str) -> Result<&'a str, String> {
    vo_module::compat::validate_import_path(path)?;
    Ok(path)
}

/// Result of an import operation.
#[derive(Debug)]
pub enum ImportResult {
    /// Successfully imported package.
    Ok(PackageKey),
    /// Import failed with error message.
    Err(String),
    /// Package is currently being imported (cycle).
    Cycle,
}

/// Trait for package importers.
pub trait Importer {
    /// Imports a package by key.
    fn import(&mut self, key: &ImportKey) -> ImportResult;
    
    /// Returns the working directory.
    fn working_dir(&self) -> &Path;
    
    /// Returns the base directory for imports.
    fn base_dir(&self) -> Option<&Path>;
}

/// A simple importer that always fails (for testing).
#[derive(Debug, Default)]
pub struct NullImporter {
    working_dir: PathBuf,
}

impl NullImporter {
    pub fn new(working_dir: PathBuf) -> Self {
        NullImporter { working_dir }
    }
}

impl Importer for NullImporter {
    fn import(&mut self, key: &ImportKey) -> ImportResult {
        ImportResult::Err(format!("cannot import \"{}\"", key.path))
    }
    
    fn working_dir(&self) -> &Path {
        &self.working_dir
    }
    
    fn base_dir(&self) -> Option<&Path> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::validate_import_path;

    #[test]
    fn test_validate_import_path_accepts_canonical_forms() {
        assert!(validate_import_path("fmt").is_ok());
        assert!(validate_import_path("encoding/json").is_ok());
        assert!(validate_import_path("github.com/vo-lang/vogui").is_ok());
        assert!(validate_import_path("github.com/vo-lang/vogui/app").is_ok());
    }

    #[test]
    fn test_validate_import_path_rejects_legacy_forms() {
        assert!(validate_import_path("./codec").is_err());
        assert!(validate_import_path("../shared").is_err());
        assert!(validate_import_path("std/io").is_err());
        assert!(validate_import_path("github.com/vo-lang/zip@v0.1.0").is_err());
        assert!(validate_import_path("example.com/acme/lib").is_err());
    }
}
