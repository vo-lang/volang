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

/// Import key identifying a package by path and source directory.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportKey {
    pub path: String,
    pub dir: String,
}

impl ImportKey {
    pub fn new(path: &str, dir: &str) -> Self {
        ImportKey {
            path: path.to_string(),
            dir: dir.to_string(),
        }
    }
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
