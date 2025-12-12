//! Source file management for the compiler.
//!
//! Provides a centralized registry of source files with:
//! - File ID assignment for cross-referencing
//! - Source text access
//! - Integration with codespan-reporting

use codespan_reporting::files::SimpleFiles;
use std::path::Path;

/// A unique identifier for a source file.
pub type FileId = usize;

/// Manages source files loaded by the compiler.
pub struct SourceManager {
    files: SimpleFiles<String, String>,
}

impl SourceManager {
    /// Create an empty source manager.
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
        }
    }

    /// Add a file with the given name and source content.
    /// Returns a unique FileId for referencing this file.
    pub fn add_file(&mut self, name: impl Into<String>, source: impl Into<String>) -> FileId {
        self.files.add(name.into(), source.into())
    }

    /// Add a file from the filesystem.
    /// Returns the FileId and any IO error that occurred.
    pub fn add_file_from_path(&mut self, path: &Path) -> std::io::Result<FileId> {
        let name = path.display().to_string();
        let source = std::fs::read_to_string(path)?;
        Ok(self.add_file(name, source))
    }

    /// Get the source text of a file by ID.
    pub fn get_source(&self, id: FileId) -> Option<&str> {
        self.files.get(id).ok().map(|f| f.source().as_str())
    }

    /// Get the file name by ID.
    pub fn get_name(&self, id: FileId) -> Option<&str> {
        self.files.get(id).ok().map(|f| f.name().as_str())
    }

    /// Get the line number (1-indexed) for a byte offset.
    pub fn get_line(&self, id: FileId, byte_offset: usize) -> Option<usize> {
        use codespan_reporting::files::Files;
        self.files
            .line_index(id, byte_offset)
            .ok()
            .map(|idx| idx + 1)
    }

    /// Get the column number (1-indexed) for a byte offset.
    pub fn get_column(&self, id: FileId, byte_offset: usize) -> Option<usize> {
        use codespan_reporting::files::Files;
        let line_idx = self.files.line_index(id, byte_offset).ok()?;
        let line_range = self.files.line_range(id, line_idx).ok()?;
        Some(byte_offset - line_range.start + 1)
    }

    /// Get line and column as a tuple (1-indexed).
    pub fn get_location(&self, id: FileId, byte_offset: usize) -> Option<(usize, usize)> {
        let line = self.get_line(id, byte_offset)?;
        let col = self.get_column(id, byte_offset)?;
        Some((line, col))
    }

    /// Format a location string like "file.gox:10:5".
    pub fn format_location(&self, id: FileId, byte_offset: usize) -> String {
        let name = self.get_name(id).unwrap_or("<unknown>");
        match self.get_location(id, byte_offset) {
            Some((line, col)) => format!("{}:{}:{}", name, line, col),
            None => name.to_string(),
        }
    }

    /// Access the underlying SimpleFiles for codespan-reporting.
    pub fn files(&self) -> &SimpleFiles<String, String> {
        &self.files
    }
}

impl Default for SourceManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_manager_location() {
        let mut sm = SourceManager::new();
        // Lines: "func main() {\n" (14 chars) + "    x := 1\n" (11 chars)
        let id = sm.add_file("test.gox", "func main() {\n    x := 1\n}");

        // 'x' is at offset 18 (after "func main() {\n    ")
        assert_eq!(sm.get_line(id, 18), Some(2));
        assert_eq!(sm.get_column(id, 18), Some(5));
        assert_eq!(sm.format_location(id, 18), "test.gox:2:5");
    }
}
