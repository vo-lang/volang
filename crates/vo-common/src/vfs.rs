//! Virtual file system abstraction.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

/// A virtual file system trait for abstracting file operations.
pub trait FileSystem: Send + Sync {
    /// Read file contents as a string.
    fn read_file(&self, path: &Path) -> io::Result<String>;
    
    /// List entries in a directory.
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;
    
    /// Check if a path exists.
    fn exists(&self, path: &Path) -> bool;
    
    /// Check if a path is a directory.
    fn is_dir(&self, path: &Path) -> bool;
}

/// Real file system implementation.
#[derive(Debug, Clone, Copy, Default)]
pub struct RealFs;

impl FileSystem for RealFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }
    
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(path)? {
            entries.push(entry?.path());
        }
        Ok(entries)
    }
    
    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }
    
    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }
}

/// In-memory file system for testing.
#[derive(Debug, Clone, Default)]
pub struct MemoryFs {
    files: HashMap<PathBuf, String>,
}

impl MemoryFs {
    pub fn new() -> Self {
        Self { files: HashMap::new() }
    }
    
    pub fn add_file(&mut self, path: impl Into<PathBuf>, content: impl Into<String>) {
        self.files.insert(path.into(), content.into());
    }
    
    pub fn with_file(mut self, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        self.add_file(path, content);
        self
    }
}

impl FileSystem for MemoryFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        self.files.get(path)
            .cloned()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("file not found: {:?}", path)))
    }
    
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();
        let path_str = path.to_string_lossy();
        
        for file_path in self.files.keys() {
            if let Some(parent) = file_path.parent() {
                if parent == path {
                    entries.push(file_path.clone());
                }
            }
            // Also check for subdirectories
            let file_str = file_path.to_string_lossy();
            if file_str.starts_with(&*path_str) {
                let rest = &file_str[path_str.len()..];
                let rest = rest.trim_start_matches('/');
                if let Some(idx) = rest.find('/') {
                    let subdir = path.join(&rest[..idx]);
                    if !entries.contains(&subdir) {
                        entries.push(subdir);
                    }
                }
            }
        }
        
        Ok(entries)
    }
    
    fn exists(&self, path: &Path) -> bool {
        if self.files.contains_key(path) {
            return true;
        }
        // Check if it's a directory (has files under it)
        let path_str = path.to_string_lossy();
        self.files.keys().any(|p| {
            let p_str = p.to_string_lossy();
            p_str.starts_with(&*path_str) && p_str.len() > path_str.len()
        })
    }
    
    fn is_dir(&self, path: &Path) -> bool {
        !self.files.contains_key(path) && self.exists(path)
    }
}

/// A collection of source files for a project.
#[derive(Debug, Clone)]
pub struct FileSet {
    /// File path -> file content
    pub files: HashMap<PathBuf, String>,
    /// Project root directory
    pub root: PathBuf,
}

impl FileSet {
    pub fn new(root: PathBuf) -> Self {
        Self {
            files: HashMap::new(),
            root,
        }
    }
    
    /// Collect all .vo files from a directory recursively.
    pub fn collect<F: FileSystem>(fs: &F, root: &Path) -> io::Result<Self> {
        let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
        let mut file_set = Self::new(root.clone());
        file_set.collect_dir(fs, &root)?;
        Ok(file_set)
    }
    
    /// Create a FileSet from a single source file.
    /// The root is set to the file's parent directory.
    pub fn from_file<F: FileSystem>(fs: &F, file_path: &Path) -> io::Result<Self> {
        let file_path = file_path.canonicalize().unwrap_or_else(|_| file_path.to_path_buf());
        let root = file_path.parent().unwrap_or(Path::new(".")).to_path_buf();
        let mut file_set = Self::new(root);
        let content = fs.read_file(&file_path)?;
        file_set.files.insert(file_path, content);
        Ok(file_set)
    }
    
    fn collect_dir<F: FileSystem>(&mut self, fs: &F, dir: &Path) -> io::Result<()> {
        for entry in fs.read_dir(dir)? {
            if fs.is_dir(&entry) {
                self.collect_dir(fs, &entry)?;
            } else if entry.extension().map_or(false, |e| e == "vo") {
                let content = fs.read_file(&entry)?;
                self.files.insert(entry, content);
            }
        }
        Ok(())
    }
    
    /// Get files grouped by their parent directory (package).
    pub fn files_by_package(&self) -> HashMap<PathBuf, Vec<(&PathBuf, &String)>> {
        let mut packages: HashMap<PathBuf, Vec<(&PathBuf, &String)>> = HashMap::new();
        
        for (path, content) in &self.files {
            let pkg_dir = path.parent().unwrap_or(Path::new("")).to_path_buf();
            packages.entry(pkg_dir).or_default().push((path, content));
        }
        
        packages
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_memory_fs() {
        let fs = MemoryFs::new()
            .with_file("/project/main.vo", "package main")
            .with_file("/project/lib/util.vo", "package lib");
        
        assert!(fs.exists(Path::new("/project/main.vo")));
        assert!(fs.exists(Path::new("/project/lib")));
        assert!(fs.is_dir(Path::new("/project/lib")));
        assert!(!fs.is_dir(Path::new("/project/main.vo")));
        
        let content = fs.read_file(Path::new("/project/main.vo")).unwrap();
        assert_eq!(content, "package main");
    }
    
    #[test]
    fn test_file_set_collect() {
        let fs = MemoryFs::new()
            .with_file("/project/main.vo", "package main")
            .with_file("/project/lib/util.vo", "package lib")
            .with_file("/project/lib/helper.vo", "package lib")
            .with_file("/project/readme.md", "# Readme");
        
        let file_set = FileSet::collect(&fs, Path::new("/project")).unwrap();
        
        // Should only collect .vo files
        assert_eq!(file_set.files.len(), 3);
        assert!(file_set.files.contains_key(Path::new("/project/main.vo")));
        assert!(file_set.files.contains_key(Path::new("/project/lib/util.vo")));
        assert!(!file_set.files.contains_key(Path::new("/project/readme.md")));
    }
}
