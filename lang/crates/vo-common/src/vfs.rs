//! Virtual file system abstraction.

use std::collections::HashMap;
use std::io::{self};
#[cfg(feature = "zip")]
use std::io::{Read, Seek};
use std::path::{Path, PathBuf};

use vo_common_core::SourceProvider;

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

    /// Root path of the file system (if any).
    fn root(&self) -> Option<&Path> {
        None
    }
}

/// Real file system implementation.
/// 
/// All operations are relative to `root`. Both `FileSystem` and `SourceProvider`
/// traits use the same path resolution: `root.join(path)`.
#[derive(Debug, Clone)]
pub struct RealFs {
    /// Root directory for all path resolution.
    root: PathBuf,
}

impl RealFs {
    /// Create a new RealFs with a root directory.
    /// All paths will be resolved relative to this root.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }
    
    /// Get the root directory.
    pub fn root(&self) -> &Path {
        &self.root
    }
}

impl FileSystem for RealFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(self.root.join(path))
    }
    
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let full_path = self.root.join(path);
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(&full_path)? {
            // Return paths relative to root
            let entry_path = entry?.path();
            if let Ok(rel) = entry_path.strip_prefix(&self.root) {
                entries.push(rel.to_path_buf());
            } else {
                entries.push(entry_path);
            }
        }
        Ok(entries)
    }
    
    fn exists(&self, path: &Path) -> bool {
        self.root.join(path).exists()
    }
    
    fn is_dir(&self, path: &Path) -> bool {
        self.root.join(path).is_dir()
    }

    fn root(&self) -> Option<&Path> {
        Some(&self.root)
    }
}

impl SourceProvider for RealFs {
    fn read_source(&self, path: &str) -> Option<String> {
        std::fs::read_to_string(self.root.join(path)).ok()
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
    
    pub fn with_files(mut self, files: impl IntoIterator<Item = (impl Into<PathBuf>, impl Into<String>)>) -> Self {
        for (path, content) in files {
            self.add_file(path, content);
        }
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
        let is_root = path == Path::new(".") || path == Path::new("") || path.as_os_str().is_empty();
        
        for file_path in self.files.keys() {
            if is_root {
                // Root directory - get top-level entries
                if let Some(first_component) = file_path.components().next() {
                    let entry = PathBuf::from(first_component.as_os_str());
                    if !entries.contains(&entry) {
                        entries.push(entry);
                    }
                }
            } else {
                // Check if file is directly in this directory
                if let Some(parent) = file_path.parent() {
                    if parent == path {
                        entries.push(file_path.clone());
                    }
                }
                // Check for subdirectories
                let path_str = format!("{}/", path.to_string_lossy());
                let file_str = file_path.to_string_lossy();
                if file_str.starts_with(&path_str) {
                    let rest = &file_str[path_str.len()..];
                    if !rest.is_empty() {
                        let entry = if let Some(idx) = rest.find('/') {
                            path.join(&rest[..idx])
                        } else {
                            path.join(rest)
                        };
                        if !entries.contains(&entry) {
                            entries.push(entry);
                        }
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
        if path == Path::new(".") || path == Path::new("") {
            return !self.files.is_empty();
        }
        let path_str = path.to_string_lossy();
        self.files.keys().any(|p| {
            let p_str = p.to_string_lossy();
            p_str.starts_with(&*path_str) && 
            p_str.len() > path_str.len() &&
            p_str.chars().nth(path_str.len()) == Some('/')
        })
    }
    
    fn is_dir(&self, path: &Path) -> bool {
        !self.files.contains_key(path) && self.exists(path)
    }

    fn root(&self) -> Option<&Path> {
        None
    }
}

impl SourceProvider for MemoryFs {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

/// Zip file system implementation.
/// 
/// All files are loaded into memory on creation. Paths are relative to `root`.
/// Both `FileSystem` and `SourceProvider` traits use the same path resolution.
#[cfg(feature = "zip")]
#[derive(Debug, Clone)]
pub struct ZipFs {
    /// All files in the zip: path relative to zip root -> content
    files: HashMap<PathBuf, String>,
    /// Root directory within the zip (empty = zip root)
    root: PathBuf,
}

#[cfg(feature = "zip")]
impl ZipFs {
    /// Create a ZipFs from a zip file path, using zip root as project root.
    pub fn from_path(path: &Path) -> io::Result<Self> {
        let file = std::fs::File::open(path)?;
        Self::from_reader(file)
    }
    
    /// Create a ZipFs with a specific root directory within the zip.
    /// 
    /// Example: `ZipFs::from_path_with_root("project.zip", "src/")` 
    /// will treat `src/` as the project root.
    pub fn from_path_with_root(path: &Path, root: impl Into<PathBuf>) -> io::Result<Self> {
        let file = std::fs::File::open(path)?;
        Self::from_reader_with_root(file, root)
    }
    
    /// Create a ZipFs from any Read + Seek source (file, bytes, etc).
    pub fn from_reader<R: Read + Seek>(reader: R) -> io::Result<Self> {
        Self::from_reader_with_root(reader, PathBuf::new())
    }
    
    /// Create a ZipFs from reader with a specific root directory.
    pub fn from_reader_with_root<R: Read + Seek>(reader: R, root: impl Into<PathBuf>) -> io::Result<Self> {
        let mut archive = zip::ZipArchive::new(reader)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        
        let mut files = HashMap::new();
        
        for i in 0..archive.len() {
            let mut file = archive.by_index(i)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            
            if !file.is_dir() {
                let path = PathBuf::from(file.name());
                let mut content = String::new();
                if file.read_to_string(&mut content).is_ok() {
                    files.insert(path, content);
                }
            }
        }
        
        Ok(Self { files, root: root.into() })
    }
    
    /// Create a ZipFs from in-memory bytes.
    pub fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        Self::from_reader(std::io::Cursor::new(bytes))
    }
    
    /// Resolve a path relative to the root.
    fn resolve(&self, path: &Path) -> PathBuf {
        if self.root.as_os_str().is_empty() {
            path.to_path_buf()
        } else if path == Path::new(".") || path == Path::new("") {
            self.root.clone()
        } else {
            self.root.join(path)
        }
    }
}

#[cfg(feature = "zip")]
impl FileSystem for ZipFs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let full_path = self.resolve(path);
        self.files.get(&full_path)
            .cloned()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("file not found: {:?}", full_path)))
    }
    
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let full_path = self.resolve(path);
        let mut entries = Vec::new();
        
        // Check if we're at the root
        let is_root = full_path.as_os_str().is_empty() 
            || full_path == Path::new(".") 
            || full_path == Path::new("");
        
        // Build the prefix string for matching
        let prefix = if is_root {
            String::new()
        } else {
            let mut s = full_path.to_string_lossy().to_string();
            if !s.ends_with('/') {
                s.push('/');
            }
            s
        };
        
        for file_path in self.files.keys() {
            let file_str = file_path.to_string_lossy();
            
            if prefix.is_empty() {
                // Root directory - get top-level entries
                if let Some(first_component) = file_path.components().next() {
                    let entry = PathBuf::from(first_component.as_os_str());
                    if !entries.contains(&entry) {
                        entries.push(entry);
                    }
                }
            } else if file_str.starts_with(&prefix) {
                // Subdirectory - get entries relative to path (not full_path)
                let rest = &file_str[prefix.len()..];
                if !rest.is_empty() {
                    let entry = if let Some(idx) = rest.find('/') {
                        path.join(&rest[..idx])
                    } else {
                        path.join(rest)
                    };
                    if !entries.contains(&entry) {
                        entries.push(entry);
                    }
                }
            }
        }
        
        Ok(entries)
    }
    
    fn exists(&self, path: &Path) -> bool {
        let full_path = self.resolve(path);
        if self.files.contains_key(&full_path) {
            return true;
        }
        // Check if it's a directory (has files under it)
        let path_str = full_path.to_string_lossy();
        self.files.keys().any(|p| {
            let p_str = p.to_string_lossy();
            p_str.starts_with(&*path_str) && 
            (p_str.len() > path_str.len() && 
             p_str.chars().nth(path_str.len()) == Some('/'))
        })
    }
    
    fn is_dir(&self, path: &Path) -> bool {
        let full_path = self.resolve(path);
        !self.files.contains_key(&full_path) && self.exists(path)
    }

    fn root(&self) -> Option<&Path> {
        None
    }
}

#[cfg(feature = "zip")]
impl SourceProvider for ZipFs {
    fn read_source(&self, path: &str) -> Option<String> {
        let full_path = self.resolve(Path::new(path));
        self.files.get(&full_path).cloned()
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
    
    /// Collect .vo files from a directory (non-recursive).
    /// Subdirectories are treated as separate packages and loaded via import.
    /// 
    /// `fs` - FileSystem to read from (paths are relative to fs.root)
    /// `dir` - Directory to collect from (relative to fs.root, use "." for root)
    /// `abs_root` - Absolute root path for the FileSet (used for source map)
    pub fn collect<F: FileSystem>(fs: &F, dir: &Path, abs_root: PathBuf) -> io::Result<Self> {
        let mut file_set = Self::new(abs_root);
        
        // Only collect .vo files in the directory (not subdirectories)
        for entry in fs.read_dir(dir)? {
            if !fs.is_dir(&entry) && entry.extension().is_some_and(|e| e == "vo") {
                let content = fs.read_file(&entry)?;
                file_set.files.insert(entry, content);
            }
        }
        
        Ok(file_set)
    }
    
    /// Create a FileSet from a single source file.
    /// 
    /// `fs` - FileSystem to read from
    /// `file_path` - Path to the file (relative to fs.root)
    /// `abs_root` - Absolute root path for the FileSet
    pub fn from_file<F: FileSystem>(fs: &F, file_path: &Path, abs_root: PathBuf) -> io::Result<Self> {
        let mut file_set = Self::new(abs_root);
        let content = fs.read_file(file_path)?;
        file_set.files.insert(file_path.to_path_buf(), content);
        Ok(file_set)
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

/// Overlay file system that combines two file systems.
/// 
/// Tries the primary file system first, falls back to secondary if not found.
/// This is useful for combining a zip file (project files) with real fs (stdlib).
#[derive(Debug, Clone)]
pub struct OverlayFs<P: FileSystem, S: FileSystem> {
    /// Primary file system (checked first)
    primary: P,
    /// Secondary file system (fallback)
    secondary: S,
}

impl<P: FileSystem, S: FileSystem> OverlayFs<P, S> {
    /// Create an overlay file system with primary and secondary layers.
    pub fn new(primary: P, secondary: S) -> Self {
        Self { primary, secondary }
    }
}

impl<P: FileSystem, S: FileSystem> FileSystem for OverlayFs<P, S> {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        self.primary.read_file(path)
            .or_else(|_| self.secondary.read_file(path))
    }
    
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        // Merge entries from both file systems
        let mut entries = self.primary.read_dir(path).unwrap_or_default();
        if let Ok(secondary_entries) = self.secondary.read_dir(path) {
            for entry in secondary_entries {
                if !entries.contains(&entry) {
                    entries.push(entry);
                }
            }
        }
        if entries.is_empty() {
            Err(io::Error::new(io::ErrorKind::NotFound, "directory not found"))
        } else {
            Ok(entries)
        }
    }
    
    fn exists(&self, path: &Path) -> bool {
        self.primary.exists(path) || self.secondary.exists(path)
    }
    
    fn is_dir(&self, path: &Path) -> bool {
        self.primary.is_dir(path) || self.secondary.is_dir(path)
    }

    fn root(&self) -> Option<&Path> {
        self.primary.root().or_else(|| self.secondary.root())
    }
}

impl<P: FileSystem, S: FileSystem> SourceProvider for OverlayFs<P, S> {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
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
            .with_file("main.vo", "package main")
            .with_file("lib/util.vo", "package lib")
            .with_file("readme.md", "# Readme");
        
        let file_set = FileSet::collect(&fs, Path::new("."), PathBuf::from(".")).unwrap();
        
        // Should only collect .vo files in root (not subdirectories)
        assert_eq!(file_set.files.len(), 1);
        assert!(file_set.files.contains_key(Path::new("main.vo")));
        assert!(!file_set.files.contains_key(Path::new("lib/util.vo"))); // subdirectory
        assert!(!file_set.files.contains_key(Path::new("readme.md")));
    }
    
    #[test]
    #[cfg(feature = "zip")]
    fn test_zip_fs_read_dir() {
        // Create a simple in-memory zip for testing
        let mut zip_buffer = std::io::Cursor::new(Vec::new());
        {
            let mut zip = zip::ZipWriter::new(&mut zip_buffer);
            let options = zip::write::SimpleFileOptions::default();
            zip.start_file("main.vo", options).unwrap();
            std::io::Write::write_all(&mut zip, b"package main").unwrap();
            zip.start_file("math/math.vo", options).unwrap();
            std::io::Write::write_all(&mut zip, b"package math").unwrap();
            zip.finish().unwrap();
        }
        
        zip_buffer.set_position(0);
        let zip_fs = ZipFs::from_reader(zip_buffer).unwrap();
        
        // Test read_dir at root
        let entries = zip_fs.read_dir(Path::new(".")).unwrap();
        assert!(entries.contains(&PathBuf::from("main.vo")));
        assert!(entries.contains(&PathBuf::from("math")));
        
        // Test is_dir
        assert!(!zip_fs.is_dir(Path::new("main.vo")));
        assert!(zip_fs.is_dir(Path::new("math")));
        
        // Test read_file
        let content = zip_fs.read_file(Path::new("main.vo")).unwrap();
        assert_eq!(content, "package main");
    }
}
