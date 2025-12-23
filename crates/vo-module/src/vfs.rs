//! Virtual File System for package resolution.
//!
//! Provides three separate VFS layers:
//! - StdVfs: Standard library packages
//! - LocalVfs: Local packages (relative paths)
//! - ModVfs: External module dependencies

use std::path::{Path, PathBuf};

use vo_common::vfs::{FileSystem, RealFs};

/// VFS configuration with three root paths.
#[derive(Debug, Clone)]
pub struct VfsConfig {
    /// Standard library root path
    pub std_root: PathBuf,
    /// Local packages root path (project directory)
    pub local_root: PathBuf,
    /// Module cache root path
    pub mod_root: PathBuf,
}

impl VfsConfig {
    /// Create a new VFS config with the given paths.
    pub fn new(std_root: PathBuf, local_root: PathBuf, mod_root: PathBuf) -> Self {
        Self { std_root, local_root, mod_root }
    }
    
    /// Create a config using environment variables and common defaults.
    pub fn from_env(project_dir: PathBuf) -> Self {
        let std_root = std::env::var("VO_STD")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                // Try common locations for stdlib
                // 1. Project dir's "stdlib" folder
                let in_project = project_dir.join("stdlib");
                if in_project.is_dir() {
                    return in_project;
                }
                // 2. Parent dir's "stdlib" (for examples/)
                if let Some(parent) = project_dir.parent() {
                    let in_parent = parent.join("stdlib");
                    if in_parent.is_dir() {
                        return in_parent;
                    }
                    // 3. Grandparent (for examples/subdir/)
                    if let Some(grandparent) = parent.parent() {
                        let in_grandparent = grandparent.join("stdlib");
                        if in_grandparent.is_dir() {
                            return in_grandparent;
                        }
                    }
                }
                // 4. Relative to executable
                if let Ok(exe) = std::env::current_exe() {
                    if let Some(exe_dir) = exe.parent() {
                        // Check ../stdlib (development) and ../lib/vo/stdlib (installed)
                        for rel in &["../../../stdlib", "../../stdlib", "../stdlib", "../lib/vo/stdlib"] {
                            let p = exe_dir.join(rel);
                            if p.is_dir() {
                                return p;
                            }
                        }
                    }
                }
                // Fallback
                project_dir.join("stdlib")
            });
        
        let mod_root = dirs::home_dir()
            .map(|h| h.join(".vo/mod"))
            .unwrap_or_else(|| project_dir.join(".vo/mod"));
        
        Self {
            std_root,
            local_root: project_dir,
            mod_root,
        }
    }
    
    /// Build a VFS from this config.
    pub fn to_vfs(&self) -> Vfs {
        Vfs::with_fs_roots(
            self.std_root.clone(),
            self.local_root.clone(),
            self.mod_root.clone(),
        )
    }
}

/// A resolved package from the VFS.
#[derive(Debug, Clone)]
pub struct VfsPackage {
    /// Package name (e.g., "fmt", "mylib")
    pub name: String,
    /// Package path (e.g., "fmt", "./mylib", "github.com/user/pkg")
    pub path: String,
    /// Source files in the package
    pub files: Vec<VfsFile>,
}

/// A source file from the VFS.
#[derive(Debug, Clone)]
pub struct VfsFile {
    /// File path relative to package
    pub path: PathBuf,
    /// File content
    pub content: String,
}

/// Package source trait for VFS implementations.
pub trait PackageSource: Send + Sync {
    /// Resolve a package by import path.
    fn resolve(&self, import_path: &str) -> Option<VfsPackage>;
    
    /// Check if this source can handle the given import path.
    fn can_handle(&self, import_path: &str) -> bool;
}

/// Standard library VFS.
pub struct StdVfs<F: FileSystem = RealFs> {
    root: PathBuf,
    fs: F,
}

impl StdVfs<RealFs> {
    pub fn new(root: PathBuf) -> Self {
        Self { root, fs: RealFs }
    }
}

impl<F: FileSystem> StdVfs<F> {
    pub fn with_fs(root: PathBuf, fs: F) -> Self {
        Self { root, fs }
    }
    
    fn load_package(&self, import_path: &str) -> Option<VfsPackage> {
        let pkg_dir = self.root.join(import_path);
        if !self.fs.is_dir(&pkg_dir) {
            return None;
        }
        
        let files = load_vo_files(&self.fs, &pkg_dir)?;
        let name = import_path.rsplit('/').next().unwrap_or(import_path).to_string();
        
        Some(VfsPackage {
            name,
            path: import_path.to_string(),
            files,
        })
    }
    
    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        self.load_package(import_path)
    }
    
    pub fn can_handle(&self, import_path: &str) -> bool {
        // Stdlib: no dots (domain names have dots)
        !import_path.contains('.')
    }
}

/// Local package VFS (relative paths).
pub struct LocalVfs<F: FileSystem = RealFs> {
    root: PathBuf,
    fs: F,
}

impl LocalVfs<RealFs> {
    pub fn new(root: PathBuf) -> Self {
        Self { root, fs: RealFs }
    }
}

impl<F: FileSystem> LocalVfs<F> {
    pub fn with_fs(root: PathBuf, fs: F) -> Self {
        Self { root, fs }
    }
    
    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        let rel_path = import_path.trim_start_matches("./");
        let pkg_dir = self.root.join(rel_path);
        if !self.fs.is_dir(&pkg_dir) {
            return None;
        }
        
        let files = load_vo_files(&self.fs, &pkg_dir)?;
        let name = rel_path.rsplit('/').next().unwrap_or(rel_path).to_string();
        
        Some(VfsPackage {
            name,
            path: import_path.to_string(),
            files,
        })
    }
    
    pub fn can_handle(&self, import_path: &str) -> bool {
        import_path.starts_with("./") || import_path.starts_with("../")
    }
}

/// External module VFS (module cache).
pub struct ModVfs<F: FileSystem = RealFs> {
    root: PathBuf,
    fs: F,
}

impl ModVfs<RealFs> {
    pub fn new(root: PathBuf) -> Self {
        Self { root, fs: RealFs }
    }
}

impl<F: FileSystem> ModVfs<F> {
    pub fn with_fs(root: PathBuf, fs: F) -> Self {
        Self { root, fs }
    }
    
    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        // e.g., "github.com/user/pkg" -> {mod_root}/github.com/user/pkg
        let pkg_dir = self.root.join(import_path);
        if !self.fs.is_dir(&pkg_dir) {
            return None;
        }
        
        let files = load_vo_files(&self.fs, &pkg_dir)?;
        let name = import_path.rsplit('/').next().unwrap_or(import_path).to_string();
        
        Some(VfsPackage {
            name,
            path: import_path.to_string(),
            files,
        })
    }
    
    pub fn can_handle(&self, import_path: &str) -> bool {
        // External: has dots in domain (not relative paths)
        // e.g., "github.com/user/pkg" but not "./mylib"
        !import_path.starts_with("./") 
            && !import_path.starts_with("../")
            && import_path.contains('.')
    }
}

/// Combined VFS that delegates to the appropriate source.
pub struct Vfs<F: FileSystem = RealFs> {
    pub std_vfs: StdVfs<F>,
    pub local_vfs: LocalVfs<F>,
    pub mod_vfs: ModVfs<F>,
}

impl Vfs<RealFs> {
    /// Create a VFS with three filesystem root paths using real filesystem.
    ///
    /// # Arguments
    /// * `std_root` - Root path for standard library (e.g., "/usr/local/vo/std")
    /// * `local_root` - Root path for local packages (usually project directory)
    /// * `mod_root` - Root path for module cache (e.g., "~/.vo/mod")
    pub fn with_fs_roots(std_root: PathBuf, local_root: PathBuf, mod_root: PathBuf) -> Self {
        Self {
            std_vfs: StdVfs::new(std_root),
            local_vfs: LocalVfs::new(local_root),
            mod_vfs: ModVfs::new(mod_root),
        }
    }
}

impl<F: FileSystem + Clone> Vfs<F> {
    /// Create a VFS with three filesystem root paths using custom filesystem.
    pub fn with_fs(std_root: PathBuf, local_root: PathBuf, mod_root: PathBuf, fs: F) -> Self {
        Self {
            std_vfs: StdVfs::with_fs(std_root, fs.clone()),
            local_vfs: LocalVfs::with_fs(local_root, fs.clone()),
            mod_vfs: ModVfs::with_fs(mod_root, fs),
        }
    }
}

impl<F: FileSystem> Vfs<F> {
    /// Resolve a package by import path.
    /// 
    /// Resolution order follows module.md spec:
    /// 1. Explicit local paths (./xxx or ../xxx) → LocalVfs
    /// 2. External dependencies (has dots like github.com/...) → ModVfs  
    /// 3. Non-dotted paths: try stdlib first, then local fallback
    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        // Explicit local paths
        if import_path.starts_with("./") || import_path.starts_with("../") {
            return self.local_vfs.resolve(import_path);
        }
        
        // External dependencies (has dots = domain name)
        if import_path.contains('.') {
            return self.mod_vfs.resolve(import_path);
        }
        
        // Non-dotted paths: try stdlib first, then local fallback
        // This matches module.md spec section 6.3:
        //   "P" (known stdlib) → stdlib
        //   "P" (not stdlib) → <project-root>/<P>/
        if let Some(pkg) = self.std_vfs.resolve(import_path) {
            return Some(pkg);
        }
        
        // Fallback to local package (e.g., "iface" → ./iface/)
        self.local_vfs.resolve(import_path)
    }
}

/// Helper to load all .vo files from a directory.
fn load_vo_files<F: FileSystem>(fs: &F, dir: &Path) -> Option<Vec<VfsFile>> {
    let mut files = Vec::new();
    
    let entries = fs.read_dir(dir).ok()?;
    for path in entries {
        if path.extension().map(|e| e == "vo").unwrap_or(false) {
            if let Ok(content) = fs.read_file(&path) {
                files.push(VfsFile {
                    path: path.file_name().unwrap().into(),
                    content,
                });
            }
        }
    }
    
    if files.is_empty() {
        None
    } else {
        Some(files)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_can_handle() {
        let std_vfs = StdVfs::new(PathBuf::new());
        let local_vfs = LocalVfs::new(PathBuf::new());
        let mod_vfs = ModVfs::new(PathBuf::new());
        
        // Stdlib
        assert!(std_vfs.can_handle("fmt"));
        assert!(std_vfs.can_handle("encoding/json"));
        assert!(!std_vfs.can_handle("github.com/user/pkg"));
        
        // Local
        assert!(local_vfs.can_handle("./mylib"));
        assert!(local_vfs.can_handle("../shared"));
        assert!(!local_vfs.can_handle("fmt"));
        
        // Mod
        assert!(mod_vfs.can_handle("github.com/user/pkg"));
        assert!(!mod_vfs.can_handle("fmt"));
        assert!(!mod_vfs.can_handle("./mylib"));
    }
}
