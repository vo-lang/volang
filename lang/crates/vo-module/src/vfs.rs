//! Package resolution system.
//!
//! Provides three package sources:
//! - StdSource: Standard library packages (embedded in binary for vo-cli)
//! - LocalSource: Local packages (relative paths)
//! - ModSource: External module dependencies

use std::path::{Path, PathBuf};

use vo_common::vfs::{FileSystem, RealFs};

/// A resolved package from the package resolver.
#[derive(Debug, Clone)]
pub struct VfsPackage {
    /// Package name (e.g., "fmt", "mylib")
    pub name: String,
    /// Package path (e.g., "fmt", "./mylib", "github.com/user/pkg")
    pub path: String,
    /// Source files in the package
    pub files: Vec<VfsFile>,
}

/// A source file from a package.
#[derive(Debug, Clone)]
pub struct VfsFile {
    /// File path relative to package
    pub path: PathBuf,
    /// File content
    pub content: String,
}

/// Standard library package source.
pub struct StdSource<F: FileSystem = RealFs> {
    fs: F,
}

impl StdSource<RealFs> {
    pub fn new(root: PathBuf) -> Self {
        Self { fs: RealFs::new(&root) }
    }
}

impl<F: FileSystem> StdSource<F> {
    pub fn with_fs(fs: F) -> Self {
        Self { fs }
    }
    
    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        resolve_package(&self.fs, import_path, import_path)
    }
    
    pub fn can_handle(&self, import_path: &str) -> bool {
        !import_path.contains('.')
    }
}

/// Local package source (relative paths).
pub struct LocalSource<F: FileSystem = RealFs> {
    fs: F,
}

impl LocalSource<RealFs> {
    pub fn new(root: PathBuf) -> Self {
        Self { fs: RealFs::new(&root) }
    }
}

impl<F: FileSystem> LocalSource<F> {
    pub fn with_fs(fs: F) -> Self {
        Self { fs }
    }
    
    /// Resolve a local import path relative to the importer's directory.
    /// 
    /// Go semantics: `../foo` from `pkg/sub/` resolves to `pkg/foo/`
    pub fn resolve(&self, import_path: &str, importer_dir: &str) -> Option<VfsPackage> {
        // Combine importer_dir with the relative import path
        let full_path = if importer_dir.is_empty() || importer_dir == "." {
            import_path.trim_start_matches("./").to_string()
        } else {
            format!("{}/{}", importer_dir, import_path.trim_start_matches("./"))
        };
        resolve_package(&self.fs, &full_path, import_path)
    }
    
    pub fn can_handle(&self, import_path: &str) -> bool {
        import_path.starts_with("./") || import_path.starts_with("../")
    }
}

/// External module package source (module cache).
pub struct ModSource<F: FileSystem = RealFs> {
    fs: F,
}

impl ModSource<RealFs> {
    pub fn new(root: PathBuf) -> Self {
        Self { fs: RealFs::new(&root) }
    }
}

impl<F: FileSystem> ModSource<F> {
    pub fn with_fs(fs: F) -> Self {
        Self { fs }
    }
    
    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        resolve_package(&self.fs, import_path, import_path)
    }
    
    pub fn can_handle(&self, import_path: &str) -> bool {
        !import_path.starts_with("./") 
            && !import_path.starts_with("../")
            && import_path.contains('.')
    }
}

/// Trait for package resolution.
pub trait Resolver: Send + Sync {
    /// Resolve an import path.
    /// 
    /// `import_path` - the import path (e.g., "fmt", "./foo", "../bar")
    /// `importer_dir` - directory of the importing package (for relative path resolution)
    fn resolve(&self, import_path: &str, importer_dir: &str) -> Option<VfsPackage>;
}

/// Package resolver with potentially different file systems for each source.
pub struct PackageResolverMixed<S: FileSystem, L: FileSystem, M: FileSystem> {
    pub std: StdSource<S>,
    pub local: LocalSource<L>,
    pub r#mod: ModSource<M>,
}

/// Type alias for resolver with same filesystem for all sources.
pub type PackageResolver<F = RealFs> = PackageResolverMixed<F, F, F>;

impl PackageResolver<RealFs> {
    /// Create a resolver with three filesystem root paths using real filesystem.
    pub fn with_roots(std_root: PathBuf, local_root: PathBuf, mod_root: PathBuf) -> Self {
        Self {
            std: StdSource::new(std_root),
            local: LocalSource::new(local_root),
            r#mod: ModSource::new(mod_root),
        }
    }
}

impl<F: FileSystem + Clone> PackageResolver<F> {
    /// Create a resolver with a single shared filesystem (e.g., MemoryFs, ZipFs).
    pub fn with_fs(fs: F) -> Self {
        Self {
            std: StdSource::with_fs(fs.clone()),
            local: LocalSource::with_fs(fs.clone()),
            r#mod: ModSource::with_fs(fs),
        }
    }
}

impl<S: FileSystem + Send + Sync, L: FileSystem + Send + Sync, M: FileSystem + Send + Sync> Resolver for PackageResolverMixed<S, L, M> {
    fn resolve(&self, import_path: &str, importer_dir: &str) -> Option<VfsPackage> {
        if import_path.starts_with("./") || import_path.starts_with("../") {
            return self.local.resolve(import_path, importer_dir);
        }
        
        if import_path.contains('.') {
            return self.r#mod.resolve(import_path);
        }
        
        if let Some(pkg) = self.std.resolve(import_path) {
            return Some(pkg);
        }
        
        self.local.resolve(import_path, importer_dir)
    }
}

/// Helper to resolve a package from a file system.
fn resolve_package<F: FileSystem>(fs: &F, fs_path: &str, import_path: &str) -> Option<VfsPackage> {
    let pkg_path = Path::new(fs_path);
    if !fs.is_dir(pkg_path) {
        return None;
    }
    
    let files = load_vo_files(fs, pkg_path)?;
    let name = fs_path.rsplit('/').next().unwrap_or(fs_path).to_string();
    
    Some(VfsPackage {
        name,
        path: import_path.to_string(),
        files,
    })
}

/// Helper to load all .vo files from a directory.
fn load_vo_files<F: FileSystem>(fs: &F, dir: &Path) -> Option<Vec<VfsFile>> {
    let mut files = Vec::new();
    
    let entries = fs.read_dir(dir).ok()?;
    for path in entries {
        if path.extension().is_some_and(|e| e == "vo") {
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
        let std_vfs = StdSource::new(PathBuf::new());
        let local_vfs = LocalSource::new(PathBuf::new());
        let mod_vfs = ModSource::new(PathBuf::new());
        
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
