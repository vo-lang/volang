//! Package resolution system.
//!
//! Provides three package sources:
//! - StdSource: Standard library packages (embedded in binary for vo-cli)
//! - LocalSource: Local packages (relative paths)
//! - ModSource: External module dependencies

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use vo_common::vfs::{FileSystem, RealFs};

/// A resolved package from the package resolver.
#[derive(Debug, Clone)]
pub struct VfsPackage {
    /// Package name (e.g., "fmt", "mylib")
    pub name: String,
    /// Package path (e.g., "fmt", "./mylib", "github.com/user/pkg")
    pub path: String,
    /// Resolved path in the underlying file system.
    pub fs_path: PathBuf,
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

    // If the package directory contains a vo.mod file, use the declared module path
    // as the canonical package path. This ensures that relative imports like
    // `"../../libs/vox"` produce the same extern lookup names as the full module
    // path `"github.com/vo-lang/vox"` — both resolve to the same VfsPackage.path.
    let canonical_path = try_read_module_path(fs, pkg_path)
        .unwrap_or_else(|| import_path.to_string());

    let name = canonical_path.rsplit('/').next().unwrap_or(&canonical_path).to_string();
    
    let fs_path_abs = match fs.root() {
        Some(root) => root.join(pkg_path),
        None => pkg_path.to_path_buf(),
    };

    Some(VfsPackage {
        name,
        path: canonical_path,
        fs_path: fs_path_abs,
        files,
    })
}

/// Try to read the `module` declaration from a `vo.mod` file in `pkg_dir`.
/// Returns the module path (e.g. `"github.com/vo-lang/vox"`) or `None` if the
/// file does not exist or contains no `module` line.
fn try_read_module_path<F: FileSystem>(fs: &F, pkg_dir: &Path) -> Option<String> {
    let vomod = pkg_dir.join("vo.mod");
    let content = fs.read_file(&vomod).ok()?;
    for line in content.lines() {
        if let Some(rest) = line.trim().strip_prefix("module ") {
            let m = rest.trim();
            if !m.is_empty() {
                return Some(m.to_string());
            }
        }
    }
    None
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

/// Decorator resolver that applies `replace` directives before delegating.
///
/// When an import path matches a replaced module (exact or sub-package),
/// the package is resolved from the replacement's local filesystem path
/// instead of the module cache.
pub struct ReplacingResolver<R> {
    inner: R,
    /// module path → absolute local directory
    replaces: HashMap<String, PathBuf>,
}

impl<R> ReplacingResolver<R> {
    pub fn new(inner: R, replaces: HashMap<String, PathBuf>) -> Self {
        Self { inner, replaces }
    }
}

impl<R: Resolver> Resolver for ReplacingResolver<R> {
    fn resolve(&self, import_path: &str, importer_dir: &str) -> Option<VfsPackage> {
        // Check if any replace directive matches this import.
        // Matches exact module path or sub-packages (module + "/").
        for (module, local_dir) in &self.replaces {
            let (matched, sub) = if import_path == module.as_str() {
                (true, "")
            } else if import_path.starts_with(module.as_str())
                && import_path.as_bytes().get(module.len()) == Some(&b'/')
            {
                (true, &import_path[module.len() + 1..])
            } else {
                (false, "")
            };

            if matched {
                let resolve_dir = if sub.is_empty() {
                    local_dir.clone()
                } else {
                    local_dir.join(sub)
                };
                let fs = RealFs::new(&resolve_dir);
                return resolve_package(&fs, ".", import_path);
            }
        }

        self.inner.resolve(import_path, importer_dir)
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
