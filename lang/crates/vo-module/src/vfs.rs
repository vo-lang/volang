//! Package resolution system.
//!
//! Provides three package sources:
//! - StdSource: Standard library packages (embedded in binary for vo-cli)
//! - LocalSource: Local packages (relative paths)
//! - ModSource: External module dependencies

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use vo_common::abi::package_abi_path;
use vo_common::vfs::{FileSystem, RealFs};
use crate::ext_manifest::extension_name_from_content;

/// A resolved package from the package resolver.
#[derive(Debug, Clone)]
pub struct VfsPackage {
    /// Package name (e.g., "fmt", "mylib")
    pub name: String,
    /// Package path (e.g., "fmt", "./mylib", "github.com/user/pkg")
    pub path: String,
    /// Stable ABI path used for extern lookup names.
    pub abi_path: String,
    /// Root module path if this package belongs to a module.
    pub module_path: Option<String>,
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
        // Combine importer_dir with the relative import path, then normalize
        // so that "../" sequences are resolved before hitting the filesystem.
        // This is critical for MemoryFs which stores keys verbatim.
        let raw = if importer_dir.is_empty() || importer_dir == "." {
            import_path.to_string()
        } else {
            format!("{}/{}", importer_dir, import_path)
        };
        let full_path = normalize_path(&raw);
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

/// Normalize a forward-slash path string by resolving `.` and `..` components.
///
/// This is needed for `MemoryFs` which stores keys verbatim and cannot resolve
/// `"pkg/sub/../foo"` the way the OS would.  RealFs is also normalized as a
/// minor cleanliness win.
fn normalize_path(path: &str) -> String {
    let mut parts: Vec<&str> = Vec::new();
    for component in path.split('/') {
        match component {
            "" | "." => {}
            ".." => { parts.pop(); }
            other => parts.push(other),
        }
    }
    if parts.is_empty() {
        ".".to_string()
    } else {
        parts.join("/")
    }
}

/// Helper to resolve a package from a file system.
fn resolve_package<F: FileSystem>(fs: &F, fs_path: &str, import_path: &str) -> Option<VfsPackage> {
    let pkg_path = Path::new(fs_path);
    if !fs.is_dir(pkg_path) {
        return None;
    }
    
    let files = load_vo_files(fs, pkg_path)?;

    let fs_path_abs = match fs.root() {
        Some(root) => root.join(pkg_path),
        None => pkg_path.to_path_buf(),
    };

    let (module_path, canonical_path) =
        resolve_canonical_package_path(fs, pkg_path, &fs_path_abs, import_path);
    let extension_name = find_extension_name_abs(&fs_path_abs)
        .or_else(|| find_extension_name_in_fs(fs, pkg_path));
    let abi_path = package_abi_path(
        &canonical_path,
        module_path.as_deref(),
        extension_name.as_deref(),
    );
    let name = abi_path.rsplit('/').next().unwrap_or(&abi_path).to_string();

    Some(VfsPackage {
        name,
        path: canonical_path,
        abi_path,
        module_path,
        fs_path: fs_path_abs,
        files,
    })
}

fn resolve_canonical_package_path<F: FileSystem>(
    fs: &F,
    pkg_path: &Path,
    abs_pkg_path: &Path,
    import_path: &str,
) -> (Option<String>, String) {
    if let Some((module_path, sub_path)) = find_module_identity_abs(abs_pkg_path) {
        let canonical_path = join_module_and_subpath(&module_path, &sub_path);
        return (Some(module_path), canonical_path);
    }
    if let Some((module_path, sub_path)) = find_module_identity_in_fs(fs, pkg_path) {
        let canonical_path = join_module_and_subpath(&module_path, &sub_path);
        return (Some(module_path), canonical_path);
    }
    (None, import_path.to_string())
}

fn find_module_identity_in_fs<F: FileSystem>(fs: &F, pkg_path: &Path) -> Option<(String, String)> {
    for ancestor in relative_path_ancestors(pkg_path) {
        if let Some(module_path) = try_read_module_path(fs, &ancestor) {
            let sub_path = diff_path(pkg_path, &ancestor);
            return Some((module_path, sub_path));
        }
    }
    None
}

fn find_module_identity_abs(abs_pkg_path: &Path) -> Option<(String, String)> {
    if !abs_pkg_path.is_absolute() {
        return None;
    }
    let mut dir = abs_pkg_path;
    loop {
        let module_path = read_module_path_from_disk(dir);
        if let Some(module_path) = module_path {
            let sub_path = abs_pkg_path
                .strip_prefix(dir)
                .ok()
                .map(path_to_forward_slashes)
                .unwrap_or_default();
            return Some((module_path, sub_path));
        }
        dir = dir.parent()?;
    }
}

fn read_module_path_from_disk(dir: &Path) -> Option<String> {
    let content = std::fs::read_to_string(dir.join("vo.mod")).ok()?;
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

fn find_extension_name_in_fs<F: FileSystem>(fs: &F, pkg_path: &Path) -> Option<String> {
    for ancestor in relative_path_ancestors(pkg_path) {
        if let Ok(content) = fs.read_file(&ancestor.join("vo.ext.toml")) {
            if let Some(name) = extension_name_from_content(&content) {
                return Some(name);
            }
        }
    }
    None
}

fn find_extension_name_abs(abs_pkg_path: &Path) -> Option<String> {
    if !abs_pkg_path.is_absolute() {
        return None;
    }
    let mut dir = abs_pkg_path;
    loop {
        if let Ok(content) = std::fs::read_to_string(dir.join("vo.ext.toml")) {
            if let Some(name) = extension_name_from_content(&content) {
                return Some(name);
            }
        }
        dir = dir.parent()?;
    }
}

fn relative_path_ancestors(path: &Path) -> Vec<PathBuf> {
    let mut ancestors = Vec::new();
    let mut current = if path.as_os_str().is_empty() {
        PathBuf::from(".")
    } else {
        path.to_path_buf()
    };
    loop {
        ancestors.push(current.clone());
        if current == Path::new(".") || current.as_os_str().is_empty() {
            break;
        }
        if !current.pop() {
            ancestors.push(PathBuf::from("."));
            break;
        }
        if current.as_os_str().is_empty() {
            current = PathBuf::from(".");
        }
    }
    ancestors
}

fn diff_path(path: &Path, base: &Path) -> String {
    path.strip_prefix(base)
        .ok()
        .map(path_to_forward_slashes)
        .unwrap_or_default()
}

fn join_module_and_subpath(module_path: &str, sub_path: &str) -> String {
    if sub_path.is_empty() {
        module_path.to_string()
    } else {
        format!("{}/{}", module_path, sub_path)
    }
}

fn path_to_forward_slashes(path: &Path) -> String {
    let raw = path.to_string_lossy().replace('\\', "/");
    if raw == "." {
        String::new()
    } else {
        raw
    }
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

/// Decorator resolver that resolves imports from the current project module
/// directly from the local filesystem before delegating.
///
/// This lets a project import its own absolute module path and sub-packages
/// (for example `github.com/vo-lang/voplay/codec`) without requiring explicit
/// `replace` directives or nested `vo.mod` files.
pub struct CurrentModuleResolver<R, F> {
    inner: R,
    local_fs: F,
    current_module: Option<String>,
}

impl<R, F> CurrentModuleResolver<R, F> {
    pub fn new(inner: R, local_fs: F, current_module: Option<String>) -> Self {
        Self {
            inner,
            local_fs,
            current_module,
        }
    }
}

impl<R: Resolver, F: FileSystem> Resolver for CurrentModuleResolver<R, F> {
    fn resolve(&self, import_path: &str, importer_dir: &str) -> Option<VfsPackage> {
        if let Some(module) = &self.current_module {
            let sub_path = if import_path == module {
                Some("")
            } else if import_path.starts_with(module.as_str())
                && import_path.as_bytes().get(module.len()) == Some(&b'/')
            {
                Some(&import_path[module.len() + 1..])
            } else {
                None
            };

            if let Some(sub_path) = sub_path {
                let local_path = if sub_path.is_empty() {
                    "."
                } else {
                    sub_path
                };
                if let Some(pkg) = resolve_package(&self.local_fs, local_path, import_path) {
                    return Some(pkg);
                }
            }
        }

        self.inner.resolve(import_path, importer_dir)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::vfs::MemoryFs;
    
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

    #[test]
    fn test_normalize_path() {
        assert_eq!(normalize_path("./foo"), "foo");
        assert_eq!(normalize_path("pkg/sub/../foo"), "pkg/foo");
        assert_eq!(normalize_path("a/b/../../c"), "c");
        assert_eq!(normalize_path("a/./b"), "a/b");
        assert_eq!(normalize_path(".."), ".");
        assert_eq!(normalize_path("a/b/c"), "a/b/c");
    }

    #[test]
    fn test_local_source_resolves_parent_relative_path_with_memory_fs() {
        let mut fs = MemoryFs::new();
        fs.add_file("shared/utils.vo", "package utils\n");

        let source = LocalSource::with_fs(fs);
        // Importer is in "pkg/sub", import path is "../../shared"
        // Raw: "pkg/sub/../../shared" → normalized: "shared"
        let pkg = source.resolve("../../shared", "pkg/sub")
            .expect("../../shared from pkg/sub should resolve to shared/");
        assert_eq!(pkg.name, "shared");
        assert_eq!(pkg.files.len(), 1);
    }

    #[test]
    fn test_current_module_resolves_root_and_subpackage_from_local_fs() {
        let mut fs = MemoryFs::new();
        fs.add_file("vo.mod", "module github.com/acme/game\n");
        fs.add_file("main.vo", "package main\n");
        fs.add_file("codec/codec.vo", "package codec\n");

        let base = PackageResolver::with_fs(fs.clone());
        let resolver = CurrentModuleResolver::new(
            base,
            fs,
            Some("github.com/acme/game".to_string()),
        );

        let root = resolver.resolve("github.com/acme/game", ".")
            .expect("root package should resolve");
        assert_eq!(root.path, "github.com/acme/game");
        assert_eq!(root.name, "game");
        assert_eq!(root.files.len(), 1);

        let sub = resolver.resolve("github.com/acme/game/codec", ".")
            .expect("subpackage should resolve");
        assert_eq!(sub.path, "github.com/acme/game/codec");
        assert_eq!(sub.name, "codec");
        assert_eq!(sub.files.len(), 1);
    }
}
