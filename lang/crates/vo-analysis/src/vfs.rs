//! Package resolution system.
//!
//! Provides package sources and resolver decorators:
//! - StdSource: Standard library packages (embedded in binary for vo-cli)
//! - ModSource: External module dependencies
//!
//! Migrated from vo-module-old/src/vfs.rs — package resolution is a compiler
//! concern, not a module protocol concern (spec §9.1).

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use vo_common::abi::package_abi_path;
use vo_common::vfs::{normalize_fs_path, FileSet, FileSystem, RealFs};
use vo_module::ext_manifest::extension_name_from_content;

use crate::project::{AnalysisError, Project};

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
        Self {
            fs: RealFs::new(&root),
        }
    }
}

impl<F: FileSystem> StdSource<F> {
    pub fn with_fs(fs: F) -> Self {
        Self { fs }
    }

    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        resolve_package(&self.fs, Path::new(import_path), import_path)
    }

    pub fn can_handle(&self, import_path: &str) -> bool {
        !import_path.contains('.')
    }
}

/// External module package source (module cache).
pub struct ModSource<F: FileSystem = RealFs> {
    fs: F,
    allowed_modules: Option<HashSet<String>>,
    module_roots: Option<HashMap<String, PathBuf>>,
}

impl ModSource<RealFs> {
    pub fn new(root: PathBuf) -> Self {
        Self {
            fs: RealFs::new(&root),
            allowed_modules: None,
            module_roots: None,
        }
    }
}

impl<F: FileSystem> ModSource<F> {
    pub fn with_fs(fs: F) -> Self {
        Self {
            fs,
            allowed_modules: None,
            module_roots: None,
        }
    }

    pub fn with_allowed_modules<I, S>(mut self, modules: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.allowed_modules = Some(modules.into_iter().map(Into::into).collect());
        self
    }

    pub fn with_module_roots<I, S, P>(mut self, roots: I) -> Self
    where
        I: IntoIterator<Item = (S, P)>,
        S: Into<String>,
        P: Into<PathBuf>,
    {
        self.module_roots = Some(
            roots
                .into_iter()
                .map(|(module, root)| (module.into(), root.into()))
                .collect(),
        );
        self
    }

    /// Configure allowed modules and locked module roots from project dependencies.
    ///
    /// This is the canonical way to wire a `ModSource` to a resolved `ProjectDeps`:
    /// - If a `vo.mod` was found, restricts resolution to allowed modules only.
    /// - If locked modules exist, maps each to its cache-relative version directory.
    pub fn with_project_deps(mut self, deps: &vo_module::project::ProjectDeps) -> Self {
        if deps.has_mod_file() {
            self = self.with_allowed_modules(deps.allowed_modules().to_vec());
        }
        if !deps.locked_modules().is_empty() {
            self = self.with_module_roots(deps.locked_modules().iter().map(|locked| {
                let rel = vo_module::cache::layout::relative_module_dir(
                    locked.path.as_str(),
                    &locked.version,
                );
                (locked.path.as_str().to_string(), rel)
            }));
        }
        self
    }

    pub fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        if !self.is_allowed(import_path) {
            return None;
        }
        if let Some((module, root)) = self.match_module_root(import_path) {
            let fs_path = if import_path == module {
                root.clone()
            } else {
                root.join(&import_path[module.len() + 1..])
            };
            return resolve_package(&self.fs, &fs_path, import_path);
        }
        resolve_package(&self.fs, Path::new(import_path), import_path)
    }

    pub fn can_handle(&self, import_path: &str) -> bool {
        import_path.contains('.')
            && import_path != "."
            && import_path != ".."
            && !import_path.starts_with("./")
            && !import_path.starts_with("../")
            && !import_path.starts_with('/')
    }

    fn is_allowed(&self, import_path: &str) -> bool {
        match &self.allowed_modules {
            None => true,
            Some(allowed_modules) => allowed_modules.iter().any(|module| {
                import_path == module
                    || (import_path.starts_with(module)
                        && import_path.as_bytes().get(module.len()) == Some(&b'/'))
            }),
        }
    }

    fn match_module_root(&self, import_path: &str) -> Option<(&str, &PathBuf)> {
        self.module_roots
            .as_ref()?
            .iter()
            .filter(|(module, _)| {
                import_path == module.as_str()
                    || (import_path.starts_with(module.as_str())
                        && import_path.as_bytes().get(module.len()) == Some(&b'/'))
            })
            .max_by_key(|(module, _)| module.len())
            .map(|(module, root)| (module.as_str(), root))
    }
}

/// Trait for package resolution.
pub trait Resolver: Send + Sync {
    /// Resolve an import path.
    ///
    /// `import_path` - the import path (e.g., "fmt", "encoding/json", "github.com/acme/lib")
    fn resolve(&self, import_path: &str) -> Option<VfsPackage>;
}

/// Package resolver with potentially different file systems for each source.
pub struct PackageResolverMixed<S: FileSystem, M: FileSystem> {
    pub std: StdSource<S>,
    pub r#mod: ModSource<M>,
}

/// Type alias for resolver with same filesystem for all sources.
pub type PackageResolver<F = RealFs> = PackageResolverMixed<F, F>;

impl PackageResolver<RealFs> {
    /// Create a resolver with stdlib and module-cache roots using real filesystem.
    pub fn with_roots(std_root: PathBuf, mod_root: PathBuf) -> Self {
        Self {
            std: StdSource::new(std_root),
            r#mod: ModSource::new(mod_root),
        }
    }
}

impl<F: FileSystem + Clone> PackageResolver<F> {
    /// Create a resolver with a single shared filesystem (e.g., MemoryFs, ZipFs).
    pub fn with_fs(fs: F) -> Self {
        Self {
            std: StdSource::with_fs(fs.clone()),
            r#mod: ModSource::with_fs(fs),
        }
    }
}

pub fn project_mod_source<F: FileSystem>(
    mod_fs: F,
    deps: &vo_module::project::ProjectDeps,
) -> ModSource<F> {
    ModSource::with_fs(mod_fs).with_project_deps(deps)
}

pub fn project_package_resolver_with_replaces<S: FileSystem, M: FileSystem, R: FileSystem>(
    std_fs: S,
    mod_fs: M,
    replace_fs: R,
    deps: &vo_module::project::ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
) -> ReplacingResolver<PackageResolverMixed<S, M>, R> {
    ReplacingResolver::with_fs(
        PackageResolverMixed {
            std: StdSource::with_fs(std_fs),
            r#mod: project_mod_source(mod_fs, deps),
        },
        replace_fs,
        workspace_replaces,
    )
}

impl<S: FileSystem + Send + Sync, M: FileSystem + Send + Sync> Resolver
    for PackageResolverMixed<S, M>
{
    fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        if import_path.contains('.') {
            return self.r#mod.resolve(import_path);
        }

        self.std.resolve(import_path)
    }
}

/// Helper to resolve a package from a file system.
fn resolve_package<F: FileSystem>(fs: &F, fs_path: &Path, import_path: &str) -> Option<VfsPackage> {
    let pkg_path = fs_path;
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
    let extension_name =
        find_extension_name_abs(&fs_path_abs).or_else(|| find_extension_name_in_fs(fs, pkg_path));
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

pub(crate) fn find_module_identity_abs(abs_pkg_path: &Path) -> Option<(String, String)> {
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

pub(crate) fn read_module_path_from_disk(dir: &Path) -> Option<String> {
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
    if base == Path::new(".") || base.as_os_str().is_empty() {
        return path_to_forward_slashes(path);
    }
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
pub struct ReplacingResolver<R, F = RealFs> {
    inner: R,
    fs: F,
    replaces: HashMap<String, PathBuf>,
}

impl<R> ReplacingResolver<R, RealFs> {
    pub fn new(inner: R, replaces: HashMap<String, PathBuf>) -> Self {
        Self {
            inner,
            fs: RealFs::new("."),
            replaces,
        }
    }
}

impl<R, F> ReplacingResolver<R, F> {
    pub fn with_fs(inner: R, fs: F, replaces: HashMap<String, PathBuf>) -> Self {
        Self { inner, fs, replaces }
    }

    fn match_replace<'a>(&'a self, import_path: &'a str) -> Option<(&'a str, &'a PathBuf, &'a str)> {
        self.replaces
            .iter()
            .filter_map(|(module, local_dir)| {
                if import_path == module.as_str() {
                    Some((module.as_str(), local_dir, ""))
                } else if import_path.starts_with(module.as_str())
                    && import_path.as_bytes().get(module.len()) == Some(&b'/')
                {
                    Some((module.as_str(), local_dir, &import_path[module.len() + 1..]))
                } else {
                    None
                }
            })
            .max_by_key(|(module, _, _)| module.len())
    }
}

impl<R: Resolver, F: FileSystem> Resolver for ReplacingResolver<R, F> {
    fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
        if let Some((_module, local_dir, sub)) = self.match_replace(import_path) {
            let resolve_dir = if sub.is_empty() {
                local_dir.clone()
            } else {
                normalize_fs_path(&local_dir.join(sub))
            };
            return resolve_package(&self.fs, &resolve_dir, import_path);
        }

        self.inner.resolve(import_path)
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
    local_root: PathBuf,
    current_module: Option<String>,
}

impl<R, F> CurrentModuleResolver<R, F> {
    pub fn new(inner: R, local_fs: F, current_module: Option<String>) -> Self {
        Self::with_root(inner, local_fs, PathBuf::from("."), current_module)
    }

    pub fn with_root(
        inner: R,
        local_fs: F,
        local_root: impl Into<PathBuf>,
        current_module: Option<String>,
    ) -> Self {
        Self {
            inner,
            local_fs,
            local_root: normalize_fs_path(&local_root.into()),
            current_module,
        }
    }
}

impl<R: Resolver, F: FileSystem> Resolver for CurrentModuleResolver<R, F> {
    fn resolve(&self, import_path: &str) -> Option<VfsPackage> {
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
                    self.local_root.clone()
                } else {
                    normalize_fs_path(&self.local_root.join(sub_path))
                };
                if let Some(pkg) = resolve_package(&self.local_fs, &local_path, import_path) {
                    return Some(pkg);
                }
            }
        }

        self.inner.resolve(import_path)
    }
}

pub fn analyze_file_set_with_current_module<R: Resolver, F: FileSystem>(
    file_set: FileSet,
    resolver: R,
    local_fs: F,
    local_root: impl Into<PathBuf>,
    current_module: Option<String>,
) -> Result<Project, AnalysisError> {
    let resolver = CurrentModuleResolver::with_root(resolver, local_fs, local_root, current_module);
    crate::project::analyze_project(file_set, &resolver)
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::vfs::MemoryFs;

    #[test]
    fn test_can_handle() {
        let std_vfs = StdSource::new(PathBuf::new());
        let mod_vfs = ModSource::new(PathBuf::new());

        // Stdlib
        assert!(std_vfs.can_handle("fmt"));
        assert!(std_vfs.can_handle("encoding/json"));
        assert!(!std_vfs.can_handle("github.com/user/pkg"));

        // Mod
        assert!(mod_vfs.can_handle("github.com/user/pkg"));
        assert!(!mod_vfs.can_handle("fmt"));
        assert!(!mod_vfs.can_handle("./mylib"));
    }

    #[test]
    fn test_package_resolver_does_not_resolve_relative_imports() {
        let mut fs = MemoryFs::new();
        fs.add_file("shared/utils.vo", "package utils\n");

        let resolver = PackageResolver::with_fs(fs);
        assert!(resolver.resolve("../../shared").is_none());
    }

    #[test]
    fn test_current_module_resolves_root_and_subpackage_from_local_fs() {
        let mut fs = MemoryFs::new();
        fs.add_file("vo.mod", "module github.com/acme/game\n\nvo 0.1\n");
        fs.add_file("main.vo", "package main\n");
        fs.add_file("codec/codec.vo", "package codec\n");

        let base = PackageResolver::with_fs(fs.clone());
        let resolver =
            CurrentModuleResolver::new(base, fs, Some("github.com/acme/game".to_string()));

        let root = resolver
            .resolve("github.com/acme/game")
            .expect("root package should resolve");
        assert_eq!(root.path, "github.com/acme/game");
        assert_eq!(root.name, "game");
        assert_eq!(root.files.len(), 1);

        let sub = resolver
            .resolve("github.com/acme/game/codec")
            .expect("subpackage should resolve");
        assert_eq!(sub.path, "github.com/acme/game/codec");
        assert_eq!(sub.name, "codec");
        assert_eq!(sub.files.len(), 1);
    }

    #[test]
    fn test_current_module_resolves_with_prefixed_local_root() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/game/vo.mod",
            "module github.com/acme/game\n\nvo 0.1\n",
        );
        fs.add_file("workspace/game/main.vo", "package main\n");
        fs.add_file("workspace/game/codec/codec.vo", "package codec\n");

        let base = PackageResolver::with_fs(fs.clone());
        let resolver = CurrentModuleResolver::with_root(
            base,
            fs,
            "workspace/game",
            Some("github.com/acme/game".to_string()),
        );

        let root = resolver
            .resolve("github.com/acme/game")
            .expect("root package should resolve from prefixed local root");
        assert_eq!(root.fs_path, PathBuf::from("workspace/game"));

        let sub = resolver
            .resolve("github.com/acme/game/codec")
            .expect("subpackage should resolve from prefixed local root");
        assert_eq!(sub.fs_path, PathBuf::from("workspace/game/codec"));
    }

    #[test]
    fn test_mod_source_resolves_versioned_module_roots() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "cache/github.com/acme/game/.vo/versions/v0.1.0/vo.mod",
            "module github.com/acme/game\n\nvo 0.1\n",
        );
        fs.add_file(
            "cache/github.com/acme/game/.vo/versions/v0.1.0/game.vo",
            "package game\n",
        );
        fs.add_file(
            "cache/github.com/acme/game/.vo/versions/v0.1.0/codec/codec.vo",
            "package codec\n",
        );

        let mod_source = ModSource::with_fs(fs).with_module_roots([(
            "github.com/acme/game",
            PathBuf::from("cache/github.com/acme/game/.vo/versions/v0.1.0"),
        )]);

        let root = mod_source
            .resolve("github.com/acme/game")
            .expect("root package should resolve from mapped version dir");
        assert_eq!(root.path, "github.com/acme/game");
        assert_eq!(
            root.fs_path,
            PathBuf::from("cache/github.com/acme/game/.vo/versions/v0.1.0")
        );

        let sub = mod_source
            .resolve("github.com/acme/game/codec")
            .expect("subpackage should resolve from mapped version dir");
        assert_eq!(sub.path, "github.com/acme/game/codec");
        assert_eq!(
            sub.fs_path,
            PathBuf::from("cache/github.com/acme/game/.vo/versions/v0.1.0/codec")
        );
    }

    #[test]
    fn test_replacing_resolver_resolves_from_memory_fs_override() {
        let mut workspace_fs = MemoryFs::new();
        workspace_fs.add_file(
            "workspace/voplay/vo.mod",
            "module github.com/vo-lang/voplay\n\nvo 0.1\n",
        );
        workspace_fs.add_file("workspace/voplay/voplay.vo", "package voplay\n");
        workspace_fs.add_file("workspace/voplay/codec/codec.vo", "package codec\n");

        let base = PackageResolver::with_fs(MemoryFs::new());
        let resolver = ReplacingResolver::with_fs(
            base,
            workspace_fs,
            HashMap::from([(
                "github.com/vo-lang/voplay".to_string(),
                PathBuf::from("workspace/voplay"),
            )]),
        );

        let root = resolver
            .resolve("github.com/vo-lang/voplay")
            .expect("root package should resolve from replacement");
        assert_eq!(root.path, "github.com/vo-lang/voplay");
        assert_eq!(root.fs_path, PathBuf::from("workspace/voplay"));

        let sub = resolver
            .resolve("github.com/vo-lang/voplay/codec")
            .expect("subpackage should resolve from replacement");
        assert_eq!(sub.path, "github.com/vo-lang/voplay/codec");
        assert_eq!(sub.fs_path, PathBuf::from("workspace/voplay/codec"));
    }
}
