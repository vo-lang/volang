//! Package resolution system.
//!
//! Provides package sources and resolver decorators:
//! - StdSource: Standard library packages (embedded in binary for vo-cli)
//! - ModSource: External module dependencies
//!
//! Migrated from vo-module-old/src/vfs.rs — package resolution is a compiler
//! concern, not a module protocol concern (spec §9.1).

use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::project::{AnalysisError, PackageIdentity, Project};
use vo_common::abi::package_abi_path;
use vo_common::vfs::{
    normalize_fs_path, sort_fs_paths, FileSet, FileSystem, RealFs, MAX_DIRECTORY_ENTRIES,
    MAX_PACKAGE_SOURCE_BYTES, MAX_PACKAGE_SOURCE_FILES, MAX_TEXT_FILE_BYTES,
};

/// A resolved package from the package resolver.
#[derive(Debug, Clone)]
pub struct VfsPackage {
    /// Canonical package and ABI identity.
    identity: PackageIdentity,
    /// Root module path if this package belongs to a module.
    module_path: Option<vo_module::identity::ModulePath>,
    /// Resolved path in the underlying file system.
    fs_path: PathBuf,
    /// Extension metadata parsed from the same filesystem view as `files`.
    ///
    /// The manifest path retains the materialized path of the owning `vo.mod`
    /// so later native-extension preparation can resolve module-relative paths
    /// without consulting a second metadata source.
    extension: Option<vo_module::ext_manifest::ExtensionManifest>,
    /// Source files in the package
    files: Vec<VfsFile>,
}

impl VfsPackage {
    /// Construct one resolved package while preserving its canonical package,
    /// module-owner, extension, and extern-ABI identity as one invariant.
    pub fn try_new(
        path: String,
        module_path: Option<String>,
        fs_path: PathBuf,
        mut extension: Option<vo_module::ext_manifest::ExtensionManifest>,
        files: Vec<VfsFile>,
    ) -> Result<Self, String> {
        let identity = PackageIdentity::new(path)?;
        let import_class = vo_module::identity::classify_import(identity.path())
            .map_err(|error| format!("resolved package identity is not importable: {error}"))?;
        if files.is_empty() {
            return Err(format!(
                "package '{}' contains no Vo source files",
                identity.path()
            ));
        }
        if files.len() > MAX_PACKAGE_SOURCE_FILES {
            return Err(format!(
                "package '{}' contains {} source files, exceeding the {MAX_PACKAGE_SOURCE_FILES}-file limit",
                identity.path(),
                files.len()
            ));
        }
        let mut total_source_bytes = 0usize;
        let mut source_names = vo_module::schema::PortablePathSet::default();
        for file in &files {
            let mut components = file.path.components();
            let Some(std::path::Component::Normal(file_name)) = components.next() else {
                return Err(format!(
                    "package '{}' contains a non-canonical source file path '{}'",
                    identity.path(),
                    file.path.display()
                ));
            };
            if components.next().is_some()
                || file.path.extension() != Some(std::ffi::OsStr::new("vo"))
            {
                return Err(format!(
                    "package '{}' source '{}' must be one canonical .vo file name",
                    identity.path(),
                    file.path.display()
                ));
            }
            let file_name = file_name.to_str().ok_or_else(|| {
                format!(
                    "package '{}' contains a non-UTF-8 source file name",
                    identity.path()
                )
            })?;
            if !source_names
                .insert_file(file_name)
                .map_err(|error| format!("invalid package source file '{file_name}': {error}"))?
            {
                return Err(format!("duplicate package source file '{file_name}'"));
            }
            if file.content.len() > MAX_TEXT_FILE_BYTES {
                return Err(format!(
                    "package '{}' source '{}' exceeds the {MAX_TEXT_FILE_BYTES}-byte text-file limit",
                    identity.path(),
                    file.path.display()
                ));
            }
            total_source_bytes = total_source_bytes
                .checked_add(file.content.len())
                .ok_or_else(|| format!("package '{}' source size overflow", identity.path()))?;
            if total_source_bytes > MAX_PACKAGE_SOURCE_BYTES {
                return Err(format!(
                    "package '{}' exceeds the {MAX_PACKAGE_SOURCE_BYTES}-byte source limit",
                    identity.path()
                ));
            }
        }
        let module_path = module_path
            .map(|module| {
                vo_module::identity::ModulePath::parse(&module).map_err(|error| error.to_string())
            })
            .transpose()?;
        if import_class == vo_module::identity::ImportClass::External && module_path.is_none() {
            return Err(format!(
                "external package '{}' has no canonical owning module",
                identity.path()
            ));
        }
        let fs_path = normalize_fs_path(&fs_path);
        let package_subpath = if let Some(module) = module_path.as_ref() {
            let Some(subpath) = module.owns_import(identity.path()) else {
                return Err(format!(
                    "package '{}' is outside its owning module '{}'",
                    identity.path(),
                    module
                ));
            };
            Some(subpath)
        } else if extension.is_some() {
            return Err(format!(
                "package '{}' carries extension metadata without an owning module",
                identity.path()
            ));
        } else {
            None
        };
        if let Some(extension) = extension.as_mut() {
            let package_subpath = package_subpath.ok_or_else(|| {
                format!(
                    "package '{}' carries extension metadata without a canonical module subpath",
                    identity.path()
                )
            })?;
            let owning_module = module_path.as_ref().ok_or_else(|| {
                format!(
                    "package '{}' carries extension metadata without an owning module",
                    identity.path()
                )
            })?;
            extension
                .validate()
                .map_err(|error| format!("invalid extension metadata: {error}"))?;
            extension.manifest_path = normalize_fs_path(&extension.manifest_path);
            let mut module_fs_path = fs_path.clone();
            for _ in package_subpath
                .split('/')
                .filter(|component| !component.is_empty())
            {
                if !module_fs_path.pop() {
                    return Err(format!(
                        "package filesystem path '{}' cannot contain module owner '{}'",
                        fs_path.display(),
                        owning_module
                    ));
                }
            }
            let expected_manifest = normalize_fs_path(&module_fs_path.join("vo.mod"));
            if extension.manifest_path != expected_manifest {
                return Err(format!(
                    "package '{}' extension manifest '{}' does not match owning module manifest '{}'",
                    identity.path(),
                    extension.manifest_path.display(),
                    expected_manifest.display()
                ));
            }
        }
        Ok(Self {
            identity,
            module_path,
            fs_path,
            extension,
            files,
        })
    }

    pub fn path(&self) -> &str {
        self.identity.path()
    }

    pub fn abi_path(&self) -> &str {
        self.identity.abi_path()
    }

    pub fn module_path(&self) -> Option<&str> {
        self.module_path.as_ref().map(|module| module.as_str())
    }

    pub fn fs_path(&self) -> &Path {
        &self.fs_path
    }

    pub fn extension(&self) -> Option<&vo_module::ext_manifest::ExtensionManifest> {
        self.extension.as_ref()
    }

    pub fn files(&self) -> &[VfsFile] {
        &self.files
    }
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

    pub fn resolve(&self, import_path: &str) -> Result<Option<VfsPackage>, String> {
        let class =
            vo_module::identity::classify_import(import_path).map_err(|error| error.to_string())?;
        if class != vo_module::identity::ImportClass::Stdlib {
            return Ok(None);
        }
        resolve_package(&self.fs, Path::new(import_path), import_path)
    }

    pub fn can_handle(&self, import_path: &str) -> bool {
        matches!(
            vo_module::identity::classify_import(import_path),
            Ok(vo_module::identity::ImportClass::Stdlib)
        )
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

    pub fn with_project_allowed_modules(mut self, deps: &vo_module::project::ProjectDeps) -> Self {
        if deps.has_mod_file() {
            self = self.with_allowed_modules(deps.allowed_modules().to_vec());
        }
        self
    }

    pub fn with_project_locked_module_roots(
        mut self,
        deps: &vo_module::project::ProjectDeps,
    ) -> Self {
        if !deps.locked_modules().is_empty() {
            self = self.with_module_roots(project_locked_module_roots(deps));
        }
        self
    }

    pub fn with_project_deps(self, deps: &vo_module::project::ProjectDeps) -> Self {
        self.with_project_allowed_modules(deps)
            .with_project_locked_module_roots(deps)
    }

    pub fn resolve(&self, import_path: &str) -> Result<Option<VfsPackage>, String> {
        let class =
            vo_module::identity::classify_import(import_path).map_err(|error| error.to_string())?;
        if class != vo_module::identity::ImportClass::External {
            return Ok(None);
        }
        if !self.is_allowed(import_path) {
            return Ok(None);
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
        matches!(
            vo_module::identity::classify_import(import_path),
            Ok(vo_module::identity::ImportClass::External)
        )
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

fn project_locked_module_roots(deps: &vo_module::project::ProjectDeps) -> Vec<(String, PathBuf)> {
    deps.locked_modules()
        .iter()
        .map(|locked| {
            let rel = vo_module::cache::layout::relative_module_dir(&locked.path, &locked.version);
            (locked.path.as_str().to_string(), rel)
        })
        .collect()
}

/// Trait for package resolution.
pub trait Resolver: Send + Sync {
    /// Resolve an import path.
    ///
    /// `import_path` - the import path (e.g., "fmt", "encoding/json", "github.com/acme/lib")
    fn resolve(&self, import_path: &str) -> Result<Option<VfsPackage>, String>;
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
    ModSource::with_fs(mod_fs)
        .with_project_allowed_modules(deps)
        .with_project_locked_module_roots(deps)
}

pub fn project_package_resolver_with_workspace_sources<
    S: FileSystem,
    M: FileSystem,
    R: FileSystem,
>(
    std_fs: S,
    mod_fs: M,
    workspace_fs: R,
    deps: &vo_module::project::ProjectDeps,
    workspace_sources: HashMap<String, PathBuf>,
) -> WorkspaceSourceResolver<PackageResolverMixed<S, M>, R> {
    WorkspaceSourceResolver::with_fs(
        PackageResolverMixed {
            std: StdSource::with_fs(std_fs),
            r#mod: project_mod_source(mod_fs, deps),
        },
        workspace_fs,
        workspace_sources,
    )
}

impl<S: FileSystem + Send + Sync, M: FileSystem + Send + Sync> Resolver
    for PackageResolverMixed<S, M>
{
    fn resolve(&self, import_path: &str) -> Result<Option<VfsPackage>, String> {
        match vo_module::identity::classify_import(import_path)
            .map_err(|error| error.to_string())?
        {
            vo_module::identity::ImportClass::External => self.r#mod.resolve(import_path),
            vo_module::identity::ImportClass::Stdlib => self.std.resolve(import_path),
        }
    }
}

/// Helper to resolve a package from a file system.
fn resolve_package<F: FileSystem>(
    fs: &F,
    fs_path: &Path,
    import_path: &str,
) -> Result<Option<VfsPackage>, String> {
    resolve_package_with_manifest_domain(fs, fs_path, import_path, ModuleManifestDomain::Project)
}

fn resolve_package_with_manifest_domain<F: FileSystem>(
    fs: &F,
    fs_path: &Path,
    import_path: &str,
    manifest_domain: ModuleManifestDomain,
) -> Result<Option<VfsPackage>, String> {
    let pkg_path = fs_path;
    if !fs.is_dir(pkg_path) {
        return Ok(None);
    }

    let Some(files) = load_vo_files(fs, pkg_path)? else {
        return Ok(None);
    };

    let fs_path_abs = match fs.root() {
        Some(root) => root.join(pkg_path),
        None => pkg_path.to_path_buf(),
    };

    let module_metadata = find_module_metadata_in_fs(fs, pkg_path, manifest_domain)?;
    let (module_path, canonical_path, extension) = match module_metadata {
        Some(metadata) => {
            let canonical_path = join_module_and_subpath(&metadata.module_path, &metadata.sub_path);
            (
                Some(metadata.module_path),
                canonical_path,
                metadata.extension,
            )
        }
        None => (None, import_path.to_string(), None),
    };
    VfsPackage::try_new(canonical_path, module_path, fs_path_abs, extension, files).map(Some)
}

pub(crate) struct PackageModuleMetadata {
    pub(crate) module_path: String,
    pub(crate) sub_path: String,
    pub(crate) extension: Option<vo_module::ext_manifest::ExtensionManifest>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModuleManifestDomain {
    Project,
    SynthesizedEphemeral,
}

/// Resolve the nearest module boundary from the supplied filesystem only.
/// Module identity, extension ABI identity, and the typed extension manifest
/// all come from one bounded read of the same `vo.mod` bytes.
fn find_module_metadata_in_fs<F: FileSystem>(
    fs: &F,
    pkg_path: &Path,
    manifest_domain: ModuleManifestDomain,
) -> Result<Option<PackageModuleMetadata>, String> {
    for ancestor in relative_path_ancestors(pkg_path) {
        let mod_path = ancestor.join("vo.mod");
        let content = match fs.read_text_limited(&mod_path, MAX_TEXT_FILE_BYTES) {
            Ok(content) => content,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => {
                return Err(format!(
                    "failed to read module file '{}': {error}",
                    mod_path.display()
                ));
            }
        };
        let mut mod_file = match manifest_domain {
            ModuleManifestDomain::Project => {
                vo_module::schema::modfile::ModFile::parse_project(&content)
            }
            ModuleManifestDomain::SynthesizedEphemeral => {
                vo_module::schema::modfile::ModFile::parse_ephemeral(&content)
            }
        }
        .map_err(|error| format!("{}: {}", mod_path.display(), error))?;
        if let Some(extension) = mod_file.extension.as_mut() {
            extension.manifest_path = materialized_fs_path(fs, &mod_path);
        }
        return Ok(Some(PackageModuleMetadata {
            module_path: mod_file.module.as_str().to_string(),
            sub_path: diff_path(pkg_path, &ancestor)?,
            extension: mod_file.extension,
        }));
    }
    Ok(None)
}

fn materialized_fs_path<F: FileSystem>(fs: &F, path: &Path) -> PathBuf {
    normalize_fs_path(&match fs.root() {
        Some(root) => root.join(path),
        None => path.to_path_buf(),
    })
}

pub(crate) fn find_module_metadata_abs(
    abs_pkg_path: &Path,
) -> Result<Option<PackageModuleMetadata>, String> {
    if !abs_pkg_path.is_absolute() {
        return Ok(None);
    }
    let mut dir = abs_pkg_path;
    loop {
        let mod_path = dir.join("vo.mod");
        match vo_common::vfs::read_text_file(&mod_path) {
            Ok(content) => {
                let mut mod_file = vo_module::schema::modfile::ModFile::parse(&content)
                    .map_err(|error| format!("{}: {error}", mod_path.display()))?;
                if let Some(extension) = mod_file.extension.as_mut() {
                    extension.manifest_path = normalize_fs_path(&mod_path);
                }
                return Ok(Some(PackageModuleMetadata {
                    module_path: mod_file.module.as_str().to_string(),
                    sub_path: diff_path(abs_pkg_path, dir)?,
                    extension: mod_file.extension,
                }));
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => {
                return Err(format!(
                    "failed to read module file '{}': {error}",
                    mod_path.display()
                ));
            }
        }
        let Some(parent) = dir.parent() else {
            return Ok(None);
        };
        dir = parent;
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

fn diff_path(path: &Path, base: &Path) -> Result<String, String> {
    let relative = if base == Path::new(".") || base.as_os_str().is_empty() {
        path
    } else {
        path.strip_prefix(base).map_err(|_| {
            format!(
                "package path '{}' is outside module root '{}'",
                path.display(),
                base.display()
            )
        })?
    };
    if relative.as_os_str().is_empty() || relative == Path::new(".") {
        return Ok(String::new());
    }
    vo_module::schema::portable_relative_path_from_path(relative).map_err(|error| {
        format!(
            "package subpath '{}' is not canonical: {error}",
            relative.display()
        )
    })
}

fn join_module_and_subpath(module_path: &str, sub_path: &str) -> String {
    if sub_path.is_empty() {
        module_path.to_string()
    } else {
        format!("{}/{}", module_path, sub_path)
    }
}

/// Helper to load all .vo files from a directory.
fn load_vo_files<F: FileSystem>(fs: &F, dir: &Path) -> Result<Option<Vec<VfsFile>>, String> {
    let mut files = Vec::new();
    let mut total_bytes = 0usize;
    let dir = normalize_fs_path(dir);

    let entries = fs.read_dir(&dir).map_err(|error| {
        format!(
            "failed to read package directory '{}': {error}",
            dir.display()
        )
    })?;
    let mut entries: Vec<_> = entries
        .into_iter()
        .map(|entry| normalize_fs_path(&entry))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    if entries.len() > MAX_DIRECTORY_ENTRIES {
        return Err(format!(
            "package directory '{}' contains more than {MAX_DIRECTORY_ENTRIES} entries",
            dir.display()
        ));
    }
    sort_fs_paths(&mut entries);
    for path in entries {
        if path.parent().map(normalize_fs_path).as_deref() != Some(dir.as_path()) {
            return Err(format!(
                "package directory '{}' returned non-child entry '{}'",
                dir.display(),
                path.display()
            ));
        }
        if !fs.is_dir(&path) && path.extension().is_some_and(|e| e == "vo") {
            if files.len() >= MAX_PACKAGE_SOURCE_FILES {
                return Err(format!(
                    "package directory '{}' contains more than {MAX_PACKAGE_SOURCE_FILES} source files",
                    dir.display()
                ));
            }
            let remaining = MAX_PACKAGE_SOURCE_BYTES.saturating_sub(total_bytes);
            let content = fs
                .read_text_limited(&path, MAX_TEXT_FILE_BYTES.min(remaining))
                .map_err(|error| {
                    format!("failed to read source file '{}': {error}", path.display())
                })?;
            total_bytes = total_bytes.checked_add(content.len()).ok_or_else(|| {
                format!(
                    "package directory '{}' exceeds the {MAX_PACKAGE_SOURCE_BYTES}-byte source limit",
                    dir.display()
                )
            })?;
            if total_bytes > MAX_PACKAGE_SOURCE_BYTES {
                return Err(format!(
                    "package directory '{}' exceeds the {MAX_PACKAGE_SOURCE_BYTES}-byte source limit",
                    dir.display()
                ));
            }
            let file_name = path.file_name().ok_or_else(|| {
                format!(
                    "package directory '{}' returned a source path without a file name: '{}'",
                    dir.display(),
                    path.display()
                )
            })?;
            files.push(VfsFile {
                path: file_name.into(),
                content,
            });
        }
    }

    if files.is_empty() {
        Ok(None)
    } else {
        Ok(Some(files))
    }
}

/// Resolver that selects authorized workspace sources before the module cache.
///
/// When an import path is owned by a selected workspace module, the package is
/// resolved from that module's local source root. All other imports delegate
/// to the registry-backed resolver.
pub struct WorkspaceSourceResolver<R, F = RealFs> {
    inner: R,
    fs: F,
    sources: HashMap<String, PathBuf>,
}

impl<R> WorkspaceSourceResolver<R, RealFs> {
    pub fn new(inner: R, sources: HashMap<String, PathBuf>) -> Self {
        Self {
            inner,
            fs: RealFs::new("."),
            sources,
        }
    }
}

impl<R, F> WorkspaceSourceResolver<R, F> {
    pub fn with_fs(inner: R, fs: F, sources: HashMap<String, PathBuf>) -> Self {
        Self { inner, fs, sources }
    }

    fn match_source<'a>(&'a self, import_path: &'a str) -> Option<(&'a str, &'a PathBuf, &'a str)> {
        self.sources
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

impl<R: Resolver, F: FileSystem> Resolver for WorkspaceSourceResolver<R, F> {
    fn resolve(&self, import_path: &str) -> Result<Option<VfsPackage>, String> {
        vo_module::identity::classify_import(import_path).map_err(|error| error.to_string())?;
        if let Some((_module, local_dir, sub)) = self.match_source(import_path) {
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
/// (for example `github.com/vo-lang/voplay/codec`) without workspace
/// sources or nested `vo.mod` files.
pub struct CurrentModuleResolver<R, F> {
    inner: R,
    local_fs: F,
    local_root: PathBuf,
    current_module: Option<String>,
    manifest_domain: ModuleManifestDomain,
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
        Self::with_root_and_manifest_domain(
            inner,
            local_fs,
            local_root,
            current_module,
            ModuleManifestDomain::Project,
        )
    }

    fn with_root_and_manifest_domain(
        inner: R,
        local_fs: F,
        local_root: impl Into<PathBuf>,
        current_module: Option<String>,
        manifest_domain: ModuleManifestDomain,
    ) -> Self {
        Self {
            inner,
            local_fs,
            local_root: normalize_fs_path(&local_root.into()),
            current_module,
            manifest_domain,
        }
    }
}

impl<R: Resolver, F: FileSystem> Resolver for CurrentModuleResolver<R, F> {
    fn resolve(&self, import_path: &str) -> Result<Option<VfsPackage>, String> {
        vo_module::identity::classify_import(import_path).map_err(|error| error.to_string())?;
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
                if let Some(pkg) = resolve_package_with_manifest_domain(
                    &self.local_fs,
                    &local_path,
                    import_path,
                    self.manifest_domain,
                )? {
                    return Ok(Some(pkg));
                }
            }
        }

        self.inner.resolve(import_path)
    }
}

/// Compatibility entry point for analyzing the module-root package.
///
/// `local_root` locates the module inside the supplied filesystem; it is not a
/// semantic package subpath. Call [`analyze_file_set_with_package_identity`]
/// whenever the analyzed package lives below the module root.
pub fn analyze_file_set_with_current_module<R: Resolver, F: FileSystem>(
    file_set: FileSet,
    resolver: R,
    local_fs: F,
    local_root: impl Into<PathBuf>,
    current_module: Option<String>,
) -> Result<Project, AnalysisError> {
    analyze_file_set_with_package_identity_inner(
        file_set,
        resolver,
        local_fs,
        local_root,
        current_module,
        None,
        FileSetAnalysisAuthority::project_module_root(),
    )
}

/// Analyze a compiler-synthesized ephemeral module whose captured `vo.mod`
/// uses the reserved `local/*` identity domain.
///
/// Callers must first derive and validate the manifest through the ephemeral
/// project boundary. Ordinary projects must use
/// [`analyze_file_set_with_current_module`], which rejects `local/*`.
pub fn analyze_file_set_with_synthesized_ephemeral_module<R: Resolver, F: FileSystem>(
    file_set: FileSet,
    resolver: R,
    local_fs: F,
    local_root: impl Into<PathBuf>,
    current_module: String,
) -> Result<Project, AnalysisError> {
    let identity = vo_module::identity::ModIdentity::parse(&current_module)
        .map_err(|error| AnalysisError::Import(error.to_string()))?;
    if !identity.is_local() {
        return Err(AnalysisError::Import(
            "synthesized ephemeral analysis requires a local/* module identity".to_string(),
        ));
    }
    analyze_file_set_with_package_identity_inner(
        file_set,
        resolver,
        local_fs,
        local_root,
        Some(current_module),
        None,
        FileSetAnalysisAuthority::synthesized_ephemeral_module_root(),
    )
}

/// Analyze a file set with explicit module-resolution and root-package
/// identities. `current_module` owns local import resolution; `current_package`
/// identifies the exact package being checked, including any subdirectory.
/// Passing neither selects the isolated ad-hoc identity and never probes the
/// host filesystem for module metadata.
pub fn analyze_file_set_with_package_identity<R: Resolver, F: FileSystem>(
    file_set: FileSet,
    resolver: R,
    local_fs: F,
    local_root: impl Into<PathBuf>,
    current_module: Option<String>,
    current_package: Option<PackageIdentity>,
) -> Result<Project, AnalysisError> {
    analyze_file_set_with_package_identity_inner(
        file_set,
        resolver,
        local_fs,
        local_root,
        current_module,
        current_package,
        FileSetAnalysisAuthority::explicit_project_package(),
    )
}

#[derive(Debug, Clone, Copy)]
struct FileSetAnalysisAuthority {
    derive_module_root_identity: bool,
    manifest_domain: ModuleManifestDomain,
}

impl FileSetAnalysisAuthority {
    const fn project_module_root() -> Self {
        Self {
            derive_module_root_identity: true,
            manifest_domain: ModuleManifestDomain::Project,
        }
    }

    const fn synthesized_ephemeral_module_root() -> Self {
        Self {
            derive_module_root_identity: true,
            manifest_domain: ModuleManifestDomain::SynthesizedEphemeral,
        }
    }

    const fn explicit_project_package() -> Self {
        Self {
            derive_module_root_identity: false,
            manifest_domain: ModuleManifestDomain::Project,
        }
    }
}

fn analyze_file_set_with_package_identity_inner<R: Resolver, F: FileSystem>(
    file_set: FileSet,
    resolver: R,
    local_fs: F,
    local_root: impl Into<PathBuf>,
    current_module: Option<String>,
    mut current_package: Option<PackageIdentity>,
    authority: FileSetAnalysisAuthority,
) -> Result<Project, AnalysisError> {
    let local_root = normalize_fs_path(&local_root.into());
    let root_metadata = if current_module.is_some() {
        find_module_metadata_in_fs(&local_fs, &local_root, authority.manifest_domain)
            .map_err(AnalysisError::Import)?
    } else {
        None
    };

    if let (Some(module), Some(metadata)) = (current_module.as_deref(), root_metadata.as_ref()) {
        if metadata.module_path != module {
            return Err(AnalysisError::Import(format!(
                "current module '{}' does not match module '{}' in the supplied filesystem",
                module, metadata.module_path
            )));
        }
    }

    if authority.derive_module_root_identity {
        current_package = current_module
            .as_deref()
            .map(PackageIdentity::new)
            .transpose()
            .map_err(AnalysisError::Import)?;
    }

    if let (Some(module), Some(package)) = (current_module.as_deref(), current_package.as_ref()) {
        let belongs_to_module = package.path() == module
            || package
                .path()
                .strip_prefix(module)
                .is_some_and(|suffix| suffix.starts_with('/'));
        if !belongs_to_module {
            return Err(AnalysisError::Import(format!(
                "current package '{}' is outside current module '{}'",
                package.path(),
                module
            )));
        }
        if let Some(metadata) = root_metadata.as_ref() {
            let expected_abi = package_abi_path(package.path());
            if package.abi_path() != expected_abi {
                return Err(AnalysisError::Import(format!(
                    "current package ABI identity '{}' does not match captured module metadata '{}'; expected '{}'",
                    package.abi_path(),
                    metadata.module_path,
                    expected_abi
                )));
            }
        }
    }
    if current_module.is_some() && current_package.is_none() {
        return Err(AnalysisError::Import(
            "current module requires an explicit root package identity".to_string(),
        ));
    }

    let root_extension = root_metadata.and_then(|metadata| metadata.extension);
    let resolver = CurrentModuleResolver::with_root_and_manifest_domain(
        resolver,
        local_fs,
        local_root,
        current_module,
        authority.manifest_domain,
    );
    match current_package {
        Some(identity) => crate::project::analyze_project_with_identity_and_extension(
            file_set,
            &resolver,
            identity,
            root_extension,
        ),
        // Absence is deliberate in this explicit API. Do not let archive,
        // memory, or ad-hoc frontends inherit a host filesystem module merely
        // because FileSet::root happens to live below one.
        None => crate::project::analyze_project_with_identity_and_extension(
            file_set,
            &resolver,
            PackageIdentity::ad_hoc(),
            None,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::vfs::MemoryFs;

    static TEMP_TEST_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    #[derive(Clone)]
    struct FaultFs {
        inner: MemoryFs,
        fail_dir: Option<PathBuf>,
        fail_file: Option<PathBuf>,
        deny_unbounded_text_reads: bool,
    }

    impl FileSystem for FaultFs {
        fn read_file(&self, path: &Path) -> std::io::Result<String> {
            if self.deny_unbounded_text_reads {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Unsupported,
                    "unbounded text reads are disabled",
                ));
            }
            if self.fail_file.as_deref() == Some(path) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::PermissionDenied,
                    "injected source read failure",
                ));
            }
            self.inner.read_file(path)
        }

        fn read_text_limited(&self, path: &Path, max_bytes: usize) -> std::io::Result<String> {
            if self.fail_file.as_deref() == Some(path) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::PermissionDenied,
                    "injected source read failure",
                ));
            }
            self.inner.read_text_limited(path, max_bytes)
        }

        fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            self.inner.read_bytes(path)
        }

        fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
            if self.fail_dir.as_deref() == Some(path) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::PermissionDenied,
                    "injected directory read failure",
                ));
            }
            self.inner.read_dir(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.inner.exists(path)
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.inner.is_dir(path)
        }
    }

    #[test]
    fn test_can_handle() {
        let std_vfs = StdSource::new(PathBuf::new());
        let mod_vfs = ModSource::new(PathBuf::new());

        // Stdlib
        assert!(std_vfs.can_handle("fmt"));
        assert!(std_vfs.can_handle("encoding/json"));
        assert!(std_vfs.can_handle("encoding/version.1"));
        assert!(!std_vfs.can_handle("github.com/user/pkg"));
        assert!(!std_vfs.can_handle("local/demo"));

        // Mod
        assert!(mod_vfs.can_handle("github.com/user/pkg"));
        assert!(mod_vfs.can_handle("github.com/user/pkg/version.1"));
        assert!(!mod_vfs.can_handle("fmt"));
        assert!(!mod_vfs.can_handle("./mylib"));
        assert!(!mod_vfs.can_handle("local/demo"));
    }

    #[test]
    fn resolved_package_construction_seals_identity_and_module_ownership() {
        let files = || {
            vec![VfsFile {
                path: PathBuf::from("lib.vo"),
                content: "package lib\n".to_string(),
            }]
        };
        let package = VfsPackage::try_new(
            "github.com/acme/lib/\u{56fe}\u{5f62}".to_string(),
            Some("github.com/acme/lib".to_string()),
            PathBuf::from("cache/lib/\u{56fe}\u{5f62}"),
            None,
            files(),
        )
        .unwrap();
        assert_eq!(package.path(), "github.com/acme/lib/\u{56fe}\u{5f62}");
        assert_eq!(
            package.abi_path(),
            package_abi_path("github.com/acme/lib/\u{56fe}\u{5f62}")
        );

        let forged_extension = vo_module::ext_manifest::parse_ext_manifest_content(
            "module = \"github.com/acme/other\"\nvo = \"^0.1.0\"\n\n[extension]\nname = \"lib\"\n\n[extension.web]\n",
            Path::new("cache/other/vo.mod"),
        )
        .unwrap();
        assert!(VfsPackage::try_new(
            "github.com/acme/lib/\u{56fe}\u{5f62}".to_string(),
            Some("github.com/acme/lib".to_string()),
            PathBuf::from("cache/lib/\u{56fe}\u{5f62}"),
            Some(forged_extension),
            files(),
        )
        .is_err());

        assert!(VfsPackage::try_new(
            "github.com/acme/lib".to_string(),
            Some("github.com/other/lib".to_string()),
            PathBuf::from("cache/lib"),
            None,
            files(),
        )
        .is_err());
        assert!(VfsPackage::try_new(
            "github.com/acme/lib".to_string(),
            None,
            PathBuf::from("cache/lib"),
            None,
            files(),
        )
        .is_err());
        assert!(VfsPackage::try_new(
            "local/demo".to_string(),
            None,
            PathBuf::from("demo"),
            None,
            files(),
        )
        .is_err());
        assert!(VfsPackage::try_new(
            "github.com/acme/lib/../other".to_string(),
            Some("github.com/acme/lib".to_string()),
            PathBuf::from("cache/lib"),
            None,
            files(),
        )
        .is_err());
        assert!(VfsPackage::try_new(
            "github.com/acme/lib".to_string(),
            Some("github.com/acme/lib".to_string()),
            PathBuf::from("cache/lib"),
            None,
            vec![VfsFile {
                path: PathBuf::from("nested/lib.vo"),
                content: "package lib\n".to_string(),
            }],
        )
        .is_err());
        assert!(VfsPackage::try_new(
            "github.com/acme/lib".to_string(),
            Some("github.com/acme/lib".to_string()),
            PathBuf::from("cache/lib"),
            None,
            vec![
                VfsFile {
                    path: PathBuf::from("A.vo"),
                    content: "package lib\n".to_string(),
                },
                VfsFile {
                    path: PathBuf::from("a.vo"),
                    content: "package lib\n".to_string(),
                },
            ],
        )
        .is_err());
    }

    #[test]
    fn package_resolver_uses_the_authoritative_first_segment_classifier() {
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("encoding/version.1/version.vo", "package version\n"),
            ),
            r#mod: ModSource::with_fs(
                MemoryFs::new()
                    .with_file(
                        "github.com/acme/lib/vo.mod",
                        "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
                    )
                    .with_file(
                        "github.com/acme/lib/version.1/version.vo",
                        "package version\n",
                    ),
            ),
        };

        assert!(resolver.resolve("encoding/version.1").unwrap().is_some());
        assert!(resolver
            .resolve("github.com/acme/lib/version.1")
            .unwrap()
            .is_some());
    }

    #[test]
    fn explicit_ad_hoc_analysis_does_not_inherit_a_host_module() {
        let id = TEMP_TEST_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "vo-analysis-explicit-ad-hoc-{}-{id}",
            std::process::id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(
            root.join("vo.mod"),
            "module = \"github.com/acme/host-module\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();

        let mut files = FileSet::new(root.clone());
        files.files.insert(
            PathBuf::from("main.vo"),
            "package main\nfunc main() {}\n".to_string(),
        );
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };

        let project = analyze_file_set_with_package_identity(
            files,
            resolver,
            MemoryFs::new(),
            PathBuf::from("."),
            None,
            None,
        )
        .unwrap();
        assert_eq!(project.main_pkg().path(), "main");
        assert_eq!(project.main_pkg().abi_path(), "main");

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn implicit_live_analysis_rejects_invalid_nearest_module_manifest() {
        let id = TEMP_TEST_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "vo-analysis-invalid-live-module-{}-{id}",
            std::process::id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(
            root.join("vo.mod"),
            "module = \"github.com/Acme/invalid\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();

        let mut files = FileSet::new(root.clone());
        files.files.insert(
            PathBuf::from("main.vo"),
            "package main\nfunc main() {}\n".to_string(),
        );
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };

        let error = match crate::project::analyze_project(files, &resolver) {
            Ok(_) => panic!("invalid live module manifest must fail analysis"),
            Err(error) => error,
        };
        assert!(error.to_string().contains("vo.mod"), "{error}");
        assert!(error.to_string().contains("canonical"), "{error}");

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn implicit_live_analysis_captures_ancestor_identity_and_extension_together() {
        let id = TEMP_TEST_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let module_root = std::env::temp_dir().join(format!(
            "vo-analysis-live-context-{}-{id}",
            std::process::id()
        ));
        let package_root = module_root.join("cmd/tool");
        std::fs::create_dir_all(&package_root).unwrap();
        std::fs::write(
            module_root.join("vo.mod"),
            concat!(
                "module = \"github.com/acme/live\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[extension]\n",
                "name = \"live_extension\"\n",
                "\n[extension.web]\n",
            ),
        )
        .unwrap();

        let mut files = FileSet::new(package_root);
        files.files.insert(
            PathBuf::from("main.vo"),
            "package main\nfunc main() {}\n".to_string(),
        );
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };

        let project = crate::project::analyze_project(files, &resolver).unwrap();
        assert_eq!(project.main_pkg().path(), "github.com/acme/live/cmd/tool");
        assert_eq!(project.extensions.len(), 1);
        assert_eq!(project.extensions[0].name, "live_extension");
        assert_eq!(
            project.extensions[0].manifest_path,
            module_root.canonicalize().unwrap().join("vo.mod")
        );

        std::fs::remove_dir_all(module_root).unwrap();
    }

    #[test]
    fn explicit_root_analysis_uses_extension_metadata_from_local_fs() {
        let id = TEMP_TEST_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "vo-analysis-root-extension-snapshot-{}-{id}",
            std::process::id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(
            root.join("vo.mod"),
            concat!(
                "module = \"github.com/acme/live\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[extension]\n",
                "name = 7\n",
            ),
        )
        .unwrap();

        let module = "github.com/acme/captured";
        let extension_name = "captured_extension";
        let local_fs = MemoryFs::new()
            .with_file(
                root.join("vo.mod"),
                format!("module = \"{module}\"\nvo = \"^0.1.0\"\n\n[extension]\nname = \"{extension_name}\"\n\n[extension.web]\n"),
            )
            .with_file(root.join("main.vo"), "package main\nfunc main() {}\n");
        let mut files = FileSet::new(root.clone());
        files.files.insert(
            PathBuf::from("main.vo"),
            "package main\nfunc main() {}\n".to_string(),
        );
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };
        let identity = PackageIdentity::new(module).unwrap();

        let project = analyze_file_set_with_package_identity(
            files,
            resolver,
            local_fs,
            root.clone(),
            Some(module.to_string()),
            Some(identity),
        )
        .expect("captured root metadata should win over the live host file");
        assert_eq!(project.extensions.len(), 1);
        assert_eq!(project.extensions[0].name, extension_name);
        assert_eq!(project.extensions[0].manifest_path, root.join("vo.mod"));

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn synthesized_ephemeral_analysis_has_an_explicit_local_identity_boundary() {
        let module = "local/ephemeral-analysis";
        let local_fs = MemoryFs::new()
            .with_file(
                "vo.mod",
                format!("module = \"{module}\"\nvo = \"^0.1.0\"\n"),
            )
            .with_file("main.vo", "package main\nfunc main() {}\n");
        let files = || {
            let mut files = FileSet::new(PathBuf::from("."));
            files.files.insert(
                PathBuf::from("main.vo"),
                "package main\nfunc main() {}\n".to_string(),
            );
            files
        };
        let resolver = || PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };

        let project_error = match analyze_file_set_with_current_module(
            files(),
            resolver(),
            local_fs.clone(),
            PathBuf::from("."),
            Some(module.to_string()),
        ) {
            Err(error) => error,
            Ok(_) => panic!("ordinary project analysis accepted local/* authority"),
        };
        assert!(project_error
            .to_string()
            .contains("reserved for toolchain-synthesized ephemeral modules"));

        let project = analyze_file_set_with_synthesized_ephemeral_module(
            files(),
            resolver(),
            local_fs,
            PathBuf::from("."),
            module.to_string(),
        )
        .expect("the explicit synthesized-ephemeral boundary must accept local/*");
        assert_eq!(project.main_pkg().path(), module);
    }

    #[test]
    fn test_package_resolver_rejects_relative_imports() {
        let mut fs = MemoryFs::new();
        fs.add_file("shared/utils.vo", "package utils\n");

        let resolver = PackageResolver::with_fs(fs);
        assert!(resolver.resolve("../../shared").is_err());
    }

    #[test]
    fn package_resolution_reports_directory_enumeration_errors() {
        let fs = FaultFs {
            inner: MemoryFs::new().with_file("broken/source.vo", "package broken\n"),
            fail_dir: Some(PathBuf::from("broken")),
            fail_file: None,
            deny_unbounded_text_reads: false,
        };

        let error = StdSource::with_fs(fs).resolve("broken").unwrap_err();
        assert!(error.contains("failed to read package directory 'broken'"));
        assert!(error.contains("injected directory read failure"));
    }

    #[test]
    fn package_resolution_reports_source_read_errors() {
        let fs = FaultFs {
            inner: MemoryFs::new().with_file("broken/source.vo", "package broken\n"),
            fail_dir: None,
            fail_file: Some(PathBuf::from("broken/source.vo")),
            deny_unbounded_text_reads: false,
        };

        let error = StdSource::with_fs(fs).resolve("broken").unwrap_err();
        assert!(error.contains("failed to read source file 'broken/source.vo'"));
        assert!(error.contains("injected source read failure"));
    }

    #[test]
    fn package_resolution_uses_bounded_text_reads_for_sources_and_module_files() {
        let fs = FaultFs {
            inner: MemoryFs::new()
                .with_file(
                    "bounded/vo.mod",
                    "module = \"github.com/acme/bounded\"\nvo = \"^0.1.0\"\n",
                )
                .with_file("bounded/main.vo", "package bounded\n"),
            fail_dir: None,
            fail_file: None,
            deny_unbounded_text_reads: true,
        };

        let package = StdSource::with_fs(fs)
            .resolve("bounded")
            .unwrap()
            .expect("bounded package should resolve");
        assert_eq!(package.path(), "github.com/acme/bounded");
        assert_eq!(package.files().len(), 1);
    }

    #[test]
    fn package_resolution_reports_module_file_read_errors() {
        let fs = FaultFs {
            inner: MemoryFs::new()
                .with_file(
                    "broken/vo.mod",
                    "module = \"github.com/acme/broken\"\nvo = \"^0.1.0\"\n",
                )
                .with_file("broken/main.vo", "package broken\n"),
            fail_dir: None,
            fail_file: Some(PathBuf::from("broken/vo.mod")),
            deny_unbounded_text_reads: false,
        };

        let error = StdSource::with_fs(fs).resolve("broken").unwrap_err();
        assert!(error.contains("failed to read module file 'broken/vo.mod'"));
        assert!(error.contains("injected source read failure"));
    }

    #[test]
    fn test_current_module_resolves_root_and_subpackage_from_local_fs() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/game\"\n\nvo = \"^0.1.0\"\n",
        );
        fs.add_file("main.vo", "package main\n");
        fs.add_file("codec/codec.vo", "package codec\n");

        let base = PackageResolver::with_fs(fs.clone());
        let resolver =
            CurrentModuleResolver::new(base, fs, Some("github.com/acme/game".to_string()));

        let root = resolver
            .resolve("github.com/acme/game")
            .unwrap()
            .expect("root package should resolve");
        assert_eq!(root.path(), "github.com/acme/game");
        assert_eq!(root.files().len(), 1);

        let sub = resolver
            .resolve("github.com/acme/game/codec")
            .unwrap()
            .expect("subpackage should resolve");
        assert_eq!(sub.path(), "github.com/acme/game/codec");
        assert_eq!(sub.files().len(), 1);
    }

    #[test]
    fn test_current_module_resolves_with_prefixed_local_root() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/game/vo.mod",
            "module = \"github.com/acme/game\"\n\nvo = \"0.1.0\"\n",
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
            .unwrap()
            .expect("root package should resolve from prefixed local root");
        assert_eq!(root.fs_path(), Path::new("workspace/game"));

        let sub = resolver
            .resolve("github.com/acme/game/codec")
            .unwrap()
            .expect("subpackage should resolve from prefixed local root");
        assert_eq!(sub.fs_path(), Path::new("workspace/game/codec"));
    }

    #[test]
    fn test_mod_source_resolves_versioned_module_roots() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "cache/github.com/acme/game/.vo/versions/0.1.0/vo.mod",
            "module = \"github.com/acme/game\"\n\nvo = \"0.1.0\"\n",
        );
        fs.add_file(
            "cache/github.com/acme/game/.vo/versions/0.1.0/game.vo",
            "package game\n",
        );
        fs.add_file(
            "cache/github.com/acme/game/.vo/versions/0.1.0/codec/codec.vo",
            "package codec\n",
        );

        let mod_source = ModSource::with_fs(fs).with_module_roots([(
            "github.com/acme/game",
            PathBuf::from("cache/github.com/acme/game/.vo/versions/0.1.0"),
        )]);

        let root = mod_source
            .resolve("github.com/acme/game")
            .unwrap()
            .expect("root package should resolve from mapped version dir");
        assert_eq!(root.path(), "github.com/acme/game");
        assert_eq!(
            root.fs_path(),
            PathBuf::from("cache/github.com/acme/game/.vo/versions/0.1.0")
        );

        let sub = mod_source
            .resolve("github.com/acme/game/codec")
            .unwrap()
            .expect("subpackage should resolve from mapped version dir");
        assert_eq!(sub.path(), "github.com/acme/game/codec");
        assert_eq!(
            sub.fs_path(),
            PathBuf::from("cache/github.com/acme/game/.vo/versions/0.1.0/codec")
        );
    }

    #[test]
    fn project_mod_source_accepts_only_the_versioned_cache_layout() {
        let deps = vo_module::project::read_inline_project_deps(
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/game\" = \"0.1.0\"\n",
            Some(concat!(
                "version = 3\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[[module]]\n",
                "path = \"github.com/acme/game\"\n",
                "version = \"0.1.0\"\n",
                "vo = \"^0.1.0\"\n",
                "release = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "dependencies = []\n",
            )),
        )
        .unwrap();
        let module = vo_module::identity::ModulePath::parse("github.com/acme/game").unwrap();
        let version = vo_module::version::ExactVersion::parse("0.1.0").unwrap();
        let module_dir = vo_module::cache::layout::relative_module_dir(&module, &version);
        let fs = MemoryFs::new()
            .with_file(
                module_dir.join("vo.mod"),
                "module = \"github.com/acme/game\"\nvo = \"^0.1.0\"\n",
            )
            .with_file(module_dir.join("game.vo"), "package game\n")
            .with_file(
                "github.com/acme/game/vo.mod",
                "module = \"github.com/acme/game\"\nvo = \"^0.1.0\"\n",
            )
            .with_file("github.com/acme/game/legacy.vo", "package game\n");

        let package = project_mod_source(fs, &deps)
            .resolve("github.com/acme/game")
            .unwrap()
            .expect("locked module must resolve from its versioned cache directory");
        assert_eq!(package.fs_path(), module_dir);
        assert!(package
            .files()
            .iter()
            .any(|file| file.path == Path::new("game.vo")));
        assert!(!package
            .files()
            .iter()
            .any(|file| file.path == Path::new("legacy.vo")));
    }

    #[test]
    fn workspace_source_resolver_uses_the_authorized_memory_fs_source() {
        let mut workspace_fs = MemoryFs::new();
        workspace_fs.add_file(
            "workspace/voplay/vo.mod",
            "module = \"github.com/vo-lang/voplay\"\n\nvo = \"0.1.0\"\n",
        );
        workspace_fs.add_file("workspace/voplay/voplay.vo", "package voplay\n");
        workspace_fs.add_file("workspace/voplay/codec/codec.vo", "package codec\n");

        let base = PackageResolver::with_fs(MemoryFs::new());
        let resolver = WorkspaceSourceResolver::with_fs(
            base,
            workspace_fs,
            HashMap::from([(
                "github.com/vo-lang/voplay".to_string(),
                PathBuf::from("workspace/voplay"),
            )]),
        );

        let root = resolver
            .resolve("github.com/vo-lang/voplay")
            .unwrap()
            .expect("root package should resolve from its workspace source");
        assert_eq!(root.path(), "github.com/vo-lang/voplay");
        assert_eq!(root.fs_path(), Path::new("workspace/voplay"));

        let sub = resolver
            .resolve("github.com/vo-lang/voplay/codec")
            .unwrap()
            .expect("subpackage should resolve from its workspace source");
        assert_eq!(sub.path(), "github.com/vo-lang/voplay/codec");
        assert_eq!(sub.fs_path(), Path::new("workspace/voplay/codec"));
    }

    #[test]
    fn workspace_source_resolution_uses_typed_metadata_from_its_filesystem() {
        let id = TEMP_TEST_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "vo-analysis-workspace-extension-snapshot-{}-{id}",
            std::process::id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("vo.mod"), "this live module file is invalid\n").unwrap();

        let module = "github.com/acme/workspace-lib";
        let extension_name = "captured_workspace_source";
        let workspace_fs = MemoryFs::new()
            .with_file(
                root.join("vo.mod"),
                format!("module = \"{module}\"\nvo = \"^0.1.0\"\n\n[extension]\nname = \"{extension_name}\"\n\n[extension.web]\n"),
            )
            .with_file(root.join("workspace_lib.vo"), "package workspace_lib\n");
        let resolver = WorkspaceSourceResolver::with_fs(
            PackageResolver::with_fs(MemoryFs::new()),
            workspace_fs,
            HashMap::from([(module.to_string(), root.clone())]),
        );

        let package = resolver
            .resolve(module)
            .expect("captured workspace metadata should parse")
            .expect("workspace package should resolve");
        assert_eq!(package.path(), module);
        assert_eq!(package.abi_path(), package_abi_path(module));
        let extension = package.extension().expect("typed extension metadata");
        assert_eq!(extension.name, extension_name);
        assert_eq!(extension.manifest_path, root.join("vo.mod"));

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn test_resolve_propagates_invalid_extension_manifest() {
        let fs = MemoryFs::new()
            .with_file("github.com/acme/game/game.vo", "package game\n")
            .with_file(
                "github.com/acme/game/vo.mod",
                concat!(
                    "module = \"github.com/acme/game\"\n",
                    "vo = \"0.1.0\"\n\n",
                    "[extension]\n",
                    "name = \"game\"\n",
                    "path = \"rust/target/release/libgame\"\n",
                ),
            );

        let resolver = PackageResolver::with_fs(fs);
        let error = resolver.resolve("github.com/acme/game").unwrap_err();
        assert!(error.contains("vo.mod"));
        assert!(error.contains("unsupported key(s) in [extension]"));
        assert!(error.contains("path"));
    }
}
