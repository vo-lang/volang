use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use vo_common::vfs::{MemoryFs, RealFs, ScopedFs};
use vo_common_core::LogRecordCore;
use vo_module::ext_manifest::ExtensionManifest;
use vo_module::operation_error::OperationError;
use vo_module::project::{
    ProjectContext, ProjectContextOptions, ProjectDeps, ProjectDepsError, ProjectDepsErrorKind,
    ProjectDepsStage, SingleFileContext,
};
use vo_module::registry::Registry;
use vo_module::schema::lockfile::LockedModule;
use vo_module::workspace::WorkspaceDiscovery;
use vo_runtime::ext_loader::NativeExtensionSpec;
use vo_stdlib::EmbeddedStdlib;

mod cache;
mod native;
mod pipeline;
mod snapshot;

#[cfg(test)]
mod tests;

const MOD_CACHE_DIR: &str = ".vo/mod";
const COMPILE_CACHE_SCHEMA_VERSION: &str = "7";
const COMPILE_CACHE_SLOT_NAMESPACE: &str = "vo-compile-cache-slot";
const COMPILE_CACHE_NATIVE_NAMESPACE: &str = "vo-compile-cache-native";

pub type PreparedNativeExtension = NativeExtensionSpec;

pub type CompileLogRecord = LogRecordCore;

type CompileLogSink = dyn Fn(CompileLogRecord) + Send + Sync;

thread_local! {
    static COMPILE_LOG_SINK: RefCell<Option<Arc<CompileLogSink>>> = RefCell::new(None);
}

#[cfg(test)]
thread_local! {
    static MOD_CACHE_ROOT_OVERRIDE: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
}

pub fn with_compile_log_sink<T, S, F>(sink: S, f: F) -> T
where
    S: Fn(CompileLogRecord) + Send + Sync + 'static,
    F: FnOnce() -> T,
{
    struct RestoreCompileLogSink(Option<Arc<CompileLogSink>>);

    impl Drop for RestoreCompileLogSink {
        fn drop(&mut self) {
            COMPILE_LOG_SINK.with(|slot| {
                *slot.borrow_mut() = self.0.take();
            });
        }
    }

    let previous = COMPILE_LOG_SINK.with(|slot| slot.borrow_mut().replace(Arc::new(sink)));
    let _restore = RestoreCompileLogSink(previous);
    f()
}

pub(super) fn emit_compile_log(record: CompileLogRecord) {
    COMPILE_LOG_SINK.with(|slot| {
        if let Some(sink) = slot.borrow().as_ref() {
            sink(record);
        }
    });
}

#[cfg(test)]
fn with_mod_cache_root_override<T, F>(root: &Path, f: F) -> T
where
    F: FnOnce() -> T,
{
    struct RestoreModCacheRoot(Option<PathBuf>);

    impl Drop for RestoreModCacheRoot {
        fn drop(&mut self) {
            MOD_CACHE_ROOT_OVERRIDE.with(|slot| {
                *slot.borrow_mut() = self.0.take();
            });
        }
    }

    let previous =
        MOD_CACHE_ROOT_OVERRIDE.with(|slot| slot.borrow_mut().replace(root.to_path_buf()));
    let _restore = RestoreModCacheRoot(previous);
    f()
}

#[derive(Debug)]
pub enum CompileError {
    Io(std::io::Error),
    Parse(String),
    Analysis(String),
    Codegen(String),
    ModuleSystem(ModuleSystemError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleSystemStage {
    Workspace,
    ModFile,
    LockFile,
    DependencyDownload,
    CachedModule,
    NativeExtension,
}

impl ModuleSystemStage {
    pub fn as_str(self) -> &'static str {
        match self {
            ModuleSystemStage::Workspace => "workspace",
            ModuleSystemStage::ModFile => "mod_file",
            ModuleSystemStage::LockFile => "lock_file",
            ModuleSystemStage::DependencyDownload => "dependency_download",
            ModuleSystemStage::CachedModule => "cached_module",
            ModuleSystemStage::NativeExtension => "native_extension",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleSystemErrorKind {
    Missing,
    ReadFailed,
    ParseFailed,
    ValidationFailed,
    DownloadFailed,
    BuildFailed,
    VerificationFailed,
    Mismatch,
    MissingMetadata,
}

impl ModuleSystemErrorKind {
    pub fn as_str(self) -> &'static str {
        match self {
            ModuleSystemErrorKind::Missing => "missing",
            ModuleSystemErrorKind::ReadFailed => "read_failed",
            ModuleSystemErrorKind::ParseFailed => "parse_failed",
            ModuleSystemErrorKind::ValidationFailed => "validation_failed",
            ModuleSystemErrorKind::DownloadFailed => "download_failed",
            ModuleSystemErrorKind::BuildFailed => "build_failed",
            ModuleSystemErrorKind::VerificationFailed => "verification_failed",
            ModuleSystemErrorKind::Mismatch => "mismatch",
            ModuleSystemErrorKind::MissingMetadata => "missing_metadata",
        }
    }
}

pub type ModuleSystemError = OperationError<ModuleSystemStage, ModuleSystemErrorKind>;

pub(super) fn module_system_error_from_project(error: ProjectDepsError) -> ModuleSystemError {
    ModuleSystemError::from_other(error, project_stage, project_kind)
}

fn project_stage(stage: ProjectDepsStage) -> ModuleSystemStage {
    match stage {
        ProjectDepsStage::Workspace => ModuleSystemStage::Workspace,
        ProjectDepsStage::ModFile => ModuleSystemStage::ModFile,
        ProjectDepsStage::LockFile => ModuleSystemStage::LockFile,
    }
}

fn project_kind(kind: ProjectDepsErrorKind) -> ModuleSystemErrorKind {
    match kind {
        ProjectDepsErrorKind::Missing => ModuleSystemErrorKind::Missing,
        ProjectDepsErrorKind::ReadFailed => ModuleSystemErrorKind::ReadFailed,
        ProjectDepsErrorKind::ParseFailed => ModuleSystemErrorKind::ParseFailed,
        ProjectDepsErrorKind::ValidationFailed => ModuleSystemErrorKind::ValidationFailed,
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Io(e) => write!(f, "IO error: {}", e),
            CompileError::Parse(e) => write!(f, "Parse error: {}", e),
            CompileError::Analysis(e) => write!(f, "Analysis error: {}", e),
            CompileError::Codegen(e) => write!(f, "Codegen error: {}", e),
            CompileError::ModuleSystem(e) => write!(f, "Module system error: {}", e),
        }
    }
}

impl std::error::Error for CompileError {}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::Io(e)
    }
}

impl From<ModuleSystemError> for CompileError {
    fn from(e: ModuleSystemError) -> Self {
        CompileError::ModuleSystem(e)
    }
}

impl CompileError {
    pub fn category(&self) -> &'static str {
        match self {
            CompileError::Io(_) => "io",
            CompileError::Parse(_) => "parse",
            CompileError::Analysis(_) => "analysis",
            CompileError::Codegen(_) => "codegen",
            CompileError::ModuleSystem(_) => "module-system",
        }
    }

    pub fn module_system(&self) -> Option<&ModuleSystemError> {
        match self {
            CompileError::ModuleSystem(error) => Some(error),
            _ => None,
        }
    }
}

pub type CompileOutput = vo_stdlib::toolchain::ToolchainModule;

#[derive(Debug, Clone)]
pub(super) struct WorkspaceCompileContext {
    pub(super) options: ProjectContextOptions,
    pub(super) file: Option<PathBuf>,
}

impl WorkspaceCompileContext {
    fn from_project(context: &ProjectContext, options: &ProjectContextOptions) -> Self {
        let file = context
            .workspace_file()
            .map(|path| path.canonicalize().unwrap_or_else(|_| path.to_path_buf()));
        let mut options = options.clone();
        if matches!(options.workspace, WorkspaceDiscovery::Explicit(_)) {
            if let Some(path) = file.as_ref() {
                options.workspace = WorkspaceDiscovery::Explicit(path.clone());
            }
        }
        Self { options, file }
    }

    pub(super) fn disabled() -> Self {
        Self {
            options: ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            file: None,
        }
    }
}

#[derive(Clone)]
struct RealPathCompileContext {
    source_root: PathBuf,
    project_root: PathBuf,
    mod_cache: PathBuf,
    package_dir: PathBuf,
    single_file: Option<PathBuf>,
    project_deps: ProjectDeps,
    current_module_override: Option<String>,
    workspace_replaces: HashMap<String, PathBuf>,
    workspace: WorkspaceCompileContext,
    module_cache_read_lease: Option<Arc<vo_module::cache::CacheReadLease>>,
}

impl RealPathCompileContext {
    fn compile_input_capture<'a>(
        &'a self,
        stdlib_source_fingerprint: &'a str,
    ) -> cache::CompileInputCapture<'a> {
        cache::CompileInputCapture {
            source_root: &self.source_root,
            project_root: &self.project_root,
            mod_cache: &self.mod_cache,
            single_file: self.single_file.as_deref(),
            project_deps: &self.project_deps,
            replaces: &self.workspace_replaces,
            workspace_options: &self.workspace.options,
            stdlib_source_fingerprint,
        }
    }

    fn into_pipeline_context(self) -> pipeline::ProjectCompileContext {
        pipeline::ProjectCompileContext {
            project_root: self.project_root,
            mod_cache: self.mod_cache,
            source_root: self.source_root,
            package_dir: self.package_dir,
            single_file: self.single_file,
            project_deps: self.project_deps,
            current_module_override: self.current_module_override,
            replaces: self.workspace_replaces,
            workspace: self.workspace,
        }
    }

    fn acquire_module_cache_read_lease(
        &self,
    ) -> Result<Option<Arc<vo_module::cache::CacheReadLease>>, ModuleSystemError> {
        if let Some(lease) = self.module_cache_read_lease.as_ref() {
            return Ok(Some(Arc::clone(lease)));
        }
        acquire_module_cache_read_lease(&self.mod_cache, self.project_deps.locked_modules())
    }
}

fn relative_package_dir(project_root: &Path, source_root: &Path) -> PathBuf {
    match source_root.strip_prefix(project_root) {
        Ok(relative) if relative.as_os_str().is_empty() => PathBuf::from("."),
        Ok(relative) => relative.to_path_buf(),
        Err(_) => PathBuf::from("."),
    }
}

fn relative_single_file_path(package_dir: &Path, entry_path: &Path) -> Option<PathBuf> {
    let file_name = entry_path.file_name()?;
    if package_dir == Path::new(".") {
        Some(PathBuf::from(file_name))
    } else {
        Some(package_dir.join(file_name))
    }
}

fn canonicalize_workspace_replaces(replaces: &mut HashMap<String, PathBuf>) {
    for root in replaces.values_mut() {
        *root = root.canonicalize().unwrap_or_else(|_| root.clone());
    }
}

fn existing_path_is_within_managed_cache(
    path: &Path,
    mod_cache: &Path,
) -> Result<bool, ModuleSystemError> {
    let cache_root = match fs::canonicalize(mod_cache) {
        Ok(path) => path,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(error) => {
            return Err(ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::ReadFailed,
                format!(
                    "failed to resolve managed module cache {}: {error}",
                    mod_cache.display(),
                ),
            )
            .with_path(mod_cache));
        }
    };
    let resolved = fs::canonicalize(path).map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::CachedModule,
            ModuleSystemErrorKind::ReadFailed,
            format!(
                "failed to resolve path {} while checking module-cache ownership: {error}",
                path.display(),
            ),
        )
        .with_path(path)
    })?;
    Ok(resolved.starts_with(cache_root))
}

fn acquire_existing_module_cache_read_lease_for_path(
    mod_cache: &Path,
    path: &Path,
) -> Result<Option<Arc<vo_module::cache::CacheReadLease>>, ModuleSystemError> {
    if !existing_path_is_within_managed_cache(path, mod_cache)? {
        return Ok(None);
    }
    vo_module::cache::acquire_read_lease(mod_cache)
        .map(Arc::new)
        .map(Some)
        .map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::ReadFailed,
                format!(
                    "failed to acquire a read lease for module cache {}: {error}",
                    mod_cache.display(),
                ),
            )
            .with_path(mod_cache)
        })
}

fn reject_workspace_replaces_in_managed_cache(
    replaces: &HashMap<String, PathBuf>,
    mod_cache: &Path,
) -> Result<(), ModuleSystemError> {
    for (module, root) in replaces {
        if existing_path_is_within_managed_cache(root, mod_cache)? {
            return Err(ModuleSystemError::new(
                ModuleSystemStage::Workspace,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "workspace override for {module} points inside the managed module cache {}; use a source directory outside the cache",
                    mod_cache.display(),
                ),
            )
            .with_path(root));
        }
    }
    Ok(())
}

fn load_real_path_compile_context_with_options(
    path: &Path,
    options: &ProjectContextOptions,
) -> Result<RealPathCompileContext, CompileError> {
    let source_root = pipeline::source_root(path);
    let mod_cache = default_mod_cache_root();
    // A project opened from the managed cache must hold the cache-wide read
    // lease before its first metadata or source read. Merely probing an
    // unrelated no-dependency project never creates the cache.
    let module_cache_read_lease =
        acquire_existing_module_cache_read_lease_for_path(&mod_cache, &source_root)?;
    let base_fs = RealFs::new(".");

    // Single-file entries go through the spec §5.6 single-file classifier so
    // that inline `/*vo:mod ... */` metadata is recognized and the spec §5.6.4
    // precedence rules are enforced uniformly for real-path compiles.
    if path.is_file() {
        let ctx =
            vo_module::project::load_single_file_context_with_options(&base_fs, path, options)
                .map_err(module_system_error_from_project)?;
        return real_path_compile_context_for_single_file(
            ctx,
            path,
            source_root,
            mod_cache,
            options,
            module_cache_read_lease,
        );
    }

    let context =
        vo_module::project::load_project_context_with_options(&base_fs, &source_root, options)
            .map_err(module_system_error_from_project)?;
    let project_root = context.project_root().to_path_buf();
    let package_dir = relative_package_dir(&project_root, &source_root);
    let workspace = WorkspaceCompileContext::from_project(&context, options);
    let (_, project_deps, mut workspace_replaces) = context.into_parts();
    canonicalize_workspace_replaces(&mut workspace_replaces);
    reject_workspace_replaces_in_managed_cache(&workspace_replaces, &mod_cache)?;
    Ok(RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file: None,
        project_deps,
        current_module_override: None,
        workspace_replaces,
        workspace,
        module_cache_read_lease,
    })
}

fn real_path_compile_context_for_single_file(
    ctx: SingleFileContext,
    path: &Path,
    source_root: PathBuf,
    mod_cache: PathBuf,
    options: &ProjectContextOptions,
    module_cache_read_lease: Option<Arc<vo_module::cache::CacheReadLease>>,
) -> Result<RealPathCompileContext, CompileError> {
    match ctx {
        SingleFileContext::Project(project_context) => real_path_context_from_project_context(
            project_context,
            path,
            source_root,
            mod_cache,
            options,
            module_cache_read_lease,
        ),
        SingleFileContext::EphemeralInlineMod {
            file_name,
            inline_mod,
            ..
        } => {
            let current_module = inline_mod.module.as_str().to_string();
            // Spec §10.2, §5.6.6: ephemeral single-file modules run in strict
            // isolation — no workspace overrides, no ancestor vo.mod/vo.lock.
            //
            // When the inline block has no `require` entries, the build sees
            // only the stdlib and the compile context is trivial. When it
            // does have require entries, a prior call to
            // `ensure_ephemeral_deps_installed` should have materialized a
            // cache-local `vo.mod`/`vo.lock` pair under
            // `<mod_cache>/ephemeral/<hash>/`; we load it read-only here so
            // the normal project-deps pipeline can take over.
            if inline_mod.require.is_empty() {
                return Ok(RealPathCompileContext {
                    source_root: source_root.clone(),
                    project_root: source_root,
                    mod_cache,
                    package_dir: PathBuf::from("."),
                    single_file: Some(file_name),
                    project_deps: ProjectDeps::default(),
                    current_module_override: Some(current_module),
                    workspace_replaces: HashMap::new(),
                    workspace: WorkspaceCompileContext::disabled(),
                    module_cache_read_lease,
                });
            }

            let module_cache_read_lease = match module_cache_read_lease {
                Some(lease) => Some(lease),
                None => acquire_existing_module_cache_read_lease_for_path(&mod_cache, &mod_cache)?,
            };
            let cached = vo_module::ephemeral::load_cached_ephemeral(&mod_cache, &inline_mod)
                .map_err(|error| {
                    ModuleSystemError::new(
                        ModuleSystemStage::LockFile,
                        ModuleSystemErrorKind::Missing,
                        format!("ephemeral cache read error: {}", error),
                    )
                })?
                .ok_or_else(|| {
                    ModuleSystemError::new(
                        ModuleSystemStage::LockFile,
                        ModuleSystemErrorKind::Missing,
                        format!(
                            "ephemeral dependencies for '{}' not yet resolved; run this file \
                             via `vo run` (auto-install) or `vo check --install`",
                            inline_mod.module,
                        ),
                    )
                })?;

            let base_fs = RealFs::new(".");
            let isolated_options = ProjectContextOptions::new(WorkspaceDiscovery::Disabled);
            let context = vo_module::project::load_project_context_with_options(
                &base_fs,
                &cached.cache_dir,
                &isolated_options,
            )
            .map_err(module_system_error_from_project)?;
            let (_, project_deps, _) = context.into_parts();
            Ok(RealPathCompileContext {
                source_root: source_root.clone(),
                project_root: source_root,
                mod_cache,
                package_dir: PathBuf::from("."),
                single_file: Some(file_name),
                project_deps,
                current_module_override: Some(current_module),
                // Ephemeral single-file modules cannot declare `replace`
                // (spec §5.6.3) and must not consult ancestor `vo.work`
                // (spec §10.1), so workspace overrides are always empty.
                workspace_replaces: HashMap::new(),
                workspace: WorkspaceCompileContext::disabled(),
                module_cache_read_lease,
            })
        }
        SingleFileContext::AdHoc { file_name, .. } => {
            // Spec §10.1: ad hoc programs see only the stdlib. No ancestor
            // `vo.mod`, no ancestor `vo.work`, no workspace overrides. The
            // single-file classifier has already enforced the spec §5.6.4
            // precedence rule (rejecting inline mod inside a project).
            Ok(RealPathCompileContext {
                source_root: source_root.clone(),
                project_root: source_root,
                mod_cache,
                package_dir: PathBuf::from("."),
                single_file: Some(file_name),
                project_deps: ProjectDeps::default(),
                current_module_override: None,
                workspace_replaces: HashMap::new(),
                workspace: WorkspaceCompileContext::disabled(),
                module_cache_read_lease,
            })
        }
    }
}

fn real_path_context_from_project_context(
    context: ProjectContext,
    path: &Path,
    source_root: PathBuf,
    mod_cache: PathBuf,
    options: &ProjectContextOptions,
    module_cache_read_lease: Option<Arc<vo_module::cache::CacheReadLease>>,
) -> Result<RealPathCompileContext, CompileError> {
    let workspace = WorkspaceCompileContext::from_project(&context, options);
    let (project_root_raw, project_deps, mut workspace_replaces) = context.into_parts();
    canonicalize_workspace_replaces(&mut workspace_replaces);
    reject_workspace_replaces_in_managed_cache(&workspace_replaces, &mod_cache)?;
    // `source_root` is canonicalized by `pipeline::source_root`; ensure
    // `project_root` matches the same canonical form so that
    // `relative_package_dir` can strip the prefix reliably across
    // platforms with symlinked temp dirs (e.g. `/var` vs `/private/var`).
    let project_root = project_root_raw.canonicalize().unwrap_or(project_root_raw);
    let package_dir = relative_package_dir(&project_root, &source_root);
    let single_file = relative_single_file_path(&package_dir, path);
    Ok(RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file,
        project_deps,
        current_module_override: None,
        workspace_replaces,
        workspace,
        module_cache_read_lease,
    })
}

fn scoped_project_fs(project_root: &Path) -> ScopedFs<RealFs> {
    ScopedFs::new(RealFs::new("."), project_root)
}

pub fn check(path: &str) -> Result<(), CompileError> {
    check_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn check_with_options(path: &str, options: &ProjectContextOptions) -> Result<(), CompileError> {
    check_path_with_options(Path::new(path), options)
}

pub fn check_path(path: &Path) -> Result<(), CompileError> {
    check_path_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn check_path_with_options(
    path: &Path,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    let context = load_real_path_compile_context_with_options(path, options)?;
    let _cache_lease = context.acquire_module_cache_read_lease()?;
    let fs = scoped_project_fs(&context.project_root);
    pipeline::check_with_project_context(fs, context.into_pipeline_context())
}

pub fn compile(path: &str) -> Result<CompileOutput, CompileError> {
    compile_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn compile_path(path: &Path) -> Result<CompileOutput, CompileError> {
    compile_path_with_options(path, &ProjectContextOptions::from_environment())
}

fn compile_path_with_options(
    path: &Path,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    if let Some(text) = path.to_str() {
        return compile_with_options(text, options);
    }
    if matches!(
        path.extension(),
        Some(extension) if extension == std::ffi::OsStr::new("voc")
            || extension == std::ffi::OsStr::new("vob")
    ) {
        return pipeline::load_bytecode(path);
    }

    let context = load_real_path_compile_context_with_options(path, options)?;
    let cache_lease = context.acquire_module_cache_read_lease()?;
    let mod_cache = context.mod_cache.clone();
    let fs = scoped_project_fs(&context.project_root);
    let mut output = pipeline::compile_with_project_context(fs, context.into_pipeline_context())?;
    retain_module_cache_lease(&mut output, &mod_cache, cache_lease);
    Ok(output)
}

pub fn compile_path_with_auto_install(path: &Path) -> Result<CompileOutput, CompileError> {
    if let Some(text) = path.to_str() {
        return compile_with_auto_install(text);
    }

    use vo_module::github_registry::GitHubRegistry;

    let options = ProjectContextOptions::from_environment();
    let registry = GitHubRegistry::new();
    let mod_cache = default_mod_cache_root();
    auto_install_dependencies(path, &mod_cache, &registry, &options)?;
    compile_path_with_cache_with_options(path, &options)
}

pub fn compile_with_options(
    path: &str,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);

    if path.ends_with(".voc") || path.ends_with(".vob") {
        return pipeline::load_bytecode(p);
    }

    if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
        return pipeline::compile_zip(Path::new(&zip_path), internal_root.as_deref());
    }

    let context = load_real_path_compile_context_with_options(p, options)?;
    let cache_lease = context.acquire_module_cache_read_lease()?;
    let mod_cache = context.mod_cache.clone();
    let fs = scoped_project_fs(&context.project_root);
    let mut output = pipeline::compile_with_project_context(fs, context.into_pipeline_context())?;
    retain_module_cache_lease(&mut output, &mod_cache, cache_lease);
    Ok(output)
}

pub fn compile_with_cache(path: &str) -> Result<CompileOutput, CompileError> {
    compile_with_cache_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn compile_with_cache_with_options(
    path: &str,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    compile_path_with_cache_with_options(Path::new(path), options)
}

fn compile_path_with_cache_with_options(
    entry_path: &Path,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    if matches!(
        entry_path.extension(),
        Some(extension) if extension == std::ffi::OsStr::new("voc")
            || extension == std::ffi::OsStr::new("vob")
    ) {
        return pipeline::load_bytecode(entry_path);
    }
    if let Some(path) = entry_path.to_str() {
        if pipeline::parse_zip_path(path).is_some() {
            return compile_with_options(path, options);
        }
    }
    let mut context = load_real_path_compile_context_with_options(entry_path, options)?;
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());
    for replace_root in context.workspace_replaces.values_mut() {
        *replace_root = replace_root
            .canonicalize()
            .unwrap_or_else(|_| replace_root.clone());
    }
    let cache_lease = context.acquire_module_cache_read_lease()?;
    let cache_slot = cache::compile_cache_slot(
        &context.source_root,
        context.single_file.as_deref().and_then(Path::file_name),
    );
    let (stdlib_snapshot, stdlib_source_fingerprint) = stdlib_compile_cache_input();
    let captured_inputs =
        cache::capture_compile_inputs(context.compile_input_capture(&stdlib_source_fingerprint))?;
    let captured_context_fs = snapshot::ResolverFs::snapshot_global(captured_inputs.snapshot());
    pipeline::validate_captured_project_context(
        &captured_context_fs,
        &context.project_root,
        &context.project_deps,
        &context.workspace_replaces,
        context.current_module_override.as_deref(),
        &context.workspace,
    )?;
    let captured_module_fs =
        snapshot::ResolverFs::snapshot(captured_inputs.snapshot(), &context.mod_cache);
    native::check_materialized_dependency_readiness_with_fs(
        &captured_module_fs,
        context.project_deps.locked_modules(),
    )
    .map_err(CompileError::ModuleSystem)?;
    let fingerprint = captured_inputs.fingerprint().to_string();

    if let Some(mut output) = cache::try_load_cache_with_options(
        &cache_slot,
        &context.source_root,
        &fingerprint,
        &context.workspace.options,
    ) {
        // The cache may persist a structurally older or independently damaged
        // copy of lock metadata. The current project context has already
        // parsed and validated the authoritative root lock and participates in
        // the cache fingerprint, so expose that exact value to callers.
        output.locked_modules = context.project_deps.locked_modules().to_vec();
        retain_module_cache_lease(&mut output, &context.mod_cache, cache_lease);
        emit_compile_log(
            CompileLogRecord::new("vo-engine", "compile_cache_hit")
                .path(compile_log_path(entry_path)),
        );
        return Ok(output);
    }

    let stdlib = stdlib_snapshot.unwrap_or_default();
    let snapshot = captured_inputs.into_snapshot();
    let post_compile_context = context.clone();
    let pipeline_context = context.into_pipeline_context();
    let mut output = pipeline::compile_with_project_snapshot(pipeline_context, stdlib, snapshot)?;

    // Dependency artifacts are returned as immutable cache paths. Validate
    // the live paths once more before exposing them to the VM so a concurrent
    // module-cache mutation cannot bypass the captured readiness check.
    native::check_materialized_dependency_readiness(
        post_compile_context.project_deps.locked_modules(),
        &post_compile_context.mod_cache,
    )
    .map_err(CompileError::ModuleSystem)?;

    // Local native extensions are built by Cargo from their live source tree.
    // Publish only while every captured input still has the same identity;
    // source analysis itself always used the immutable snapshot above.
    let post_compile_inputs = cache::capture_compile_inputs(
        post_compile_context.compile_input_capture(&stdlib_source_fingerprint),
    );
    let recaptured_fingerprint = post_compile_inputs
        .as_ref()
        .ok()
        .map(|post| post.fingerprint());
    ensure_native_output_generation_is_current(
        !output.extensions.is_empty(),
        &fingerprint,
        recaptured_fingerprint,
    )?;
    retain_module_cache_lease(&mut output, &post_compile_context.mod_cache, cache_lease);
    if post_compile_inputs
        .as_ref()
        .is_ok_and(|post| post.fingerprint() == fingerprint)
    {
        cache::save_compile_cache(&cache_slot, &fingerprint, &output);
        emit_compile_log(
            CompileLogRecord::new("vo-engine", "compile_cache_store")
                .path(compile_log_path(entry_path)),
        );
    }

    Ok(output)
}

fn ensure_native_output_generation_is_current(
    has_native_extensions: bool,
    captured_fingerprint: &str,
    recaptured_fingerprint: Option<&str>,
) -> Result<(), CompileError> {
    if !has_native_extensions || recaptured_fingerprint == Some(captured_fingerprint) {
        return Ok(());
    }
    Err(CompileError::ModuleSystem(ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::Mismatch,
        "compile inputs changed while native extensions were prepared; retry the build so source analysis and native artifacts come from one generation",
    )))
}

/// Prepares the stdlib identity used by the native compile cache.
///
/// Debug builds retain the live source-tree snapshot so a cache miss analyzes
/// exactly the bytes that produced the fingerprint. Release assets are
/// immutable, so cache hits can reuse a process-wide identity without loading
/// the full source map.
fn stdlib_compile_cache_input() -> (Option<EmbeddedStdlib>, Cow<'static, str>) {
    #[cfg(debug_assertions)]
    {
        stdlib_compile_cache_input_with(EmbeddedStdlib::new)
    }

    #[cfg(not(debug_assertions))]
    {
        (
            None,
            Cow::Borrowed(EmbeddedStdlib::immutable_source_fingerprint()),
        )
    }
}

#[cfg(debug_assertions)]
fn stdlib_compile_cache_input_with(
    load: impl FnOnce() -> EmbeddedStdlib,
) -> (Option<EmbeddedStdlib>, Cow<'static, str>) {
    let stdlib = load();
    let fingerprint = stdlib.source_fingerprint();
    (Some(stdlib), Cow::Owned(fingerprint))
}

fn compile_log_path(path: &Path) -> String {
    path.to_str()
        .map(str::to_owned)
        .unwrap_or_else(|| format!("{path:?}"))
}

pub fn compile_from_memory(fs: MemoryFs, root: &Path) -> Result<CompileOutput, CompileError> {
    pipeline::compile_prepared_project(fs, root, None)
}

pub fn compile_source_at(source: &str, root: &Path) -> Result<CompileOutput, CompileError> {
    let mut mem = MemoryFs::new();
    mem.add_file("main.vo", source);
    pipeline::compile_prepared_project(mem, root, Some(std::ffi::OsStr::new("main.vo")))
}

pub fn compile_string(code: &str) -> Result<CompileOutput, CompileError> {
    let temp_dir = std::env::temp_dir().join("vo_compile");
    fs::create_dir_all(&temp_dir)?;
    compile_source_at(code, &temp_dir)
}

pub fn compile_with_auto_install(path: &str) -> Result<CompileOutput, CompileError> {
    compile_with_auto_install_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn compile_with_auto_install_with_options(
    path: &str,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    use vo_module::github_registry::GitHubRegistry;

    let registry = GitHubRegistry::new();
    compile_with_auto_install_using_registry(path, &registry, options)
}

pub fn check_with_auto_install(path: &str) -> Result<(), CompileError> {
    check_with_auto_install_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn check_with_auto_install_with_options(
    path: &str,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    check_path_with_auto_install_with_options(Path::new(path), options)
}

pub fn check_path_with_auto_install(path: &Path) -> Result<(), CompileError> {
    check_path_with_auto_install_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn check_path_with_auto_install_with_options(
    path: &Path,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    use vo_module::github_registry::GitHubRegistry;

    let registry = GitHubRegistry::new();
    check_path_with_auto_install_using_registry(path, &registry, options)
}

fn compile_with_auto_install_using_registry(
    path: &str,
    registry: &dyn Registry,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);
    let mod_cache = default_mod_cache_root();
    auto_install_dependencies(p, &mod_cache, registry, options)?;
    compile_with_cache_with_options(path, options)
}

fn check_path_with_auto_install_using_registry(
    path: &Path,
    registry: &dyn Registry,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    let mod_cache = default_mod_cache_root();
    auto_install_dependencies(path, &mod_cache, registry, options)?;
    check_path_with_options(path, options)
}

/// Pre-flight ephemeral dependency resolution (spec §5.6, §10.2).
///
/// When `path` is a single file whose leading `/*vo:mod*/` block declares
/// `require` entries, this runs the solver, writes a canonical
/// `vo.mod`/`vo.lock` pair under `<mod_cache>/ephemeral/<hash>/`, and
/// populates the shared module cache with the resolved dependencies — all
/// idempotent, so a second invocation with the same inline body is a no-op.
/// For any other input (projects, ad hoc files, inline mods with no
/// require entries), this is a no-op.
fn auto_install_dependencies(
    path: &Path,
    mod_cache: &Path,
    registry: &dyn Registry,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    let source_root = pipeline::source_root(path);
    let _module_cache_read_lease =
        acquire_existing_module_cache_read_lease_for_path(mod_cache, &source_root)?;
    if path.is_file() {
        let ctx = vo_module::project::load_single_file_context_with_options(
            &RealFs::new("."),
            path,
            options,
        )
        .map_err(module_system_error_from_project)?;
        return match ctx {
            SingleFileContext::Project(project_context) => {
                let (_, project_deps, mut workspace_replaces) = project_context.into_parts();
                canonicalize_workspace_replaces(&mut workspace_replaces);
                reject_workspace_replaces_in_managed_cache(&workspace_replaces, mod_cache)?;
                auto_download_project_deps(&project_deps, mod_cache, registry)
            }
            SingleFileContext::EphemeralInlineMod { inline_mod, .. } => {
                if inline_mod.require.is_empty() {
                    return Ok(());
                }
                vo_module::ephemeral::resolve_and_cache_ephemeral(
                    mod_cache,
                    &inline_mod,
                    registry,
                    "vo compile",
                )
                .map_err(|error| {
                    ModuleSystemError::new(
                        ModuleSystemStage::DependencyDownload,
                        ModuleSystemErrorKind::DownloadFailed,
                        format!("ephemeral dependency resolution failed for {path:?}: {error}"),
                    )
                    .with_path(path)
                })?;
                Ok(())
            }
            SingleFileContext::AdHoc { .. } => Ok(()),
        };
    }

    let context = vo_module::project::load_project_context_with_options(
        &RealFs::new("."),
        &source_root,
        options,
    )
    .map_err(module_system_error_from_project)?;
    let (_, project_deps, mut workspace_replaces) = context.into_parts();
    canonicalize_workspace_replaces(&mut workspace_replaces);
    reject_workspace_replaces_in_managed_cache(&workspace_replaces, mod_cache)?;
    auto_download_project_deps(&project_deps, mod_cache, registry)
}

fn auto_download_project_deps(
    project_deps: &ProjectDeps,
    mod_cache: &Path,
    registry: &dyn Registry,
) -> Result<(), CompileError> {
    if !project_deps.has_mod_file() || project_deps.locked_modules().is_empty() {
        return Ok(());
    }
    let cache_fs = RealFs::new(mod_cache);
    let dependency_state = project_deps
        .locked_modules()
        .iter()
        .map(|locked| {
            let module_dir =
                vo_module::cache::layout::relative_module_dir(&locked.path, &locked.version);
            let cached = vo_module::cache::validate::validate_installed_module(
                &cache_fs,
                &module_dir,
                locked,
            )
            .is_ok()
                && locked.artifacts.iter().all(|artifact| {
                    vo_module::cache::validate::validate_installed_artifact(
                        &cache_fs,
                        &module_dir,
                        locked,
                        &artifact.id,
                    )
                    .is_ok()
                });
            (
                locked.path.as_str().to_string(),
                locked.version.to_string(),
                cached,
            )
        })
        .collect::<Vec<_>>();

    for (module, version, cached) in &dependency_state {
        if *cached {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "dependency_cached")
                    .module(module)
                    .version(version),
            );
        } else {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "dependency_fetch_start")
                    .module(module)
                    .version(version),
            );
        }
    }

    vo_module::lifecycle::download_materialized_dependencies(
        mod_cache,
        project_deps.locked_modules(),
        registry,
    )
    .map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::DependencyDownload,
            ModuleSystemErrorKind::DownloadFailed,
            format!("failed to download dependencies: {}", e),
        )
        .with_path(mod_cache)
    })?;

    for (module, version, cached) in dependency_state {
        if !cached {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "dependency_fetch_done")
                    .module(module)
                    .version(version),
            );
        }
    }

    Ok(())
}

pub fn default_mod_cache_root() -> PathBuf {
    #[cfg(test)]
    if let Some(root) = MOD_CACHE_ROOT_OVERRIDE.with(|slot| slot.borrow().clone()) {
        return root;
    }
    if let Some(root) = std::env::var_os("VO_MOD_CACHE") {
        return PathBuf::from(root);
    }
    std::env::var_os("HOME")
        .map(PathBuf::from)
        .map(|home| home.join(MOD_CACHE_DIR))
        .unwrap_or_else(|| PathBuf::from(MOD_CACHE_DIR))
}

pub fn prepare_native_extension_specs(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
    mod_cache: &Path,
) -> Result<Vec<PreparedNativeExtension>, ModuleSystemError> {
    let cache_lease =
        acquire_native_extension_cache_read_lease(manifests, locked_modules, mod_cache)?;
    let mut specs = native::prepare_native_extension_specs_for_frozen_build(
        manifests,
        locked_modules,
        mod_cache,
    )?;
    retain_native_extension_cache_lease(&mut specs, mod_cache, cache_lease);
    Ok(specs)
}

fn acquire_native_extension_cache_read_lease(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
    mod_cache: &Path,
) -> Result<Option<Arc<vo_module::cache::CacheReadLease>>, ModuleSystemError> {
    if !locked_modules.is_empty() {
        return acquire_module_cache_read_lease(mod_cache, locked_modules);
    }
    for manifest in manifests {
        if let Some(lease) =
            acquire_existing_module_cache_read_lease_for_path(mod_cache, &manifest.manifest_path)?
        {
            return Ok(Some(lease));
        }
    }
    Ok(None)
}

fn acquire_module_cache_read_lease(
    mod_cache: &Path,
    locked_modules: &[LockedModule],
) -> Result<Option<Arc<vo_module::cache::CacheReadLease>>, ModuleSystemError> {
    if locked_modules.is_empty() {
        return Ok(None);
    }
    vo_module::cache::acquire_read_lease(mod_cache)
        .map(Arc::new)
        .map(Some)
        .map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::ReadFailed,
                format!(
                    "failed to acquire a read lease for module cache {}: {error}",
                    mod_cache.display(),
                ),
            )
            .with_path(mod_cache)
        })
}

fn retain_module_cache_lease(
    output: &mut CompileOutput,
    mod_cache: &Path,
    cache_lease: Option<Arc<vo_module::cache::CacheReadLease>>,
) {
    retain_native_extension_cache_lease(&mut output.extensions, mod_cache, cache_lease);
}

fn retain_native_extension_cache_lease(
    extensions: &mut [NativeExtensionSpec],
    mod_cache: &Path,
    cache_lease: Option<Arc<vo_module::cache::CacheReadLease>>,
) {
    let Some(cache_lease) = cache_lease else {
        return;
    };
    let cache_root = mod_cache
        .canonicalize()
        .unwrap_or_else(|_| mod_cache.to_path_buf());
    for extension in extensions {
        let native_path = extension
            .native_path
            .canonicalize()
            .unwrap_or_else(|_| extension.native_path.clone());
        if native_path.starts_with(&cache_root) {
            extension.retain_lifetime_resource(Arc::clone(&cache_lease));
        }
    }
}

#[cfg(test)]
mod cache_lease_tests {
    use super::*;
    use std::sync::mpsc;
    use std::time::Duration;

    #[test]
    fn cached_native_spec_holds_read_lease_until_last_clone_drops() {
        let root = std::env::temp_dir().join(format!(
            "vo-engine-cache-lease-{}-{:?}",
            std::process::id(),
            std::thread::current().id(),
        ));
        let _ = fs::remove_dir_all(&root);
        let lease = Arc::new(
            vo_module::cache::acquire_read_lease(&root).expect("initialize cache ownership"),
        );
        let cached_dir = root.join("github.com@acme@cached").join("v1.0.0");
        fs::create_dir_all(&cached_dir).expect("create cached module directory");
        let native_path = cached_dir.join("libcached.so");
        fs::write(&native_path, b"test native artifact").expect("write cached artifact");

        let mut specs = vec![NativeExtensionSpec::new(
            "cached",
            "github.com/acme/cached",
            native_path,
            cached_dir.join("vo.mod"),
        )];
        retain_native_extension_cache_lease(&mut specs, &root, Some(lease));
        let retained_clone = specs[0].clone();
        drop(specs);

        let (started_tx, started_rx) = mpsc::channel();
        let (done_tx, done_rx) = mpsc::channel();
        let clean_root = root.clone();
        let cleaner = std::thread::spawn(move || {
            started_tx.send(()).expect("announce cache clean");
            let result = vo_module::ops::mod_clean(&clean_root, &clean_root, false);
            done_tx.send(result).expect("report cache clean");
        });
        started_rx
            .recv_timeout(Duration::from_secs(2))
            .expect("cache cleaner must start");
        let premature = done_rx.recv_timeout(Duration::from_millis(100));
        let was_blocked = matches!(premature, Err(mpsc::RecvTimeoutError::Timeout));

        drop(retained_clone);
        let clean_result = match premature {
            Ok(result) => result,
            Err(mpsc::RecvTimeoutError::Timeout) => done_rx
                .recv_timeout(Duration::from_secs(5))
                .expect("cache clean must finish after the lease drops"),
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                panic!("cache cleaner disconnected before reporting its result")
            }
        };
        clean_result.expect("clean module cache");
        cleaner.join().expect("join cache cleaner");
        assert!(
            was_blocked,
            "cache clean completed while a spec held its lease"
        );
        fs::remove_dir_all(root).expect("remove cache lease test root");
    }
}
