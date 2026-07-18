use std::borrow::Cow;
#[cfg(test)]
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use vo_common::vfs::{MemoryFs, RealFs};
use vo_common_core::LogRecordCore;
use vo_module::ext_manifest::ExtensionManifest;
use vo_module::operation_error::OperationError;
use vo_module::project::{
    ProjectAuthority, ProjectContext, ProjectContextOptions, ProjectDeps, ProjectDepsError,
    ProjectDepsErrorKind, ProjectDepsStage, SingleFileContext, SingleFileSourceGeneration,
    WorkspaceModule,
};
use vo_module::registry::Registry;
use vo_module::schema::lockfile::LockedModule;
use vo_module::workspace::WorkspaceDiscovery;
use vo_runtime::ext_loader::NativeExtensionSpec;
use vo_stdlib::EmbeddedStdlib;

mod cache;
mod host_input;
mod native;
mod pipeline;
mod snapshot;

#[cfg(test)]
mod tests;

// The default is versioned independently of the package protocol. A new cache
// layout gets a fresh owned leaf instead of adopting or deleting legacy data.
const DEFAULT_MOD_CACHE_PARENT: &str = ".vo/mod";
const COMPILE_CACHE_SCHEMA_VERSION: &str = "10";
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
    static MOD_CACHE_ROOT_LOOKUPS: Cell<usize> = const { Cell::new(0) };
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

#[cfg(test)]
fn mod_cache_root_lookup_count() -> usize {
    MOD_CACHE_ROOT_LOOKUPS.with(Cell::get)
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
    CompileInputs,
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
            ModuleSystemStage::CompileInputs => "compile_inputs",
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
    pub(super) generation: String,
}

impl WorkspaceCompileContext {
    fn from_project(context: &ProjectContext, options: &ProjectContextOptions) -> Self {
        // Freeze the exact path spelling selected by the project-context
        // generation. Re-canonicalizing here can change case on a
        // case-insensitive filesystem, making the immutable snapshot reload
        // describe a different authority-input path than the graph captured
        // immediately above.
        let file = context.workspace_file().map(Path::to_path_buf);
        let mut options = options.clone();
        if matches!(options.workspace, WorkspaceDiscovery::Explicit(_)) {
            if let Some(path) = file.as_ref() {
                options.workspace = WorkspaceDiscovery::Explicit(path.clone());
            }
        }
        Self {
            options,
            file,
            generation: context.workspace_generation().to_string(),
        }
    }

    pub(super) fn disabled() -> Self {
        Self {
            options: ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            file: None,
            generation: String::new(),
        }
    }
}

#[derive(Clone)]
pub(super) struct ProjectGraphContext {
    pub(super) authority: ProjectAuthority,
    pub(super) workspace_modules: Vec<WorkspaceModule>,
    pub(super) project_metadata_generation: String,
    pub(super) validated_input_files: Vec<PathBuf>,
}

impl ProjectGraphContext {
    pub(super) fn from_project(context: &ProjectContext) -> Self {
        Self {
            authority: context.authority(),
            workspace_modules: context.workspace_modules().to_vec(),
            project_metadata_generation: context.project_metadata_generation().to_string(),
            validated_input_files: context.validated_input_files().to_vec(),
        }
    }

    pub(super) fn empty() -> Self {
        Self {
            authority: ProjectAuthority::Empty,
            workspace_modules: Vec::new(),
            project_metadata_generation: String::new(),
            validated_input_files: Vec::new(),
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
    single_file_source_generation: Option<SingleFileSourceGeneration>,
    graph: ProjectGraphContext,
    project_deps: ProjectDeps,
    current_module_override: Option<String>,
    workspace_sources: HashMap<String, PathBuf>,
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
            single_file_source_generation: self.single_file_source_generation.as_ref(),
            graph: &self.graph,
            project_deps: &self.project_deps,
            workspace_sources: &self.workspace_sources,
            workspace_options: &self.workspace.options,
            workspace_generation: &self.workspace.generation,
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
            graph: self.graph,
            project_deps: self.project_deps,
            current_module_override: self.current_module_override,
            workspace_sources: self.workspace_sources,
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

fn canonicalize_workspace_sources(workspace_sources: &mut HashMap<String, PathBuf>) {
    for root in workspace_sources.values_mut() {
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

fn reject_workspace_sources_in_managed_cache(
    workspace_sources: &HashMap<String, PathBuf>,
    mod_cache: &Path,
) -> Result<(), ModuleSystemError> {
    for (module, root) in workspace_sources {
        if existing_path_is_within_managed_cache(root, mod_cache)? {
            return Err(ModuleSystemError::new(
                ModuleSystemStage::Workspace,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "workspace source for {module} points inside the managed module cache {}; use a source directory outside the cache",
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
    let canonical_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let path = canonical_path.as_path();
    let source_root = pipeline::source_root(path);
    let mod_cache = default_mod_cache_root()?;
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
        let (ctx, source_generation) =
            vo_module::project::load_single_file_context_with_options_and_generation(
                &base_fs, path, options,
            )
            .map_err(module_system_error_from_project)?;
        return real_path_compile_context_for_single_file(
            ctx,
            source_generation,
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
    let graph = ProjectGraphContext::from_project(&context);
    let (_, project_deps, mut workspace_sources) = context.into_parts();
    canonicalize_workspace_sources(&mut workspace_sources);
    reject_workspace_sources_in_managed_cache(&workspace_sources, &mod_cache)?;
    Ok(RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file: None,
        single_file_source_generation: None,
        graph,
        project_deps,
        current_module_override: None,
        workspace_sources,
        workspace,
        module_cache_read_lease,
    })
}

fn real_path_compile_context_for_single_file(
    ctx: SingleFileContext,
    source_generation: SingleFileSourceGeneration,
    path: &Path,
    source_root: PathBuf,
    mod_cache: PathBuf,
    options: &ProjectContextOptions,
    module_cache_read_lease: Option<Arc<vo_module::cache::CacheReadLease>>,
) -> Result<RealPathCompileContext, CompileError> {
    match ctx {
        SingleFileContext::Project(project_context) => real_path_context_from_project_context(
            project_context,
            source_generation,
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
            // Single-file modules are isolated from ancestor projects and
            // workspaces, and their dependency scope is exactly the stdlib.
            Ok(RealPathCompileContext {
                source_root: source_root.clone(),
                project_root: source_root,
                mod_cache,
                package_dir: PathBuf::from("."),
                single_file: Some(file_name),
                single_file_source_generation: Some(source_generation),
                graph: ProjectGraphContext::empty(),
                project_deps: ProjectDeps::default(),
                current_module_override: Some(current_module),
                workspace_sources: HashMap::new(),
                workspace: WorkspaceCompileContext::disabled(),
                module_cache_read_lease,
            })
        }
        SingleFileContext::AdHoc { file_name, .. } => {
            // Spec §10.1: ad hoc programs see only the stdlib. No ancestor
            // `vo.mod`, no ancestor `vo.work`, no workspace sources. The
            // single-file classifier has already enforced the spec §5.6.4
            // precedence rule (rejecting inline mod inside a project).
            Ok(RealPathCompileContext {
                source_root: source_root.clone(),
                project_root: source_root,
                mod_cache,
                package_dir: PathBuf::from("."),
                single_file: Some(file_name),
                single_file_source_generation: Some(source_generation),
                graph: ProjectGraphContext::empty(),
                project_deps: ProjectDeps::default(),
                current_module_override: None,
                workspace_sources: HashMap::new(),
                workspace: WorkspaceCompileContext::disabled(),
                module_cache_read_lease,
            })
        }
    }
}

fn real_path_context_from_project_context(
    context: ProjectContext,
    source_generation: SingleFileSourceGeneration,
    path: &Path,
    source_root: PathBuf,
    mod_cache: PathBuf,
    options: &ProjectContextOptions,
    module_cache_read_lease: Option<Arc<vo_module::cache::CacheReadLease>>,
) -> Result<RealPathCompileContext, CompileError> {
    let workspace = WorkspaceCompileContext::from_project(&context, options);
    let graph = ProjectGraphContext::from_project(&context);
    let (project_root_raw, project_deps, mut workspace_sources) = context.into_parts();
    canonicalize_workspace_sources(&mut workspace_sources);
    reject_workspace_sources_in_managed_cache(&workspace_sources, &mod_cache)?;
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
        single_file_source_generation: Some(source_generation),
        graph,
        project_deps,
        current_module_override: None,
        workspace_sources,
        workspace,
        module_cache_read_lease,
    })
}

pub fn check(path: &str) -> Result<(), CompileError> {
    check_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn check_with_options(path: &str, options: &ProjectContextOptions) -> Result<(), CompileError> {
    if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
        return pipeline::check_zip(Path::new(&zip_path), internal_root.as_deref());
    }
    check_path_with_options(Path::new(path), options)
}

pub fn check_path(path: &Path) -> Result<(), CompileError> {
    check_path_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn check_path_with_options(
    path: &Path,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    if path.extension() == Some(std::ffi::OsStr::new("zip")) {
        return pipeline::check_zip(path, None);
    }
    let context = load_real_path_compile_context_with_options(path, options)?;
    let _cache_lease = context.acquire_module_cache_read_lease()?;
    let (stdlib_snapshot, stdlib_source_fingerprint) = stdlib_compile_cache_input();
    let captured =
        cache::capture_compile_inputs(context.compile_input_capture(&stdlib_source_fingerprint))?;
    let fingerprint = captured.fingerprint().to_string();
    let post_check_context = context.clone();
    pipeline::check_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib_snapshot.unwrap_or_default(),
        captured.into_snapshot(),
    )?;
    validate_live_compile_input_generation(
        &post_check_context,
        &stdlib_source_fingerprint,
        &fingerprint,
    )
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
    if path.extension() == Some(std::ffi::OsStr::new("zip")) {
        return pipeline::compile_zip(path, None);
    }

    compile_real_path_without_cache(path, options)
}

pub fn compile_path_with_auto_install(path: &Path) -> Result<CompileOutput, CompileError> {
    if let Some(text) = path.to_str() {
        return compile_with_auto_install(text);
    }
    if path.extension() == Some(std::ffi::OsStr::new("zip")) {
        return pipeline::compile_zip(path, None);
    }

    use vo_module::github_registry::GitHubRegistry;

    let options = ProjectContextOptions::from_environment();
    let registry = GitHubRegistry::new();
    let mod_cache = default_mod_cache_root()?;
    auto_install_dependencies(path, &mod_cache, &registry, &options)?;
    compile_path_with_cache_with_options(path, &options)
}

pub fn compile_with_options(
    path: &str,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);

    if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
        return pipeline::compile_zip(Path::new(&zip_path), internal_root.as_deref());
    }
    if path.ends_with(".voc") || path.ends_with(".vob") {
        return pipeline::load_bytecode(p);
    }

    compile_real_path_without_cache(p, options)
}

fn compile_real_path_without_cache(
    path: &Path,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    let mut context = load_real_path_compile_context_with_options(path, options)?;
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());
    canonicalize_workspace_sources(&mut context.workspace_sources);
    let cache_lease = context.acquire_module_cache_read_lease()?;
    let mod_cache = context.mod_cache.clone();
    let (stdlib_snapshot, stdlib_source_fingerprint) = stdlib_compile_cache_input();
    let captured =
        cache::capture_compile_inputs(context.compile_input_capture(&stdlib_source_fingerprint))?;
    let fingerprint = captured.fingerprint().to_string();
    let post_compile_context = context.clone();
    let mut output = pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib_snapshot.unwrap_or_default(),
        captured.into_snapshot(),
    )?;

    native::check_materialized_dependency_readiness(
        post_compile_context.project_deps.locked_modules(),
        &post_compile_context.mod_cache,
    )
    .map_err(CompileError::ModuleSystem)?;
    validate_live_compile_input_generation(
        &post_compile_context,
        &stdlib_source_fingerprint,
        &fingerprint,
    )?;
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
    if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
        return pipeline::compile_zip(Path::new(&zip_path), internal_root.as_deref());
    }
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
    if entry_path.extension() == Some(std::ffi::OsStr::new("zip")) {
        return pipeline::compile_zip(entry_path, None);
    }
    let mut context = load_real_path_compile_context_with_options(entry_path, options)?;
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());
    for workspace_source_root in context.workspace_sources.values_mut() {
        *workspace_source_root = workspace_source_root
            .canonicalize()
            .unwrap_or_else(|_| workspace_source_root.clone());
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
        &context.graph,
        &context.project_deps,
        &context.workspace_sources,
        context.current_module_override.as_deref(),
        &context.workspace,
    )?;
    let captured_module_fs =
        snapshot::ResolverFs::snapshot(captured_inputs.snapshot(), &context.mod_cache);
    let captured_ready_modules = native::check_materialized_dependency_readiness_with_fs(
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
        if native::cached_native_extension_specs_match_frozen_inputs(
            &mut output.extensions,
            &captured_context_fs,
            &captured_ready_modules,
            &context.mod_cache,
            &context.workspace.options.workspace,
        ) {
            // The cache may persist a structurally older or independently damaged
            // copy of lock metadata. The current project context has already
            // parsed and validated the authoritative root lock and participates in
            // the cache fingerprint, so expose that exact value to callers.
            output.locked_modules = context.project_deps.locked_modules().to_vec();
            validate_live_compile_input_generation(
                &context,
                &stdlib_source_fingerprint,
                &fingerprint,
            )?;
            retain_module_cache_lease(&mut output, &context.mod_cache, cache_lease);
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "compile_cache_hit")
                    .path(compile_log_path(entry_path)),
            );
            return Ok(output);
        }
        cache::discard_compile_cache_entry(&cache_slot, &fingerprint);
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

    // Local native extensions prepare immutable load copies from their own
    // stable, pre/post-validated Cargo input generation. The project snapshot
    // below independently protects every source and metadata byte consumed by
    // analysis; cache-hit extension validation rechecks only the extensions
    // retained in the compiled output.
    validate_live_compile_input_generation(
        &post_compile_context,
        &stdlib_source_fingerprint,
        &fingerprint,
    )?;
    retain_module_cache_lease(&mut output, &post_compile_context.mod_cache, cache_lease);
    cache::save_compile_cache(&cache_slot, &fingerprint, &output);
    validate_live_compile_input_generation(
        &post_compile_context,
        &stdlib_source_fingerprint,
        &fingerprint,
    )?;
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "compile_cache_store")
            .path(compile_log_path(entry_path)),
    );

    Ok(output)
}

fn validate_live_compile_input_generation(
    context: &RealPathCompileContext,
    stdlib_source_fingerprint: &str,
    captured_fingerprint: &str,
) -> Result<(), CompileError> {
    pipeline::validate_live_workspace_generation(&context.project_root, &context.workspace)?;
    let recaptured =
        cache::capture_compile_inputs(context.compile_input_capture(stdlib_source_fingerprint))?;
    ensure_compile_output_generation_is_current(captured_fingerprint, recaptured.fingerprint())
}

fn ensure_compile_output_generation_is_current(
    captured_fingerprint: &str,
    recaptured_fingerprint: &str,
) -> Result<(), CompileError> {
    if recaptured_fingerprint == captured_fingerprint {
        return Ok(());
    }
    Err(CompileError::ModuleSystem(ModuleSystemError::new(
        ModuleSystemStage::CompileInputs,
        ModuleSystemErrorKind::Mismatch,
        "compile inputs changed before output publication; retry so analysis, bytecode, native artifacts, and cache state come from one generation",
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
    if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
        return pipeline::compile_zip(Path::new(&zip_path), internal_root.as_deref());
    }
    let p = Path::new(path);
    let mod_cache = default_mod_cache_root()?;
    auto_install_dependencies(p, &mod_cache, registry, options)?;
    compile_with_cache_with_options(path, options)
}

fn check_path_with_auto_install_using_registry(
    path: &Path,
    registry: &dyn Registry,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    if let Some(path) = path.to_str() {
        if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
            return pipeline::check_zip(Path::new(&zip_path), internal_root.as_deref());
        }
    } else if path.extension() == Some(std::ffi::OsStr::new("zip")) {
        return pipeline::check_zip(path, None);
    }
    let mod_cache = default_mod_cache_root()?;
    auto_install_dependencies(path, &mod_cache, registry, options)?;
    check_path_with_options(path, options)
}

/// Materialize dependencies for project inputs before compilation.
///
/// Ad-hoc and inline single-file inputs are standard-library-only and never
/// consult a registry or write dependency state.
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
                let (_, project_deps, mut workspace_sources) = project_context.into_parts();
                canonicalize_workspace_sources(&mut workspace_sources);
                reject_workspace_sources_in_managed_cache(&workspace_sources, mod_cache)?;
                auto_download_project_deps(&project_deps, mod_cache, registry)
            }
            SingleFileContext::EphemeralInlineMod { .. } => Ok(()),
            SingleFileContext::AdHoc { .. } => Ok(()),
        };
    }

    let context = vo_module::project::load_project_context_with_options(
        &RealFs::new("."),
        &source_root,
        options,
    )
    .map_err(module_system_error_from_project)?;
    let (_, project_deps, mut workspace_sources) = context.into_parts();
    canonicalize_workspace_sources(&mut workspace_sources);
    reject_workspace_sources_in_managed_cache(&workspace_sources, mod_cache)?;
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
            .is_ok();
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

    vo_module::lifecycle::download_project_dependencies(mod_cache, project_deps, registry)
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

pub fn default_mod_cache_root() -> Result<PathBuf, ModuleSystemError> {
    #[cfg(test)]
    {
        MOD_CACHE_ROOT_LOOKUPS.with(|count| count.set(count.get() + 1));
        if let Some(root) = MOD_CACHE_ROOT_OVERRIDE.with(|slot| slot.borrow().clone()) {
            return Ok(root);
        }
    }
    select_mod_cache_root(
        std::env::var_os("VO_MOD_CACHE").map(PathBuf::from),
        dirs::home_dir(),
    )
}

fn select_mod_cache_root(
    configured: Option<PathBuf>,
    home: Option<PathBuf>,
) -> Result<PathBuf, ModuleSystemError> {
    if let Some(root) = configured {
        if root.as_os_str().is_empty() {
            return Err(mod_cache_root_configuration_error(
                "VO_MOD_CACHE must not be empty; unset it to use the versioned per-user default",
                None,
            ));
        }
        if !root.is_absolute() {
            return Err(mod_cache_root_configuration_error(
                format!(
                    "VO_MOD_CACHE must be an absolute path, found {}",
                    root.display(),
                ),
                Some(&root),
            ));
        }
        return Ok(root);
    }

    let home = home.ok_or_else(|| {
        mod_cache_root_configuration_error(
            "cannot determine the user home directory; set VO_MOD_CACHE to an absolute path",
            None,
        )
    })?;
    if !home.is_absolute() {
        return Err(mod_cache_root_configuration_error(
            format!(
                "the resolved user home directory must be absolute, found {}",
                home.display(),
            ),
            Some(&home),
        ));
    }
    Ok(home
        .join(DEFAULT_MOD_CACHE_PARENT)
        .join(vo_module::cache::CACHE_LAYOUT_GENERATION))
}

fn mod_cache_root_configuration_error(
    detail: impl Into<String>,
    path: Option<&Path>,
) -> ModuleSystemError {
    let error = ModuleSystemError::new(
        ModuleSystemStage::CachedModule,
        ModuleSystemErrorKind::ValidationFailed,
        detail,
    );
    match path {
        Some(path) => error.with_path(path),
        None => error,
    }
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
        let cached_dir = root.join("github.com@acme@cached").join("1.0.0");
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
            let result = vo_module::ops::cache_clean(&clean_root);
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
