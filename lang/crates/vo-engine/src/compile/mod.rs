use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use vo_common::vfs::{MemoryFs, RealFs, ScopedFs};
use vo_common_core::LogRecordCore;
use vo_module::ext_manifest::ExtensionManifest;
use vo_module::operation_error::OperationError;
use vo_module::project::{ProjectDeps, ProjectDepsError, ProjectDepsErrorKind, ProjectDepsStage};
use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::NativeExtensionSpec;

mod cache;
mod native;
mod pipeline;

#[cfg(test)]
mod tests;

const MOD_CACHE_DIR: &str = ".vo/mod";
const COMPILE_CACHE_SCHEMA_VERSION: &str = "4";
const COMPILE_CACHE_SLOT_NAMESPACE: &str = "vo-compile-cache-slot";
const COMPILE_CACHE_NATIVE_NAMESPACE: &str = "vo-compile-cache-native";

pub type PreparedNativeExtension = NativeExtensionSpec;

pub type CompileLogRecord = LogRecordCore;

type CompileLogSink = dyn Fn(CompileLogRecord) + Send + Sync;

thread_local! {
    static COMPILE_LOG_SINK: RefCell<Option<Arc<CompileLogSink>>> = RefCell::new(None);
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

struct RealPathCompileContext {
    source_root: PathBuf,
    project_root: PathBuf,
    mod_cache: PathBuf,
    package_dir: PathBuf,
    single_file: Option<PathBuf>,
    project_deps: ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
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

fn load_real_path_compile_context(path: &Path) -> Result<RealPathCompileContext, CompileError> {
    let source_root = pipeline::source_root(path);
    let mod_cache = default_mod_cache_root();
    let base_fs = RealFs::new(".");
    let context = vo_module::project::load_project_context(&base_fs, &source_root)
        .map_err(module_system_error_from_project)?;
    let package_dir = relative_package_dir(&context.project_root, &source_root);
    let single_file = if path.is_file() {
        relative_single_file_path(&package_dir, path)
    } else {
        None
    };
    Ok(RealPathCompileContext {
        source_root,
        project_root: context.project_root,
        mod_cache,
        package_dir,
        single_file,
        project_deps: context.project_deps,
        workspace_replaces: context.workspace_replaces,
    })
}

fn scoped_project_fs(project_root: &Path) -> ScopedFs<RealFs> {
    ScopedFs::new(RealFs::new("."), project_root)
}

pub fn check(path: &str) -> Result<(), CompileError> {
    let p = Path::new(path);
    let RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file,
        project_deps,
        workspace_replaces,
    } = load_real_path_compile_context(p)?;
    pipeline::check_with_project_context(
        scoped_project_fs(&project_root),
        project_root,
        mod_cache,
        source_root,
        package_dir,
        single_file,
        project_deps,
        workspace_replaces,
    )
}

pub fn compile(path: &str) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);

    if path.ends_with(".voc") || path.ends_with(".vob") {
        return pipeline::load_bytecode(p);
    }

    if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
        return pipeline::compile_zip(Path::new(&zip_path), internal_root.as_deref());
    }

    let RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file,
        project_deps,
        workspace_replaces,
    } = load_real_path_compile_context(p)?;
    pipeline::compile_with_project_context(
        scoped_project_fs(&project_root),
        project_root,
        mod_cache,
        source_root,
        package_dir,
        single_file,
        project_deps,
        workspace_replaces,
    )
}

pub fn compile_with_cache(path: &str) -> Result<CompileOutput, CompileError> {
    let entry_path = Path::new(path);
    if path.ends_with(".voc") || path.ends_with(".vob") || pipeline::parse_zip_path(path).is_some()
    {
        return compile(path);
    }
    let RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file,
        project_deps,
        workspace_replaces,
    } = load_real_path_compile_context(entry_path)?;
    let cache_slot = cache::compile_cache_slot(
        &source_root,
        single_file.as_deref().and_then(Path::file_name),
    );
    let fingerprint = cache::compute_compile_cache_fingerprint(
        &source_root,
        &project_root,
        &mod_cache,
        single_file.as_deref(),
        &workspace_replaces,
    )?;

    if let Some(output) = cache::try_load_cache(&cache_slot, &source_root, &fingerprint) {
        emit_compile_log(CompileLogRecord::new("vo-engine", "compile_cache_hit").path(path));
        return Ok(output);
    }

    let output = pipeline::compile_with_project_context(
        scoped_project_fs(&project_root),
        project_root,
        mod_cache,
        source_root,
        package_dir,
        single_file,
        project_deps,
        workspace_replaces,
    )?;

    cache::save_compile_cache(&cache_slot, &fingerprint, &output);
    emit_compile_log(CompileLogRecord::new("vo-engine", "compile_cache_store").path(path));

    Ok(output)
}

pub fn compile_from_memory(fs: MemoryFs, root: &Path) -> Result<CompileOutput, CompileError> {
    pipeline::compile_with_fs(fs, root, None)
}

pub fn compile_source_at(source: &str, root: &Path) -> Result<CompileOutput, CompileError> {
    let mut mem = MemoryFs::new();
    mem.add_file("main.vo", source);
    pipeline::compile_with_fs(mem, root, Some(std::ffi::OsStr::new("main.vo")))
}

pub fn compile_string(code: &str) -> Result<CompileOutput, CompileError> {
    let temp_dir = std::env::temp_dir().join("vo_compile");
    fs::create_dir_all(&temp_dir)?;
    compile_source_at(code, &temp_dir)
}

pub fn compile_with_auto_install(path: &str) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);
    let root = pipeline::source_root(p);
    let mod_cache = default_mod_cache_root();
    auto_download_locked_modules(&root, &mod_cache)?;
    compile_with_cache(path)
}

pub fn check_with_auto_install(path: &str) -> Result<(), CompileError> {
    let p = Path::new(path);
    let root = pipeline::source_root(p);
    let mod_cache = default_mod_cache_root();
    auto_download_locked_modules(&root, &mod_cache)?;
    check(path)
}

fn auto_download_locked_modules(root: &Path, mod_cache: &Path) -> Result<(), CompileError> {
    use vo_module::github_registry::GitHubRegistry;

    let plan = vo_module::lifecycle::load_project_locked_dependency_plan(root, mod_cache)
        .map_err(module_system_error_from_project)?;
    let project_deps = plan.project_deps;

    if !project_deps.has_mod_file() || project_deps.locked_modules().is_empty() {
        return Ok(());
    }
    let Some(lock_file) = project_deps.lock_file() else {
        return Ok(());
    };

    let dependency_state = plan.dependency_state;
    for dependency in &dependency_state {
        if dependency.cached {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "dependency_cached")
                    .module(&dependency.module)
                    .version(&dependency.version),
            );
        } else {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "dependency_fetch_start")
                    .module(&dependency.module)
                    .version(&dependency.version),
            );
        }
    }

    let registry = GitHubRegistry::new();
    vo_module::lifecycle::download_locked_dependencies(mod_cache, lock_file, &registry).map_err(
        |e| {
            ModuleSystemError::new(
                ModuleSystemStage::DependencyDownload,
                ModuleSystemErrorKind::DownloadFailed,
                format!("failed to download dependencies: {}", e),
            )
            .with_path(mod_cache)
        },
    )?;

    for dependency in dependency_state {
        if !dependency.cached {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "dependency_fetch_done")
                    .module(dependency.module)
                    .version(dependency.version),
            );
        }
    }

    Ok(())
}

pub fn default_mod_cache_root() -> PathBuf {
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
    native::prepare_native_extension_specs_for_frozen_build(manifests, locked_modules, mod_cache)
}
