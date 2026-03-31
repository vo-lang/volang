use std::cell::RefCell;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use vo_common::vfs::{MemoryFs, RealFs};
use vo_common_core::LogRecordCore;
use vo_module::ext_manifest::ExtensionManifest;
use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::NativeExtensionSpec;
use vo_vm::bytecode::Module;

mod cache;
mod native;
mod pipeline;
mod project_prepare;

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

#[derive(Debug, Clone)]
pub struct ModuleSystemError {
    pub stage: ModuleSystemStage,
    pub kind: ModuleSystemErrorKind,
    pub module_path: Option<String>,
    pub version: Option<String>,
    pub path: Option<String>,
    pub detail: String,
}

impl ModuleSystemError {
    fn new(stage: ModuleSystemStage, kind: ModuleSystemErrorKind, detail: impl Into<String>) -> Self {
        Self {
            stage,
            kind,
            module_path: None,
            version: None,
            path: None,
            detail: detail.into(),
        }
    }

    fn with_locked(mut self, locked: &LockedModule) -> Self {
        self.module_path = Some(locked.path.as_str().to_string());
        self.version = Some(locked.version.to_string());
        self
    }

    fn with_path(mut self, path: &Path) -> Self {
        self.path = Some(path.display().to_string());
        self
    }
}

impl std::fmt::Display for ModuleSystemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.detail)
    }
}

impl std::error::Error for ModuleSystemError {}

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

#[derive(Debug, Clone)]
pub struct CompileOutput {
    pub module: Module,
    pub source_root: PathBuf,
    pub extensions: Vec<PreparedNativeExtension>,
    pub locked_modules: Vec<LockedModule>,
}

pub fn check(path: &str) -> Result<(), CompileError> {
    let p = Path::new(path);
    let root = pipeline::source_root(p);
    let single_file = if p.is_file() {
        Some(p.file_name().unwrap_or_default())
    } else {
        None
    };
    pipeline::check_with_fs(RealFs::new(&root), &root, single_file)
}

pub fn compile(path: &str) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);

    if path.ends_with(".voc") || path.ends_with(".vob") {
        return pipeline::load_bytecode(p);
    }

    if let Some((zip_path, internal_root)) = pipeline::parse_zip_path(path) {
        return pipeline::compile_zip(Path::new(&zip_path), internal_root.as_deref());
    }

    let root = pipeline::source_root(p);
    let single_file = if p.is_file() {
        Some(p.file_name().unwrap_or_default())
    } else {
        None
    };
    pipeline::compile_with_fs(RealFs::new(&root), &root, single_file)
}

pub fn compile_with_cache(path: &str) -> Result<CompileOutput, CompileError> {
    let entry_path = Path::new(path);
    if path.ends_with(".voc") || path.ends_with(".vob") || pipeline::parse_zip_path(path).is_some() {
        return compile(path);
    }
    let root = pipeline::source_root(entry_path);
    let single_file = if entry_path.is_file() {
        Some(entry_path.file_name().unwrap_or_default())
    } else {
        None
    };
    let replaces = project_prepare::read_all_replaces(&root)?;
    let cache_slot = cache::compile_cache_slot(&root, single_file);
    let fingerprint =
        cache::compute_compile_cache_fingerprint(&root, single_file, &replaces)?;

    if let Some(output) = cache::try_load_cache(&cache_slot, &root, &fingerprint) {
        emit_compile_log(CompileLogRecord::new("vo-engine", "compile_cache_hit").path(path));
        return Ok(output);
    }

    let output = compile(path)?;

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
    auto_download_locked_modules(&root)?;
    compile_with_cache(path)
}

pub fn check_with_auto_install(path: &str) -> Result<(), CompileError> {
    let p = Path::new(path);
    let root = pipeline::source_root(p);
    auto_download_locked_modules(&root)?;
    check(path)
}

fn locked_module_fully_cached(mod_cache: &Path, locked: &LockedModule) -> bool {
    let fs = RealFs::new(mod_cache);
    let module_dir = vo_module::cache::layout::relative_module_dir(locked.path.as_str(), &locked.version);
    if vo_module::cache::validate::validate_installed_module(&fs, &module_dir, locked).is_err() {
        return false;
    }
    locked.artifacts.iter().all(|artifact| {
        let artifact_path = module_dir.join("artifacts").join(&artifact.id.name);
        vo_module::cache::validate::validate_installed_artifact(&fs, &artifact_path, locked, artifact)
            .is_ok()
    })
}

fn auto_download_locked_modules(root: &Path) -> Result<(), CompileError> {
    use vo_module::github_registry::GitHubRegistry;

    let project_deps = vo_module::project::read_project_deps(&RealFs::new(root), &[])
        .map_err(|error| CompileError::ModuleSystem(project_prepare::project_deps_error_to_module_system(error)))?;

    if !project_deps.has_mod_file || project_deps.locked_modules.is_empty() {
        return Ok(());
    }
    let Some(lock_file) = project_deps.lock_file.as_ref() else {
        return Ok(());
    };

    let mod_cache = default_mod_cache_root();
    let registry = GitHubRegistry::new();
    let module_cache_state = project_deps
        .locked_modules
        .iter()
        .map(|locked| {
            let fully_cached = locked_module_fully_cached(&mod_cache, locked);
            if fully_cached {
                emit_compile_log(
                    CompileLogRecord::new("vo-engine", "dependency_cached")
                        .module(locked.path.as_str())
                        .version(locked.version.to_string()),
                );
            } else {
                emit_compile_log(
                    CompileLogRecord::new("vo-engine", "dependency_fetch_start")
                        .module(locked.path.as_str())
                        .version(locked.version.to_string()),
                );
            }
            (
                locked.path.to_string(),
                locked.version.to_string(),
                fully_cached,
            )
        })
        .collect::<Vec<_>>();
    vo_module::cache::install::populate_locked_cache(&mod_cache, lock_file, &registry).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::DependencyDownload,
            ModuleSystemErrorKind::DownloadFailed,
            format!("failed to download dependencies: {}", e),
        )
        .with_path(&mod_cache)
    })?;
    for (module, version, fully_cached) in module_cache_state {
        if !fully_cached {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "dependency_fetch_done")
                    .module(module)
                    .version(version),
            );
        }
    }

    for locked in &project_deps.locked_modules {
        let cache_dir = vo_module::cache::layout::cache_dir(&mod_cache, &locked.path, &locked.version);
        let manifests = vo_module::ext_manifest::discover_extensions(&cache_dir).map_err(|e| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ParseFailed,
                format!(
                    "invalid cached extension manifest for {}@{}: {}",
                    locked.path, locked.version, e,
                ),
            )
            .with_locked(locked)
            .with_path(&cache_dir)
        })?;
        prepare_native_extension_specs(&manifests, &project_deps.locked_modules)?;
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
) -> Result<Vec<PreparedNativeExtension>, ModuleSystemError> {
    native::prepare_native_extension_specs_for_frozen_build(
        manifests,
        locked_modules,
        &default_mod_cache_root(),
    )
}
