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

struct RealPathCompileContext {
    source_root: PathBuf,
    project_root: PathBuf,
    mod_cache: PathBuf,
    package_dir: PathBuf,
    single_file: Option<PathBuf>,
    project_deps: ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
}

impl RealPathCompileContext {
    fn into_pipeline_context(self) -> pipeline::ProjectCompileContext {
        pipeline::ProjectCompileContext {
            project_root: self.project_root,
            mod_cache: self.mod_cache,
            source_root: self.source_root,
            package_dir: self.package_dir,
            single_file: self.single_file,
            project_deps: self.project_deps,
            replaces: self.workspace_replaces,
        }
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

fn load_real_path_compile_context_with_options(
    path: &Path,
    options: &ProjectContextOptions,
) -> Result<RealPathCompileContext, CompileError> {
    let source_root = pipeline::source_root(path);
    let mod_cache = default_mod_cache_root();
    let base_fs = RealFs::new(".");

    // Single-file entries go through the spec §5.6 single-file classifier so
    // that inline `/*vo:mod ... */` metadata is recognized and the spec §5.6.4
    // precedence rules are enforced uniformly for real-path compiles.
    if path.is_file() {
        let ctx =
            vo_module::project::load_single_file_context_with_options(&base_fs, path, options)
                .map_err(module_system_error_from_project)?;
        return real_path_compile_context_for_single_file(ctx, path, source_root, mod_cache);
    }

    let context =
        vo_module::project::load_project_context_with_options(&base_fs, &source_root, options)
            .map_err(module_system_error_from_project)?;
    let project_root = context.project_root().to_path_buf();
    let package_dir = relative_package_dir(&project_root, &source_root);
    let (_, project_deps, workspace_replaces) = context.into_parts();
    Ok(RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file: None,
        project_deps,
        workspace_replaces,
    })
}

fn real_path_compile_context_for_single_file(
    ctx: SingleFileContext,
    path: &Path,
    source_root: PathBuf,
    mod_cache: PathBuf,
) -> Result<RealPathCompileContext, CompileError> {
    match ctx {
        SingleFileContext::Project(project_context) => Ok(real_path_context_from_project_context(
            project_context,
            path,
            source_root,
            mod_cache,
        )),
        SingleFileContext::EphemeralInlineMod {
            file_name,
            inline_mod,
            ..
        } => {
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
                    workspace_replaces: HashMap::new(),
                });
            }

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
            let context = vo_module::project::load_project_context(&base_fs, &cached.cache_dir)
                .map_err(module_system_error_from_project)?;
            let (_, project_deps, _) = context.into_parts();
            Ok(RealPathCompileContext {
                source_root: source_root.clone(),
                project_root: source_root,
                mod_cache,
                package_dir: PathBuf::from("."),
                single_file: Some(file_name),
                project_deps,
                // Ephemeral single-file modules cannot declare `replace`
                // (spec §5.6.3) and must not consult ancestor `vo.work`
                // (spec §10.1), so workspace overrides are always empty.
                workspace_replaces: HashMap::new(),
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
                workspace_replaces: HashMap::new(),
            })
        }
    }
}

fn real_path_context_from_project_context(
    context: ProjectContext,
    path: &Path,
    source_root: PathBuf,
    mod_cache: PathBuf,
) -> RealPathCompileContext {
    let (project_root_raw, project_deps, workspace_replaces) = context.into_parts();
    // `source_root` is canonicalized by `pipeline::source_root`; ensure
    // `project_root` matches the same canonical form so that
    // `relative_package_dir` can strip the prefix reliably across
    // platforms with symlinked temp dirs (e.g. `/var` vs `/private/var`).
    let project_root = project_root_raw.canonicalize().unwrap_or(project_root_raw);
    let package_dir = relative_package_dir(&project_root, &source_root);
    let single_file = relative_single_file_path(&package_dir, path);
    RealPathCompileContext {
        source_root,
        project_root,
        mod_cache,
        package_dir,
        single_file,
        project_deps,
        workspace_replaces,
    }
}

fn scoped_project_fs(project_root: &Path) -> ScopedFs<RealFs> {
    ScopedFs::new(RealFs::new("."), project_root)
}

pub fn check(path: &str) -> Result<(), CompileError> {
    check_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn check_with_options(path: &str, options: &ProjectContextOptions) -> Result<(), CompileError> {
    let p = Path::new(path);
    let context = load_real_path_compile_context_with_options(p, options)?;
    let fs = scoped_project_fs(&context.project_root);
    pipeline::check_with_project_context(fs, context.into_pipeline_context())
}

pub fn compile(path: &str) -> Result<CompileOutput, CompileError> {
    compile_with_options(path, &ProjectContextOptions::from_environment())
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
    let fs = scoped_project_fs(&context.project_root);
    pipeline::compile_with_project_context(fs, context.into_pipeline_context())
}

pub fn compile_with_cache(path: &str) -> Result<CompileOutput, CompileError> {
    compile_with_cache_with_options(path, &ProjectContextOptions::from_environment())
}

pub fn compile_with_cache_with_options(
    path: &str,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    let entry_path = Path::new(path);
    if path.ends_with(".voc") || path.ends_with(".vob") || pipeline::parse_zip_path(path).is_some()
    {
        return compile_with_options(path, options);
    }
    let context = load_real_path_compile_context_with_options(entry_path, options)?;
    let cache_slot = cache::compile_cache_slot(
        &context.source_root,
        context.single_file.as_deref().and_then(Path::file_name),
    );
    let fingerprint = cache::compute_compile_cache_fingerprint(
        &context.source_root,
        &context.project_root,
        &context.mod_cache,
        context.single_file.as_deref(),
        &context.project_deps,
        &context.workspace_replaces,
    )?;

    if let Some(output) = cache::try_load_cache(&cache_slot, &context.source_root, &fingerprint) {
        emit_compile_log(CompileLogRecord::new("vo-engine", "compile_cache_hit").path(path));
        return Ok(output);
    }

    let fs = scoped_project_fs(&context.project_root);
    let output = pipeline::compile_with_project_context(fs, context.into_pipeline_context())?;

    cache::save_compile_cache(&cache_slot, &fingerprint, &output);
    emit_compile_log(CompileLogRecord::new("vo-engine", "compile_cache_store").path(path));

    Ok(output)
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
    use vo_module::github_registry::GitHubRegistry;

    let registry = GitHubRegistry::new();
    check_with_auto_install_using_registry(path, &registry, options)
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

fn check_with_auto_install_using_registry(
    path: &str,
    registry: &dyn Registry,
    options: &ProjectContextOptions,
) -> Result<(), CompileError> {
    let p = Path::new(path);
    let mod_cache = default_mod_cache_root();
    auto_install_dependencies(p, &mod_cache, registry, options)?;
    check_with_options(path, options)
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
    if path.is_file() {
        let ctx = vo_module::project::load_single_file_context_with_options(
            &RealFs::new("."),
            path,
            options,
        )
        .map_err(module_system_error_from_project)?;
        return match ctx {
            SingleFileContext::Project(project_context) => {
                let (_, project_deps, _) = project_context.into_parts();
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
                        format!(
                            "ephemeral dependency resolution failed for {}: {}",
                            path.display(),
                            error
                        ),
                    )
                    .with_path(path)
                })?;
                Ok(())
            }
            SingleFileContext::AdHoc { .. } => Ok(()),
        };
    }

    let root = pipeline::source_root(path);
    let context =
        vo_module::project::load_project_context_with_options(&RealFs::new("."), &root, options)
            .map_err(module_system_error_from_project)?;
    let (_, project_deps, _) = context.into_parts();
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
    let Some(lock_file) = project_deps.lock_file() else {
        return Ok(());
    };

    let cache_fs = RealFs::new(mod_cache);
    let dependency_state = project_deps
        .locked_modules()
        .iter()
        .map(|locked| {
            let module_dir = vo_module::cache::layout::relative_module_dir(
                locked.path.as_str(),
                &locked.version,
            );
            let cached = vo_module::cache::validate::validate_installed_module(
                &cache_fs,
                &module_dir,
                locked,
            )
            .is_ok()
                && locked.artifacts.iter().all(|artifact| {
                    let artifact_path = module_dir.join("artifacts").join(&artifact.id.name);
                    vo_module::cache::validate::validate_installed_artifact(
                        &cache_fs,
                        &artifact_path,
                        locked,
                        artifact,
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

    vo_module::lifecycle::download_locked_dependencies(mod_cache, lock_file, registry).map_err(
        |e| {
            ModuleSystemError::new(
                ModuleSystemStage::DependencyDownload,
                ModuleSystemErrorKind::DownloadFailed,
                format!("failed to download dependencies: {}", e),
            )
            .with_path(mod_cache)
        },
    )?;

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
