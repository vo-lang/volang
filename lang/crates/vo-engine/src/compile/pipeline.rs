use std::collections::HashMap;
use std::ffi::OsStr;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use sha2::{Digest, Sha256};
use vo_analysis::project::{PackageIdentity, Project as AnalysisProject};
use vo_analysis::vfs::{
    analyze_file_set_with_package_identity, project_package_resolver_with_workspace_sources,
};
use vo_codegen::compile_project;
use vo_common::vfs::{
    normalize_fs_path, FileSet, FileSystem, RealFs, ZipFs, MAX_ZIP_ARCHIVE_BYTES,
};
use vo_module::project::{
    ProjectContext, ProjectContextOptions, ProjectDeps, SingleFileContext, WorkspaceModule,
};
use vo_module::readiness::ReadyModule;
use vo_module::workspace::WorkspaceDiscovery;
use vo_stdlib::EmbeddedStdlib;
use vo_vm::bytecode::Module;

use super::native::{
    check_materialized_dependency_readiness_with_fs,
    prepare_native_extension_specs_with_readiness_and_workspace,
};
use super::snapshot::{CompileInputSnapshot, ResolverFs};
use super::{
    CompileError, CompileOutput, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage,
};

// In-memory compilation rejects every host-backed dependency before analysis.
// This virtual root is carried only to keep the shared analysis/output types
// uniform; its resolver is an immutable empty snapshot and never touches the
// host filesystem.
const IN_MEMORY_MODULE_CACHE_ROOT: &str = ".vo-in-memory-module-cache";

struct PreparedProject<F> {
    fs: F,
    stdlib: Option<EmbeddedStdlib>,
    module_fs: ResolverFs,
    workspace_source_fs: ResolverFs,
    native_input_fs: ResolverFs,
    file_set: FileSet,
    source_root: PathBuf,
    local_root: PathBuf,
    mod_cache: PathBuf,
    workspace_sources: HashMap<String, PathBuf>,
    project_deps: ProjectDeps,
    current_module: Option<String>,
    current_package: Option<PackageIdentity>,
    ready_modules: Vec<ReadyModule>,
    workspace: super::WorkspaceCompileContext,
}

struct AnalyzedCompilation {
    project: AnalysisProject,
    source_root: PathBuf,
    mod_cache: PathBuf,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
    ready_modules: Vec<ReadyModule>,
    workspace: super::WorkspaceCompileContext,
    native_input_fs: ResolverFs,
}

pub(super) struct ProjectCompileContext {
    pub(super) project_root: PathBuf,
    pub(super) mod_cache: PathBuf,
    pub(super) source_root: PathBuf,
    pub(super) package_dir: PathBuf,
    pub(super) single_file: Option<PathBuf>,
    pub(super) graph: super::ProjectGraphContext,
    pub(super) project_deps: ProjectDeps,
    /// Explicit module identity for ephemeral inline modules whose dependency
    /// context is intentionally otherwise empty.
    pub(super) current_module_override: Option<String>,
    pub(super) workspace_sources: HashMap<String, PathBuf>,
    pub(super) workspace: super::WorkspaceCompileContext,
}

fn package_subpath(package_dir: &Path) -> Result<String, CompileError> {
    let mut segments = Vec::new();
    for component in package_dir.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::Normal(segment) => {
                let segment = segment.to_str().ok_or_else(|| {
                    CompileError::Analysis(format!(
                        "package path is not valid UTF-8: {}",
                        package_dir.display()
                    ))
                })?;
                segments.push(segment);
            }
            _ => {
                return Err(CompileError::Analysis(format!(
                    "package path must stay within the project root: {}",
                    package_dir.display()
                )))
            }
        }
    }
    Ok(segments.join("/"))
}

fn compilation_identities(
    project_deps: &ProjectDeps,
    current_module_override: Option<&str>,
    package_dir: &Path,
) -> Result<(Option<String>, Option<PackageIdentity>), CompileError> {
    let declared_module = project_deps.current_module();
    if let (Some(override_module), Some(declared_module)) =
        (current_module_override, declared_module)
    {
        if override_module != declared_module {
            return Err(CompileError::Analysis(format!(
                "compile context module identity mismatch: {override_module} != {declared_module}"
            )));
        }
    }

    let current_module = current_module_override
        .or(declared_module)
        .map(str::to_string);
    let Some(module) = current_module.as_deref() else {
        return Ok((None, None));
    };

    let subpath = package_subpath(package_dir)?;
    let package_path = if subpath.is_empty() {
        module.to_string()
    } else {
        format!("{module}/{subpath}")
    };
    let package_identity = PackageIdentity::new(package_path).map_err(|error| {
        CompileError::Analysis(format!("invalid current package identity: {error}"))
    })?;
    Ok((current_module, Some(package_identity)))
}

impl<F: FileSystem> PreparedProject<F> {
    fn load_memory_prepared(
        fs: F,
        context: ProjectCompileContext,
        empty_message: &'static str,
    ) -> Result<Self, CompileError> {
        reject_unfrozen_memory_inputs(&context)?;
        let empty_snapshot = Arc::new(CompileInputSnapshot::default());
        let module_fs = ResolverFs::snapshot(Arc::clone(&empty_snapshot), &context.mod_cache);
        let workspace_source_fs = ResolverFs::snapshot_global(empty_snapshot);
        Self::load_prepared_with_inputs(
            fs,
            context,
            empty_message,
            None,
            module_fs,
            workspace_source_fs,
        )
    }

    fn load_prepared_with_inputs(
        fs: F,
        context: ProjectCompileContext,
        empty_message: &'static str,
        stdlib: Option<EmbeddedStdlib>,
        module_fs: ResolverFs,
        workspace_source_fs: ResolverFs,
    ) -> Result<Self, CompileError> {
        let (current_module, current_package) = compilation_identities(
            &context.project_deps,
            context.current_module_override.as_deref(),
            &context.package_dir,
        )?;
        let file_set = collect_file_set(
            &fs,
            &context.package_dir,
            context.single_file.as_deref(),
            context.project_root.clone(),
            empty_message,
        )?;
        let ready_modules = check_materialized_dependency_readiness_with_fs(
            &module_fs,
            context.project_deps.locked_modules(),
        )
        .map_err(CompileError::ModuleSystem)?;
        let native_input_fs = workspace_source_fs.clone();
        Ok(Self {
            fs,
            stdlib,
            module_fs,
            workspace_source_fs,
            native_input_fs,
            file_set,
            source_root: context.source_root,
            local_root: PathBuf::from("."),
            mod_cache: context.mod_cache,
            workspace_sources: context.workspace_sources,
            project_deps: context.project_deps,
            current_module,
            current_package,
            ready_modules,
            workspace: context.workspace,
        })
    }

    fn analyze(self) -> Result<AnalyzedCompilation, CompileError> {
        let locked_modules = self.project_deps.locked_modules().to_vec();
        let resolver = project_package_resolver_with_workspace_sources(
            self.stdlib.unwrap_or_default(),
            self.module_fs,
            self.workspace_source_fs,
            &self.project_deps,
            self.workspace_sources,
        );
        let project = analyze_file_set_with_package_identity(
            self.file_set,
            resolver,
            self.fs,
            self.local_root,
            self.current_module,
            self.current_package,
        )
        .map_err(|e| CompileError::Analysis(format!("{}", e)))?;
        Ok(AnalyzedCompilation {
            project,
            source_root: self.source_root,
            mod_cache: self.mod_cache,
            locked_modules,
            ready_modules: self.ready_modules,
            workspace: self.workspace,
            native_input_fs: self.native_input_fs,
        })
    }

    fn check(self) -> Result<(), CompileError> {
        self.analyze()?.prepare_extensions_for_frozen_build()
    }

    fn compile(self) -> Result<CompileOutput, CompileError> {
        self.analyze()?.into_output()
    }
}

fn reject_unfrozen_memory_inputs(context: &ProjectCompileContext) -> Result<(), CompileError> {
    let mod_file = context.project_deps.mod_file();
    let has_external_dependencies = mod_file
        .is_some_and(|mod_file| !mod_file.dependencies.is_empty() || mod_file.extension.is_some())
        || context.project_deps.lock_file().is_some()
        || !context.project_deps.locked_modules().is_empty()
        || !context.workspace_sources.is_empty();
    if !has_external_dependencies {
        return Ok(());
    }
    Err(CompileError::ModuleSystem(
        ModuleSystemError::new(
            ModuleSystemStage::CompileInputs,
            ModuleSystemErrorKind::ValidationFailed,
            "in-memory compilation accepts only self-contained source graphs; external dependencies, workspace sources, and extension metadata require a real project path so every host input can be frozen",
        )
        .with_path(&context.project_root),
    ))
}

impl AnalyzedCompilation {
    fn prepare_extensions_for_frozen_build(&self) -> Result<(), CompileError> {
        prepare_native_extension_specs_with_readiness_and_workspace(
            &self.project.extensions,
            &self.ready_modules,
            &self.mod_cache,
            &self.workspace.options.workspace,
            Some(&self.native_input_fs),
        )
        .map_err(CompileError::ModuleSystem)?;
        Ok(())
    }

    fn into_output(self) -> Result<CompileOutput, CompileError> {
        let extensions = prepare_native_extension_specs_with_readiness_and_workspace(
            &self.project.extensions,
            &self.ready_modules,
            &self.mod_cache,
            &self.workspace.options.workspace,
            Some(&self.native_input_fs),
        )
        .map_err(CompileError::ModuleSystem)?;

        let module =
            compile_project(&self.project).map_err(|e| CompileError::Codegen(format!("{}", e)))?;
        verify_generated_module(&module)?;

        Ok(CompileOutput {
            module,
            source_root: self.source_root,
            extensions,
            locked_modules: self.locked_modules,
        })
    }
}

fn verify_generated_module(module: &Module) -> Result<(), CompileError> {
    vo_common_core::verifier::verify_module(module)
        .map(|_| ())
        .map_err(|err| CompileError::Codegen(format!("generated invalid bytecode: {err}")))
}

fn invalid_bytecode_error(err: impl std::fmt::Display) -> CompileError {
    CompileError::Io(std::io::Error::new(
        std::io::ErrorKind::InvalidData,
        format!("invalid bytecode: {err}"),
    ))
}

fn collect_file_set<F: FileSystem>(
    fs: &F,
    dir: &Path,
    single_file: Option<&Path>,
    abs_root: PathBuf,
    empty_message: &'static str,
) -> Result<FileSet, CompileError> {
    let file_set = if let Some(file_path) = single_file {
        FileSet::from_file(fs, file_path, abs_root)?
    } else {
        FileSet::collect(fs, dir, abs_root)?
    };

    if file_set.files.is_empty() {
        return Err(CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            empty_message,
        )));
    }

    Ok(file_set)
}

pub(super) fn source_root(path: &Path) -> PathBuf {
    if path.is_dir() {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    } else {
        path.canonicalize()
            .unwrap_or_else(|_| path.to_path_buf())
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf()
    }
}

pub(super) fn load_bytecode(path: &Path) -> Result<CompileOutput, CompileError> {
    let bytes = super::host_input::read_stable_regular_file(
        path,
        vo_common_core::serialize::MAX_VOB_BYTES,
    )?;
    let module = vo_vm::bytecode::Module::deserialize(&bytes).map_err(|e| {
        CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("{:?}", e),
        ))
    })?;
    vo_common_core::verifier::verify_module(&module)
        .map(|_| ())
        .map_err(invalid_bytecode_error)?;
    Ok(CompileOutput {
        module,
        source_root: path.parent().unwrap_or(Path::new(".")).to_path_buf(),
        extensions: Vec::new(),
        locked_modules: Vec::new(),
    })
}

pub(super) fn compile_prepared_project<F: FileSystem>(
    fs: F,
    root: &Path,
    single_file: Option<&OsStr>,
) -> Result<CompileOutput, CompileError> {
    let mod_cache = PathBuf::from(IN_MEMORY_MODULE_CACHE_ROOT);
    let options = ProjectContextOptions::new(WorkspaceDiscovery::Disabled);

    // Single-file entries go through the spec §5.6 single-file classifier so
    // that inline `/*vo:mod ... */` metadata is recognized and the spec §5.6.4
    // precedence rules are enforced.
    if let Some(single_file_os) = single_file {
        let file_path = PathBuf::from(single_file_os);
        let ctx =
            vo_module::project::load_single_file_context_with_options(&fs, &file_path, &options)
                .map_err(super::module_system_error_from_project)?;
        return compile_from_single_file_context(fs, ctx, root, file_path, mod_cache);
    }

    let context = vo_module::project::load_project_context_with_options(&fs, root, &options)
        .map_err(super::module_system_error_from_project)?;
    let graph = super::ProjectGraphContext::from_project(&context);
    let (_, project_deps, workspace_sources) = context.into_parts();
    PreparedProject::load_memory_prepared(
        fs,
        ProjectCompileContext {
            project_root: root.to_path_buf(),
            mod_cache,
            source_root: root.to_path_buf(),
            package_dir: PathBuf::from("."),
            single_file: None,
            graph,
            project_deps,
            current_module_override: None,
            workspace_sources,
            workspace: super::WorkspaceCompileContext::disabled(),
        },
        "no .vo files found",
    )?
    .compile()
}

fn with_zip_project<T>(
    zip_path: &Path,
    internal_root: Option<&str>,
    operation: impl FnOnce(PreparedProject<ZipFs>) -> Result<T, CompileError>,
) -> Result<T, CompileError> {
    let archive =
        super::host_input::read_stable_regular_file_snapshot(zip_path, MAX_ZIP_ARCHIVE_BYTES)?;
    let archive_generation = archive.generation.clone();
    let archive_digest: [u8; 32] = Sha256::digest(&archive.bytes).into();
    let zip_fs = ZipFs::from_reader_with_root(
        Cursor::new(archive.bytes.as_slice()),
        internal_root.unwrap_or(""),
    )?;
    drop(archive);

    let archive_root = zip_path
        .canonicalize()
        .unwrap_or_else(|_| zip_path.to_path_buf());
    let virtual_root = Path::new(".");
    let options = ProjectContextOptions::new(WorkspaceDiscovery::Disabled);
    let context =
        vo_module::project::load_project_context_with_options(&zip_fs, virtual_root, &options)
            .map_err(super::module_system_error_from_project)?;
    let graph = super::ProjectGraphContext::from_project(&context);
    let (_, project_deps, workspace_sources) = context.into_parts();
    let project = PreparedProject::load_memory_prepared(
        zip_fs,
        ProjectCompileContext {
            project_root: archive_root.clone(),
            mod_cache: PathBuf::from(IN_MEMORY_MODULE_CACHE_ROOT),
            source_root: archive_root,
            package_dir: PathBuf::from("."),
            single_file: None,
            graph,
            project_deps,
            current_module_override: None,
            workspace_sources,
            workspace: super::WorkspaceCompileContext::disabled(),
        },
        "no .vo files found in zip",
    )?;
    let result = operation(project)?;

    let live_archive =
        super::host_input::read_stable_regular_file_snapshot(zip_path, MAX_ZIP_ARCHIVE_BYTES)?;
    let live_digest: [u8; 32] = Sha256::digest(&live_archive.bytes).into();
    if live_archive.generation != archive_generation || live_digest != archive_digest {
        return Err(CompileError::ModuleSystem(
            ModuleSystemError::new(
                ModuleSystemStage::CompileInputs,
                ModuleSystemErrorKind::Mismatch,
                "zip archive changed while its immutable compile snapshot was in use",
            )
            .with_path(zip_path),
        ));
    }

    Ok(result)
}

pub(super) fn compile_zip(
    zip_path: &Path,
    internal_root: Option<&str>,
) -> Result<CompileOutput, CompileError> {
    with_zip_project(zip_path, internal_root, PreparedProject::compile)
}

pub(super) fn check_zip(zip_path: &Path, internal_root: Option<&str>) -> Result<(), CompileError> {
    with_zip_project(zip_path, internal_root, PreparedProject::check)
}

pub(super) fn parse_zip_path(path: &str) -> Option<(String, Option<String>)> {
    if let Some((prefix, internal_root)) = path.rsplit_once(".zip:") {
        return Some((format!("{prefix}.zip"), Some(internal_root.to_string())));
    }
    path.ends_with(".zip").then(|| (path.to_string(), None))
}

fn compile_from_single_file_context<F: FileSystem>(
    fs: F,
    ctx: SingleFileContext,
    compile_root: &Path,
    file_path: PathBuf,
    mod_cache: PathBuf,
) -> Result<CompileOutput, CompileError> {
    let project_context =
        single_file_context_to_project_compile_context(ctx, compile_root, file_path, mod_cache)?;
    PreparedProject::load_memory_prepared(fs, project_context, "no .vo files found")?.compile()
}

fn single_file_context_to_project_compile_context(
    ctx: SingleFileContext,
    compile_root: &Path,
    file_path: PathBuf,
    mod_cache: PathBuf,
) -> Result<ProjectCompileContext, CompileError> {
    match ctx {
        SingleFileContext::Project(project_context) => {
            let graph = super::ProjectGraphContext::from_project(&project_context);
            let (_, project_deps, workspace_sources) = project_context.into_parts();
            let package_dir = file_path
                .parent()
                .filter(|parent| !parent.as_os_str().is_empty())
                .unwrap_or_else(|| Path::new("."))
                .to_path_buf();
            Ok(ProjectCompileContext {
                project_root: compile_root.to_path_buf(),
                mod_cache,
                source_root: compile_root.to_path_buf(),
                package_dir,
                single_file: Some(file_path),
                graph,
                project_deps,
                current_module_override: None,
                workspace_sources,
                workspace: super::WorkspaceCompileContext::disabled(),
            })
        }
        SingleFileContext::EphemeralInlineMod { inline_mod, .. } => {
            let current_module = inline_mod.module.as_str().to_string();
            Ok(ProjectCompileContext {
                project_root: compile_root.to_path_buf(),
                mod_cache,
                source_root: compile_root.to_path_buf(),
                package_dir: PathBuf::from("."),
                single_file: Some(file_path),
                graph: super::ProjectGraphContext::empty(),
                project_deps: ProjectDeps::default(),
                current_module_override: Some(current_module),
                workspace_sources: HashMap::new(),
                workspace: super::WorkspaceCompileContext::disabled(),
            })
        }
        SingleFileContext::AdHoc { .. } => Ok(ProjectCompileContext {
            project_root: compile_root.to_path_buf(),
            mod_cache,
            source_root: compile_root.to_path_buf(),
            package_dir: PathBuf::from("."),
            single_file: Some(file_path),
            graph: super::ProjectGraphContext::empty(),
            project_deps: ProjectDeps::default(),
            current_module_override: None,
            workspace_sources: HashMap::new(),
            workspace: super::WorkspaceCompileContext::disabled(),
        }),
    }
}

pub(super) fn compile_with_project_snapshot(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    snapshot: Arc<CompileInputSnapshot>,
) -> Result<CompileOutput, CompileError> {
    load_project_from_snapshot(context, stdlib, snapshot)?.compile()
}

pub(super) fn check_with_project_snapshot(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    snapshot: Arc<CompileInputSnapshot>,
) -> Result<(), CompileError> {
    load_project_from_snapshot(context, stdlib, snapshot)?.check()
}

fn load_project_from_snapshot(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    snapshot: Arc<CompileInputSnapshot>,
) -> Result<PreparedProject<ResolverFs>, CompileError> {
    let context_fs = ResolverFs::snapshot_global(Arc::clone(&snapshot));
    validate_captured_project_context(
        &context_fs,
        &context.project_root,
        &context.graph,
        &context.project_deps,
        &context.workspace_sources,
        context.current_module_override.as_deref(),
        &context.workspace,
    )?;
    let project_fs = ResolverFs::snapshot(Arc::clone(&snapshot), &context.project_root);
    let module_fs = ResolverFs::snapshot(Arc::clone(&snapshot), &context.mod_cache);
    let workspace_source_fs = ResolverFs::snapshot_global(snapshot);
    PreparedProject::load_prepared_with_inputs(
        project_fs,
        context,
        "no .vo files found",
        Some(stdlib),
        module_fs,
        workspace_source_fs,
    )
}

pub(super) fn validate_captured_project_context<F: FileSystem>(
    snapshot_fs: &F,
    project_root: &Path,
    expected_graph: &super::ProjectGraphContext,
    expected: &ProjectDeps,
    workspace_sources: &HashMap<String, PathBuf>,
    current_module_override: Option<&str>,
    workspace: &super::WorkspaceCompileContext,
) -> Result<(), CompileError> {
    let captured_context = vo_module::project::load_project_context_with_options(
        snapshot_fs,
        project_root,
        &workspace.options,
    )
    .map_err(captured_context_reload_error)?;

    if normalize_fs_path(captured_context.project_root()) != normalize_fs_path(project_root) {
        return Err(captured_context_mismatch(
            ModuleSystemStage::ModFile,
            "project root",
        ));
    }
    if normalized_optional_path(captured_context.workspace_file())
        != normalized_optional_path(workspace.file.as_deref())
    {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "vo.work provenance",
        ));
    }
    if normalized_workspace_sources(captured_context.workspace_sources())
        != normalized_workspace_sources(workspace_sources)
    {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "vo.work source map",
        ));
    }
    validate_captured_project_graph(&captured_context, expected_graph)?;

    // Inline ephemeral dependencies live in a cache-local project, while this
    // filesystem is rooted beside the source file. Their typed dependency
    // context is fingerprinted separately. Here we enforce the classifier
    // invariant that a host vo.mod did not appear after inline selection.
    if current_module_override.is_some() {
        if captured_context.project_deps().has_mod_file() {
            return Err(captured_context_mismatch(
                ModuleSystemStage::ModFile,
                "vo.mod",
            ));
        }
        return Ok(());
    }

    let expected_mod = render_project_mod(expected)?;
    let captured_mod = render_project_mod(captured_context.project_deps())?;
    if expected_mod != captured_mod {
        return Err(captured_context_mismatch(
            ModuleSystemStage::ModFile,
            "vo.mod",
        ));
    }

    let expected_lock = render_project_lock(expected)?;
    let captured_lock = render_project_lock(captured_context.project_deps())?;
    if expected_lock != captured_lock {
        return Err(captured_context_mismatch(
            ModuleSystemStage::LockFile,
            "vo.lock",
        ));
    }
    Ok(())
}

fn validate_captured_project_graph(
    captured: &ProjectContext,
    expected: &super::ProjectGraphContext,
) -> Result<(), CompileError> {
    // Ephemeral/ad-hoc contexts have no ProjectContext graph. Their source
    // classification generation is validated during bounded input capture.
    if expected.project_metadata_generation.is_empty() {
        return Ok(());
    }
    if captured.authority() != expected.authority {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "project dependency authority",
        ));
    }
    if captured.project_metadata_generation() != expected.project_metadata_generation {
        return Err(captured_context_mismatch(
            ModuleSystemStage::ModFile,
            "project metadata generation",
        ));
    }
    if normalized_workspace_modules(captured.workspace_modules())?
        != normalized_workspace_modules(&expected.workspace_modules)?
    {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "workspace authority graph",
        ));
    }
    if normalized_input_files(captured.validated_input_files())
        != normalized_input_files(&expected.validated_input_files)
    {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "project authority input closure",
        ));
    }
    Ok(())
}

pub(super) fn validate_live_workspace_generation(
    project_root: &Path,
    workspace: &super::WorkspaceCompileContext,
) -> Result<(), CompileError> {
    if workspace.generation.is_empty() {
        return Ok(());
    }
    if workspace.file.is_none() {
        let live_workspace = vo_module::workspace::discover_workfile_in_with(
            &RealFs::new("."),
            project_root,
            &workspace.options.workspace,
        )
        .map_err(live_workspace_reload_error)?;
        if live_workspace.is_some() {
            return Err(captured_context_mismatch(
                ModuleSystemStage::Workspace,
                "workspace directory generation",
            ));
        }
        return Ok(());
    }
    let live_context = vo_module::project::load_project_context_with_options(
        &RealFs::new("."),
        project_root,
        &workspace.options,
    )
    .map_err(captured_context_reload_error)?;
    if live_context.workspace_generation() != workspace.generation {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "workspace directory generation",
        ));
    }
    Ok(())
}

fn live_workspace_reload_error(error: vo_module::Error) -> CompileError {
    let kind = match &error {
        vo_module::Error::Io(_) => ModuleSystemErrorKind::ReadFailed,
        vo_module::Error::WorkFileParse(_) => ModuleSystemErrorKind::ParseFailed,
        _ => ModuleSystemErrorKind::ValidationFailed,
    };
    CompileError::ModuleSystem(ModuleSystemError::new(
        ModuleSystemStage::Workspace,
        kind,
        format!("live workspace generation cannot be reloaded: {error}"),
    ))
}

fn captured_context_reload_error(error: vo_module::project::ProjectDepsError) -> CompileError {
    let stage = match error.stage() {
        vo_module::project::ProjectDepsStage::Workspace => ModuleSystemStage::Workspace,
        vo_module::project::ProjectDepsStage::ModFile => ModuleSystemStage::ModFile,
        vo_module::project::ProjectDepsStage::LockFile => ModuleSystemStage::LockFile,
    };
    CompileError::ModuleSystem(ModuleSystemError::new(
        stage,
        ModuleSystemErrorKind::Mismatch,
        format!(
            "captured project context cannot be reloaded from one metadata generation: {error}"
        ),
    ))
}

fn normalized_optional_path(path: Option<&Path>) -> Option<PathBuf> {
    path.map(normalize_fs_path)
}

fn normalized_workspace_sources(
    workspace_sources: &HashMap<String, PathBuf>,
) -> Vec<(String, PathBuf)> {
    let mut entries = workspace_sources
        .iter()
        .map(|(module, path)| (module.clone(), normalize_fs_path(path)))
        .collect::<Vec<_>>();
    entries.sort();
    entries
}

fn normalized_workspace_modules(
    modules: &[WorkspaceModule],
) -> Result<Vec<(String, PathBuf, String)>, CompileError> {
    let mut entries = modules
        .iter()
        .map(|module| {
            let declaration = module.mod_file().render().map_err(|error| {
                CompileError::ModuleSystem(ModuleSystemError::new(
                    ModuleSystemStage::Workspace,
                    ModuleSystemErrorKind::ValidationFailed,
                    format!(
                        "failed to render authorized workspace manifest for {}: {error}",
                        module.module()
                    ),
                ))
            })?;
            Ok((
                module.module().as_str().to_string(),
                normalize_fs_path(module.directory()),
                declaration,
            ))
        })
        .collect::<Result<Vec<_>, CompileError>>()?;
    entries.sort();
    Ok(entries)
}

fn normalized_input_files(paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut entries = paths
        .iter()
        .map(|path| normalize_fs_path(path))
        .collect::<Vec<_>>();
    entries.sort();
    entries.dedup();
    entries
}

fn render_project_mod(project_deps: &ProjectDeps) -> Result<Option<String>, CompileError> {
    project_deps
        .mod_file()
        .map(|mod_file| mod_file.render())
        .transpose()
        .map_err(|error| {
            CompileError::ModuleSystem(ModuleSystemError::new(
                ModuleSystemStage::ModFile,
                ModuleSystemErrorKind::ValidationFailed,
                format!("failed to render loaded vo.mod metadata: {error}"),
            ))
        })
}

fn render_project_lock(project_deps: &ProjectDeps) -> Result<Option<String>, CompileError> {
    project_deps
        .lock_file()
        .map(|lock_file| lock_file.render())
        .transpose()
        .map_err(|error| {
            CompileError::ModuleSystem(ModuleSystemError::new(
                ModuleSystemStage::LockFile,
                ModuleSystemErrorKind::ValidationFailed,
                format!("failed to render loaded vo.lock metadata: {error}"),
            ))
        })
}

fn captured_context_mismatch(stage: ModuleSystemStage, file: &str) -> CompileError {
    CompileError::ModuleSystem(ModuleSystemError::new(
        stage,
        ModuleSystemErrorKind::Mismatch,
        format!(
            "captured {file} does not match the project context loaded before snapshot capture; retry the build after concurrent project metadata updates finish"
        ),
    ))
}
