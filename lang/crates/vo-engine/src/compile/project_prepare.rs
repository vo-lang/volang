use std::collections::HashMap;
use std::path::{Path, PathBuf};

use vo_analysis::vfs::{ModSource, PackageResolverMixed, ReplacingResolver, StdSource};
use vo_common::vfs::{FileSystem, RealFs};
use vo_module::project::{ProjectDeps, ProjectDepsError, ProjectDepsErrorKind, ProjectDepsStage};
use vo_stdlib::EmbeddedStdlib;

use super::{default_mod_cache_root, CompileError, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage};

fn workspace_error_to_compile_error(error: vo_module::Error) -> CompileError {
    let kind = match &error {
        vo_module::Error::Io(_) => ModuleSystemErrorKind::ReadFailed,
        vo_module::Error::WorkFileParse(_) => ModuleSystemErrorKind::ParseFailed,
        _ => ModuleSystemErrorKind::ValidationFailed,
    };
    CompileError::ModuleSystem(ModuleSystemError::new(
        ModuleSystemStage::Workspace,
        kind,
        error.to_string(),
    ))
}

pub(super) fn read_all_replaces(root: &Path) -> Result<HashMap<String, PathBuf>, CompileError> {
    read_all_replaces_with_fs(&RealFs::new("."), root)
}

pub(super) fn read_all_replaces_with_fs<F: FileSystem>(
    fs: &F,
    root: &Path,
) -> Result<HashMap<String, PathBuf>, CompileError> {
    vo_module::workspace::load_workspace_replaces(fs, root, None)
        .map_err(workspace_error_to_compile_error)
}

pub(super) fn project_deps_error_to_module_system(error: ProjectDepsError) -> ModuleSystemError {
    let stage = match error.stage {
        ProjectDepsStage::ModFile => ModuleSystemStage::ModFile,
        ProjectDepsStage::LockFile => ModuleSystemStage::LockFile,
    };
    let kind = match error.kind {
        ProjectDepsErrorKind::Missing => ModuleSystemErrorKind::Missing,
        ProjectDepsErrorKind::ReadFailed => ModuleSystemErrorKind::ReadFailed,
        ProjectDepsErrorKind::ParseFailed => ModuleSystemErrorKind::ParseFailed,
        ProjectDepsErrorKind::ValidationFailed => ModuleSystemErrorKind::ValidationFailed,
    };
    let mut mapped = ModuleSystemError::new(stage, kind, error.detail);
    if let Some(path) = error.path.as_ref() {
        mapped = mapped.with_path(path);
    }
    mapped
}

pub(super) fn load_project_deps_for_engine<F: FileSystem>(
    fs: &F,
    workspace_replaces: &HashMap<String, PathBuf>,
) -> Result<ProjectDeps, CompileError> {
    let excluded_modules = workspace_replaces.keys().cloned().collect::<Vec<_>>();
    vo_module::project::read_project_deps(fs, &excluded_modules)
        .map_err(|error| CompileError::ModuleSystem(project_deps_error_to_module_system(error)))
}

pub(super) fn create_resolver(plan: &ProjectDeps) -> PackageResolverMixed<EmbeddedStdlib, RealFs> {
    let mod_root = default_mod_cache_root();
    PackageResolverMixed {
        std: StdSource::with_fs(EmbeddedStdlib::new()),
        r#mod: ModSource::with_fs(RealFs::new(mod_root)).with_project_deps(plan),
    }
}

pub(super) fn replacing_resolver(
    plan: &ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
) -> ReplacingResolver<PackageResolverMixed<EmbeddedStdlib, RealFs>> {
    ReplacingResolver::new(create_resolver(plan), workspace_replaces)
}
