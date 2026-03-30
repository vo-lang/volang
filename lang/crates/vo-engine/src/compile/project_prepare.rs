use std::collections::HashMap;
use std::path::{Path, PathBuf};

use vo_analysis::vfs::{ModSource, PackageResolverMixed, ReplacingResolver, StdSource};
use vo_common::vfs::{FileSystem, RealFs};
use vo_module::identity::ModulePath;
use vo_module::project::{ProjectDeps, ProjectDepsError, ProjectDepsErrorKind, ProjectDepsStage};
use vo_module::schema::modfile::ModFile;
use vo_stdlib::EmbeddedStdlib;

use super::{default_mod_cache_root, CompileError, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage};
use super::native::locked_module_cache_relative_dir;

fn try_read_root_module(root: &Path) -> Option<ModulePath> {
    let mod_path = root.join("vo.mod");
    let content = std::fs::read_to_string(mod_path).ok()?;
    let mod_file = ModFile::parse(&content).ok()?;
    Some(mod_file.module)
}

pub(super) fn read_all_replaces(root: &Path) -> Result<HashMap<String, PathBuf>, CompileError> {
    use vo_module::schema::workfile::WorkFile;
    use vo_module::workspace;

    let Some(workfile_path) = workspace::discover_workfile(root) else {
        return Ok(HashMap::new());
    };
    let workfile_dir = workfile_path.parent().unwrap_or(root);
    let content = std::fs::read_to_string(&workfile_path).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::Workspace,
            ModuleSystemErrorKind::ReadFailed,
            format!("vo.work read error: {}", e),
        )
        .with_path(&workfile_path)
    })?;
    let workfile = WorkFile::parse(&content).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::Workspace,
            ModuleSystemErrorKind::ParseFailed,
            format!("vo.work parse error: {}", e),
        )
        .with_path(&workfile_path)
    })?;
    let root_module = try_read_root_module(root);
    let overrides = workspace::resolve_validated_overrides(&workfile, workfile_dir, root_module.as_ref()).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::Workspace,
            ModuleSystemErrorKind::ValidationFailed,
            format!("vo.work error: {}", e),
        )
        .with_path(&workfile_path)
    })?;

    let canonical_root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    let mut map = HashMap::new();
    for ov in overrides {
        let abs = ov.local_dir.canonicalize().unwrap_or(ov.local_dir);
        if abs == canonical_root {
            continue;
        }
        map.insert(ov.module.as_str().to_string(), abs);
    }
    Ok(map)
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
    let mut mod_source = ModSource::with_fs(RealFs::new(mod_root.clone()));
    if plan.has_mod_file {
        mod_source = mod_source.with_allowed_modules(plan.allowed_modules.clone());
    }
    if !plan.locked_modules.is_empty() {
        mod_source = mod_source.with_module_roots(plan.locked_modules.iter().map(|locked| {
            let rel = locked_module_cache_relative_dir(locked);
            (locked.path.as_str().to_string(), rel)
        }));
    }

    PackageResolverMixed {
        std: StdSource::with_fs(EmbeddedStdlib::new()),
        r#mod: mod_source,
    }
}

pub(super) fn replacing_resolver(
    plan: &ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
) -> ReplacingResolver<PackageResolverMixed<EmbeddedStdlib, RealFs>> {
    ReplacingResolver::new(create_resolver(plan), workspace_replaces)
}
