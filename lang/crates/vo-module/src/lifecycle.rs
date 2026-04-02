use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::path::Path;

use vo_common::vfs::{FileSystem, RealFs};

use crate::identity::ModulePath;
use crate::registry::Registry;
use crate::schema::lockfile::{LockFile, LockedModule};
use crate::schema::modfile::ModFile;
use crate::solver::{self, ResolvedGraph, SolvePreferences};
use crate::version::{ExactVersion, ToolchainConstraint};
use crate::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSelection {
    pub module: ModulePath,
    pub version: ExactVersion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LockedDependencyState {
    pub module: String,
    pub version: String,
    pub cached: bool,
}

#[derive(Debug, Clone)]
pub struct ProjectLockedDependencyPlan {
    pub project_deps: crate::project::ProjectDeps,
    pub dependency_state: Vec<LockedDependencyState>,
}

impl ModuleSelection {
    pub fn new(module: ModulePath, version: ExactVersion) -> Self {
        Self { module, version }
    }

    pub fn parse(module: &str, version: &str) -> Result<Self, Error> {
        Ok(Self::new(ModulePath::parse(module)?, ExactVersion::parse(version)?))
    }

    pub fn from_locked_module(locked: &LockedModule) -> Self {
        Self::new(locked.path.clone(), locked.version.clone())
    }

    pub fn spec(&self) -> String {
        format!("{}@{}", self.module.as_str(), self.version)
    }
}

#[derive(Debug, Default)]
pub struct ModuleSelectionPlanner {
    stack: Vec<ModuleSelection>,
    visited: BTreeSet<String>,
    selected_versions: BTreeMap<String, String>,
}

impl ModuleSelectionPlanner {
    pub fn new(initial: Vec<ModuleSelection>) -> Result<Self, Error> {
        let mut planner = Self::default();
        planner.push_all(initial)?;
        Ok(planner)
    }

    pub fn push_all<I>(&mut self, selections: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = ModuleSelection>,
    {
        for selection in selections {
            remember_selected_version(&mut self.selected_versions, &selection)?;
            self.stack.push(selection);
        }
        Ok(())
    }

    pub fn next(&mut self) -> Option<ModuleSelection> {
        while let Some(selection) = self.stack.pop() {
            if self.visited.insert(selection.spec()) {
                return Some(selection);
            }
        }
        None
    }
}

pub fn locked_module_selections(locked_modules: &[LockedModule]) -> Vec<ModuleSelection> {
    locked_modules
        .iter()
        .map(ModuleSelection::from_locked_module)
        .collect()
}

pub fn plan_locked_dependencies<F: FileSystem>(
    cache_fs: &F,
    locked_modules: &[LockedModule],
) -> Vec<LockedDependencyState> {
    locked_modules
        .iter()
        .map(|locked| LockedDependencyState {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            cached: locked_module_fully_cached(cache_fs, locked),
        })
        .collect()
}

pub fn plan_locked_dependencies_in(
    cache_root: &Path,
    locked_modules: &[LockedModule],
) -> Vec<LockedDependencyState> {
    plan_locked_dependencies(&RealFs::new(cache_root), locked_modules)
}

pub fn load_project_locked_dependency_plan(
    project_root: &Path,
    cache_root: &Path,
) -> Result<ProjectLockedDependencyPlan, crate::project::ProjectDepsError> {
    let project_deps = crate::project::load_project_context(&RealFs::new("."), project_root)?.project_deps;
    let dependency_state = plan_locked_dependencies_in(cache_root, project_deps.locked_modules());
    Ok(ProjectLockedDependencyPlan {
        project_deps,
        dependency_state,
    })
}

pub fn download_locked_dependencies(
    cache_root: &Path,
    lock_file: &LockFile,
    registry: &dyn Registry,
) -> Result<(), Error> {
    crate::cache::install::populate_locked_cache(cache_root, lock_file, registry)
}

pub fn prepare_lock_file(
    mod_file: &ModFile,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
    created_by: &str,
) -> Result<Option<LockFile>, Error> {
    if mod_file.require.is_empty() {
        return Ok(None);
    }
    let graph = solve_from_modfile(mod_file, registry, prefs)?;
    Ok(Some(crate::lock::generate_lock(mod_file, &graph, created_by)?))
}

pub fn verify_locked_dependencies(
    cache_root: &Path,
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
    crate::lock::verify_root_consistency(mod_file, lock_file)?;
    crate::lock::verify_graph_completeness(mod_file, lock_file)?;
    crate::cache::install::verify_locked_cache(cache_root, lock_file)
}

pub fn latest_supported_requirement_version(
    project_vo: &ToolchainConstraint,
    module: &ModulePath,
    registry: &dyn Registry,
) -> Result<ExactVersion, Error> {
    let versions = registry.list_versions(module)?;
    let mut compatible = crate::registry::filter_compatible_versions(module, &versions)
        .into_iter()
        .filter(|version| !version.semver().is_prerelease())
        .collect::<Vec<_>>();
    compatible.sort_by(|left, right| right.cmp(left));

    let mut first_toolchain_mismatch: Option<String> = None;
    for version in compatible {
        let (manifest, _) = registry.fetch_manifest_raw(module, &version)?;
        crate::registry::validate_manifest(&manifest, module, &version)?;
        if project_vo.is_subset_of(&manifest.vo) {
            return Ok(version);
        }
        if first_toolchain_mismatch.is_none() {
            first_toolchain_mismatch = Some(manifest.vo.to_string());
        }
    }

    if let Some(dependency_constraint) = first_toolchain_mismatch {
        return Err(Error::DependencyToolchainMismatch {
            module: module.as_str().to_string(),
            project_constraint: project_vo.to_string(),
            dependency_constraint,
        });
    }

    Err(Error::NoSatisfyingVersion {
        module: module.as_str().to_string(),
        detail: "no non-prerelease versions found".to_string(),
    })
}

pub fn infer_module_path(import_path: &str, registry: &dyn Registry) -> Result<ModulePath, Error> {
    let segments: Vec<&str> = import_path.split('/').collect();
    if segments.len() < 3 {
        return Err(Error::InvalidImportPath(format!(
            "cannot infer module from import path: {import_path}"
        )));
    }

    let mut first_error: Option<Error> = None;
    for end in (3..=segments.len()).rev() {
        let candidate_str = segments[..end].join("/");
        let candidate = match ModulePath::parse(&candidate_str) {
            Ok(candidate) => candidate,
            Err(_) => continue,
        };
        match registry.probe_module_path(&candidate) {
            Ok(true) => return Ok(candidate),
            Ok(false) => {}
            Err(error) => {
                if first_error.is_none() {
                    first_error = Some(error);
                }
            }
        }
    }

    if let Some(error) = first_error {
        return Err(error);
    }

    Err(Error::NoSatisfyingVersion {
        module: import_path.to_string(),
        detail: "could not infer a published owning module for import".to_string(),
    })
}

pub fn clean_cache(cache_root: &Path, lock_file: Option<&LockFile>) -> Result<u64, Error> {
    if !cache_root.exists() {
        return Ok(0);
    }

    let locked_dirs: BTreeSet<PathBuf> = lock_file
        .map(|lock_file| {
            lock_file
                .resolved
                .iter()
                .map(|locked| crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version))
                .collect()
        })
        .unwrap_or_default();

    let mut removed = 0u64;
    for entry in std::fs::read_dir(cache_root)? {
        let entry = entry?;
        let module_dir = entry.path();
        if !module_dir.is_dir() {
            continue;
        }
        for version_entry in std::fs::read_dir(&module_dir)? {
            let version_entry = version_entry?;
            let version_dir = version_entry.path();
            if !version_dir.is_dir() || locked_dirs.contains(&version_dir) {
                continue;
            }
            std::fs::remove_dir_all(&version_dir)?;
            removed += 1;
        }
        if std::fs::read_dir(&module_dir)?.next().is_none() {
            std::fs::remove_dir(&module_dir)?;
        }
    }

    Ok(removed)
}

fn solve_from_modfile(
    mod_file: &ModFile,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<ResolvedGraph, Error> {
    let reqs = mod_file
        .require
        .iter()
        .map(|require| (require.module.clone(), require.constraint.clone()))
        .collect::<Vec<_>>();
    solver::solve(&mod_file.module, &mod_file.vo, &reqs, registry, prefs)
}

pub fn normalize_locked_modules(locked_modules: Vec<LockedModule>) -> Result<Vec<LockedModule>, Error> {
    let mut selected_versions = BTreeMap::new();
    let mut visited = BTreeSet::new();
    let mut normalized = Vec::new();

    for locked in locked_modules {
        let selection = ModuleSelection::from_locked_module(&locked);
        remember_selected_version(&mut selected_versions, &selection)?;
        if !visited.insert(selection.spec()) {
            continue;
        }
        normalized.push(locked);
    }

    Ok(normalized)
}

pub fn walk_module_selection_closure<F>(
    initial: Vec<ModuleSelection>,
    mut dependencies_of: F,
) -> Result<Vec<ModuleSelection>, Error>
where
    F: FnMut(&ModuleSelection) -> Result<Vec<ModuleSelection>, Error>,
{
    let mut planner = ModuleSelectionPlanner::new(initial)?;
    let mut resolved = Vec::new();

    while let Some(selection) = planner.next() {
        let deps = dependencies_of(&selection)?;
        planner.push_all(deps)?;
        resolved.push(selection);
    }

    Ok(resolved)
}

fn remember_selected_version(
    selected_versions: &mut BTreeMap<String, String>,
    selection: &ModuleSelection,
) -> Result<(), Error> {
    let module = selection.module.as_str();
    let version = selection.version.to_string();
    match selected_versions.get(module) {
        Some(existing) if existing != &version => Err(Error::SelectedVersionConflict {
            module: module.to_string(),
            existing: existing.clone(),
            requested: version,
        }),
        Some(_) => Ok(()),
        None => {
            selected_versions.insert(module.to_string(), version);
            Ok(())
        }
    }
}

fn locked_module_fully_cached<F: FileSystem>(cache_fs: &F, locked: &LockedModule) -> bool {
    let module_dir = crate::cache::layout::relative_module_dir(locked.path.as_str(), &locked.version);
    if crate::cache::validate::validate_installed_module(cache_fs, &module_dir, locked).is_err() {
        return false;
    }
    locked.artifacts.iter().all(|artifact| {
        let artifact_path = module_dir.join("artifacts").join(&artifact.id.name);
        crate::cache::validate::validate_installed_artifact(cache_fs, &artifact_path, locked, artifact)
            .is_ok()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::schema::lockfile::LockedModule;
    use crate::version::ToolchainConstraint;
    use vo_common::vfs::MemoryFs;

    fn selection(module: &str, version: &str) -> ModuleSelection {
        ModuleSelection::parse(module, version).unwrap()
    }

    fn locked(module: &str, version: &str) -> LockedModule {
        LockedModule {
            path: ModulePath::parse(module).unwrap(),
            version: ExactVersion::parse(version).unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            commit: "1111111111111111111111111111111111111111".to_string(),
            release_manifest: Digest::parse(
                "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            )
            .unwrap(),
            source: Digest::parse(
                "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            )
            .unwrap(),
            deps: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    fn locked_cached(module: &str, version: &str, manifest_raw: &[u8], source_raw: &[u8]) -> LockedModule {
        LockedModule {
            path: ModulePath::parse(module).unwrap(),
            version: ExactVersion::parse(version).unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            commit: "2222222222222222222222222222222222222222".to_string(),
            release_manifest: Digest::from_sha256(manifest_raw),
            source: Digest::from_sha256(source_raw),
            deps: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    #[test]
    fn test_normalize_locked_modules_rejects_conflicting_versions() {
        let err = normalize_locked_modules(vec![
            locked("github.com/acme/lib", "v1.0.0"),
            locked("github.com/acme/lib", "v2.0.0"),
        ])
        .unwrap_err();

        assert!(matches!(err, Error::SelectedVersionConflict { .. }));
    }

    #[test]
    fn test_walk_module_selection_closure_dedupes_revisited_modules() {
        let resolved = walk_module_selection_closure(
            vec![selection("github.com/acme/app", "v1.0.0")],
            |current| match current.module.as_str() {
                "github.com/acme/app" => Ok(vec![
                    selection("github.com/acme/lib", "v1.0.0"),
                    selection("github.com/acme/util", "v1.0.0"),
                ]),
                "github.com/acme/lib" => Ok(vec![selection("github.com/acme/util", "v1.0.0")]),
                _ => Ok(Vec::new()),
            },
        )
        .unwrap();

        let specs = resolved.into_iter().map(|selection| selection.spec()).collect::<Vec<_>>();
        assert_eq!(
            specs,
            vec![
                "github.com/acme/app@v1.0.0",
                "github.com/acme/util@v1.0.0",
                "github.com/acme/lib@v1.0.0",
            ]
        );
    }

    #[test]
    fn test_walk_module_selection_closure_rejects_conflicting_versions() {
        let err = walk_module_selection_closure(
            vec![selection("github.com/acme/app", "v1.0.0")],
            |current| match current.module.as_str() {
                "github.com/acme/app" => Ok(vec![
                    selection("github.com/acme/lib", "v1.0.0"),
                    selection("github.com/acme/lib", "v2.0.0"),
                ]),
                _ => Ok(Vec::new()),
            },
        )
        .unwrap_err();

        assert!(matches!(err, Error::SelectedVersionConflict { .. }));
    }

    #[test]
    fn test_plan_locked_dependencies_reports_uncached_module() {
        let locked = locked("github.com/acme/lib", "v1.0.0");
        let planned = plan_locked_dependencies(&MemoryFs::new(), std::slice::from_ref(&locked));
        assert_eq!(planned.len(), 1);
        assert_eq!(planned[0].module, "github.com/acme/lib");
        assert_eq!(planned[0].version, "v1.0.0");
        assert!(!planned[0].cached);
    }

    #[test]
    fn test_plan_locked_dependencies_reports_cached_module() {
        let manifest_raw = br#"{"schema_version":1}"#;
        let source_raw = b"source-package";
        let locked = locked_cached("github.com/acme/lib", "v1.0.0", manifest_raw, source_raw);
        let module_dir = crate::cache::layout::relative_module_dir(locked.path.as_str(), &locked.version);
        let mut fs = MemoryFs::new();
        fs.add_file(
            module_dir.join("vo.mod"),
            format!("module {}\nvo {}\n", locked.path, locked.vo),
        );
        fs.add_file(
            module_dir.join(crate::cache::layout::VERSION_MARKER),
            format!("{}\n", locked.version),
        );
        fs.add_file(
            module_dir.join(crate::cache::layout::SOURCE_DIGEST_MARKER),
            format!("{}\n", locked.source),
        );
        fs.add_file(module_dir.join("vo.release.json"), String::from_utf8(manifest_raw.to_vec()).unwrap());

        let planned = plan_locked_dependencies(&fs, std::slice::from_ref(&locked));
        assert_eq!(planned.len(), 1);
        assert!(planned[0].cached);
    }
}
