use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;

use crate::identity::ModulePath;
use crate::registry::Registry;
use crate::schema::lockfile::LockFile;
use crate::schema::modfile::ModFile;
use crate::solver::{self, ResolvedGraph, SolvePreferences};
use crate::version::{ExactVersion, ToolchainConstraint};
use crate::Error;

pub fn download_locked_dependencies(
    cache_root: &Path,
    lock_file: &LockFile,
    registry: &dyn Registry,
) -> Result<(), Error> {
    crate::cache::install::populate_locked_cache(cache_root, lock_file, registry)
}

pub(crate) fn prepare_lock_file(
    mod_file: &ModFile,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
    created_by: &str,
) -> Result<Option<LockFile>, Error> {
    let reqs = solved_requirements(mod_file);
    if reqs.is_empty() {
        return Ok(None);
    }
    let graph = solve_from_requirements(mod_file, &reqs, registry, prefs)?;
    Ok(Some(crate::lock::generate_lock(
        mod_file, &graph, created_by,
    )?))
}

pub(crate) fn verify_locked_dependencies(
    cache_root: &Path,
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
    let excluded_modules = mod_file
        .replace
        .iter()
        .map(|replace| replace.module.as_str().to_string())
        .collect::<Vec<_>>();
    crate::lock::verify_root_consistency(mod_file, lock_file)?;
    crate::lock::verify_graph_completeness(mod_file, lock_file, &excluded_modules)?;
    crate::cache::install::verify_locked_cache(cache_root, lock_file)
}

pub(crate) fn latest_supported_requirement_version(
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

pub(crate) fn infer_module_path(
    import_path: &str,
    registry: &dyn Registry,
) -> Result<ModulePath, Error> {
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

pub(crate) fn clean_cache(cache_root: &Path, lock_file: Option<&LockFile>) -> Result<u64, Error> {
    if !cache_root.exists() {
        return Ok(0);
    }

    let locked_dirs: BTreeSet<PathBuf> = lock_file
        .map(|lock_file| {
            lock_file
                .resolved
                .iter()
                .map(|locked| {
                    crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version)
                })
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

fn solved_requirements(mod_file: &ModFile) -> Vec<(ModulePath, crate::version::DepConstraint)> {
    let replaced = mod_file
        .replace
        .iter()
        .map(|replace| replace.module.as_str())
        .collect::<BTreeSet<_>>();
    mod_file
        .require
        .iter()
        .filter(|require| !replaced.contains(require.module.as_str()))
        .map(|require| (require.module.clone(), require.constraint.clone()))
        .collect::<Vec<_>>()
}

fn solve_from_requirements(
    mod_file: &ModFile,
    reqs: &[(ModulePath, crate::version::DepConstraint)],
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<ResolvedGraph, Error> {
    solver::solve(
        mod_file.module.as_str(),
        &mod_file.vo,
        reqs,
        registry,
        prefs,
    )
}
