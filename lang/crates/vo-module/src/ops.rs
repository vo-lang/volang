use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use crate::identity::{classify_import, find_owning_module, ImportClass, ModulePath};
use crate::lock;
use crate::registry::Registry;
use crate::schema::lockfile::LockFile;
use crate::schema::modfile::{ModFile, Require};
use crate::solver::{self, ResolvedGraph, SolvePreferences};
use crate::version::{DepConstraint, ExactVersion};
use crate::workspace;
use crate::Error;

// ============================================================
// vo mod init
// ============================================================

/// Create a new `vo.mod` file at the given directory.
pub fn mod_init(dir: &Path, module_path: &str, vo_constraint: &str) -> Result<(), Error> {
    let mp = ModulePath::parse(module_path)?;
    let vo = crate::version::ToolchainConstraint::parse(vo_constraint)?;
    let mf = ModFile {
        module: mp,
        vo,
        require: vec![],
    };
    let content = mf.render();
    let path = dir.join("vo.mod");
    std::fs::write(&path, content)?;
    Ok(())
}

// ============================================================
// vo mod add
// ============================================================

/// Add or update a direct dependency in `vo.mod` and refresh `vo.lock`.
/// If `constraint` is None, resolves the latest non-prerelease version
/// and writes the equivalent compatible constraint.
pub fn mod_add(
    project_dir: &Path,
    dep_path: &str,
    constraint: Option<&str>,
    cache_root: &Path,
    registry: &dyn Registry,
    created_by: &str,
) -> Result<(), Error> {
    let mut mf = read_mod_file(project_dir)?;
    let dep_mp = ModulePath::parse(dep_path)?;

    let dep_constraint = match constraint {
        Some(c) => DepConstraint::parse(c)?,
        None => {
            let latest = latest_supported_requirement_version(&mf.vo, &dep_mp, registry)?;
            // Write ^MAJOR.MINOR.PATCH
            DepConstraint {
                op: crate::version::ConstraintOp::Compatible,
                version: latest.semver().clone(),
            }
        }
    };

    // Add or update the require entry
    if let Some(existing) = mf.require.iter_mut().find(|r| r.module == dep_mp) {
        existing.constraint = dep_constraint;
    } else {
        mf.require.push(Require {
            module: dep_mp,
            constraint: dep_constraint,
        });
    }

    let lock_file = prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
    write_mod_file(project_dir, &mf)?;
    write_or_remove_lock_file(project_dir, lock_file.as_ref())?;
    ensure_locked_modules_cached(cache_root, lock_file.as_ref(), registry)?;

    Ok(())
}

// ============================================================
// vo mod update
// ============================================================

/// Re-solve dependency constraints, possibly targeting a specific module.
/// With an explicit `target`, preserves unrelated locked versions.
pub fn mod_update(
    project_dir: &Path,
    target: Option<&str>,
    cache_root: &Path,
    registry: &dyn Registry,
    created_by: &str,
) -> Result<(), Error> {
    let mf = read_mod_file(project_dir)?;

    let prefs = if let Some(target_str) = target {
        let target_mp = ModulePath::parse(target_str)?;
        let existing_lock = read_lock_file(project_dir).ok();
        let mut locked = BTreeMap::new();
        if let Some(ref lf) = existing_lock {
            for lm in &lf.resolved {
                locked.insert(lm.path.clone(), lm.version.clone());
            }
        }
        SolvePreferences {
            target_update: Some(target_mp),
            locked,
        }
    } else {
        SolvePreferences::default()
    };

    let lock_file = prepare_lock_file(&mf, registry, &prefs, created_by)?;
    write_or_remove_lock_file(project_dir, lock_file.as_ref())?;
    ensure_locked_modules_cached(cache_root, lock_file.as_ref(), registry)?;

    Ok(())
}

// ============================================================
// vo mod sync
// ============================================================

/// Recompute the full dependency graph from `vo.mod` and write a fresh `vo.lock`.
pub fn mod_sync(
    project_dir: &Path,
    cache_root: &Path,
    registry: &dyn Registry,
    created_by: &str,
) -> Result<(), Error> {
    let mf = read_mod_file(project_dir)?;
    let lock_file = prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
    write_or_remove_lock_file(project_dir, lock_file.as_ref())?;
    ensure_locked_modules_cached(cache_root, lock_file.as_ref(), registry)?;
    Ok(())
}

// ============================================================
// vo mod download
// ============================================================

/// Fetch artifacts pinned by `vo.lock` into cache. Does NOT re-solve.
pub fn mod_download(
    project_dir: &Path,
    cache_root: &Path,
    registry: &dyn Registry,
) -> Result<(), Error> {
    let lf = read_lock_file(project_dir)?;
    crate::cache::install::populate_locked_cache(cache_root, &lf, registry)?;
    Ok(())
}

// ============================================================
// vo mod verify
// ============================================================

/// Verify root `vo.mod` / `vo.lock` consistency and cached artifacts.
pub fn mod_verify(project_dir: &Path, cache_root: &Path) -> Result<(), Error> {
    let mf = read_mod_file(project_dir)?;
    let lf = read_lock_file(project_dir)?;
    lock::verify_root_consistency(&mf, &lf)?;
    lock::verify_graph_completeness(&mf, &lf)?;
    crate::cache::install::verify_locked_cache(cache_root, &lf)?;
    Ok(())
}

// ============================================================
// vo mod remove
// ============================================================

/// Remove a direct dependency from `vo.mod` and refresh `vo.lock`.
pub fn mod_remove(
    project_dir: &Path,
    dep_path: &str,
    cache_root: &Path,
    registry: &dyn Registry,
    created_by: &str,
) -> Result<(), Error> {
    let mut mf = read_mod_file(project_dir)?;
    let dep_mp = ModulePath::parse(dep_path)?;

    let orig_len = mf.require.len();
    mf.require.retain(|r| r.module != dep_mp);
    if mf.require.len() == orig_len {
        return Err(Error::ModFileParse(format!(
            "{dep_path} is not a direct dependency"
        )));
    }

    let lock_file = prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
    write_mod_file(project_dir, &mf)?;
    write_or_remove_lock_file(project_dir, lock_file.as_ref())?;
    ensure_locked_modules_cached(cache_root, lock_file.as_ref(), registry)?;

    Ok(())
}

// ============================================================
// vo mod tidy
// ============================================================

/// Synchronize `vo.mod` require entries with actual import usage.
///
/// `external_imports` is the set of external import paths found by scanning
/// all `.vo` source files in the project (caller is responsible for scanning).
///
/// - Adds missing require entries (resolves latest compatible version).
/// - Removes require entries not referenced by any import.
/// - Re-solves the dependency graph and writes `vo.lock`.
pub fn mod_tidy(
    project_dir: &Path,
    external_imports: &BTreeSet<String>,
    cache_root: &Path,
    registry: &dyn Registry,
    created_by: &str,
) -> Result<TidyResult, Error> {
    let mut mf = read_mod_file(project_dir)?;
    let existing_lock = read_lock_file(project_dir).ok();

    // Determine which modules are actually needed by imports.
    let mut needed_modules: BTreeSet<ModulePath> = BTreeSet::new();
    for import_path in external_imports {
        if let Some((module, _)) =
            find_owning_module(import_path, mf.require.iter().map(|r| &r.module))
        {
            needed_modules.insert(module.clone());
            continue;
        }
        if let Some(lock_file) = existing_lock.as_ref() {
            if let Some((module, _)) = find_owning_module(
                import_path,
                lock_file.resolved.iter().map(|locked| &locked.path),
            ) {
                needed_modules.insert(module.clone());
                continue;
            }
        }
        needed_modules.insert(infer_module_path(import_path, registry)?);
    }

    // Compute added and removed modules.
    let existing: BTreeSet<ModulePath> = mf.require.iter().map(|r| r.module.clone()).collect();
    let added: Vec<ModulePath> = needed_modules.difference(&existing).cloned().collect();
    let removed: Vec<ModulePath> = existing.difference(&needed_modules).cloned().collect();

    // Remove unused requires.
    mf.require.retain(|r| needed_modules.contains(&r.module));

    // Add new requires (resolve latest compatible version).
    for mp in &added {
        let latest = latest_supported_requirement_version(&mf.vo, mp, registry)?;
        mf.require.push(Require {
            module: mp.clone(),
            constraint: DepConstraint {
                op: crate::version::ConstraintOp::Compatible,
                version: latest.semver().clone(),
            },
        });
    }

    let lock_file = prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
    write_mod_file(project_dir, &mf)?;
    write_or_remove_lock_file(project_dir, lock_file.as_ref())?;
    ensure_locked_modules_cached(cache_root, lock_file.as_ref(), registry)?;

    Ok(TidyResult {
        added: added.iter().map(|m| m.as_str().to_string()).collect(),
        removed: removed.iter().map(|m| m.as_str().to_string()).collect(),
    })
}

/// Result of a tidy operation.
#[derive(Debug, Clone)]
pub struct TidyResult {
    pub added: Vec<String>,
    pub removed: Vec<String>,
}

/// Infer the module path from an external import path.
///
/// For `github.com/acme/lib/util/sub`, the module is `github.com/acme/lib`
/// (3 segments for standard repos). For paths with a major-version suffix
/// like `github.com/acme/lib/v2/util`, the module is `github.com/acme/lib/v2`.
fn infer_module_path(import_path: &str, registry: &dyn Registry) -> Result<ModulePath, Error> {
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

// ============================================================
// vo mod why
// ============================================================

/// Explain why a module is in the dependency graph.
///
/// Returns the shortest dependency chain from the root module to the target,
/// or an error if the target is not in the lock file.
pub fn mod_why(project_dir: &Path, target: &str) -> Result<Vec<String>, Error> {
    let mf = read_mod_file(project_dir)?;
    let lf = read_lock_file(project_dir)?;
    let target_mp = ModulePath::parse(target)?;

    if lf.find(&target_mp).is_none() {
        return Err(Error::LockFileParse(format!("{target} is not in vo.lock")));
    }

    // BFS from root's direct deps to target.
    let root_str = mf.module.as_str().to_string();
    let mut queue: std::collections::VecDeque<Vec<String>> = std::collections::VecDeque::new();
    let mut visited: BTreeSet<String> = BTreeSet::new();

    // Seed with root's direct requirements.
    for req in &mf.require {
        let path = vec![root_str.clone(), req.module.as_str().to_string()];
        if req.module == target_mp {
            return Ok(path);
        }
        if visited.insert(req.module.as_str().to_string()) {
            queue.push_back(path);
        }
    }

    while let Some(chain) = queue.pop_front() {
        let current = chain.last().unwrap();
        let current_mp = ModulePath::parse(current)?;
        if let Some(lm) = lf.find(&current_mp) {
            for dep in &lm.deps {
                let dep_str = dep.as_str().to_string();
                let mut new_chain = chain.clone();
                new_chain.push(dep_str.clone());
                if *dep == target_mp {
                    return Ok(new_chain);
                }
                if visited.insert(dep_str) {
                    queue.push_back(new_chain);
                }
            }
        }
    }

    // Target is in vo.lock but unreachable from root (orphaned).
    Err(Error::LockFileParse(format!(
        "{target} is in vo.lock but not reachable from root module"
    )))
}

// ============================================================
// vo mod clean
// ============================================================

/// Clean the module cache.
///
/// If `keep_locked` is true and a lock file exists, only removes modules
/// NOT referenced by the current `vo.lock`. Otherwise removes everything.
pub fn mod_clean(
    project_dir: &Path,
    cache_root: &Path,
    keep_locked: bool,
) -> Result<CleanResult, Error> {
    if !cache_root.exists() {
        return Ok(CleanResult { removed_dirs: 0 });
    }

    let locked_dirs: BTreeSet<std::path::PathBuf> = if keep_locked {
        if let Ok(lf) = read_lock_file(project_dir) {
            lf.resolved
                .iter()
                .map(|lm| crate::cache::layout::cache_dir(cache_root, &lm.path, &lm.version))
                .collect()
        } else {
            BTreeSet::new()
        }
    } else {
        BTreeSet::new()
    };

    let mut removed = 0u64;
    // Iterate top-level module dirs (github.com@owner@repo)
    let entries = std::fs::read_dir(cache_root)?;
    for entry in entries {
        let entry = entry?;
        let module_dir = entry.path();
        if !module_dir.is_dir() {
            continue;
        }
        // Iterate version dirs inside each module dir
        let version_entries = std::fs::read_dir(&module_dir)?;
        for ve in version_entries {
            let ve = ve?;
            let version_dir = ve.path();
            if !version_dir.is_dir() {
                continue;
            }
            if locked_dirs.contains(&version_dir) {
                continue;
            }
            std::fs::remove_dir_all(&version_dir)?;
            removed += 1;
        }
        // Remove the module dir if now empty.
        if std::fs::read_dir(&module_dir)?.next().is_none() {
            std::fs::remove_dir(&module_dir)?;
        }
    }

    Ok(CleanResult {
        removed_dirs: removed,
    })
}

/// Result of a clean operation.
#[derive(Debug, Clone)]
pub struct CleanResult {
    pub removed_dirs: u64,
}

// ============================================================
// Frozen build entry point
// ============================================================

/// Frozen build check: verify that `vo.lock` exists and all cached artifacts
/// are present and valid. This is the entry point for `vo build/check/test/run`.
///
/// Does NOT access the network. Does NOT mutate `vo.mod` or `vo.lock`.
pub fn frozen_build_check(
    project_dir: &Path,
    cache_root: &Path,
    has_external_imports: bool,
) -> Result<Option<LockFile>, Error> {
    if !has_external_imports {
        return Ok(None);
    }

    let lf_path = project_dir.join("vo.lock");
    if !lf_path.exists() {
        return Err(Error::MissingLockFile);
    }

    let mf = read_mod_file(project_dir)?;
    let lf = read_lock_file(project_dir)?;

    lock::verify_root_consistency(&mf, &lf)?;
    lock::verify_graph_completeness(&mf, &lf)?;
    crate::cache::install::verify_locked_cache(cache_root, &lf)?;

    Ok(Some(lf))
}

// ============================================================
// Import ownership resolution — spec §9.1
// ============================================================

/// Resolve which module owns an import path, considering root module,
/// workspace overrides, and locked modules.
///
/// Returns the owning module path and the package subpath within that module.
pub fn resolve_import_owner<'a>(
    import_path: &'a str,
    root_module: &'a ModulePath,
    overrides: &'a [workspace::Override],
    lock_file: &'a LockFile,
) -> Result<(&'a ModulePath, &'a str), Error> {
    let class = classify_import(import_path)?;
    if class == ImportClass::Stdlib {
        return Err(Error::InvalidImportPath(
            "stdlib imports are not resolved by the module system".to_string(),
        ));
    }

    // Step 2: root module owns it?
    if let Some(sub) = root_module.owns_import(import_path) {
        return Ok((root_module, sub));
    }

    // Step 3: workspace override owns it? (longest prefix match)
    if let Some(result) = find_owning_module(import_path, overrides.iter().map(|ov| &ov.module)) {
        return Ok(result);
    }

    // Step 4: locked module owns it? (longest prefix match)
    if let Some(result) =
        find_owning_module(import_path, lock_file.resolved.iter().map(|lm| &lm.path))
    {
        return Ok(result);
    }

    Err(Error::InvalidImportPath(format!(
        "import path \"{}\" is not owned by any resolved module",
        import_path
    )))
}

// ============================================================
// File I/O helpers
// ============================================================

pub fn read_mod_file(project_dir: &Path) -> Result<ModFile, Error> {
    let path = project_dir.join("vo.mod");
    let content = std::fs::read_to_string(&path)?;
    ModFile::parse(&content)
}

pub fn read_lock_file(project_dir: &Path) -> Result<LockFile, Error> {
    let path = project_dir.join("vo.lock");
    let content = std::fs::read_to_string(&path)?;
    LockFile::parse(&content)
}

fn write_mod_file(project_dir: &Path, mf: &ModFile) -> Result<(), Error> {
    let path = project_dir.join("vo.mod");
    std::fs::write(&path, mf.render())?;
    Ok(())
}

fn write_lock_file(project_dir: &Path, lf: &LockFile) -> Result<(), Error> {
    let path = project_dir.join("vo.lock");
    std::fs::write(&path, lf.render())?;
    Ok(())
}

fn remove_lock_file_if_exists(project_dir: &Path) -> Result<(), Error> {
    let path = project_dir.join("vo.lock");
    if path.exists() {
        std::fs::remove_file(path)?;
    }
    Ok(())
}

fn write_or_remove_lock_file(
    project_dir: &Path,
    lock_file: Option<&LockFile>,
) -> Result<(), Error> {
    match lock_file {
        Some(lock_file) => write_lock_file(project_dir, lock_file),
        None => remove_lock_file_if_exists(project_dir),
    }
}

pub fn ensure_locked_modules_cached(
    cache_root: &Path,
    lock_file: Option<&LockFile>,
    registry: &dyn Registry,
) -> Result<(), Error> {
    if let Some(lock_file) = lock_file {
        crate::cache::install::populate_locked_cache(cache_root, lock_file, registry)?;
    }
    Ok(())
}

fn prepare_lock_file(
    mf: &ModFile,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
    created_by: &str,
) -> Result<Option<LockFile>, Error> {
    if mf.require.is_empty() {
        return Ok(None);
    }
    let graph = solve_from_modfile(mf, registry, prefs)?;
    Ok(Some(lock::generate_lock(mf, &graph, created_by)?))
}

fn latest_supported_requirement_version(
    project_vo: &crate::version::ToolchainConstraint,
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

fn solve_from_modfile(
    mf: &ModFile,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<ResolvedGraph, Error> {
    let reqs: Vec<(ModulePath, DepConstraint)> = mf
        .require
        .iter()
        .map(|r| (r.module.clone(), r.constraint.clone()))
        .collect();
    solver::solve(&mf.module, &mf.vo, &reqs, registry, prefs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_owns_import_exact() {
        let mp = ModulePath::parse("github.com/acme/app").unwrap();
        assert_eq!(mp.owns_import("github.com/acme/app"), Some(""));
    }

    #[test]
    fn test_owns_import_subpath() {
        let mp = ModulePath::parse("github.com/acme/app").unwrap();
        assert_eq!(mp.owns_import("github.com/acme/app/util"), Some("util"));
    }

    #[test]
    fn test_owns_import_no_match() {
        let mp = ModulePath::parse("github.com/acme/app").unwrap();
        assert_eq!(mp.owns_import("github.com/acme/other"), None);
    }

    #[test]
    fn test_owns_import_partial_segment() {
        // "github.com/acme/appkit" should NOT match "github.com/acme/app"
        let mp = ModulePath::parse("github.com/acme/app").unwrap();
        assert_eq!(mp.owns_import("github.com/acme/appkit"), None);
    }
}
