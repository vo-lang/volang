use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use crate::identity::{find_owning_module, ModulePath};
use crate::lifecycle;
use crate::project;
use crate::registry::Registry;
use crate::schema::lockfile::LockFile;
use crate::schema::modfile::{ModFile, Require};
use crate::solver::SolvePreferences;
use crate::version::DepConstraint;
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
    project::write_mod_file(dir, &mf)
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
            let latest =
                lifecycle::latest_supported_requirement_version(&mf.vo, &dep_mp, registry)?;
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

    let lock_file =
        lifecycle::prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
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

    let lock_file = lifecycle::prepare_lock_file(&mf, registry, &prefs, created_by)?;
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
    let lock_file =
        lifecycle::prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
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
    lifecycle::download_locked_dependencies(cache_root, &lf, registry)?;
    Ok(())
}

// ============================================================
// vo mod verify
// ============================================================

/// Verify root `vo.mod` / `vo.lock` consistency and cached artifacts.
pub fn mod_verify(project_dir: &Path, cache_root: &Path) -> Result<(), Error> {
    let mf = read_mod_file(project_dir)?;
    let lf = read_lock_file(project_dir)?;
    lifecycle::verify_locked_dependencies(cache_root, &mf, &lf)?;
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

    let lock_file =
        lifecycle::prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
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
        needed_modules.insert(lifecycle::infer_module_path(import_path, registry)?);
    }

    // Compute added and removed modules.
    let existing: BTreeSet<ModulePath> = mf.require.iter().map(|r| r.module.clone()).collect();
    let added: Vec<ModulePath> = needed_modules.difference(&existing).cloned().collect();
    let removed: Vec<ModulePath> = existing.difference(&needed_modules).cloned().collect();

    // Remove unused requires.
    mf.require.retain(|r| needed_modules.contains(&r.module));

    // Add new requires (resolve latest compatible version).
    for mp in &added {
        let latest = lifecycle::latest_supported_requirement_version(&mf.vo, mp, registry)?;
        mf.require.push(Require {
            module: mp.clone(),
            constraint: DepConstraint {
                op: crate::version::ConstraintOp::Compatible,
                version: latest.semver().clone(),
            },
        });
    }

    let lock_file =
        lifecycle::prepare_lock_file(&mf, registry, &SolvePreferences::default(), created_by)?;
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
    let lock_file = if keep_locked {
        read_lock_file(project_dir).ok()
    } else {
        None
    };
    Ok(CleanResult {
        removed_dirs: lifecycle::clean_cache(cache_root, lock_file.as_ref())?,
    })
}

/// Result of a clean operation.
#[derive(Debug, Clone)]
pub struct CleanResult {
    pub removed_dirs: u64,
}

// ============================================================
// File I/O helpers
// ============================================================

fn read_mod_file(project_dir: &Path) -> Result<ModFile, Error> {
    project::read_mod_file(project_dir)
}

fn read_lock_file(project_dir: &Path) -> Result<LockFile, Error> {
    project::read_lock_file(project_dir)
}

fn write_mod_file(project_dir: &Path, mf: &ModFile) -> Result<(), Error> {
    project::write_mod_file(project_dir, mf)
}

fn write_or_remove_lock_file(
    project_dir: &Path,
    lock_file: Option<&LockFile>,
) -> Result<(), Error> {
    project::write_or_remove_lock_file(project_dir, lock_file)
}

pub fn ensure_locked_modules_cached(
    cache_root: &Path,
    lock_file: Option<&LockFile>,
    registry: &dyn Registry,
) -> Result<(), Error> {
    if let Some(lock_file) = lock_file {
        lifecycle::download_locked_dependencies(cache_root, lock_file, registry)?;
    }
    Ok(())
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
