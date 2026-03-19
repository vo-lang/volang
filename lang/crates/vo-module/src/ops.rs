use std::collections::BTreeMap;
use std::path::Path;

use crate::identity::{classify_import, find_owning_module, ImportClass, ModulePath};
use crate::version::DepConstraint;
use crate::lock;
use crate::materialize;
use crate::registry::Registry;
use crate::schema::lockfile::LockFile;
use crate::schema::modfile::{ModFile, Require};
use crate::solver::{self, ResolvedGraph, SolvePreferences};
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
    registry: &dyn Registry,
    created_by: &str,
) -> Result<(), Error> {
    let mut mf = read_mod_file(project_dir)?;
    let dep_mp = ModulePath::parse(dep_path)?;

    let dep_constraint = match constraint {
        Some(c) => DepConstraint::parse(c)?,
        None => {
            // Resolve latest non-prerelease version
            let versions = registry.list_versions(&dep_mp)?;
            let compatible = crate::registry::filter_compatible_versions(&dep_mp, &versions);
            let latest = compatible
                .iter()
                .filter(|v| !v.semver().is_prerelease())
                .max()
                .ok_or_else(|| Error::NoSatisfyingVersion {
                    module: dep_path.to_string(),
                    detail: "no non-prerelease versions found; specify an explicit constraint"
                        .to_string(),
                })?;
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

    // Write updated vo.mod
    write_mod_file(project_dir, &mf)?;

    // Re-solve and write vo.lock
    let graph = solve_from_modfile(&mf, registry, &SolvePreferences::default())?;
    let lf = lock::generate_lock(&mf, &graph, created_by)?;
    write_lock_file(project_dir, &lf)?;

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

    let graph = solve_from_modfile(&mf, registry, &prefs)?;
    let lf = lock::generate_lock(&mf, &graph, created_by)?;
    write_lock_file(project_dir, &lf)?;

    Ok(())
}

// ============================================================
// vo mod sync
// ============================================================

/// Recompute the full dependency graph from `vo.mod` and write a fresh `vo.lock`.
pub fn mod_sync(
    project_dir: &Path,
    registry: &dyn Registry,
    created_by: &str,
) -> Result<(), Error> {
    let mf = read_mod_file(project_dir)?;
    let graph = solve_from_modfile(&mf, registry, &SolvePreferences::default())?;
    let lf = lock::generate_lock(&mf, &graph, created_by)?;
    write_lock_file(project_dir, &lf)?;
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
    materialize::download_all(cache_root, &lf, registry)?;
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
    materialize::verify_frozen_cache(cache_root, &lf)?;
    Ok(())
}

// ============================================================
// vo mod remove
// ============================================================

/// Remove a direct dependency from `vo.mod` and refresh `vo.lock`.
pub fn mod_remove(
    project_dir: &Path,
    dep_path: &str,
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

    write_mod_file(project_dir, &mf)?;

    // Re-solve to prune orphaned transitive dependencies
    let graph = solve_from_modfile(&mf, registry, &SolvePreferences::default())?;
    let lf = lock::generate_lock(&mf, &graph, created_by)?;
    write_lock_file(project_dir, &lf)?;

    Ok(())
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
    materialize::verify_frozen_cache(cache_root, &lf)?;

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
    if let Some(result) = find_owning_module(import_path, lock_file.resolved.iter().map(|lm| &lm.path)) {
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
