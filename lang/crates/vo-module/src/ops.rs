use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use crate::identity::ModulePath;
use crate::lifecycle;
use crate::project;
use crate::registry::{Registry, RegistryOperation};
use crate::schema::modfile::{ModFile, Require};
use crate::solver::SolvePreferences;
use crate::version::DepConstraint;
use crate::Error;

/// Canonical lock-file state after a module lifecycle operation.
///
/// Projects without external requirements deliberately omit `vo.lock`.  The
/// explicit status keeps callers from reporting that a lock was written or
/// verified when the canonical graph has no lock file.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LockFileStatus {
    Present,
    NotRequired,
}

fn read_optional_verified_lock_file(
    project_dir: &Path,
    mod_file: &ModFile,
) -> Result<Option<crate::schema::lockfile::LockFile>, Error> {
    match project::read_lock_file(project_dir) {
        Ok(lock_file) => {
            crate::lock::verify_root_consistency(mod_file, &lock_file)?;
            crate::lock::verify_graph_completeness(mod_file, &lock_file)?;
            Ok(Some(lock_file))
        }
        Err(Error::Io(error)) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(error),
    }
}

fn read_lock_file_for_declared_graph(
    project_dir: &Path,
    mod_file: &ModFile,
) -> Result<Option<crate::schema::lockfile::LockFile>, Error> {
    match read_optional_verified_lock_file(project_dir, mod_file)? {
        Some(lock_file) => Ok(Some(lock_file)),
        None if mod_file.require.is_empty() => Ok(None),
        None => Err(Error::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "vo.lock is required because vo.mod declares external requirements; run `vo mod sync`",
        ))),
    }
}

fn longest_indexed_owner<'a>(
    import_path: &str,
    owners: &'a BTreeMap<String, ModulePath>,
) -> Option<&'a ModulePath> {
    let mut prefix = import_path;
    loop {
        if let Some(owner) = owners.get(prefix) {
            return Some(owner);
        }
        let (parent, _) = prefix.rsplit_once('/')?;
        prefix = parent;
    }
}

// ============================================================
// vo mod init
// ============================================================

/// Create a new `vo.mod` file at the given directory.
pub fn mod_init(dir: &Path, module_path: &str, vo_constraint: &str) -> Result<(), Error> {
    // `vo mod init` is for on-disk projects only. Ephemeral `local/*`
    // identities are synthesized by the toolchain and never initialized via
    // this command (spec §5.6.2).
    let mp = ModulePath::parse(module_path)?;
    let vo = crate::version::ToolchainConstraint::parse(vo_constraint)?;
    let mf = ModFile {
        module: mp.into(),
        vo,
        require: vec![],
        web: None,
        extension: None,
        replace: vec![],
    };
    mf.validate()?;
    let _mutation = project::lock_project_mutation(dir)?;
    project::write_new_mod_file(dir, &mf)
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
    let _mutation = project::lock_project_mutation(project_dir)?;
    let registry = RegistryOperation::new(registry);
    let mut mf = project::read_mod_file(project_dir)?;
    let dep_mp = ModulePath::parse(dep_path)?;
    if mf.module.as_github() == Some(&dep_mp) {
        return Err(Error::ModFileParse(format!(
            "module {} must not require itself",
            dep_mp
        )));
    }
    if mf.require.iter().all(|require| require.module != dep_mp)
        && mf.require.len() == crate::MAX_MODULE_DEPENDENCIES
    {
        return Err(Error::ModFileParse(format!(
            "require contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }

    let dep_constraint = match constraint {
        Some(c) => DepConstraint::parse(c)?,
        None => {
            let latest =
                lifecycle::latest_supported_requirement_version(&mf.vo, &dep_mp, &registry)?;
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
    mf.validate()?;

    let lock_file =
        lifecycle::prepare_lock_file(&mf, &registry, &SolvePreferences::default(), created_by)?;
    if let Some(lock_file) = lock_file.as_ref() {
        lifecycle::download_locked_dependencies(cache_root, lock_file, &registry)?;
    }
    project::write_project_files(project_dir, &mf, lock_file.as_ref())?;

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
    let _mutation = project::lock_project_mutation(project_dir)?;
    let registry = RegistryOperation::new(registry);
    let mf = project::read_mod_file(project_dir)?;

    let prefs = if let Some(target_str) = target {
        let target_mp = ModulePath::parse(target_str)?;
        let existing_lock = read_optional_verified_lock_file(project_dir, &mf)?;
        let target_is_direct = mf.require.iter().any(|require| require.module == target_mp);
        let target_is_locked = existing_lock
            .as_ref()
            .is_some_and(|lock| lock.find(&target_mp).is_some());
        if !target_is_direct && !target_is_locked {
            return Err(Error::ModFileParse(format!(
                "update target {target_mp} is absent from the project dependency graph"
            )));
        }
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

    let lock_file = lifecycle::prepare_lock_file(&mf, &registry, &prefs, created_by)?;
    if let Some(lock_file) = lock_file.as_ref() {
        lifecycle::download_locked_dependencies(cache_root, lock_file, &registry)?;
    }
    project::write_or_remove_lock_file(project_dir, lock_file.as_ref())?;

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
) -> Result<LockFileStatus, Error> {
    let _mutation = project::lock_project_mutation(project_dir)?;
    let registry = RegistryOperation::new(registry);
    let mf = project::read_mod_file(project_dir)?;
    let lock_file =
        lifecycle::prepare_lock_file(&mf, &registry, &SolvePreferences::default(), created_by)?;
    if let Some(lock_file) = lock_file.as_ref() {
        lifecycle::download_locked_dependencies(cache_root, lock_file, &registry)?;
    }
    project::write_or_remove_lock_file(project_dir, lock_file.as_ref())?;
    Ok(if lock_file.is_some() {
        LockFileStatus::Present
    } else {
        LockFileStatus::NotRequired
    })
}

// ============================================================
// vo mod download
// ============================================================

/// Fetch artifacts pinned by `vo.lock` into cache. Does NOT re-solve.
pub fn mod_download(
    project_dir: &Path,
    cache_root: &Path,
    registry: &dyn Registry,
) -> Result<LockFileStatus, Error> {
    let _project_guard = project::lock_project_mutation(project_dir)?;
    let registry = RegistryOperation::new(registry);
    let mf = project::read_mod_file(project_dir)?;
    let Some(lf) = read_lock_file_for_declared_graph(project_dir, &mf)? else {
        return Ok(LockFileStatus::NotRequired);
    };
    lifecycle::download_locked_dependencies(cache_root, &lf, &registry)?;
    Ok(LockFileStatus::Present)
}

// ============================================================
// vo mod verify
// ============================================================

/// Verify root `vo.mod` / `vo.lock` consistency and cached artifacts.
pub fn mod_verify(project_dir: &Path, cache_root: &Path) -> Result<LockFileStatus, Error> {
    let _project_guard = project::lock_project_mutation(project_dir)?;
    let mf = project::read_mod_file(project_dir)?;
    let Some(lf) = read_lock_file_for_declared_graph(project_dir, &mf)? else {
        return Ok(LockFileStatus::NotRequired);
    };
    lifecycle::verify_locked_dependencies(cache_root, &mf, &lf)?;
    Ok(LockFileStatus::Present)
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
    let _mutation = project::lock_project_mutation(project_dir)?;
    let registry = RegistryOperation::new(registry);
    let mut mf = project::read_mod_file(project_dir)?;
    let dep_mp = ModulePath::parse(dep_path)?;

    let orig_len = mf.require.len();
    mf.require.retain(|r| r.module != dep_mp);
    if mf.require.len() == orig_len {
        return Err(Error::ModFileParse(format!(
            "{dep_path} is not a direct dependency"
        )));
    }
    mf.validate()?;

    let lock_file =
        lifecycle::prepare_lock_file(&mf, &registry, &SolvePreferences::default(), created_by)?;
    if let Some(lock_file) = lock_file.as_ref() {
        lifecycle::download_locked_dependencies(cache_root, lock_file, &registry)?;
    }
    project::write_project_files(project_dir, &mf, lock_file.as_ref())?;

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
    let _mutation = project::lock_project_mutation(project_dir)?;
    if external_imports.len() > crate::MAX_SOLVER_GRAPH_EDGES {
        return Err(Error::ResolutionLimitExceeded {
            resource: "external import count for mod tidy".to_string(),
            limit: crate::MAX_SOLVER_GRAPH_EDGES,
        });
    }
    let registry = RegistryOperation::new(registry);
    let mut mf = project::read_mod_file(project_dir)?;
    let existing_lock = read_optional_verified_lock_file(project_dir, &mf)?;

    // Build one owner index from the complete validated authority. Looking up
    // each slash prefix makes longest-owner selection proportional to import
    // depth instead of multiplying imports by the whole module graph.
    let mut owner_index = BTreeMap::new();
    for module in mf.require.iter().map(|require| &require.module).chain(
        existing_lock
            .iter()
            .flat_map(|lock| lock.resolved.iter().map(|locked| &locked.path)),
    ) {
        owner_index.insert(module.as_str().to_string(), module.clone());
    }

    // Determine which modules are actually needed by imports.
    let mut needed_modules: BTreeSet<ModulePath> = BTreeSet::new();
    for import_path in external_imports {
        if crate::identity::classify_import(import_path)? != crate::identity::ImportClass::External
        {
            return Err(Error::InvalidImportPath(format!(
                "mod tidy expected an external import path: {import_path}"
            )));
        }
        if let Some(module) = longest_indexed_owner(import_path, &owner_index) {
            needed_modules.insert(module.clone());
            continue;
        }
        needed_modules.insert(lifecycle::infer_module_path(import_path, &registry)?);
    }
    if needed_modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::ResolutionLimitExceeded {
            resource: "required module count for mod tidy".to_string(),
            limit: crate::MAX_MODULE_DEPENDENCIES,
        });
    }

    // Compute added and removed modules.
    let existing: BTreeSet<ModulePath> = mf.require.iter().map(|r| r.module.clone()).collect();
    let added: Vec<ModulePath> = needed_modules.difference(&existing).cloned().collect();
    let removed: Vec<ModulePath> = existing.difference(&needed_modules).cloned().collect();

    // Remove unused requires.
    mf.require.retain(|r| needed_modules.contains(&r.module));
    mf.validate()?;

    // Add new requires (resolve latest compatible version).
    for mp in &added {
        let latest = lifecycle::latest_supported_requirement_version(&mf.vo, mp, &registry)?;
        mf.require.push(Require {
            module: mp.clone(),
            constraint: DepConstraint {
                op: crate::version::ConstraintOp::Compatible,
                version: latest.semver().clone(),
            },
        });
    }
    // `added` is already a unique bounded set. Validate the complete authored
    // manifest once so a maximal tidy does linear validation work instead of
    // repeatedly rescanning every previously appended requirement.
    mf.validate()?;

    let lock_file =
        lifecycle::prepare_lock_file(&mf, &registry, &SolvePreferences::default(), created_by)?;
    if let Some(lock_file) = lock_file.as_ref() {
        lifecycle::download_locked_dependencies(cache_root, lock_file, &registry)?;
    }
    project::write_project_files(project_dir, &mf, lock_file.as_ref())?;

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
    let _project_guard = project::lock_project_mutation(project_dir)?;
    let mf = project::read_mod_file(project_dir)?;
    let target_mp = ModulePath::parse(target)?;
    if mf.module.as_github() == Some(&target_mp) {
        return Ok(vec![mf.module.as_str().to_string()]);
    }
    let Some(lf) = read_lock_file_for_declared_graph(project_dir, &mf)? else {
        return Err(Error::DependencyGraph(format!(
            "{target} is not in the module dependency graph"
        )));
    };

    let locked_by_path = lf
        .resolved
        .iter()
        .map(|locked| (&locked.path, locked))
        .collect::<BTreeMap<_, _>>();
    if !locked_by_path.contains_key(&target_mp) {
        return Err(Error::DependencyGraph(format!(
            "{target} is not in the module dependency graph"
        )));
    }

    // BFS from root's direct deps to target.
    let root_str = mf.module.as_str().to_string();
    let mut queue = std::collections::VecDeque::<ModulePath>::new();
    let mut predecessor = BTreeMap::<ModulePath, Option<ModulePath>>::new();

    // Seed with root's direct requirements.
    for req in &mf.require {
        predecessor.entry(req.module.clone()).or_insert(None);
        if req.module == target_mp {
            return Ok(vec![root_str, req.module.as_str().to_string()]);
        }
        queue.push_back(req.module.clone());
    }

    while let Some(current) = queue.pop_front() {
        if let Some(lm) = locked_by_path.get(&current) {
            for dep in &lm.deps {
                if predecessor.contains_key(&dep.module) {
                    continue;
                }
                predecessor.insert(dep.module.clone(), Some(current.clone()));
                if dep.module == target_mp {
                    let mut chain = Vec::new();
                    let mut cursor = target_mp.clone();
                    chain.push(cursor.as_str().to_string());
                    while let Some(Some(parent)) = predecessor.get(&cursor) {
                        cursor = parent.clone();
                        chain.push(cursor.as_str().to_string());
                    }
                    chain.push(root_str);
                    chain.reverse();
                    return Ok(chain);
                }
                queue.push_back(dep.module.clone());
            }
        }
    }

    // Target is in vo.lock but unreachable from root (orphaned).
    Err(Error::DependencyGraph(format!(
        "{target} is locked but not reachable from the root module"
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
    let _project_guard = if keep_locked {
        Some(project::lock_project_mutation(project_dir)?)
    } else {
        None
    };
    let lock_file = if keep_locked {
        let mod_file = project::read_mod_file(project_dir)?;
        let lock_file = project::read_lock_file(project_dir)?;
        crate::lock::verify_root_consistency(&mod_file, &lock_file)?;
        crate::lock::verify_graph_completeness(&mod_file, &lock_file)?;
        Some(lock_file)
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    struct PanicRegistry;

    impl Registry for PanicRegistry {
        fn list_version_candidates(
            &self,
            _module: &ModulePath,
        ) -> Result<Vec<crate::version::ExactVersion>, Error> {
            panic!("invalid tidy input must be rejected before registry access")
        }

        fn fetch_manifest_raw(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            panic!("invalid tidy input must be rejected before registry access")
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            panic!("invalid tidy input must be rejected before registry access")
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _artifact: &crate::identity::ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            panic!("invalid tidy input must be rejected before registry access")
        }
    }

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

    #[test]
    fn mod_init_refuses_to_overwrite_existing_manifest() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("vo.mod");
        let original = "module github.com/acme/existing\nvo ^0.1.0\n";
        fs::write(&path, original).unwrap();

        let error = mod_init(temp.path(), "github.com/acme/new", "^0.1.0").unwrap_err();

        assert!(matches!(
            error,
            Error::Io(ref error) if error.kind() == std::io::ErrorKind::AlreadyExists
        ));
        assert_eq!(fs::read_to_string(path).unwrap(), original);
    }

    #[test]
    fn optional_lock_only_treats_not_found_as_absent() {
        let temp = tempfile::tempdir().unwrap();
        let mod_file = ModFile::parse("module github.com/acme/app\nvo ^0.1.0\n").unwrap();
        assert!(read_optional_verified_lock_file(temp.path(), &mod_file)
            .unwrap()
            .is_none());

        fs::write(temp.path().join("vo.lock"), "not valid lock data").unwrap();
        assert!(matches!(
            read_optional_verified_lock_file(temp.path(), &mod_file).unwrap_err(),
            Error::LockFileParse(_)
        ));
    }

    #[test]
    fn empty_graph_lifecycle_uses_one_explicit_lockless_state() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "module github.com/acme/app\nvo ^0.1.0\n",
        )
        .unwrap();
        fs::write(project.path().join("vo.lock"), "stale lock data").unwrap();

        assert_eq!(
            mod_sync(project.path(), cache.path(), &PanicRegistry, "vo test",).unwrap(),
            LockFileStatus::NotRequired
        );
        assert!(!project.path().join("vo.lock").exists());
        assert_eq!(
            mod_verify(project.path(), cache.path()).unwrap(),
            LockFileStatus::NotRequired
        );
        assert_eq!(
            mod_download(project.path(), cache.path(), &PanicRegistry).unwrap(),
            LockFileStatus::NotRequired
        );
    }

    #[test]
    fn missing_lock_is_an_error_when_external_requirements_exist() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            concat!(
                "module github.com/acme/app\n",
                "vo ^0.1.0\n",
                "require github.com/acme/lib ^1.0.0\n",
            ),
        )
        .unwrap();

        let error = mod_verify(project.path(), cache.path()).unwrap_err();
        assert!(matches!(
            &error,
            Error::Io(error) if error.kind() == std::io::ErrorKind::NotFound
        ));
        assert!(error
            .to_string()
            .contains("vo.lock is required because vo.mod declares external requirements"));
    }

    #[test]
    fn malformed_lock_fails_closed_even_for_an_empty_graph() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "module github.com/acme/app\nvo ^0.1.0\n",
        )
        .unwrap();
        fs::write(project.path().join("vo.lock"), "malformed").unwrap();

        assert!(matches!(
            mod_verify(project.path(), cache.path()).unwrap_err(),
            Error::LockFileParse(_)
        ));
        assert!(matches!(
            mod_download(project.path(), cache.path(), &PanicRegistry).unwrap_err(),
            Error::LockFileParse(_)
        ));
    }

    #[test]
    fn why_handles_the_canonical_lockless_graph_semantically() {
        let project = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "module github.com/acme/app\nvo ^0.1.0\n",
        )
        .unwrap();

        assert_eq!(
            mod_why(project.path(), "github.com/acme/app").unwrap(),
            vec!["github.com/acme/app"]
        );
        let error = mod_why(project.path(), "github.com/acme/lib").unwrap_err();
        assert!(matches!(&error, Error::DependencyGraph(_)));
        assert!(error
            .to_string()
            .contains("github.com/acme/lib is not in the module dependency graph"));
    }

    #[test]
    fn mod_clean_fails_closed_when_lock_file_is_invalid() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "module github.com/acme/app\nvo ^0.1.0\n",
        )
        .unwrap();
        fs::write(project.path().join("vo.lock"), "not valid lock data").unwrap();
        let sentinel = cache.path().join("sentinel");
        fs::create_dir_all(&sentinel).unwrap();
        fs::write(sentinel.join("keep"), "untouched").unwrap();

        let error = mod_clean(project.path(), cache.path(), true).unwrap_err();

        assert!(matches!(error, Error::LockFileParse(_)));
        assert!(sentinel.join("keep").is_file());
    }

    #[test]
    fn mod_clean_requires_an_explicit_all_without_a_lock_file() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();

        let error = mod_clean(project.path(), cache.path(), true).unwrap_err();

        assert!(matches!(
            error,
            Error::Io(ref error) if error.kind() == std::io::ErrorKind::NotFound
        ));
    }

    #[test]
    fn mod_tidy_validates_imports_even_when_an_existing_module_prefix_matches() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            concat!(
                "module github.com/acme/app\n",
                "vo ^0.1.0\n",
                "require github.com/acme/lib ^1.0.0\n",
            ),
        )
        .unwrap();
        let imports = BTreeSet::from(["github.com/acme/lib/../escape".to_string()]);

        let error = mod_tidy(
            project.path(),
            &imports,
            cache.path(),
            &PanicRegistry,
            "vo test",
        )
        .unwrap_err();

        assert!(matches!(error, Error::InvalidImportPath(_)), "{error}");
    }
}
