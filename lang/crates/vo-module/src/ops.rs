use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::path::Path;

use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{
    normalize_fs_path, sort_fs_paths, FileSystem, FileSystemEntryKind, RealFs,
    MAX_DIRECTORY_ENTRIES, MAX_PACKAGE_SOURCE_BYTES, MAX_PACKAGE_SOURCE_FILES, MAX_TEXT_FILE_BYTES,
};

use crate::identity::ModulePath;
use crate::lifecycle;
use crate::project;
use crate::registry::{Registry, RegistryOperation};
use crate::schema::modfile::{Dependency, ModFile};
use crate::solver::SolvePreferences;
use crate::version::DepConstraint;
use crate::Error;

/// Canonical lock-file state after a module lifecycle operation.
///
/// Projects without external dependencies deliberately omit `vo.lock`.  The
/// explicit status keeps callers from reporting that a lock was written or
/// verified when the canonical graph has no lock file.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LockFileStatus {
    Present,
    NotRequired,
}

/// Read structurally valid prior selections for a graph mutation.
///
/// A lock whose root no longer matches the authored manifest cannot authorize
/// a build, though its individually authenticated selections remain useful as
/// minimal-change solve preferences. A lock that cannot be parsed remains an
/// error: silently discarding a corrupt generated file would hide a real
/// project-integrity failure. Only
/// commands whose authored graph is already empty and whose documented action
/// is lock cleanup may deliberately bypass this reader.
#[derive(Debug, Default)]
struct SelectionLockState {
    captured: Option<crate::schema::lockfile::LockFile>,
    usable: Option<crate::schema::lockfile::LockFile>,
}

impl SelectionLockState {
    fn as_ref(&self) -> Option<&crate::schema::lockfile::LockFile> {
        self.usable.as_ref()
    }

    fn captured(&self) -> Option<&crate::schema::lockfile::LockFile> {
        self.captured.as_ref()
    }

    #[cfg(test)]
    fn is_none(&self) -> bool {
        self.usable.is_none()
    }

    #[cfg(test)]
    fn is_some(&self) -> bool {
        self.usable.is_some()
    }

    fn iter(&self) -> impl Iterator<Item = &crate::schema::lockfile::LockFile> {
        self.usable.iter()
    }
}

fn read_optional_selection_lock_file(
    project_dir: &Path,
    _mod_file: &ModFile,
) -> Result<SelectionLockState, Error> {
    match project::read_lock_file_stable(project_dir) {
        Ok(lock_file) => Ok(SelectionLockState {
            captured: Some(lock_file.clone()),
            usable: Some(lock_file),
        }),
        Err(Error::Io(error)) if error.kind() == std::io::ErrorKind::NotFound => {
            Ok(SelectionLockState::default())
        }
        Err(error) => Err(error),
    }
}

fn locked_version_preferences(
    lock_file: Option<&crate::schema::lockfile::LockFile>,
) -> BTreeMap<ModulePath, crate::version::ExactVersion> {
    lock_file
        .into_iter()
        .flat_map(|lock| &lock.modules)
        .map(|module| (module.path.clone(), module.version.clone()))
        .collect()
}

fn require_portable_dependencies(mod_file: &ModFile, operation: &str) -> Result<(), Error> {
    if let Some(local) = mod_file
        .dependencies
        .iter()
        .find(|dependency| dependency.module.is_local())
    {
        return Err(Error::DependencyGraph(format!(
            "{operation} cannot select unpublished module {}; use `vo work sync` for a workspace graph",
            local.module
        )));
    }
    Ok(())
}

#[cfg(test)]
fn read_stable_declared_graph(
    project_dir: &Path,
) -> Result<(ModFile, Option<crate::schema::lockfile::LockFile>), Error> {
    let project_plan = project::read_project_plan_at_root(project_dir).map_err(|error| {
        use crate::project::{ProjectPlanErrorKind, ProjectPlanStage};
        match (error.stage(), error.kind()) {
            (ProjectPlanStage::ModFile, ProjectPlanErrorKind::ParseFailed) => {
                Error::ModFileParse(error.to_string())
            }
            (ProjectPlanStage::LockFile, ProjectPlanErrorKind::ParseFailed) => {
                Error::LockFileParse(error.to_string())
            }
            (_, ProjectPlanErrorKind::Missing) => Error::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                error.to_string(),
            )),
            (_, ProjectPlanErrorKind::ReadFailed) => {
                Error::Io(std::io::Error::other(error.to_string()))
            }
            _ => Error::DependencyGraph(error.to_string()),
        }
    })?;
    let mod_file = project_plan
        .mod_file()
        .cloned()
        .ok_or_else(|| Error::ModFileParse("this operation requires vo.mod".to_string()))?;
    Ok((mod_file, project_plan.lock_file().cloned()))
}

#[cfg(test)]
fn ensure_declared_graph_unchanged(
    project_dir: &Path,
    expected_mod_file: &ModFile,
    expected_lock_file: Option<&crate::schema::lockfile::LockFile>,
    operation: &str,
) -> Result<(), Error> {
    let (current_mod_file, current_lock_file) = read_stable_declared_graph(project_dir)?;
    if &current_mod_file != expected_mod_file || current_lock_file.as_ref() != expected_lock_file {
        return Err(Error::DependencyGraph(format!(
            "project declaration changed while {operation} was running; retry against the current vo.mod/vo.lock graph"
        )));
    }
    Ok(())
}

fn load_project_context_for_operation(
    project_dir: &Path,
    operation: &str,
) -> Result<crate::project::ProjectContext, Error> {
    crate::project::load_project_context(&RealFs::new("."), project_dir).map_err(|error| {
        Error::DependencyGraph(format!(
            "{operation} cannot capture the project graph: {error}"
        ))
    })
}

fn ensure_project_context_unchanged(
    project_dir: &Path,
    expected: &crate::project::ProjectContext,
    operation: &str,
) -> Result<(), Error> {
    let current = load_project_context_for_operation(project_dir, operation)?;
    if !current.has_same_root_authority(expected)
        || current.workspace_file() != expected.workspace_file()
        || current.workspace_generation() != expected.workspace_generation()
    {
        return Err(Error::DependencyGraph(format!(
            "project graph changed while {operation} was running; retry against the current vo.mod/vo.lock/vo.work graph"
        )));
    }
    Ok(())
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

fn initial_mod_file_with_constraint(
    module_path: &str,
    vo_constraint: &str,
) -> Result<ModFile, Error> {
    // Project manifests may use a public identity or the unpublished
    // `local/*` namespace; release validation enforces publishability.
    let module = crate::identity::ModIdentity::parse(module_path)?;
    let vo = crate::version::ToolchainConstraint::parse(vo_constraint)?;
    let manifest = ModFile {
        format: 1,
        module,
        version: crate::version::ExactVersion::parse(crate::INITIAL_MODULE_VERSION)?,
        vo,
        dependencies: vec![],
        web: None,
        extension: None,
    };
    manifest.validate()?;
    Ok(manifest)
}

/// Build the canonical initial manifest for a new on-disk module.
///
/// The owning toolchain constraint is deliberately selected inside
/// `vo-module`; callers cannot mint a project with a surface-specific version.
pub fn initial_mod_file(module_path: &str) -> Result<ModFile, Error> {
    initial_mod_file_with_constraint(module_path, crate::TOOLCHAIN_CONSTRAINT)
}

/// Render the exact bytes used by every typed module-initialization surface.
pub fn render_initial_mod_file(module_path: &str) -> Result<String, Error> {
    initial_mod_file(module_path)?.render()
}

/// Create a new `vo.mod` file at the given directory.
pub fn mod_init(dir: &Path, module_path: &str) -> Result<(), Error> {
    let manifest = initial_mod_file(module_path)?;
    let mutation = project::lock_project_mutation(dir)?;
    project::write_new_mod_file(dir, &mutation, &manifest)
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
) -> Result<(), Error> {
    let registry = RegistryOperation::new(registry);
    let original_mod = project::read_mod_file_stable(project_dir)?;
    let existing_lock = read_optional_selection_lock_file(project_dir, &original_mod)?;
    let mut mf = original_mod.clone();
    let dep_mp = ModulePath::parse(dep_path)?;
    require_portable_dependencies(&original_mod, "vo mod add")?;
    if dep_mp.is_local() {
        return Err(Error::DependencyGraph(format!(
            "vo mod add cannot select unpublished module {dep_mp}; add its intent to vo.mod and run `vo work sync`"
        )));
    }
    if mf.module.as_public() == Some(&dep_mp) {
        return Err(Error::ModFileParse(format!(
            "module {} must not depend on itself",
            dep_mp
        )));
    }
    if mf
        .dependencies
        .iter()
        .all(|dependency| dependency.module != dep_mp)
        && mf.dependencies.len() == crate::MAX_MODULE_DEPENDENCIES
    {
        return Err(Error::ModFileParse(format!(
            "[dependencies] contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }

    let dep_constraint = match constraint {
        Some(c) => DepConstraint::parse(c)?,
        None => {
            let latest =
                lifecycle::latest_supported_dependency_version(&mf.vo, &dep_mp, &registry)?;
            // Write ^MAJOR.MINOR.PATCH
            DepConstraint {
                op: crate::version::ConstraintOp::Compatible,
                version: latest.semver().clone(),
            }
        }
    };

    // Add or update the dependency entry.
    if let Some(existing) = mf.dependencies.iter_mut().find(|r| r.module == dep_mp) {
        existing.constraint = dep_constraint;
    } else {
        mf.dependencies.push(Dependency {
            module: dep_mp.clone(),
            constraint: dep_constraint,
        });
    }
    mf.validate()?;
    require_portable_dependencies(&mf, "vo mod add")?;

    let preferences =
        SolvePreferences::update_target(dep_mp, locked_version_preferences(existing_lock.as_ref()));
    let lock_file = lifecycle::prepare_lock_file(&mf, &registry, &preferences)?;
    project::write_project_files_if_unchanged(
        project_dir,
        &original_mod,
        existing_lock.captured(),
        &mf,
        lock_file.as_ref(),
        "vo mod add",
    )?;

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
) -> Result<(), Error> {
    let registry = RegistryOperation::new(registry);
    let mf = project::read_mod_file_stable(project_dir)?;
    require_portable_dependencies(&mf, "vo mod update")?;
    // An untargeted update on an already empty graph is an explicit cleanup
    // operation. Targeted updates still inspect any existing lock so corrupt
    // state cannot be used to name a selection or silently discarded.
    let existing_lock = if target.is_none() && mf.dependencies.is_empty() {
        SelectionLockState::default()
    } else {
        read_optional_selection_lock_file(project_dir, &mf)?
    };

    let mut selected_target = None;
    let prefs = if let Some(target_str) = target {
        let target_mp = ModulePath::parse(target_str)?;
        let target_is_direct = mf
            .dependencies
            .iter()
            .any(|dependency| dependency.module == target_mp);
        let target_is_locked = existing_lock
            .as_ref()
            .is_some_and(|lock| lock.find(&target_mp).is_some());
        if !target_is_direct && !target_is_locked {
            return Err(Error::ModFileParse(format!(
                "update target {target_mp} is absent from the project dependency graph"
            )));
        }
        selected_target = Some(target_mp.clone());
        SolvePreferences::update_target(
            target_mp,
            locked_version_preferences(existing_lock.as_ref()),
        )
    } else {
        SolvePreferences::default()
    };

    let lock_file = lifecycle::prepare_lock_file(&mf, &registry, &prefs)?;
    if let Some(target_mp) = selected_target {
        if lock_file
            .as_ref()
            .is_none_or(|lock| lock.find(&target_mp).is_none())
        {
            return Err(Error::DependencyGraph(format!(
                "update target {target_mp} is absent from the current dependency graph after resolution; vo.mod and vo.lock were left unchanged"
            )));
        }
    }
    if mf.dependencies.is_empty() {
        project::remove_lock_for_dependency_free_root(project_dir, &mf, "vo mod update")?;
    } else {
        project::write_project_files_if_unchanged(
            project_dir,
            &mf,
            existing_lock.captured(),
            &mf,
            lock_file.as_ref(),
            "vo mod update",
        )?;
    }

    Ok(())
}

// ============================================================
// vo mod sync
// ============================================================

/// Recompute the full dependency graph from `vo.mod` and write a fresh `vo.lock`.
/// An already empty authored graph explicitly removes any stale lock bytes.
pub fn mod_sync(project_dir: &Path, registry: &dyn Registry) -> Result<LockFileStatus, Error> {
    let registry = RegistryOperation::new(registry);
    let mf = project::read_mod_file_stable(project_dir)?;
    if mf.dependencies.is_empty() {
        project::remove_lock_for_dependency_free_root(project_dir, &mf, "vo mod sync")?;
        return Ok(LockFileStatus::NotRequired);
    }
    require_portable_dependencies(&mf, "vo mod sync")?;
    let existing_lock = read_optional_selection_lock_file(project_dir, &mf)?;
    let preferences =
        SolvePreferences::preserve_locked(locked_version_preferences(existing_lock.as_ref()));
    let lock_file = lifecycle::prepare_lock_file(&mf, &registry, &preferences)?;
    project::write_project_files_if_unchanged(
        project_dir,
        &mf,
        existing_lock.captured(),
        &mf,
        lock_file.as_ref(),
        "vo mod sync",
    )?;
    Ok(if lock_file.is_some() {
        LockFileStatus::Present
    } else {
        LockFileStatus::NotRequired
    })
}

struct WorkspaceOverlayRegistry<'a> {
    base: &'a dyn Registry,
    manifests: BTreeMap<
        ModulePath,
        (
            crate::version::ExactVersion,
            crate::schema::manifest::ReleaseManifest,
            Vec<u8>,
        ),
    >,
}

impl Registry for WorkspaceOverlayRegistry<'_> {
    fn list_version_candidates(
        &self,
        module: &ModulePath,
    ) -> Result<Vec<crate::version::ExactVersion>, Error> {
        match self.manifests.get(module) {
            Some((version, _, _)) => Ok(vec![version.clone()]),
            None => self.base.list_version_candidates(module),
        }
    }

    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &crate::version::ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        match self.manifests.get(module) {
            Some((selected, _, raw)) if selected == version => Ok(raw.clone()),
            Some((selected, _, _)) => Err(Error::RegistryNotFound {
                resource: format!(
                    "workspace module {module} selects {selected}, requested {version}"
                ),
            }),
            None => self.base.fetch_manifest_raw(module, version),
        }
    }

    fn parse_resolution_manifest(
        &self,
        module: &ModulePath,
        version: &crate::version::ExactVersion,
        raw: &[u8],
    ) -> Result<crate::schema::manifest::ReleaseManifest, Error> {
        match self.manifests.get(module) {
            Some((selected, manifest, expected_raw))
                if selected == version && expected_raw.as_slice() == raw =>
            {
                Ok(manifest.clone())
            }
            Some((selected, _, _)) => Err(Error::InvalidReleaseMetadata(format!(
                "workspace resolution descriptor for {module} changed or selected {selected}, requested {version}"
            ))),
            None if module.is_local() => Err(Error::RegistryNotFound {
                resource: format!("local module {module} is absent from the selected workspace"),
            }),
            None => self.base.parse_resolution_manifest(module, version, raw),
        }
    }

    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &crate::version::ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        self.base.fetch_source_package(module, version, asset_name)
    }

    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &crate::version::ExactVersion,
        artifact: &crate::identity::ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        self.base.fetch_artifact(module, version, artifact)
    }
}

/// Select one mixed workspace/registry graph and write the root `vo.lock`.
/// Workspace members are exact candidates; registry listing is used only for
/// ModuleIds absent from the selected workspace.
pub fn work_sync(project_dir: &Path, registry: &dyn Registry) -> Result<LockFileStatus, Error> {
    let root = project::read_mod_file_stable(project_dir)?;
    if root.dependencies.is_empty() {
        project::remove_lock_for_dependency_free_root(project_dir, &root, "vo work sync")?;
        return Ok(LockFileStatus::NotRequired);
    }
    let existing_lock = read_optional_selection_lock_file(project_dir, &root)?;
    let (workfile, members) = crate::workspace::discover_workspace_candidates_in_with_generation(
        &RealFs::new("."),
        project_dir,
        Some(&root.module),
        &crate::workspace::WorkspaceDiscovery::Auto,
    )?;
    let Some(workfile) = workfile else {
        return Err(Error::WorkspaceValidation(
            "vo work sync requires an applicable vo.work containing the active project".to_string(),
        ));
    };

    let mut manifests = BTreeMap::new();
    let mut intents = BTreeMap::new();
    for member in &members {
        let mod_file = member.mod_file();
        let intent = crate::lock::module_intent_digest(mod_file)?;
        let mut dependencies = mod_file
            .dependencies
            .iter()
            .map(|dependency| crate::schema::manifest::ManifestDependency {
                module: dependency.module.clone(),
                constraint: dependency.constraint.clone(),
            })
            .collect::<Vec<_>>();
        dependencies.sort_by(|left, right| left.module.cmp(&right.module));
        let manifest = crate::schema::manifest::ReleaseManifest {
            format: 1,
            module: member.module.clone(),
            version: mod_file.version.clone(),
            vo: mod_file.vo.clone(),
            intent: intent.clone(),
            dependencies,
            source: crate::schema::manifest::ManifestSource {
                name: crate::schema::manifest::SOURCE_ARCHIVE_ASSET_NAME.to_string(),
                size: 1,
                digest: crate::digest::Digest::from_sha256(b"workspace-source-placeholder"),
                tree: crate::digest::Digest::from_sha256(b"workspace-tree-placeholder"),
            },
            artifacts: Vec::new(),
        };
        let raw = manifest.render_resolution_descriptor()?.into_bytes();
        manifests.insert(
            member.module.clone(),
            (mod_file.version.clone(), manifest, raw),
        );
        intents.insert(member.module.clone(), intent);
    }

    let overlay = WorkspaceOverlayRegistry {
        base: registry,
        manifests,
    };
    let registry = RegistryOperation::new(&overlay);
    let mut lock_file = lifecycle::prepare_lock_file(
        &root,
        &registry,
        &SolvePreferences::preserve_locked(locked_version_preferences(existing_lock.as_ref())),
    )?
    .ok_or_else(|| {
        Error::DependencyGraph("workspace graph unexpectedly became empty".to_string())
    })?;
    for module in &mut lock_file.modules {
        if let Some(intent) = intents.get(&module.path) {
            module.origin = crate::schema::lockfile::LockOrigin::Workspace;
            module.release = None;
            module.intent = Some(intent.clone());
        }
    }
    lock_file.validate()?;
    workfile.validate(&RealFs::new("."))?;
    for member in &members {
        member.validate_generation(&RealFs::new("."))?;
    }
    project::write_project_files_if_unchanged(
        project_dir,
        &root,
        existing_lock.captured(),
        &root,
        Some(&lock_file),
        "vo work sync",
    )?;
    Ok(LockFileStatus::Present)
}

// ============================================================
// vo mod fetch
// ============================================================

/// Fetch releases selected by `vo.lock` and their authenticated
/// `vo.release.json` artifacts into cache. Does not re-solve.
pub fn mod_fetch(
    project_dir: &Path,
    cache_root: &Path,
    registry: &dyn Registry,
) -> Result<LockFileStatus, Error> {
    let registry = RegistryOperation::new(registry);
    let context = load_project_context_for_operation(project_dir, "vo mod fetch")?;
    let Some(lf) = context.project_plan().lock_file() else {
        ensure_project_context_unchanged(project_dir, &context, "vo mod fetch")?;
        return Ok(LockFileStatus::NotRequired);
    };
    lifecycle::download_locked_dependencies(cache_root, lf, &registry)?;
    crate::readiness::validate_materialized_project_graph(&RealFs::new(cache_root), &context)?;
    ensure_project_context_unchanged(project_dir, &context, "vo mod fetch")?;
    Ok(LockFileStatus::Present)
}

// ============================================================
// vo mod verify
// ============================================================

/// Verify root `vo.mod` / `vo.lock` consistency and cached artifacts.
pub fn mod_verify(project_dir: &Path, cache_root: &Path) -> Result<LockFileStatus, Error> {
    let context = load_project_context_for_operation(project_dir, "vo mod verify")?;
    let Some(mf) = context.project_plan().mod_file() else {
        return Err(Error::ModFileParse(
            "vo mod verify requires a project with vo.mod".to_string(),
        ));
    };
    let Some(lf) = context.project_plan().lock_file() else {
        ensure_project_context_unchanged(project_dir, &context, "vo mod verify")?;
        return Ok(LockFileStatus::NotRequired);
    };
    lifecycle::verify_locked_dependencies(cache_root, mf, lf)?;
    crate::readiness::validate_materialized_project_graph(&RealFs::new(cache_root), &context)?;
    ensure_project_context_unchanged(project_dir, &context, "vo mod verify")?;
    Ok(LockFileStatus::Present)
}

// ============================================================
// vo mod remove
// ============================================================

/// Remove a direct dependency from `vo.mod` and refresh `vo.lock`.
pub fn mod_remove(
    project_dir: &Path,
    dep_path: &str,
    registry: &dyn Registry,
) -> Result<(), Error> {
    let registry = RegistryOperation::new(registry);
    let original_mod = project::read_mod_file_stable(project_dir)?;
    let existing_lock = read_optional_selection_lock_file(project_dir, &original_mod)?;
    let mut mf = original_mod.clone();
    let dep_mp = ModulePath::parse(dep_path)?;

    let orig_len = mf.dependencies.len();
    mf.dependencies.retain(|r| r.module != dep_mp);
    if mf.dependencies.len() == orig_len {
        return Err(Error::ModFileParse(format!(
            "{dep_path} is not a direct dependency"
        )));
    }
    mf.validate()?;
    require_portable_dependencies(&mf, "vo mod remove")?;

    let preferences =
        SolvePreferences::preserve_locked(locked_version_preferences(existing_lock.as_ref()));
    let lock_file = lifecycle::prepare_lock_file(&mf, &registry, &preferences)?;
    project::write_project_files_if_unchanged(
        project_dir,
        &original_mod,
        existing_lock.captured(),
        &mf,
        lock_file.as_ref(),
        "vo mod remove",
    )?;

    Ok(())
}

// ============================================================
// vo mod tidy
// ============================================================

const MAX_TIDY_SOURCE_SNAPSHOT_ATTEMPTS: usize = 8;
const MAX_TIDY_SOURCE_BYTES: usize = MAX_PACKAGE_SOURCE_BYTES * 4;
const MAX_TIDY_PARSE_DIAGNOSTICS: usize = 8;

#[derive(Clone, Debug, Eq, PartialEq)]
struct TidySourceSnapshot {
    external_imports: BTreeSet<String>,
    generation: String,
}

fn capture_stable_tidy_source_snapshot(project_dir: &Path) -> Result<TidySourceSnapshot, Error> {
    let fs = RealFs::new(".");
    let mut previous = capture_tidy_source_snapshot_once(&fs, project_dir)?;
    for _ in 1..MAX_TIDY_SOURCE_SNAPSHOT_ATTEMPTS {
        let current = capture_tidy_source_snapshot_once(&fs, project_dir)?;
        if current == previous {
            return Ok(current);
        }
        previous = current;
    }
    Err(Error::SourceScan(format!(
        "project source tree {} changed during every bounded tidy snapshot attempt",
        project_dir.display(),
    )))
}

fn capture_tidy_source_snapshot_once<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
) -> Result<TidySourceSnapshot, Error> {
    let project_dir = normalize_fs_path(project_dir);
    match fs.entry_kind(&project_dir).map_err(Error::Io)? {
        FileSystemEntryKind::Directory => {}
        kind => {
            return Err(Error::SourceScan(format!(
                "tidy source root {} must be a real directory; found {kind:?}",
                project_dir.display(),
            )))
        }
    }

    let mut pending = vec![(project_dir.clone(), 0usize)];
    let mut visited = BTreeSet::new();
    let mut sources = BTreeMap::<std::path::PathBuf, String>::new();
    let mut external_imports = BTreeSet::new();
    let mut entry_count = 0usize;
    let mut source_bytes = 0usize;

    while let Some((directory, depth)) = pending.pop() {
        if !visited.insert(directory.clone()) {
            continue;
        }
        if depth > crate::schema::MAX_PORTABLE_PATH_COMPONENTS {
            return Err(Error::SourceScan(format!(
                "tidy source scan exceeds the {}-directory depth limit at {}",
                crate::schema::MAX_PORTABLE_PATH_COMPONENTS,
                directory.display(),
            )));
        }
        let mut raw_entries = fs.read_dir(&directory).map_err(Error::Io)?;
        entry_count = entry_count
            .checked_add(raw_entries.len())
            .ok_or_else(|| Error::SourceScan("tidy source entry count overflow".to_string()))?;
        if entry_count > MAX_DIRECTORY_ENTRIES {
            return Err(Error::SourceScan(format!(
                "tidy source scan exceeds the {MAX_DIRECTORY_ENTRIES}-entry limit",
            )));
        }
        sort_fs_paths(&mut raw_entries);
        let mut entries = BTreeSet::new();
        for raw_entry in raw_entries {
            let entry = normalize_fs_path(&raw_entry);
            let parent = normalize_fs_path(entry.parent().unwrap_or_else(|| Path::new(".")));
            if parent != directory {
                return Err(Error::SourceScan(format!(
                    "tidy source directory {} returned non-child entry {}",
                    directory.display(),
                    raw_entry.display(),
                )));
            }
            if !entries.insert(entry.clone()) {
                return Err(Error::SourceScan(format!(
                    "tidy source directory {} returned duplicate entry {}",
                    directory.display(),
                    entry.display(),
                )));
            }
        }

        if let Some((alias, canonical)) =
            crate::schema::first_portable_name_alias(&entries, &["vo.mod"])
        {
            return Err(Error::SourceScan(format!(
                "tidy source directory {} contains portable alias {} for canonical protocol file {canonical}",
                directory.display(),
                alias.display(),
            )));
        }
        if let Some(boundary) = entries
            .iter()
            .find(|entry| entry.file_name() == Some(OsStr::new("vo.mod")))
        {
            let kind = fs.entry_kind(boundary).map_err(Error::Io)?;
            if kind != FileSystemEntryKind::RegularFile {
                return Err(Error::SourceScan(format!(
                    "module boundary {} must be a regular file without links or special entries; found {kind:?}",
                    boundary.display(),
                )));
            }
            if depth > 0 {
                continue;
            }
        }

        let mut child_directories = Vec::new();
        for entry in entries {
            match fs.entry_kind(&entry).map_err(Error::Io)? {
                FileSystemEntryKind::Directory => {
                    if !skip_tidy_source_directory(&entry) {
                        child_directories.push(entry);
                    }
                }
                FileSystemEntryKind::RegularFile
                    if entry.extension() == Some(OsStr::new("vo")) =>
                {
                    if sources.len() == MAX_PACKAGE_SOURCE_FILES {
                        return Err(Error::SourceScan(format!(
                            "tidy source scan exceeds the {MAX_PACKAGE_SOURCE_FILES}-file limit",
                        )));
                    }
                    let content = crate::workspace::read_stable_regular_text_file(
                        fs,
                        &entry,
                        MAX_TEXT_FILE_BYTES,
                        "tidy source file",
                    )?;
                    source_bytes = source_bytes.checked_add(content.len()).ok_or_else(|| {
                        Error::SourceScan("tidy source byte count overflow".to_string())
                    })?;
                    if source_bytes > MAX_TIDY_SOURCE_BYTES {
                        return Err(Error::SourceScan(format!(
                            "tidy source scan exceeds the {MAX_TIDY_SOURCE_BYTES}-byte source limit at {}",
                            entry.display(),
                        )));
                    }
                    collect_tidy_external_imports(&entry, &content, &mut external_imports)?;
                    sources.insert(entry, content);
                }
                FileSystemEntryKind::RegularFile => {}
                kind => {
                    return Err(Error::SourceScan(format!(
                        "tidy source entry {} must be a regular file or directory without links; found {kind:?}",
                        entry.display(),
                    )))
                }
            }
        }
        for child in child_directories.into_iter().rev() {
            pending.push((child, depth + 1));
        }
    }

    let mut hasher = StableHasher::new("vo-mod-tidy-source-generation-v1");
    hasher.update_path("project_root", &project_dir);
    for (path, content) in sources {
        hasher.update_path("source_path", &path);
        hasher.update_str("source_content", &content);
    }
    Ok(TidySourceSnapshot {
        external_imports,
        generation: hasher.finish(),
    })
}

fn skip_tidy_source_directory(path: &Path) -> bool {
    let Some(name) = path.file_name() else {
        return false;
    };
    name.as_encoded_bytes().first() == Some(&b'.')
        || matches!(
            name.to_str(),
            Some("vendor" | "testdata" | "node_modules" | "target" | "dist")
        )
}

fn collect_tidy_external_imports(
    path: &Path,
    content: &str,
    imports: &mut BTreeSet<String>,
) -> Result<(), Error> {
    let (file, diagnostics, _) = vo_syntax::parse(content, 0);
    if diagnostics.has_errors() {
        let mut detail = diagnostics
            .iter()
            .take(MAX_TIDY_PARSE_DIAGNOSTICS)
            .map(|diagnostic| diagnostic.message.as_str())
            .collect::<Vec<_>>()
            .join("; ");
        let omitted = diagnostics.len().saturating_sub(MAX_TIDY_PARSE_DIAGNOSTICS);
        if omitted > 0 {
            detail.push_str(&format!("; ... and {omitted} more diagnostic(s)"));
        }
        return Err(Error::SourceScan(format!(
            "failed to parse {} while scanning tidy imports: {detail}",
            path.display(),
        )));
    }
    for import in &file.imports {
        let import_path = import.path.value.clone();
        if crate::identity::classify_import(&import_path)? == crate::identity::ImportClass::External
        {
            imports.insert(import_path);
        }
    }
    Ok(())
}

#[cfg(test)]
struct TidySourceRevalidationPause {
    reached: std::sync::mpsc::SyncSender<()>,
    resume: std::sync::mpsc::Receiver<()>,
}

#[cfg(test)]
static TIDY_SOURCE_REVALIDATION_PAUSES: std::sync::OnceLock<
    std::sync::Mutex<std::collections::HashMap<std::path::PathBuf, TidySourceRevalidationPause>>,
> = std::sync::OnceLock::new();

#[cfg(test)]
fn pause_tidy_before_source_revalidation_for_test(project_dir: &Path) {
    let pause = TIDY_SOURCE_REVALIDATION_PAUSES
        .get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&normalize_fs_path(project_dir));
    if let Some(pause) = pause {
        pause
            .reached
            .send(())
            .expect("tidy revalidation observer must remain available");
        pause
            .resume
            .recv()
            .expect("tidy revalidation controller must resume the operation");
    }
}

/// Synchronize `vo.mod` dependency entries with actual import usage.
///
/// - Adds missing dependency entries (resolves the latest compatible version).
/// - Removes dependency entries not referenced by any import.
/// - Re-solves the dependency graph and writes `vo.lock`.
pub fn mod_tidy(project_dir: &Path, registry: &dyn Registry) -> Result<TidyResult, Error> {
    let registry = RegistryOperation::new(registry);
    let original_mod = project::read_mod_file_stable(project_dir)?;
    let existing_lock = read_optional_selection_lock_file(project_dir, &original_mod)?;
    let mut mf = original_mod.clone();
    let source_snapshot = capture_stable_tidy_source_snapshot(project_dir)?;
    let external_imports = &source_snapshot.external_imports;
    if external_imports.len() > crate::MAX_SOLVER_GRAPH_EDGES {
        return Err(Error::ResolutionLimitExceeded {
            resource: "external import count for mod tidy".to_string(),
            limit: crate::MAX_SOLVER_GRAPH_EDGES,
        });
    }

    // Build one owner index from the complete validated authority. Looking up
    // each slash prefix makes longest-owner selection proportional to import
    // depth instead of multiplying imports by the whole module graph.
    let mut owner_index = BTreeMap::new();
    for module in mf
        .dependencies
        .iter()
        .map(|dependency| &dependency.module)
        .chain(
            existing_lock
                .iter()
                .flat_map(|lock| lock.modules.iter().map(|locked| &locked.path)),
        )
    {
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
        if let Some(local_path) = import_path.strip_prefix(crate::identity::LOCAL_NAMESPACE_PREFIX)
        {
            let module_end = local_path
                .find('/')
                .map(|offset| crate::identity::LOCAL_NAMESPACE_PREFIX.len() + offset)
                .unwrap_or(import_path.len());
            needed_modules.insert(ModulePath::parse(&import_path[..module_end])?);
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
    if let Some(local) = needed_modules.iter().find(|module| module.is_local()) {
        return Err(Error::DependencyGraph(format!(
            "vo mod tidy cannot select unpublished module {local}; update vo.mod intent and run `vo work sync`"
        )));
    }

    // Compute added and removed modules.
    let existing: BTreeSet<ModulePath> = mf.dependencies.iter().map(|r| r.module.clone()).collect();
    let added: Vec<ModulePath> = needed_modules.difference(&existing).cloned().collect();
    let removed: Vec<ModulePath> = existing.difference(&needed_modules).cloned().collect();

    // Remove unused dependencies.
    mf.dependencies
        .retain(|r| needed_modules.contains(&r.module));
    mf.validate()?;

    // Add new dependencies (resolve the latest compatible version).
    for mp in &added {
        let latest = lifecycle::latest_supported_dependency_version(&mf.vo, mp, &registry)?;
        mf.dependencies.push(Dependency {
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
    require_portable_dependencies(&mf, "vo mod tidy")?;

    let preferences =
        SolvePreferences::preserve_locked(locked_version_preferences(existing_lock.as_ref()));
    let lock_file = lifecycle::prepare_lock_file(&mf, &registry, &preferences)?;

    #[cfg(test)]
    pause_tidy_before_source_revalidation_for_test(project_dir);
    let current_source_snapshot = capture_stable_tidy_source_snapshot(project_dir)?;
    if current_source_snapshot != source_snapshot {
        return Err(Error::SourceScan(
            "project source tree changed while vo mod tidy was resolving; vo.mod and vo.lock were left unchanged"
                .to_string(),
        ));
    }
    project::write_project_files_if_unchanged(
        project_dir,
        &original_mod,
        existing_lock.captured(),
        &mf,
        lock_file.as_ref(),
        "vo mod tidy",
    )?;

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
/// `options` selects the effective build authority or the explicit declared
/// registry view. The returned chain always describes that exact snapshot.
pub fn mod_why(
    project_dir: &Path,
    cache_root: &Path,
    target: &str,
    options: &crate::snapshot::SnapshotOptions,
) -> Result<Vec<String>, Error> {
    crate::snapshot::ProjectSnapshot::capture(project_dir, cache_root, options)?
        .dependency_chain(target)
}

// ============================================================
// vo cache clean
// ============================================================

/// Remove every installed module version from the named cache root.
///
/// This operation is intentionally global and lives under `vo cache`: callers
/// cannot accidentally mistake one project's lock graph for cache-wide liveness.
pub fn cache_clean(cache_root: &Path) -> Result<CleanResult, Error> {
    Ok(CleanResult {
        removed_dirs: lifecycle::clean_cache(cache_root, None)?,
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
            panic!("operation must reject input before registry access")
        }

        fn fetch_manifest_raw(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            panic!("operation must reject input before registry access")
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            panic!("operation must reject input before registry access")
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _artifact: &crate::identity::ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            panic!("operation must reject input before registry access")
        }
    }

    struct SolveOnlyRegistry;

    impl Registry for SolveOnlyRegistry {
        fn list_version_candidates(
            &self,
            _module: &ModulePath,
        ) -> Result<Vec<crate::version::ExactVersion>, Error> {
            Ok(vec![crate::version::ExactVersion::parse("1.0.0").unwrap()])
        }

        fn fetch_manifest_raw(
            &self,
            module: &ModulePath,
            version: &crate::version::ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            crate::schema::manifest::ReleaseManifest {
                format: 1,
                module: module.clone(),
                version: version.clone(),
                vo: crate::version::ToolchainConstraint::parse("0.1.0").unwrap(),
                intent: crate::digest::Digest::from_sha256(
                    format!("{module}@{version}-solve-intent").as_bytes(),
                ),
                dependencies: vec![],
                source: crate::schema::manifest::ManifestSource {
                    name: "source.tar.gz".into(),
                    size: 1,
                    digest: crate::digest::Digest::from_sha256(b"source"),
                    tree: crate::digest::Digest::from_sha256(b"tree"),
                },
                artifacts: vec![],
            }
            .render()
            .map(String::into_bytes)
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            panic!("dependency mutation must not fetch source packages")
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _artifact: &crate::identity::ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            panic!("dependency mutation must not fetch artifacts")
        }
    }

    struct VersionedRegistry {
        versions: BTreeMap<ModulePath, Vec<crate::version::ExactVersion>>,
    }

    impl VersionedRegistry {
        fn new(entries: &[(&str, &[&str])]) -> Self {
            let versions = entries
                .iter()
                .map(|(module, versions)| {
                    (
                        ModulePath::parse(module).unwrap(),
                        versions
                            .iter()
                            .map(|version| crate::version::ExactVersion::parse(version).unwrap())
                            .collect(),
                    )
                })
                .collect();
            Self { versions }
        }
    }

    impl Registry for VersionedRegistry {
        fn list_version_candidates(
            &self,
            module: &ModulePath,
        ) -> Result<Vec<crate::version::ExactVersion>, Error> {
            self.versions
                .get(module)
                .cloned()
                .ok_or_else(|| Error::RegistryNotFound {
                    resource: module.to_string(),
                })
        }

        fn fetch_manifest_raw(
            &self,
            module: &ModulePath,
            version: &crate::version::ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            if !self
                .versions
                .get(module)
                .is_some_and(|versions| versions.contains(version))
            {
                return Err(Error::RegistryNotFound {
                    resource: format!("{module}@{version}"),
                });
            }
            crate::schema::manifest::ReleaseManifest {
                format: 1,
                module: module.clone(),
                version: version.clone(),
                vo: crate::version::ToolchainConstraint::parse("0.1.0").unwrap(),
                intent: crate::digest::Digest::from_sha256(
                    format!("{module}@{version}-versioned-intent").as_bytes(),
                ),
                dependencies: vec![],
                source: crate::schema::manifest::ManifestSource {
                    name: "source.tar.gz".into(),
                    size: 1,
                    digest: crate::digest::Digest::from_sha256(b"source"),
                    tree: crate::digest::Digest::from_sha256(b"tree"),
                },
                artifacts: vec![],
            }
            .render()
            .map(String::into_bytes)
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            panic!("graph mutation must not fetch source packages")
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &crate::version::ExactVersion,
            _artifact: &crate::identity::ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            panic!("graph mutation must not fetch artifacts")
        }
    }

    fn write_two_dependency_project(project: &Path) {
        fs::write(
            project.join("vo.mod"),
            concat!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "\n[dependencies]\n",
                "\"github.com/acme/lib\" = \"^1.0.0\"\n",
                "\"github.com/acme/other\" = \"^1.0.0\"\n",
            ),
        )
        .unwrap();
    }

    fn selected_version(project: &Path, module: &str) -> String {
        let module = ModulePath::parse(module).unwrap();
        project::read_lock_file(project)
            .unwrap()
            .find(&module)
            .unwrap()
            .version
            .to_string()
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
    fn initial_manifest_uses_the_module_protocol_toolchain_authority() {
        let rendered = render_initial_mod_file("github.com/acme/app").unwrap();
        assert_eq!(
            rendered,
            format!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"{}\"\n",
                crate::TOOLCHAIN_CONSTRAINT,
            ),
        );
        assert_eq!(crate::TOOLCHAIN_CONSTRAINT, crate::TOOLCHAIN_VERSION,);

        let explicit = initial_mod_file_with_constraint("github.com/acme/app", "9.8.7").unwrap();
        assert_eq!(explicit.vo.to_string(), "9.8.7");
    }

    #[test]
    fn mod_init_refuses_to_overwrite_existing_manifest() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("vo.mod");
        let original = "format = 1\nmodule = \"github.com/acme/existing\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
        fs::write(&path, original).unwrap();

        let error = mod_init(temp.path(), "github.com/acme/new").unwrap_err();

        assert!(matches!(
            error,
            Error::Io(ref error) if error.kind() == std::io::ErrorKind::AlreadyExists
        ));
        assert_eq!(fs::read_to_string(path).unwrap(), original);
    }

    #[test]
    fn mod_init_rejects_an_orphaned_lock_without_creating_a_manifest() {
        let temp = tempfile::tempdir().unwrap();
        let lock_path = temp.path().join("vo.lock");
        let original = b"unrelated pre-existing lock bytes\n";
        fs::write(&lock_path, original).unwrap();

        let error = mod_init(temp.path(), "github.com/acme/new").unwrap_err();

        assert!(matches!(
            error,
            Error::Io(ref error) if error.kind() == std::io::ErrorKind::AlreadyExists
        ));
        assert!(!temp.path().join("vo.mod").exists());
        assert_eq!(fs::read(lock_path).unwrap(), original);
    }

    #[test]
    fn selection_lock_tolerates_graph_drift_but_rejects_corruption() {
        let temp = tempfile::tempdir().unwrap();
        let empty_mod = ModFile::parse(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        assert!(read_optional_selection_lock_file(temp.path(), &empty_mod)
            .unwrap()
            .is_none());

        // Mutation commands inspect malformed lock bytes even when the
        // authored graph is empty. The two explicit cleanup paths bypass this
        // helper at their call sites.
        fs::write(temp.path().join("vo.lock"), "not valid lock data").unwrap();
        assert!(matches!(
            read_optional_selection_lock_file(temp.path(), &empty_mod).unwrap_err(),
            Error::LockFileParse(_)
        ));

        let mod_file = ModFile::parse(concat!(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n",
            "\n[dependencies]\n",
            "\"github.com/acme/lib\" = \"^1.0.0\"\n",
        ))
        .unwrap();
        fs::write(temp.path().join("vo.lock"), "not valid lock data").unwrap();
        assert!(matches!(
            read_optional_selection_lock_file(temp.path(), &mod_file).unwrap_err(),
            Error::LockFileParse(_)
        ));

        let drifted_lock = crate::schema::lockfile::LockFile {
            format: crate::schema::lockfile::LOCK_FILE_VERSION,
            root: crate::lock::module_intent_digest(&mod_file).unwrap(),
            modules: vec![crate::schema::lockfile::LockedModule {
                path: ModulePath::parse("github.com/acme/other").unwrap(),
                version: crate::version::ExactVersion::parse("1.0.0").unwrap(),
                origin: crate::schema::lockfile::LockOrigin::Registry,
                release: Some(crate::digest::Digest::from_sha256(b"release")),
                intent: None,
            }],
        };
        fs::write(temp.path().join("vo.lock"), drifted_lock.render().unwrap()).unwrap();
        assert!(read_optional_selection_lock_file(temp.path(), &mod_file)
            .unwrap()
            .is_some());

        let mut different_root = drifted_lock;
        different_root.root = crate::digest::Digest::from_sha256(b"different-root-intent");
        fs::write(
            temp.path().join("vo.lock"),
            different_root.render().unwrap(),
        )
        .unwrap();
        assert!(read_optional_selection_lock_file(temp.path(), &mod_file)
            .unwrap()
            .is_some());
    }

    #[test]
    fn empty_graph_mutations_reject_malformed_lock_without_writes() {
        let mod_bytes =
            b"format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
        let lock_bytes = b"malformed lock that must remain visible\n";

        let add_project = tempfile::tempdir().unwrap();
        fs::write(add_project.path().join("vo.mod"), mod_bytes).unwrap();
        fs::write(add_project.path().join("vo.lock"), lock_bytes).unwrap();
        let error = mod_add(
            add_project.path(),
            "github.com/acme/lib",
            Some("^1.0.0"),
            &PanicRegistry,
        )
        .unwrap_err();
        assert!(matches!(error, Error::LockFileParse(_)), "{error}");
        assert_eq!(
            fs::read(add_project.path().join("vo.mod")).unwrap(),
            mod_bytes
        );
        assert_eq!(
            fs::read(add_project.path().join("vo.lock")).unwrap(),
            lock_bytes
        );

        let tidy_project = tempfile::tempdir().unwrap();
        fs::write(tidy_project.path().join("vo.mod"), mod_bytes).unwrap();
        fs::write(tidy_project.path().join("vo.lock"), lock_bytes).unwrap();
        let error = mod_tidy(tidy_project.path(), &PanicRegistry).unwrap_err();
        assert!(matches!(error, Error::LockFileParse(_)), "{error}");
        assert_eq!(
            fs::read(tidy_project.path().join("vo.mod")).unwrap(),
            mod_bytes
        );
        assert_eq!(
            fs::read(tidy_project.path().join("vo.lock")).unwrap(),
            lock_bytes
        );

        let targeted_update_project = tempfile::tempdir().unwrap();
        fs::write(targeted_update_project.path().join("vo.mod"), mod_bytes).unwrap();
        fs::write(targeted_update_project.path().join("vo.lock"), lock_bytes).unwrap();
        let error = mod_update(
            targeted_update_project.path(),
            Some("github.com/acme/lib"),
            &PanicRegistry,
        )
        .unwrap_err();
        assert!(matches!(error, Error::LockFileParse(_)), "{error}");
        assert_eq!(
            fs::read(targeted_update_project.path().join("vo.mod")).unwrap(),
            mod_bytes
        );
        assert_eq!(
            fs::read(targeted_update_project.path().join("vo.lock")).unwrap(),
            lock_bytes
        );
    }

    #[test]
    fn dependency_mutation_solves_and_writes_without_materializing_cache() {
        let project = tempfile::tempdir().unwrap();
        std::fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();

        mod_add(
            project.path(),
            "github.com/acme/lib",
            Some("^1.0.0"),
            &SolveOnlyRegistry,
        )
        .unwrap();

        let mod_file = project::read_mod_file(project.path()).unwrap();
        assert_eq!(mod_file.dependencies.len(), 1);
        let lock_file = project::read_lock_file(project.path()).unwrap();
        assert_eq!(lock_file.modules.len(), 1);
    }

    #[test]
    fn mutation_commands_have_explicit_stable_update_boundaries() {
        let project = tempfile::tempdir().unwrap();
        write_two_dependency_project(project.path());
        let initial = VersionedRegistry::new(&[
            ("github.com/acme/lib", &["1.0.0"]),
            ("github.com/acme/other", &["1.0.0"]),
        ]);
        mod_sync(project.path(), &initial).unwrap();

        let expanded = VersionedRegistry::new(&[
            ("github.com/acme/lib", &["1.0.0", "1.1.0"]),
            ("github.com/acme/other", &["1.0.0", "1.1.0"]),
            ("github.com/acme/third", &["1.0.0", "1.1.0"]),
        ]);

        // Sync repairs the graph while retaining every still-valid selection.
        mod_sync(project.path(), &expanded).unwrap();
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.0.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/other"),
            "1.0.0"
        );

        // A targeted update advances only its selected module.
        mod_update(project.path(), Some("github.com/acme/other"), &expanded).unwrap();
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.0.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/other"),
            "1.1.0"
        );

        // Add/update targets the named module and leaves unrelated nodes stable.
        mod_add(
            project.path(),
            "github.com/acme/third",
            Some("^1.0.0"),
            &expanded,
        )
        .unwrap();
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.0.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/other"),
            "1.1.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/third"),
            "1.1.0"
        );

        mod_remove(project.path(), "github.com/acme/third", &expanded).unwrap();
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.0.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/other"),
            "1.1.0"
        );

        // An untargeted update is the one command that deliberately advances all.
        mod_update(project.path(), None, &expanded).unwrap();
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.1.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/other"),
            "1.1.0"
        );
    }

    #[test]
    fn targeted_update_requires_target_in_resolved_graph_without_writes() {
        let project = tempfile::tempdir().unwrap();
        let mod_bytes = concat!(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n",
            "\n[dependencies]\n",
            "\"github.com/acme/current\" = \"^1.0.0\"\n",
        )
        .as_bytes();
        fs::write(project.path().join("vo.mod"), mod_bytes).unwrap();

        // This structurally valid lock represents prior graph intent. The
        // named module remains eligible as an update target until solving the
        // current authored graph proves that it has disappeared.
        let prior_lock = crate::schema::lockfile::LockFile {
            format: crate::schema::lockfile::LOCK_FILE_VERSION,
            root: crate::lock::module_intent_digest(
                &ModFile::parse(std::str::from_utf8(mod_bytes).unwrap()).unwrap(),
            )
            .unwrap(),
            modules: vec![crate::schema::lockfile::LockedModule {
                path: ModulePath::parse("github.com/acme/prior").unwrap(),
                version: crate::version::ExactVersion::parse("1.0.0").unwrap(),
                origin: crate::schema::lockfile::LockOrigin::Registry,
                release: Some(crate::digest::Digest::from_sha256(b"prior release")),
                intent: None,
            }],
        };
        let lock_bytes = prior_lock.render().unwrap().into_bytes();
        fs::write(project.path().join("vo.lock"), &lock_bytes).unwrap();

        let registry = VersionedRegistry::new(&[("github.com/acme/current", &["1.0.0"])]);
        let error =
            mod_update(project.path(), Some("github.com/acme/prior"), &registry).unwrap_err();

        assert!(matches!(error, Error::DependencyGraph(_)), "{error}");
        assert!(
            error
                .to_string()
                .contains("update target github.com/acme/prior is absent from the current dependency graph after resolution"),
            "{error}"
        );
        assert_eq!(fs::read(project.path().join("vo.mod")).unwrap(), mod_bytes);
        assert_eq!(
            fs::read(project.path().join("vo.lock")).unwrap(),
            lock_bytes
        );
    }

    #[test]
    fn sync_repairs_manifest_drift_without_upgrading_unchanged_nodes() {
        let project = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            concat!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "\n[dependencies]\n",
                "\"github.com/acme/lib\" = \"^1.0.0\"\n",
            ),
        )
        .unwrap();
        let initial = VersionedRegistry::new(&[("github.com/acme/lib", &["1.0.0"])]);
        mod_sync(project.path(), &initial).unwrap();

        write_two_dependency_project(project.path());
        let expanded = VersionedRegistry::new(&[
            ("github.com/acme/lib", &["1.0.0", "1.1.0"]),
            ("github.com/acme/other", &["1.0.0", "1.1.0"]),
        ]);
        mod_sync(project.path(), &expanded).unwrap();
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.0.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/other"),
            "1.1.0"
        );

        // Root drift regenerates authority while retaining valid selections.
        let mut mod_file = project::read_mod_file(project.path()).unwrap();
        mod_file.vo = crate::version::ToolchainConstraint::parse("0.1.1").unwrap();
        fs::write(project.path().join("vo.mod"), mod_file.render().unwrap()).unwrap();
        mod_sync(project.path(), &expanded).unwrap();
        assert_eq!(
            project::read_lock_file(project.path()).unwrap().root,
            crate::lock::module_intent_digest(&mod_file).unwrap()
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.0.0"
        );
    }

    #[test]
    fn tidy_preserves_versions_for_dependencies_that_remain_in_use() {
        let project = tempfile::tempdir().unwrap();
        write_two_dependency_project(project.path());
        let initial = VersionedRegistry::new(&[
            ("github.com/acme/lib", &["1.0.0"]),
            ("github.com/acme/other", &["1.0.0"]),
        ]);
        mod_sync(project.path(), &initial).unwrap();
        let expanded = VersionedRegistry::new(&[
            ("github.com/acme/lib", &["1.0.0", "1.1.0"]),
            ("github.com/acme/other", &["1.0.0", "1.1.0"]),
        ]);
        fs::write(
            project.path().join("main.vo"),
            concat!(
                "package main\n",
                "import \"github.com/acme/lib/pkg\"\n",
                "import \"github.com/acme/other/pkg\"\n",
            ),
        )
        .unwrap();

        let result = mod_tidy(project.path(), &expanded).unwrap();

        assert!(result.added.is_empty());
        assert!(result.removed.is_empty());
        assert_eq!(
            selected_version(project.path(), "github.com/acme/lib"),
            "1.0.0"
        );
        assert_eq!(
            selected_version(project.path(), "github.com/acme/other"),
            "1.0.0"
        );
    }

    #[test]
    fn tidy_stops_at_nested_module_boundaries_and_rejects_their_aliases() {
        let project = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(project.path().join("main.vo"), "package main\n").unwrap();
        fs::create_dir(project.path().join("nested")).unwrap();
        fs::write(
            project.path().join("nested/vo.mod"),
            "format = 1\nmodule = \"github.com/acme/nested\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(
            project.path().join("nested/main.vo"),
            "package nested\nimport \"github.com/acme/hidden/pkg\"\n",
        )
        .unwrap();

        let result = mod_tidy(project.path(), &PanicRegistry).unwrap();
        assert!(result.added.is_empty());
        assert!(result.removed.is_empty());
        assert!(!project.path().join("vo.lock").exists());

        let mut memory = vo_common::vfs::MemoryFs::new();
        memory.add_file(
            "project/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        memory.add_file("project/nested/VO.MOD", "alias");
        memory.add_file("project/nested/main.vo", "package nested\n");
        let error = capture_tidy_source_snapshot_once(&memory, Path::new("project")).unwrap_err();
        assert!(error.to_string().contains("portable alias"), "{error}");
        assert!(error.to_string().contains("VO.MOD"), "{error}");
    }

    #[test]
    fn tidy_revalidates_the_complete_source_generation_before_commit() {
        let project = tempfile::tempdir().unwrap();
        let mod_bytes =
            b"format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
        fs::write(project.path().join("vo.mod"), mod_bytes).unwrap();
        let source_path = project.path().join("main.vo");
        fs::write(&source_path, "package main\n").unwrap();

        let (reached_tx, reached_rx) = std::sync::mpsc::sync_channel(1);
        let (resume_tx, resume_rx) = std::sync::mpsc::sync_channel(1);
        assert!(TIDY_SOURCE_REVALIDATION_PAUSES
            .get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                normalize_fs_path(project.path()),
                TidySourceRevalidationPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let project_dir = project.path().to_path_buf();
        let tidy = std::thread::spawn(move || mod_tidy(&project_dir, &PanicRegistry));
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        fs::write(&source_path, "package main\n\n// same imports, new bytes\n").unwrap();
        resume_tx.send(()).unwrap();

        let error = tidy.join().unwrap().unwrap_err();
        assert!(
            matches!(error, Error::SourceScan(ref detail) if detail.contains("changed while vo mod tidy was resolving")),
            "{error}",
        );
        assert_eq!(fs::read(project.path().join("vo.mod")).unwrap(), mod_bytes);
        assert!(!project.path().join("vo.lock").exists());
        assert!(!project.path().join(".vo-project.transaction").exists());
    }

    #[test]
    fn empty_graph_lifecycle_uses_one_explicit_lockless_state() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(project.path().join("vo.lock"), "stale lock data for update").unwrap();

        mod_update(project.path(), None, &PanicRegistry).unwrap();
        assert!(!project.path().join("vo.lock").exists());

        fs::write(project.path().join("vo.lock"), "stale lock data for sync").unwrap();

        assert_eq!(
            mod_sync(project.path(), &PanicRegistry).unwrap(),
            LockFileStatus::NotRequired
        );
        assert!(!project.path().join("vo.lock").exists());
        assert_eq!(
            mod_verify(project.path(), cache.path()).unwrap(),
            LockFileStatus::NotRequired
        );
        assert_eq!(
            mod_fetch(project.path(), cache.path(), &PanicRegistry).unwrap(),
            LockFileStatus::NotRequired
        );
    }

    #[test]
    fn missing_lock_is_an_error_when_external_dependencies_exist() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            concat!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "[dependencies]\n",
                "\"github.com/acme/lib\" = \"0.1.0\"\n",
            ),
        )
        .unwrap();

        let error = mod_verify(project.path(), cache.path()).unwrap_err();
        assert!(error
            .to_string()
            .contains("vo.lock is required whenever vo.mod declares external dependencies"));
    }

    #[test]
    fn any_lock_is_rejected_for_the_canonical_lockless_graph() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(project.path().join("vo.lock"), "malformed").unwrap();

        assert!(matches!(
            mod_verify(project.path(), cache.path()).unwrap_err(),
            Error::DependencyGraph(_)
        ));
        assert!(matches!(
            mod_fetch(project.path(), cache.path(), &PanicRegistry).unwrap_err(),
            Error::DependencyGraph(_)
        ));
    }

    #[test]
    fn why_handles_the_canonical_lockless_graph_semantically() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        let options = crate::snapshot::SnapshotOptions::declared();
        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();

        assert_eq!(
            mod_why(
                project.path(),
                cache.path(),
                "github.com/acme/app",
                &options,
            )
            .unwrap(),
            vec!["github.com/acme/app"]
        );
        let error = mod_why(
            project.path(),
            cache.path(),
            "github.com/acme/lib",
            &options,
        )
        .unwrap_err();
        assert!(matches!(&error, Error::DependencyGraph(_)));
        assert!(error
            .to_string()
            .contains("github.com/acme/lib is not in the module dependency graph"));
    }

    #[test]
    fn why_explains_the_exact_locked_workspace_graph() {
        let root = tempfile::tempdir().unwrap();
        let root = root.path().canonicalize().unwrap();
        let app = root.join("app");
        let library = root.join("lib");
        let cache = root.join("cache");
        fs::create_dir(&app).unwrap();
        fs::create_dir(&library).unwrap();
        fs::write(
            root.join("vo.work"),
            "format = 1\nmembers = [\"app\", \"lib\"]\n",
        )
        .unwrap();
        fs::write(
            app.join("vo.mod"),
            concat!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "[dependencies]\n",
                "\"github.com/acme/lib\" = \"0.1.0\"\n",
            ),
        )
        .unwrap();
        fs::write(
            library.join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        assert_eq!(
            work_sync(&app, &PanicRegistry).unwrap(),
            LockFileStatus::Present
        );

        let effective = crate::snapshot::SnapshotOptions {
            mode: crate::snapshot::GraphMode::Effective,
            workspace: crate::workspace::WorkspaceDiscovery::Auto,
        };
        assert_eq!(
            mod_why(&app, &cache, "github.com/acme/lib", &effective,).unwrap(),
            ["github.com/acme/app", "github.com/acme/lib"]
        );
    }

    #[test]
    fn read_only_commands_do_not_create_a_project_lock() {
        let project = tempfile::tempdir().unwrap();
        let cache = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();

        assert_eq!(
            mod_verify(project.path(), cache.path()).unwrap(),
            LockFileStatus::NotRequired
        );
        assert_eq!(
            mod_why(
                project.path(),
                cache.path(),
                "github.com/acme/app",
                &crate::snapshot::SnapshotOptions::declared(),
            )
            .unwrap(),
            ["github.com/acme/app"]
        );
        assert!(!project.path().join(".vo-project.lock").exists());
    }

    #[test]
    fn read_only_lifecycle_revalidation_rejects_graph_drift() {
        let project = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        let (mod_file, lock_file) = read_stable_declared_graph(project.path()).unwrap();
        ensure_declared_graph_unchanged(
            project.path(),
            &mod_file,
            lock_file.as_ref(),
            "vo mod verify",
        )
        .unwrap();

        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/replaced\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        let error = ensure_declared_graph_unchanged(
            project.path(),
            &mod_file,
            lock_file.as_ref(),
            "vo mod verify",
        )
        .unwrap_err();
        assert!(matches!(error, Error::DependencyGraph(_)), "{error}");
        assert!(
            error
                .to_string()
                .contains("project declaration changed while vo mod verify was running"),
            "{error}",
        );
    }

    #[test]
    fn mod_verify_does_not_initialize_a_missing_module_cache() {
        let project = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        mod_add(
            project.path(),
            "github.com/acme/lib",
            Some("^1.0.0"),
            &SolveOnlyRegistry,
        )
        .unwrap();
        let missing_cache = project.path().join("missing-cache");

        mod_verify(project.path(), &missing_cache).unwrap_err();
        assert!(!missing_cache.exists());
    }

    #[test]
    fn mod_tidy_validates_imports_even_when_an_existing_module_prefix_matches() {
        let project = tempfile::tempdir().unwrap();
        fs::write(
            project.path().join("vo.mod"),
            concat!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "[dependencies]\n",
                "\"github.com/acme/lib\" = \"^1.0.0\"\n",
            ),
        )
        .unwrap();
        fs::write(
            project.path().join("main.vo"),
            "package main\nimport \"github.com/acme/lib/../escape\"\n",
        )
        .unwrap();

        let error = mod_tidy(project.path(), &PanicRegistry).unwrap_err();

        assert!(matches!(error, Error::InvalidImportPath(_)), "{error}");
    }
}
