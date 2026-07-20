use std::collections::BTreeMap;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::atomic::{AtomicU64, Ordering};

use vo_common::vfs::{FileSystem, RealFs};

use crate::artifact::required_artifacts_for_target;
use crate::async_solver::RootRequirement;
use crate::cache::install::ExtractedSourceFile;
use crate::cache::layout::{relative_module_dir, SOURCE_DIGEST_MARKER, VERSION_MARKER};
use crate::cache::validate::{
    validate_installed_artifact, validate_installed_module,
    validate_installed_module_with_metadata, InstalledModuleError, InstalledModuleMetadata,
};
use crate::digest::{verify_size_and_digest, Digest};
use crate::identity::{ArtifactId, ModulePath};
use crate::lock::{locked_module_from_manifest_raw, validate_locked_module_against_manifest};
use crate::project;
use crate::readiness::{check_module_readiness, ModuleReadiness, ReadinessFailure, ReadyModule};
use crate::schema::lockfile::{LockFile, LockedModule};
use crate::schema::manifest::{ManifestArtifact, ReleaseManifest};
use crate::schema::modfile::ModFile;
use crate::schema::TreeManifest;
use crate::version::{DepConstraint, ExactVersion};
use crate::{Error, Result};

pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

#[derive(Debug)]
pub enum SourcePayload {
    Package(Vec<u8>),
    Files {
        source_files: Vec<ExtractedSourceFile>,
        tree_raw: Vec<u8>,
    },
}

/// One frozen async solve, its canonical lock, and the exact materialization
/// result installed from the same manifest bytes.
#[derive(Debug)]
pub struct ResolvedModFileInstall {
    /// The canonical lock for a non-empty external graph. Dependency-free
    /// modules have no lockfile by protocol.
    pub lock_file: Option<LockFile>,
    pub ready: Vec<ReadyModule>,
}

pub trait AsyncRegistry {
    /// List untrusted published release candidates. Callers must validate a
    /// candidate's raw manifest before selecting it.
    fn list_version_candidates<'a>(
        &'a self,
        module: &'a ModulePath,
    ) -> BoxFuture<'a, Result<Vec<ExactVersion>>>;

    fn fetch_manifest_raw<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
    ) -> BoxFuture<'a, Result<Vec<u8>>>;

    fn fetch_source<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
        asset_name: &'a str,
    ) -> BoxFuture<'a, Result<SourcePayload>>;

    fn fetch_artifact<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
        artifact: &'a ArtifactId,
    ) -> BoxFuture<'a, Result<Vec<u8>>>;
}

pub trait InstallSurface: FileSystem {
    /// Whether this surface is the native real filesystem implementation and
    /// can use descriptor-anchored cache transactions directly.
    fn supports_anchored_transactions(&self) -> bool {
        false
    }

    /// Remove `path` and all of its descendants, succeeding when it is absent.
    /// Implementations must keep the operation confined to their configured root.
    fn remove_tree(&self, path: &Path) -> Result<()>;

    fn mkdir_all(&self, path: &Path) -> Result<()>;

    /// Create exactly one new directory and fail if the leaf already exists.
    fn create_dir(&self, path: &Path) -> Result<()>;

    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> Result<()>;

    /// Preserve authenticated POSIX source mode when the surface can express
    /// it. Windows and virtual browser filesystems intentionally ignore the
    /// host execution semantic while retaining it in `vo.tree.json`.
    fn set_source_mode(&self, _path: &Path, _mode: crate::schema::SourceFileMode) -> Result<()> {
        Ok(())
    }

    /// Atomically publish one confined tree at a missing destination.
    /// Implementations must return `AlreadyExists` without changing either
    /// path when the destination exists; check-and-replace is forbidden.
    fn publish_noreplace(&self, from: &Path, to: &Path) -> Result<()>;

    fn write_text(&self, path: &Path, content: &str) -> Result<()> {
        self.write_bytes(path, content.as_bytes())
    }
}

fn parse_requested_manifest_bytes(
    manifest_raw: &[u8],
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<ReleaseManifest> {
    crate::registry::parse_manifest_bytes(manifest_raw, module, version)
}

fn scoped_install_path<F: FileSystem>(
    fs: &F,
    path: &Path,
    allow_leaf_symlink: bool,
) -> Result<PathBuf> {
    crate::schema::portable_relative_path_from_path(path).map_err(|error| {
        Error::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("invalid install path {}: {error}", path.display()),
        ))
    })?;
    let Some(root) = fs.root() else {
        return Ok(path.to_path_buf());
    };
    crate::cache::mutation_lock::require_real_directory(root, "module cache root")?;

    // Lexical validation alone cannot confine a mutation when an existing
    // path component is a symlink (or another platform-specific redirect).
    // Reject redirects in every ancestor and verify canonicalized existing
    // components stay under the configured root. `remove_tree` may unlink a
    // symlink at the leaf itself, so callers opt into that narrow exception.
    let canonical_root = std::fs::canonicalize(root).map_err(Error::Io)?;
    let component_count = path.components().count();
    let mut candidate = root.to_path_buf();
    for (index, component) in path.components().enumerate() {
        candidate.push(component.as_os_str());
        let is_leaf = index + 1 == component_count;
        if is_leaf && allow_leaf_symlink {
            match std::fs::symlink_metadata(&candidate) {
                Ok(_) => crate::cache::mutation_lock::require_exact_leaf_spelling(
                    &candidate,
                    "install path",
                )?,
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                    crate::cache::mutation_lock::reject_portable_leaf_aliases(
                        &candidate,
                        "install path",
                    )?;
                }
                Err(error) => return Err(Error::Io(error)),
            }
            break;
        }
        let metadata = match std::fs::symlink_metadata(&candidate) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                crate::cache::mutation_lock::reject_portable_leaf_aliases(
                    &candidate,
                    "install path",
                )?;
                break;
            }
            Err(error) => return Err(Error::Io(error)),
        };
        crate::cache::mutation_lock::require_exact_leaf_spelling(&candidate, "install path")?;
        if metadata.file_type().is_symlink() {
            return Err(Error::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "install path {} traverses symbolic link {}",
                    path.display(),
                    candidate.display()
                ),
            )));
        }
        let canonical_candidate = std::fs::canonicalize(&candidate).map_err(Error::Io)?;
        if !canonical_candidate.starts_with(&canonical_root) {
            return Err(Error::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "install path {} escapes configured root {} through {}",
                    path.display(),
                    root.display(),
                    candidate.display()
                ),
            )));
        }
    }
    Ok(root.join(path))
}

impl InstallSurface for RealFs {
    fn supports_anchored_transactions(&self) -> bool {
        true
    }

    fn remove_tree(&self, path: &Path) -> Result<()> {
        let path = scoped_install_path(self, path, true)?;
        let metadata = match std::fs::symlink_metadata(&path) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(error) => return Err(Error::Io(error)),
        };
        if metadata.is_dir() && !metadata.file_type().is_symlink() {
            std::fs::remove_dir_all(path)?;
        } else {
            std::fs::remove_file(path)?;
        }
        Ok(())
    }

    fn mkdir_all(&self, path: &Path) -> Result<()> {
        scoped_install_path(self, path, false)?;
        let mut current = self.root().to_path_buf();
        for component in path.components() {
            let std::path::Component::Normal(component) = component else {
                return Err(Error::Io(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("install directory must be canonical: {}", path.display()),
                )));
            };
            current.push(component);
            crate::cache::mutation_lock::ensure_real_directory(&current, "install directory")?;
        }
        Ok(())
    }

    fn create_dir(&self, path: &Path) -> Result<()> {
        let path = scoped_install_path(self, path, false)?;
        crate::cache::mutation_lock::reject_portable_leaf_aliases(&path, "install directory")?;
        std::fs::create_dir(&path)?;
        crate::cache::mutation_lock::require_real_directory(&path, "install directory")?;
        Ok(())
    }

    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> Result<()> {
        use std::io::Write;

        let path = scoped_install_path(self, path, false)?;
        crate::cache::mutation_lock::reject_portable_leaf_aliases(&path, "install file")?;
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path)?;
        file.write_all(bytes)?;
        file.sync_all()?;
        Ok(())
    }

    fn publish_noreplace(&self, from: &Path, to: &Path) -> Result<()> {
        let from = scoped_install_path(self, from, false)?;
        let to = scoped_install_path(self, to, false)?;
        crate::cache::mutation_lock::require_exact_leaf_spelling(&from, "install source")?;
        crate::cache::mutation_lock::reject_portable_leaf_aliases(&to, "install destination")?;
        match std::fs::symlink_metadata(&to) {
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Ok(_) => {
                return Err(Error::Io(std::io::Error::new(
                    std::io::ErrorKind::AlreadyExists,
                    format!("install destination already exists: {}", to.display()),
                )));
            }
            Err(error) => return Err(Error::Io(error)),
        }
        sync_native_staged_path(&from)?;
        let from_parent = from.parent().map(Path::to_path_buf);
        let to_parent = to.parent().map(Path::to_path_buf);
        let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(self.root())?;
        mutation_lock.publish_noreplace(&from, &to)?;
        if let Some(parent) = from_parent {
            sync_native_directory(&parent)?;
        }
        if let Some(parent) = to_parent {
            sync_native_directory(&parent)?;
        }
        Ok(())
    }
}

#[cfg(unix)]
const MAX_NATIVE_STAGED_TREE_DEPTH: usize = crate::schema::MAX_PORTABLE_PATH_COMPONENTS;

#[cfg(unix)]
fn sync_native_staged_path(path: &Path) -> Result<()> {
    let metadata = std::fs::symlink_metadata(path)?;
    if metadata.is_file() {
        std::fs::File::open(path)?.sync_all()?;
        return Ok(());
    }
    if !metadata.is_dir() || metadata.file_type().is_symlink() {
        return Err(Error::SourceScan(format!(
            "staged install path must be a regular file or directory: {}",
            path.display(),
        )));
    }
    let mut pending = vec![(path.to_path_buf(), 0usize)];
    let mut directories = Vec::new();
    let mut visited_entries = 0usize;
    let max_materialized_entries = crate::MAX_SOURCE_ARCHIVE_ENTRIES
        .saturating_add(crate::schema::source_files::CACHE_OWNED_ROOT_ENTRY_BUDGET);
    while let Some((directory, depth)) = pending.pop() {
        directories.push(directory.clone());
        for entry in std::fs::read_dir(&directory)? {
            let child = entry?.path();
            visited_entries = visited_entries.checked_add(1).ok_or_else(|| {
                Error::SourceScan("staged install entry count overflows usize".to_string())
            })?;
            if visited_entries > max_materialized_entries {
                return Err(Error::SourceScan(format!(
                    "staged install tree contains more than {} entries",
                    max_materialized_entries,
                )));
            }
            let metadata = std::fs::symlink_metadata(&child)?;
            if metadata.file_type().is_symlink() {
                return Err(Error::SourceScan(format!(
                    "staged install tree contains symbolic link {}",
                    child.display(),
                )));
            }
            if metadata.is_dir() {
                let child_depth = depth.checked_add(1).ok_or_else(|| {
                    Error::SourceScan("staged install directory depth overflows usize".to_string())
                })?;
                if child_depth > MAX_NATIVE_STAGED_TREE_DEPTH {
                    return Err(Error::SourceScan(format!(
                        "staged install tree exceeds the {MAX_NATIVE_STAGED_TREE_DEPTH}-level depth limit at {}",
                        child.display(),
                    )));
                }
                pending.push((child, child_depth));
            } else if metadata.is_file() {
                std::fs::File::open(child)?.sync_all()?;
            } else {
                return Err(Error::SourceScan(
                    "staged install tree contains a special file".to_string(),
                ));
            }
        }
    }
    for directory in directories.into_iter().rev() {
        sync_native_directory(&directory)?;
    }
    Ok(())
}

#[cfg(not(unix))]
fn sync_native_staged_path(_path: &Path) -> Result<()> {
    Ok(())
}

#[cfg(unix)]
fn sync_native_directory(path: &Path) -> Result<()> {
    std::fs::File::open(path)?.sync_all()?;
    Ok(())
}

#[cfg(not(unix))]
fn sync_native_directory(_path: &Path) -> Result<()> {
    Ok(())
}

static INSTALL_TRANSACTION_COUNTER: AtomicU64 = AtomicU64::new(0);

struct ManifestSnapshot {
    manifest: ReleaseManifest,
    raw: Vec<u8>,
}

impl ManifestSnapshot {
    fn try_new(module: &ModulePath, version: &ExactVersion, raw: Vec<u8>) -> Result<Self> {
        // Registry implementations are untrusted protocol adapters. Raw bytes
        // are the digest-bound authority, so derive the typed value from them
        // instead of accepting a separately supplied Rust value.
        let manifest = parse_requested_manifest_bytes(&raw, module, version)?;
        Ok(Self { manifest, raw })
    }
}

struct PreparedSourceTree {
    files: Vec<ExtractedSourceFile>,
    tree_raw: Vec<u8>,
}

pub async fn resolve_latest_version<R: AsyncRegistry>(
    registry: &R,
    module: &ModulePath,
) -> Result<ExactVersion> {
    resolve_direct_request_version(registry, module, None).await
}

pub async fn resolve_version_with_constraint<R: AsyncRegistry>(
    registry: &R,
    module: &ModulePath,
    constraint: &DepConstraint,
) -> Result<ExactVersion> {
    resolve_direct_request_version(registry, module, Some(constraint.clone())).await
}

/// A direct-version query still solves the selected release's complete graph.
/// This keeps candidate validity, infrastructure-error handling, snapshot
/// stability, and aggregate work budgets identical to installation requests.
async fn resolve_direct_request_version<R: AsyncRegistry>(
    registry: &R,
    module: &ModulePath,
    constraint: Option<DepConstraint>,
) -> Result<ExactVersion> {
    let requirement = RootRequirement {
        module: module.clone(),
        constraint,
    };
    let mut graph = crate::async_solver::solve(
        "direct module version request",
        None,
        &[requirement],
        registry,
        &BTreeMap::new(),
    )
    .await?;
    graph
        .modules
        .remove(module)
        .map(|resolved| resolved.version)
        .ok_or_else(|| Error::NoSatisfyingVersion {
            module: module.as_str().to_string(),
            detail: "resolved graph omitted the directly requested module".to_string(),
        })
}

async fn ensure_resolved_graph<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    graph: crate::solver::ResolvedGraph,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    let mut budget = crate::registry::MaterializedGraphBudget::default();
    let mut ready = Vec::new();
    ready
        .try_reserve(graph.modules.len())
        .map_err(|_| Error::ResolutionLimitExceeded {
            resource: "ready module graph allocation".to_string(),
            limit: crate::MAX_MODULE_DEPENDENCIES,
        })?;
    for (_, resolved) in graph.modules {
        let manifest = ManifestSnapshot {
            manifest: resolved.manifest,
            raw: resolved.manifest_raw,
        };
        let locked = locked_module_from_manifest_raw(&manifest.manifest, &manifest.raw);
        ready.push(
            ensure_locked_module_ready_with_budget(
                surface,
                registry,
                &locked,
                target,
                Some(manifest),
                &mut budget,
            )
            .await?,
        );
    }
    Ok(ready)
}

pub async fn ensure_project_plan<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    project_plan: &project::ProjectPlan,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    if !project_plan.has_mod_file() || project_plan.locked_modules().is_empty() {
        return Ok(Vec::new());
    }
    let normalized = normalize_locked_modules(project_plan.locked_modules().to_vec())?;
    let mut budget = crate::registry::MaterializedGraphBudget::default();
    let mut ready = Vec::new();
    ready
        .try_reserve(normalized.len())
        .map_err(|_| Error::ResolutionLimitExceeded {
            resource: "ready module graph allocation".to_string(),
            limit: crate::MAX_MODULE_DEPENDENCIES,
        })?;
    for locked in &normalized {
        if locked.origin == crate::schema::lockfile::LockOrigin::Workspace {
            continue;
        }
        ready.push(
            ensure_locked_module_ready_with_budget(
                surface,
                registry,
                locked,
                target,
                None,
                &mut budget,
            )
            .await?,
        );
    }
    Ok(ready)
}

pub async fn ensure_mod_file_dependencies<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    mod_file: &ModFile,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    Ok(
        resolve_mod_file_lock_and_ensure(surface, registry, mod_file, target)
            .await?
            .ready,
    )
}

/// Solve, lock, and materialize a root module from one frozen registry graph.
/// The returned lock is generated before the graph is consumed by install, so
/// cache metadata and nested dependency locks cannot influence selection.
pub async fn resolve_mod_file_lock_and_ensure<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    mod_file: &ModFile,
    target: &str,
) -> Result<ResolvedModFileInstall> {
    mod_file.validate()?;
    if mod_file.dependencies.is_empty() {
        return Ok(ResolvedModFileInstall {
            lock_file: None,
            ready: Vec::new(),
        });
    }
    let requirements = mod_file
        .dependencies
        .iter()
        .map(|req| RootRequirement {
            module: req.module.clone(),
            constraint: Some(req.constraint.clone()),
        })
        .collect::<Vec<_>>();
    let preferred_versions = BTreeMap::new();
    let graph = crate::async_solver::solve(
        mod_file.module.as_str(),
        Some(&mod_file.vo),
        &requirements,
        registry,
        &preferred_versions,
    )
    .await?;
    let lock_file = crate::lock::generate_lock(mod_file, &graph)?;
    let ready = ensure_resolved_graph(surface, registry, graph, target).await?;
    Ok(ResolvedModFileInstall {
        lock_file: Some(lock_file),
        ready,
    })
}

fn normalize_locked_modules(locked_modules: Vec<LockedModule>) -> Result<Vec<LockedModule>> {
    if locked_modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::ResolutionLimitExceeded {
            resource: "locked module graph".to_string(),
            limit: crate::MAX_MODULE_DEPENDENCIES,
        });
    }
    let mut selected = BTreeMap::<ModulePath, LockedModule>::new();
    for locked in locked_modules {
        match selected.get(&locked.path) {
            Some(existing) if existing.version != locked.version => {
                return Err(Error::SelectedVersionConflict {
                    module: locked.path.to_string(),
                    existing: existing.version.to_string(),
                    requested: locked.version.to_string(),
                });
            }
            Some(existing) if existing != &locked => {
                return Err(Error::LockFileParse(format!(
                    "conflicting duplicate lock metadata for {} {}",
                    locked.path, locked.version
                )));
            }
            Some(_) => continue,
            None => {
                selected.insert(locked.path.clone(), locked);
            }
        }
    }
    let normalized = selected.into_values().collect::<Vec<_>>();
    crate::schema::lockfile::validate_materialized_module_limits(&normalized)?;
    Ok(normalized)
}

#[cfg(test)]
async fn ensure_locked_module_ready<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    locked: &LockedModule,
    target: &str,
    prefetched_manifest: Option<ManifestSnapshot>,
) -> Result<ReadyModule> {
    let mut budget = crate::registry::MaterializedGraphBudget::default();
    ensure_locked_module_ready_with_budget(
        surface,
        registry,
        locked,
        target,
        prefetched_manifest,
        &mut budget,
    )
    .await
}

async fn ensure_locked_module_ready_with_budget<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    locked: &LockedModule,
    target: &str,
    prefetched_manifest: Option<ManifestSnapshot>,
    budget: &mut crate::registry::MaterializedGraphBudget,
) -> Result<ReadyModule> {
    let module_dir = relative_module_dir(&locked.path, &locked.version);
    let (installed, module_kind) = inspect_surface_module(surface, &module_dir, locked)?;
    let metadata = match installed {
        Ok(metadata) => {
            budget.charge_release(
                metadata.release_manifest_bytes,
                metadata.release.artifacts.len(),
            )?;
            metadata
        }
        Err(error) => {
            if module_kind != vo_common::vfs::FileSystemEntryKind::Missing {
                return Err(Error::SourceScan(format!(
                    "module cache destination {} contains invalid {module_kind:?} data ({error}); clean the module cache before installing",
                    module_dir.display(),
                )));
            }
            let manifest = match prefetched_manifest {
                Some(manifest) => manifest,
                None => fetch_locked_manifest(registry, locked).await?,
            };
            budget.charge_release(manifest.raw.len(), manifest.manifest.artifacts.len())?;
            install_source(surface, registry, locked, &manifest).await?;
            let (installed, _) = inspect_surface_module(surface, &module_dir, locked)?;
            installed.map_err(installed_module_error_to_error)?
        }
    };

    for required_artifact in required_artifacts_for_target(
        locked,
        &metadata.release.artifacts,
        metadata.extension.as_ref(),
        target,
    )? {
        let artifact_path = module_dir.join(&required_artifact.cache_relative_path);
        let (installed, artifact_kind) = inspect_surface_artifact(
            surface,
            &module_dir,
            locked,
            &required_artifact.artifact.id,
            &artifact_path,
        )?;
        if installed.is_ok() {
            continue;
        }
        if artifact_kind != vo_common::vfs::FileSystemEntryKind::Missing {
            let error = installed.expect_err("invalid artifact inspection must carry an error");
            return Err(Error::SourceScan(format!(
                "artifact cache destination {} contains invalid {artifact_kind:?} data ({error}); clean the module cache before installing",
                artifact_path.display(),
            )));
        }
        let bytes = registry
            .fetch_artifact(
                &locked.path,
                &locked.version,
                &required_artifact.artifact.id,
            )
            .await?;
        verify_size_and_digest(
            &bytes,
            required_artifact.artifact.size,
            &required_artifact.artifact.digest,
            format!(
                "artifact {} for {} {}",
                required_artifact.artifact.id.name, locked.path, locked.version,
            ),
        )?;
        commit_prepared_artifact(
            surface,
            &module_dir,
            locked,
            required_artifact.artifact,
            &artifact_path,
            &bytes,
        )?;
    }

    match check_module_readiness(surface, locked, target) {
        ModuleReadiness::Ready(ready) => Ok(*ready),
        ModuleReadiness::NotReady(failure) => Err(readiness_failure_to_error(failure)),
    }
}

fn commit_prepared_artifact<S: InstallSurface>(
    surface: &S,
    module_dir: &Path,
    locked: &LockedModule,
    artifact: &ManifestArtifact,
    artifact_path: &Path,
    bytes: &[u8],
) -> Result<()> {
    let mutation_lock = acquire_surface_mutation_lock(surface)?;
    let _identity_lock = acquire_surface_identity_lock(
        mutation_lock.as_ref(),
        &format!(
            "artifact:{}@{}:{}",
            locked.path, locked.version, artifact.id
        ),
    )?;
    validate_surface_module(surface, mutation_lock.as_ref(), module_dir, locked)?;
    if validate_surface_artifact(
        surface,
        mutation_lock.as_ref(),
        module_dir,
        locked,
        &artifact.id,
    )
    .is_ok()
    {
        return Ok(());
    }
    let destination_kind = surface_entry_kind(surface, mutation_lock.as_ref(), artifact_path)?;
    if destination_kind != vo_common::vfs::FileSystemEntryKind::Missing {
        return Err(Error::SourceScan(format!(
            "artifact cache destination {} already contains invalid {destination_kind:?} data; clean the module cache before installing",
            artifact_path.display(),
        )));
    }
    let mut transaction = SurfaceTransaction::begin(
        surface,
        mutation_lock.as_ref(),
        "artifact",
        &format!("{}@{}:{}", locked.path, locked.version, artifact.id),
    )?;
    transaction.write(Path::new("payload"), bytes)?;
    transaction.verify(Path::new("payload"), bytes)?;
    if validate_surface_artifact(
        surface,
        mutation_lock.as_ref(),
        module_dir,
        locked,
        &artifact.id,
    )
    .is_ok()
    {
        transaction.cleanup()?;
        return Ok(());
    }
    let parent = artifact_path.parent().ok_or_else(|| {
        Error::SourceScan(format!(
            "artifact cache path has no parent: {}",
            artifact_path.display(),
        ))
    })?;
    ensure_surface_directory(surface, mutation_lock.as_ref(), parent)?;
    let destination_kind = surface_entry_kind(surface, mutation_lock.as_ref(), artifact_path)?;
    if destination_kind != vo_common::vfs::FileSystemEntryKind::Missing {
        transaction.cleanup()?;
        return Err(Error::SourceScan(format!(
            "artifact cache destination {} already contains invalid {destination_kind:?} data; clean the module cache before installing",
            artifact_path.display(),
        )));
    }
    if let Err(error) = transaction.publish_file(Path::new("payload"), artifact_path) {
        if is_publication_collision(&error)
            && validate_surface_artifact(
                surface,
                mutation_lock.as_ref(),
                module_dir,
                locked,
                &artifact.id,
            )
            .is_ok()
        {
            let _ = transaction.cleanup();
            return Ok(());
        }
        return Err(error);
    }
    validate_surface_artifact(
        surface,
        mutation_lock.as_ref(),
        module_dir,
        locked,
        &artifact.id,
    )?;
    if surface_entry_kind(surface, mutation_lock.as_ref(), artifact_path)?
        != vo_common::vfs::FileSystemEntryKind::RegularFile
    {
        return Err(Error::SourceScan(format!(
            "artifact cache destination {} changed after publication",
            artifact_path.display(),
        )));
    }
    transaction.cleanup()?;
    Ok(())
}

async fn fetch_locked_manifest<R: AsyncRegistry>(
    registry: &R,
    locked: &LockedModule,
) -> Result<ManifestSnapshot> {
    let raw = registry
        .fetch_manifest_raw(&locked.path, &locked.version)
        .await?;
    let snapshot = ManifestSnapshot::try_new(&locked.path, &locked.version, raw)?;
    let digest = Digest::from_sha256(&snapshot.raw);
    validate_locked_module_against_manifest(locked, &snapshot.manifest, &digest)?;
    Ok(snapshot)
}

async fn install_source<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    locked: &LockedModule,
    manifest: &ManifestSnapshot,
) -> Result<()> {
    let source = registry
        .fetch_source(
            &locked.path,
            &locked.version,
            &manifest.manifest.source.name,
        )
        .await?;
    let prepared = match source {
        SourcePayload::Package(source_package) => {
            verify_size_and_digest(
                &source_package,
                manifest.manifest.source.size,
                &manifest.manifest.source.digest,
                format!("source package for {} {}", locked.path, locked.version),
            )?;
            let files = crate::cache::install::extract_source_entries(&source_package).map_err(
                |error| {
                    Error::SourceScan(format!("{} for {} {}", error, locked.path, locked.version,))
                },
            )?;
            verify_package_payload(&files.tree_bytes, &files.files, &manifest.manifest, locked)?;
            PreparedSourceTree {
                files: files.files,
                tree_raw: files.tree_bytes,
            }
        }
        SourcePayload::Files {
            source_files,
            tree_raw,
        } => {
            verify_package_payload(&tree_raw, &source_files, &manifest.manifest, locked)?;
            PreparedSourceTree {
                files: source_files,
                tree_raw,
            }
        }
    };
    preflight_source_install(&prepared, locked, manifest)?;
    commit_prepared_source(surface, prepared, locked, manifest)
}

fn preflight_source_install(
    prepared: &PreparedSourceTree,
    locked: &LockedModule,
    manifest: &ManifestSnapshot,
) -> Result<()> {
    let module_dir = relative_module_dir(&locked.path, &locked.version);
    let mut fs = BorrowedPreflightFs::new();
    for file in &prepared.files {
        fs.insert(
            &module_dir.join(&file.path),
            PreflightFile::Borrowed(&file.bytes),
        )?;
    }
    fs.insert(
        &module_dir.join("vo.tree.json"),
        PreflightFile::Borrowed(&prepared.tree_raw),
    )?;
    fs.insert(
        &module_dir.join(VERSION_MARKER),
        PreflightFile::Owned(format!("{}\n", locked.version).into_bytes()),
    )?;
    fs.insert(
        &module_dir.join(SOURCE_DIGEST_MARKER),
        PreflightFile::Owned(format!("{}\n", manifest.manifest.source.digest).into_bytes()),
    )?;
    fs.insert(
        &module_dir.join("vo.release.json"),
        PreflightFile::Borrowed(&manifest.raw),
    )?;
    validate_installed_module(&fs, &module_dir, locked).map_err(installed_module_error_to_error)
}

enum PreflightFile<'a> {
    Borrowed(&'a [u8]),
    Owned(Vec<u8>),
}

impl PreflightFile<'_> {
    fn bytes(&self) -> &[u8] {
        match self {
            Self::Borrowed(bytes) => bytes,
            Self::Owned(bytes) => bytes.as_slice(),
        }
    }
}

#[derive(Default)]
struct PreflightNode<'a> {
    children: BTreeMap<String, usize>,
    file: Option<PreflightFile<'a>>,
}

/// Read-only trie over prepared source buffers. Validation allocates at most
/// the one file currently requested by `FileSystem`; the full source tree
/// remains borrowed from `PreparedSourceTree` without a second content copy.
struct BorrowedPreflightFs<'a> {
    nodes: Vec<PreflightNode<'a>>,
}

impl<'a> BorrowedPreflightFs<'a> {
    fn new() -> Self {
        Self {
            nodes: vec![PreflightNode::default()],
        }
    }

    fn insert(&mut self, path: &Path, file: PreflightFile<'a>) -> Result<()> {
        let mut node_index = 0usize;
        for component in path.components() {
            let std::path::Component::Normal(component) = component else {
                return Err(Error::SourceScan(format!(
                    "preflight path must be canonical and relative: {}",
                    path.display(),
                )));
            };
            let component = component.to_str().ok_or_else(|| {
                Error::SourceScan(format!(
                    "preflight path must be valid UTF-8: {}",
                    path.display(),
                ))
            })?;
            if self.nodes[node_index].file.is_some() {
                return Err(Error::SourceScan(format!(
                    "preflight path descends through a file: {}",
                    path.display(),
                )));
            }
            node_index = match self.nodes[node_index].children.get(component).copied() {
                Some(index) => index,
                None => {
                    let index = self.nodes.len();
                    self.nodes.push(PreflightNode::default());
                    self.nodes[node_index]
                        .children
                        .insert(component.to_string(), index);
                    index
                }
            };
        }
        let node = &mut self.nodes[node_index];
        if node.file.is_some() || !node.children.is_empty() {
            return Err(Error::SourceScan(format!(
                "preflight source contains a duplicate or file/directory conflict: {}",
                path.display(),
            )));
        }
        node.file = Some(file);
        Ok(())
    }

    fn node(&self, path: &Path) -> std::io::Result<Option<&PreflightNode<'a>>> {
        let mut node = &self.nodes[0];
        for component in path.components() {
            match component {
                std::path::Component::CurDir => continue,
                std::path::Component::Normal(component) => {
                    let component = component.to_str().ok_or_else(|| {
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidInput,
                            "preflight path is not valid UTF-8",
                        )
                    })?;
                    let Some(index) = node.children.get(component) else {
                        return Ok(None);
                    };
                    node = &self.nodes[*index];
                }
                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        format!("preflight path must be relative: {}", path.display()),
                    ));
                }
            }
        }
        Ok(Some(node))
    }

    fn file_bytes(&self, path: &Path) -> std::io::Result<&[u8]> {
        self.node(path)?
            .and_then(|node| node.file.as_ref())
            .map(PreflightFile::bytes)
            .ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("preflight file is missing: {}", path.display()),
                )
            })
    }
}

impl vo_common::vfs::FileSystem for BorrowedPreflightFs<'_> {
    fn read_file(&self, path: &Path) -> std::io::Result<String> {
        self.read_text_limited(path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
    }

    fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        self.file_bytes(path).map(<[u8]>::to_vec)
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> std::io::Result<Vec<u8>> {
        let bytes = self.file_bytes(path)?;
        if bytes.len() > max_bytes {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "preflight file {} exceeds the {max_bytes}-byte limit",
                    path.display(),
                ),
            ));
        }
        Ok(bytes.to_vec())
    }

    fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
        let node = self.node(path)?.ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("preflight directory is missing: {}", path.display()),
            )
        })?;
        if node.file.is_some() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotADirectory,
                format!("preflight path is a file: {}", path.display()),
            ));
        }
        if node.children.len() > vo_common::vfs::MAX_DIRECTORY_ENTRIES {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "preflight directory contains too many entries",
            ));
        }
        let mut children = node
            .children
            .keys()
            .map(|name| path.join(name))
            .collect::<Vec<_>>();
        vo_common::vfs::sort_fs_paths(&mut children);
        Ok(children)
    }

    fn exists(&self, path: &Path) -> bool {
        self.node(path).ok().flatten().is_some()
    }

    fn is_dir(&self, path: &Path) -> bool {
        self.node(path)
            .ok()
            .flatten()
            .is_some_and(|node| node.file.is_none())
    }

    fn entry_kind(&self, path: &Path) -> std::io::Result<vo_common::vfs::FileSystemEntryKind> {
        Ok(match self.node(path)? {
            Some(node) if node.file.is_some() => vo_common::vfs::FileSystemEntryKind::RegularFile,
            Some(_) => vo_common::vfs::FileSystemEntryKind::Directory,
            None => vo_common::vfs::FileSystemEntryKind::Missing,
        })
    }
}

fn commit_prepared_source<S: InstallSurface>(
    surface: &S,
    prepared: PreparedSourceTree,
    locked: &LockedModule,
    manifest: &ManifestSnapshot,
) -> Result<()> {
    let mutation_lock = acquire_surface_mutation_lock(surface)?;
    let _identity_lock = acquire_surface_identity_lock(
        mutation_lock.as_ref(),
        &format!("source:{}@{}", locked.path, locked.version),
    )?;
    let module_dir = relative_module_dir(&locked.path, &locked.version);
    let module_parent = module_dir.parent().ok_or_else(|| {
        Error::SourceScan(format!(
            "module cache directory has no parent: {}",
            module_dir.display(),
        ))
    })?;
    if validate_surface_module(surface, mutation_lock.as_ref(), &module_dir, locked).is_ok() {
        return Ok(());
    }
    let existing_kind = surface_entry_kind(surface, mutation_lock.as_ref(), &module_dir)?;
    if existing_kind != vo_common::vfs::FileSystemEntryKind::Missing {
        return Err(Error::SourceScan(format!(
            "module cache destination {} already contains an invalid {existing_kind:?} entry; clean the module cache before installing",
            module_dir.display(),
        )));
    }
    let mut transaction = SurfaceTransaction::begin(
        surface,
        mutation_lock.as_ref(),
        "source",
        &format!("{}@{}", locked.path, locked.version),
    )?;

    let PreparedSourceTree { files, tree_raw } = prepared;
    let stage_result = (|| {
        // Consume one prepared buffer immediately after its staged write is
        // verified. Memory-backed surfaces therefore replace prepared bytes
        // with staged bytes progressively instead of retaining two complete
        // source trees at once.
        for file in files {
            transaction.write_source(&file.path, &file.bytes, file.mode)?;
            transaction.verify(&file.path, &file.bytes)?;
        }
        transaction.write(Path::new("vo.tree.json"), &tree_raw)?;
        transaction.verify(Path::new("vo.tree.json"), &tree_raw)?;
        transaction.write(
            Path::new(VERSION_MARKER),
            format!("{}\n", locked.version).as_bytes(),
        )?;
        transaction.write(
            Path::new(SOURCE_DIGEST_MARKER),
            format!("{}\n", manifest.manifest.source.digest).as_bytes(),
        )?;
        transaction.write(Path::new("vo.release.json"), &manifest.raw)?;
        transaction.verify(
            Path::new(VERSION_MARKER),
            format!("{}\n", locked.version).as_bytes(),
        )?;
        transaction.verify(
            Path::new(SOURCE_DIGEST_MARKER),
            format!("{}\n", manifest.manifest.source.digest).as_bytes(),
        )?;
        transaction.verify(Path::new("vo.release.json"), &manifest.raw)
    })();
    stage_result?;

    // A concurrent installer may have completed while this tree was staged.
    if validate_surface_module(surface, mutation_lock.as_ref(), &module_dir, locked).is_ok() {
        transaction.cleanup()?;
        return Ok(());
    }
    ensure_surface_directory(surface, mutation_lock.as_ref(), module_parent)?;

    let existing_kind = surface_entry_kind(surface, mutation_lock.as_ref(), &module_dir)?;
    if existing_kind != vo_common::vfs::FileSystemEntryKind::Missing {
        transaction.cleanup()?;
        return Err(Error::SourceScan(format!(
            "module cache destination {} already contains an invalid {existing_kind:?} entry; clean the module cache before installing",
            module_dir.display(),
        )));
    }
    if let Err(error) = transaction.publish_tree(&module_dir) {
        if is_publication_collision(&error)
            && validate_surface_module(surface, mutation_lock.as_ref(), &module_dir, locked).is_ok()
        {
            let _ = transaction.cleanup();
            return Ok(());
        }
        return Err(error);
    }

    validate_surface_module(surface, mutation_lock.as_ref(), &module_dir, locked)?;
    if surface_entry_kind(surface, mutation_lock.as_ref(), &module_dir)?
        != vo_common::vfs::FileSystemEntryKind::Directory
    {
        return Err(Error::SourceScan(format!(
            "module cache destination {} changed after publication",
            module_dir.display(),
        )));
    }
    Ok(())
}

fn acquire_surface_mutation_lock<S: InstallSurface>(
    surface: &S,
) -> Result<Option<crate::cache::mutation_lock::CacheMutationLock>> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        surface
            .root()
            .map(crate::cache::mutation_lock::CacheMutationLock::shared)
            .transpose()
    }
    #[cfg(target_arch = "wasm32")]
    {
        let _ = surface;
        Ok(None)
    }
}

fn inspect_surface_module<S: InstallSurface>(
    surface: &S,
    module_dir: &Path,
    locked: &LockedModule,
) -> Result<(
    std::result::Result<InstalledModuleMetadata, InstalledModuleError>,
    vo_common::vfs::FileSystemEntryKind,
)> {
    let mutation_lock = acquire_surface_mutation_lock(surface)?;
    let validation = if surface.supports_anchored_transactions() {
        let mutation_lock = mutation_lock.as_ref().ok_or_else(|| {
            Error::Io(std::io::Error::other(
                "anchored install surface is missing its cache mutation lock",
            ))
        })?;
        validate_installed_module_with_metadata(&mutation_lock.file_system(), module_dir, locked)
    } else {
        validate_installed_module_with_metadata(surface, module_dir, locked)
    };
    let kind = surface_entry_kind(surface, mutation_lock.as_ref(), module_dir)?;
    Ok((validation, kind))
}

fn inspect_surface_artifact<S: InstallSurface>(
    surface: &S,
    module_dir: &Path,
    locked: &LockedModule,
    artifact: &ArtifactId,
    artifact_path: &Path,
) -> Result<(
    std::result::Result<(), InstalledModuleError>,
    vo_common::vfs::FileSystemEntryKind,
)> {
    let mutation_lock = acquire_surface_mutation_lock(surface)?;
    let validation = if surface.supports_anchored_transactions() {
        let mutation_lock = mutation_lock.as_ref().ok_or_else(|| {
            Error::Io(std::io::Error::other(
                "anchored install surface is missing its cache mutation lock",
            ))
        })?;
        validate_installed_artifact(&mutation_lock.file_system(), module_dir, locked, artifact)
    } else {
        validate_installed_artifact(surface, module_dir, locked, artifact)
    };
    let kind = surface_entry_kind(surface, mutation_lock.as_ref(), artifact_path)?;
    Ok((validation, kind))
}

fn is_publication_collision(error: &Error) -> bool {
    matches!(error, Error::Io(error) if error.kind() == std::io::ErrorKind::AlreadyExists)
}

fn acquire_surface_identity_lock(
    mutation_lock: Option<&crate::cache::mutation_lock::CacheMutationLock>,
    identity: &str,
) -> Result<Option<crate::cache::mutation_lock::CacheMutationLock>> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        mutation_lock
            .map(|mutation_lock| mutation_lock.identity_lock(identity))
            .transpose()
    }
    #[cfg(target_arch = "wasm32")]
    {
        let _ = (mutation_lock, identity);
        Ok(None)
    }
}

fn surface_entry_kind<S: InstallSurface>(
    surface: &S,
    mutation_lock: Option<&crate::cache::mutation_lock::CacheMutationLock>,
    path: &Path,
) -> Result<vo_common::vfs::FileSystemEntryKind> {
    if surface.supports_anchored_transactions() {
        let mutation_lock = mutation_lock.ok_or_else(|| {
            Error::Io(std::io::Error::other(
                "anchored install surface is missing its cache mutation lock",
            ))
        })?;
        mutation_lock.entry_kind(path)
    } else {
        surface.entry_kind(path).map_err(Error::Io)
    }
}

fn validate_surface_module<S: InstallSurface>(
    surface: &S,
    mutation_lock: Option<&crate::cache::mutation_lock::CacheMutationLock>,
    module_dir: &Path,
    locked: &LockedModule,
) -> Result<()> {
    if surface.supports_anchored_transactions() {
        let mutation_lock = mutation_lock.ok_or_else(|| {
            Error::Io(std::io::Error::other(
                "anchored install surface is missing its cache mutation lock",
            ))
        })?;
        return validate_installed_module(&mutation_lock.file_system(), module_dir, locked)
            .map_err(installed_module_error_to_error);
    }
    validate_installed_module(surface, module_dir, locked).map_err(installed_module_error_to_error)
}

fn validate_surface_artifact<S: InstallSurface>(
    surface: &S,
    mutation_lock: Option<&crate::cache::mutation_lock::CacheMutationLock>,
    module_dir: &Path,
    locked: &LockedModule,
    artifact: &ArtifactId,
) -> Result<()> {
    if surface.supports_anchored_transactions() {
        let mutation_lock = mutation_lock.ok_or_else(|| {
            Error::Io(std::io::Error::other(
                "anchored install surface is missing its cache mutation lock",
            ))
        })?;
        return validate_installed_artifact(
            &mutation_lock.file_system(),
            module_dir,
            locked,
            artifact,
        )
        .map_err(installed_module_error_to_error);
    }
    validate_installed_artifact(surface, module_dir, locked, artifact)
        .map_err(installed_module_error_to_error)
}

fn ensure_surface_directory<S: InstallSurface>(
    surface: &S,
    mutation_lock: Option<&crate::cache::mutation_lock::CacheMutationLock>,
    path: &Path,
) -> Result<()> {
    if surface.supports_anchored_transactions() {
        let mutation_lock = mutation_lock.ok_or_else(|| {
            Error::Io(std::io::Error::other(
                "anchored install surface is missing its cache mutation lock",
            ))
        })?;
        mutation_lock.ensure_directory(path)
    } else {
        surface.mkdir_all(path)
    }
}

struct SurfaceTransaction<'a, S: InstallSurface> {
    surface: &'a S,
    path: PathBuf,
    native: Option<crate::cache::mutation_lock::CacheTransaction>,
    active: bool,
}

impl<'a, S: InstallSurface> SurfaceTransaction<'a, S> {
    fn begin(
        surface: &'a S,
        mutation_lock: Option<&crate::cache::mutation_lock::CacheMutationLock>,
        role: &str,
        identity: &str,
    ) -> Result<Self> {
        if surface.supports_anchored_transactions() {
            let mutation_lock = mutation_lock.ok_or_else(|| {
                Error::Io(std::io::Error::other(
                    "anchored install surface is missing its cache mutation lock",
                ))
            })?;
            let native = mutation_lock.begin_transaction(&format!("{role}:{identity}"))?;
            let path = native.relative_path().to_path_buf();
            return Ok(Self {
                surface,
                path,
                native: Some(native),
                active: true,
            });
        }

        let path = allocate_surface_transaction_directory(surface, role, identity)?;
        Ok(Self {
            surface,
            path,
            native: None,
            active: true,
        })
    }

    fn write(&self, relative: &Path, bytes: &[u8]) -> Result<()> {
        if let Some(native) = &self.native {
            return native.write_file(relative, bytes);
        }
        write_bytes(self.surface, &self.path.join(relative), bytes)
    }

    fn write_source(
        &self,
        relative: &Path,
        bytes: &[u8],
        mode: crate::schema::SourceFileMode,
    ) -> Result<()> {
        if let Some(native) = &self.native {
            return native.write_source_file(relative, bytes, mode);
        }
        let path = self.path.join(relative);
        write_bytes(self.surface, &path, bytes)?;
        self.surface.set_source_mode(&path, mode)
    }

    fn verify(&self, relative: &Path, expected: &[u8]) -> Result<()> {
        let (kind, found) = if let Some(native) = &self.native {
            (
                native.entry_kind(relative)?,
                native.read_file(relative, expected.len())?,
            )
        } else {
            let path = self.path.join(relative);
            (
                self.surface.entry_kind(&path).map_err(Error::Io)?,
                self.surface
                    .read_bytes_limited(&path, expected.len())
                    .map_err(Error::Io)?,
            )
        };
        if kind != vo_common::vfs::FileSystemEntryKind::RegularFile {
            return Err(Error::SourceScan(format!(
                "staged source path {} must be a regular file, found {kind:?}",
                self.path.join(relative).display(),
            )));
        }
        if found != expected {
            return Err(Error::DigestMismatch {
                context: format!("staged source file {}", self.path.join(relative).display()),
                expected: Digest::from_sha256(expected).to_string(),
                found: Digest::from_sha256(&found).to_string(),
            });
        }
        Ok(())
    }

    fn publish_tree(&mut self, destination: &Path) -> Result<()> {
        if let Some(native) = &mut self.native {
            native.publish_tree(destination)?;
        } else {
            self.surface.publish_noreplace(&self.path, destination)?;
        }
        self.active = false;
        Ok(())
    }

    fn publish_file(&mut self, source: &Path, destination: &Path) -> Result<()> {
        if let Some(native) = &mut self.native {
            native.publish_file(source, destination)
        } else {
            self.surface
                .publish_noreplace(&self.path.join(source), destination)
        }
    }

    fn cleanup(&mut self) -> Result<()> {
        if !self.active {
            return Ok(());
        }
        if let Some(native) = &mut self.native {
            native.cleanup()?;
        } else {
            self.surface.remove_tree(&self.path)?;
        }
        self.active = false;
        Ok(())
    }
}

impl<S: InstallSurface> Drop for SurfaceTransaction<'_, S> {
    fn drop(&mut self) {
        let _ = self.cleanup();
    }
}

fn allocate_surface_transaction_directory<S: InstallSurface>(
    surface: &S,
    role: &str,
    identity: &str,
) -> Result<PathBuf> {
    let staging_root = Path::new(crate::cache::layout::STAGING_DIR);
    surface.mkdir_all(staging_root)?;
    let identity_digest = Digest::from_sha256(identity.as_bytes());
    let digest_start = "sha256:".len();
    let digest = &identity_digest.as_str()[digest_start..digest_start + 24];
    for _ in 0..64 {
        let nonce = INSTALL_TRANSACTION_COUNTER.fetch_add(1, Ordering::Relaxed);
        let path = staging_root.join(format!("{role}-{digest}-{nonce:016x}"));
        match surface.create_dir(&path) {
            Ok(()) => return Ok(path),
            Err(Error::Io(error)) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error),
        }
    }
    Err(Error::SourceScan(format!(
        "failed to allocate unique {role} staging directory"
    )))
}

fn verify_package_payload(
    tree_raw: &[u8],
    files: &[ExtractedSourceFile],
    release: &ReleaseManifest,
    locked: &LockedModule,
) -> Result<()> {
    crate::digest::verify_digest(
        tree_raw,
        &release.source.tree,
        format!("vo.tree.json for {} {}", locked.path, locked.version),
    )?;
    let package = TreeManifest::parse(tree_raw).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid vo.tree.json for {} {}: {error}",
            locked.path, locked.version,
        ))
    })?;
    let found = source_file_entries(files)?;
    if found != package.files {
        let detail =
            crate::schema::source_files::package_file_set_mismatch_detail(&package.files, &found);
        return Err(Error::InvalidReleaseMetadata(format!(
            "source payload for {} {} does not match vo.tree.json: {detail}",
            locked.path, locked.version,
        )));
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct SourceFileEntryLimits {
    max_files: usize,
    max_entry_bytes: usize,
    max_total_bytes: usize,
}

fn source_file_entries(
    files: &[ExtractedSourceFile],
) -> Result<Vec<crate::schema::SourceFileEntry>> {
    source_file_entries_with_limits(
        files,
        SourceFileEntryLimits {
            max_files: crate::MAX_SOURCE_ARCHIVE_ENTRIES.saturating_sub(1),
            max_entry_bytes: crate::MAX_SOURCE_ARCHIVE_ENTRY_BYTES,
            max_total_bytes: crate::MAX_EXTRACTED_SOURCE_BYTES,
        },
    )
}

fn source_file_entries_with_limits(
    files: &[ExtractedSourceFile],
    limits: SourceFileEntryLimits,
) -> Result<Vec<crate::schema::SourceFileEntry>> {
    if files.len() > limits.max_files {
        return Err(Error::SourceScan(format!(
            "source payload contains more than {} files",
            limits.max_files,
        )));
    }
    let mut entries = Vec::new();
    entries
        .try_reserve(files.len())
        .map_err(|_| Error::SourceScan("failed to reserve source file entries".to_string()))?;
    let mut path_keys = crate::schema::PortablePathSet::default();
    let mut total_size = 0usize;
    for file in files {
        let path = &file.path;
        let content = &file.bytes;
        let portable_path =
            crate::schema::portable_relative_path_from_path(path).map_err(|error| {
                Error::SourceScan(format!(
                    "source payload path must be a normalized portable relative path: {}: {error}",
                    path.display(),
                ))
            })?;
        if !crate::schema::is_package_file_candidate(&portable_path).map_err(Error::SourceScan)? {
            return Err(Error::SourceScan(format!(
                "source payload path is reserved by the module protocol: {}",
                path.display(),
            )));
        }
        let inserted = path_keys
            .insert_file(&portable_path)
            .map_err(Error::SourceScan)?;
        if !inserted {
            return Err(Error::SourceScan(format!(
                "source payload contains duplicate path {portable_path:?}",
            )));
        }
        if content.len() > limits.max_entry_bytes {
            return Err(Error::SourceScan(format!(
                "source payload file {} is {} bytes, exceeding the {}-byte archive-entry limit",
                path.display(),
                content.len(),
                limits.max_entry_bytes,
            )));
        }
        total_size = total_size.checked_add(content.len()).ok_or_else(|| {
            Error::SourceScan("source payload byte size overflows usize".to_string())
        })?;
        if total_size > limits.max_total_bytes {
            return Err(Error::SourceScan(format!(
                "source payload exceeds the {}-byte extracted-source limit",
                limits.max_total_bytes,
            )));
        }
        let content_size = u64::try_from(content.len())
            .map_err(|_| Error::SourceScan("source file size exceeds u64".to_string()))?;
        entries.push(crate::schema::SourceFileEntry {
            path: portable_path,
            mode: file.mode,
            size: content_size,
            digest: Digest::from_sha256(content),
        });
    }
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    crate::schema::validate_package_file_set(&entries).map_err(Error::SourceScan)?;
    Ok(entries)
}

#[cfg(test)]
fn validate_source_files(files: &[ExtractedSourceFile]) -> Result<()> {
    source_file_entries(files).map(drop)
}

fn write_bytes<S: InstallSurface>(surface: &S, path: &Path, bytes: &[u8]) -> Result<()> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            surface.mkdir_all(parent)?;
        }
    }
    surface.write_bytes(path, bytes)
}

fn installed_module_error_to_error(error: InstalledModuleError) -> Error {
    Error::SourceScan(error.to_string())
}

fn readiness_failure_to_error(failure: ReadinessFailure) -> Error {
    match failure {
        ReadinessFailure::SourceNotReady { error } => installed_module_error_to_error(*error),
        ReadinessFailure::ArtifactNotReady {
            module,
            version,
            error,
            ..
        } => Error::MissingArtifact {
            module,
            version,
            detail: error.to_string(),
        },
        ReadinessFailure::UnsupportedNativeTarget {
            module,
            version,
            target,
            ..
        } => Error::SourceScan(format!(
            "vo.mod does not declare extension-native support for target {} in {}@{}",
            target, module, version,
        )),
        ReadinessFailure::ArtifactResolutionFailed { error, .. } => *error,
        ReadinessFailure::LockedGraphInvalid { error } => *error,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::schema::manifest::{ManifestArtifact, ManifestSource, ReleaseManifest};
    use crate::version::{DepConstraint, ToolchainConstraint};
    use std::sync::Mutex;
    use std::task::{Context, Poll, Waker};

    fn regular_file(path: impl Into<PathBuf>, bytes: impl Into<Vec<u8>>) -> ExtractedSourceFile {
        ExtractedSourceFile {
            path: path.into(),
            mode: crate::schema::SourceFileMode::Regular,
            bytes: bytes.into(),
        }
    }

    #[test]
    fn source_file_preflight_rejects_aliases_and_cache_metadata_paths() {
        let aliases = vec![
            regular_file("Source/main.vo", b"package source".to_vec()),
            regular_file("source/other.vo", b"package source".to_vec()),
        ];
        assert!(validate_source_files(&aliases).is_err());

        let reserved = vec![regular_file(
            "ARTIFACTS/injected.wasm",
            b"not an artifact".to_vec(),
        )];
        assert!(validate_source_files(&reserved).is_err());
    }

    fn browser_source_files() -> Vec<ExtractedSourceFile> {
        vec![
            regular_file(
                "vo.mod",
                b"format = 1\nmodule = \"github.com/acme/pkg\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n".to_vec(),
            ),
            regular_file("src/main.vo", b"package main\n".to_vec()),
            regular_file("empty.vo", Vec::new()),
        ]
    }

    #[test]
    fn source_file_entries_match_package_protocol_and_ignore_payload_order() {
        let files = browser_source_files();
        let forward = source_file_entries(&files).unwrap();
        let mut reversed_files = files;
        reversed_files.reverse();
        let reversed = source_file_entries(&reversed_files).unwrap();

        assert_eq!(forward, reversed);
        assert_eq!(forward.len(), 3);
        assert_eq!(forward[0].path, "empty.vo");
        assert_eq!(forward[1].path, "src/main.vo");
        assert_eq!(forward[2].path, "vo.mod");
    }

    #[test]
    fn files_payload_uses_archive_protocol_file_limit() {
        let mut files = Vec::with_capacity(
            vo_common::vfs::MAX_PACKAGE_SOURCE_FILES
                .checked_add(1)
                .unwrap(),
        );
        files.push(regular_file("vo.mod", b"module = {}\n".to_vec()));
        for index in 0..vo_common::vfs::MAX_PACKAGE_SOURCE_FILES {
            files.push(regular_file(format!("assets/{index:05}.bin"), Vec::new()));
        }

        let entries = source_file_entries(&files).unwrap();

        assert_eq!(entries.len(), vo_common::vfs::MAX_PACKAGE_SOURCE_FILES + 1,);
    }

    #[test]
    fn files_payload_checks_count_and_bytes_before_hashing() {
        let files = vec![
            regular_file("vo.mod", b"mod".to_vec()),
            regular_file("asset.bin", b"data".to_vec()),
        ];
        let limits = |max_files, max_entry_bytes, max_total_bytes| SourceFileEntryLimits {
            max_files,
            max_entry_bytes,
            max_total_bytes,
        };

        let count_error = source_file_entries_with_limits(&files, limits(1, 10, 10)).unwrap_err();
        assert!(count_error.to_string().contains("more than 1 files"));

        let entry_error = source_file_entries_with_limits(&files, limits(2, 3, 10)).unwrap_err();
        assert!(entry_error.to_string().contains("archive-entry limit"));

        let total_error = source_file_entries_with_limits(&files, limits(2, 10, 6)).unwrap_err();
        assert!(total_error.to_string().contains("extracted-source limit"));
    }

    #[test]
    fn files_payload_uses_the_shared_portable_path_budget() {
        let maximum_component = "a".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES);
        validate_source_files(&[regular_file(&maximum_component, Vec::new())]).unwrap();

        let oversized_component = "a".repeat(
            crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES
                .checked_add(1)
                .unwrap(),
        );
        let error =
            validate_source_files(&[regular_file(oversized_component, Vec::new())]).unwrap_err();
        assert!(error.to_string().contains("portable"), "{error}");
    }

    #[test]
    fn files_payload_enforces_aggregate_complete_path_key_bytes() {
        let deep_path = |branch: usize| {
            let mut components = vec![format!("root{branch:011}")];
            components.extend(std::iter::repeat_n("component000000".to_string(), 255));
            let path = components.join("/");
            assert_eq!(path.len(), crate::schema::MAX_PORTABLE_PATH_BYTES - 1);
            path
        };
        let mut files = (0..31)
            .map(|branch| regular_file(deep_path(branch), Vec::new()))
            .collect::<Vec<_>>();
        validate_source_files(&files).unwrap();

        files.push(regular_file(deep_path(31), Vec::new()));
        let error = validate_source_files(&files).unwrap_err();
        assert!(error.to_string().contains("path-key limit"), "{error}");
    }

    #[test]
    fn real_fs_publish_noreplace_preserves_both_paths_on_collision() {
        let root = tempfile::tempdir().unwrap();
        let fs = RealFs::new(root.path());
        <RealFs as InstallSurface>::write_bytes(&fs, Path::new("source"), b"new").unwrap();
        <RealFs as InstallSurface>::write_bytes(&fs, Path::new("target"), b"old").unwrap();

        let error = <RealFs as InstallSurface>::publish_noreplace(
            &fs,
            Path::new("source"),
            Path::new("target"),
        )
        .unwrap_err();
        assert!(matches!(
            error,
            Error::Io(ref source) if source.kind() == std::io::ErrorKind::AlreadyExists
        ));
        assert_eq!(fs.read_bytes(Path::new("source")).unwrap(), b"new");
        assert_eq!(fs.read_bytes(Path::new("target")).unwrap(), b"old");
    }

    #[cfg(unix)]
    #[test]
    fn native_staged_sync_rejects_excessive_directory_depth() {
        let root = tempfile::tempdir().unwrap();
        let mut directory = root.path().to_path_buf();
        for _ in 0..=MAX_NATIVE_STAGED_TREE_DEPTH {
            directory.push("d");
            std::fs::create_dir(&directory).unwrap();
        }

        let error = sync_native_staged_path(root.path()).unwrap_err();

        assert!(error.to_string().contains("depth limit"), "{error}");
    }

    #[cfg(unix)]
    #[test]
    fn real_fs_mutations_reject_symlink_ancestors_and_only_unlink_leaf_symlinks() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        let outside_tree = outside.path().join("tree");
        std::fs::create_dir(&outside_tree).unwrap();
        let outside_file = outside_tree.join("keep.vo");
        std::fs::write(&outside_file, b"keep").unwrap();

        let fs = RealFs::new(root.path());
        symlink(&outside_tree, root.path().join("ancestor-link")).unwrap();
        for result in [
            <RealFs as InstallSurface>::remove_tree(&fs, Path::new("ancestor-link/keep.vo")),
            <RealFs as InstallSurface>::mkdir_all(&fs, Path::new("ancestor-link/new-dir")),
            <RealFs as InstallSurface>::write_bytes(
                &fs,
                Path::new("ancestor-link/keep.vo"),
                b"changed",
            ),
        ] {
            assert!(result.is_err());
        }
        assert_eq!(std::fs::read(&outside_file).unwrap(), b"keep");

        let leaf_link = root.path().join("leaf-link");
        symlink(&outside_tree, &leaf_link).unwrap();
        <RealFs as InstallSurface>::remove_tree(&fs, Path::new("leaf-link")).unwrap();
        assert!(!leaf_link.exists());
        assert_eq!(std::fs::read(&outside_file).unwrap(), b"keep");
    }

    struct RecordingSurface {
        _root: tempfile::TempDir,
        fs: RealFs,
        mutations: Mutex<Vec<PathBuf>>,
        writes_before_failure: Mutex<Option<usize>>,
    }

    impl Default for RecordingSurface {
        fn default() -> Self {
            let root = tempfile::tempdir().unwrap();
            drop(crate::cache::acquire_read_lease(root.path()).unwrap());
            let fs = RealFs::new(root.path());
            Self {
                _root: root,
                fs,
                mutations: Mutex::new(Vec::new()),
                writes_before_failure: Mutex::new(None),
            }
        }
    }

    impl RecordingSurface {
        fn mutations(&self) -> Vec<PathBuf> {
            self.mutations.lock().unwrap().clone()
        }

        fn seed_text(&self, path: &Path, content: &str) {
            if let Some(parent) = path.parent() {
                <RealFs as InstallSurface>::mkdir_all(&self.fs, parent).unwrap();
            }
            <RealFs as InstallSurface>::write_text(&self.fs, path, content).unwrap();
        }

        fn fail_writes_after(&self, successful_writes: usize) {
            *self.writes_before_failure.lock().unwrap() = Some(successful_writes);
        }
    }

    impl FileSystem for RecordingSurface {
        fn read_file(&self, path: &Path) -> std::io::Result<String> {
            self.fs.read_file(path)
        }

        fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            self.fs.read_bytes(path)
        }

        fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> std::io::Result<Vec<u8>> {
            self.fs.read_bytes_limited(path, max_bytes)
        }

        fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
            self.fs.read_dir(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.fs.exists(path)
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.fs.is_dir(path)
        }

        fn entry_kind(&self, path: &Path) -> std::io::Result<vo_common::vfs::FileSystemEntryKind> {
            self.fs.entry_kind(path)
        }

        fn root(&self) -> Option<&Path> {
            Some(self.fs.root())
        }
    }

    impl InstallSurface for RecordingSurface {
        fn remove_tree(&self, path: &Path) -> Result<()> {
            self.mutations.lock().unwrap().push(path.to_path_buf());
            <RealFs as InstallSurface>::remove_tree(&self.fs, path)
        }

        fn mkdir_all(&self, path: &Path) -> Result<()> {
            self.mutations.lock().unwrap().push(path.to_path_buf());
            <RealFs as InstallSurface>::mkdir_all(&self.fs, path)
        }

        fn create_dir(&self, path: &Path) -> Result<()> {
            self.mutations.lock().unwrap().push(path.to_path_buf());
            <RealFs as InstallSurface>::create_dir(&self.fs, path)
        }

        fn write_bytes(&self, path: &Path, bytes: &[u8]) -> Result<()> {
            self.mutations.lock().unwrap().push(path.to_path_buf());
            let mut remaining = self.writes_before_failure.lock().unwrap();
            if let Some(remaining) = remaining.as_mut() {
                if *remaining == 0 {
                    return Err(Error::Io(std::io::Error::other(
                        "injected staging write failure",
                    )));
                }
                *remaining -= 1;
            }
            <RealFs as InstallSurface>::write_bytes(&self.fs, path, bytes)
        }

        fn publish_noreplace(&self, from: &Path, to: &Path) -> Result<()> {
            self.mutations.lock().unwrap().push(from.to_path_buf());
            self.mutations.lock().unwrap().push(to.to_path_buf());
            <RealFs as InstallSurface>::publish_noreplace(&self.fs, from, to)
        }
    }

    struct FilesRegistry {
        source_files: Vec<ExtractedSourceFile>,
        tree_raw: Vec<u8>,
    }

    struct PackageRegistry {
        source_package: Vec<u8>,
    }

    struct CandidateRegistry {
        candidates: Vec<ExactVersion>,
        manifests: BTreeMap<ExactVersion, Vec<u8>>,
    }

    #[derive(Default)]
    struct NoFetchRegistry {
        calls: Mutex<Vec<&'static str>>,
    }

    impl NoFetchRegistry {
        fn calls(&self) -> Vec<&'static str> {
            self.calls.lock().unwrap().clone()
        }

        fn record(&self, operation: &'static str) {
            self.calls.lock().unwrap().push(operation);
        }
    }

    impl AsyncRegistry for NoFetchRegistry {
        fn list_version_candidates<'a>(
            &'a self,
            _module: &'a ModulePath,
        ) -> BoxFuture<'a, Result<Vec<ExactVersion>>> {
            self.record("list_version_candidates");
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected version-list fetch".to_string(),
                ))
            })
        }

        fn fetch_manifest_raw<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            self.record("fetch_manifest_raw");
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected release-manifest fetch".to_string(),
                ))
            })
        }

        fn fetch_source<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _asset_name: &'a str,
        ) -> BoxFuture<'a, Result<SourcePayload>> {
            self.record("fetch_source");
            Box::pin(async { Err(Error::RegistryError("unexpected source fetch".to_string())) })
        }

        fn fetch_artifact<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _artifact: &'a ArtifactId,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            self.record("fetch_artifact");
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected artifact fetch".to_string(),
                ))
            })
        }
    }

    impl AsyncRegistry for CandidateRegistry {
        fn list_version_candidates<'a>(
            &'a self,
            _module: &'a ModulePath,
        ) -> BoxFuture<'a, Result<Vec<ExactVersion>>> {
            let candidates = self.candidates.clone();
            Box::pin(async move { Ok(candidates) })
        }

        fn fetch_manifest_raw<'a>(
            &'a self,
            _module: &'a ModulePath,
            version: &'a ExactVersion,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            let result = self.manifests.get(version).cloned().ok_or_else(|| {
                Error::RegistryError(format!("missing candidate manifest for {version}"))
            });
            Box::pin(async move { result })
        }

        fn fetch_source<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _asset_name: &'a str,
        ) -> BoxFuture<'a, Result<SourcePayload>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected candidate source fetch".to_string(),
                ))
            })
        }

        fn fetch_artifact<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _artifact: &'a ArtifactId,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected candidate artifact fetch".to_string(),
                ))
            })
        }
    }

    impl AsyncRegistry for FilesRegistry {
        fn list_version_candidates<'a>(
            &'a self,
            _module: &'a ModulePath,
        ) -> BoxFuture<'a, Result<Vec<ExactVersion>>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected list_version_candidates call".to_string(),
                ))
            })
        }

        fn fetch_manifest_raw<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected fetch_manifest_raw call".to_string(),
                ))
            })
        }

        fn fetch_source<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _asset_name: &'a str,
        ) -> BoxFuture<'a, Result<SourcePayload>> {
            let source_files = self.source_files.clone();
            let tree_raw = self.tree_raw.clone();
            Box::pin(async move {
                Ok(SourcePayload::Files {
                    source_files,
                    tree_raw,
                })
            })
        }

        fn fetch_artifact<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _artifact: &'a ArtifactId,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected fetch_artifact call".to_string(),
                ))
            })
        }
    }

    impl AsyncRegistry for PackageRegistry {
        fn list_version_candidates<'a>(
            &'a self,
            _module: &'a ModulePath,
        ) -> BoxFuture<'a, Result<Vec<ExactVersion>>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected list_version_candidates call".to_string(),
                ))
            })
        }

        fn fetch_manifest_raw<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected fetch_manifest_raw call".to_string(),
                ))
            })
        }

        fn fetch_source<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _asset_name: &'a str,
        ) -> BoxFuture<'a, Result<SourcePayload>> {
            let source_package = self.source_package.clone();
            Box::pin(async move { Ok(SourcePayload::Package(source_package)) })
        }

        fn fetch_artifact<'a>(
            &'a self,
            _module: &'a ModulePath,
            _version: &'a ExactVersion,
            _artifact: &'a ArtifactId,
        ) -> BoxFuture<'a, Result<Vec<u8>>> {
            Box::pin(async {
                Err(Error::RegistryError(
                    "unexpected fetch_artifact call".to_string(),
                ))
            })
        }
    }

    fn poll_ready<F: Future>(future: F) -> F::Output {
        let mut future = std::pin::pin!(future);
        let mut context = Context::from_waker(Waker::noop());
        match future.as_mut().poll(&mut context) {
            Poll::Ready(output) => output,
            Poll::Pending => panic!("test future unexpectedly returned Pending"),
        }
    }

    #[test]
    fn dependency_free_async_resolution_returns_no_lock_without_registry_or_cache_work() {
        let surface = RecordingSurface::default();
        let registry = CandidateRegistry {
            candidates: Vec::new(),
            manifests: BTreeMap::new(),
        };
        let mod_file = ModFile::parse_project(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();

        let resolved = poll_ready(resolve_mod_file_lock_and_ensure(
            &surface,
            &registry,
            &mod_file,
            "wasm32-unknown-unknown",
        ))
        .unwrap();

        assert!(resolved.lock_file.is_none());
        assert!(resolved.ready.is_empty());
        assert!(surface.mutations().is_empty());
    }

    fn tree_bytes(files: &[ExtractedSourceFile]) -> Vec<u8> {
        TreeManifest {
            format: 1,
            files: source_file_entries(files).unwrap(),
        }
        .render()
        .unwrap()
    }

    fn browser_manifest_fixture(
        files: &[ExtractedSourceFile],
    ) -> (LockedModule, ManifestSnapshot, Vec<u8>) {
        let tree_raw = tree_bytes(files);
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("0.1.0").unwrap();
        let mod_bytes = files
            .iter()
            .find(|file| file.path == Path::new("vo.mod"))
            .expect("fixture must contain vo.mod")
            .bytes
            .as_slice();
        let mod_file = ModFile::parse_project(std::str::from_utf8(mod_bytes).unwrap()).unwrap();
        let manifest = ReleaseManifest {
            format: 1,
            module: module.clone(),
            version,
            vo: ToolchainConstraint::parse("0.1.0").unwrap(),
            intent: crate::lock::module_intent_digest(&mod_file).unwrap(),
            dependencies: Vec::new(),
            source: ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 7,
                digest: Digest::from_sha256(b"archive"),
                tree: Digest::from_sha256(&tree_raw),
            },
            artifacts: Vec::new(),
        };
        let raw = manifest.render().unwrap().into_bytes();
        let locked = locked_module_from_manifest_raw(&manifest, &raw);
        (locked, ManifestSnapshot { manifest, raw }, tree_raw)
    }

    struct NativeArtifactBrowserFixture {
        source_files: Vec<ExtractedSourceFile>,
        locked: LockedModule,
        manifest: ManifestSnapshot,
        tree_raw: Vec<u8>,
        artifact: ManifestArtifact,
        artifact_bytes: Vec<u8>,
    }

    fn native_artifact_browser_fixture() -> NativeArtifactBrowserFixture {
        let artifact_bytes = b"native-artifact".to_vec();
        let artifact = ManifestArtifact {
            id: ArtifactId {
                kind: "extension-native".to_string(),
                target: "aarch64-apple-darwin".to_string(),
                name: "libpkg.dylib".to_string(),
            },
            size: artifact_bytes.len() as u64,
            digest: Digest::from_sha256(&artifact_bytes),
        };
        let mod_content = concat!(
            "format = 1\nmodule = \"github.com/acme/pkg\"\nversion = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n\n",
            "[extension]\n",
            "name = \"pkg\"\n\n",
            "[extension.native]\n",
            "targets = [\"aarch64-apple-darwin\"]\n",
        )
        .as_bytes()
        .to_vec();
        let files = vec![regular_file("vo.mod", mod_content.clone())];
        let tree_raw = tree_bytes(&files);
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("0.1.0").unwrap();
        let mod_file = ModFile::parse_project(std::str::from_utf8(&mod_content).unwrap()).unwrap();
        let manifest = ReleaseManifest {
            format: 1,
            module: module.clone(),
            version,
            vo: ToolchainConstraint::parse("0.1.0").unwrap(),
            intent: crate::lock::module_intent_digest(&mod_file).unwrap(),
            dependencies: Vec::new(),
            source: ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 7,
                digest: Digest::from_sha256(b"archive"),
                tree: Digest::from_sha256(&tree_raw),
            },
            artifacts: vec![artifact.clone()],
        };
        let raw = manifest.render().unwrap().into_bytes();
        let locked = locked_module_from_manifest_raw(&manifest, &raw);
        NativeArtifactBrowserFixture {
            source_files: files,
            locked,
            manifest: ManifestSnapshot { manifest, raw },
            tree_raw,
            artifact,
            artifact_bytes,
        }
    }

    #[test]
    fn candidate_selection_skips_newer_invalid_release_manifests() {
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let invalid_version = ExactVersion::parse("0.1.1").unwrap();
        let valid_version = ExactVersion::parse("0.1.0").unwrap();
        let (_, valid_manifest, _) = browser_manifest_fixture(&browser_source_files());
        let registry = CandidateRegistry {
            candidates: vec![valid_version.clone(), invalid_version.clone()],
            manifests: BTreeMap::from([
                (invalid_version, b"{}".to_vec()),
                (valid_version.clone(), valid_manifest.raw),
            ]),
        };

        assert_eq!(
            poll_ready(resolve_latest_version(&registry, &module)).unwrap(),
            valid_version
        );
        assert_eq!(
            poll_ready(resolve_version_with_constraint(
                &registry,
                &module,
                &DepConstraint::parse("^0.1.0").unwrap(),
            ))
            .unwrap(),
            valid_version
        );
    }

    #[test]
    fn latest_version_requires_an_explicit_prerelease_constraint() {
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let stable_version = ExactVersion::parse("0.1.0").unwrap();
        let prerelease_version = ExactVersion::parse("0.1.1-alpha.1").unwrap();
        let (_, stable_manifest, _) = browser_manifest_fixture(&browser_source_files());
        let mut prerelease_manifest = stable_manifest.manifest.clone();
        prerelease_manifest.version = prerelease_version.clone();
        let prerelease_raw = prerelease_manifest.render().unwrap().into_bytes();
        let registry = CandidateRegistry {
            candidates: vec![stable_version.clone(), prerelease_version.clone()],
            manifests: BTreeMap::from([
                (stable_version.clone(), stable_manifest.raw),
                (prerelease_version.clone(), prerelease_raw),
            ]),
        };

        assert_eq!(
            poll_ready(resolve_latest_version(&registry, &module)).unwrap(),
            stable_version
        );
        assert_eq!(
            poll_ready(resolve_version_with_constraint(
                &registry,
                &module,
                &DepConstraint::parse("0.1.1-alpha.1").unwrap(),
            ))
            .unwrap(),
            prerelease_version
        );
    }

    #[test]
    fn file_set_mismatches_fail_before_any_cache_mutation() {
        let expected_files = browser_source_files();
        let (locked, manifest, tree_raw) = browser_manifest_fixture(&expected_files);

        let mut wrong_content = expected_files.clone();
        wrong_content
            .iter_mut()
            .find(|file| file.path == Path::new("src/main.vo"))
            .unwrap()
            .bytes = b"package nope\n".to_vec();

        let mut missing_file = expected_files.clone();
        missing_file.retain(|file| file.path != Path::new("empty.vo"));

        let mut extra_file = expected_files;
        extra_file.push(regular_file("extra.vo", Vec::new()));

        for (case, files) in [
            ("wrong content", wrong_content),
            ("missing file", missing_file),
            ("extra file", extra_file),
        ] {
            let surface = RecordingSurface::default();
            let registry = FilesRegistry {
                source_files: files,
                tree_raw: tree_raw.clone(),
            };
            let error =
                poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

            assert!(
                matches!(error, Error::InvalidReleaseMetadata(_)),
                "{case}: unexpected error: {error}"
            );
            assert!(surface.mutations().is_empty(), "{case}: mutated cache");
            assert!(
                !surface.exists(&relative_module_dir(&locked.path, &locked.version)),
                "{case}: created module directory"
            );
        }
    }

    #[test]
    fn package_payload_rejects_invalid_archive_before_any_cache_mutation() {
        let files = browser_source_files();
        let (locked, manifest, _) = browser_manifest_fixture(&files);
        let surface = RecordingSurface::default();
        let registry = PackageRegistry {
            // This matches the fixture's authenticated size and digest, so the
            // common canonical archive parser owns the rejection.
            source_package: b"archive".to_vec(),
        };

        let error =
            poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

        assert!(matches!(&error, Error::SourceScan(_)), "{error}");
        assert!(surface.mutations().is_empty());
        assert!(!surface.exists(&relative_module_dir(&locked.path, &locked.version)));
    }

    #[test]
    fn matching_file_set_refuses_to_replace_an_invalid_existing_cache_tree() {
        let files = browser_source_files();
        let (locked, manifest, tree_raw) = browser_manifest_fixture(&files);
        let surface = RecordingSurface::default();
        let registry = FilesRegistry {
            source_files: files,
            tree_raw,
        };
        let module_dir = relative_module_dir(&locked.path, &locked.version);
        let stale_path = module_dir.join("stale.vo");
        surface.seed_text(&stale_path, "package stale\n");
        assert!(surface.exists(&stale_path));

        let error =
            poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

        assert!(
            error.to_string().contains("clean the module cache"),
            "{error}"
        );
        assert_eq!(surface.read_file(&stale_path).unwrap(), "package stale\n");
        assert!(!surface.exists(&module_dir.join("src/main.vo")));
    }

    #[test]
    fn ensure_ready_rejects_invalid_source_cache_before_registry_io() {
        let files = browser_source_files();
        let (locked, manifest, _) = browser_manifest_fixture(&files);
        let surface = RecordingSurface::default();
        let module_dir = relative_module_dir(&locked.path, &locked.version);
        surface.seed_text(&module_dir.join("stale.vo"), "package stale\n");
        let registry = NoFetchRegistry::default();

        let error = poll_ready(ensure_locked_module_ready(
            &surface,
            &registry,
            &locked,
            "wasm32-unknown-unknown",
            Some(manifest),
        ))
        .unwrap_err();

        assert!(
            error.to_string().contains("clean the module cache"),
            "{error}"
        );
        assert!(registry.calls().is_empty(), "{:?}", registry.calls());
    }

    #[test]
    fn ensure_ready_rejects_invalid_artifact_cache_before_registry_io() {
        let NativeArtifactBrowserFixture {
            source_files,
            locked,
            manifest,
            tree_raw,
            artifact,
            mut artifact_bytes,
        } = native_artifact_browser_fixture();
        let surface = RecordingSurface::default();
        let source_registry = FilesRegistry {
            source_files,
            tree_raw,
        };
        poll_ready(install_source(
            &surface,
            &source_registry,
            &locked,
            &manifest,
        ))
        .unwrap();
        let module_dir = relative_module_dir(&locked.path, &locked.version);
        let artifact_path =
            module_dir.join(crate::artifact::artifact_relative_path(&artifact.id).unwrap());
        artifact_bytes[0] ^= 1;
        surface.seed_text(
            &artifact_path,
            std::str::from_utf8(&artifact_bytes).unwrap(),
        );
        let registry = NoFetchRegistry::default();

        let error = poll_ready(ensure_locked_module_ready(
            &surface,
            &registry,
            &locked,
            "aarch64-apple-darwin",
            Some(manifest),
        ))
        .unwrap_err();

        assert!(
            error.to_string().contains("clean the module cache"),
            "{error}"
        );
        assert!(registry.calls().is_empty(), "{:?}", registry.calls());
    }

    #[test]
    fn files_payload_rejects_embedded_package_protocol_path() {
        let mut source_files = browser_source_files();
        let (locked, manifest, tree_raw) = browser_manifest_fixture(&source_files);
        source_files.push(regular_file("vo.tree.json", tree_raw.clone()));
        let surface = RecordingSurface::default();
        let registry = FilesRegistry {
            source_files,
            tree_raw,
        };

        let error =
            poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

        assert!(error.to_string().contains("reserved"), "{error}");
        assert!(surface.mutations().is_empty());
    }

    #[test]
    fn staging_write_failure_preserves_existing_cache_and_publishes_nothing() {
        let source_files = browser_source_files();
        let (locked, manifest, tree_raw) = browser_manifest_fixture(&source_files);
        let surface = RecordingSurface::default();
        let existing = Path::new("existing-cache/v1/keep.vo");
        surface.seed_text(existing, "package keep\n");
        surface.fail_writes_after(1);
        let registry = FilesRegistry {
            source_files,
            tree_raw,
        };

        let error =
            poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

        assert!(
            error.to_string().contains("injected staging write failure"),
            "{error}"
        );
        assert_eq!(surface.read_file(existing).unwrap(), "package keep\n");
        assert!(!surface.exists(&relative_module_dir(&locked.path, &locked.version)));
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn async_source_install_propagates_committed_durability_failure() {
        let source_files = browser_source_files();
        let (locked, manifest, tree_raw) = browser_manifest_fixture(&source_files);
        let root = tempfile::tempdir().unwrap();
        let surface = RealFs::new(root.path());
        let registry = FilesRegistry {
            source_files,
            tree_raw,
        };
        let module_dir = relative_module_dir(&locked.path, &locked.version);
        crate::cache::mutation_lock::fail_publication_sync_for_test(&root.path().join(&module_dir));

        let error =
            poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationDurabilityUnconfirmed { ref path, .. }
                if path.ends_with("github.com@acme@pkg/0.1.0")
        ));
        validate_installed_module(&surface, &module_dir, &locked).unwrap();
        poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap();
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn async_artifact_install_propagates_committed_durability_failure() {
        let NativeArtifactBrowserFixture {
            source_files,
            locked,
            manifest,
            tree_raw,
            artifact,
            artifact_bytes,
        } = native_artifact_browser_fixture();
        let root = tempfile::tempdir().unwrap();
        let surface = RealFs::new(root.path());
        let registry = FilesRegistry {
            source_files,
            tree_raw,
        };
        poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap();
        let module_dir = relative_module_dir(&locked.path, &locked.version);
        let artifact_path =
            module_dir.join(crate::artifact::artifact_relative_path(&artifact.id).unwrap());
        crate::cache::mutation_lock::fail_publication_sync_for_test(
            &root.path().join(&artifact_path),
        );

        let error = commit_prepared_artifact(
            &surface,
            &module_dir,
            &locked,
            &artifact,
            &artifact_path,
            &artifact_bytes,
        )
        .unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationDurabilityUnconfirmed { ref path, .. }
                if path.ends_with("libpkg.dylib")
        ));
        validate_installed_artifact(&surface, &module_dir, &locked, &artifact.id).unwrap();
        commit_prepared_artifact(
            &surface,
            &module_dir,
            &locked,
            &artifact,
            &artifact_path,
            &artifact_bytes,
        )
        .unwrap();
    }
}
