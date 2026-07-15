use std::collections::{BTreeMap, BTreeSet};
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::atomic::{AtomicU64, Ordering};

use vo_common::vfs::{FileSystem, RealFs};

use crate::artifact::required_artifacts_for_target;
use crate::async_solver::RootRequirement;
use crate::cache::layout::{relative_module_dir, SOURCE_DIGEST_MARKER, VERSION_MARKER};
use crate::cache::validate::{
    validate_installed_artifact, validate_installed_module,
    validate_installed_module_with_metadata, InstalledModuleError,
};
use crate::digest::{verify_size_and_digest, Digest};
use crate::identity::{ArtifactId, ModulePath};
use crate::lock::{locked_module_from_manifest_raw, validate_locked_module_against_manifest};
use crate::project;
use crate::readiness::{
    check_materialized_modules_readiness, check_module_readiness, ModuleReadiness,
    ReadinessFailure, ReadyModule,
};
use crate::schema::lockfile::{LockFile, LockedModule};
use crate::schema::manifest::ReleaseManifest;
use crate::schema::modfile::ModFile;
use crate::version::{DepConstraint, ExactVersion};
use crate::{Error, Result};

pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

pub enum SourcePayload {
    Package(Vec<u8>),
    Files {
        source_files: Vec<(PathBuf, String)>,
        web_manifest_raw: Vec<u8>,
    },
}

/// One frozen async solve, its canonical lock, and the exact materialization
/// result installed from the same manifest bytes.
#[derive(Debug)]
pub struct ResolvedModFileInstall {
    pub lock_file: LockFile,
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
    while let Some((directory, depth)) = pending.pop() {
        directories.push(directory.clone());
        for entry in std::fs::read_dir(&directory)? {
            let child = entry?.path();
            visited_entries = visited_entries.checked_add(1).ok_or_else(|| {
                Error::SourceScan("staged install entry count overflows usize".to_string())
            })?;
            if visited_entries > crate::MAX_SOURCE_ARCHIVE_ENTRIES {
                return Err(Error::SourceScan(format!(
                    "staged install tree contains more than {} entries",
                    crate::MAX_SOURCE_ARCHIVE_ENTRIES,
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
    files: Vec<(PathBuf, String)>,
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
    excluded_modules: &BTreeSet<ModulePath>,
) -> Result<Vec<ReadyModule>> {
    let mut locked_modules = Vec::new();
    locked_modules
        .try_reserve(graph.modules.len())
        .map_err(|_| Error::ResolutionLimitExceeded {
            resource: "selected lock graph allocation".to_string(),
            limit: crate::MAX_MODULE_DEPENDENCIES,
        })?;
    for (_, resolved) in graph.modules {
        let manifest = ManifestSnapshot {
            manifest: resolved.manifest,
            raw: resolved.manifest_raw,
        };
        let locked = locked_module_from_manifest_raw(&manifest.manifest, &manifest.raw);
        if excluded_modules.contains(&locked.path) {
            continue;
        }
        ensure_locked_module_ready(surface, registry, &locked, target, Some(manifest)).await?;
        locked_modules.push(locked);
    }
    check_materialized_modules_readiness(surface, &locked_modules, target)
        .map_err(readiness_failure_to_error)
}

pub async fn ensure_project_deps<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    project_deps: &project::ProjectDeps,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    if !project_deps.has_mod_file() || project_deps.locked_modules().is_empty() {
        return Ok(Vec::new());
    }
    let normalized = normalize_locked_modules(project_deps.locked_modules().to_vec())?;
    for locked in &normalized {
        ensure_locked_module_ready(surface, registry, locked, target, None).await?;
    }
    check_materialized_modules_readiness(surface, &normalized, target)
        .map_err(readiness_failure_to_error)
}

pub async fn ensure_mod_file_requirements<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    mod_file: &ModFile,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    Ok(resolve_mod_file_lock_and_ensure(
        surface,
        registry,
        mod_file,
        target,
        "vo async module resolver",
    )
    .await?
    .ready)
}

/// Solve, lock, and materialize a root module from one frozen registry graph.
/// The returned lock is generated before the graph is consumed by install, so
/// cache metadata and nested dependency locks cannot influence selection.
pub async fn resolve_mod_file_lock_and_ensure<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    mod_file: &ModFile,
    target: &str,
    created_by: &str,
) -> Result<ResolvedModFileInstall> {
    resolve_mod_file_lock_and_ensure_materialized(
        surface,
        registry,
        mod_file,
        target,
        created_by,
        &BTreeSet::new(),
    )
    .await
}

/// Solve and lock the complete root graph while materializing only the
/// registry-backed subset. Workspace replacements remain present in the lock
/// authority and are excluded solely from cache installation.
pub async fn resolve_mod_file_lock_and_ensure_materialized<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    mod_file: &ModFile,
    target: &str,
    created_by: &str,
    excluded_modules: &BTreeSet<ModulePath>,
) -> Result<ResolvedModFileInstall> {
    mod_file.validate()?;
    let requirements = mod_file
        .require
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
    let lock_file = crate::lock::generate_lock(mod_file, &graph, created_by)?;
    let ready = ensure_resolved_graph(surface, registry, graph, target, excluded_modules).await?;
    Ok(ResolvedModFileInstall { lock_file, ready })
}

pub async fn ensure_locked_modules<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    locked_modules: Vec<LockedModule>,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    let normalized = normalize_locked_modules(locked_modules)?;
    for locked in &normalized {
        ensure_locked_module_ready(surface, registry, locked, target, None).await?;
    }
    check_materialized_modules_readiness(surface, &normalized, target)
        .map_err(readiness_failure_to_error)
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

async fn ensure_locked_module_ready<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    locked: &LockedModule,
    target: &str,
    prefetched_manifest: Option<ManifestSnapshot>,
) -> Result<ReadyModule> {
    let module_dir = relative_module_dir(&locked.path, &locked.version);
    let ext_manifest = match validate_installed_module_with_metadata(surface, &module_dir, locked) {
        Ok(ext_manifest) => ext_manifest,
        Err(_) => {
            let manifest = match prefetched_manifest {
                Some(manifest) => manifest,
                None => fetch_locked_manifest(registry, locked).await?,
            };
            install_source(surface, registry, locked, &manifest).await?;
            validate_installed_module_with_metadata(surface, &module_dir, locked)
                .map_err(installed_module_error_to_error)?
        }
    };

    for required_artifact in required_artifacts_for_target(locked, ext_manifest.as_ref(), target)? {
        let artifact_path = module_dir.join(&required_artifact.cache_relative_path);
        if validate_installed_artifact(
            surface,
            &module_dir,
            locked,
            &required_artifact.locked_artifact.id,
        )
        .is_ok()
        {
            continue;
        }
        let bytes = registry
            .fetch_artifact(
                &locked.path,
                &locked.version,
                &required_artifact.locked_artifact.id,
            )
            .await?;
        verify_size_and_digest(
            &bytes,
            required_artifact.locked_artifact.size,
            &required_artifact.locked_artifact.digest,
            format!(
                "artifact {} for {} {}",
                required_artifact.locked_artifact.id.name, locked.path, locked.version,
            ),
        )?;
        commit_prepared_artifact(
            surface,
            &module_dir,
            locked,
            required_artifact.locked_artifact,
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
    artifact: &crate::schema::lockfile::LockedArtifact,
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
    let files = match source {
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
            drop(source_package);
            verify_source_file_set(
                &files,
                manifest.manifest.source.files_size,
                &manifest.manifest.source.files_digest,
                format!("source file set for {} {}", locked.path, locked.version),
            )?;
            files
        }
        SourcePayload::Files {
            mut source_files,
            web_manifest_raw,
        } => {
            validate_exact_source_payload(&source_files)?;
            verify_source_file_set(
                &source_files,
                manifest.manifest.source.files_size,
                &manifest.manifest.source.files_digest,
                format!("source file set for {} {}", locked.path, locked.version),
            )?;
            verify_size_and_digest(
                &web_manifest_raw,
                manifest.manifest.web_manifest.size,
                &manifest.manifest.web_manifest.digest,
                format!("vo.web.json for {} {}", locked.path, locked.version),
            )?;
            let web_manifest = String::from_utf8(web_manifest_raw).map_err(|error| {
                Error::InvalidReleaseMetadata(format!(
                    "vo.web.json for {} {} is not valid UTF-8: {}",
                    locked.path,
                    locked.version,
                    error.utf8_error(),
                ))
            })?;
            source_files.push((PathBuf::from("vo.web.json"), web_manifest));
            source_files
        }
    };
    preflight_source_install(&files, locked, manifest)?;
    commit_prepared_source(surface, PreparedSourceTree { files }, locked, manifest)
}

fn preflight_source_install(
    files: &[(PathBuf, String)],
    locked: &LockedModule,
    manifest: &ManifestSnapshot,
) -> Result<()> {
    let module_dir = relative_module_dir(&locked.path, &locked.version);
    let mut fs = BorrowedPreflightFs::new();
    for (relative_path, content) in files {
        fs.insert(
            &module_dir.join(relative_path),
            PreflightFile::Borrowed(content.as_bytes()),
        )?;
    }
    fs.insert(
        &module_dir.join(VERSION_MARKER),
        PreflightFile::Owned(format!("{}\n", locked.version).into_bytes()),
    )?;
    fs.insert(
        &module_dir.join(SOURCE_DIGEST_MARKER),
        PreflightFile::Owned(format!("{}\n", locked.source).into_bytes()),
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

    let stage_result = (|| {
        for (relative_path, content) in &prepared.files {
            transaction.write(relative_path, content.as_bytes())?;
        }
        transaction.write(
            Path::new(VERSION_MARKER),
            format!("{}\n", locked.version).as_bytes(),
        )?;
        transaction.write(
            Path::new(SOURCE_DIGEST_MARKER),
            format!("{}\n", locked.source).as_bytes(),
        )?;
        transaction.write(Path::new("vo.release.json"), &manifest.raw)?;
        verify_transaction_source(&transaction, &prepared.files, locked, manifest)
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

fn verify_transaction_source<S: InstallSurface>(
    transaction: &SurfaceTransaction<'_, S>,
    files: &[(PathBuf, String)],
    locked: &LockedModule,
    manifest: &ManifestSnapshot,
) -> Result<()> {
    for (relative_path, expected) in files {
        transaction.verify(relative_path, expected.as_bytes())?;
    }
    transaction.verify(
        Path::new(VERSION_MARKER),
        format!("{}\n", locked.version).as_bytes(),
    )?;
    transaction.verify(
        Path::new(SOURCE_DIGEST_MARKER),
        format!("{}\n", locked.source).as_bytes(),
    )?;
    transaction.verify(Path::new("vo.release.json"), &manifest.raw)
}

fn verify_source_file_set(
    files: &[(PathBuf, String)],
    expected_size: u64,
    expected_digest: &Digest,
    context: impl Into<String>,
) -> Result<()> {
    let context = context.into();
    let found = source_file_set_integrity(files)?;
    if found.total_size != expected_size {
        return Err(Error::DigestMismatch {
            context: format!("{context}: size mismatch"),
            expected: format!("{expected_size} bytes"),
            found: format!("{} bytes", found.total_size),
        });
    }
    if found.digest != *expected_digest {
        return Err(Error::DigestMismatch {
            context,
            expected: expected_digest.to_string(),
            found: found.digest.to_string(),
        });
    }
    Ok(())
}

fn source_file_set_integrity(
    files: &[(PathBuf, String)],
) -> Result<crate::schema::CanonicalSourceFileSet> {
    let entries = source_file_entries(files)?;
    crate::schema::canonical_source_file_set(&entries).map_err(Error::SourceScan)
}

fn source_file_entries(files: &[(PathBuf, String)]) -> Result<Vec<crate::schema::SourceFileEntry>> {
    let mut entries = Vec::new();
    entries
        .try_reserve(files.len().min(vo_common::vfs::MAX_PACKAGE_SOURCE_FILES))
        .map_err(|_| Error::SourceScan("failed to reserve source file entries".to_string()))?;
    for (path, content) in files {
        let portable_path =
            crate::schema::portable_relative_path_from_path(path).map_err(|error| {
                Error::SourceScan(format!(
                    "source payload path must be a normalized portable relative path: {}: {error}",
                    path.display(),
                ))
            })?;
        if crate::schema::is_reserved_module_cache_path(&portable_path) {
            return Err(Error::SourceScan(format!(
                "source package path is reserved for module-cache metadata: {}",
                path.display()
            )));
        }
        if !crate::schema::is_source_file_set_candidate(&portable_path)
            .map_err(Error::SourceScan)?
        {
            if portable_path == "vo.web.json" {
                continue;
            }
            return Err(Error::SourceScan(format!(
                "source package path aliases protocol file vo.web.json: {}",
                path.display(),
            )));
        }
        if entries.len() >= vo_common::vfs::MAX_PACKAGE_SOURCE_FILES {
            return Err(Error::SourceScan(format!(
                "source payload contains more than {} source files",
                vo_common::vfs::MAX_PACKAGE_SOURCE_FILES
            )));
        }
        let content_size = u64::try_from(content.len())
            .map_err(|_| Error::SourceScan("source file size exceeds u64".to_string()))?;
        entries.push(crate::schema::SourceFileEntry {
            path: portable_path,
            size: content_size,
            digest: Digest::from_sha256(content.as_bytes()),
        });
    }
    Ok(entries)
}

fn validate_exact_source_payload(files: &[(PathBuf, String)]) -> Result<()> {
    for (path, _) in files {
        let portable = crate::schema::portable_relative_path_from_path(path).map_err(|error| {
            Error::SourceScan(format!(
                "source payload path must be a normalized portable relative path: {}: {error}",
                path.display(),
            ))
        })?;
        if !crate::schema::is_source_file_set_candidate(&portable).map_err(Error::SourceScan)? {
            return Err(Error::SourceScan(format!(
                "source payload must contain only declared source files; protocol path {} has a separate field",
                path.display(),
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
fn validate_source_files(files: &[(PathBuf, String)]) -> Result<()> {
    source_file_set_integrity(files).map(drop)
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
    use crate::schema::manifest::{
        ManifestArtifact, ManifestSource, ManifestWebManifest, ReleaseManifest,
    };
    use crate::version::{DepConstraint, ToolchainConstraint};
    use std::sync::Mutex;
    use std::task::{Context, Poll, Waker};

    #[test]
    fn source_file_preflight_rejects_aliases_and_cache_metadata_paths() {
        let aliases = vec![
            (
                PathBuf::from("Source/main.vo"),
                "package source".to_string(),
            ),
            (
                PathBuf::from("source/other.vo"),
                "package source".to_string(),
            ),
        ];
        assert!(validate_source_files(&aliases).is_err());

        let reserved = vec![(
            PathBuf::from("ARTIFACTS/injected.wasm"),
            "not an artifact".to_string(),
        )];
        assert!(validate_source_files(&reserved).is_err());
    }

    fn browser_source_files() -> Vec<(PathBuf, String)> {
        vec![
            (
                PathBuf::from("vo.mod"),
                "module github.com/acme/pkg\nvo ^0.1.0\n".to_string(),
            ),
            (PathBuf::from("src/main.vo"), "package main\n".to_string()),
            (PathBuf::from("empty.vo"), String::new()),
        ]
    }

    #[test]
    fn source_file_set_integrity_matches_browser_protocol_and_ignores_payload_order() {
        let files = browser_source_files();
        let forward = source_file_set_integrity(&files).unwrap();
        let mut reversed_files = files;
        reversed_files.reverse();
        let reversed = source_file_set_integrity(&reversed_files).unwrap();

        assert_eq!(forward.total_size, 50);
        assert_eq!(forward.total_size, reversed.total_size);
        assert_eq!(forward.digest, reversed.digest);
        assert_eq!(
            forward.digest,
            Digest::parse(
                "sha256:093f04d156c7a013973bc7e25003fa1a304ac97cd22453004c8646101198e6bd"
            )
            .unwrap()
        );
    }

    #[test]
    fn files_payload_uses_the_shared_portable_path_budget() {
        let maximum_component = "a".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES);
        validate_exact_source_payload(&[(PathBuf::from(&maximum_component), String::new())])
            .unwrap();

        let oversized_component = "a".repeat(
            crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES
                .checked_add(1)
                .unwrap(),
        );
        let error =
            validate_exact_source_payload(&[(PathBuf::from(oversized_component), String::new())])
                .unwrap_err();
        assert!(error.to_string().contains("portable"), "{error}");
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
        source_files: Vec<(PathBuf, String)>,
        web_manifest_raw: Vec<u8>,
    }

    struct CandidateRegistry {
        candidates: Vec<ExactVersion>,
        manifests: BTreeMap<ExactVersion, Vec<u8>>,
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
            let web_manifest_raw = self.web_manifest_raw.clone();
            Box::pin(async move {
                Ok(SourcePayload::Files {
                    source_files,
                    web_manifest_raw,
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

    fn poll_ready<F: Future>(future: F) -> F::Output {
        let mut future = std::pin::pin!(future);
        let mut context = Context::from_waker(Waker::noop());
        match future.as_mut().poll(&mut context) {
            Poll::Ready(output) => output,
            Poll::Pending => panic!("test future unexpectedly returned Pending"),
        }
    }

    fn browser_manifest_fixture(
        files: &[(PathBuf, String)],
    ) -> (LockedModule, ManifestSnapshot, String) {
        let integrity = source_file_set_integrity(files).unwrap();
        let mut source_entries = source_file_entries(files).unwrap();
        source_entries.sort_by(|left, right| left.path.cmp(&right.path));
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("v0.1.0").unwrap();
        let web_raw = format!(
            "{}\n",
            serde_json::to_string_pretty(&serde_json::json!({
                "schema_version": 1,
                "module": module.as_str(),
                "version": version.to_string(),
                "commit": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "module_root": module.module_root(),
                "vo": "^0.1.0",
                "require": [],
                "source_digest": integrity.digest,
                "source": source_entries,
                "web": null,
                "extension": null,
                "artifacts": [],
            }))
            .unwrap()
        );
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: module.clone(),
            version,
            commit: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            module_root: module.module_root().to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
            source: ManifestSource {
                name: "pkg-v0.1.0.tar.gz".to_string(),
                size: 7,
                digest: Digest::from_sha256(b"archive"),
                files_size: integrity.total_size,
                files_digest: integrity.digest,
            },
            web_manifest: ManifestWebManifest {
                size: web_raw.len() as u64,
                digest: Digest::from_sha256(web_raw.as_bytes()),
            },
            artifacts: Vec::new(),
        };
        let raw = manifest.render().unwrap().into_bytes();
        let locked = locked_module_from_manifest_raw(&manifest, &raw);
        (locked, ManifestSnapshot { manifest, raw }, web_raw)
    }

    struct NativeArtifactBrowserFixture {
        source_files: Vec<(PathBuf, String)>,
        locked: LockedModule,
        manifest: ManifestSnapshot,
        web_raw: String,
        artifact: crate::schema::lockfile::LockedArtifact,
        artifact_bytes: Vec<u8>,
    }

    fn native_artifact_browser_fixture() -> NativeArtifactBrowserFixture {
        let artifact_bytes = b"native-artifact".to_vec();
        let artifact = crate::schema::lockfile::LockedArtifact {
            id: ArtifactId {
                kind: "extension-native".to_string(),
                target: "aarch64-apple-darwin".to_string(),
                name: "libpkg.dylib".to_string(),
            },
            size: artifact_bytes.len() as u64,
            digest: Digest::from_sha256(&artifact_bytes),
        };
        let mod_content = concat!(
            "module github.com/acme/pkg\n",
            "vo ^0.1.0\n\n",
            "[extension]\n",
            "name = \"pkg\"\n\n",
            "[extension.native]\n",
            "path = \"rust/target/{profile}/libpkg\"\n\n",
            "[[extension.native.targets]]\n",
            "target = \"aarch64-apple-darwin\"\n",
            "library = \"libpkg.dylib\"\n",
        )
        .to_string();
        let files = vec![(PathBuf::from("vo.mod"), mod_content.clone())];
        let integrity = source_file_set_integrity(&files).unwrap();
        let source_entries = source_file_entries(&files).unwrap();
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("v0.1.0").unwrap();
        let parsed_mod = crate::schema::modfile::ModFile::parse(&mod_content).unwrap();
        let extension = parsed_mod.extension.as_ref().unwrap();
        let web_raw = format!(
            "{}\n",
            serde_json::to_string_pretty(&serde_json::json!({
                "schema_version": 1,
                "module": module.as_str(),
                "version": version.to_string(),
                "commit": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "module_root": module.module_root(),
                "vo": "^0.1.0",
                "require": [],
                "source_digest": integrity.digest,
                "source": source_entries,
                "web": null,
                "extension": {
                    "name": extension.name,
                    "include": extension.include,
                    "wasm": extension.wasm,
                    "web": extension.web,
                },
                "artifacts": [],
            }))
            .unwrap()
        );
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: module.clone(),
            version,
            commit: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            module_root: module.module_root().to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
            source: ManifestSource {
                name: "pkg-v0.1.0.tar.gz".to_string(),
                size: 7,
                digest: Digest::from_sha256(b"archive"),
                files_size: integrity.total_size,
                files_digest: integrity.digest,
            },
            web_manifest: ManifestWebManifest {
                size: web_raw.len() as u64,
                digest: Digest::from_sha256(web_raw.as_bytes()),
            },
            artifacts: vec![ManifestArtifact {
                id: artifact.id.clone(),
                size: artifact.size,
                digest: artifact.digest.clone(),
            }],
        };
        let raw = manifest.render().unwrap().into_bytes();
        let locked = locked_module_from_manifest_raw(&manifest, &raw);
        NativeArtifactBrowserFixture {
            source_files: files,
            locked,
            manifest: ManifestSnapshot { manifest, raw },
            web_raw,
            artifact,
            artifact_bytes,
        }
    }

    #[test]
    fn candidate_selection_skips_newer_invalid_release_manifests() {
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let invalid_version = ExactVersion::parse("v0.1.1").unwrap();
        let valid_version = ExactVersion::parse("v0.1.0").unwrap();
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
        let stable_version = ExactVersion::parse("v0.1.0").unwrap();
        let prerelease_version = ExactVersion::parse("v0.1.1-alpha.1").unwrap();
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
                &DepConstraint::parse("v0.1.1-alpha.1").unwrap(),
            ))
            .unwrap(),
            prerelease_version
        );
    }

    #[test]
    fn file_set_mismatches_fail_before_any_cache_mutation() {
        let expected_files = browser_source_files();
        let (locked, manifest, web_raw) = browser_manifest_fixture(&expected_files);

        let mut wrong_content = expected_files.clone();
        wrong_content
            .iter_mut()
            .find(|(path, _)| path == Path::new("src/main.vo"))
            .unwrap()
            .1 = "package nope\n".to_string();

        let mut missing_file = expected_files.clone();
        missing_file.retain(|(path, _)| path != Path::new("empty.vo"));

        let mut extra_file = expected_files;
        extra_file.push((PathBuf::from("extra.vo"), String::new()));

        for (case, files) in [
            ("wrong content", wrong_content),
            ("missing file", missing_file),
            ("extra file", extra_file),
        ] {
            let surface = RecordingSurface::default();
            let registry = FilesRegistry {
                source_files: files,
                web_manifest_raw: web_raw.clone().into_bytes(),
            };
            let error =
                poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

            match &error {
                Error::DigestMismatch { context, .. } => assert_eq!(
                    context, "source file set for github.com/acme/pkg v0.1.0",
                    "{case}"
                ),
                _ => panic!("{case}: unexpected error: {error}"),
            }
            assert!(surface.mutations().is_empty(), "{case}: mutated cache");
            assert!(
                !surface.exists(&relative_module_dir(&locked.path, &locked.version)),
                "{case}: created module directory"
            );
        }
    }

    #[test]
    fn matching_file_set_refuses_to_replace_an_invalid_existing_cache_tree() {
        let files = browser_source_files();
        let (locked, manifest, web_raw) = browser_manifest_fixture(&files);
        let surface = RecordingSurface::default();
        let registry = FilesRegistry {
            source_files: files,
            web_manifest_raw: web_raw.into_bytes(),
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
    fn files_payload_has_one_structural_web_manifest_and_rejects_protocol_path_in_source_files() {
        let mut source_files = browser_source_files();
        let (locked, manifest, web_raw) = browser_manifest_fixture(&source_files);
        source_files.push((PathBuf::from("vo.web.json"), web_raw.clone()));
        let surface = RecordingSurface::default();
        let registry = FilesRegistry {
            source_files,
            web_manifest_raw: web_raw.into_bytes(),
        };

        let error =
            poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

        assert!(error.to_string().contains("separate field"), "{error}");
        assert!(surface.mutations().is_empty());
    }

    #[test]
    fn staging_write_failure_preserves_existing_cache_and_publishes_nothing() {
        let source_files = browser_source_files();
        let (locked, manifest, web_raw) = browser_manifest_fixture(&source_files);
        let surface = RecordingSurface::default();
        let existing = Path::new("existing-cache/v1/keep.vo");
        surface.seed_text(existing, "package keep\n");
        surface.fail_writes_after(1);
        let registry = FilesRegistry {
            source_files,
            web_manifest_raw: web_raw.into_bytes(),
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
        let (locked, manifest, web_raw) = browser_manifest_fixture(&source_files);
        let root = tempfile::tempdir().unwrap();
        let surface = RealFs::new(root.path());
        let registry = FilesRegistry {
            source_files,
            web_manifest_raw: web_raw.into_bytes(),
        };
        let module_dir = relative_module_dir(&locked.path, &locked.version);
        crate::cache::mutation_lock::fail_publication_sync_for_test(&root.path().join(&module_dir));

        let error =
            poll_ready(install_source(&surface, &registry, &locked, &manifest)).unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationDurabilityUnconfirmed { ref path, .. }
                if path.ends_with("github.com@acme@pkg/v0.1.0")
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
            web_raw,
            artifact,
            artifact_bytes,
        } = native_artifact_browser_fixture();
        let root = tempfile::tempdir().unwrap();
        let surface = RealFs::new(root.path());
        let registry = FilesRegistry {
            source_files,
            web_manifest_raw: web_raw.into_bytes(),
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
