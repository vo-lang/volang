use std::collections::{BTreeMap, BTreeSet};
use std::future::Future;
use std::path::{Component, Path, PathBuf};
use std::pin::Pin;

use vo_common::vfs::{FileSystem, RealFs};

use crate::artifact::required_artifacts_for_target;
use crate::cache::layout::{
    discover_installed_version, relative_module_dir, SOURCE_DIGEST_MARKER, VERSION_MARKER,
};
use crate::cache::validate::{
    validate_installed_artifact, validate_installed_module, InstalledModuleError,
};
use crate::digest::{verify_size_and_digest, Digest};
use crate::ext_manifest::{parse_ext_manifest_content, ExtensionManifest};
use crate::identity::{ArtifactId, ModulePath};
use crate::lock::{locked_module_from_manifest_raw, validate_locked_module_against_manifest};
use crate::project;
use crate::readiness::{
    check_module_readiness, check_project_readiness, ModuleReadiness, ReadinessFailure, ReadyModule,
};
use crate::registry::{
    filter_compatible_versions, parse_requested_release_manifest, validate_manifest,
};
use crate::schema::lockfile::LockedModule;
use crate::schema::manifest::ReleaseManifest;
use crate::schema::modfile::ModFile;
use crate::version::{DepConstraint, ExactVersion};
use crate::{Error, Result};

pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

pub enum SourcePayload {
    Package(Vec<u8>),
    Files(Vec<(PathBuf, String)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleLoadRequest {
    Latest(ModulePath),
    InstalledOrLatest(ModulePath),
    Constraint {
        module: ModulePath,
        constraint: DepConstraint,
    },
}

pub trait AsyncRegistry {
    fn list_versions<'a>(
        &'a self,
        module: &'a ModulePath,
    ) -> BoxFuture<'a, Result<Vec<ExactVersion>>>;

    fn fetch_manifest_raw<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
    ) -> BoxFuture<'a, Result<(ReleaseManifest, Vec<u8>)>>;

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
    fn mkdir_all(&self, path: &Path) -> Result<()>;

    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> Result<()>;

    fn write_text(&self, path: &Path, content: &str) -> Result<()> {
        self.write_bytes(path, content.as_bytes())
    }
}

async fn resolve_module_request<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    request: ModuleLoadRequest,
) -> Result<(ModulePath, ExactVersion)> {
    match request {
        ModuleLoadRequest::Latest(module) => {
            let version = resolve_latest_version(registry, &module).await?;
            Ok((module, version))
        }
        ModuleLoadRequest::InstalledOrLatest(module) => {
            let version = match installed_exact_version(surface, &module)? {
                Some(version) => version,
                None => resolve_latest_version(registry, &module).await?,
            };
            Ok((module, version))
        }
        ModuleLoadRequest::Constraint { module, constraint } => {
            let version = if constraint.op == crate::version::ConstraintOp::Exact {
                ExactVersion::parse(&constraint.to_string()).map_err(|error| {
                    Error::InvalidVersion(format!(
                        "exact constraint for {} is not a valid version: {}",
                        module, error,
                    ))
                })?
            } else {
                resolve_version_with_constraint(registry, &module, &constraint).await?
            };
            Ok((module, version))
        }
    }
}

fn installed_exact_version<F: FileSystem>(
    fs: &F,
    module: &ModulePath,
) -> Result<Option<ExactVersion>> {
    let Some(version) =
        discover_installed_version(fs, Path::new(""), module.as_str()).map_err(|error| {
            Error::SourceScan(format!(
                "discover installed version for {}: {}",
                module, error
            ))
        })?
    else {
        return Ok(None);
    };
    ExactVersion::parse(&version).map(Some).map_err(|error| {
        Error::InvalidVersion(format!(
            "installed version marker for {}: {}",
            module, error
        ))
    })
}

fn read_installed_locked_module<F: FileSystem>(
    fs: &F,
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<LockedModule> {
    let manifest_rel = relative_module_dir(module.as_str(), version).join("vo.release.json");
    let manifest_raw = fs.read_bytes(&manifest_rel).map_err(|error| {
        Error::SourceScan(format!(
            "read {} for {} {}: {}",
            manifest_rel.display(),
            module,
            version,
            error,
        ))
    })?;
    let manifest = parse_requested_manifest_bytes(&manifest_raw, module, version)?;
    Ok(locked_module_from_manifest_raw(&manifest, &manifest_raw))
}

fn parse_requested_manifest_bytes(
    manifest_raw: &[u8],
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<ReleaseManifest> {
    let manifest_content = std::str::from_utf8(manifest_raw).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid UTF-8 in vo.release.json for {} {}: {}",
            module, version, error,
        ))
    })?;
    parse_requested_release_manifest(manifest_content, module, version)
}

fn read_installed_project_locked_modules<F: FileSystem>(
    fs: &F,
    locked: &LockedModule,
) -> Result<Vec<LockedModule>> {
    let module_dir = relative_module_dir(locked.path.as_str(), &locked.version);
    let lock_path = module_dir.join("vo.lock");
    if !fs.exists(&lock_path) {
        if locked.deps.is_empty() {
            return Ok(Vec::new());
        }
        return Err(Error::LockFileParse(format!(
            "cached module {} {} is missing vo.lock for declared dependencies",
            locked.path, locked.version,
        )));
    }
    let content = fs.read_file(&lock_path).map_err(|error| {
        Error::LockFileParse(format!(
            "read {} for {} {}: {}",
            lock_path.display(),
            locked.path,
            locked.version,
            error,
        ))
    })?;
    let lock_file = crate::schema::lockfile::LockFile::parse(&content)?;
    if lock_file.root.module.as_str() != locked.path.as_str() {
        return Err(Error::RootMismatch {
            field: "module".to_string(),
            mod_value: locked.path.to_string(),
            lock_value: lock_file.root.module.as_str().to_string(),
        });
    }
    if lock_file.root.vo != locked.vo {
        return Err(Error::RootMismatch {
            field: "vo".to_string(),
            mod_value: locked.vo.to_string(),
            lock_value: lock_file.root.vo.to_string(),
        });
    }
    Ok(lock_file.resolved)
}

fn read_installed_extension_manifest<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
) -> Result<Option<ExtensionManifest>> {
    let manifest_rel = module_dir.join("vo.ext.toml");
    if !fs.exists(&manifest_rel) {
        return Ok(None);
    }
    let content = fs.read_file(&manifest_rel).map_err(|error| {
        Error::SourceScan(format!("read {}: {}", manifest_rel.display(), error))
    })?;
    parse_ext_manifest_content(&content, &scoped_path(fs, &manifest_rel)).map(Some)
}

fn scoped_path<F: FileSystem>(fs: &F, path: &Path) -> PathBuf {
    match fs.root() {
        Some(root) => root.join(path),
        None => path.to_path_buf(),
    }
}

impl InstallSurface for RealFs {
    fn mkdir_all(&self, path: &Path) -> Result<()> {
        std::fs::create_dir_all(scoped_path(self, path))?;
        Ok(())
    }

    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> Result<()> {
        std::fs::write(scoped_path(self, path), bytes)?;
        Ok(())
    }
}

#[derive(Clone)]
struct SelectedModule {
    module: ModulePath,
    version: ExactVersion,
}

impl SelectedModule {
    fn from_locked(locked: &LockedModule) -> Self {
        Self {
            module: locked.path.clone(),
            version: locked.version.clone(),
        }
    }

    fn spec(&self) -> String {
        format!("{}@{}", self.module, self.version)
    }
}

struct ManifestSnapshot {
    manifest: ReleaseManifest,
    raw: Vec<u8>,
}

pub async fn resolve_latest_version<R: AsyncRegistry>(
    registry: &R,
    module: &ModulePath,
) -> Result<ExactVersion> {
    let mut versions = filter_compatible_versions(module, &registry.list_versions(module).await?);
    versions.sort_by(|a, b| b.cmp(a));
    versions.dedup();
    versions
        .into_iter()
        .next()
        .ok_or_else(|| Error::NoSatisfyingVersion {
            module: module.as_str().to_string(),
            detail: format!(
                "no published release is compatible with module path {}",
                module
            ),
        })
}

pub async fn resolve_version_with_constraint<R: AsyncRegistry>(
    registry: &R,
    module: &ModulePath,
    constraint: &DepConstraint,
) -> Result<ExactVersion> {
    let mut versions = filter_compatible_versions(module, &registry.list_versions(module).await?);
    versions.sort_by(|a, b| b.cmp(a));
    versions.dedup();
    versions
        .into_iter()
        .find(|version| constraint.satisfies(version))
        .ok_or_else(|| Error::NoSatisfyingVersion {
            module: module.as_str().to_string(),
            detail: format!(
                "no published release satisfies dependency constraint {}",
                constraint,
            ),
        })
}

pub async fn ensure_module_requests<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    requests: Vec<ModuleLoadRequest>,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    if requests.is_empty() {
        return Ok(Vec::new());
    }
    let mut initial = Vec::with_capacity(requests.len());
    for request in requests {
        initial.push(resolve_module_request(surface, registry, request).await?);
    }
    ensure_versioned_module_closure(surface, registry, initial, target).await
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
    ensure_locked_modules(
        surface,
        registry,
        project_deps.locked_modules().to_vec(),
        target,
    )
    .await
}

pub async fn ensure_mod_file_requirements<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    mod_file: &ModFile,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    let requests = mod_file
        .require
        .iter()
        .map(|req| ModuleLoadRequest::Constraint {
            module: req.module.clone(),
            constraint: req.constraint.clone(),
        })
        .collect::<Vec<_>>();
    ensure_module_requests(surface, registry, requests, target).await
}

pub fn collect_installed_locked_module_closure<F: FileSystem>(
    fs: &F,
    modules: &[ModulePath],
) -> Result<Vec<LockedModule>> {
    if modules.is_empty() {
        return Ok(Vec::new());
    }
    let mut initial = Vec::with_capacity(modules.len());
    for module in modules {
        let Some(version) = installed_exact_version(fs, module)? else {
            return Err(Error::SourceScan(format!(
                "module {} is not installed in cache",
                module,
            )));
        };
        initial.push((module.clone(), version));
    }
    collect_locked_modules_from_exact_versions(fs, &initial)
}

pub fn collect_locked_modules_from_exact_versions<F: FileSystem>(
    fs: &F,
    initial: &[(ModulePath, ExactVersion)],
) -> Result<Vec<LockedModule>> {
    if initial.is_empty() {
        return Ok(Vec::new());
    }
    let mut stack = Vec::new();
    let mut selected_versions = BTreeMap::new();
    push_selected_modules(
        &mut stack,
        &mut selected_versions,
        initial
            .iter()
            .cloned()
            .map(|(module, version)| SelectedModule { module, version }),
    )?;
    let mut visited = BTreeSet::new();
    let mut locked_modules = Vec::new();
    loop {
        let Some(selection) = stack.pop() else {
            break;
        };
        if !visited.insert(selection.spec()) {
            continue;
        }
        let locked = read_installed_locked_module(fs, &selection.module, &selection.version)?;
        let deps = read_installed_project_locked_modules(fs, &locked)?;
        push_selected_modules(
            &mut stack,
            &mut selected_versions,
            deps.iter().map(SelectedModule::from_locked),
        )?;
        locked_modules.push(locked);
    }
    normalize_locked_modules(locked_modules)
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
    check_project_readiness(surface, &normalized, target).map_err(readiness_failure_to_error)
}

pub async fn ensure_versioned_module_closure<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    initial: Vec<(ModulePath, ExactVersion)>,
    target: &str,
) -> Result<Vec<ReadyModule>> {
    let mut stack = Vec::new();
    let mut selected_versions = BTreeMap::new();
    push_selected_modules(
        &mut stack,
        &mut selected_versions,
        initial
            .into_iter()
            .map(|(module, version)| SelectedModule { module, version }),
    )?;
    let mut visited = BTreeSet::new();
    let mut locked_modules = Vec::new();

    loop {
        let Some(selection) = stack.pop() else {
            break;
        };
        if !visited.insert(selection.spec()) {
            continue;
        }
        let (locked, prefetched_manifest) =
            resolve_selected_locked_module(surface, registry, &selection).await?;
        remember_selected_version(
            &mut selected_versions,
            &SelectedModule::from_locked(&locked),
        )?;
        ensure_locked_module_ready(surface, registry, &locked, target, prefetched_manifest).await?;
        let deps = read_installed_project_locked_modules(surface, &locked)?;
        push_selected_modules(
            &mut stack,
            &mut selected_versions,
            deps.iter().map(SelectedModule::from_locked),
        )?;
        locked_modules.push(locked);
    }

    check_project_readiness(surface, &locked_modules, target).map_err(readiness_failure_to_error)
}

fn remember_selected_version(
    selected_versions: &mut BTreeMap<String, String>,
    selection: &SelectedModule,
) -> Result<()> {
    let module = selection.module.as_str().to_string();
    let version = selection.version.to_string();
    match selected_versions.get(&module) {
        Some(existing) if existing != &version => Err(Error::SelectedVersionConflict {
            module,
            existing: existing.clone(),
            requested: version,
        }),
        Some(_) => Ok(()),
        None => {
            selected_versions.insert(module, version);
            Ok(())
        }
    }
}

fn push_selected_modules<I>(
    stack: &mut Vec<SelectedModule>,
    selected_versions: &mut BTreeMap<String, String>,
    selections: I,
) -> Result<()>
where
    I: IntoIterator<Item = SelectedModule>,
{
    for selection in selections {
        remember_selected_version(selected_versions, &selection)?;
        stack.push(selection);
    }
    Ok(())
}

fn normalize_locked_modules(locked_modules: Vec<LockedModule>) -> Result<Vec<LockedModule>> {
    let mut selected_versions = BTreeMap::new();
    let mut visited = BTreeSet::new();
    let mut normalized = Vec::new();

    for locked in locked_modules {
        let selection = SelectedModule::from_locked(&locked);
        remember_selected_version(&mut selected_versions, &selection)?;
        if !visited.insert(selection.spec()) {
            continue;
        }
        normalized.push(locked);
    }

    Ok(normalized)
}

async fn resolve_selected_locked_module<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    selection: &SelectedModule,
) -> Result<(LockedModule, Option<ManifestSnapshot>)> {
    let module_dir = relative_module_dir(selection.module.as_str(), &selection.version);
    let manifest_path = module_dir.join("vo.release.json");
    if let Ok(manifest_raw) = surface.read_bytes(&manifest_path) {
        if let Ok(manifest) =
            parse_requested_manifest_bytes(&manifest_raw, &selection.module, &selection.version)
        {
            return Ok((
                locked_module_from_manifest_raw(&manifest, &manifest_raw),
                None,
            ));
        }
    }

    let (manifest, raw) = registry
        .fetch_manifest_raw(&selection.module, &selection.version)
        .await?;
    validate_manifest(&manifest, &selection.module, &selection.version)?;
    Ok((
        locked_module_from_manifest_raw(&manifest, &raw),
        Some(ManifestSnapshot { manifest, raw }),
    ))
}

async fn ensure_locked_module_ready<S: InstallSurface, R: AsyncRegistry>(
    surface: &S,
    registry: &R,
    locked: &LockedModule,
    target: &str,
    prefetched_manifest: Option<ManifestSnapshot>,
) -> Result<ReadyModule> {
    let module_dir = relative_module_dir(locked.path.as_str(), &locked.version);
    let ext_manifest = if validate_installed_module(surface, &module_dir, locked).is_ok() {
        read_installed_extension_manifest(surface, &module_dir)?
    } else {
        let manifest = match prefetched_manifest {
            Some(manifest) => manifest,
            None => fetch_locked_manifest(registry, locked).await?,
        };
        install_source(surface, registry, locked, &manifest).await?;
        read_installed_extension_manifest(surface, &module_dir)?
    };

    for required_artifact in required_artifacts_for_target(locked, ext_manifest.as_ref(), target)? {
        let artifact_path = module_dir.join(&required_artifact.cache_relative_path);
        if validate_installed_artifact(
            surface,
            &artifact_path,
            locked,
            required_artifact.locked_artifact,
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
        write_bytes(surface, &artifact_path, &bytes)?;
    }

    match check_module_readiness(surface, locked, ext_manifest.as_ref(), target) {
        ModuleReadiness::Ready(ready) => Ok(*ready),
        ModuleReadiness::NotReady(failure) => Err(readiness_failure_to_error(failure)),
    }
}

async fn fetch_locked_manifest<R: AsyncRegistry>(
    registry: &R,
    locked: &LockedModule,
) -> Result<ManifestSnapshot> {
    let (manifest, raw) = registry
        .fetch_manifest_raw(&locked.path, &locked.version)
        .await?;
    let digest = Digest::from_sha256(&raw);
    validate_locked_module_against_manifest(locked, &manifest, &digest)?;
    Ok(ManifestSnapshot { manifest, raw })
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
            crate::cache::install::extract_source_entries(&source_package).map_err(|error| {
                Error::SourceScan(format!("{} for {} {}", error, locked.path, locked.version,))
            })?
        }
        SourcePayload::Files(files) => files,
    };
    let module_dir = relative_module_dir(locked.path.as_str(), &locked.version);
    surface.mkdir_all(&module_dir)?;
    write_source_files(surface, &module_dir, &files)?;
    write_text(
        surface,
        &module_dir.join(VERSION_MARKER),
        &format!("{}\n", locked.version),
    )?;
    write_text(
        surface,
        &module_dir.join(SOURCE_DIGEST_MARKER),
        &format!("{}\n", locked.source),
    )?;
    write_bytes(surface, &module_dir.join("vo.release.json"), &manifest.raw)?;
    validate_installed_module(surface, &module_dir, locked).map_err(installed_module_error_to_error)
}

fn write_source_files<S: InstallSurface>(
    surface: &S,
    module_dir: &Path,
    files: &[(PathBuf, String)],
) -> Result<()> {
    for (relative_path, content) in files {
        validate_relative_source_path(relative_path)?;
        write_text(surface, &module_dir.join(relative_path), content)?;
    }
    Ok(())
}

fn validate_relative_source_path(path: &Path) -> Result<()> {
    if path.as_os_str().is_empty() {
        return Err(Error::SourceScan(
            "source package contained an empty path".to_string(),
        ));
    }
    for component in path.components() {
        match component {
            Component::Normal(_) | Component::CurDir => {}
            Component::ParentDir => {
                return Err(Error::SourceScan(format!(
                    "source package path must not contain '..': {}",
                    path.display(),
                )))
            }
            Component::RootDir | Component::Prefix(_) => {
                return Err(Error::SourceScan(format!(
                    "source package path must be relative: {}",
                    path.display(),
                )))
            }
        }
    }
    Ok(())
}

fn write_bytes<S: InstallSurface>(surface: &S, path: &Path, bytes: &[u8]) -> Result<()> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            surface.mkdir_all(parent)?;
        }
    }
    surface.write_bytes(path, bytes)
}

fn write_text<S: InstallSurface>(surface: &S, path: &Path, content: &str) -> Result<()> {
    write_bytes(surface, path, content.as_bytes())
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
        ReadinessFailure::ExtensionManifestReadFailed { detail, .. } => Error::SourceScan(detail),
        ReadinessFailure::ExtensionManifestParseFailed { detail, .. } => {
            Error::ExtManifestParse(detail)
        }
        ReadinessFailure::UnsupportedNativeTarget {
            module,
            version,
            target,
            ..
        } => Error::SourceScan(format!(
            "vo.ext.toml does not declare extension-native support for target {} in {}@{}",
            target, module, version,
        )),
        ReadinessFailure::ArtifactResolutionFailed { error, .. } => *error,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::schema::manifest::{ManifestRequire, ManifestSource, ReleaseManifest};
    use crate::version::{DepConstraint, ToolchainConstraint};
    use vo_common::vfs::MemoryFs;

    fn render_mod_file(module: &ModulePath, requires: &[(&str, &str)]) -> String {
        let mut content = format!("module {}\nvo ^0.1.0\n", module);
        if requires.is_empty() {
            return content;
        }
        content.push('\n');
        let mut sorted = requires.to_vec();
        sorted.sort_by(|a, b| a.0.cmp(b.0));
        for (dep, constraint) in sorted {
            content.push_str(&format!("require {} {}\n", dep, constraint));
        }
        content
    }

    fn populate_cached_module(
        fs: &mut MemoryFs,
        module_raw: &str,
        version_raw: &str,
        requires: &[(&str, &str)],
        lock_modules: Vec<LockedModule>,
    ) -> LockedModule {
        let module = ModulePath::parse(module_raw).unwrap();
        let version = ExactVersion::parse(version_raw).unwrap();
        let mut require = requires
            .iter()
            .map(|(dep, constraint)| ManifestRequire {
                module: ModulePath::parse(dep).unwrap(),
                constraint: DepConstraint::parse(constraint).unwrap(),
            })
            .collect::<Vec<_>>();
        require.sort_by(|a, b| a.module.cmp(&b.module));
        let source_bytes = format!("source:{}@{}", module, version).into_bytes();
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: module.clone(),
            version: version.clone(),
            commit: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            module_root: module.module_root().to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require,
            source: ManifestSource {
                name: format!("{}-source.tar.gz", module.repo()),
                size: source_bytes.len() as u64,
                digest: Digest::from_sha256(&source_bytes),
            },
            artifacts: Vec::new(),
        };
        let manifest_raw = manifest.render();
        let locked = locked_module_from_manifest_raw(&manifest, manifest_raw.as_bytes());
        let module_dir = relative_module_dir(module.as_str(), &version);
        fs.add_file(
            module_dir.join("vo.mod"),
            render_mod_file(&module, requires),
        );
        fs.add_file(module_dir.join(VERSION_MARKER), format!("{}\n", version));
        fs.add_file(
            module_dir.join(SOURCE_DIGEST_MARKER),
            format!("{}\n", locked.source),
        );
        fs.add_file(module_dir.join("vo.release.json"), manifest_raw);
        if !lock_modules.is_empty() {
            let mod_file = ModFile::parse(&render_mod_file(&module, requires)).unwrap();
            let lock_file = project::build_lock_file_from_mod_file(
                &mod_file,
                lock_modules,
                "test synthetic lock",
            );
            fs.add_file(module_dir.join("vo.lock"), lock_file.render());
        }
        locked
    }

    #[test]
    fn collect_locked_modules_from_exact_versions_uses_requested_version_even_when_cache_has_multiple_versions(
    ) {
        let mut fs = MemoryFs::new();
        let dep_locked =
            populate_cached_module(&mut fs, "github.com/acme/dep", "v0.1.2", &[], Vec::new());
        let _ = populate_cached_module(&mut fs, "github.com/acme/dep", "v0.1.3", &[], Vec::new());
        let _ = populate_cached_module(
            &mut fs,
            "github.com/acme/app",
            "v0.2.0",
            &[("github.com/acme/dep", "v0.1.2")],
            vec![dep_locked.clone()],
        );

        let locked = collect_locked_modules_from_exact_versions(
            &fs,
            &[(
                ModulePath::parse("github.com/acme/app").unwrap(),
                ExactVersion::parse("v0.2.0").unwrap(),
            )],
        )
        .unwrap();

        assert_eq!(locked.len(), 2);
        let app = locked
            .iter()
            .find(|module| module.path.as_str() == "github.com/acme/app")
            .unwrap();
        assert_eq!(app.version.to_string(), "v0.2.0");
        let dep = locked
            .iter()
            .find(|module| module.path.as_str() == "github.com/acme/dep")
            .unwrap();
        assert_eq!(dep.version.to_string(), "v0.1.2");
    }
}
