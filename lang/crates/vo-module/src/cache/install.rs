use std::collections::BTreeSet;
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::cache::layout::{cache_key, SOURCE_DIGEST_MARKER, VERSION_MARKER};
use crate::digest::{verify_digest, verify_size_and_digest};
use crate::identity::ModulePath;
use crate::lock::locked_module_from_manifest_raw;
use crate::registry::Registry;
use crate::schema::lockfile::{LockFile, LockRoot, LockedArtifact, LockedModule};
use crate::version::ExactVersion;
use crate::Error;

static TEMP_PATH_COUNTER: AtomicU64 = AtomicU64::new(0);

struct TempPathGuard {
    path: PathBuf,
    is_dir: bool,
}

impl TempPathGuard {
    fn dir(path: PathBuf) -> Self {
        Self { path, is_dir: true }
    }

    fn file(path: PathBuf) -> Self {
        Self {
            path,
            is_dir: false,
        }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempPathGuard {
    fn drop(&mut self) {
        if self.is_dir {
            let _ = std::fs::remove_dir_all(&self.path);
        } else {
            let _ = std::fs::remove_file(&self.path);
        }
    }
}

fn next_temp_path(parent: &Path, stem: &str) -> PathBuf {
    let counter = TEMP_PATH_COUNTER.fetch_add(1, Ordering::Relaxed);
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    parent.join(format!(
        ".{stem}.{}.{}.tmp",
        std::process::id(),
        timestamp + counter as u128,
    ))
}

fn create_unique_stage_dir(parent: &Path, stem: &str) -> Result<TempPathGuard, Error> {
    for _ in 0..64 {
        let path = next_temp_path(parent, stem);
        match std::fs::create_dir(&path) {
            Ok(()) => return Ok(TempPathGuard::dir(path)),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(Error::Io(error)),
        }
    }
    Err(Error::SourceScan(format!(
        "failed to allocate unique staging directory in {}",
        parent.display(),
    )))
}

fn create_unique_temp_file(
    parent: &Path,
    stem: &str,
) -> Result<(std::fs::File, TempPathGuard), Error> {
    for _ in 0..64 {
        let path = next_temp_path(parent, stem);
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        {
            Ok(file) => return Ok((file, TempPathGuard::file(path))),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(Error::Io(error)),
        }
    }
    Err(Error::SourceScan(format!(
        "failed to allocate unique temporary file in {}",
        parent.display(),
    )))
}

fn atomic_write_bytes(path: &Path, bytes: &[u8]) -> Result<(), Error> {
    let parent = path
        .parent()
        .ok_or_else(|| Error::SourceScan(format!("path has no parent: {}", path.display())))?;
    std::fs::create_dir_all(parent)?;
    let stem = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("artifact");
    let (mut file, temp) = create_unique_temp_file(parent, stem)?;
    file.write_all(bytes)?;
    file.sync_all()?;
    drop(file);

    match std::fs::rename(temp.path(), path) {
        Ok(()) => Ok(()),
        Err(rename_error) => {
            if let Ok(existing) = std::fs::read(path) {
                if existing == bytes {
                    return Ok(());
                }
            }
            if path.exists() {
                match std::fs::remove_file(path) {
                    Ok(()) => {
                        std::fs::rename(temp.path(), path)?;
                        Ok(())
                    }
                    Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                        std::fs::rename(temp.path(), path)?;
                        Ok(())
                    }
                    Err(error) => Err(Error::Io(error)),
                }
            } else {
                Err(Error::Io(rename_error))
            }
        }
    }
}

// ── Source package extraction (platform-agnostic) ────────────────────────────

enum ArchiveEntry {
    Directory(PathBuf),
    File(PathBuf, Vec<u8>),
}

/// Extract source entries from a tar.gz release source package.
///
/// Strips the top-level archive directory (standard release convention),
/// filters to allowed module files plus any paths declared in
/// `[extension].include` of `vo.ext.toml`, and skips non-UTF-8 entries
/// except for declared include paths.
///
/// Returns entries with paths relative to the module root.
pub fn extract_source_entries(archive_bytes: &[u8]) -> Result<Vec<(PathBuf, String)>, String> {
    let archive_entries = read_archive_entries(archive_bytes)?;
    let include_paths = source_package_include_paths(&archive_entries)?;
    let mut entries = Vec::new();

    for entry in archive_entries {
        let ArchiveEntry::File(relative_path, bytes) = entry else {
            continue;
        };
        if !source_entry_allowed(&relative_path, &include_paths) {
            continue;
        }
        let content = match String::from_utf8(bytes) {
            Ok(c) => c,
            Err(error) => {
                if included_source_path_matches(&relative_path, &include_paths) {
                    return Err(format!(
                        "included file {} is not valid UTF-8: {}",
                        relative_path.display(),
                        error
                    ));
                }
                continue;
            }
        };
        entries.push((relative_path, content));
    }

    Ok(entries)
}

fn source_package_include_paths(entries: &[ArchiveEntry]) -> Result<BTreeSet<PathBuf>, String> {
    let Some(bytes) = entries.iter().find_map(|entry| match entry {
        ArchiveEntry::File(relative_path, bytes) if relative_path == Path::new("vo.ext.toml") => {
            Some(bytes.as_slice())
        }
        _ => None,
    }) else {
        return Ok(BTreeSet::new());
    };
    let content = std::str::from_utf8(bytes).map_err(|error| {
        format!(
            "vo.ext.toml in source package is not valid UTF-8: {}",
            error
        )
    })?;
    let declared = crate::ext_manifest::include_paths_from_content(content)
        .map_err(|error| error.to_string())?;
    let mut paths = BTreeSet::new();
    for p in declared {
        paths.insert(normalize_source_relative_path(&p)?);
    }
    Ok(paths)
}

fn normalize_source_relative_path(path: &Path) -> Result<PathBuf, String> {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => normalized.push(part),
            Component::CurDir => {}
            _ => {
                return Err(format!(
                    "include path must be a relative path inside the module: {}",
                    path.display()
                ))
            }
        };
    }
    if normalized.as_os_str().is_empty() {
        return Err("include path must not be empty".to_string());
    }
    Ok(normalized)
}

fn strip_archive_root(
    raw_path: &Path,
    archive_root: &mut Option<String>,
) -> Result<Option<PathBuf>, String> {
    let mut components = raw_path.components();
    let first = match components.next() {
        Some(first) => first,
        None => return Ok(None),
    };
    let std::path::Component::Normal(first) = first else {
        return Err(format!(
            "invalid source package entry path: {}",
            raw_path.display()
        ));
    };
    let first = first.to_string_lossy().to_string();
    match archive_root {
        Some(existing) if *existing != first => {
            return Err("source package must unpack into a single top-level directory".to_string());
        }
        Some(_) => {}
        None => *archive_root = Some(first),
    }
    let stripped = components.as_path();
    if stripped.as_os_str().is_empty() {
        return Ok(None);
    }
    Ok(Some(stripped.to_path_buf()))
}

fn source_entry_allowed(path: &Path, studio_asset_paths: &BTreeSet<PathBuf>) -> bool {
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    let path_str = path.to_string_lossy();
    path_str.ends_with(".vo")
        || name == "vo.mod"
        || name == "vo.lock"
        || name == "vo.ext.toml"
        || included_source_path_matches(path, studio_asset_paths)
}

fn included_source_path_matches(path: &Path, include_paths: &BTreeSet<PathBuf>) -> bool {
    include_paths
        .iter()
        .any(|include_path| path == include_path || path.starts_with(include_path))
}

fn read_archive_entries(data: &[u8]) -> Result<Vec<ArchiveEntry>, String> {
    use std::io::Read;

    let gz = flate2::read::GzDecoder::new(data);
    let mut archive = tar::Archive::new(gz);
    let mut archive_root: Option<String> = None;
    let mut entries = Vec::new();

    for entry in archive.entries().map_err(|e| e.to_string())? {
        let mut entry = entry.map_err(|e| e.to_string())?;
        let entry_type = entry.header().entry_type();
        if entry_type.is_symlink() || entry_type.is_hard_link() {
            return Err("archive contains a symlink or hardlink, which is not allowed".to_string());
        }

        let raw_path = entry.path().map_err(|e| e.to_string())?.into_owned();
        let Some(relative_path) = strip_archive_root(&raw_path, &mut archive_root)? else {
            continue;
        };
        validate_archive_entry_path(&relative_path)?;

        if entry_type.is_dir() {
            entries.push(ArchiveEntry::Directory(relative_path));
            continue;
        }
        if entry_type.is_file() {
            let mut bytes = Vec::new();
            entry.read_to_end(&mut bytes).map_err(|e| e.to_string())?;
            entries.push(ArchiveEntry::File(relative_path, bytes));
        }
    }

    if archive_root.is_none() {
        return Err("source package archive is empty".to_string());
    }

    Ok(entries)
}

fn validate_archive_entry_path(relative_path: &Path) -> Result<(), String> {
    for component in relative_path.components() {
        if !matches!(component, std::path::Component::Normal(_)) {
            return Err(format!(
                "archive entry contains invalid path component: {}",
                relative_path.display()
            ));
        }
    }
    Ok(())
}

/// Check if a source package is already cached and valid.
fn is_source_cached(cache_root: &Path, locked: &LockedModule) -> bool {
    validate_source_cache_entry(cache_root, locked).is_ok()
}

/// Check if a specific artifact is cached and valid.
fn is_artifact_cached(cache_root: &Path, locked: &LockedModule, artifact: &LockedArtifact) -> bool {
    validate_artifact_cache_entry(cache_root, locked, artifact).is_ok()
}

/// Download and verify a source package into the cache.
fn download_source(
    cache_root: &Path,
    locked: &LockedModule,
    registry: &dyn Registry,
    source_asset_name: &str,
    manifest_raw: &[u8],
) -> Result<(), Error> {
    if is_source_cached(cache_root, locked) {
        return Ok(());
    }

    let data = registry.fetch_source_package(&locked.path, &locked.version, source_asset_name)?;

    verify_digest(
        &data,
        &locked.source,
        format!("source package for {} {}", locked.path, locked.version),
    )?;

    let dir = crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
    let parent = dir.parent().ok_or_else(|| {
        Error::SourceScan(format!(
            "cache directory has no parent for {} {}",
            locked.path, locked.version,
        ))
    })?;
    std::fs::create_dir_all(parent)?;

    let stage = create_unique_stage_dir(
        parent,
        &format!("{}-{}", cache_key(locked.path.as_str()), locked.version),
    )?;

    safe_unpack_tar_gz(&data, stage.path()).map_err(|e| Error::DigestMismatch {
        context: format!("source package for {} {}", locked.path, locked.version),
        expected: "safe archive".to_string(),
        found: e,
    })?;

    if !stage.path().join("vo.mod").is_file() {
        return Err(Error::SourceScan(format!(
            "source package for {} {} does not contain vo.mod at the module root",
            locked.path, locked.version,
        )));
    }

    // Write metadata files used by compile-time frozen-build validation
    std::fs::write(
        stage.path().join(SOURCE_DIGEST_MARKER),
        format!("{}\n", locked.source),
    )?;
    std::fs::write(
        stage.path().join(VERSION_MARKER),
        format!("{}\n", locked.version),
    )?;
    std::fs::write(stage.path().join("vo.release.json"), manifest_raw)?;

    let stage_module_dir = stage.path().strip_prefix(parent).map_err(|error| {
        Error::SourceScan(format!(
            "failed to derive staged cache path for {} {}: {}",
            locked.path, locked.version, error,
        ))
    })?;
    let stage_fs = vo_common::vfs::RealFs::new(parent);
    crate::cache::validate::validate_installed_module(&stage_fs, stage_module_dir, locked)
        .map_err(installed_module_error_to_cache_error)?;

    if is_source_cached(cache_root, locked) {
        return Ok(());
    }

    match std::fs::rename(stage.path(), &dir) {
        Ok(()) => {}
        Err(rename_error) => {
            if is_source_cached(cache_root, locked) {
                return Ok(());
            }
            if dir.exists() {
                std::fs::remove_dir_all(&dir)?;
                std::fs::rename(stage.path(), &dir)?;
            } else {
                return Err(Error::Io(rename_error));
            }
        }
    }

    validate_source_cache_entry(cache_root, locked)
}

/// Download and verify a target-specific artifact into the cache.
fn download_artifact(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
    registry: &dyn Registry,
) -> Result<(), Error> {
    if is_artifact_cached(cache_root, locked, artifact) {
        return Ok(());
    }

    let data = registry.fetch_artifact(&locked.path, &locked.version, &artifact.id.name)?;

    verify_size_and_digest(
        &data,
        artifact.size,
        &artifact.digest,
        format!(
            "artifact {} for {} {}",
            artifact.id.name, locked.path, locked.version
        ),
    )?;

    let dir = crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
    let art_dir = dir.join("artifacts");
    let artifact_path = art_dir.join(&artifact.id.name);
    atomic_write_bytes(&artifact_path, &data)?;

    validate_artifact_cache_entry(cache_root, locked, artifact)
}

/// Verify that all locked modules and their required artifacts are present in the cache
/// with correct digests. This is the frozen-build verification step.
///
/// Returns an error describing the first missing or invalid entry.
pub fn verify_locked_cache(cache_root: &Path, lock_file: &LockFile) -> Result<(), Error> {
    for locked in &lock_file.resolved {
        validate_source_cache_entry(cache_root, locked)?;

        for artifact in &locked.artifacts {
            validate_artifact_cache_entry(cache_root, locked, artifact)?;
        }
    }
    Ok(())
}

/// A single unit of download work identified during the planning phase.
struct DownloadJob<'a> {
    locked: &'a LockedModule,
    source_asset: Option<String>,
    manifest_raw: Option<Vec<u8>>,
    artifacts: Vec<&'a LockedArtifact>,
}

#[derive(Debug, Clone)]
pub struct ExactInstallResult {
    pub cache_dir: PathBuf,
    pub locked: LockedModule,
}

pub fn install_exact_module(
    cache_root: &Path,
    registry: &dyn Registry,
    module: &ModulePath,
    version: &ExactVersion,
    created_by: &str,
) -> Result<ExactInstallResult, Error> {
    let (manifest, manifest_raw) = registry.fetch_manifest_raw(module, version)?;
    let locked = locked_module_from_manifest_raw(&manifest, &manifest_raw);
    let lock_file = LockFile {
        version: 1,
        created_by: created_by.to_string(),
        root: LockRoot {
            module: module.clone().into(),
            vo: manifest.vo.clone(),
        },
        resolved: vec![locked.clone()],
    };
    populate_locked_cache(cache_root, &lock_file, registry)?;
    Ok(ExactInstallResult {
        cache_dir: crate::cache::layout::cache_dir(cache_root, module, version),
        locked,
    })
}

/// Download all missing source packages and artifacts for the locked graph.
///
/// Phase 1 (sequential): fetch manifests (uses registry cache) and determine
/// which source packages and artifacts need downloading.
/// Phase 2 (parallel on native, sequential on wasm): download all identified
/// items concurrently with progress output to stderr.
pub fn populate_locked_cache(
    cache_root: &Path,
    lock_file: &LockFile,
    registry: &dyn Registry,
) -> Result<(), Error> {
    // Phase 1: plan downloads (sequential — manifests may use the registry cache).
    let mut jobs: Vec<DownloadJob> = Vec::new();
    for locked in &lock_file.resolved {
        let needs_source = !is_source_cached(cache_root, locked);
        let missing_artifacts: Vec<&LockedArtifact> = locked
            .artifacts
            .iter()
            .filter(|a| !is_artifact_cached(cache_root, locked, a))
            .collect();

        if !needs_source && missing_artifacts.is_empty() {
            continue;
        }

        let (source_asset, manifest_raw) = if needs_source {
            let (manifest, raw) = registry.fetch_manifest_raw(&locked.path, &locked.version)?;
            verify_digest(
                &raw,
                &locked.release_manifest,
                format!("release manifest for {} {}", locked.path, locked.version),
            )?;
            crate::lock::validate_locked_module_against_manifest(
                locked,
                &manifest,
                &crate::digest::Digest::from_sha256(&raw),
            )?;
            (Some(manifest.source.name.clone()), Some(raw))
        } else {
            (None, None)
        };

        jobs.push(DownloadJob {
            locked,
            source_asset,
            manifest_raw,
            artifacts: missing_artifacts,
        });
    }

    if jobs.is_empty() {
        return Ok(());
    }

    let total = jobs.len();
    eprintln!("downloading {} module(s)...", total);

    // Phase 2: execute downloads.
    #[cfg(not(target_arch = "wasm32"))]
    {
        download_jobs_parallel(cache_root, &jobs, registry, total)?;
    }
    #[cfg(target_arch = "wasm32")]
    {
        download_jobs_sequential(cache_root, &jobs, registry, total)?;
    }

    Ok(())
}

/// Sequential download fallback (used on wasm).
#[cfg(target_arch = "wasm32")]
fn download_jobs_sequential(
    cache_root: &Path,
    jobs: &[DownloadJob],
    registry: &dyn Registry,
    total: usize,
) -> Result<(), Error> {
    for (i, job) in jobs.iter().enumerate() {
        eprintln!(
            "  [{}/{}] {} {}",
            i + 1,
            total,
            job.locked.path,
            job.locked.version
        );
        if let (Some(asset_name), Some(raw)) =
            (job.source_asset.as_deref(), job.manifest_raw.as_deref())
        {
            download_source(cache_root, job.locked, registry, asset_name, raw)?;
        }
        for artifact in &job.artifacts {
            download_artifact(cache_root, job.locked, artifact, registry)?;
        }
    }
    Ok(())
}

/// Parallel download using std::thread::scope (native only).
#[cfg(not(target_arch = "wasm32"))]
fn download_jobs_parallel(
    cache_root: &Path,
    jobs: &[DownloadJob],
    registry: &dyn Registry,
    total: usize,
) -> Result<(), Error> {
    use std::sync::atomic::{AtomicUsize, Ordering};

    let counter = AtomicUsize::new(0);
    let first_error: std::sync::Mutex<Option<Error>> = std::sync::Mutex::new(None);
    let max_threads = 8usize.min(jobs.len());

    let counter_ref = &counter;
    let error_ref = &first_error;

    std::thread::scope(|s| {
        let chunk_size = jobs.len().div_ceil(max_threads);
        for chunk in jobs.chunks(chunk_size) {
            s.spawn(move || {
                for job in chunk {
                    if error_ref.lock().unwrap().is_some() {
                        return;
                    }

                    let idx = counter_ref.fetch_add(1, Ordering::Relaxed) + 1;
                    eprintln!(
                        "  [{}/{}] {} {}",
                        idx, total, job.locked.path, job.locked.version
                    );

                    if let (Some(asset_name), Some(raw)) =
                        (job.source_asset.as_deref(), job.manifest_raw.as_deref())
                    {
                        if let Err(e) =
                            download_source(cache_root, job.locked, registry, asset_name, raw)
                        {
                            *error_ref.lock().unwrap() = Some(e);
                            return;
                        }
                    }
                    for artifact in &job.artifacts {
                        if error_ref.lock().unwrap().is_some() {
                            return;
                        }
                        if let Err(e) =
                            download_artifact(cache_root, job.locked, artifact, registry)
                        {
                            *error_ref.lock().unwrap() = Some(e);
                            return;
                        }
                    }
                }
            });
        }
    });

    if let Some(e) = first_error.into_inner().unwrap() {
        return Err(e);
    }
    Ok(())
}

fn validate_source_cache_entry(cache_root: &Path, locked: &LockedModule) -> Result<(), Error> {
    let module_dir =
        crate::cache::layout::relative_module_dir(locked.path.as_str(), &locked.version);
    let fs = vo_common::vfs::RealFs::new(cache_root);
    crate::cache::validate::validate_installed_module(&fs, &module_dir, locked)
        .map_err(installed_module_error_to_cache_error)
}

fn installed_module_error_to_cache_error(
    error: crate::cache::validate::InstalledModuleError,
) -> Error {
    use crate::cache::validate::{InstalledModuleErrorKind, InstalledModuleField};

    let crate::cache::validate::InstalledModuleError {
        module,
        version,
        field,
        kind,
    } = error;
    match (field, *kind) {
        (
            InstalledModuleField::ReleaseManifest,
            InstalledModuleErrorKind::LockedModuleMismatch {
                field,
                expected,
                found,
            },
        ) => Error::LockedModuleMismatch {
            module,
            field,
            expected,
            found,
        },
        (
            InstalledModuleField::ReleaseManifest,
            InstalledModuleErrorKind::Mismatch { expected, found },
        ) => Error::LockedModuleMismatch {
            module,
            field: "release_manifest".to_string(),
            expected,
            found,
        },
        (
            InstalledModuleField::ReleaseManifest,
            InstalledModuleErrorKind::ParseFailed { detail },
        ) => Error::ManifestParse(detail),
        (
            InstalledModuleField::ReleaseManifest,
            InstalledModuleErrorKind::ValidationFailed { detail },
        ) => Error::InvalidReleaseMetadata(detail),
        (InstalledModuleField::ExtManifest, InstalledModuleErrorKind::ParseFailed { detail }) => {
            Error::ExtManifestParse(detail)
        }
        (
            InstalledModuleField::ExtManifest,
            InstalledModuleErrorKind::ValidationFailed { detail },
        ) => Error::InvalidReleaseMetadata(detail),
        (_, kind) => Error::MissingArtifact {
            module,
            version,
            detail: kind.to_string(),
        },
    }
}

fn validate_artifact_cache_entry(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
) -> Result<(), Error> {
    let module_dir =
        crate::cache::layout::relative_module_dir(locked.path.as_str(), &locked.version);
    let artifact_path = module_dir.join("artifacts").join(&artifact.id.name);
    let fs = vo_common::vfs::RealFs::new(cache_root);
    crate::cache::validate::validate_installed_artifact(&fs, &artifact_path, locked, artifact)
        .map_err(|e| Error::MissingArtifact {
            module: e.module,
            version: e.version,
            detail: format!("{}", e.kind),
        })
}

/// Safely unpack a tar.gz archive into `dest`, rejecting entries with
/// path-traversal components (`..`), absolute paths, or symlinks.
fn safe_unpack_tar_gz(data: &[u8], dest: &Path) -> Result<(), String> {
    for entry in read_archive_entries(data)? {
        match entry {
            ArchiveEntry::Directory(relative_path) => {
                std::fs::create_dir_all(dest.join(relative_path)).map_err(|e| e.to_string())?;
            }
            ArchiveEntry::File(relative_path, bytes) => {
                let full_path = dest.join(relative_path);
                if let Some(parent) = full_path.parent() {
                    std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
                }
                std::fs::write(&full_path, &bytes).map_err(|e| e.to_string())?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

    use crate::digest::Digest;
    use crate::identity::ArtifactId;
    use crate::identity::ModulePath;
    use crate::schema::lockfile::LockedArtifact;
    use crate::schema::manifest::{ManifestSource, ReleaseManifest};
    use crate::version::ExactVersion;
    use crate::version::ToolchainConstraint;

    struct CountingRegistry {
        source_fetches: AtomicUsize,
        source_bytes: Vec<u8>,
    }

    impl CountingRegistry {
        fn new(source_bytes: Vec<u8>) -> Self {
            Self {
                source_fetches: AtomicUsize::new(0),
                source_bytes,
            }
        }
    }

    impl Registry for CountingRegistry {
        fn list_versions(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
            Ok(Vec::new())
        }

        fn fetch_manifest(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
        ) -> Result<crate::schema::manifest::ReleaseManifest, Error> {
            Err(Error::RegistryError("unused in test".to_string()))
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            self.source_fetches.fetch_add(1, AtomicOrdering::Relaxed);
            Ok(self.source_bytes.clone())
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("unused in test".to_string()))
        }
    }

    fn test_locked_module(
        module: &str,
        version: &str,
        source_raw: &[u8],
    ) -> (LockedModule, Vec<u8>) {
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: ModulePath::parse(module).unwrap(),
            version: ExactVersion::parse(version).unwrap(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            module_root: ModulePath::parse(module).unwrap().module_root().to_string(),
            vo: ToolchainConstraint::parse("0.1.0").unwrap(),
            require: Vec::new(),
            source: ManifestSource {
                name: format!("{}-source.tar.gz", version),
                size: source_raw.len() as u64,
                digest: Digest::from_sha256(source_raw),
            },
            artifacts: Vec::new(),
        };
        let manifest_raw = format!("{}\n", manifest.render()).into_bytes();
        let locked = crate::lock::locked_module_from_manifest_raw(&manifest, &manifest_raw);
        (locked, manifest_raw)
    }

    fn write_cached_source(module_dir: &Path, locked: &LockedModule, manifest_raw: &[u8]) {
        std::fs::create_dir_all(module_dir).unwrap();
        std::fs::write(
            module_dir.join("vo.mod"),
            format!("module {}\nvo {}\n", locked.path, locked.vo),
        )
        .unwrap();
        std::fs::write(
            module_dir.join(VERSION_MARKER),
            format!("{}\n", locked.version),
        )
        .unwrap();
        std::fs::write(
            module_dir.join(SOURCE_DIGEST_MARKER),
            format!("{}\n", locked.source),
        )
        .unwrap();
        std::fs::write(module_dir.join("vo.release.json"), manifest_raw).unwrap();
    }

    fn build_source_archive(root: &str, files: &[(&str, &str)]) -> Vec<u8> {
        let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
        let mut builder = tar::Builder::new(encoder);
        for (relative_path, content) in files {
            let full_path = format!("{root}/{relative_path}");
            let bytes = content.as_bytes();
            let mut header = tar::Header::new_gnu();
            header.set_size(bytes.len() as u64);
            header.set_mode(0o644);
            header.set_cksum();
            builder
                .append_data(&mut header, full_path, Cursor::new(bytes))
                .unwrap();
        }
        builder.into_inner().unwrap().finish().unwrap()
    }

    #[test]
    fn test_extract_source_entries_keeps_declared_include_files() {
        let archive = build_source_archive(
            "demo-v1.0.0",
            &[
                ("vo.mod", "module github.com/acme/demo\nvo 0.1.0\n"),
                (
                    "vo.ext.toml",
                    concat!(
                        "[extension]\n",
                        "name = \"demo\"\n",
                        "include = [\"js/dist/studio_renderer.js\"]\n\n",
                        "[extension.native]\n",
                        "path = \"rust/target/{profile}/libdemo\"\n\n",
                        "[[extension.native.targets]]\n",
                        "target = \"aarch64-apple-darwin\"\n",
                        "library = \"libdemo.dylib\"\n",
                    ),
                ),
                ("main.vo", "package main\nfunc main() {}\n"),
                ("js/dist/studio_renderer.js", "export const renderer = 1;\n"),
                ("js/dist/ignored.js", "export const ignored = 1;\n"),
            ],
        );

        let entries = extract_source_entries(&archive).unwrap();
        let paths = entries
            .into_iter()
            .map(|(path, _)| path)
            .collect::<Vec<_>>();

        assert!(paths.contains(&PathBuf::from("vo.mod")));
        assert!(paths.contains(&PathBuf::from("vo.ext.toml")));
        assert!(paths.contains(&PathBuf::from("main.vo")));
        assert!(paths.contains(&PathBuf::from("js/dist/studio_renderer.js")));
        assert!(!paths.contains(&PathBuf::from("js/dist/ignored.js")));
    }

    #[test]
    fn test_extract_source_entries_keeps_declared_include_directories() {
        let archive = build_source_archive(
            "demo-v1.0.0",
            &[
                ("vo.mod", "module github.com/acme/demo\nvo 0.1.0\n"),
                (
                    "vo.ext.toml",
                    concat!(
                        "[extension]\n",
                        "name = \"demo\"\n",
                        "include = [\"js/dist\"]\n\n",
                        "[extension.native]\n",
                        "path = \"rust/target/{profile}/libdemo\"\n\n",
                        "[[extension.native.targets]]\n",
                        "target = \"aarch64-apple-darwin\"\n",
                        "library = \"libdemo.dylib\"\n",
                    ),
                ),
                ("main.vo", "package main\nfunc main() {}\n"),
                (
                    "js/dist/voplay-render-island.js",
                    "export { bootstrapWebView } from './bootstrap_webview.js';\n",
                ),
                (
                    "js/dist/bootstrap_webview.js",
                    "export const bootstrapWebView = 1;\n",
                ),
                ("js/dist/nested/helper.js", "export const helper = 1;\n"),
            ],
        );

        let entries = extract_source_entries(&archive).unwrap();
        let paths = entries
            .into_iter()
            .map(|(path, _)| path)
            .collect::<Vec<_>>();

        assert!(paths.contains(&PathBuf::from("vo.mod")));
        assert!(paths.contains(&PathBuf::from("vo.ext.toml")));
        assert!(paths.contains(&PathBuf::from("main.vo")));
        assert!(paths.contains(&PathBuf::from("js/dist/voplay-render-island.js")));
        assert!(paths.contains(&PathBuf::from("js/dist/bootstrap_webview.js")));
        assert!(paths.contains(&PathBuf::from("js/dist/nested/helper.js")));
    }

    struct ExactInstallRegistry {
        manifest: ReleaseManifest,
        source_bytes: Vec<u8>,
        source_fetches: AtomicUsize,
    }

    impl Registry for ExactInstallRegistry {
        fn list_versions(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
            Ok(Vec::new())
        }

        fn fetch_manifest(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
        ) -> Result<ReleaseManifest, Error> {
            Ok(self.manifest.clone())
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            self.source_fetches.fetch_add(1, AtomicOrdering::Relaxed);
            Ok(self.source_bytes.clone())
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("unused in test".to_string()))
        }
    }

    #[test]
    fn test_create_unique_stage_dir_returns_distinct_paths() {
        let temp = tempfile::tempdir().unwrap();
        let first = create_unique_stage_dir(temp.path(), "stage").unwrap();
        let second = create_unique_stage_dir(temp.path(), "stage").unwrap();

        assert_ne!(first.path(), second.path());
        assert!(first.path().is_dir());
        assert!(second.path().is_dir());
    }

    #[test]
    fn test_atomic_write_bytes_replaces_existing_file() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("artifact.bin");

        std::fs::write(&path, b"old-bytes").unwrap();
        atomic_write_bytes(&path, b"new-bytes").unwrap();

        assert_eq!(std::fs::read(&path).unwrap(), b"new-bytes");
    }

    #[test]
    fn test_download_source_skips_fetch_when_cache_is_already_valid() {
        let temp = tempfile::tempdir().unwrap();
        let source_raw = b"source-package";
        let (locked, manifest_raw) =
            test_locked_module("github.com/acme/lib", "v1.0.0", source_raw);
        let module_dir =
            crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
        write_cached_source(&module_dir, &locked, &manifest_raw);
        let registry = CountingRegistry::new(b"unused-source".to_vec());

        download_source(
            temp.path(),
            &locked,
            &registry,
            "source.tar.gz",
            &manifest_raw,
        )
        .unwrap();

        assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 0);
    }

    #[test]
    fn test_download_artifact_replaces_invalid_existing_cache_file() {
        let temp = tempfile::tempdir().unwrap();
        let source_raw = b"source-package";
        let (mut locked, manifest_raw) =
            test_locked_module("github.com/acme/lib", "v1.0.0", source_raw);
        let module_dir =
            crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
        write_cached_source(&module_dir, &locked, &manifest_raw);

        let artifact_bytes = b"fresh-artifact";
        let artifact = LockedArtifact {
            id: ArtifactId {
                kind: "extension-native".to_string(),
                target: "aarch64-apple-darwin".to_string(),
                name: "libdemo.dylib".to_string(),
            },
            size: artifact_bytes.len() as u64,
            digest: Digest::from_sha256(artifact_bytes),
        };
        locked.artifacts.push(artifact.clone());

        let art_path = module_dir.join("artifacts").join(&artifact.id.name);
        std::fs::create_dir_all(art_path.parent().unwrap()).unwrap();
        std::fs::write(&art_path, b"stale").unwrap();

        struct ArtifactRegistry {
            bytes: Vec<u8>,
        }

        impl Registry for ArtifactRegistry {
            fn list_versions(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
                Ok(Vec::new())
            }

            fn fetch_manifest(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
            ) -> Result<crate::schema::manifest::ReleaseManifest, Error> {
                Err(Error::RegistryError("unused in test".to_string()))
            }

            fn fetch_source_package(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
                _asset_name: &str,
            ) -> Result<Vec<u8>, Error> {
                Err(Error::RegistryError("unused in test".to_string()))
            }

            fn fetch_artifact(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
                _asset_name: &str,
            ) -> Result<Vec<u8>, Error> {
                Ok(self.bytes.clone())
            }
        }

        let registry = ArtifactRegistry {
            bytes: artifact_bytes.to_vec(),
        };
        download_artifact(temp.path(), &locked, &artifact, &registry).unwrap();

        assert_eq!(std::fs::read(&art_path).unwrap(), artifact_bytes);
    }

    #[test]
    fn test_validate_source_cache_entry_preserves_locked_module_mismatch() {
        let temp = tempfile::tempdir().unwrap();
        let source_raw = b"source-package";
        let (mut locked, manifest_raw) =
            test_locked_module("github.com/acme/lib", "v1.0.0", source_raw);
        let module_dir =
            crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
        write_cached_source(&module_dir, &locked, &manifest_raw);

        locked.artifacts.push(LockedArtifact {
            id: ArtifactId {
                kind: "extension-native".to_string(),
                target: "aarch64-apple-darwin".to_string(),
                name: "libdemo.dylib".to_string(),
            },
            size: 5,
            digest: Digest::parse(
                "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
            )
            .unwrap(),
        });

        let error = validate_source_cache_entry(temp.path(), &locked).unwrap_err();

        assert!(matches!(
            error,
            Error::LockedModuleMismatch { ref field, .. } if field == "artifacts"
        ));
    }

    #[test]
    fn test_install_exact_module_rejects_packaged_ext_contract_mismatch() {
        let temp = tempfile::tempdir().unwrap();
        let source_bytes = build_source_archive(
            "lib-v1.2.3",
            &[
                ("vo.mod", "module github.com/acme/lib\nvo ^0.1.0\n"),
                (
                    "vo.ext.toml",
                    concat!(
                        "[extension]\n",
                        "name = \"lib\"\n\n",
                        "[extension.wasm]\n",
                        "type = \"bindgen\"\n",
                        "wasm = \"lib.wasm\"\n",
                        "js_glue = \"lib.js\"\n",
                    ),
                ),
                ("lib.vo", "package lib\nfunc Hello() {}\n"),
            ],
        );
        let source_digest = Digest::from_sha256(&source_bytes);
        let manifest = ReleaseManifest::parse(&format!(
            r#"{{
  "schema_version": 1,
  "module": "github.com/acme/lib",
  "version": "v1.2.3",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "module_root": ".",
  "vo": "^0.1.0",
  "require": [],
  "source": {{
    "name": "lib-v1.2.3-source.tar.gz",
    "size": {},
    "digest": "{}"
  }},
  "artifacts": [
    {{
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "lib.wasm",
      "size": 4,
      "digest": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    }}
  ]
}}"#,
            source_bytes.len(),
            source_digest,
        ))
        .unwrap();
        let registry = ExactInstallRegistry {
            manifest,
            source_bytes,
            source_fetches: AtomicUsize::new(0),
        };
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("v1.2.3").unwrap();

        let error =
            install_exact_module(temp.path(), &registry, &module, &version, "vo test").unwrap_err();

        assert!(matches!(error, Error::InvalidReleaseMetadata(_)));
    }

    #[test]
    fn test_install_exact_module_populates_cache_and_returns_locked_metadata() {
        let temp = tempfile::tempdir().unwrap();
        let source_bytes = build_source_archive(
            "lib-v1.2.3",
            &[
                ("vo.mod", "module github.com/acme/lib\nvo ^0.1.0\n"),
                ("lib.vo", "package lib\nfunc Hello() {}\n"),
            ],
        );
        let source_digest = Digest::from_sha256(&source_bytes);
        let manifest = ReleaseManifest::parse(&format!(
            r#"{{
  "schema_version": 1,
  "module": "github.com/acme/lib",
  "version": "v1.2.3",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "module_root": ".",
  "vo": "^0.1.0",
  "require": [],
  "source": {{
    "name": "lib-v1.2.3-source.tar.gz",
    "size": {},
    "digest": "{}"
  }},
  "artifacts": []
}}"#,
            source_bytes.len(),
            source_digest
        ))
        .unwrap();
        let registry = ExactInstallRegistry {
            manifest: manifest.clone(),
            source_bytes: source_bytes.clone(),
            source_fetches: AtomicUsize::new(0),
        };
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("v1.2.3").unwrap();

        let installed =
            install_exact_module(temp.path(), &registry, &module, &version, "vo test").unwrap();

        assert_eq!(
            installed.cache_dir,
            crate::cache::layout::cache_dir(temp.path(), &module, &version)
        );
        assert_eq!(installed.locked.path, module);
        assert_eq!(installed.locked.version, version);
        assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 1);
        assert_eq!(
            std::fs::read_to_string(installed.cache_dir.join("vo.mod")).unwrap(),
            "module github.com/acme/lib\nvo ^0.1.0\n"
        );
        assert_eq!(
            std::fs::read_to_string(installed.cache_dir.join(VERSION_MARKER))
                .unwrap()
                .trim(),
            "v1.2.3"
        );
        assert_eq!(
            std::fs::read_to_string(installed.cache_dir.join(SOURCE_DIGEST_MARKER))
                .unwrap()
                .trim(),
            source_digest.to_string()
        );
    }
}
