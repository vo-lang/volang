use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::registry::Registry;
use crate::schema::lockfile::{LockFile, LockedArtifact, LockedModule};
use crate::version::ExactVersion;
use crate::Error;

// ── Shared cache layout (platform-agnostic) ──────────────────────────────────

/// Metadata file name for the cached source digest marker.
pub const SOURCE_DIGEST_MARKER: &str = ".vo-source-digest";

/// Metadata file name for the cached version marker.
pub const VERSION_MARKER: &str = ".vo-version";

/// Encode a module path as a flat directory name.
///
/// Replaces `/` with `@` (forbidden in module-path segments) so that
/// different module paths never collide in a flat directory listing.
///
/// Example: `github.com/acme/lib` → `github.com@acme@lib`
pub fn cache_key(module: &str) -> String {
    module.replace('/', "@")
}

/// Relative cache directory path for a module version (no root prefix).
///
/// Returns `"<encoded_module>/<version>"`, suitable for joining with an
/// OS cache root or prepending with `/` for a VFS root.
pub fn relative_module_dir(module: &str, version: &str) -> String {
    format!("{}/{}", cache_key(module), version)
}

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

fn create_unique_temp_file(parent: &Path, stem: &str) -> Result<(std::fs::File, TempPathGuard), Error> {
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
    let parent = path.parent().ok_or_else(|| {
        Error::SourceScan(format!("path has no parent: {}", path.display()))
    })?;
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

/// A single file entry extracted from a release source package archive.
pub struct SourceEntry {
    /// Path relative to the module root (e.g. `src/main.vo`, `vo.mod`).
    pub relative_path: PathBuf,
    /// UTF-8 file content.
    pub content: String,
}

/// Extract source entries from a tar.gz release source package.
///
/// Strips the top-level archive directory (standard release convention),
/// filters to allowed file types (`.vo`, `vo.mod`, `vo.lock`, `vo.ext.toml`),
/// and skips non-UTF-8 entries.
///
/// Returns entries with paths relative to the module root.
pub fn extract_source_entries(archive_bytes: &[u8]) -> Result<Vec<SourceEntry>, String> {
    use std::io::Read;

    let gz = flate2::read::GzDecoder::new(archive_bytes);
    let mut archive = tar::Archive::new(gz);
    let mut archive_root: Option<String> = None;
    let mut entries = Vec::new();

    for entry in archive.entries().map_err(|e| e.to_string())? {
        let mut entry = entry.map_err(|e| e.to_string())?;
        if !entry.header().entry_type().is_file() {
            continue;
        }
        let raw_path = entry.path().map_err(|e| e.to_string())?.into_owned();
        let Some(stripped) = strip_archive_root(&raw_path, &mut archive_root)? else {
            continue;
        };
        if !source_entry_allowed(&stripped) {
            continue;
        }
        let mut buf = Vec::new();
        entry.read_to_end(&mut buf).map_err(|e| e.to_string())?;
        let content = match String::from_utf8(buf) {
            Ok(c) => c,
            Err(_) => continue,
        };
        entries.push(SourceEntry {
            relative_path: stripped,
            content,
        });
    }

    if archive_root.is_none() {
        return Err("source package archive is empty".to_string());
    }

    Ok(entries)
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

fn source_entry_allowed(path: &Path) -> bool {
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    let path_str = path.to_string_lossy();
    path_str.ends_with(".vo") || name == "vo.mod" || name == "vo.lock" || name == "vo.ext.toml"
}

/// Cache directory layout helper.
/// Cache key: `<cache_root>/<module_path_encoded>/<version>/`
///
/// The module path is encoded by replacing `/` with `@` (which is forbidden
/// in module-path segments) so that different paths never collide.
/// For example `github.com/acme/lib` → `github.com@acme@lib`.
pub fn cache_dir(cache_root: &Path, module: &ModulePath, version: &ExactVersion) -> PathBuf {
    cache_root
        .join(cache_key(module.as_str()))
        .join(version.to_string())
}

/// Relative cache directory for a module (no cache_root prefix).
/// Useful as a VFS module root key.
pub fn cache_relative_dir(module: &ModulePath, version: &ExactVersion) -> PathBuf {
    PathBuf::from(cache_key(module.as_str())).join(version.to_string())
}

/// Check if a source package is already cached and valid.
pub fn is_source_cached(cache_root: &Path, locked: &LockedModule) -> bool {
    validate_source_cache_entry(cache_root, locked).is_ok()
}

/// Check if a specific artifact is cached and valid.
pub fn is_artifact_cached(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
) -> bool {
    validate_artifact_cache_entry(cache_root, locked, artifact).is_ok()
}

/// Download and verify a source package into the cache.
pub fn download_source(
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

    // Verify digest
    let computed = Digest::from_sha256(&data);
    if computed != locked.source {
        return Err(Error::DigestMismatch {
            context: format!("source package for {} {}", locked.path, locked.version),
            expected: locked.source.as_str().to_string(),
            found: computed.as_str().to_string(),
        });
    }

    let dir = cache_dir(cache_root, &locked.path, &locked.version);
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
pub fn download_artifact(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
    registry: &dyn Registry,
) -> Result<(), Error> {
    if is_artifact_cached(cache_root, locked, artifact) {
        return Ok(());
    }

    let data = registry.fetch_artifact(&locked.path, &locked.version, &artifact.id.name)?;

    // Verify size
    if data.len() as u64 != artifact.size {
        return Err(Error::DigestMismatch {
            context: format!(
                "artifact {} for {} {}: size mismatch",
                artifact.id.name, locked.path, locked.version
            ),
            expected: format!("{} bytes", artifact.size),
            found: format!("{} bytes", data.len()),
        });
    }

    // Verify digest
    let computed = Digest::from_sha256(&data);
    if computed != artifact.digest {
        return Err(Error::DigestMismatch {
            context: format!(
                "artifact {} for {} {}",
                artifact.id.name, locked.path, locked.version
            ),
            expected: artifact.digest.as_str().to_string(),
            found: computed.as_str().to_string(),
        });
    }

    let dir = cache_dir(cache_root, &locked.path, &locked.version);
    let art_dir = dir.join("artifacts");
    let artifact_path = art_dir.join(&artifact.id.name);
    atomic_write_bytes(&artifact_path, &data)?;

    validate_artifact_cache_entry(cache_root, locked, artifact)
}

/// Verify that all locked modules and their required artifacts are present in the cache
/// with correct digests. This is the frozen-build verification step.
///
/// Returns an error describing the first missing or invalid entry.
pub fn verify_frozen_cache(cache_root: &Path, lock_file: &LockFile) -> Result<(), Error> {
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

/// Download all missing source packages and artifacts for the locked graph.
///
/// Phase 1 (sequential): fetch manifests (uses registry cache) and determine
/// which source packages and artifacts need downloading.
/// Phase 2 (parallel on native, sequential on wasm): download all identified
/// items concurrently with progress output to stderr.
pub fn download_all(
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
            let computed = Digest::from_sha256(&raw);
            if computed != locked.release_manifest {
                return Err(Error::DigestMismatch {
                    context: format!("release manifest for {} {}", locked.path, locked.version),
                    expected: locked.release_manifest.as_str().to_string(),
                    found: computed.as_str().to_string(),
                });
            }
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
    let dir = cache_dir(cache_root, &locked.path, &locked.version);
    if !dir.is_dir() {
        return Err(Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: "source package not in cache".to_string(),
        });
    }

    let version =
        read_trimmed_metadata(&dir.join(VERSION_MARKER)).ok_or_else(|| Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: "cached version metadata is missing".to_string(),
        })?;
    if version != locked.version.to_string() {
        return Err(Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: "cached version metadata does not match vo.lock".to_string(),
        });
    }

    let source_digest =
        read_trimmed_metadata(&dir.join(SOURCE_DIGEST_MARKER)).ok_or_else(|| {
            Error::MissingArtifact {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                detail: "cached source digest metadata is missing".to_string(),
            }
        })?;
    if source_digest != locked.source.as_str() {
        return Err(Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: "cached source digest does not match vo.lock".to_string(),
        });
    }

    if !dir.join("vo.mod").is_file() {
        return Err(Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: "cached module is missing vo.mod".to_string(),
        });
    }

    let manifest_path = dir.join("vo.release.json");
    let manifest_raw = std::fs::read(&manifest_path).map_err(|_| Error::MissingArtifact {
        module: locked.path.as_str().to_string(),
        version: locked.version.to_string(),
        detail: "cached module is missing vo.release.json".to_string(),
    })?;
    let manifest_digest = Digest::from_sha256(&manifest_raw);
    if manifest_digest != locked.release_manifest {
        return Err(Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: "cached release manifest does not match vo.lock".to_string(),
        });
    }

    Ok(())
}

fn validate_artifact_cache_entry(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
) -> Result<(), Error> {
    let path = cache_dir(cache_root, &locked.path, &locked.version)
        .join("artifacts")
        .join(&artifact.id.name);
    let bytes = std::fs::read(&path).map_err(|_| Error::MissingArtifact {
        module: locked.path.as_str().to_string(),
        version: locked.version.to_string(),
        detail: format!("artifact {} not in cache", artifact.id.name),
    })?;
    if bytes.len() as u64 != artifact.size {
        return Err(Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: format!("artifact {} size does not match vo.lock", artifact.id.name),
        });
    }
    let digest = Digest::from_sha256(&bytes);
    if digest != artifact.digest {
        return Err(Error::MissingArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: format!(
                "artifact {} digest does not match vo.lock",
                artifact.id.name
            ),
        });
    }
    Ok(())
}

fn read_trimmed_metadata(path: &Path) -> Option<String> {
    std::fs::read_to_string(path)
        .ok()
        .map(|value| value.trim().to_string())
}

/// Safely unpack a tar.gz archive into `dest`, rejecting entries with
/// path-traversal components (`..`), absolute paths, or symlinks.
fn safe_unpack_tar_gz(data: &[u8], dest: &Path) -> Result<(), String> {
    use std::io::Read;

    let gz = flate2::read::GzDecoder::new(data);
    let mut archive = tar::Archive::new(gz);
    let mut archive_root: Option<String> = None;

    for entry in archive.entries().map_err(|e| e.to_string())? {
        let mut entry = entry.map_err(|e| e.to_string())?;
        let header = entry.header();

        // Reject symlinks and hardlinks.
        let entry_type = header.entry_type();
        if entry_type.is_symlink() || entry_type.is_hard_link() {
            return Err("archive contains a symlink or hardlink, which is not allowed".to_string());
        }

        let raw_path = entry.path().map_err(|e| e.to_string())?.into_owned();
        let Some(relative_path) = strip_archive_root(&raw_path, &mut archive_root)? else {
            continue;
        };

        for component in relative_path.components() {
            if !matches!(component, std::path::Component::Normal(_)) {
                return Err(format!(
                    "archive entry contains invalid path component: {}",
                    relative_path.display()
                ));
            }
        }

        let full_path = dest.join(&relative_path);

        if !full_path.starts_with(dest) {
            return Err(format!(
                "archive entry escapes destination: {}",
                relative_path.display()
            ));
        }

        if entry_type.is_dir() {
            std::fs::create_dir_all(&full_path).map_err(|e| e.to_string())?;
        } else if entry_type.is_file() {
            if let Some(parent) = full_path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
            }
            let mut buf = Vec::new();
            entry.read_to_end(&mut buf).map_err(|e| e.to_string())?;
            std::fs::write(&full_path, &buf).map_err(|e| e.to_string())?;
        }
        // Skip other entry types (device nodes, etc.)
    }

    if archive_root.is_none() {
        return Err("source package archive is empty".to_string());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

    use crate::identity::ArtifactId;
    use crate::schema::lockfile::LockedArtifact;
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

    fn test_locked_module(module: &str, version: &str, manifest_raw: &[u8], source_digest: &Digest) -> LockedModule {
        LockedModule {
            path: ModulePath::parse(module).unwrap(),
            version: ExactVersion::parse(version).unwrap(),
            vo: ToolchainConstraint::parse("0.1.0").unwrap(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            release_manifest: Digest::from_sha256(manifest_raw),
            source: source_digest.clone(),
            deps: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    fn write_cached_source(module_dir: &Path, locked: &LockedModule, manifest_raw: &[u8]) {
        std::fs::create_dir_all(module_dir).unwrap();
        std::fs::write(
            module_dir.join("vo.mod"),
            format!("module {}\nvo 0.1.0\n", locked.path),
        )
        .unwrap();
        std::fs::write(module_dir.join(VERSION_MARKER), format!("{}\n", locked.version)).unwrap();
        std::fs::write(
            module_dir.join(SOURCE_DIGEST_MARKER),
            format!("{}\n", locked.source),
        )
        .unwrap();
        std::fs::write(module_dir.join("vo.release.json"), manifest_raw).unwrap();
    }

    #[test]
    fn test_cache_dir() {
        let root = Path::new("/tmp/vo-cache");
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let v = ExactVersion::parse("v1.2.3").unwrap();
        let dir = cache_dir(root, &mp, &v);
        assert_eq!(
            dir,
            PathBuf::from("/tmp/vo-cache/github.com@acme@lib/v1.2.3")
        );
    }

    #[test]
    fn test_digest_from_sha256() {
        let d = Digest::from_sha256(b"hello");
        assert_eq!(
            d.as_str(),
            "sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
        );
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
        let manifest_raw = b"{\"module\":\"github.com/acme/lib\"}\n";
        let source_digest = Digest::parse(
            "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        )
        .unwrap();
        let locked = test_locked_module(
            "github.com/acme/lib",
            "v1.0.0",
            manifest_raw,
            &source_digest,
        );
        let module_dir = cache_dir(temp.path(), &locked.path, &locked.version);
        write_cached_source(&module_dir, &locked, manifest_raw);
        let registry = CountingRegistry::new(b"unused-source".to_vec());

        download_source(
            temp.path(),
            &locked,
            &registry,
            "source.tar.gz",
            manifest_raw,
        )
        .unwrap();

        assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 0);
    }

    #[test]
    fn test_download_artifact_replaces_invalid_existing_cache_file() {
        let temp = tempfile::tempdir().unwrap();
        let manifest_raw = b"{\"module\":\"github.com/acme/lib\"}\n";
        let source_digest = Digest::parse(
            "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        )
        .unwrap();
        let mut locked = test_locked_module(
            "github.com/acme/lib",
            "v1.0.0",
            manifest_raw,
            &source_digest,
        );
        let module_dir = cache_dir(temp.path(), &locked.path, &locked.version);
        write_cached_source(&module_dir, &locked, manifest_raw);

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
}
