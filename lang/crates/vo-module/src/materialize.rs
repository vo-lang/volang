use std::path::{Path, PathBuf};

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

    let stage_name = format!(".{}.installing", locked.version);
    let stage_dir = parent.join(stage_name);
    if stage_dir.exists() {
        std::fs::remove_dir_all(&stage_dir)?;
    }
    std::fs::create_dir_all(&stage_dir)?;

    safe_unpack_tar_gz(&data, &stage_dir).map_err(|e| Error::DigestMismatch {
        context: format!("source package for {} {}", locked.path, locked.version),
        expected: "safe archive".to_string(),
        found: e,
    })?;

    if !stage_dir.join("vo.mod").is_file() {
        std::fs::remove_dir_all(&stage_dir)?;
        return Err(Error::SourceScan(format!(
            "source package for {} {} does not contain vo.mod at the module root",
            locked.path, locked.version,
        )));
    }

    // Write metadata files used by compile-time frozen-build validation
    std::fs::write(
        stage_dir.join(SOURCE_DIGEST_MARKER),
        format!("{}\n", locked.source),
    )?;
    std::fs::write(
        stage_dir.join(VERSION_MARKER),
        format!("{}\n", locked.version),
    )?;
    std::fs::write(stage_dir.join("vo.release.json"), manifest_raw)?;

    if dir.exists() {
        std::fs::remove_dir_all(&dir)?;
    }
    std::fs::rename(&stage_dir, &dir)?;

    Ok(())
}

/// Download and verify a target-specific artifact into the cache.
pub fn download_artifact(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
    registry: &dyn Registry,
) -> Result<(), Error> {
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
    std::fs::create_dir_all(&art_dir)?;
    std::fs::write(art_dir.join(&artifact.id.name), &data)?;

    Ok(())
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
}
