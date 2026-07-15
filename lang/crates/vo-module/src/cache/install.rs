use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use vo_common::vfs::FileSystemEntryKind;

use crate::cache::layout::{cache_key, SOURCE_DIGEST_MARKER, VERSION_MARKER};
use crate::digest::{verify_digest, verify_size_and_digest, Digest};
use crate::identity::ModulePath;
use crate::lock::locked_module_from_manifest_raw;
use crate::registry::Registry;
use crate::schema::lockfile::{LockFile, LockedArtifact, LockedModule};
use crate::version::ExactVersion;
use crate::{
    Error, MAX_EXTRACTED_SOURCE_BYTES, MAX_SOURCE_ARCHIVE_ENTRIES, MAX_SOURCE_ARCHIVE_ENTRY_BYTES,
};

// ── Source package extraction (platform-agnostic) ────────────────────────────

/// Extract source entries from a tar.gz release source package.
///
/// Strips the top-level archive directory (standard release convention),
/// parses the embedded `vo.web.json`, and returns exactly the UTF-8 source
/// files declared by its authenticated source set plus `vo.web.json` itself.
/// Binary archive payloads remain available as release inputs but are never
/// materialized into the source cache.
///
/// Returns entries with paths relative to the module root.
pub fn extract_source_entries(archive_bytes: &[u8]) -> Result<Vec<(PathBuf, String)>, String> {
    let web_path = Path::new("vo.web.json");
    let mut web_bytes = None;
    scan_archive_files(archive_bytes, |relative_path, bytes| {
        if relative_path == web_path {
            web_bytes = Some(bytes);
        }
        Ok(())
    })?;
    let web_bytes = web_bytes.ok_or_else(|| {
        "source package does not contain vo.web.json at the module root".to_string()
    })?;
    let web = crate::schema::WebManifest::parse(&web_bytes)
        .map_err(|error| format!("source package vo.web.json is invalid: {error}"))?;

    let declared = web
        .source
        .iter()
        .map(|entry| (PathBuf::from(&entry.path), entry))
        .collect::<BTreeMap<_, _>>();
    let mut source_files = BTreeMap::new();
    scan_archive_files(archive_bytes, |path, bytes| {
        if path == web_path {
            return Ok(());
        }
        let Some(expected) = declared.get(&path) else {
            if std::str::from_utf8(&bytes).is_ok() {
                return Err(format!(
                    "source package contains UTF-8 file {} that is not declared by vo.web.json",
                    path.display(),
                ));
            }
            return Ok(());
        };
        let found_size = u64::try_from(bytes.len()).unwrap_or(u64::MAX);
        let found_digest = crate::digest::Digest::from_sha256(&bytes);
        if found_size != expected.size || found_digest != expected.digest {
            return Err(format!(
                "source package file {:?} does not match vo.web.json: expected {} ({} bytes), found {} ({} bytes)",
                expected.path, expected.digest, expected.size, found_digest, found_size,
            ));
        }
        let content = String::from_utf8(bytes).map_err(|error| {
            format!(
                "source package file {:?} declared by vo.web.json is not valid UTF-8: {}",
                expected.path,
                error.utf8_error(),
            )
        })?;
        source_files.insert(path, content);
        Ok(())
    })?;

    let mut selected = Vec::new();
    selected
        .try_reserve(web.source.len().saturating_add(1))
        .map_err(|_| "failed to reserve source package file set".to_string())?;
    for expected in &web.source {
        let path = PathBuf::from(&expected.path);
        let content = source_files.remove(&path).ok_or_else(|| {
            format!(
                "source package is missing file {:?} declared by vo.web.json",
                expected.path,
            )
        })?;
        selected.push((path, content));
    }
    let web_content = String::from_utf8(web_bytes).map_err(|error| {
        format!(
            "source package vo.web.json is not valid UTF-8: {}",
            error.utf8_error(),
        )
    })?;
    selected.push((web_path.to_path_buf(), web_content));
    Ok(selected)
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
    let first = first
        .to_str()
        .ok_or_else(|| "source package entry path is not valid UTF-8".to_string())?;
    crate::schema::validate_portable_path_component(first)
        .map_err(|_| format!("invalid source package root component {first:?}"))?;
    let first = first.to_string();
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

fn scan_archive_files(
    data: &[u8],
    mut visit: impl FnMut(PathBuf, Vec<u8>) -> Result<(), String>,
) -> Result<(), String> {
    if u64::try_from(data.len()).unwrap_or(u64::MAX) > crate::MAX_SOURCE_ARCHIVE_BYTES {
        return Err(format!(
            "source package archive exceeds the {}-byte limit",
            crate::MAX_SOURCE_ARCHIVE_BYTES
        ));
    }
    let gz = flate2::read::GzDecoder::new(data);
    let mut archive = tar::Archive::new(gz);
    let mut archive_root: Option<String> = None;
    let mut seen = crate::schema::PortablePathSet::default();
    let mut total_bytes = 0usize;
    let mut archive_entry_count = 0usize;

    for entry in archive.entries().map_err(|e| e.to_string())? {
        archive_entry_count = archive_entry_count
            .checked_add(1)
            .ok_or_else(|| "source package entry count overflow".to_string())?;
        if archive_entry_count > MAX_SOURCE_ARCHIVE_ENTRIES {
            return Err(format!(
                "source package contains more than {MAX_SOURCE_ARCHIVE_ENTRIES} entries"
            ));
        }
        let mut entry = entry.map_err(|e| e.to_string())?;
        let entry_type = entry.header().entry_type();
        if entry_type.is_symlink() || entry_type.is_hard_link() {
            return Err("archive contains a symlink or hardlink, which is not allowed".to_string());
        }

        let raw_path_bytes = entry.path_bytes();
        let raw_path_text = std::str::from_utf8(&raw_path_bytes)
            .map_err(|_| "source package entry path is not valid UTF-8".to_string())?;
        let portable_raw_path = if entry_type.is_dir() {
            raw_path_text.strip_suffix('/').unwrap_or(raw_path_text)
        } else {
            raw_path_text
        };
        crate::schema::validate_portable_relative_path(portable_raw_path)
            .map_err(|_| format!("source package entry path is not portable: {raw_path_text:?}"))?;
        let raw_path = entry.path().map_err(|e| e.to_string())?.into_owned();
        let Some(relative_path) = strip_archive_root(&raw_path, &mut archive_root)? else {
            continue;
        };
        let portable = validate_archive_entry_path(&relative_path)?;
        let inserted = if entry_type.is_dir() {
            seen.insert_directory(&portable)
        } else if entry_type.is_file() {
            seen.insert_file(&portable)
        } else {
            return Err(format!(
                "source package entry {} has an unsupported archive entry type",
                relative_path.display()
            ));
        }
        .map_err(|error| format!("invalid source package path: {error}"))?;
        if !inserted {
            return Err(format!(
                "source package contains duplicate path: {}",
                relative_path.display()
            ));
        }

        if entry_type.is_dir() {
            continue;
        }
        if entry_type.is_file() {
            let advertised_size = usize::try_from(entry.size()).unwrap_or(usize::MAX);
            if advertised_size > MAX_SOURCE_ARCHIVE_ENTRY_BYTES {
                return Err(format!(
                    "source package entry {} exceeds the {}-byte limit",
                    relative_path.display(),
                    MAX_SOURCE_ARCHIVE_ENTRY_BYTES
                ));
            }
            let bytes = read_archive_entry_limited(
                &mut entry,
                MAX_SOURCE_ARCHIVE_ENTRY_BYTES,
                &relative_path,
            )?;
            total_bytes = total_bytes
                .checked_add(bytes.len())
                .ok_or_else(|| "source package extracted size overflow".to_string())?;
            if total_bytes > MAX_EXTRACTED_SOURCE_BYTES {
                return Err(format!(
                    "source package extracted content exceeds the {MAX_EXTRACTED_SOURCE_BYTES}-byte limit"
                ));
            }
            visit(relative_path, bytes)?;
        }
    }

    if archive_root.is_none() {
        return Err("source package archive is empty".to_string());
    }

    Ok(())
}

fn read_archive_entry_limited(
    reader: &mut impl std::io::Read,
    max_bytes: usize,
    path: &Path,
) -> Result<Vec<u8>, String> {
    let mut bytes = Vec::new();
    let mut buffer = [0u8; 8 * 1024];
    loop {
        let remaining = max_bytes.saturating_sub(bytes.len());
        let limit = buffer.len().min(remaining.saturating_add(1));
        let count = match reader.read(&mut buffer[..limit]) {
            Ok(count) => count,
            Err(error) if error.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error.to_string()),
        };
        if count == 0 {
            return Ok(bytes);
        }
        if count > remaining {
            return Err(format!(
                "source package entry {} exceeds the {max_bytes}-byte limit",
                path.display()
            ));
        }
        bytes.try_reserve(count).map_err(|_| {
            format!(
                "failed to reserve memory for source package entry {}",
                path.display()
            )
        })?;
        bytes.extend_from_slice(&buffer[..count]);
    }
}

fn validate_archive_entry_path(relative_path: &Path) -> Result<String, String> {
    let portable =
        crate::schema::portable_relative_path_from_path(relative_path).map_err(|_| {
            format!(
                "archive entry path is not portable: {}",
                relative_path.display()
            )
        })?;
    if crate::schema::is_reserved_module_cache_path(&portable) {
        return Err(format!(
            "archive entry path is reserved for module-cache metadata: {}",
            relative_path.display()
        ));
    }
    for component in relative_path.components() {
        if !matches!(component, std::path::Component::Normal(_)) {
            return Err(format!(
                "archive entry contains invalid path component: {}",
                relative_path.display()
            ));
        }
    }
    Ok(portable)
}

fn is_source_cached_anchored(
    mutation_lock: &crate::cache::mutation_lock::CacheMutationLock,
    locked: &LockedModule,
) -> bool {
    validate_source_cache_entry_with_fs(&mutation_lock.file_system(), locked).is_ok()
}

fn is_artifact_cached_anchored(
    mutation_lock: &crate::cache::mutation_lock::CacheMutationLock,
    locked: &LockedModule,
    artifact: &LockedArtifact,
) -> bool {
    validate_artifact_cache_entry_with_fs(&mutation_lock.file_system(), locked, artifact).is_ok()
}

fn is_publication_collision(error: &Error) -> bool {
    matches!(error, Error::Io(error) if error.kind() == std::io::ErrorKind::AlreadyExists)
}

/// Download and verify a source package into the cache.
fn download_source(
    cache_root: &Path,
    locked: &LockedModule,
    registry: &dyn Registry,
    source_asset_name: &str,
    expected_size: u64,
    manifest_raw: &[u8],
) -> Result<(), Error> {
    {
        let read_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
        if is_source_cached_anchored(&read_lock, locked) {
            return Ok(());
        }
    }

    let data = registry.fetch_source_package(&locked.path, &locked.version, source_asset_name)?;

    verify_size_and_digest(
        &data,
        expected_size,
        &locked.source,
        format!("source package for {} {}", locked.path, locked.version),
    )?;
    let source_entries = extract_source_entries(&data).map_err(|error| Error::DigestMismatch {
        context: format!("source package for {} {}", locked.path, locked.version),
        expected: "validated vo.web.json source file set".to_string(),
        found: error,
    })?;
    drop(data);

    let dir = crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
    let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
    let _identity_lock =
        mutation_lock.identity_lock(&format!("source:{}@{}", locked.path, locked.version))?;
    if is_source_cached_anchored(&mutation_lock, locked) {
        return Ok(());
    }
    let stage_module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    let mut transaction =
        mutation_lock.begin_transaction(&format!("source:{}@{}", locked.path, locked.version))?;
    transaction.create_dir_all(&stage_module_dir)?;

    for (relative_path, content) in source_entries {
        transaction.write_file(&stage_module_dir.join(relative_path), content.as_bytes())?;
    }

    if transaction.entry_kind(&stage_module_dir.join("vo.mod"))? != FileSystemEntryKind::RegularFile
    {
        return Err(Error::SourceScan(format!(
            "source package for {} {} does not contain vo.mod at the module root",
            locked.path, locked.version,
        )));
    }
    if transaction.entry_kind(&stage_module_dir.join("vo.web.json"))?
        != FileSystemEntryKind::RegularFile
    {
        return Err(Error::SourceScan(format!(
            "source package for {} {} does not contain vo.web.json at the module root",
            locked.path, locked.version,
        )));
    }

    // Write metadata files used by compile-time frozen-build validation
    transaction.write_file(
        &stage_module_dir.join(SOURCE_DIGEST_MARKER),
        format!("{}\n", locked.source).as_bytes(),
    )?;
    transaction.write_file(
        &stage_module_dir.join(VERSION_MARKER),
        format!("{}\n", locked.version).as_bytes(),
    )?;
    transaction.write_file(&stage_module_dir.join("vo.release.json"), manifest_raw)?;

    let stage_fs = transaction.file_system();
    crate::cache::validate::validate_installed_module(&stage_fs, &stage_module_dir, locked)
        .map_err(installed_module_error_to_cache_error)?;

    if is_source_cached_anchored(&mutation_lock, locked) {
        return Ok(());
    }
    let module_parent_relative = PathBuf::from(cache_key(&locked.path));
    mutation_lock.ensure_directory(&module_parent_relative)?;
    match mutation_lock.entry_kind(&stage_module_dir)? {
        FileSystemEntryKind::Missing => {}
        other => {
            return Err(Error::SourceScan(format!(
                "module cache destination {} already contains invalid {other:?} data; clean the module cache before installing",
                dir.display(),
            )));
        }
    }
    if let Err(rename_error) = transaction.publish_directory(&stage_module_dir, &stage_module_dir) {
        if is_publication_collision(&rename_error)
            && is_source_cached_anchored(&mutation_lock, locked)
        {
            return Ok(());
        }
        return Err(rename_error);
    }

    validate_source_cache_entry_with_fs(&mutation_lock.file_system(), locked)?;
    if mutation_lock.entry_kind(&stage_module_dir)? != FileSystemEntryKind::Directory {
        return Err(Error::SourceScan(format!(
            "module cache destination {} changed after publication",
            dir.display(),
        )));
    }
    Ok(())
}

/// Download and verify a target-specific artifact into the cache.
fn download_artifact(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
    registry: &dyn Registry,
) -> Result<(), Error> {
    {
        let read_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
        if is_artifact_cached_anchored(&read_lock, locked, artifact) {
            return Ok(());
        }

        // Artifact mutation is only allowed beneath an already authenticated
        // source tree, which also proves the cache-key/version parent chain.
        validate_source_cache_entry_with_fs(&read_lock.file_system(), locked)?;
    }

    let data = registry.fetch_artifact(&locked.path, &locked.version, &artifact.id)?;

    verify_size_and_digest(
        &data,
        artifact.size,
        &artifact.digest,
        format!(
            "artifact {} for {} {}",
            artifact.id.name, locked.path, locked.version
        ),
    )?;

    let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
    let _identity_lock = mutation_lock.identity_lock(&format!(
        "artifact:{}@{}:{}",
        locked.path, locked.version, artifact.id
    ))?;
    // Cleanup may have removed the authenticated source tree while the
    // artifact was downloaded. Revalidate it while holding the shared cache
    // mutation lock before creating any artifact directories.
    validate_source_cache_entry_with_fs(&mutation_lock.file_system(), locked)?;

    let module_relative = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    let relative_artifact_path = crate::artifact::artifact_relative_path(&artifact.id)
        .map_err(Error::InvalidReleaseMetadata)?;
    let artifact_parent_relative = module_relative.join(
        relative_artifact_path
            .parent()
            .ok_or_else(|| Error::SourceScan("artifact path has no parent".to_string()))?,
    );
    mutation_lock.ensure_directory(&artifact_parent_relative)?;
    let artifact_parent = cache_root.join(&artifact_parent_relative);
    let artifact_relative = module_relative.join(&relative_artifact_path);
    let artifact_path = cache_root
        .join(&module_relative)
        .join(&relative_artifact_path);
    let mut transaction = mutation_lock.begin_transaction(&format!(
        "artifact:{}@{}:{}",
        locked.path, locked.version, artifact.id
    ))?;
    transaction.write_file(Path::new("payload"), &data)?;
    let staged_bytes = transaction.read_file(Path::new("payload"), data.len())?;
    if staged_bytes != data {
        return Err(Error::DigestMismatch {
            context: format!("staged artifact {}", artifact.id.name),
            expected: Digest::from_sha256(&data).to_string(),
            found: Digest::from_sha256(&staged_bytes).to_string(),
        });
    }

    if is_artifact_cached_anchored(&mutation_lock, locked, artifact) {
        return Ok(());
    }
    let actual_artifact_parent = artifact_path
        .parent()
        .ok_or_else(|| Error::SourceScan("artifact cache path has no parent".to_string()))?;
    if artifact_parent != actual_artifact_parent {
        return Err(Error::SourceScan(
            "artifact parent path changed during preparation".to_string(),
        ));
    }
    match mutation_lock.entry_kind(&artifact_relative)? {
        FileSystemEntryKind::Missing => {}
        other => {
            return Err(Error::SourceScan(format!(
                "artifact cache destination {} already contains invalid {other:?} data; clean the module cache before installing",
                artifact_path.display(),
            )));
        }
    }
    if let Err(rename_error) = transaction.publish_file(Path::new("payload"), &artifact_relative) {
        if is_publication_collision(&rename_error)
            && is_artifact_cached_anchored(&mutation_lock, locked, artifact)
        {
            return Ok(());
        }
        return Err(rename_error);
    }
    validate_artifact_cache_entry_with_fs(&mutation_lock.file_system(), locked, artifact)?;
    if mutation_lock.entry_kind(&artifact_relative)? != FileSystemEntryKind::RegularFile {
        return Err(Error::SourceScan(format!(
            "artifact cache destination {} changed after publication",
            artifact_path.display(),
        )));
    }
    Ok(())
}

/// Verify that all locked modules and their required artifacts are present in the cache
/// with correct digests. This is the frozen-build verification step.
///
/// Returns an error describing the first missing or invalid entry.
pub fn verify_locked_cache(cache_root: &Path, lock_file: &LockFile) -> Result<(), Error> {
    verify_locked_modules(cache_root, &lock_file.resolved)
}

fn verify_locked_modules(cache_root: &Path, locked_modules: &[LockedModule]) -> Result<(), Error> {
    let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
    let cache_fs = mutation_lock.file_system();
    for locked in locked_modules {
        validate_source_cache_entry_with_fs(&cache_fs, locked)?;

        for artifact in &locked.artifacts {
            validate_artifact_cache_entry_with_fs(&cache_fs, locked, artifact)?;
        }
    }
    Ok(())
}

/// A single unit of download work identified during the planning phase.
struct DownloadJob<'a> {
    locked: &'a LockedModule,
    source_asset: Option<String>,
    source_size: Option<u64>,
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
    _created_by: &str,
) -> Result<ExactInstallResult, Error> {
    let (manifest, manifest_raw) =
        crate::registry::fetch_verified_manifest_raw(registry, module, version)?;
    let locked = locked_module_from_manifest_raw(&manifest, &manifest_raw);
    populate_locked_modules(cache_root, std::slice::from_ref(&locked), registry)?;
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
    populate_locked_modules(cache_root, &lock_file.resolved, registry)
}

pub(crate) fn populate_locked_modules(
    cache_root: &Path,
    locked_modules: &[LockedModule],
    registry: &dyn Registry,
) -> Result<(), Error> {
    crate::schema::lockfile::validate_materialized_module_limits(locked_modules)?;
    // Keep the cache-wide read lease around local inspection only. Registry
    // calls and downloads can block, while each commit takes its own shared
    // mutation lease and revalidates the destination before publishing.
    let plans = {
        let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
        locked_modules
            .iter()
            .filter_map(|locked| {
                let needs_source = !is_source_cached_anchored(&mutation_lock, locked);
                let missing_artifacts = locked
                    .artifacts
                    .iter()
                    .filter(|artifact| {
                        !is_artifact_cached_anchored(&mutation_lock, locked, artifact)
                    })
                    .collect::<Vec<_>>();
                (needs_source || !missing_artifacts.is_empty()).then_some((
                    locked,
                    needs_source,
                    missing_artifacts,
                ))
            })
            .collect::<Vec<_>>()
    };

    // Fetch authenticated manifests after releasing the cache lease.
    let mut jobs = Vec::new();
    jobs.try_reserve(plans.len())
        .map_err(|_| Error::SourceScan("failed to reserve module download plan".to_string()))?;
    for (locked, needs_source, missing_artifacts) in plans {
        let (source_asset, source_size, manifest_raw) = if needs_source {
            let (manifest, raw) = crate::registry::fetch_verified_manifest_raw(
                registry,
                &locked.path,
                &locked.version,
            )?;
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
            (
                Some(manifest.source.name.clone()),
                Some(manifest.source.size),
                Some(raw),
            )
        } else {
            (None, None, None)
        };

        jobs.push(DownloadJob {
            locked,
            source_asset,
            source_size,
            manifest_raw,
            artifacts: missing_artifacts,
        });
    }

    if jobs.is_empty() {
        return verify_locked_modules(cache_root, locked_modules);
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

    verify_locked_modules(cache_root, locked_modules)
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
        if let (Some(asset_name), Some(source_size), Some(raw)) = (
            job.source_asset.as_deref(),
            job.source_size,
            job.manifest_raw.as_deref(),
        ) {
            download_source(
                cache_root,
                job.locked,
                registry,
                asset_name,
                source_size,
                raw,
            )?;
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
                    if error_ref
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .is_some()
                    {
                        return;
                    }

                    let idx = counter_ref.fetch_add(1, Ordering::Relaxed) + 1;
                    eprintln!(
                        "  [{}/{}] {} {}",
                        idx, total, job.locked.path, job.locked.version
                    );

                    if let (Some(asset_name), Some(source_size), Some(raw)) = (
                        job.source_asset.as_deref(),
                        job.source_size,
                        job.manifest_raw.as_deref(),
                    ) {
                        if let Err(e) = download_source(
                            cache_root,
                            job.locked,
                            registry,
                            asset_name,
                            source_size,
                            raw,
                        ) {
                            *error_ref
                                .lock()
                                .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(e);
                            return;
                        }
                    }
                    for artifact in &job.artifacts {
                        if error_ref
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .is_some()
                        {
                            return;
                        }
                        if let Err(e) =
                            download_artifact(cache_root, job.locked, artifact, registry)
                        {
                            *error_ref
                                .lock()
                                .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(e);
                            return;
                        }
                    }
                }
            });
        }
    });

    if let Some(e) = first_error
        .into_inner()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
    {
        return Err(e);
    }
    Ok(())
}

fn validate_source_cache_entry_with_fs<F: vo_common::vfs::FileSystem>(
    fs: &F,
    locked: &LockedModule,
) -> Result<(), Error> {
    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    crate::cache::validate::validate_installed_module(fs, &module_dir, locked)
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

fn validate_artifact_cache_entry_with_fs<F: vo_common::vfs::FileSystem>(
    fs: &F,
    locked: &LockedModule,
    artifact: &LockedArtifact,
) -> Result<(), Error> {
    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    crate::cache::validate::validate_installed_artifact(fs, &module_dir, locked, &artifact.id)
        .map_err(|e| Error::MissingArtifact {
            module: e.module,
            version: e.version,
            detail: format!("{}", e.kind),
        })
}

#[cfg(test)]
mod tests;
