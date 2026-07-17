use std::cell::Cell;
use std::collections::BTreeMap;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use vo_common::vfs::FileSystemEntryKind;

use crate::cache::layout::{cache_key, SOURCE_DIGEST_MARKER, VERSION_MARKER};
use crate::digest::{verify_digest, verify_size_and_digest, Digest};
use crate::registry::Registry;
use crate::schema::lockfile::{LockFile, LockedModule};
use crate::schema::manifest::{ManifestArtifact, ReleaseManifest, SOURCE_ARCHIVE_ROOT_DIR};
use crate::schema::SourceFileMode;
use crate::{
    Error, MAX_EXTRACTED_SOURCE_BYTES, MAX_SOURCE_ARCHIVE_ENTRIES, MAX_SOURCE_ARCHIVE_ENTRY_BYTES,
};

// ── Source package extraction (platform-agnostic) ────────────────────────────

/// Extract source entries from a tar.gz release source package.
///
/// Every archive entry must be a regular file below the fixed `source/`
/// directory. The root prefix is stripped, the embedded `vo.package.json` is
/// parsed, and the exact raw-byte source closure is returned. Directory and
/// other metadata entries are rejected because they are absent from the
/// authenticated package closure.
///
/// Paths are relative to the module root. Package bytes are returned
/// separately so the caller can authenticate them against `vo.release.json`.
#[derive(Debug, Clone)]
pub struct ExtractedSource {
    pub package_bytes: Vec<u8>,
    pub files: Vec<ExtractedSourceFile>,
}

#[derive(Debug, Clone)]
pub struct ExtractedSourceFile {
    pub path: PathBuf,
    pub mode: SourceFileMode,
    pub bytes: Vec<u8>,
}

pub fn extract_source_entries(archive_bytes: &[u8]) -> Result<ExtractedSource, String> {
    let package_path = Path::new("vo.package.json");
    let mut archive_files = BTreeMap::new();
    scan_archive_files(archive_bytes, |relative_path, mode, bytes| {
        if archive_files
            .insert(
                relative_path.clone(),
                ExtractedSourceFile {
                    path: relative_path.clone(),
                    mode,
                    bytes,
                },
            )
            .is_some()
        {
            return Err(format!(
                "source package contains duplicate file {}",
                relative_path.display()
            ));
        }
        Ok(())
    })?;
    let package_file = archive_files.remove(package_path).ok_or_else(|| {
        "source package does not contain vo.package.json at the module root".to_string()
    })?;
    if package_file.mode != SourceFileMode::Regular {
        return Err("source package vo.package.json must use regular mode".to_string());
    }
    let package_bytes = package_file.bytes;
    let package = crate::schema::PackageManifest::parse(&package_bytes)
        .map_err(|error| format!("source package vo.package.json is invalid: {error}"))?;

    let mut files = Vec::new();
    files
        .try_reserve(package.files.len())
        .map_err(|_| "failed to reserve source package file set".to_string())?;
    for expected in &package.files {
        let path = PathBuf::from(&expected.path);
        let archived = archive_files.remove(&path).ok_or_else(|| {
            format!(
                "source package is missing file {:?} declared by vo.package.json",
                expected.path
            )
        })?;
        if archived.mode != expected.mode {
            return Err(format!(
                "source package file {:?} mode does not match vo.package.json: expected {:?}, found {:?}",
                expected.path, expected.mode, archived.mode,
            ));
        }
        let found_size = u64::try_from(archived.bytes.len()).unwrap_or(u64::MAX);
        let found_digest = crate::digest::Digest::from_sha256(&archived.bytes);
        if found_size != expected.size || found_digest != expected.digest {
            return Err(format!(
                "source package file {:?} does not match vo.package.json: expected {} ({} bytes), found {} ({} bytes)",
                expected.path, expected.digest, expected.size, found_digest, found_size,
            ));
        }
        files.push(archived);
    }
    if let Some((path, _)) = archive_files.first_key_value() {
        return Err(format!(
            "source package contains file {} that is absent from vo.package.json",
            path.display()
        ));
    }
    Ok(ExtractedSource {
        package_bytes,
        files,
    })
}

const MAX_SOURCE_ARCHIVE_WIRE_PATH_BYTES: usize =
    SOURCE_ARCHIVE_ROOT_DIR.len() + 1 + crate::schema::MAX_PORTABLE_PATH_BYTES;
const MAX_GNU_LONG_NAME_PAYLOAD_BYTES: usize = MAX_SOURCE_ARCHIVE_WIRE_PATH_BYTES + 1;
const MAX_RAW_SOURCE_ARCHIVE_ENTRIES: usize = MAX_SOURCE_ARCHIVE_ENTRIES * 2;

#[derive(Clone, Default)]
struct TarPaddingGuard {
    remaining: Rc<Cell<usize>>,
}

impl TarPaddingGuard {
    fn expect_after(&self, size: u64) -> Result<(), String> {
        if self.remaining.get() != 0 {
            return Err(
                "source package parser did not consume the preceding tar entry padding".to_string(),
            );
        }
        let remainder = usize::try_from(size % 512).expect("tar block remainder fits usize");
        self.remaining.set((512 - remainder) % 512);
        Ok(())
    }
}

struct CanonicalTarReader<R> {
    inner: R,
    padding: TarPaddingGuard,
}

impl<R> CanonicalTarReader<R> {
    fn new(inner: R) -> (Self, TarPaddingGuard) {
        let padding = TarPaddingGuard::default();
        (
            Self {
                inner,
                padding: padding.clone(),
            },
            padding,
        )
    }

    fn into_inner(self) -> R {
        self.inner
    }
}

impl<R: Read> Read for CanonicalTarReader<R> {
    fn read(&mut self, buffer: &mut [u8]) -> std::io::Result<usize> {
        let read = self.inner.read(buffer)?;
        let checked = read.min(self.padding.remaining.get());
        if buffer[..checked].iter().any(|byte| *byte != 0) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "source package tar entry padding must be zero",
            ));
        }
        self.padding
            .remaining
            .set(self.padding.remaining.get() - checked);
        Ok(read)
    }
}

fn strip_archive_root(raw_path: &str) -> Result<PathBuf, String> {
    if raw_path.len() > MAX_SOURCE_ARCHIVE_WIRE_PATH_BYTES {
        return Err(format!(
            "source package wire path exceeds the {MAX_SOURCE_ARCHIVE_WIRE_PATH_BYTES}-byte limit"
        ));
    }
    if raw_path == SOURCE_ARCHIVE_ROOT_DIR {
        return Err(
            "source package entries must name a regular file below the single top-level directory"
                .to_string(),
        );
    }
    let prefix = format!("{SOURCE_ARCHIVE_ROOT_DIR}/");
    let relative = raw_path.strip_prefix(&prefix).ok_or_else(|| {
        format!("source package root component must be exactly {SOURCE_ARCHIVE_ROOT_DIR:?}")
    })?;
    if relative.is_empty() {
        return Err(
            "source package entries must name a regular file below the single top-level directory"
                .to_string(),
        );
    }
    crate::schema::validate_portable_relative_path(relative)
        .map_err(|error| format!("source package entry path is not portable: {error}"))?;
    Ok(PathBuf::from(relative))
}

fn canonical_gnu_long_name_header(path_size: usize) -> Result<tar::Header, String> {
    let payload_size = u64::try_from(path_size)
        .map_err(|_| "GNU long-name path size exceeds u64".to_string())?
        .checked_add(1)
        .ok_or_else(|| "GNU long-name payload size overflows u64".to_string())?;
    let mut header = tar::Header::new_gnu();
    let name = b"././@LongLink";
    header.as_gnu_mut().expect("new GNU header").name[..name.len()].copy_from_slice(name);
    header.set_mode(0o644);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    header.set_size(payload_size);
    header.set_entry_type(tar::EntryType::GNULongName);
    header.set_cksum();
    Ok(header)
}

fn canonical_source_file_header(path: &str, size: u64, mode: u32) -> Result<tar::Header, String> {
    let mut header = tar::Header::new_gnu();
    header.set_size(size);
    header.set_mode(mode);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    let inline_limit = header.as_old().name.len();
    if path.len() <= inline_limit {
        header
            .set_path(path)
            .map_err(|error| format!("cannot encode source package path {path:?}: {error}"))?;
    } else {
        let valid_prefix = std::str::from_utf8(&path.as_bytes()[..inline_limit])
            .map(|_| inline_limit)
            .unwrap_or_else(|error| error.valid_up_to());
        if valid_prefix == 0 {
            return Err(format!(
                "source package path {path:?} has no complete UTF-8 scalar in its GNU header prefix"
            ));
        }
        header.as_gnu_mut().expect("new GNU header").name[..valid_prefix]
            .copy_from_slice(&path.as_bytes()[..valid_prefix]);
    }
    header.set_cksum();
    Ok(header)
}

fn read_canonical_gnu_long_name(
    entry: &mut impl Read,
    header: &tar::Header,
) -> Result<String, String> {
    let advertised_size = usize::try_from(header.size().map_err(|error| error.to_string())?)
        .map_err(|_| "GNU long-name payload size exceeds usize".to_string())?;
    if !(2..=MAX_GNU_LONG_NAME_PAYLOAD_BYTES).contains(&advertised_size) {
        return Err(format!(
            "GNU long-name payload is {advertised_size} bytes; expected 2..={MAX_GNU_LONG_NAME_PAYLOAD_BYTES}"
        ));
    }
    let expected_header = canonical_gnu_long_name_header(advertised_size - 1)?;
    if header.as_bytes() != expected_header.as_bytes() {
        return Err("source package GNU long-name header is not canonical".to_string());
    }
    let mut bytes = read_archive_entry_limited(
        entry,
        MAX_GNU_LONG_NAME_PAYLOAD_BYTES,
        Path::new("GNU long-name payload"),
    )?;
    if bytes.len() != advertised_size {
        return Err(format!(
            "GNU long-name payload advertised {advertised_size} bytes and yielded {}",
            bytes.len(),
        ));
    }
    if bytes.last() != Some(&0) || bytes[..bytes.len() - 1].contains(&0) {
        return Err("GNU long-name payload must contain exactly one trailing NUL".to_string());
    }
    bytes.pop();
    let path = String::from_utf8(bytes)
        .map_err(|error| format!("GNU long-name path is not valid UTF-8: {error}"))?;
    if path.len() <= tar::Header::new_gnu().as_old().name.len() {
        return Err(
            "GNU long-name extension is noncanonical for a path of at most 100 bytes".to_string(),
        );
    }
    Ok(path)
}

fn scan_archive_files(
    data: &[u8],
    mut visit: impl FnMut(PathBuf, SourceFileMode, Vec<u8>) -> Result<(), String>,
) -> Result<(), String> {
    if u64::try_from(data.len()).unwrap_or(u64::MAX) > crate::MAX_SOURCE_ARCHIVE_BYTES {
        return Err(format!(
            "source package archive exceeds the {}-byte limit",
            crate::MAX_SOURCE_ARCHIVE_BYTES
        ));
    }
    let decoder = flate2::bufread::GzDecoder::new(data);
    let (tar_reader, padding_guard) = CanonicalTarReader::new(decoder);
    let mut archive = tar::Archive::new(tar_reader);
    let mut seen = crate::schema::PortablePathSet::default();
    let mut total_bytes = 0usize;
    let mut archive_entry_count = 0usize;
    let mut raw_entry_count = 0usize;
    let mut pending_long_name = None;
    let mut previous_path: Option<String> = None;
    let mut entries = archive
        .entries()
        .map_err(|error| error.to_string())?
        .raw(true);

    for entry in entries.by_ref() {
        raw_entry_count = raw_entry_count
            .checked_add(1)
            .ok_or_else(|| "source package raw entry count overflow".to_string())?;
        if raw_entry_count > MAX_RAW_SOURCE_ARCHIVE_ENTRIES {
            return Err(format!(
                "source package contains more than {MAX_RAW_SOURCE_ARCHIVE_ENTRIES} physical tar entries"
            ));
        }
        let mut entry = entry.map_err(|error| error.to_string())?;
        let entry_type = entry.header().entry_type();
        if entry_type.is_gnu_longname() {
            if pending_long_name.is_some() {
                return Err(
                    "GNU long-name headers must be followed immediately by one regular file"
                        .to_string(),
                );
            }
            let header = entry.header().clone();
            pending_long_name = Some(read_canonical_gnu_long_name(&mut entry, &header)?);
            padding_guard.expect_after(entry.size())?;
            continue;
        }
        if !entry_type.is_file() {
            return Err(format!(
                "source package entries must name a regular file below the single top-level directory; tar entry type {entry_type:?} is forbidden"
            ));
        }
        archive_entry_count = archive_entry_count
            .checked_add(1)
            .ok_or_else(|| "source package entry count overflow".to_string())?;
        if archive_entry_count > MAX_SOURCE_ARCHIVE_ENTRIES {
            return Err(format!(
                "source package contains more than {MAX_SOURCE_ARCHIVE_ENTRIES} entries"
            ));
        }
        let used_long_name = pending_long_name.is_some();
        let raw_path_text = if let Some(path) = pending_long_name.take() {
            path
        } else {
            let raw_path_bytes = entry.path_bytes();
            std::str::from_utf8(raw_path_bytes.as_ref())
                .map_err(|_| "source package entry path is not valid UTF-8".to_string())?
                .to_string()
        };
        let inline_limit = tar::Header::new_gnu().as_old().name.len();
        if used_long_name != (raw_path_text.len() > inline_limit) {
            return Err(format!(
                "source package path {raw_path_text:?} does not use the canonical GNU long-name form"
            ));
        }
        if previous_path
            .as_deref()
            .is_some_and(|previous| previous.as_bytes() >= raw_path_text.as_bytes())
        {
            return Err(format!(
                "source package entries must be strictly sorted; {raw_path_text:?} follows {:?}",
                previous_path.as_deref().unwrap_or_default(),
            ));
        }
        previous_path = Some(raw_path_text.clone());
        let relative_path = strip_archive_root(&raw_path_text)?;
        let mode = match entry.header().mode().map_err(|error| error.to_string())? {
            0o644 => SourceFileMode::Regular,
            0o755 => SourceFileMode::Executable,
            mode => {
                return Err(format!(
                    "source package entry {} uses non-canonical mode {mode:#o}; expected 0644 or 0755",
                    relative_path.display(),
                ));
            }
        };
        let advertised_archive_size = entry.size();
        let expected_header = canonical_source_file_header(
            &raw_path_text,
            advertised_archive_size,
            match mode {
                SourceFileMode::Regular => 0o644,
                SourceFileMode::Executable => 0o755,
            },
        )?;
        if entry.header().as_bytes() != expected_header.as_bytes() {
            return Err(format!(
                "source package entry {} header is not canonical",
                relative_path.display(),
            ));
        }
        let portable = validate_archive_entry_path(&relative_path)?;
        let inserted = seen
            .insert_file(&portable)
            .map_err(|error| format!("invalid source package path: {error}"))?;
        if !inserted {
            return Err(format!(
                "source package contains duplicate path: {}",
                relative_path.display()
            ));
        }

        let advertised_size = usize::try_from(entry.size()).unwrap_or(usize::MAX);
        let entry_limit = if portable == "vo.package.json" || portable.ends_with(".vo") {
            vo_common::vfs::MAX_TEXT_FILE_BYTES
        } else {
            MAX_SOURCE_ARCHIVE_ENTRY_BYTES
        };
        if advertised_size > entry_limit {
            return Err(format!(
                "source package entry {} exceeds the {}-byte limit",
                relative_path.display(),
                entry_limit
            ));
        }
        let bytes = read_archive_entry_limited(&mut entry, entry_limit, &relative_path)?;
        padding_guard.expect_after(advertised_archive_size)?;
        total_bytes = total_bytes
            .checked_add(bytes.len())
            .ok_or_else(|| "source package extracted size overflow".to_string())?;
        if total_bytes > MAX_EXTRACTED_SOURCE_BYTES {
            return Err(format!(
                "source package extracted content exceeds the {MAX_EXTRACTED_SOURCE_BYTES}-byte limit"
            ));
        }
        visit(relative_path, mode, bytes)?;
    }
    if pending_long_name.is_some() {
        return Err("GNU long-name header is not followed by a regular file".to_string());
    }
    if archive_entry_count == 0 {
        return Err("source package archive is empty".to_string());
    }

    let tar_reader = archive.into_inner();
    if padding_guard.remaining.get() != 0 {
        return Err(
            "source package ended before its final tar entry padding was consumed".to_string(),
        );
    }
    let mut decoder = tar_reader.into_inner();
    let mut canonical_tar_end = [0u8; 512];
    decoder
        .read_exact(&mut canonical_tar_end)
        .map_err(|error| {
            format!("source package is missing its canonical tar end block: {error}")
        })?;
    if canonical_tar_end.iter().any(|byte| *byte != 0) {
        return Err("source package has nonzero bytes after the first tar end block".to_string());
    }
    let mut trailing = [0u8; 1];
    if decoder
        .read(&mut trailing)
        .map_err(|error| format!("source package gzip trailer is invalid: {error}"))?
        != 0
    {
        return Err(
            "source package has decompressed bytes after the canonical tar ending".to_string(),
        );
    }
    if !decoder.into_inner().is_empty() {
        return Err(
            "source package contains a concatenated gzip member or trailing compressed bytes"
                .to_string(),
        );
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
    if portable != "vo.package.json" && crate::schema::is_reserved_module_cache_path(&portable) {
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

fn cached_source_metadata_anchored(
    mutation_lock: &crate::cache::mutation_lock::CacheMutationLock,
    locked: &LockedModule,
) -> Result<Option<crate::cache::validate::InstalledModuleMetadata>, Error> {
    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    match mutation_lock.entry_kind(&module_dir)? {
        FileSystemEntryKind::Missing => Ok(None),
        FileSystemEntryKind::Directory => {
            load_source_metadata_with_fs(&mutation_lock.file_system(), locked)
                .map(Some)
                .map_err(|error| invalid_existing_cache_entry(&module_dir, error))
        }
        other => Err(Error::SourceScan(format!(
            "module cache destination {} contains invalid {other:?} data; clean the module cache before fetching",
            module_dir.display(),
        ))),
    }
}

fn is_source_cached_anchored(
    mutation_lock: &crate::cache::mutation_lock::CacheMutationLock,
    locked: &LockedModule,
) -> Result<bool, Error> {
    cached_source_metadata_anchored(mutation_lock, locked).map(|metadata| metadata.is_some())
}

fn is_artifact_cached_anchored(
    mutation_lock: &crate::cache::mutation_lock::CacheMutationLock,
    locked: &LockedModule,
    artifact: &ManifestArtifact,
) -> Result<bool, Error> {
    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    let artifact_path = module_dir.join(
        crate::artifact::artifact_relative_path(&artifact.id)
            .map_err(Error::InvalidReleaseMetadata)?,
    );
    match mutation_lock.entry_kind(&artifact_path)? {
        FileSystemEntryKind::Missing => Ok(false),
        FileSystemEntryKind::RegularFile => {
            validate_artifact_cache_entry_with_fs(&mutation_lock.file_system(), locked, artifact)
                .map(|()| true)
                .map_err(|error| invalid_existing_cache_entry(&artifact_path, error))
        }
        other => Err(Error::SourceScan(format!(
            "artifact cache destination {} contains invalid {other:?} data; clean the module cache before fetching",
            artifact_path.display(),
        ))),
    }
}

fn invalid_existing_cache_entry(path: &Path, error: Error) -> Error {
    Error::SourceScan(format!(
        "cache destination {} contains invalid data ({error}); clean the module cache before fetching",
        path.display(),
    ))
}

fn is_publication_collision(error: &Error) -> bool {
    matches!(error, Error::Io(error) if error.kind() == std::io::ErrorKind::AlreadyExists)
}

/// Download and verify a source package into the cache.
fn download_source(
    cache_root: &Path,
    locked: &LockedModule,
    registry: &dyn Registry,
    release: &ReleaseManifest,
    manifest_raw: &[u8],
) -> Result<(), Error> {
    {
        let read_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
        if is_source_cached_anchored(&read_lock, locked)? {
            return Ok(());
        }
    }

    let data =
        registry.fetch_source_package(&locked.path, &locked.version, &release.source.name)?;

    verify_size_and_digest(
        &data,
        release.source.size,
        &release.source.digest,
        format!("source package for {} {}", locked.path, locked.version),
    )?;
    let extracted = extract_source_entries(&data).map_err(|error| Error::DigestMismatch {
        context: format!("source package for {} {}", locked.path, locked.version),
        expected: "validated vo.package.json source file set".to_string(),
        found: error,
    })?;
    verify_size_and_digest(
        &extracted.package_bytes,
        release.package.size,
        &release.package.digest,
        format!("vo.package.json for {} {}", locked.path, locked.version),
    )?;
    drop(data);

    let dir = crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
    let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
    let _identity_lock =
        mutation_lock.identity_lock(&format!("source:{}@{}", locked.path, locked.version))?;
    if is_source_cached_anchored(&mutation_lock, locked)? {
        return Ok(());
    }
    let stage_module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    let mut transaction =
        mutation_lock.begin_transaction(&format!("source:{}@{}", locked.path, locked.version))?;
    transaction.create_dir_all(&stage_module_dir)?;

    for file in extracted.files {
        transaction.write_source_file(&stage_module_dir.join(file.path), &file.bytes, file.mode)?;
    }
    transaction.write_file(
        &stage_module_dir.join("vo.package.json"),
        &extracted.package_bytes,
    )?;

    if transaction.entry_kind(&stage_module_dir.join("vo.mod"))? != FileSystemEntryKind::RegularFile
    {
        return Err(Error::SourceScan(format!(
            "source package for {} {} does not contain vo.mod at the module root",
            locked.path, locked.version,
        )));
    }
    if transaction.entry_kind(&stage_module_dir.join("vo.package.json"))?
        != FileSystemEntryKind::RegularFile
    {
        return Err(Error::SourceScan(format!(
            "source package for {} {} does not contain vo.package.json at the module root",
            locked.path, locked.version,
        )));
    }

    // Write metadata files used by compile-time frozen-build validation
    transaction.write_file(
        &stage_module_dir.join(SOURCE_DIGEST_MARKER),
        format!("{}\n", release.source.digest).as_bytes(),
    )?;
    transaction.write_file(
        &stage_module_dir.join(VERSION_MARKER),
        format!("{}\n", locked.version).as_bytes(),
    )?;
    transaction.write_file(&stage_module_dir.join("vo.release.json"), manifest_raw)?;

    let stage_fs = transaction.file_system();
    crate::cache::validate::validate_installed_module(&stage_fs, &stage_module_dir, locked)
        .map_err(installed_module_error_to_cache_error)?;

    if is_source_cached_anchored(&mutation_lock, locked)? {
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
            && is_source_cached_anchored(&mutation_lock, locked)?
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
    artifact: &ManifestArtifact,
    registry: &dyn Registry,
) -> Result<(), Error> {
    {
        let read_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
        if is_artifact_cached_anchored(&read_lock, locked, artifact)? {
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

    if is_artifact_cached_anchored(&mutation_lock, locked, artifact)? {
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
            && is_artifact_cached_anchored(&mutation_lock, locked, artifact)?
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
    verify_locked_modules(cache_root, &lock_file.modules)
}

fn verify_locked_modules(cache_root: &Path, locked_modules: &[LockedModule]) -> Result<(), Error> {
    if locked_modules.is_empty() {
        return Ok(());
    }
    let mutation_lock =
        crate::cache::mutation_lock::CacheMutationLock::shared_existing(cache_root)?;
    let cache_fs = mutation_lock.file_system();
    let mut budget = crate::registry::MaterializedGraphBudget::default();
    for locked in locked_modules {
        let metadata = load_source_metadata_with_fs(&cache_fs, locked)?;
        budget.charge_release(
            metadata.release_manifest_bytes,
            metadata.release.artifacts.len(),
        )?;
        for artifact in &metadata.release.artifacts {
            validate_artifact_cache_entry_with_fs(&cache_fs, locked, artifact)?;
        }
    }
    Ok(())
}

/// A single unit of download work identified during the planning phase.
struct DownloadJob<'a> {
    locked: &'a LockedModule,
    release: ReleaseManifest,
    manifest_raw: Option<Vec<u8>>,
    artifacts: Vec<ManifestArtifact>,
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
    populate_locked_modules(cache_root, &lock_file.modules, registry)
}

pub(crate) fn populate_locked_modules(
    cache_root: &Path,
    locked_modules: &[LockedModule],
    registry: &dyn Registry,
) -> Result<(), Error> {
    crate::schema::lockfile::validate_materialized_module_limits(locked_modules)?;
    let mut budget = crate::registry::MaterializedGraphBudget::default();
    let mut jobs = Vec::new();
    jobs.try_reserve(locked_modules.len())
        .map_err(|_| Error::SourceScan("failed to reserve module download plan".to_string()))?;
    for locked in locked_modules {
        let cached_metadata = {
            let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
            cached_source_metadata_anchored(&mutation_lock, locked)?
        };
        let needs_source = cached_metadata.is_none();
        let (release, manifest_raw, manifest_bytes) = if let Some(metadata) = cached_metadata {
            (metadata.release, None, metadata.release_manifest_bytes)
        } else {
            let (manifest, raw) = crate::registry::fetch_verified_manifest_raw(
                registry,
                &locked.path,
                &locked.version,
            )?;
            verify_digest(
                &raw,
                &locked.release,
                format!("release manifest for {} {}", locked.path, locked.version),
            )?;
            crate::lock::validate_locked_module_against_manifest(
                locked,
                &manifest,
                &crate::digest::Digest::from_sha256(&raw),
            )?;
            let manifest_bytes = raw.len();
            (manifest, Some(raw), manifest_bytes)
        };
        budget.charge_release(manifest_bytes, release.artifacts.len())?;

        let missing_artifacts = {
            let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
            let mut missing = Vec::new();
            missing.try_reserve(release.artifacts.len()).map_err(|_| {
                Error::SourceScan("failed to reserve artifact download plan".to_string())
            })?;
            for artifact in &release.artifacts {
                if !is_artifact_cached_anchored(&mutation_lock, locked, artifact)? {
                    missing.push(artifact.clone());
                }
            }
            missing
        };
        if !needs_source && missing_artifacts.is_empty() {
            continue;
        }

        jobs.push(DownloadJob {
            locked,
            release,
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
        if let Some(raw) = job.manifest_raw.as_deref() {
            download_source(cache_root, job.locked, registry, &job.release, raw)?;
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

                    if let Some(raw) = job.manifest_raw.as_deref() {
                        if let Err(e) =
                            download_source(cache_root, job.locked, registry, &job.release, raw)
                        {
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
    load_source_metadata_with_fs(fs, locked).map(drop)
}

fn load_source_metadata_with_fs<F: vo_common::vfs::FileSystem>(
    fs: &F,
    locked: &LockedModule,
) -> Result<crate::cache::validate::InstalledModuleMetadata, Error> {
    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    crate::cache::validate::validate_installed_module_with_metadata(fs, &module_dir, locked)
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
            field: "release".to_string(),
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
    artifact: &ManifestArtifact,
) -> Result<(), Error> {
    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    crate::cache::validate::validate_installed_artifact_from_metadata(
        fs,
        &module_dir,
        locked,
        artifact,
    )
    .map_err(|e| Error::MissingArtifact {
        module: e.module,
        version: e.version,
        detail: format!("{}", e.kind),
    })
}

#[cfg(test)]
mod tests;
