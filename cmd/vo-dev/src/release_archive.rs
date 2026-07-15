use crate::config::ReleaseFile;
use crate::release_config::{
    artifact_name, provenance_name, read_checked_sha256, release_binary_name, sha256_file,
};
use crate::release_identity::ReleaseIdentity;
use anyhow::{anyhow, bail, Context, Result};
use flate2::bufread::GzDecoder;
use flate2::{Compression, GzBuilder};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::{self, File, OpenOptions};
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use tar::{Builder, Header};

const BUILD_RECEIPT_SCHEMA: u32 = 1;
const PROVENANCE_SCHEMA: u32 = 1;
const MAX_RELEASE_BINARY_SIZE: u64 = 512 * 1024 * 1024;
const MAX_RELEASE_ARCHIVE_SIZE: u64 = MAX_RELEASE_BINARY_SIZE + 2 * 1024 * 1024;
const MAX_RELEASE_EVIDENCE_SIZE: u64 = 1024 * 1024;
static TEMP_SEQUENCE: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ReleaseBuildReceipt {
    schema: u32,
    identity: ReleaseIdentity,
    target: String,
    binary: BinaryRecord,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ReleaseProvenance {
    schema: u32,
    identity: ReleaseIdentity,
    target: String,
    archive: ArchiveRecord,
    binary: BinaryRecord,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct BinaryRecord {
    path: String,
    sha256: String,
    size: u64,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ArchiveRecord {
    path: String,
    sha256: String,
    size: u64,
    format: String,
}

pub(crate) fn clear_release_build_outputs(
    root: &Path,
    release: &ReleaseFile,
    target: &str,
) -> Result<()> {
    let binary_name = release_binary_name(release, target);
    for path in [
        release_binary_path(root, target, &binary_name),
        build_receipt_path(root, target),
    ] {
        match fs::symlink_metadata(&path) {
            Ok(metadata) if metadata.file_type().is_file() => {
                fs::remove_file(&path).with_context(|| {
                    format!("could not clear stale release output {}", path.display())
                })?
            }
            Ok(_) => bail!(
                "stale release output must be a regular file: {}",
                path.display()
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => {
                return Err(error).with_context(|| {
                    format!("could not inspect stale release output {}", path.display())
                })
            }
        }
    }
    Ok(())
}

pub(crate) fn record_release_build(
    root: &Path,
    release: &ReleaseFile,
    target: &str,
    identity: &ReleaseIdentity,
) -> Result<()> {
    let binary_name = release_binary_name(release, target);
    let binary_path = release_binary_path(root, target, &binary_name);
    let binary = binary_record(&binary_path, &binary_name)?;
    validate_embedded_build_identity(&binary_path, identity)?;
    let receipt = ReleaseBuildReceipt {
        schema: BUILD_RECEIPT_SCHEMA,
        identity: identity.clone(),
        target: target.to_string(),
        binary,
    };
    write_json_atomic(&build_receipt_path(root, target), &receipt)
}

pub(crate) fn package_release_binary(
    root: &Path,
    release: &ReleaseFile,
    target: &str,
    identity: &ReleaseIdentity,
) -> Result<String> {
    let receipt_path = build_receipt_path(root, target);
    let receipt: ReleaseBuildReceipt = read_canonical_json(&receipt_path)?;
    if receipt.schema != BUILD_RECEIPT_SCHEMA {
        bail!(
            "release build receipt schema must be {BUILD_RECEIPT_SCHEMA}, got {}",
            receipt.schema
        );
    }
    if &receipt.identity != identity {
        bail!("release build receipt identity differs from the tagged checkout");
    }
    if receipt.target != target {
        bail!(
            "release build receipt target mismatch: expected {target}, got {}",
            receipt.target
        );
    }

    let binary_name = release_binary_name(release, target);
    let binary_path = release_binary_path(root, target, &binary_name);
    let binary = binary_record(&binary_path, &binary_name)?;
    if receipt.binary != binary {
        bail!("release binary changed after its verified build receipt was written");
    }

    let tarball_name = artifact_name(release, target);
    let tarball_path = root.join(&tarball_name);
    create_deterministic_tarball(
        &tarball_path,
        &binary_path,
        &binary_name,
        identity.source_date_epoch,
    )?;
    verify_deterministic_tarball(&tarball_path, &binary, identity.source_date_epoch)?;

    let archive = ArchiveRecord {
        path: tarball_name.clone(),
        sha256: sha256_file(&tarball_path)?,
        size: regular_file_size(&tarball_path)?,
        format: "tar+gzip-v1".to_string(),
    };
    let provenance = ReleaseProvenance {
        schema: PROVENANCE_SCHEMA,
        identity: identity.clone(),
        target: target.to_string(),
        archive,
        binary,
    };
    write_json_atomic(&root.join(provenance_name(release, target)), &provenance)?;
    write_text_atomic(
        &root.join(format!("{tarball_name}.sha256")),
        &format!("{}  {tarball_name}\n", provenance.archive.sha256),
    )?;
    read_checked_sha256(root, &tarball_name)?;
    validate_release_artifact(root, release, target, identity)?;
    Ok(tarball_name)
}

pub(crate) fn validate_release_artifacts(
    dir: &Path,
    release: &ReleaseFile,
    identity: &ReleaseIdentity,
) -> Result<()> {
    for target in &release.targets {
        validate_release_artifact(dir, release, &target.target, identity)?;
    }
    Ok(())
}

fn validate_release_artifact(
    dir: &Path,
    release: &ReleaseFile,
    target: &str,
    identity: &ReleaseIdentity,
) -> Result<()> {
    let provenance_path = dir.join(provenance_name(release, target));
    let provenance: ReleaseProvenance = read_canonical_json(&provenance_path)?;
    if provenance.schema != PROVENANCE_SCHEMA {
        bail!(
            "release provenance schema must be {PROVENANCE_SCHEMA}, got {}",
            provenance.schema
        );
    }
    if &provenance.identity != identity {
        bail!(
            "release provenance identity mismatch for {}",
            provenance_path.display()
        );
    }
    if provenance.target != target {
        bail!(
            "release provenance target mismatch: expected {target}, got {}",
            provenance.target
        );
    }

    let tarball_name = artifact_name(release, target);
    if provenance.archive.path != tarball_name {
        bail!(
            "release provenance archive path mismatch: expected {tarball_name}, got {}",
            provenance.archive.path
        );
    }
    if provenance.archive.format != "tar+gzip-v1" {
        bail!(
            "unsupported release archive format {}",
            provenance.archive.format
        );
    }
    let tarball_path = dir.join(&tarball_name);
    let actual_size = regular_file_size(&tarball_path)?;
    if actual_size > MAX_RELEASE_ARCHIVE_SIZE {
        bail!(
            "release archive {tarball_name} is too large: {actual_size} bytes exceeds {MAX_RELEASE_ARCHIVE_SIZE}"
        );
    }
    if provenance.archive.size != actual_size {
        bail!(
            "release archive size mismatch for {tarball_name}: expected {}, got {actual_size}",
            provenance.archive.size
        );
    }
    let actual_sha256 = sha256_file(&tarball_path)?;
    if provenance.archive.sha256 != actual_sha256 {
        bail!(
            "release archive digest mismatch for {tarball_name}: expected {}, got {actual_sha256}",
            provenance.archive.sha256
        );
    }

    let binary_name = release_binary_name(release, target);
    if provenance.binary.path != binary_name {
        bail!(
            "release provenance binary path mismatch: expected {binary_name}, got {}",
            provenance.binary.path
        );
    }
    validate_binary_record(&provenance.binary)?;
    verify_deterministic_tarball(
        &tarball_path,
        &provenance.binary,
        identity.source_date_epoch,
    )
}

fn create_deterministic_tarball(
    output_path: &Path,
    binary_path: &Path,
    binary_name: &str,
    source_date_epoch: u64,
) -> Result<()> {
    if source_date_epoch > u32::MAX as u64 {
        bail!("SOURCE_DATE_EPOCH exceeds deterministic gzip timestamp range");
    }
    validate_release_binary_size(regular_file_size(binary_path)?)?;
    reject_existing_non_file(output_path)?;
    let (temp_path, output) = create_temp_file(output_path)?;
    let result = (|| {
        let encoder = GzBuilder::new()
            .mtime(source_date_epoch as u32)
            .operating_system(255)
            .write(BufWriter::new(output), Compression::best());
        let mut builder = Builder::new(encoder);
        let binary_size = regular_file_size(binary_path)?;
        let header = deterministic_tar_header(binary_name, binary_size, source_date_epoch)?;
        let mut binary = File::open(binary_path)
            .with_context(|| format!("could not read {}", binary_path.display()))?;
        builder
            .append(&header, &mut binary)
            .context("could not append release binary to deterministic archive")?;
        let encoder = builder
            .into_inner()
            .context("could not finalize deterministic tar stream")?;
        let mut output = encoder
            .finish()
            .context("could not finalize deterministic gzip stream")?;
        output.flush().context("could not flush release archive")?;
        output
            .get_ref()
            .sync_all()
            .context("could not sync release archive")?;
        Ok(())
    })();
    if let Err(error) = result {
        let _ = fs::remove_file(&temp_path);
        return Err(error);
    }
    replace_regular_file(&temp_path, output_path)
}

fn verify_deterministic_tarball(
    archive_path: &Path,
    binary: &BinaryRecord,
    source_date_epoch: u64,
) -> Result<()> {
    let archive_size = regular_file_size(archive_path)?;
    if archive_size > MAX_RELEASE_ARCHIVE_SIZE {
        bail!(
            "release archive is too large: {} has {archive_size} bytes, limit {MAX_RELEASE_ARCHIVE_SIZE}",
            archive_path.display()
        );
    }
    validate_binary_record(binary)?;

    let mut gzip_prefix = [0_u8; 10];
    File::open(archive_path)
        .with_context(|| format!("could not read {}", archive_path.display()))?
        .read_exact(&mut gzip_prefix)
        .with_context(|| {
            format!(
                "release archive has a truncated gzip header: {}",
                archive_path.display()
            )
        })?;
    let mut expected_gzip_prefix = [0x1f, 0x8b, 0x08, 0, 0, 0, 0, 0, 2, 255];
    expected_gzip_prefix[4..8].copy_from_slice(&(source_date_epoch as u32).to_le_bytes());
    if gzip_prefix != expected_gzip_prefix {
        bail!(
            "release archive must use a canonical gzip header: {}",
            archive_path.display()
        );
    }

    let file = File::open(archive_path)
        .with_context(|| format!("could not read {}", archive_path.display()))?;
    let mut decoder = GzDecoder::new(BufReader::new(file));
    let mut header = [0_u8; 512];
    decoder
        .read_exact(&mut header)
        .context("release archive has a truncated tar header")?;
    let expected_header = deterministic_tar_header(&binary.path, binary.size, source_date_epoch)?;
    if header != *expected_header.as_bytes() {
        bail!("release archive entry header is not canonical");
    }

    let mut hasher = Sha256::new();
    let mut size = 0_u64;
    let mut buffer = [0_u8; 64 * 1024];
    while size < binary.size {
        let remaining = binary.size - size;
        let limit = usize::try_from(remaining.min(buffer.len() as u64)).unwrap();
        let read = decoder
            .read(&mut buffer[..limit])
            .context("could not read release archive binary")?;
        if read == 0 {
            bail!("release archive binary is truncated");
        }
        size += read as u64;
        hasher.update(&buffer[..read]);
    }
    let digest = format!("{:x}", hasher.finalize());
    if size != binary.size || digest != binary.sha256 {
        bail!("release archive binary bytes differ from provenance");
    }

    let padding = (512 - binary.size % 512) % 512;
    require_zero_bytes(&mut decoder, padding, "release archive entry padding")?;
    require_zero_bytes(&mut decoder, 1024, "release archive terminator")?;
    let mut extra = [0_u8; 1];
    if decoder
        .read(&mut extra)
        .context("could not finish validating release gzip stream")?
        != 0
    {
        bail!("release archive contains data after its canonical tar terminator");
    }
    let mut compressed_input = decoder.into_inner();
    if compressed_input
        .read(&mut extra)
        .context("could not inspect release archive trailing bytes")?
        != 0
    {
        bail!("release archive contains trailing bytes after its gzip member");
    }
    Ok(())
}

fn require_zero_bytes<R: Read>(reader: &mut R, mut remaining: u64, label: &str) -> Result<()> {
    let mut buffer = [0_u8; 1024];
    while remaining > 0 {
        let limit = usize::try_from(remaining.min(buffer.len() as u64)).unwrap();
        reader
            .read_exact(&mut buffer[..limit])
            .with_context(|| format!("{label} is truncated"))?;
        if buffer[..limit].iter().any(|byte| *byte != 0) {
            bail!("{label} must contain only zero bytes");
        }
        remaining -= limit as u64;
    }
    Ok(())
}

fn deterministic_tar_header(path: &str, size: u64, source_date_epoch: u64) -> Result<Header> {
    let mut header = Header::new_ustar();
    header.set_size(size);
    header.set_mode(0o755);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(source_date_epoch);
    header
        .set_path(path)
        .with_context(|| format!("invalid archive binary path {path}"))?;
    header.set_cksum();
    Ok(header)
}

fn binary_record(path: &Path, logical_path: &str) -> Result<BinaryRecord> {
    let size = regular_file_size(path)?;
    validate_release_binary_size(size)
        .with_context(|| format!("invalid release binary {}", path.display()))?;
    let record = BinaryRecord {
        path: logical_path.to_string(),
        sha256: sha256_file(path)?,
        size,
    };
    validate_binary_record(&record)
        .with_context(|| format!("invalid release binary {}", path.display()))?;
    Ok(record)
}

fn validate_binary_record(binary: &BinaryRecord) -> Result<()> {
    if binary.path.is_empty() {
        bail!("release binary path cannot be empty");
    }
    validate_release_binary_size(binary.size)?;
    if binary.sha256.len() != 64
        || !binary.sha256.chars().all(|ch| ch.is_ascii_hexdigit())
        || binary.sha256 != binary.sha256.to_ascii_lowercase()
    {
        bail!("release binary sha256 must be 64 lowercase hexadecimal characters");
    }
    Ok(())
}

fn validate_release_binary_size(size: u64) -> Result<()> {
    if size == 0 {
        bail!("release binary is empty");
    }
    if size > MAX_RELEASE_BINARY_SIZE {
        bail!("release binary is too large: {size} bytes exceeds {MAX_RELEASE_BINARY_SIZE}");
    }
    Ok(())
}

fn validate_embedded_build_identity(path: &Path, identity: &ReleaseIdentity) -> Result<()> {
    for (field, value) in [
        ("commit", identity.commit.as_bytes()),
        ("build date", identity.build_date.as_bytes()),
    ] {
        if !file_contains(path, value)? {
            bail!(
                "release binary {} does not embed the verified {field} value",
                path.display()
            );
        }
    }
    Ok(())
}

fn file_contains(path: &Path, needle: &[u8]) -> Result<bool> {
    if needle.is_empty() {
        return Ok(true);
    }
    let mut reader = BufReader::new(
        File::open(path).with_context(|| format!("could not read {}", path.display()))?,
    );
    let mut carry = Vec::new();
    let mut chunk = [0_u8; 64 * 1024];
    loop {
        let read = reader
            .read(&mut chunk)
            .with_context(|| format!("could not inspect {}", path.display()))?;
        if read == 0 {
            return Ok(false);
        }
        carry.extend_from_slice(&chunk[..read]);
        if carry.windows(needle.len()).any(|window| window == needle) {
            return Ok(true);
        }
        let keep = needle.len().saturating_sub(1).min(carry.len());
        carry.drain(..carry.len() - keep);
    }
}

fn release_binary_path(root: &Path, target: &str, binary_name: &str) -> PathBuf {
    root.join("target")
        .join(target)
        .join("release")
        .join(binary_name)
}

fn build_receipt_path(root: &Path, target: &str) -> PathBuf {
    root.join("target")
        .join(target)
        .join("release")
        .join("vo.release-build.json")
}

fn regular_file_size(path: &Path) -> Result<u64> {
    let metadata = fs::symlink_metadata(path)
        .with_context(|| format!("could not inspect {}", path.display()))?;
    if !metadata.file_type().is_file() {
        bail!("release input must be a regular file: {}", path.display());
    }
    Ok(metadata.len())
}

fn read_canonical_json<T>(path: &Path) -> Result<T>
where
    T: DeserializeOwned + Serialize,
{
    let bytes = read_file_limited(path, MAX_RELEASE_EVIDENCE_SIZE, "JSON release evidence")?;
    let value: T = serde_json::from_slice(&bytes)
        .with_context(|| format!("could not parse {}", path.display()))?;
    let canonical = canonical_json(&value)?;
    if bytes != canonical {
        bail!("JSON release evidence is not canonical: {}", path.display());
    }
    Ok(value)
}

fn read_file_limited(path: &Path, limit: u64, label: &str) -> Result<Vec<u8>> {
    regular_file_size(path)?;
    let file = File::open(path).with_context(|| format!("could not read {}", path.display()))?;
    let mut reader = file.take(limit + 1);
    let mut bytes = Vec::new();
    reader
        .read_to_end(&mut bytes)
        .with_context(|| format!("could not read {}", path.display()))?;
    if bytes.len() as u64 > limit {
        bail!(
            "{label} is too large: {} exceeds {limit} bytes",
            path.display()
        );
    }
    Ok(bytes)
}

fn write_json_atomic<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    write_bytes_atomic(path, &canonical_json(value)?)
}

fn canonical_json<T: Serialize>(value: &T) -> Result<Vec<u8>> {
    let mut bytes = serde_json::to_vec_pretty(value)?;
    bytes.push(b'\n');
    Ok(bytes)
}

pub(crate) fn write_text_atomic(path: &Path, text: &str) -> Result<()> {
    write_bytes_atomic(path, text.as_bytes())
}

fn write_bytes_atomic(path: &Path, bytes: &[u8]) -> Result<()> {
    reject_existing_non_file(path)?;
    let (temp_path, mut file) = create_temp_file(path)?;
    let result = (|| {
        file.write_all(bytes)
            .with_context(|| format!("could not write {}", temp_path.display()))?;
        file.sync_all()
            .with_context(|| format!("could not sync {}", temp_path.display()))?;
        Ok(())
    })();
    if let Err(error) = result {
        let _ = fs::remove_file(&temp_path);
        return Err(error);
    }
    drop(file);
    replace_regular_file(&temp_path, path)
}

fn create_temp_file(destination: &Path) -> Result<(PathBuf, File)> {
    let parent = destination
        .parent()
        .ok_or_else(|| anyhow!("release output has no parent: {}", destination.display()))?;
    fs::create_dir_all(parent).with_context(|| format!("could not create {}", parent.display()))?;
    let name = destination
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| {
            anyhow!(
                "release output has an invalid filename: {}",
                destination.display()
            )
        })?;
    for _ in 0..32 {
        let sequence = TEMP_SEQUENCE.fetch_add(1, Ordering::Relaxed);
        let path = parent.join(format!(".{name}.tmp-{}-{sequence}", std::process::id()));
        match OpenOptions::new().write(true).create_new(true).open(&path) {
            Ok(file) => return Ok((path, file)),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => {
                return Err(error).with_context(|| format!("could not create {}", path.display()))
            }
        }
    }
    bail!(
        "could not allocate a temporary release output for {}",
        destination.display()
    )
}

fn replace_regular_file(source: &Path, destination: &Path) -> Result<()> {
    reject_existing_non_file(destination)?;
    match fs::rename(source, destination) {
        Ok(()) => return Ok(()),
        Err(error) if destination.exists() => {
            fs::remove_file(destination).with_context(|| {
                format!(
                    "could not replace existing release output {} after rename failed: {error}",
                    destination.display()
                )
            })?;
        }
        Err(error) => {
            return Err(error).with_context(|| {
                format!(
                    "could not move verified release output {} to {}",
                    source.display(),
                    destination.display()
                )
            })
        }
    }
    fs::rename(source, destination).with_context(|| {
        format!(
            "could not move verified release output {} to {}",
            source.display(),
            destination.display()
        )
    })
}

fn reject_existing_non_file(path: &Path) -> Result<()> {
    match fs::symlink_metadata(path) {
        Ok(metadata) if metadata.file_type().is_file() => Ok(()),
        Ok(_) => bail!(
            "release output path must be a regular file: {}",
            path.display()
        ),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error).with_context(|| format!("could not inspect {}", path.display())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{
        ReleaseCross, ReleaseHomebrew, ReleaseNotes, ReleasePackage, ReleaseSdk, ReleaseTarget,
    };
    use std::env;

    #[test]
    fn deterministic_archive_round_trips_and_repeats() {
        let root = unique_test_dir("vo-dev-release-archive");
        fs::create_dir_all(&root).unwrap();
        let binary_path = root.join("vo");
        fs::write(&binary_path, b"deterministic vo binary").unwrap();
        let binary = binary_record(&binary_path, "vo").unwrap();
        let first = root.join("first.tar.gz");
        let second = root.join("second.tar.gz");
        create_deterministic_tarball(&first, &binary_path, "vo", 1_700_000_000).unwrap();
        create_deterministic_tarball(&second, &binary_path, "vo", 1_700_000_000).unwrap();
        verify_deterministic_tarball(&first, &binary, 1_700_000_000).unwrap();
        assert_eq!(fs::read(first).unwrap(), fs::read(second).unwrap());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn deterministic_archive_rejects_noncanonical_header_and_trailing_bytes() {
        let root = unique_test_dir("vo-dev-release-archive-strict");
        fs::create_dir_all(&root).unwrap();
        let binary_path = root.join("vo");
        fs::write(&binary_path, b"strict vo binary").unwrap();
        let binary = binary_record(&binary_path, "vo").unwrap();

        let bad_header = root.join("bad-header.tar.gz");
        create_deterministic_tarball(&bad_header, &binary_path, "vo", 1_700_000_000).unwrap();
        let mut bytes = fs::read(&bad_header).unwrap();
        bytes[9] = 3;
        fs::write(&bad_header, bytes).unwrap();
        assert!(verify_deterministic_tarball(&bad_header, &binary, 1_700_000_000).is_err());

        let trailing = root.join("trailing.tar.gz");
        create_deterministic_tarball(&trailing, &binary_path, "vo", 1_700_000_000).unwrap();
        OpenOptions::new()
            .append(true)
            .open(&trailing)
            .unwrap()
            .write_all(b"trailing")
            .unwrap();
        assert!(verify_deterministic_tarball(&trailing, &binary, 1_700_000_000).is_err());

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn canonical_json_rejects_unknown_or_noncanonical_evidence() {
        let root = unique_test_dir("vo-dev-release-json");
        fs::create_dir_all(&root).unwrap();
        let path = root.join("receipt.json");
        fs::write(
            &path,
            r#"{"schema":1,"identity":{"tag":"v0.1.1","version":"0.1.1","commit":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","build_date":"2026-01-01T00:00:00+00:00","source_date_epoch":1700000000},"target":"x","binary":{"path":"vo","sha256":"x","size":1}}"#,
        )
        .unwrap();
        assert!(read_canonical_json::<ReleaseBuildReceipt>(&path).is_err());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn oversized_release_inputs_fail_before_unbounded_reads() {
        let root = unique_test_dir("vo-dev-release-size-limits");
        fs::create_dir_all(&root).unwrap();

        let evidence = root.join("evidence.json");
        File::create(&evidence)
            .unwrap()
            .set_len(MAX_RELEASE_EVIDENCE_SIZE + 1)
            .unwrap();
        let evidence_error = read_canonical_json::<ReleaseBuildReceipt>(&evidence).unwrap_err();
        assert!(evidence_error.to_string().contains("too large"));

        let binary = root.join("vo");
        File::create(&binary)
            .unwrap()
            .set_len(MAX_RELEASE_BINARY_SIZE + 1)
            .unwrap();
        let binary_error = binary_record(&binary, "vo").unwrap_err();
        assert!(binary_error
            .chain()
            .any(|cause| cause.to_string().contains("too large")));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn build_receipt_and_package_bind_complete_release_identity() {
        let root = unique_test_dir("vo-dev-release-package");
        let target = "x86_64-unknown-linux-gnu";
        let binary_dir = root.join("target").join(target).join("release");
        fs::create_dir_all(&binary_dir).unwrap();
        let identity = ReleaseIdentity {
            tag: "v0.1.1".to_string(),
            version: "0.1.1".to_string(),
            commit: "a".repeat(40),
            build_date: "2026-01-02T03:04:05+00:00".to_string(),
            source_date_epoch: 1_767_323_045,
        };
        fs::write(
            binary_dir.join("vo"),
            format!(
                "fake binary with {} and {}",
                identity.commit, identity.build_date
            ),
        )
        .unwrap();
        let release = sample_release(target);

        record_release_build(&root, &release, target, &identity).unwrap();
        let tarball = package_release_binary(&root, &release, target, &identity).unwrap();
        let first = fs::read(root.join(&tarball)).unwrap();
        package_release_binary(&root, &release, target, &identity).unwrap();
        assert_eq!(first, fs::read(root.join(&tarball)).unwrap());
        assert!(root.join(format!("{tarball}.sha256")).is_file());
        assert!(root.join(format!("{tarball}.provenance.json")).is_file());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn release_build_clears_stale_binary_and_receipt_before_cargo() {
        let root = unique_test_dir("vo-dev-release-clear-stale");
        let target = "x86_64-unknown-linux-gnu";
        let release = sample_release(target);
        let release_dir = root.join("target").join(target).join("release");
        fs::create_dir_all(&release_dir).unwrap();
        let binary = release_dir.join("vo");
        let receipt = release_dir.join("vo.release-build.json");
        fs::write(&binary, "stale binary").unwrap();
        fs::write(&receipt, "stale receipt").unwrap();

        clear_release_build_outputs(&root, &release, target).unwrap();

        assert!(!binary.exists());
        assert!(!receipt.exists());
        fs::remove_dir_all(root).unwrap();
    }

    fn sample_release(target: &str) -> ReleaseFile {
        ReleaseFile {
            version: 2,
            package: ReleasePackage {
                crate_name: "vo".to_string(),
                binary: "vo".to_string(),
                artifact_prefix: "vo".to_string(),
                build_args: vec![
                    "--release".to_string(),
                    "--locked".to_string(),
                    "-p".to_string(),
                    "vo".to_string(),
                ],
                release_opt_level: "3".to_string(),
                release_lto: "thin".to_string(),
            },
            sdk: ReleaseSdk {
                registry: "crates-io".to_string(),
                internal_standalone: Vec::new(),
                packages: vec!["vo-common-core".to_string()],
            },
            cross: ReleaseCross {
                version: "0.2.5".to_string(),
            },
            notes: ReleaseNotes {
                product_name: "Vo".to_string(),
                homebrew: Vec::new(),
                manual_install: "Install manually.".to_string(),
            },
            homebrew: ReleaseHomebrew {
                repository: "vo-lang/homebrew-vo".to_string(),
                formula_path: "Formula/vo.rb".to_string(),
            },
            targets: vec![ReleaseTarget {
                target: target.to_string(),
                os: "ubuntu-22.04".to_string(),
                use_cross: false,
            }],
        }
    }

    fn unique_test_dir(name: &str) -> PathBuf {
        let mut path = env::temp_dir();
        path.push(format!(
            "{name}-{}-{}",
            std::process::id(),
            TEMP_SEQUENCE.fetch_add(1, Ordering::Relaxed)
        ));
        if path.exists() {
            fs::remove_dir_all(&path).unwrap();
        }
        path
    }
}
