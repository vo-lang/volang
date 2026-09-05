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

const BUILD_RECEIPT_SCHEMA: u32 = 6;
const PROVENANCE_SCHEMA: u32 = 6;
const MAX_RELEASE_BINARY_SIZE: u64 = 512 * 1024 * 1024;
const MAX_RELEASE_ARCHIVE_SIZE: u64 = MAX_RELEASE_BINARY_SIZE * 4 + 8 * 1024 * 1024;
const MAX_RELEASE_EVIDENCE_SIZE: u64 = 1024 * 1024;
const MAX_UI_WEB_RUNTIME_FILES: usize = 512;
const UI_WEB_RUNTIME_ARCHIVE_ROOT: &str = "share/volang/ui-web";
static TEMP_SEQUENCE: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ReleaseBuildReceipt {
    schema: u32,
    identity: ReleaseIdentity,
    target: String,
    binary: BinaryRecord,
    aot_runtime: BinaryRecord,
    ui_aot_runtime: BinaryRecord,
    ui_web_runtime: Vec<BinaryRecord>,
    ui_product: UiProductEvidence,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ReleaseProvenance {
    schema: u32,
    identity: ReleaseIdentity,
    target: String,
    archive: ArchiveRecord,
    binary: BinaryRecord,
    aot_runtime: BinaryRecord,
    ui_aot_runtime: BinaryRecord,
    ui_web_runtime: Vec<BinaryRecord>,
    ui_product: UiProductEvidence,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct UiProductEvidence {
    schema: String,
    status: String,
    declaration_sha256: String,
    gate_count: u32,
    showcase_count: u32,
    ci_schema: String,
    ci_status: String,
    ci_profile: String,
    ci_commit: String,
    ci_bundle_sha256: String,
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

#[derive(Clone, Copy)]
struct UiWebRuntimeInput<'a> {
    root: &'a Path,
    records: &'a [BinaryRecord],
}

#[derive(Clone, Copy)]
struct ArchiveBinaryInput<'a> {
    path: &'a Path,
    name: &'a str,
}

pub(crate) fn clear_release_build_outputs(
    root: &Path,
    release: &ReleaseFile,
    target: &str,
) -> Result<()> {
    let binary_name = release_binary_name(release, target);
    let runtime_name = release_aot_runtime_name(target);
    let ui_runtime_name = release_ui_aot_runtime_name(target);
    for path in [
        release_binary_path(root, target, &binary_name),
        release_binary_path(root, target, runtime_name),
        release_binary_path(root, target, ui_runtime_name),
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
    let runtime_name = release_aot_runtime_name(target);
    let runtime_path = release_binary_path(root, target, runtime_name);
    let aot_runtime = binary_record(&runtime_path, runtime_name)?;
    let ui_runtime_name = release_ui_aot_runtime_name(target);
    let ui_runtime_path = release_binary_path(root, target, ui_runtime_name);
    let ui_aot_runtime = binary_record(&ui_runtime_path, ui_runtime_name)?;
    let ui_web_runtime = ui_web_runtime_records(root)?;
    let ui_product = ui_product_evidence(root, identity)?;
    validate_embedded_build_identity(&binary_path, identity)?;
    let receipt = ReleaseBuildReceipt {
        schema: BUILD_RECEIPT_SCHEMA,
        identity: identity.clone(),
        target: target.to_string(),
        binary,
        aot_runtime,
        ui_aot_runtime,
        ui_web_runtime,
        ui_product,
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
    let runtime_name = release_aot_runtime_name(target);
    let runtime_path = release_binary_path(root, target, runtime_name);
    let aot_runtime = binary_record(&runtime_path, runtime_name)?;
    if receipt.aot_runtime != aot_runtime {
        bail!("AOT runtime changed after its verified build receipt was written");
    }
    let ui_runtime_name = release_ui_aot_runtime_name(target);
    let ui_runtime_path = release_binary_path(root, target, ui_runtime_name);
    let ui_aot_runtime = binary_record(&ui_runtime_path, ui_runtime_name)?;
    if receipt.ui_aot_runtime != ui_aot_runtime {
        bail!("UI AOT runtime changed after its verified build receipt was written");
    }
    let ui_web_runtime = ui_web_runtime_records(root)?;
    if receipt.ui_web_runtime != ui_web_runtime {
        bail!("UI Web runtime changed after its verified build receipt was written");
    }
    let ui_product = ui_product_evidence(root, identity)?;
    if receipt.ui_product != ui_product {
        bail!("UI product certification changed after its verified build receipt was written");
    }

    let tarball_name = artifact_name(release, target);
    let tarball_path = root.join(&tarball_name);
    create_deterministic_tarball(
        &tarball_path,
        ArchiveBinaryInput {
            path: &binary_path,
            name: &binary_name,
        },
        ArchiveBinaryInput {
            path: &runtime_path,
            name: runtime_name,
        },
        ArchiveBinaryInput {
            path: &ui_runtime_path,
            name: ui_runtime_name,
        },
        UiWebRuntimeInput {
            root,
            records: &ui_web_runtime,
        },
        identity.source_date_epoch,
    )?;
    verify_deterministic_tarball(
        &tarball_path,
        &binary,
        &aot_runtime,
        &ui_aot_runtime,
        &ui_web_runtime,
        identity.source_date_epoch,
    )?;

    let archive = ArchiveRecord {
        path: tarball_name.clone(),
        sha256: sha256_file(&tarball_path)?,
        size: regular_file_size(&tarball_path)?,
        format: "tar+gzip-v4".to_string(),
    };
    let provenance = ReleaseProvenance {
        schema: PROVENANCE_SCHEMA,
        identity: identity.clone(),
        target: target.to_string(),
        archive,
        binary,
        aot_runtime,
        ui_aot_runtime,
        ui_web_runtime,
        ui_product,
    };
    write_json_atomic(&root.join(provenance_name(release, target)), &provenance)?;
    write_text_atomic(
        &root.join(format!("{tarball_name}.sha256")),
        &format!("{}  {tarball_name}\n", provenance.archive.sha256),
    )?;
    read_checked_sha256(root, &tarball_name)?;
    validate_release_artifact(root, root, release, target, identity)?;
    Ok(tarball_name)
}

pub(crate) fn validate_release_artifacts(
    root: &Path,
    dir: &Path,
    release: &ReleaseFile,
    identity: &ReleaseIdentity,
) -> Result<()> {
    for target in &release.targets {
        validate_release_artifact(root, dir, release, &target.target, identity)?;
    }
    Ok(())
}

fn validate_release_artifact(
    root: &Path,
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
    if provenance.archive.format != "tar+gzip-v4" {
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
    let runtime_name = release_aot_runtime_name(target);
    if provenance.aot_runtime.path != runtime_name {
        bail!(
            "release provenance AOT runtime path mismatch: expected {runtime_name}, got {}",
            provenance.aot_runtime.path
        );
    }
    validate_binary_record(&provenance.aot_runtime)?;
    let ui_runtime_name = release_ui_aot_runtime_name(target);
    if provenance.ui_aot_runtime.path != ui_runtime_name {
        bail!(
            "release provenance UI AOT runtime path mismatch: expected {ui_runtime_name}, got {}",
            provenance.ui_aot_runtime.path
        );
    }
    validate_binary_record(&provenance.ui_aot_runtime)?;
    validate_ui_web_runtime_records(&provenance.ui_web_runtime)?;
    let expected_ui_product = ui_product_evidence(root, identity)?;
    validate_ui_product_evidence(&provenance.ui_product, &identity.commit)?;
    if provenance.ui_product != expected_ui_product {
        bail!("release provenance CI certification differs from the verified bundle");
    }
    verify_deterministic_tarball(
        &tarball_path,
        &provenance.binary,
        &provenance.aot_runtime,
        &provenance.ui_aot_runtime,
        &provenance.ui_web_runtime,
        identity.source_date_epoch,
    )
}

fn ui_product_evidence(root: &Path, identity: &ReleaseIdentity) -> Result<UiProductEvidence> {
    #[cfg(test)]
    if !root.join("ui/certification.toml").exists() {
        return Ok(UiProductEvidence {
            schema: "volang.ui.product-evidence.v2".to_string(),
            status: "product-certified".to_string(),
            declaration_sha256: "0".repeat(64),
            gate_count: 12,
            showcase_count: 5,
            ci_schema: "volang.ci.certification.v1".to_string(),
            ci_status: "certified".to_string(),
            ci_profile: "main".to_string(),
            ci_commit: identity.commit.clone(),
            ci_bundle_sha256: "0".repeat(64),
        });
    }
    let (declaration_sha256, gate_count, showcase_count) = ui_declaration_evidence(root)?;
    let configured = std::env::var("VO_CI_CERTIFICATION_PATH")
        .unwrap_or_else(|_| "target/ci/release-input/certification.json".to_string());
    let relative = Path::new(&configured);
    if configured.is_empty()
        || relative.is_absolute()
        || relative.components().any(|component| {
            matches!(
                component,
                std::path::Component::ParentDir
                    | std::path::Component::RootDir
                    | std::path::Component::Prefix(_)
            )
        })
    {
        bail!("VO_CI_CERTIFICATION_PATH must be repository-relative");
    }
    let certification = crate::ci::verify_release_bundle(root, &root.join(relative))?;
    if certification.commit != identity.commit {
        bail!("UI certification commit differs from the release identity");
    }
    let evidence = UiProductEvidence {
        schema: "volang.ui.product-evidence.v2".to_string(),
        status: "product-certified".to_string(),
        declaration_sha256,
        gate_count,
        showcase_count,
        ci_schema: "volang.ci.certification.v1".to_string(),
        ci_status: certification.status,
        ci_profile: certification.profile,
        ci_commit: certification.commit,
        ci_bundle_sha256: certification.sha256,
    };
    validate_ui_product_evidence(&evidence, &identity.commit)?;
    Ok(evidence)
}

fn ui_declaration_evidence(root: &Path) -> Result<(String, u32, u32)> {
    const SOURCES: [&str; 17] = [
        "ui/certification.toml",
        "ui/product-certification.toml",
        "ui/product-roadmap.toml",
        "ui/capabilities.toml",
        "ui/delivery.toml",
        "ui/quality-matrix.toml",
        "ui/module-profiles.toml",
        "ui/kit/catalog.toml",
        "ui/docs/getting-started.md",
        "ui/docs/authoring-guide.md",
        "ui/docs/testing-troubleshooting.md",
        "ui/docs/accessibility-localization.md",
        "ui/docs/security.md",
        "ui/docs/compatibility-migration.md",
        "ui/docs/contributing-support.md",
        "ui/docs/release-notes-1.0.md",
        "ui/docs/release-policy.md",
    ];
    let status = crate::ui_certification::certification_status(root)?;
    if status != "declaration-valid" {
        bail!("UI declaration validation did not succeed");
    }
    let mut hasher = Sha256::new();
    hasher.update(b"volang-ui-declaration-evidence-v1\0");
    for relative in SOURCES {
        let path = root.join(relative);
        let metadata = fs::symlink_metadata(&path)
            .with_context(|| format!("could not inspect UI product evidence {}", path.display()))?;
        if !metadata.file_type().is_file() || metadata.len() > MAX_RELEASE_EVIDENCE_SIZE {
            bail!(
                "UI product evidence must be a bounded regular file: {}",
                path.display()
            );
        }
        let bytes = fs::read(&path)
            .with_context(|| format!("could not read UI product evidence {}", path.display()))?;
        hasher.update(relative.as_bytes());
        hasher.update([0]);
        hasher.update((bytes.len() as u64).to_le_bytes());
        hasher.update(&bytes);
    }
    Ok((format!("{:x}", hasher.finalize()), 12, 5))
}

fn validate_ui_product_evidence(evidence: &UiProductEvidence, commit: &str) -> Result<()> {
    if evidence.schema != "volang.ui.product-evidence.v2"
        || evidence.status != "product-certified"
        || !valid_sha256(&evidence.declaration_sha256)
        || evidence.gate_count != 12
        || evidence.showcase_count != 5
        || evidence.ci_schema != "volang.ci.certification.v1"
        || evidence.ci_status != "certified"
        || evidence.ci_profile != "main"
        || evidence.ci_commit != commit
        || !valid_sha256(&evidence.ci_bundle_sha256)
    {
        bail!("release provenance contains invalid UI product evidence");
    }
    Ok(())
}

fn valid_sha256(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
}

fn create_deterministic_tarball(
    output_path: &Path,
    binary: ArchiveBinaryInput<'_>,
    runtime: ArchiveBinaryInput<'_>,
    ui_runtime: ArchiveBinaryInput<'_>,
    ui_web_runtime: UiWebRuntimeInput<'_>,
    source_date_epoch: u64,
) -> Result<()> {
    if source_date_epoch > u32::MAX as u64 {
        bail!("SOURCE_DATE_EPOCH exceeds deterministic gzip timestamp range");
    }
    validate_release_binary_size(regular_file_size(binary.path)?)?;
    validate_release_binary_size(regular_file_size(runtime.path)?)?;
    validate_release_binary_size(regular_file_size(ui_runtime.path)?)?;
    validate_ui_web_runtime_records(ui_web_runtime.records)?;
    reject_existing_non_file(output_path)?;
    let (temp_path, output) = create_temp_file(output_path)?;
    let result = (|| {
        let encoder = GzBuilder::new()
            .mtime(source_date_epoch as u32)
            .operating_system(255)
            .write(BufWriter::new(output), Compression::best());
        let mut builder = Builder::new(encoder);
        let binary_size = regular_file_size(binary.path)?;
        let header = deterministic_tar_header(binary.name, binary_size, source_date_epoch)?;
        let mut binary_file = File::open(binary.path)
            .with_context(|| format!("could not read {}", binary.path.display()))?;
        builder
            .append(&header, &mut binary_file)
            .context("could not append release binary to deterministic archive")?;
        let runtime_size = regular_file_size(runtime.path)?;
        let header = deterministic_tar_header(runtime.name, runtime_size, source_date_epoch)?;
        let mut runtime_file = File::open(runtime.path)
            .with_context(|| format!("could not read {}", runtime.path.display()))?;
        builder
            .append(&header, &mut runtime_file)
            .context("could not append AOT runtime to deterministic archive")?;
        let ui_runtime_size = regular_file_size(ui_runtime.path)?;
        let header = deterministic_tar_header(ui_runtime.name, ui_runtime_size, source_date_epoch)?;
        let mut ui_runtime_file = File::open(ui_runtime.path)
            .with_context(|| format!("could not read {}", ui_runtime.path.display()))?;
        builder
            .append(&header, &mut ui_runtime_file)
            .context("could not append UI AOT runtime to deterministic archive")?;
        for asset in ui_web_runtime.records {
            let source = ui_web_runtime_source_path(ui_web_runtime.root, asset)?;
            let header = deterministic_tar_header_with_mode(
                &asset.path,
                asset.size,
                source_date_epoch,
                0o644,
            )?;
            let mut file = File::open(&source)
                .with_context(|| format!("could not read {}", source.display()))?;
            builder
                .append(&header, &mut file)
                .with_context(|| format!("could not append UI Web runtime {}", asset.path))?;
        }
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
    aot_runtime: &BinaryRecord,
    ui_aot_runtime: &BinaryRecord,
    ui_web_runtime: &[BinaryRecord],
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
    validate_binary_record(aot_runtime)?;
    validate_binary_record(ui_aot_runtime)?;
    validate_ui_web_runtime_records(ui_web_runtime)?;

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
    verify_tar_entry(&mut decoder, binary, source_date_epoch, "release binary")?;
    verify_tar_entry(&mut decoder, aot_runtime, source_date_epoch, "AOT runtime")?;
    verify_tar_entry(
        &mut decoder,
        ui_aot_runtime,
        source_date_epoch,
        "UI AOT runtime",
    )?;
    for asset in ui_web_runtime {
        verify_tar_entry_with_mode(
            &mut decoder,
            asset,
            source_date_epoch,
            0o644,
            "UI Web runtime",
        )?;
    }
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

fn verify_tar_entry<R: Read>(
    reader: &mut R,
    record: &BinaryRecord,
    source_date_epoch: u64,
    label: &str,
) -> Result<()> {
    verify_tar_entry_with_mode(reader, record, source_date_epoch, 0o755, label)
}

fn verify_tar_entry_with_mode<R: Read>(
    reader: &mut R,
    record: &BinaryRecord,
    source_date_epoch: u64,
    mode: u32,
    label: &str,
) -> Result<()> {
    let mut header = [0_u8; 512];
    reader
        .read_exact(&mut header)
        .with_context(|| format!("release archive has a truncated {label} tar header"))?;
    let expected_header =
        deterministic_tar_header_with_mode(&record.path, record.size, source_date_epoch, mode)?;
    if header != *expected_header.as_bytes() {
        bail!("release archive {label} header is not canonical");
    }

    let mut hasher = Sha256::new();
    let mut size = 0_u64;
    let mut buffer = [0_u8; 64 * 1024];
    while size < record.size {
        let remaining = record.size - size;
        let limit = usize::try_from(remaining.min(buffer.len() as u64)).unwrap();
        let read = reader
            .read(&mut buffer[..limit])
            .with_context(|| format!("could not read release archive {label}"))?;
        if read == 0 {
            bail!("release archive {label} is truncated");
        }
        size += read as u64;
        hasher.update(&buffer[..read]);
    }
    let digest = format!("{:x}", hasher.finalize());
    if size != record.size || digest != record.sha256 {
        bail!("release archive {label} bytes differ from provenance");
    }
    let padding = (512 - record.size % 512) % 512;
    require_zero_bytes(reader, padding, &format!("release archive {label} padding"))
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
    deterministic_tar_header_with_mode(path, size, source_date_epoch, 0o755)
}

fn deterministic_tar_header_with_mode(
    path: &str,
    size: u64,
    source_date_epoch: u64,
    mode: u32,
) -> Result<Header> {
    let mut header = Header::new_ustar();
    header.set_size(size);
    header.set_mode(mode);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(source_date_epoch);
    header
        .set_path(path)
        .with_context(|| format!("invalid archive binary path {path}"))?;
    header.set_cksum();
    Ok(header)
}

fn ui_web_runtime_records(root: &Path) -> Result<Vec<BinaryRecord>> {
    let runtime_root = root.join("lang/crates/vo-web");
    let mut files = Vec::new();
    for relative in ["dist", "pkg", "aot-support"] {
        collect_ui_web_runtime_files(&runtime_root.join(relative), &mut files)?;
    }
    files.sort();
    if files.len() > MAX_UI_WEB_RUNTIME_FILES {
        bail!(
            "UI Web runtime has {} files, limit {MAX_UI_WEB_RUNTIME_FILES}",
            files.len()
        );
    }
    let mut records = Vec::with_capacity(files.len());
    for file in files {
        let relative = file.strip_prefix(&runtime_root).with_context(|| {
            format!(
                "UI Web runtime file escaped {}: {}",
                runtime_root.display(),
                file.display()
            )
        })?;
        let relative = archive_relative_path(relative)?;
        records.push(binary_record(
            &file,
            &format!("{UI_WEB_RUNTIME_ARCHIVE_ROOT}/{relative}"),
        )?);
    }
    validate_ui_web_runtime_records(&records)?;
    Ok(records)
}

fn collect_ui_web_runtime_files(directory: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    let metadata = fs::symlink_metadata(directory)
        .with_context(|| format!("could not inspect UI Web runtime {}", directory.display()))?;
    if !metadata.file_type().is_dir() {
        bail!(
            "UI Web runtime path must be a directory: {}",
            directory.display()
        );
    }
    let mut entries = directory
        .read_dir()
        .with_context(|| format!("could not read UI Web runtime {}", directory.display()))?
        .collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(std::fs::DirEntry::file_name);
    for entry in entries {
        let path = entry.path();
        let metadata = fs::symlink_metadata(&path)
            .with_context(|| format!("could not inspect UI Web runtime {}", path.display()))?;
        if metadata.file_type().is_symlink() {
            bail!("UI Web runtime cannot contain symlinks: {}", path.display());
        }
        if metadata.file_type().is_dir() {
            collect_ui_web_runtime_files(&path, files)?;
        } else if metadata.file_type().is_file() {
            files.push(path);
            if files.len() > MAX_UI_WEB_RUNTIME_FILES {
                bail!("UI Web runtime file count exceeds {MAX_UI_WEB_RUNTIME_FILES}");
            }
        } else {
            bail!(
                "UI Web runtime contains a special filesystem entry: {}",
                path.display()
            );
        }
    }
    Ok(())
}

fn archive_relative_path(path: &Path) -> Result<String> {
    let mut parts = Vec::new();
    for component in path.components() {
        match component {
            std::path::Component::Normal(part) => {
                let part = part.to_str().ok_or_else(|| {
                    anyhow!("UI Web runtime path is not UTF-8: {}", path.display())
                })?;
                if part.is_empty() {
                    bail!("UI Web runtime path contains an empty component");
                }
                parts.push(part);
            }
            _ => bail!("UI Web runtime path is not relative: {}", path.display()),
        }
    }
    if parts.is_empty() {
        bail!("UI Web runtime path cannot be empty");
    }
    Ok(parts.join("/"))
}

fn validate_ui_web_runtime_records(records: &[BinaryRecord]) -> Result<()> {
    if records.is_empty() || records.len() > MAX_UI_WEB_RUNTIME_FILES {
        bail!("UI Web runtime file inventory is empty or exceeds its bound");
    }
    let prefix = format!("{UI_WEB_RUNTIME_ARCHIVE_ROOT}/");
    let mut previous = None;
    let mut total_size = 0_u64;
    for record in records {
        validate_binary_record(record)?;
        let relative = record.path.strip_prefix(&prefix).ok_or_else(|| {
            anyhow!(
                "UI Web runtime archive path is outside {UI_WEB_RUNTIME_ARCHIVE_ROOT}: {}",
                record.path
            )
        })?;
        archive_relative_path(Path::new(relative))?;
        if previous.is_some_and(|path: &str| path >= record.path.as_str()) {
            bail!("UI Web runtime inventory must be strictly sorted and unique");
        }
        previous = Some(record.path.as_str());
        total_size = total_size
            .checked_add(record.size)
            .ok_or_else(|| anyhow!("UI Web runtime total size overflow"))?;
    }
    if total_size > MAX_RELEASE_BINARY_SIZE {
        bail!("UI Web runtime is too large: {total_size} bytes exceeds {MAX_RELEASE_BINARY_SIZE}");
    }
    for required in [
        "dist/index.js",
        "dist/ui_dom.js",
        "dist/ui_system.js",
        "pkg/vo_web.js",
        "pkg/vo_web_bg.wasm",
        "aot-support/vo_aot_support_wasm.js",
        "aot-support/vo_aot_support_wasm_bg.wasm",
    ] {
        let expected = format!("{UI_WEB_RUNTIME_ARCHIVE_ROOT}/{required}");
        if records
            .binary_search_by_key(&expected.as_str(), |record| record.path.as_str())
            .is_err()
        {
            bail!("UI Web runtime is missing {required}");
        }
    }
    Ok(())
}

fn ui_web_runtime_source_path(root: &Path, record: &BinaryRecord) -> Result<PathBuf> {
    let prefix = format!("{UI_WEB_RUNTIME_ARCHIVE_ROOT}/");
    let relative = record
        .path
        .strip_prefix(&prefix)
        .ok_or_else(|| anyhow!("invalid UI Web runtime archive path {}", record.path))?;
    archive_relative_path(Path::new(relative))?;
    Ok(root.join("lang/crates/vo-web").join(relative))
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

fn release_aot_runtime_name(target: &str) -> &'static str {
    if target.contains("windows") {
        "vo_aot_runtime.lib"
    } else {
        "libvo_aot_runtime.a"
    }
}

fn release_ui_aot_runtime_name(target: &str) -> &'static str {
    if target.contains("windows") {
        "vo_ui_aot_runtime_native.lib"
    } else {
        "libvo_ui_aot_runtime_native.a"
    }
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
    use crate::config::{ReleaseHomebrew, ReleaseNotes, ReleasePackage, ReleaseSdk, ReleaseTarget};
    use std::env;

    #[test]
    fn deterministic_archive_round_trips_and_repeats() {
        let root = unique_test_dir("vo-dev-release-archive");
        fs::create_dir_all(&root).unwrap();
        let binary_path = root.join("vo");
        fs::write(&binary_path, b"deterministic vo binary").unwrap();
        let binary = binary_record(&binary_path, "vo").unwrap();
        let runtime_path = root.join("libvo_aot_runtime.a");
        fs::write(&runtime_path, b"deterministic AOT runtime").unwrap();
        let runtime = binary_record(&runtime_path, "libvo_aot_runtime.a").unwrap();
        let ui_runtime_path = root.join("libvo_ui_aot_runtime_native.a");
        fs::write(&ui_runtime_path, b"deterministic UI AOT runtime").unwrap();
        let ui_runtime = binary_record(&ui_runtime_path, "libvo_ui_aot_runtime_native.a").unwrap();
        let ui_web_runtime = write_ui_web_runtime_fixture(&root);
        let first = root.join("first.tar.gz");
        let second = root.join("second.tar.gz");
        create_deterministic_tarball(
            &first,
            ArchiveBinaryInput {
                path: &binary_path,
                name: "vo",
            },
            ArchiveBinaryInput {
                path: &runtime_path,
                name: "libvo_aot_runtime.a",
            },
            ArchiveBinaryInput {
                path: &ui_runtime_path,
                name: "libvo_ui_aot_runtime_native.a",
            },
            UiWebRuntimeInput {
                root: &root,
                records: &ui_web_runtime,
            },
            1_700_000_000,
        )
        .unwrap();
        create_deterministic_tarball(
            &second,
            ArchiveBinaryInput {
                path: &binary_path,
                name: "vo",
            },
            ArchiveBinaryInput {
                path: &runtime_path,
                name: "libvo_aot_runtime.a",
            },
            ArchiveBinaryInput {
                path: &ui_runtime_path,
                name: "libvo_ui_aot_runtime_native.a",
            },
            UiWebRuntimeInput {
                root: &root,
                records: &ui_web_runtime,
            },
            1_700_000_000,
        )
        .unwrap();
        verify_deterministic_tarball(
            &first,
            &binary,
            &runtime,
            &ui_runtime,
            &ui_web_runtime,
            1_700_000_000,
        )
        .unwrap();
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
        let runtime_path = root.join("libvo_aot_runtime.a");
        fs::write(&runtime_path, b"strict AOT runtime").unwrap();
        let runtime = binary_record(&runtime_path, "libvo_aot_runtime.a").unwrap();
        let ui_runtime_path = root.join("libvo_ui_aot_runtime_native.a");
        fs::write(&ui_runtime_path, b"strict UI AOT runtime").unwrap();
        let ui_runtime = binary_record(&ui_runtime_path, "libvo_ui_aot_runtime_native.a").unwrap();
        let ui_web_runtime = write_ui_web_runtime_fixture(&root);

        let bad_header = root.join("bad-header.tar.gz");
        create_deterministic_tarball(
            &bad_header,
            ArchiveBinaryInput {
                path: &binary_path,
                name: "vo",
            },
            ArchiveBinaryInput {
                path: &runtime_path,
                name: "libvo_aot_runtime.a",
            },
            ArchiveBinaryInput {
                path: &ui_runtime_path,
                name: "libvo_ui_aot_runtime_native.a",
            },
            UiWebRuntimeInput {
                root: &root,
                records: &ui_web_runtime,
            },
            1_700_000_000,
        )
        .unwrap();
        let mut bytes = fs::read(&bad_header).unwrap();
        bytes[9] = 3;
        fs::write(&bad_header, bytes).unwrap();
        assert!(verify_deterministic_tarball(
            &bad_header,
            &binary,
            &runtime,
            &ui_runtime,
            &ui_web_runtime,
            1_700_000_000,
        )
        .is_err());

        let trailing = root.join("trailing.tar.gz");
        create_deterministic_tarball(
            &trailing,
            ArchiveBinaryInput {
                path: &binary_path,
                name: "vo",
            },
            ArchiveBinaryInput {
                path: &runtime_path,
                name: "libvo_aot_runtime.a",
            },
            ArchiveBinaryInput {
                path: &ui_runtime_path,
                name: "libvo_ui_aot_runtime_native.a",
            },
            UiWebRuntimeInput {
                root: &root,
                records: &ui_web_runtime,
            },
            1_700_000_000,
        )
        .unwrap();
        OpenOptions::new()
            .append(true)
            .open(&trailing)
            .unwrap()
            .write_all(b"trailing")
            .unwrap();
        assert!(verify_deterministic_tarball(
            &trailing,
            &binary,
            &runtime,
            &ui_runtime,
            &ui_web_runtime,
            1_700_000_000,
        )
        .is_err());

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
        fs::write(
            binary_dir.join("libvo_aot_runtime.a"),
            "fake static AOT runtime",
        )
        .unwrap();
        fs::write(
            binary_dir.join("libvo_ui_aot_runtime_native.a"),
            "fake static UI AOT runtime",
        )
        .unwrap();
        write_ui_web_runtime_fixture(&root);
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
    fn repository_ui_declaration_evidence_is_bounded_and_valid() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let (digest, gate_count, showcase_count) = ui_declaration_evidence(&root).unwrap();
        assert!(valid_sha256(&digest));
        assert_ne!(digest, "0".repeat(64));
        assert_eq!(gate_count, 12);
        assert_eq!(showcase_count, 5);
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
        let runtime = release_dir.join("libvo_aot_runtime.a");
        let ui_runtime = release_dir.join("libvo_ui_aot_runtime_native.a");
        fs::write(&binary, "stale binary").unwrap();
        fs::write(&runtime, "stale runtime").unwrap();
        fs::write(&ui_runtime, "stale UI runtime").unwrap();
        fs::write(&receipt, "stale receipt").unwrap();

        clear_release_build_outputs(&root, &release, target).unwrap();

        assert!(!binary.exists());
        assert!(!runtime.exists());
        assert!(!ui_runtime.exists());
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
            }],
        }
    }

    fn write_ui_web_runtime_fixture(root: &Path) -> Vec<BinaryRecord> {
        let runtime = root.join("lang/crates/vo-web");
        for relative in [
            "dist/index.js",
            "dist/ui_dom.js",
            "dist/ui_system.js",
            "pkg/vo_web.js",
            "pkg/vo_web_bg.wasm",
            "pkg/snippets/runtime/inline0.js",
            "aot-support/vo_aot_support_wasm.js",
            "aot-support/vo_aot_support_wasm_bg.wasm",
        ] {
            let path = runtime.join(relative);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(&path, format!("fixture:{relative}")).unwrap();
        }
        ui_web_runtime_records(root).unwrap()
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
