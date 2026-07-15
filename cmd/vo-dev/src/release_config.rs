use crate::config::{ReleaseFile, ReleaseTarget};
use anyhow::{anyhow, bail, Context, Result};
use sha2::{Digest, Sha256};
use std::collections::HashSet;
use std::fs;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};

const MAX_SHA256_EVIDENCE_SIZE: u64 = 1024;

pub(crate) fn lint_release_file(release: &ReleaseFile) -> Result<()> {
    if release.version != 2 {
        bail!("eng/release.toml version must be 2");
    }
    if release.package.crate_name.trim().is_empty() {
        bail!("release package crate cannot be empty");
    }
    validate_release_token(
        "release package crate",
        &release.package.crate_name,
        &['-', '_'],
    )?;
    if release.package.binary.trim().is_empty() {
        bail!("release package binary cannot be empty");
    }
    validate_release_token(
        "release package binary",
        &release.package.binary,
        &['-', '_'],
    )?;
    if release.package.artifact_prefix.trim().is_empty() {
        bail!("release package artifact_prefix cannot be empty");
    }
    validate_release_token(
        "release package artifact_prefix",
        &release.package.artifact_prefix,
        &['-'],
    )?;
    if release.package.build_args.is_empty() {
        bail!("release package build_args cannot be empty");
    }
    if release
        .package
        .build_args
        .iter()
        .any(|arg| arg.trim().is_empty() || arg.trim() != arg)
    {
        bail!("release package build_args cannot contain empty or padded arguments");
    }
    if release
        .package
        .build_args
        .iter()
        .any(|arg| arg == "--target" || arg.starts_with("--target="))
    {
        bail!(
            "release package build_args must not declare --target; targets come from eng/release.toml"
        );
    }
    if !release
        .package
        .build_args
        .iter()
        .any(|arg| arg == "--release")
    {
        bail!("release package build_args must include --release");
    }
    if !release
        .package
        .build_args
        .iter()
        .any(|arg| arg == "--locked")
    {
        bail!("release package build_args must include --locked");
    }
    if !build_args_reference_crate(&release.package.build_args, &release.package.crate_name) {
        bail!(
            "release package build_args must build declared crate {}",
            release.package.crate_name
        );
    }
    if !matches!(
        release.package.release_opt_level.as_str(),
        "0" | "1" | "2" | "3" | "s" | "z"
    ) {
        bail!(
            "release package release_opt_level has invalid value {}",
            release.package.release_opt_level
        );
    }
    if !matches!(
        release.package.release_lto.as_str(),
        "off" | "false" | "thin" | "fat" | "true"
    ) {
        bail!(
            "release package release_lto has invalid value {}",
            release.package.release_lto
        );
    }
    if release.sdk.registry != "crates-io" {
        bail!("release sdk registry must be crates-io");
    }
    if release.sdk.packages.is_empty() {
        bail!("release sdk packages cannot be empty");
    }
    let mut sdk_packages = HashSet::new();
    for package in &release.sdk.packages {
        validate_release_token("release sdk package", package, &['-', '_'])?;
        if !sdk_packages.insert(package) {
            bail!("duplicate release sdk package {package}");
        }
    }
    let mut standalone_packages = HashSet::new();
    for package in &release.sdk.internal_standalone {
        validate_release_path("release sdk internal_standalone", package)?;
        if !standalone_packages.insert(package) {
            bail!("duplicate release sdk internal_standalone path {package}");
        }
    }
    if release.cross.version.trim().is_empty() {
        bail!("release cross version cannot be empty");
    }
    validate_semver_like("release cross version", &release.cross.version)?;
    if release.notes.product_name.trim().is_empty() {
        bail!("release notes product_name cannot be empty");
    }
    if release.notes.manual_install.trim().is_empty() {
        bail!("release notes manual_install cannot be empty");
    }
    if release.homebrew.repository.trim().is_empty() {
        bail!("release homebrew repository cannot be empty");
    }
    validate_github_repository("release homebrew repository", &release.homebrew.repository)?;
    if release.homebrew.formula_path.trim().is_empty() {
        bail!("release homebrew formula_path cannot be empty");
    }
    validate_release_path(
        "release homebrew formula_path",
        &release.homebrew.formula_path,
    )?;
    if !release.homebrew.formula_path.ends_with(".rb") {
        bail!("release homebrew formula_path must point to a Ruby formula");
    }
    if release.targets.is_empty() {
        bail!("release targets cannot be empty");
    }
    let mut seen = HashSet::new();
    for target in &release.targets {
        if !seen.insert(target.target.clone()) {
            bail!("duplicate release target {}", target.target);
        }
        if target.target.trim().is_empty() || target.target.trim() != target.target {
            bail!("release target cannot be empty or padded");
        }
        validate_release_token("release target", &target.target, &['-', '_'])?;
        if target.os.trim().is_empty() {
            bail!("release target {} os cannot be empty", target.target);
        }
        validate_release_token("release target os", &target.os, &['-', '_', '.'])?;
    }
    Ok(())
}

fn build_args_reference_crate(args: &[String], crate_name: &str) -> bool {
    args.windows(2)
        .any(|pair| matches!(pair[0].as_str(), "-p" | "--package") && pair[1] == crate_name)
        || args
            .iter()
            .any(|arg| arg.strip_prefix("--package=") == Some(crate_name))
}

fn validate_release_token(field: &str, value: &str, extra: &[char]) -> Result<()> {
    if value.trim().is_empty() {
        bail!("{field} cannot be empty");
    }
    if value.trim() != value {
        bail!("{field} cannot contain surrounding whitespace");
    }
    if !value
        .chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || extra.contains(&ch))
    {
        bail!("{field} contains unsupported characters: {value}");
    }
    Ok(())
}

fn validate_release_path(field: &str, value: &str) -> Result<()> {
    if value.trim().is_empty() {
        bail!("{field} cannot be empty");
    }
    if value.trim() != value {
        bail!("{field} cannot contain surrounding whitespace");
    }
    if Path::new(value).is_absolute() {
        bail!("{field} must be repository-relative");
    }
    if value.contains(':') || value.contains('\\') || value.contains("//") {
        bail!("{field} must use clean repository-relative / separators");
    }
    let trimmed = value.trim_end_matches('/');
    if trimmed.is_empty() {
        bail!("{field} cannot point to repository root");
    }
    for segment in trimmed.split('/') {
        if segment.is_empty() || segment == "." || segment == ".." {
            bail!("{field} contains invalid segment {segment:?}");
        }
    }
    Ok(())
}

fn validate_github_repository(field: &str, value: &str) -> Result<()> {
    let Some((owner, repo)) = value.split_once('/') else {
        bail!("{field} must be owner/repo");
    };
    if repo.contains('/') {
        bail!("{field} must contain exactly one slash");
    }
    validate_release_token(&format!("{field} owner"), owner, &['-'])?;
    validate_release_token(&format!("{field} repo"), repo, &['-', '_'])?;
    Ok(())
}

fn validate_semver_like(field: &str, value: &str) -> Result<()> {
    if value.trim() != value || value.trim().is_empty() {
        bail!("{field} cannot be empty or padded");
    }
    let parts = value.split('.').collect::<Vec<_>>();
    if parts.len() < 2
        || parts
            .iter()
            .any(|part| part.is_empty() || !part.chars().all(|ch| ch.is_ascii_digit()))
    {
        bail!("{field} must be a numeric dotted version");
    }
    Ok(())
}

pub(crate) fn release_target<'a>(
    release: &'a ReleaseFile,
    target: &str,
) -> Result<&'a ReleaseTarget> {
    release
        .targets
        .iter()
        .find(|item| item.target == target)
        .ok_or_else(|| anyhow!("unknown release target: {target}"))
}

pub(crate) fn artifact_name(release: &ReleaseFile, target: &str) -> String {
    format!("{}-{target}.tar.gz", release.package.artifact_prefix)
}

pub(crate) fn provenance_name(release: &ReleaseFile, target: &str) -> String {
    format!("{}.provenance.json", artifact_name(release, target))
}

pub(crate) fn release_binary_name(release: &ReleaseFile, target: &str) -> String {
    if target.contains("windows") {
        format!("{}.exe", release.package.binary)
    } else {
        release.package.binary.clone()
    }
}

pub(crate) fn sha256_file(path: &Path) -> Result<String> {
    let metadata = fs::symlink_metadata(path)
        .with_context(|| format!("could not inspect {}", path.display()))?;
    if !metadata.file_type().is_file() {
        bail!("sha256 input must be a regular file: {}", path.display());
    }
    let file =
        fs::File::open(path).with_context(|| format!("could not read {}", path.display()))?;
    let mut reader = BufReader::new(file);
    let mut hasher = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let read = reader
            .read(&mut buffer)
            .with_context(|| format!("could not hash {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(format!("{:x}", hasher.finalize()))
}

pub(crate) fn release_artifact_files(release: &ReleaseFile, dir: &Path) -> Result<Vec<PathBuf>> {
    let mut expected_names = HashSet::new();
    for target in &release.targets {
        let tarball = artifact_name(release, &target.target);
        expected_names.insert(tarball.clone());
        expected_names.insert(format!("{tarball}.sha256"));
        expected_names.insert(provenance_name(release, &target.target));
    }

    let mut actual_names = HashSet::new();
    for entry in fs::read_dir(dir).with_context(|| format!("could not read {}", dir.display()))? {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name().into_string().map_err(|_| {
            anyhow!(
                "release artifact directory contains a non-UTF-8 entry: {}",
                path.display()
            )
        })?;
        let metadata = fs::symlink_metadata(&path)
            .with_context(|| format!("could not inspect {}", path.display()))?;
        if !metadata.file_type().is_file() {
            bail!(
                "release artifact directory entries must be regular files: {}",
                path.display()
            );
        }
        actual_names.insert(name);
        if actual_names.len() > expected_names.len() {
            bail!(
                "release artifact directory contains more entries than the {} files declared by eng/release.toml",
                expected_names.len()
            );
        }
    }
    let mut missing = expected_names
        .difference(&actual_names)
        .cloned()
        .collect::<Vec<_>>();
    let mut unexpected = actual_names
        .difference(&expected_names)
        .cloned()
        .collect::<Vec<_>>();
    missing.sort();
    unexpected.sort();
    if !missing.is_empty() || !unexpected.is_empty() {
        if !missing.is_empty() {
            eprintln!("missing release artifacts: {}", missing.join(", "));
        }
        if !unexpected.is_empty() {
            eprintln!("unexpected release artifacts: {}", unexpected.join(", "));
        }
        bail!("release artifact set does not match eng/release.toml");
    }
    for target in &release.targets {
        read_checked_sha256(dir, &artifact_name(release, &target.target))?;
    }

    let mut files = expected_names
        .into_iter()
        .map(|name| dir.join(name))
        .collect::<Vec<_>>();
    files.sort();
    Ok(files)
}

pub(crate) fn read_checked_sha256(dir: &Path, tarball: &str) -> Result<String> {
    let expected = read_sha256(dir, tarball)?;
    let actual = sha256_file(&dir.join(tarball))
        .with_context(|| format!("could not hash release artifact {tarball}"))?;
    if !expected.eq_ignore_ascii_case(&actual) {
        bail!("checksum mismatch for {tarball}: expected {expected}, got {actual}");
    }
    Ok(expected.to_ascii_lowercase())
}

fn read_sha256(dir: &Path, tarball: &str) -> Result<String> {
    let path = dir.join(format!("{tarball}.sha256"));
    let metadata = fs::symlink_metadata(&path)
        .with_context(|| format!("could not inspect {}", path.display()))?;
    if !metadata.file_type().is_file() {
        bail!("sha256 evidence must be a regular file: {}", path.display());
    }
    if metadata.len() > MAX_SHA256_EVIDENCE_SIZE {
        bail!(
            "sha256 evidence is too large: {} exceeds {MAX_SHA256_EVIDENCE_SIZE} bytes",
            path.display()
        );
    }
    let file =
        fs::File::open(&path).with_context(|| format!("could not read {}", path.display()))?;
    let mut reader = file.take(MAX_SHA256_EVIDENCE_SIZE + 1);
    let mut text = String::new();
    reader
        .read_to_string(&mut text)
        .with_context(|| format!("could not read UTF-8 sha256 evidence {}", path.display()))?;
    if text.len() as u64 > MAX_SHA256_EVIDENCE_SIZE {
        bail!(
            "sha256 evidence is too large while reading: {} exceeds {MAX_SHA256_EVIDENCE_SIZE} bytes",
            path.display()
        );
    }
    let line = text
        .strip_suffix('\n')
        .ok_or_else(|| anyhow!("sha256 file must end with one newline: {}", path.display()))?;
    if line.contains('\n') || line.contains('\r') {
        bail!(
            "sha256 file must contain exactly one line: {}",
            path.display()
        );
    }
    let (digest, filename) = line.split_once("  ").ok_or_else(|| {
        anyhow!(
            "sha256 file must use '<digest>  <filename>': {}",
            path.display()
        )
    })?;
    if filename != tarball {
        bail!(
            "sha256 filename mismatch in {}: expected {tarball}, got {filename}",
            path.display()
        );
    }
    validate_sha256_digest(digest)
        .with_context(|| format!("invalid sha256 file: {}", path.display()))?;
    if digest != digest.to_ascii_lowercase() {
        bail!("sha256 digest must be lowercase in {}", path.display());
    }
    Ok(digest.to_string())
}

fn validate_sha256_digest(value: &str) -> Result<()> {
    if value.len() != 64 || !value.chars().all(|ch| ch.is_ascii_hexdigit()) {
        bail!("sha256 digest must be 64 hex characters");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{
        ReleaseCross, ReleaseHomebrew, ReleaseNotes, ReleasePackage, ReleaseSdk, ReleaseTarget,
    };
    use std::env;

    const EMPTY_SHA256: &str = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";

    #[test]
    fn release_artifact_files_rejects_unexpected_files() {
        let dir = unique_test_dir("vo-dev-release-artifacts-unexpected");
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("vo-aarch64-apple-darwin.tar.gz"), "").unwrap();
        fs::write(dir.join("vo-aarch64-apple-darwin.tar.gz.sha256"), "").unwrap();
        fs::write(dir.join("vo-old-target.tar.gz"), "").unwrap();

        let error = release_artifact_files(&sample_release(), &dir).unwrap_err();
        assert!(error.to_string().contains("artifact set"));

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn release_artifact_files_returns_expected_evidence_triplet() {
        let dir = unique_test_dir("vo-dev-release-artifacts-ok");
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("vo-aarch64-apple-darwin.tar.gz"), "").unwrap();
        fs::write(
            dir.join("vo-aarch64-apple-darwin.tar.gz.sha256"),
            format!("{EMPTY_SHA256}  vo-aarch64-apple-darwin.tar.gz\n"),
        )
        .unwrap();
        fs::write(
            dir.join("vo-aarch64-apple-darwin.tar.gz.provenance.json"),
            "{}\n",
        )
        .unwrap();

        let files = release_artifact_files(&sample_release(), &dir).unwrap();
        let names = files
            .iter()
            .map(|path| path.file_name().unwrap().to_string_lossy().to_string())
            .collect::<Vec<_>>();
        assert_eq!(
            names,
            vec![
                "vo-aarch64-apple-darwin.tar.gz",
                "vo-aarch64-apple-darwin.tar.gz.provenance.json",
                "vo-aarch64-apple-darwin.tar.gz.sha256"
            ]
        );

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn read_sha256_rejects_malformed_digest() {
        let dir = unique_test_dir("vo-dev-release-bad-sha");
        fs::create_dir_all(&dir).unwrap();
        fs::write(
            dir.join("vo-aarch64-apple-darwin.tar.gz.sha256"),
            "not-a-sha file\n",
        )
        .unwrap();

        let error = read_sha256(&dir, "vo-aarch64-apple-darwin.tar.gz").unwrap_err();
        assert!(error.to_string().contains("sha256 file"));

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn read_checked_sha256_rejects_digest_mismatch() {
        let dir = unique_test_dir("vo-dev-release-bad-sha-match");
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("vo-aarch64-apple-darwin.tar.gz"), "not empty").unwrap();
        fs::write(
            dir.join("vo-aarch64-apple-darwin.tar.gz.sha256"),
            format!("{EMPTY_SHA256}  vo-aarch64-apple-darwin.tar.gz\n"),
        )
        .unwrap();

        let error = read_checked_sha256(&dir, "vo-aarch64-apple-darwin.tar.gz").unwrap_err();
        assert!(error.to_string().contains("checksum mismatch"));

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn read_sha256_rejects_wrong_artifact_name() {
        let dir = unique_test_dir("vo-dev-release-sha-name");
        fs::create_dir_all(&dir).unwrap();
        fs::write(
            dir.join("vo-aarch64-apple-darwin.tar.gz.sha256"),
            format!("{EMPTY_SHA256}  other.tar.gz\n"),
        )
        .unwrap();

        let error = read_sha256(&dir, "vo-aarch64-apple-darwin.tar.gz").unwrap_err();
        assert!(error.to_string().contains("filename mismatch"));
        fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn read_sha256_rejects_oversized_evidence_before_reading_it() {
        let dir = unique_test_dir("vo-dev-release-sha-size");
        fs::create_dir_all(&dir).unwrap();
        fs::File::create(dir.join("vo-aarch64-apple-darwin.tar.gz.sha256"))
            .unwrap()
            .set_len(MAX_SHA256_EVIDENCE_SIZE + 1)
            .unwrap();

        let error = read_sha256(&dir, "vo-aarch64-apple-darwin.tar.gz").unwrap_err();
        assert!(error.to_string().contains("too large"));
        fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn lint_release_accepts_sample_config() {
        lint_release_file(&sample_release()).unwrap();
    }

    #[test]
    fn lint_release_rejects_build_args_for_other_crate() {
        let mut release = sample_release();
        release.package.build_args = vec![
            "--release".to_string(),
            "--locked".to_string(),
            "-p".to_string(),
            "other".to_string(),
        ];

        let error = lint_release_file(&release).unwrap_err();
        assert!(error.to_string().contains("declared crate"));
    }

    #[test]
    fn lint_release_rejects_target_in_build_args() {
        let mut release = sample_release();
        release.package.build_args.push("--target".to_string());
        release
            .package
            .build_args
            .push("aarch64-apple-darwin".to_string());

        let error = lint_release_file(&release).unwrap_err();
        assert!(error.to_string().contains("must not declare --target"));
    }

    #[test]
    fn lint_release_rejects_unsafe_homebrew_formula_path() {
        let mut release = sample_release();
        release.homebrew.formula_path = "../Formula/vo.rb".to_string();

        let error = lint_release_file(&release).unwrap_err();
        assert!(error.to_string().contains("invalid segment"));
    }

    fn sample_release() -> ReleaseFile {
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
                target: "aarch64-apple-darwin".to_string(),
                os: "macos-14".to_string(),
                use_cross: false,
            }],
        }
    }

    fn unique_test_dir(name: &str) -> PathBuf {
        let mut path = env::temp_dir();
        path.push(format!("{name}-{}", std::process::id()));
        if path.exists() {
            fs::remove_dir_all(&path).unwrap();
        }
        path
    }
}
