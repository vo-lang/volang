use crate::config::{load_project, load_release};
use crate::release_archive::{
    clear_release_build_outputs, package_release_binary, record_release_build,
    validate_release_artifacts, write_text_atomic,
};
use crate::release_config::{
    artifact_name, lint_release_file, read_checked_sha256, release_artifact_files, release_target,
    sha256_file,
};
use crate::release_homebrew::{
    homebrew_checkout_path, homebrew_version_progression, replace_formula_target_sha,
    replace_release_version, validate_homebrew_formula_targets,
    validate_release_version_progression, HomebrewVersionProgression,
};
use crate::release_identity::{
    resolve_release_identity, validated_release_version, SourceCleanliness,
};
use crate::release_sdk::{cmd_sdk_plan, validate_sdk_publish_boundary};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::{Component, Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::thread;

const MAX_RELEASE_NOTES_SIZE: u64 = 1024 * 1024;
const MAX_HOMEBREW_FORMULA_SIZE: u64 = 1024 * 1024;
const MAX_GH_JSON_OUTPUT_SIZE: usize = 8 * 1024 * 1024;
const MAX_GH_ERROR_OUTPUT_SIZE: usize = 1024 * 1024;

#[derive(Debug, Serialize)]
struct ReleaseMatrix {
    include: Vec<ReleaseMatrixRow>,
}

#[derive(Debug, Serialize)]
struct ReleaseMatrixRow {
    target: String,
    os: String,
    artifact_name: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GitHubReleaseView {
    body: String,
    is_draft: bool,
    is_immutable: bool,
    is_prerelease: bool,
    name: String,
    tag_name: String,
}

#[derive(Debug, Deserialize)]
struct GitHubImmutableReleaseSettings {
    enabled: bool,
}

#[derive(Debug, Deserialize)]
struct GitHubReleaseAssets {
    assets: Vec<GitHubReleaseAsset>,
}

#[derive(Debug, Deserialize)]
struct GitHubReleaseAsset {
    name: String,
    size: u64,
    #[serde(default)]
    digest: Option<String>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ExistingReleaseState {
    Draft,
    Published,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ReleaseAssetSnapshot {
    path: PathBuf,
    name: String,
    size: u64,
    sha256: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ReleaseRepository {
    api_repo: String,
    gh_repo: String,
    git_url: String,
}

#[derive(Debug)]
struct BoundedCommandOutput {
    status: ExitStatus,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

#[derive(Debug)]
struct BoundedRead {
    bytes: Vec<u8>,
    exceeded: bool,
}

pub(crate) fn cmd_release(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev release matrix|metadata|version|sdk-plan|homebrew-repository|homebrew-metadata|build|package|verify|notes|publish|update-homebrew ...");
    }
    match args.remove(0).as_str() {
        "matrix" => cmd_matrix(root, args),
        "metadata" => cmd_metadata(root, args),
        "version" => cmd_version(root, args),
        "sdk-plan" => {
            let release = load_release(root)?;
            lint_release_file(&release)?;
            cmd_sdk_plan(root, &release, args)
        }
        "homebrew-repository" => cmd_homebrew_repository(root, args),
        "homebrew-metadata" => cmd_homebrew_metadata(root, args),
        "build" => cmd_build(root, args),
        "package" => cmd_package(root, args),
        "verify" => cmd_verify(root, args),
        "notes" => cmd_notes(root, args),
        "publish" => cmd_publish(root, args),
        "update-homebrew" => cmd_update_homebrew(root, args),
        other => bail!("unknown release command: {other}"),
    }
}

pub(crate) fn lint_release(root: &Path) -> Result<()> {
    let release = load_release(root)?;
    lint_release_file(&release)?;
    release_repository(root)?;
    validate_sdk_publish_boundary(root, &release)
}

fn cmd_metadata(root: &Path, args: Vec<String>) -> Result<()> {
    let mut tag = None;
    let mut commit = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--tag" => {
                i += 1;
                tag = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--tag requires a value"))?
                        .clone(),
                );
            }
            "--commit" => {
                i += 1;
                commit = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--commit requires a value"))?
                        .clone(),
                );
            }
            other => bail!("unknown release metadata argument: {other}"),
        }
        i += 1;
    }
    let identity = resolve_release_identity(
        root,
        tag.as_deref(),
        commit.as_deref(),
        SourceCleanliness::AllFiles,
    )?;
    println!("{}", serde_json::to_string_pretty(&identity)?);
    Ok(())
}

fn cmd_matrix(root: &Path, args: Vec<String>) -> Result<()> {
    ensure_no_args("release matrix", &args)?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let matrix = ReleaseMatrix {
        include: release
            .targets
            .iter()
            .map(|target| ReleaseMatrixRow {
                target: target.target.clone(),
                os: target.os.clone(),
                artifact_name: artifact_name(&release, &target.target),
            })
            .collect(),
    };
    println!("{}", serde_json::to_string_pretty(&matrix)?);
    Ok(())
}

fn cmd_version(root: &Path, args: Vec<String>) -> Result<()> {
    let mut tag = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--tag" => {
                i += 1;
                tag = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--tag requires a value"))?
                        .clone(),
                );
            }
            other => bail!("unknown release version argument: {other}"),
        }
        i += 1;
    }
    let tag = release_tag(tag.as_deref())?;
    let version = validated_release_version(root, &tag)?;
    println!("{version}");
    Ok(())
}

fn cmd_homebrew_repository(root: &Path, args: Vec<String>) -> Result<()> {
    ensure_no_args("release homebrew-repository", &args)?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    println!("{}", release.homebrew.repository);
    Ok(())
}

fn cmd_homebrew_metadata(root: &Path, args: Vec<String>) -> Result<()> {
    ensure_no_args("release homebrew-metadata", &args)?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let checkout_path = homebrew_checkout_path(&release)?;
    println!(
        "{}",
        serde_json::to_string_pretty(&serde_json::json!({
            "repository": release.homebrew.repository,
            "checkout_path": checkout_path,
            "formula_path": release.homebrew.formula_path,
        }))?
    );
    Ok(())
}

fn cmd_build(root: &Path, args: Vec<String>) -> Result<()> {
    if args.len() != 1 {
        bail!("usage: vo-dev release build <target>");
    }
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let target = release_target(&release, &args[0])?;
    let identity = resolve_release_identity(root, None, None, SourceCleanliness::AllFiles)?;
    clear_release_build_outputs(root, &release, &target.target)?;

    run_status(
        Command::new("rustup")
            .args(["target", "add", &target.target])
            .current_dir(root),
        "rustup target add",
    )?;

    let mut command = Command::new("cargo");
    command
        .arg("build")
        .args(&release.package.build_args)
        .args(["--target", &target.target])
        .env(
            "CARGO_PROFILE_RELEASE_OPT_LEVEL",
            &release.package.release_opt_level,
        )
        .env("CARGO_PROFILE_RELEASE_LTO", &release.package.release_lto)
        .env("CARGO_TARGET_DIR", root.join("target"))
        .env_remove("CARGO_BUILD_TARGET")
        .env("VO_BUILD_COMMIT", &identity.commit)
        .env("VO_BUILD_DATE", &identity.build_date)
        .env("SOURCE_DATE_EPOCH", identity.source_date_epoch.to_string())
        .current_dir(root);
    run_status(&mut command, &format!("cargo build {}", target.target))?;
    record_release_build(root, &release, &target.target, &identity)
}

fn cmd_package(root: &Path, args: Vec<String>) -> Result<()> {
    if args.len() != 1 {
        bail!("usage: vo-dev release package <target>");
    }
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let target = release_target(&release, &args[0])?;
    let identity = resolve_release_identity(root, None, None, SourceCleanliness::AllFiles)?;
    let tarball = package_release_binary(root, &release, &target.target, &identity)?;
    println!("{tarball}");
    Ok(())
}

fn cmd_verify(root: &Path, args: Vec<String>) -> Result<()> {
    let mut tag = None;
    let mut artifacts = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--tag" => {
                i += 1;
                tag = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--tag requires a value"))?
                        .clone(),
                );
            }
            "--artifacts" => {
                i += 1;
                artifacts = Some(PathBuf::from(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--artifacts requires a value"))?,
                ));
            }
            other => bail!("unknown release verify argument: {other}"),
        }
        i += 1;
    }

    let tag = release_tag(tag.as_deref())?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let identity =
        resolve_release_identity(root, Some(&tag), None, SourceCleanliness::TrackedFiles)?;
    let artifacts = safe_repo_path(
        root,
        &artifacts.unwrap_or_else(|| PathBuf::from("artifacts")),
        "release artifact directory",
        true,
    )?;
    let files = release_artifact_files(&release, &artifacts)?;
    validate_release_artifacts(&artifacts, &release, &identity)?;
    let snapshot = snapshot_release_assets(&files)?;
    verify_local_release_asset_snapshot(&snapshot)?;
    println!(
        "verified {} release assets for {tag} at {}",
        snapshot.len(),
        identity.commit
    );
    Ok(())
}

fn cmd_notes(root: &Path, args: Vec<String>) -> Result<()> {
    let mut tag = None;
    let mut out = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--tag" => {
                i += 1;
                tag = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--tag requires a value"))?
                        .clone(),
                );
            }
            "--out" => {
                i += 1;
                out = Some(PathBuf::from(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--out requires a value"))?,
                ));
            }
            other => bail!("unknown release notes argument: {other}"),
        }
        i += 1;
    }
    let tag = release_tag(tag.as_deref())?;
    validated_release_version(root, &tag)?;
    let out = out.ok_or_else(|| anyhow!("release notes requires --out <path>"))?;
    let out = safe_repo_path(root, &out, "release notes output", false)?;
    let release = load_release(root)?;
    lint_release_file(&release)?;

    let mut notes = String::new();
    notes.push_str(&format!("## {} {tag}\n\n", release.notes.product_name));
    if !release.notes.homebrew.is_empty() {
        notes.push_str("### Install via Homebrew\n");
        notes.push_str("```sh\n");
        for line in &release.notes.homebrew {
            notes.push_str(line);
            notes.push('\n');
        }
        notes.push_str("```\n\n");
    }
    notes.push_str("### Manual install\n");
    notes.push_str(&release.notes.manual_install);
    notes.push('\n');
    reject_non_regular_output(&out)?;
    if notes.len() as u64 > MAX_RELEASE_NOTES_SIZE {
        bail!(
            "generated release notes exceed {MAX_RELEASE_NOTES_SIZE} bytes: {}",
            out.display()
        );
    }
    write_text_atomic(&out, &notes)
        .with_context(|| format!("could not write release notes to {}", out.display()))
}

fn cmd_publish(root: &Path, args: Vec<String>) -> Result<()> {
    let mut tag = None;
    let mut artifacts = None;
    let mut notes = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--tag" => {
                i += 1;
                tag = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--tag requires a value"))?
                        .clone(),
                );
            }
            "--artifacts" => {
                i += 1;
                artifacts = Some(PathBuf::from(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--artifacts requires a value"))?,
                ));
            }
            "--notes" => {
                i += 1;
                notes = Some(PathBuf::from(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--notes requires a value"))?,
                ));
            }
            other => bail!("unknown release publish argument: {other}"),
        }
        i += 1;
    }
    let tag = release_tag(tag.as_deref())?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let repository = release_repository(root)?;
    let identity =
        resolve_release_identity(root, Some(&tag), None, SourceCleanliness::TrackedFiles)?;
    let artifacts = safe_repo_path(
        root,
        &artifacts.unwrap_or_else(|| PathBuf::from("artifacts")),
        "release artifact directory",
        true,
    )?;
    let notes = safe_repo_path(
        root,
        &notes.unwrap_or_else(|| PathBuf::from("RELEASE_NOTES.md")),
        "release notes file",
        true,
    )?;
    let notes_text =
        read_regular_text_with_limit(&notes, "release notes file", MAX_RELEASE_NOTES_SIZE)?;
    let files = release_artifact_files(&release, &artifacts)?;
    validate_release_artifacts(&artifacts, &release, &identity)?;
    let expected_assets = snapshot_release_assets(&files)?;
    validate_release_artifacts(&artifacts, &release, &identity)?;
    verify_local_release_asset_snapshot(&expected_assets)?;
    let prerelease = !semver::Version::parse(&identity.version)?.pre.is_empty();
    let release_name = format!("Vo {tag}");
    verify_immutable_releases_enabled(root, &repository)?;
    verify_remote_tag_commit(root, &repository, &tag, &identity.commit)?;

    if let Some(existing) = github_release_view(root, &repository, &tag)? {
        if validate_existing_release(&existing, &tag, prerelease, &release_name, &notes_text)?
            == ExistingReleaseState::Published
        {
            verify_remote_release_assets(root, &repository, &tag, &expected_assets)?;
            verify_remote_tag_commit(root, &repository, &tag, &identity.commit)?;
            println!("GitHub release {tag} is already published with the verified assets");
            return Ok(());
        }
        let mut edit = Command::new("gh");
        edit.args(["release", "edit", &tag, "--title"])
            .arg(&release_name)
            .arg("--notes-file")
            .arg("-")
            .arg("--verify-tag")
            .arg("--target")
            .arg(&identity.commit);
        if prerelease {
            edit.arg("--prerelease");
        } else {
            edit.arg("--prerelease=false");
        }
        pin_gh_repository(&mut edit, &repository);
        run_status_with_input(
            edit.current_dir(root),
            "gh release edit draft",
            notes_text.as_bytes(),
        )?;
    } else {
        let mut create = Command::new("gh");
        create
            .args([
                "release",
                "create",
                &tag,
                "--draft",
                "--verify-tag",
                "--target",
            ])
            .arg(&identity.commit)
            .arg("--title")
            .arg(&release_name)
            .arg("--notes-file")
            .arg("-");
        if prerelease {
            create.arg("--prerelease");
        } else {
            create.arg("--prerelease=false");
        }
        pin_gh_repository(&mut create, &repository);
        run_status_with_input(
            create.current_dir(root),
            "gh release create draft",
            notes_text.as_bytes(),
        )?;
    }

    let draft = github_release_view(root, &repository, &tag)?
        .ok_or_else(|| anyhow!("GitHub release {tag} disappeared before asset upload"))?;
    validate_draft_release(&draft, &tag, prerelease, &release_name, &notes_text)?;

    let mut upload = Command::new("gh");
    verify_local_release_asset_snapshot(&expected_assets)?;
    upload.args(["release", "upload", &tag]);
    upload.args(&files);
    upload.arg("--clobber");
    pin_gh_repository(&mut upload, &repository);
    run_status(upload.current_dir(root), "gh release upload to draft")?;
    verify_local_release_asset_snapshot(&expected_assets)?;
    verify_remote_release_assets(root, &repository, &tag, &expected_assets)?;
    verify_remote_tag_commit(root, &repository, &tag, &identity.commit)?;
    let draft = github_release_view(root, &repository, &tag)?
        .ok_or_else(|| anyhow!("GitHub release {tag} disappeared before publication"))?;
    validate_draft_release(&draft, &tag, prerelease, &release_name, &notes_text)?;
    verify_immutable_releases_enabled(root, &repository)?;

    let mut publish = Command::new("gh");
    publish
        .args([
            "release",
            "edit",
            &tag,
            "--draft=false",
            "--verify-tag",
            "--title",
        ])
        .arg(&release_name)
        .arg("--notes-file")
        .arg("-")
        .arg("--target")
        .arg(&identity.commit);
    if prerelease {
        publish.arg("--prerelease");
    } else {
        publish.arg("--prerelease=false");
    }
    pin_gh_repository(&mut publish, &repository);
    run_status_with_input(
        publish.current_dir(root),
        "gh release publish verified draft",
        notes_text.as_bytes(),
    )?;
    let published = github_release_view(root, &repository, &tag)?
        .ok_or_else(|| anyhow!("GitHub release {tag} disappeared after publication"))?;
    if validate_existing_release(&published, &tag, prerelease, &release_name, &notes_text)?
        != ExistingReleaseState::Published
    {
        bail!("GitHub release {tag} remained a draft after publication");
    }
    verify_remote_tag_commit(root, &repository, &tag, &identity.commit)?;
    verify_remote_release_assets(root, &repository, &tag, &expected_assets)?;
    Ok(())
}

fn validate_draft_release(
    release: &GitHubReleaseView,
    tag: &str,
    prerelease: bool,
    expected_name: &str,
    expected_body: &str,
) -> Result<()> {
    if !release.is_draft {
        bail!("GitHub release {tag} was published before the verified publication step");
    }
    validate_release_identity_and_content(release, tag, prerelease, expected_name, expected_body)
}

fn validate_release_identity_and_content(
    release: &GitHubReleaseView,
    tag: &str,
    prerelease: bool,
    expected_name: &str,
    expected_body: &str,
) -> Result<()> {
    if release.tag_name != tag {
        bail!(
            "GitHub release tag mismatch: expected {tag}, got {}",
            release.tag_name
        );
    }
    if release.is_prerelease != prerelease {
        bail!(
            "GitHub release {tag} prerelease state mismatch: expected {prerelease}, got {}",
            release.is_prerelease
        );
    }
    if release.name != expected_name {
        bail!(
            "GitHub release {tag} title mismatch: expected {expected_name:?}, got {:?}",
            release.name
        );
    }
    if release.body != expected_body {
        bail!("GitHub release {tag} notes do not match the verified release notes");
    }
    Ok(())
}

fn validate_existing_release(
    release: &GitHubReleaseView,
    tag: &str,
    prerelease: bool,
    expected_name: &str,
    expected_body: &str,
) -> Result<ExistingReleaseState> {
    if release.tag_name != tag {
        bail!(
            "GitHub release tag mismatch: expected {tag}, got {}",
            release.tag_name
        );
    }
    if release.is_draft {
        return Ok(ExistingReleaseState::Draft);
    }
    validate_release_identity_and_content(release, tag, prerelease, expected_name, expected_body)?;
    if !release.is_immutable {
        bail!("published GitHub release {tag} is mutable; enable immutable releases before publishing");
    }
    Ok(ExistingReleaseState::Published)
}

fn cmd_update_homebrew(root: &Path, args: Vec<String>) -> Result<()> {
    let mut repo = None;
    let mut artifacts = None;
    let mut version = None;
    let mut allow_superseded = false;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--repo" => {
                i += 1;
                repo = Some(PathBuf::from(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--repo requires a value"))?,
                ));
            }
            "--artifacts" => {
                i += 1;
                artifacts = Some(PathBuf::from(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--artifacts requires a value"))?,
                ));
            }
            "--version" => {
                i += 1;
                version = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--version requires a value"))?
                        .clone(),
                );
            }
            "--allow-superseded" => allow_superseded = true,
            other => bail!("unknown release update-homebrew argument: {other}"),
        }
        i += 1;
    }
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let identity = resolve_release_identity(root, None, None, SourceCleanliness::TrackedFiles)?;
    let repo = safe_repo_path(
        root,
        &repo.ok_or_else(|| anyhow!("update-homebrew requires --repo <path>"))?,
        "Homebrew checkout directory",
        true,
    )?;
    let artifacts = safe_repo_path(
        root,
        &artifacts.ok_or_else(|| anyhow!("update-homebrew requires --artifacts <path>"))?,
        "release artifact directory",
        true,
    )?;
    let version = version.ok_or_else(|| anyhow!("update-homebrew requires --version <version>"))?;
    if version != identity.version {
        bail!(
            "Homebrew version must match tagged workspace version {}",
            identity.version
        );
    }
    release_artifact_files(&release, &artifacts)?;
    validate_release_artifacts(&artifacts, &release, &identity)?;
    let formula_path = safe_descendant_path(
        &repo,
        Path::new(&release.homebrew.formula_path),
        "Homebrew formula",
        true,
    )?;
    let mut text =
        read_regular_text_with_limit(&formula_path, "Homebrew formula", MAX_HOMEBREW_FORMULA_SIZE)?;

    let progression = homebrew_version_progression(&text, &version)?;
    if allow_superseded && progression == HomebrewVersionProgression::Superseded {
        println!("Homebrew formula already contains a newer release; this update is superseded");
        return Ok(());
    }
    validate_release_version_progression(&text, &version)?;
    validate_homebrew_formula_targets(&release, &text)?;
    text = replace_release_version(&text, &version)?;
    for target in &release.targets {
        let sha = read_checked_sha256(&artifacts, &artifact_name(&release, &target.target))?;
        text = replace_formula_target_sha(&text, &target.target, &sha)?;
    }
    if text.len() as u64 > MAX_HOMEBREW_FORMULA_SIZE {
        bail!("updated Homebrew formula exceeds {MAX_HOMEBREW_FORMULA_SIZE} bytes");
    }
    write_text_atomic(&formula_path, &text)
        .with_context(|| format!("could not write {}", formula_path.display()))
}

fn release_tag(explicit: Option<&str>) -> Result<String> {
    let mut selected = explicit.map(str::to_string);
    for name in ["VO_RELEASE_TAG", "GITHUB_REF_NAME"] {
        let Ok(value) = env::var(name) else {
            continue;
        };
        if value.is_empty() || value.trim() != value {
            bail!("{name} must be non-empty and have no surrounding whitespace");
        }
        if let Some(selected) = &selected {
            if selected != &value {
                bail!("conflicting release tags: {selected} and {name}={value}");
            }
        } else {
            selected = Some(value);
        }
    }
    selected.ok_or_else(|| {
        anyhow!("release command requires --tag, VO_RELEASE_TAG, or GITHUB_REF_NAME")
    })
}

fn safe_repo_path(root: &Path, relative: &Path, label: &str, must_exist: bool) -> Result<PathBuf> {
    safe_descendant_path(root, relative, label, must_exist)
}

fn safe_descendant_path(
    base: &Path,
    relative: &Path,
    label: &str,
    must_exist: bool,
) -> Result<PathBuf> {
    let text = relative
        .to_str()
        .ok_or_else(|| anyhow!("{label} path must be UTF-8"))?;
    if text.is_empty() || text.contains('\\') || text.contains(':') {
        bail!("{label} must use a clean relative path");
    }
    let components = relative.components().collect::<Vec<_>>();
    if components.is_empty()
        || components
            .iter()
            .any(|component| !matches!(component, Component::Normal(_)))
    {
        bail!("{label} must be a clean relative path without '.', '..', or roots");
    }

    let canonical_base = base
        .canonicalize()
        .with_context(|| format!("could not canonicalize {label} base {}", base.display()))?;
    let mut current = canonical_base.clone();
    for (index, component) in components.iter().enumerate() {
        let Component::Normal(segment) = component else {
            unreachable!();
        };
        current.push(segment);
        match fs::symlink_metadata(&current) {
            Ok(metadata) => {
                if metadata.file_type().is_symlink() {
                    bail!(
                        "{label} path must not traverse symlinks: {}",
                        current.display()
                    );
                }
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                let is_leaf = index + 1 == components.len();
                if must_exist || !is_leaf {
                    bail!("{label} path is missing: {}", current.display());
                }
            }
            Err(error) => {
                return Err(error).with_context(|| {
                    format!("could not inspect {label} path {}", current.display())
                })
            }
        }
    }
    if current.exists() {
        let canonical = current.canonicalize().with_context(|| {
            format!("could not canonicalize {label} path {}", current.display())
        })?;
        if !canonical.starts_with(&canonical_base) {
            bail!("{label} path escapes its declared base");
        }
    }
    Ok(current)
}

fn ensure_regular_file(path: &Path, label: &str) -> Result<()> {
    let metadata = fs::symlink_metadata(path)
        .with_context(|| format!("could not inspect {label} {}", path.display()))?;
    if !metadata.file_type().is_file() {
        bail!("{label} must be a regular file: {}", path.display());
    }
    Ok(())
}

fn ensure_regular_file_with_limit(path: &Path, label: &str, limit: u64) -> Result<u64> {
    ensure_regular_file(path, label)?;
    let size = fs::symlink_metadata(path)
        .with_context(|| format!("could not inspect {label} {}", path.display()))?
        .len();
    if size > limit {
        bail!(
            "{label} exceeds the {limit}-byte size limit: {} has {size} bytes",
            path.display()
        );
    }
    Ok(size)
}

fn read_regular_text_with_limit(path: &Path, label: &str, limit: u64) -> Result<String> {
    ensure_regular_file_with_limit(path, label, limit)?;
    let file =
        File::open(path).with_context(|| format!("could not read {label} {}", path.display()))?;
    let mut reader = file.take(limit + 1);
    let mut bytes = Vec::new();
    reader
        .read_to_end(&mut bytes)
        .with_context(|| format!("could not read {label} {}", path.display()))?;
    if bytes.len() as u64 > limit {
        bail!(
            "{label} exceeds the {limit}-byte size limit while reading: {}",
            path.display()
        );
    }
    String::from_utf8(bytes).with_context(|| format!("{label} must be UTF-8: {}", path.display()))
}

fn reject_non_regular_output(path: &Path) -> Result<()> {
    match fs::symlink_metadata(path) {
        Ok(metadata) if metadata.file_type().is_file() => Ok(()),
        Ok(_) => bail!("release output must be a regular file: {}", path.display()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error)
            .with_context(|| format!("could not inspect release output {}", path.display())),
    }
}

fn release_repository(root: &Path) -> Result<ReleaseRepository> {
    let project = load_project(root)?;
    parse_release_repository(&project.repo.name, &project.repo.module)
}

fn parse_release_repository(name: &str, module: &str) -> Result<ReleaseRepository> {
    let path = module
        .strip_prefix("github.com/")
        .ok_or_else(|| anyhow!("release repository module must use github.com/OWNER/REPOSITORY"))?;
    let mut components = path.split('/');
    let owner = components.next().unwrap_or_default();
    let repository = components.next().unwrap_or_default();
    if components.next().is_some()
        || !valid_github_owner(owner)
        || !valid_github_repository(repository)
    {
        bail!("release repository module must use github.com/OWNER/REPOSITORY");
    }
    if repository != name {
        bail!(
            "release repository name mismatch: project name {name:?}, module repository {repository:?}"
        );
    }
    Ok(ReleaseRepository {
        api_repo: path.to_string(),
        gh_repo: module.to_string(),
        git_url: format!("https://{module}.git"),
    })
}

fn valid_github_owner(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 39
        && !value.starts_with('-')
        && !value.ends_with('-')
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
}

fn valid_github_repository(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 100
        && value != "."
        && value != ".."
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
}

fn pin_gh_repository(command: &mut Command, repository: &ReleaseRepository) {
    command.args(["--repo", &repository.gh_repo]);
}

fn verify_immutable_releases_enabled(root: &Path, repository: &ReleaseRepository) -> Result<()> {
    let endpoint = format!("repos/{}/immutable-releases", repository.api_repo);
    let mut command = Command::new("gh");
    command.args(["api", "--hostname", "github.com", "--method", "GET"]);
    command.arg(endpoint).current_dir(root);
    if let Some(token) = env::var_os("VO_RELEASE_SETTINGS_TOKEN") {
        if token.is_empty() {
            bail!("VO_RELEASE_SETTINGS_TOKEN is empty");
        }
        command.env("GH_TOKEN", token);
    }
    let output = run_bounded_output(
        &mut command,
        "inspect immutable release settings",
        MAX_GH_JSON_OUTPUT_SIZE,
        MAX_GH_ERROR_OUTPUT_SIZE,
    )?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        bail!("could not confirm immutable releases for the canonical repository: {stderr}");
    }
    let settings: GitHubImmutableReleaseSettings = serde_json::from_slice(&output.stdout)
        .context("could not parse immutable release settings")?;
    if !settings.enabled {
        bail!("immutable releases are disabled for the canonical repository");
    }
    Ok(())
}

fn github_release_view(
    root: &Path,
    repository: &ReleaseRepository,
    tag: &str,
) -> Result<Option<GitHubReleaseView>> {
    let mut command = Command::new("gh");
    command.args([
        "release",
        "view",
        tag,
        "--json",
        "body,isDraft,isImmutable,isPrerelease,name,tagName",
    ]);
    pin_gh_repository(&mut command, repository);
    command.current_dir(root);
    let output = run_bounded_output(
        &mut command,
        "gh release view",
        MAX_GH_JSON_OUTPUT_SIZE,
        MAX_GH_ERROR_OUTPUT_SIZE,
    )?;
    if output.status.success() {
        return serde_json::from_slice(&output.stdout)
            .context("could not parse gh release view output")
            .map(Some);
    }
    let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
    let lowered = stderr.to_ascii_lowercase();
    if lowered.contains("release not found") || lowered.contains("http 404") {
        return Ok(None);
    }
    bail!("gh release view failed: {stderr}")
}

fn verify_remote_tag_commit(
    root: &Path,
    repository: &ReleaseRepository,
    tag: &str,
    expected_commit: &str,
) -> Result<()> {
    let direct_ref = format!("refs/tags/{tag}");
    let peeled_ref = format!("{direct_ref}^{{}}");
    let mut command = Command::new("git");
    command
        .args(["ls-remote", "--tags", "--exit-code", &repository.git_url])
        .arg(&direct_ref)
        .arg(&peeled_ref)
        .current_dir(root);
    let output = run_bounded_output(
        &mut command,
        "resolve remote release tag",
        MAX_GH_JSON_OUTPUT_SIZE,
        MAX_GH_ERROR_OUTPUT_SIZE,
    )?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        bail!("could not resolve remote release tag {tag}: {stderr}");
    }
    let stdout = String::from_utf8(output.stdout).context("git ls-remote output was not UTF-8")?;
    let actual_commit = parse_remote_tag_commit(&stdout, &direct_ref, &peeled_ref)?;
    if actual_commit != expected_commit {
        bail!("remote release tag {tag} resolves to {actual_commit}, expected {expected_commit}");
    }
    Ok(())
}

fn parse_remote_tag_commit(output: &str, direct_ref: &str, peeled_ref: &str) -> Result<String> {
    let mut direct = None;
    let mut peeled = None;
    for line in output.lines() {
        let (object, reference) = line
            .split_once('\t')
            .ok_or_else(|| anyhow!("malformed git ls-remote line: {line:?}"))?;
        if !matches!(object.len(), 40 | 64)
            || !object.chars().all(|ch| ch.is_ascii_hexdigit())
            || object != object.to_ascii_lowercase()
        {
            bail!("git ls-remote returned invalid object id {object:?}");
        }
        let slot = if reference == direct_ref {
            &mut direct
        } else if reference == peeled_ref {
            &mut peeled
        } else {
            bail!("git ls-remote returned unexpected ref {reference:?}");
        };
        if slot.replace(object.to_string()).is_some() {
            bail!("git ls-remote returned duplicate ref {reference}");
        }
    }
    peeled
        .or(direct)
        .ok_or_else(|| anyhow!("git ls-remote returned no release tag ref"))
}

fn snapshot_release_assets(files: &[PathBuf]) -> Result<Vec<ReleaseAssetSnapshot>> {
    let mut names = HashSet::new();
    let mut snapshots = Vec::with_capacity(files.len());
    for path in files {
        ensure_regular_file(path, "local release asset")?;
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .ok_or_else(|| anyhow!("release asset filename must be UTF-8: {}", path.display()))?
            .to_string();
        if !names.insert(name.clone()) {
            bail!("release asset set contains duplicate filename {name}");
        }
        let size = fs::symlink_metadata(path)?.len();
        let sha256 = sha256_file(path)?;
        if fs::symlink_metadata(path)?.len() != size {
            bail!(
                "release asset changed while its upload snapshot was recorded: {}",
                path.display()
            );
        }
        snapshots.push(ReleaseAssetSnapshot {
            path: path.clone(),
            name,
            size,
            sha256,
        });
    }
    Ok(snapshots)
}

fn verify_local_release_asset_snapshot(snapshots: &[ReleaseAssetSnapshot]) -> Result<()> {
    for snapshot in snapshots {
        ensure_regular_file(&snapshot.path, "local release asset")?;
        let size = fs::symlink_metadata(&snapshot.path)?.len();
        if size != snapshot.size {
            bail!(
                "release asset changed after validation: {} size is {size}, expected {}",
                snapshot.path.display(),
                snapshot.size
            );
        }
        let sha256 = sha256_file(&snapshot.path)?;
        if sha256 != snapshot.sha256 {
            bail!(
                "release asset changed after validation: {} digest is {sha256}, expected {}",
                snapshot.path.display(),
                snapshot.sha256
            );
        }
    }
    Ok(())
}

fn verify_remote_release_assets(
    root: &Path,
    repository: &ReleaseRepository,
    tag: &str,
    snapshots: &[ReleaseAssetSnapshot],
) -> Result<()> {
    let mut command = Command::new("gh");
    command.args(["release", "view", tag, "--json", "assets"]);
    pin_gh_repository(&mut command, repository);
    command.current_dir(root);
    let output = run_bounded_output(
        &mut command,
        "inspect uploaded GitHub release assets",
        MAX_GH_JSON_OUTPUT_SIZE,
        MAX_GH_ERROR_OUTPUT_SIZE,
    )?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        bail!("could not inspect uploaded GitHub release assets: {stderr}");
    }
    let remote: GitHubReleaseAssets = serde_json::from_slice(&output.stdout)
        .context("could not parse uploaded GitHub release assets")?;
    let mut expected = HashMap::new();
    for snapshot in snapshots {
        if expected
            .insert(
                snapshot.name.clone(),
                (snapshot.size, format!("sha256:{}", snapshot.sha256)),
            )
            .is_some()
        {
            bail!(
                "release asset snapshot contains duplicate filename {}",
                snapshot.name
            );
        }
    }
    let mut actual_names = HashSet::new();
    for asset in remote.assets {
        if !actual_names.insert(asset.name.clone()) {
            bail!("GitHub release contains duplicate asset {}", asset.name);
        }
        let Some((expected_size, expected_digest)) = expected.get(&asset.name) else {
            bail!("GitHub release contains unexpected asset {}", asset.name);
        };
        if asset.size != *expected_size {
            bail!(
                "GitHub release asset {} has size {}, expected {expected_size}",
                asset.name,
                asset.size
            );
        }
        let digest = asset
            .digest
            .filter(|digest| !digest.is_empty())
            .ok_or_else(|| anyhow!("GitHub release asset {} has no server digest", asset.name))?;
        if digest != *expected_digest {
            bail!(
                "GitHub release asset {} has digest {}, expected {expected_digest}",
                asset.name,
                digest
            );
        }
    }
    let expected_names = expected.keys().cloned().collect::<HashSet<_>>();
    if actual_names != expected_names {
        let mut missing = expected_names
            .difference(&actual_names)
            .cloned()
            .collect::<Vec<_>>();
        missing.sort();
        bail!("GitHub release is missing assets: {}", missing.join(", "));
    }
    Ok(())
}

fn run_status(command: &mut Command, description: &str) -> Result<()> {
    let status = command
        .status()
        .with_context(|| format!("could not run {description}"))?;
    if !status.success() {
        bail!("{description} failed");
    }
    Ok(())
}

fn run_bounded_output(
    command: &mut Command,
    description: &str,
    stdout_limit: usize,
    stderr_limit: usize,
) -> Result<BoundedCommandOutput> {
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .with_context(|| format!("could not run {description}"))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| anyhow!("could not capture stdout for {description}"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| anyhow!("could not capture stderr for {description}"))?;
    let stdout_reader = thread::spawn(move || read_bounded(stdout, stdout_limit));
    let stderr_reader = thread::spawn(move || read_bounded(stderr, stderr_limit));
    let status = child
        .wait()
        .with_context(|| format!("could not wait for {description}"))?;
    let stdout = stdout_reader
        .join()
        .map_err(|_| anyhow!("stdout reader panicked for {description}"))?
        .with_context(|| format!("could not read stdout for {description}"))?;
    let stderr = stderr_reader
        .join()
        .map_err(|_| anyhow!("stderr reader panicked for {description}"))?
        .with_context(|| format!("could not read stderr for {description}"))?;
    if stdout.exceeded {
        bail!("{description} stdout exceeded the {stdout_limit}-byte limit");
    }
    if stderr.exceeded {
        bail!("{description} stderr exceeded the {stderr_limit}-byte limit");
    }
    Ok(BoundedCommandOutput {
        status,
        stdout: stdout.bytes,
        stderr: stderr.bytes,
    })
}

fn read_bounded<R: Read>(mut reader: R, limit: usize) -> io::Result<BoundedRead> {
    let mut bytes = Vec::with_capacity(limit.min(64 * 1024));
    let mut exceeded = false;
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let read = reader.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        let retain = (limit - bytes.len()).min(read);
        bytes.extend_from_slice(&buffer[..retain]);
        exceeded |= retain != read;
    }
    Ok(BoundedRead { bytes, exceeded })
}

fn run_status_with_input(command: &mut Command, description: &str, input: &[u8]) -> Result<()> {
    let mut child = command
        .stdin(Stdio::piped())
        .spawn()
        .with_context(|| format!("could not run {description}"))?;
    let write_result = child
        .stdin
        .take()
        .ok_or_else(|| anyhow!("could not open stdin for {description}"))?
        .write_all(input)
        .with_context(|| format!("could not send input to {description}"));
    let status = child
        .wait()
        .with_context(|| format!("could not wait for {description}"))?;
    write_result?;
    if !status.success() {
        bail!("{description} failed");
    }
    Ok(())
}

fn ensure_no_args(usage: &str, args: &[String]) -> Result<()> {
    if !args.is_empty() {
        bail!("usage: vo-dev {usage}");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn release_repository_is_canonical_and_pins_gh_commands() {
        let repository = parse_release_repository("volang", "github.com/vo-lang/volang").unwrap();
        assert_eq!(repository.api_repo, "vo-lang/volang");
        assert_eq!(repository.gh_repo, "github.com/vo-lang/volang");
        assert_eq!(repository.git_url, "https://github.com/vo-lang/volang.git");

        let mut command = Command::new("gh");
        command.args(["release", "view", "v0.1.4"]);
        pin_gh_repository(&mut command, &repository);
        let args = command
            .get_args()
            .map(|argument| argument.to_string_lossy().into_owned())
            .collect::<Vec<_>>();
        assert_eq!(
            args,
            [
                "release",
                "view",
                "v0.1.4",
                "--repo",
                "github.com/vo-lang/volang"
            ]
        );
    }

    #[test]
    fn release_repository_rejects_ambiguous_or_mismatched_modules() {
        for module in [
            "vo-lang/volang",
            "https://github.com/vo-lang/volang",
            "github.com/vo-lang/volang/extra",
            "github.com/-vo-lang/volang",
            "github.com/vo-lang/../volang",
            "github.com/vo lang/volang",
        ] {
            assert!(
                parse_release_repository("volang", module).is_err(),
                "{module}"
            );
        }
        assert!(parse_release_repository("other", "github.com/vo-lang/volang").is_err());
    }

    #[test]
    fn existing_published_release_is_an_idempotent_terminal_state() {
        let release = GitHubReleaseView {
            body: "verified notes\n".to_string(),
            is_draft: false,
            is_immutable: true,
            is_prerelease: false,
            name: "Vo v0.1.1".to_string(),
            tag_name: "v0.1.1".to_string(),
        };

        assert_eq!(
            validate_existing_release(&release, "v0.1.1", false, "Vo v0.1.1", "verified notes\n")
                .unwrap(),
            ExistingReleaseState::Published
        );
    }

    #[test]
    fn draft_release_must_match_verified_identity_and_content() {
        let mut release = GitHubReleaseView {
            body: "verified notes\n".to_string(),
            is_draft: true,
            is_immutable: false,
            is_prerelease: false,
            name: "Vo v0.1.4".to_string(),
            tag_name: "v0.1.4".to_string(),
        };

        validate_draft_release(&release, "v0.1.4", false, "Vo v0.1.4", "verified notes\n").unwrap();

        release.body = "changed".to_string();
        assert!(
            validate_draft_release(&release, "v0.1.4", false, "Vo v0.1.4", "verified notes\n",)
                .unwrap_err()
                .to_string()
                .contains("notes")
        );

        release.body = "verified notes\n".to_string();
        release.is_draft = false;
        assert!(
            validate_draft_release(&release, "v0.1.4", false, "Vo v0.1.4", "verified notes\n",)
                .unwrap_err()
                .to_string()
                .contains("published before")
        );
    }

    #[test]
    fn existing_published_release_must_match_immutable_identity_and_content() {
        let mut release = GitHubReleaseView {
            body: "verified notes\n".to_string(),
            is_draft: false,
            is_immutable: true,
            is_prerelease: true,
            name: "Vo v0.1.1".to_string(),
            tag_name: "v0.1.1".to_string(),
        };

        assert!(validate_existing_release(
            &release,
            "v0.1.1",
            false,
            "Vo v0.1.1",
            "verified notes\n"
        )
        .unwrap_err()
        .to_string()
        .contains("prerelease state mismatch"));
        assert!(validate_existing_release(
            &release,
            "v0.1.2",
            true,
            "Vo v0.1.2",
            "verified notes\n"
        )
        .unwrap_err()
        .to_string()
        .contains("tag mismatch"));

        release.is_prerelease = false;
        release.is_immutable = false;
        assert!(validate_existing_release(
            &release,
            "v0.1.1",
            false,
            "Vo v0.1.1",
            "verified notes\n"
        )
        .unwrap_err()
        .to_string()
        .contains("mutable"));

        release.is_immutable = true;
        release.name = "wrong".to_string();
        assert!(validate_existing_release(
            &release,
            "v0.1.1",
            false,
            "Vo v0.1.1",
            "verified notes\n"
        )
        .unwrap_err()
        .to_string()
        .contains("title mismatch"));

        release.name = "Vo v0.1.1".to_string();
        release.body = "wrong notes".to_string();
        assert!(validate_existing_release(
            &release,
            "v0.1.1",
            false,
            "Vo v0.1.1",
            "verified notes\n"
        )
        .unwrap_err()
        .to_string()
        .contains("notes"));
    }

    #[test]
    fn remote_tag_parser_prefers_peeled_annotated_commit() {
        let tag_object = "1".repeat(40);
        let commit = "2".repeat(40);
        let output = format!("{tag_object}\trefs/tags/v0.1.1\n{commit}\trefs/tags/v0.1.1^{{}}\n");

        assert_eq!(
            parse_remote_tag_commit(&output, "refs/tags/v0.1.1", "refs/tags/v0.1.1^{}",).unwrap(),
            commit
        );
    }

    #[test]
    fn remote_tag_parser_accepts_lightweight_tag_and_rejects_extra_refs() {
        let commit = "3".repeat(40);
        assert_eq!(
            parse_remote_tag_commit(
                &format!("{commit}\trefs/tags/v0.1.1\n"),
                "refs/tags/v0.1.1",
                "refs/tags/v0.1.1^{}",
            )
            .unwrap(),
            commit
        );
        assert!(parse_remote_tag_commit(
            &format!("{commit}\trefs/tags/v0.1.2\n"),
            "refs/tags/v0.1.1",
            "refs/tags/v0.1.1^{}",
        )
        .unwrap_err()
        .to_string()
        .contains("unexpected ref"));
    }

    #[test]
    fn release_asset_snapshot_rejects_same_size_content_aba() {
        let root = std::env::temp_dir().join(format!(
            "vo-dev-release-asset-snapshot-{}",
            std::process::id()
        ));
        if root.exists() {
            fs::remove_dir_all(&root).unwrap();
        }
        fs::create_dir_all(&root).unwrap();
        let asset = root.join("asset.tar.gz");
        fs::write(&asset, b"first").unwrap();
        let snapshot = snapshot_release_assets(std::slice::from_ref(&asset)).unwrap();
        fs::write(&asset, b"other").unwrap();

        let error = verify_local_release_asset_snapshot(&snapshot).unwrap_err();
        assert!(error.to_string().contains("digest"));
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn release_notes_reader_enforces_size_and_utf8_boundaries() {
        let root = std::env::temp_dir().join(format!(
            "vo-dev-release-notes-boundary-{}",
            std::process::id()
        ));
        if root.exists() {
            fs::remove_dir_all(&root).unwrap();
        }
        fs::create_dir_all(&root).unwrap();

        let oversized = root.join("oversized.md");
        File::create(&oversized)
            .unwrap()
            .set_len(MAX_RELEASE_NOTES_SIZE + 1)
            .unwrap();
        assert!(read_regular_text_with_limit(
            &oversized,
            "release notes file",
            MAX_RELEASE_NOTES_SIZE
        )
        .unwrap_err()
        .to_string()
        .contains("size limit"));

        let invalid_utf8 = root.join("invalid.md");
        fs::write(&invalid_utf8, [0xff]).unwrap();
        assert!(read_regular_text_with_limit(
            &invalid_utf8,
            "release notes file",
            MAX_RELEASE_NOTES_SIZE
        )
        .unwrap_err()
        .to_string()
        .contains("UTF-8"));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn bounded_subprocess_capture_rejects_oversized_stdout_and_stderr() {
        let executable = std::env::current_exe().unwrap();

        let mut stdout_child = Command::new(&executable);
        stdout_child
            .args([
                "--exact",
                "release_system::tests::bounded_output_child",
                "--nocapture",
            ])
            .env("VO_DEV_BOUNDED_OUTPUT_CHILD", "stdout");
        let stdout_error =
            run_bounded_output(&mut stdout_child, "stdout limit test", 256, 1024 * 1024)
                .unwrap_err();
        assert!(stdout_error.to_string().contains("stdout exceeded"));

        let mut stderr_child = Command::new(executable);
        stderr_child
            .args([
                "--exact",
                "release_system::tests::bounded_output_child",
                "--nocapture",
            ])
            .env("VO_DEV_BOUNDED_OUTPUT_CHILD", "stderr");
        let stderr_error =
            run_bounded_output(&mut stderr_child, "stderr limit test", 1024 * 1024, 256)
                .unwrap_err();
        assert!(stderr_error.to_string().contains("stderr exceeded"));
    }

    #[test]
    fn bounded_output_child() {
        match std::env::var("VO_DEV_BOUNDED_OUTPUT_CHILD").as_deref() {
            Ok("stdout") => std::io::stdout().write_all(&[b'x'; 4096]).unwrap(),
            Ok("stderr") => std::io::stderr().write_all(&[b'e'; 4096]).unwrap(),
            _ => {}
        }
    }
}
