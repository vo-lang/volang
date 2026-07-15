use crate::config::load_release;
use crate::github_output::write_github_output;
use crate::release_archive::{
    clear_release_build_outputs, package_release_binary, record_release_build,
    validate_release_artifacts, write_text_atomic,
};
use crate::release_config::{
    artifact_name, lint_release_file, read_checked_sha256, release_artifact_files, release_target,
    sha256_file,
};
use crate::release_homebrew::{
    homebrew_checkout_path, replace_formula_target_sha, replace_release_version,
    validate_homebrew_formula_targets,
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
    use_cross: bool,
    artifact_name: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GitHubReleaseView {
    is_draft: bool,
    is_prerelease: bool,
    tag_name: String,
    target_commitish: String,
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

#[derive(Clone, Debug, Eq, PartialEq)]
struct ReleaseAssetSnapshot {
    path: PathBuf,
    name: String,
    size: u64,
    sha256: String,
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
        bail!("usage: vo-dev release matrix|metadata|cross-version|version|sdk-plan|homebrew-repository|homebrew-metadata|build|package|notes|publish|update-homebrew ...");
    }
    match args.remove(0).as_str() {
        "matrix" => cmd_matrix(root, args),
        "metadata" => cmd_metadata(root, args),
        "cross-version" => {
            ensure_no_args("release cross-version", &args)?;
            let release = load_release(root)?;
            lint_release_file(&release)?;
            println!("{}", release.cross.version);
            Ok(())
        }
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
        "notes" => cmd_notes(root, args),
        "publish" => cmd_publish(root, args),
        "update-homebrew" => cmd_update_homebrew(root, args),
        other => bail!("unknown release command: {other}"),
    }
}

pub(crate) fn lint_release(root: &Path) -> Result<()> {
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let workflow_path = root.join(".github/workflows/release.yml");
    let workflow = fs::read_to_string(&workflow_path)
        .with_context(|| format!("could not read {}", workflow_path.display()))?;
    lint_release_workflow_text(&workflow)?;
    let setup_path = root.join(".github/actions/setup-rust/action.yml");
    let setup = fs::read_to_string(&setup_path)
        .with_context(|| format!("could not read {}", setup_path.display()))?;
    lint_external_action_pins("release Rust setup Action", &setup)?;
    validate_sdk_publish_boundary(root, &release)
}

fn lint_release_workflow_text(workflow: &str) -> Result<()> {
    for required in [
        "release metadata --tag",
        "task run release-contract",
        "release build \"${{ matrix.target }}\"",
        "release package \"${{ matrix.target }}\"",
        "release publish --tag",
        "VO_RELEASE_TAG: ${{ needs.plan.outputs.tag }}",
        "VO_BUILD_COMMIT: ${{ needs.plan.outputs.commit }}",
        "VO_BUILD_DATE: ${{ needs.plan.outputs.build_date }}",
        "SOURCE_DATE_EPOCH: ${{ needs.plan.outputs.source_date_epoch }}",
        "${{ matrix.artifact_name }}.provenance.json",
        "if-no-files-found: error",
        "cancel-in-progress: false",
        "fetch-depth: 0",
        "HOMEBREW_TAP_TOKEN: ${{ secrets.HOMEBREW_TAP_TOKEN }}",
        "x-access-token:${HOMEBREW_TAP_TOKEN}",
    ] {
        if !workflow.contains(required) {
            bail!("release workflow is missing required protocol fragment {required:?}");
        }
    }
    for forbidden in [
        "VO_BUILD_DATE: ${{ github.ref_name }}",
        "group: release-${{",
        "tar -czf",
        "cargo publish",
    ] {
        if workflow.contains(forbidden) {
            bail!("release workflow contains forbidden protocol fragment {forbidden:?}");
        }
    }
    for line in workflow.lines() {
        if line.contains("cargo run")
            && line.contains("-p vo-dev")
            && line.contains("-- release")
            && !line.contains("--locked")
        {
            bail!("release workflow vo-dev commands must use Cargo.lock: {line}");
        }
    }
    if !workflow.lines().any(|line| line.trim() == "group: release") {
        bail!("release workflow must serialize every tag through concurrency group 'release'");
    }
    lint_external_action_pins("release workflow", workflow)?;
    Ok(())
}

fn lint_external_action_pins(source: &str, contents: &str) -> Result<()> {
    let mut external_actions = 0_usize;
    for (index, line) in contents.lines().enumerate() {
        let trimmed = line.trim();
        let Some(spec) = trimmed
            .strip_prefix("- uses:")
            .or_else(|| trimmed.strip_prefix("uses:"))
            .map(str::trim)
            .and_then(|value| value.split_whitespace().next())
        else {
            continue;
        };
        if spec.starts_with("./") {
            continue;
        }
        external_actions += 1;
        let Some((action, revision)) = spec.rsplit_once('@') else {
            bail!(
                "{source} external Action has no immutable revision on line {}: {spec}",
                index + 1
            );
        };
        if action.is_empty()
            || revision.len() != 40
            || !revision.chars().all(|ch| ch.is_ascii_hexdigit())
            || revision != revision.to_ascii_lowercase()
        {
            bail!(
                "{source} external Action must use a full lowercase commit id on line {}: {spec}",
                index + 1
            );
        }
    }
    if external_actions == 0 {
        bail!("{source} must declare at least one pinned external Action");
    }
    Ok(())
}

fn cmd_metadata(root: &Path, args: Vec<String>) -> Result<()> {
    let mut tag = None;
    let mut commit = None;
    let mut github_output = false;
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
            "--github-output" => github_output = true,
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
    if github_output {
        write_github_output(&[
            ("tag", identity.tag),
            ("version", identity.version),
            ("commit", identity.commit),
            ("build_date", identity.build_date),
            ("source_date_epoch", identity.source_date_epoch.to_string()),
        ])
    } else {
        println!("{}", serde_json::to_string_pretty(&identity)?);
        Ok(())
    }
}

fn cmd_matrix(root: &Path, args: Vec<String>) -> Result<()> {
    let github_output = parse_flag_only(args, "--github-output", "release matrix")?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let matrix = ReleaseMatrix {
        include: release
            .targets
            .iter()
            .map(|target| ReleaseMatrixRow {
                target: target.target.clone(),
                os: target.os.clone(),
                use_cross: target.use_cross,
                artifact_name: artifact_name(&release, &target.target),
            })
            .collect(),
    };
    if github_output {
        write_github_output(&[("matrix", serde_json::to_string(&matrix)?)])
    } else {
        println!("{}", serde_json::to_string_pretty(&matrix)?);
        Ok(())
    }
}

fn cmd_version(root: &Path, args: Vec<String>) -> Result<()> {
    let mut tag = None;
    let mut github_output = false;
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
            "--github-output" => github_output = true,
            other => bail!("unknown release version argument: {other}"),
        }
        i += 1;
    }
    let tag = release_tag(tag.as_deref())?;
    let version = validated_release_version(root, &tag)?;
    if github_output {
        write_github_output(&[("version", version)])
    } else {
        println!("{version}");
        Ok(())
    }
}

fn cmd_homebrew_repository(root: &Path, args: Vec<String>) -> Result<()> {
    let github_output = parse_flag_only(args, "--github-output", "release homebrew-repository")?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    if github_output {
        write_github_output(&[("repository", release.homebrew.repository)])
    } else {
        println!("{}", release.homebrew.repository);
        Ok(())
    }
}

fn cmd_homebrew_metadata(root: &Path, args: Vec<String>) -> Result<()> {
    let github_output = parse_flag_only(args, "--github-output", "release homebrew-metadata")?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let checkout_path = homebrew_checkout_path(&release)?;
    if github_output {
        write_github_output(&[
            ("repository", release.homebrew.repository),
            ("checkout_path", checkout_path),
            ("formula_path", release.homebrew.formula_path),
        ])
    } else {
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

    let binary = if target.use_cross { "cross" } else { "cargo" };
    let mut command = Command::new(binary);
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
    run_status(&mut command, &format!("{binary} build {}", target.target))?;
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

    if let Some(existing) = github_release_view(root, &tag)? {
        if existing.tag_name != tag {
            bail!(
                "GitHub release tag mismatch: expected {tag}, got {}",
                existing.tag_name
            );
        }
        if !existing.is_draft {
            bail!("GitHub release {tag} is already published and is immutable");
        }
        if existing.target_commitish != identity.commit && existing.target_commitish != tag {
            bail!(
                "GitHub draft release {tag} targets {}, expected {}",
                existing.target_commitish,
                identity.commit
            );
        }
        let mut edit = Command::new("gh");
        edit.args(["release", "edit", &tag, "--title"])
            .arg(format!("Vo {tag}"))
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
            .arg(format!("Vo {tag}"))
            .arg("--notes-file")
            .arg("-");
        if prerelease {
            create.arg("--prerelease");
        } else {
            create.arg("--prerelease=false");
        }
        run_status_with_input(
            create.current_dir(root),
            "gh release create draft",
            notes_text.as_bytes(),
        )?;
    }

    let mut upload = Command::new("gh");
    verify_local_release_asset_snapshot(&expected_assets)?;
    upload.args(["release", "upload", &tag]);
    upload.args(&files);
    upload.arg("--clobber");
    run_status(upload.current_dir(root), "gh release upload to draft")?;
    verify_local_release_asset_snapshot(&expected_assets)?;
    verify_remote_release_assets(root, &tag, &expected_assets)?;

    let mut publish = Command::new("gh");
    publish
        .args(["release", "edit", &tag, "--draft=false", "--verify-tag"])
        .arg("--target")
        .arg(&identity.commit);
    if prerelease {
        publish.arg("--prerelease");
    } else {
        publish.args(["--prerelease=false", "--latest"]);
    }
    run_status(
        publish.current_dir(root),
        "gh release publish verified draft",
    )?;
    let published = github_release_view(root, &tag)?
        .ok_or_else(|| anyhow!("GitHub release {tag} disappeared after publication"))?;
    if published.is_draft {
        bail!("GitHub release {tag} remained a draft after publication");
    }
    if published.is_prerelease != prerelease {
        bail!(
            "GitHub release {tag} prerelease state mismatch: expected {prerelease}, got {}",
            published.is_prerelease
        );
    }
    if published.tag_name != tag || published.target_commitish != identity.commit {
        bail!(
            "published GitHub release identity mismatch: expected {tag} at {}, got {} at {}",
            identity.commit,
            published.tag_name,
            published.target_commitish
        );
    }
    verify_remote_release_assets(root, &tag, &expected_assets)?;
    Ok(())
}

fn cmd_update_homebrew(root: &Path, args: Vec<String>) -> Result<()> {
    let mut repo = None;
    let mut artifacts = None;
    let mut version = None;
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

fn github_release_view(root: &Path, tag: &str) -> Result<Option<GitHubReleaseView>> {
    let mut command = Command::new("gh");
    command
        .args([
            "release",
            "view",
            tag,
            "--json",
            "isDraft,isPrerelease,tagName,targetCommitish",
        ])
        .current_dir(root);
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
    tag: &str,
    snapshots: &[ReleaseAssetSnapshot],
) -> Result<()> {
    let mut command = Command::new("gh");
    command
        .args(["release", "view", tag, "--json", "assets"])
        .current_dir(root);
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

fn parse_flag_only(args: Vec<String>, flag: &str, usage: &str) -> Result<bool> {
    let mut seen = false;
    for arg in args {
        if arg == flag {
            seen = true;
        } else {
            bail!("usage: vo-dev {usage} [{flag}]");
        }
    }
    Ok(seen)
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
    fn release_workflow_contract_rejects_tag_as_build_date() {
        let workflow = canonical_workflow().replace(
            "VO_BUILD_DATE: ${{ needs.plan.outputs.build_date }}",
            "VO_BUILD_DATE: ${{ github.ref_name }}",
        );
        let error = lint_release_workflow_text(&workflow).unwrap_err();
        assert!(error.to_string().contains("VO_BUILD_DATE"));
    }

    #[test]
    fn release_workflow_contract_accepts_bound_metadata_and_provenance() {
        lint_release_workflow_text(canonical_workflow()).unwrap();
    }

    #[test]
    fn release_workflow_contract_rejects_unlocked_vo_dev_commands() {
        let workflow = canonical_workflow().replace(
            "cargo run -p vo-dev --locked -- release build",
            "cargo run -p vo-dev -- release build",
        );
        let error = lint_release_workflow_text(&workflow).unwrap_err();
        assert!(error.to_string().contains("Cargo.lock"));
    }

    #[test]
    fn release_workflow_contract_rejects_movable_action_tags() {
        let workflow = canonical_workflow().replace(
            "actions/checkout@93cb6efe18208431cddfb8368fd83d5badbf9bfd",
            "actions/checkout@v5",
        );
        let error = lint_release_workflow_text(&workflow).unwrap_err();
        assert!(error.to_string().contains("full lowercase commit id"));
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

    fn canonical_workflow() -> &'static str {
        r#"
cancel-in-progress: false
group: release
fetch-depth: 0
- uses: actions/checkout@93cb6efe18208431cddfb8368fd83d5badbf9bfd # v5
run: cargo run -p vo-dev --locked -- task run release-contract
run: cargo run -p vo-dev --locked -- release metadata --tag "${{ github.ref_name }}"
run: cargo run -p vo-dev --locked -- release build "${{ matrix.target }}"
run: cargo run -p vo-dev --locked -- release package "${{ matrix.target }}"
run: cargo run -p vo-dev --locked -- release publish --tag "${{ needs.plan.outputs.tag }}"
VO_RELEASE_TAG: ${{ needs.plan.outputs.tag }}
VO_BUILD_COMMIT: ${{ needs.plan.outputs.commit }}
VO_BUILD_DATE: ${{ needs.plan.outputs.build_date }}
SOURCE_DATE_EPOCH: ${{ needs.plan.outputs.source_date_epoch }}
HOMEBREW_TAP_TOKEN: ${{ secrets.HOMEBREW_TAP_TOKEN }}
x-access-token:${HOMEBREW_TAP_TOKEN}
path: ${{ matrix.artifact_name }}.provenance.json
if-no-files-found: error
"#
    }
}
