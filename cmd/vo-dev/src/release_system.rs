use crate::config::load_release;
use crate::github_output::write_github_output;
use crate::release_config::{
    artifact_name, lint_release_file, read_checked_sha256, release_artifact_files,
    release_binary_name, release_target, sha256_file,
};
use crate::release_homebrew::{
    homebrew_checkout_path, replace_formula_target_sha, replace_release_version,
    validate_homebrew_formula_targets,
};
use anyhow::{anyhow, bail, Context, Result};
use serde::Serialize;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

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

pub(crate) fn cmd_release(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev release matrix|cross-version|version|homebrew-repository|homebrew-metadata|build|package|notes|publish|update-homebrew ...");
    }
    match args.remove(0).as_str() {
        "matrix" => cmd_matrix(root, args),
        "cross-version" => {
            ensure_no_args("release cross-version", &args)?;
            let release = load_release(root)?;
            lint_release_file(&release)?;
            println!("{}", release.cross.version);
            Ok(())
        }
        "version" => cmd_version(args),
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
    lint_release_file(&release)
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

fn cmd_version(args: Vec<String>) -> Result<()> {
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
    let tag = tag
        .or_else(|| env::var("GITHUB_REF_NAME").ok())
        .ok_or_else(|| anyhow!("release version requires --tag or GITHUB_REF_NAME"))?;
    let version = tag.strip_prefix('v').unwrap_or(&tag).to_string();
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
        .current_dir(root);
    run_status(&mut command, &format!("{binary} build {}", target.target))
}

fn cmd_package(root: &Path, args: Vec<String>) -> Result<()> {
    if args.len() != 1 {
        bail!("usage: vo-dev release package <target>");
    }
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let target = release_target(&release, &args[0])?;
    let binary_name = release_binary_name(&release, &target.target);
    let binary_path = root
        .join("target")
        .join(&target.target)
        .join("release")
        .join(&binary_name);
    if !binary_path.is_file() {
        bail!("release binary is missing: {}", binary_path.display());
    }
    let tarball = artifact_name(&release, &target.target);
    let status = Command::new("tar")
        .arg("-czf")
        .arg(&tarball)
        .arg("-C")
        .arg(binary_path.parent().unwrap())
        .arg(&binary_name)
        .current_dir(root)
        .status()
        .context("could not run tar")?;
    if !status.success() {
        bail!("tar failed for {}", target.target);
    }
    let digest = sha256_file(&root.join(&tarball))?;
    fs::write(
        root.join(format!("{tarball}.sha256")),
        format!("{digest}  {tarball}\n"),
    )
    .with_context(|| format!("could not write {tarball}.sha256"))?;
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
    let tag = tag
        .or_else(|| env::var("GITHUB_REF_NAME").ok())
        .ok_or_else(|| anyhow!("release notes requires --tag or GITHUB_REF_NAME"))?;
    let out = out.ok_or_else(|| anyhow!("release notes requires --out <path>"))?;
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
    fs::write(root.join(out), notes).context("could not write release notes")
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
    let tag = tag
        .or_else(|| env::var("GITHUB_REF_NAME").ok())
        .ok_or_else(|| anyhow!("release publish requires --tag or GITHUB_REF_NAME"))?;
    let release = load_release(root)?;
    lint_release_file(&release)?;
    let artifacts = root.join(artifacts.unwrap_or_else(|| PathBuf::from("artifacts")));
    let notes = root.join(notes.unwrap_or_else(|| PathBuf::from("RELEASE_NOTES.md")));
    let files = release_artifact_files(&release, &artifacts)?;
    let prerelease = tag.contains('-');

    let exists = Command::new("gh")
        .args(["release", "view", &tag])
        .current_dir(root)
        .status()
        .context("could not run gh release view")?
        .success();
    if exists {
        let mut edit = Command::new("gh");
        edit.args(["release", "edit", &tag, "--title"])
            .arg(format!("Vo {tag}"))
            .arg("--notes-file")
            .arg(&notes);
        if prerelease {
            edit.arg("--prerelease");
        }
        run_status(edit.current_dir(root), "gh release edit")?;

        let mut upload = Command::new("gh");
        upload.args(["release", "upload", &tag]);
        upload.args(&files);
        upload.arg("--clobber");
        run_status(upload.current_dir(root), "gh release upload")?;
    } else {
        let mut create = Command::new("gh");
        create.args(["release", "create", &tag]);
        create.args(&files);
        create.arg("--title").arg(format!("Vo {tag}"));
        create.arg("--notes-file").arg(&notes);
        if prerelease {
            create.arg("--prerelease");
        }
        run_status(create.current_dir(root), "gh release create")?;
    }
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
    let repo = root.join(repo.ok_or_else(|| anyhow!("update-homebrew requires --repo <path>"))?);
    let artifacts =
        root.join(artifacts.ok_or_else(|| anyhow!("update-homebrew requires --artifacts <path>"))?);
    let version = version.ok_or_else(|| anyhow!("update-homebrew requires --version <version>"))?;
    let formula_path = repo.join(&release.homebrew.formula_path);
    let mut text = fs::read_to_string(&formula_path)
        .with_context(|| format!("could not read {}", formula_path.display()))?;

    validate_homebrew_formula_targets(&release, &text)?;
    text = replace_release_version(&text, &version)?;
    for target in &release.targets {
        let sha = read_checked_sha256(&artifacts, &artifact_name(&release, &target.target))?;
        text = replace_formula_target_sha(&text, &target.target, &sha)?;
    }
    fs::write(&formula_path, text)
        .with_context(|| format!("could not write {}", formula_path.display()))
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
