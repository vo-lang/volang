mod evidence;
mod model;
mod plan;

use anyhow::{anyhow, bail, Result};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Component, Path, PathBuf};

pub(crate) struct VerifiedCertification {
    pub(crate) profile: String,
    pub(crate) status: String,
    pub(crate) commit: String,
    pub(crate) sha256: String,
}

pub(crate) fn cmd_ci(root: &Path, mut args: Vec<String>) -> Result<()> {
    let command = args
        .first()
        .map(String::as_str)
        .ok_or_else(|| anyhow!(usage()))?;
    match command {
        "lint" => {
            args.remove(0);
            if !args.is_empty() {
                bail!(usage());
            }
            let manifest = model::load_manifest(root)?;
            println!(
                "CI contract: {} profiles, {} tasks",
                manifest.profiles.len(),
                manifest.tasks.len()
            );
            Ok(())
        }
        "plan" => {
            args.remove(0);
            cmd_plan(root, args, false)
        }
        "explain" => {
            args.remove(0);
            cmd_plan(root, args, true)
        }
        "record" => {
            args.remove(0);
            cmd_record(root, args)
        }
        "certify" => {
            args.remove(0);
            cmd_certify(root, args)
        }
        "verify" => {
            args.remove(0);
            cmd_verify(root, args)
        }
        _ => bail!(usage()),
    }
}

fn cmd_plan(root: &Path, args: Vec<String>, explain: bool) -> Result<()> {
    let mut profile = explain.then(|| "pull-request".to_string());
    let mut base = None;
    let mut head = None;
    let mut changed_files = Vec::new();
    let mut output = None;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--profile" => profile = Some(next_value(&args, &mut index, "--profile")?),
            "--base" => base = Some(next_value(&args, &mut index, "--base")?),
            "--head" => head = Some(next_value(&args, &mut index, "--head")?),
            "--changed-file" => {
                changed_files.push(next_value(&args, &mut index, "--changed-file")?)
            }
            "--output" => output = Some(next_value(&args, &mut index, "--output")?),
            argument => bail!("unknown ci plan argument {argument}\n{}", usage()),
        }
        index += 1;
    }
    let profile = profile.ok_or_else(|| anyhow!("ci plan requires --profile"))?;
    let plan = plan::build_plan(
        root,
        &profile,
        base.as_deref(),
        head.as_deref(),
        &changed_files,
    )?;
    if let Some(output) = output {
        let output = resolve_repo_output(root, &output)?;
        plan::write_plan(&output, &plan)?;
    }
    if explain {
        for (id, reasons) in &plan.decisions {
            println!("{id}:\n  {}", reasons.join("\n  "));
        }
    } else {
        println!("{}", serde_json::to_string_pretty(&plan)?);
    }
    Ok(())
}

fn cmd_record(root: &Path, args: Vec<String>) -> Result<()> {
    let mut plan_path = None;
    let mut task_id = None;
    let mut output = None;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--plan" => plan_path = Some(next_value(&args, &mut index, "--plan")?),
            "--task" => task_id = Some(next_value(&args, &mut index, "--task")?),
            "--output" => output = Some(next_value(&args, &mut index, "--output")?),
            argument => bail!("unknown ci record argument {argument}\n{}", usage()),
        }
        index += 1;
    }
    let plan_path = resolve_repo_input(
        root,
        &plan_path.ok_or_else(|| anyhow!("ci record requires --plan"))?,
    )?;
    let task_id = task_id.ok_or_else(|| anyhow!("ci record requires --task"))?;
    let output = resolve_repo_output(
        root,
        &output.ok_or_else(|| anyhow!("ci record requires --output"))?,
    )?;
    evidence::record(
        root,
        evidence::RecordOptions {
            plan_path: &plan_path,
            task_id: &task_id,
            output: &output,
        },
    )?;
    println!("recorded certifiable CI evidence for {task_id}");
    Ok(())
}

fn cmd_certify(root: &Path, args: Vec<String>) -> Result<()> {
    let mut plan_path = None;
    let mut evidence_dir = None;
    let mut output = None;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--plan" => plan_path = Some(next_value(&args, &mut index, "--plan")?),
            "--evidence-dir" => {
                evidence_dir = Some(next_value(&args, &mut index, "--evidence-dir")?)
            }
            "--output" => output = Some(next_value(&args, &mut index, "--output")?),
            argument => bail!("unknown ci certify argument {argument}\n{}", usage()),
        }
        index += 1;
    }
    let plan_path = resolve_repo_input(
        root,
        &plan_path.ok_or_else(|| anyhow!("ci certify requires --plan"))?,
    )?;
    let evidence_dir = resolve_repo_input(
        root,
        &evidence_dir.ok_or_else(|| anyhow!("ci certify requires --evidence-dir"))?,
    )?;
    let output = resolve_repo_output(
        root,
        &output.ok_or_else(|| anyhow!("ci certify requires --output"))?,
    )?;
    let bundle = evidence::certify(root, &plan_path, &evidence_dir, &output)?;
    println!(
        "certified CI profile {} with {} task receipts",
        bundle.profile,
        bundle.evidence.len()
    );
    Ok(())
}

fn cmd_verify(root: &Path, args: Vec<String>) -> Result<()> {
    let mut bundle_path = None;
    let mut profile = None;
    let mut artifact_task = None;
    let mut artifact = None;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--bundle" => bundle_path = Some(next_value(&args, &mut index, "--bundle")?),
            "--profile" => profile = Some(next_value(&args, &mut index, "--profile")?),
            "--artifact-task" => {
                artifact_task = Some(next_value(&args, &mut index, "--artifact-task")?)
            }
            "--artifact" => artifact = Some(next_value(&args, &mut index, "--artifact")?),
            argument => bail!("unknown ci verify argument {argument}\n{}", usage()),
        }
        index += 1;
    }
    if artifact_task.is_some() != artifact.is_some() {
        bail!("--artifact-task and --artifact must be supplied together");
    }
    let bundle_path = resolve_repo_input(
        root,
        &bundle_path.ok_or_else(|| anyhow!("ci verify requires --bundle"))?,
    )?;
    let bundle = evidence::read_and_verify_bundle(root, &bundle_path, profile.as_deref())?;
    if let (Some(task), Some(artifact)) = (artifact_task, artifact) {
        let artifact = resolve_repo_input(root, &artifact)?;
        evidence::verify_artifact(root, &bundle, &task, &artifact)?;
    }
    println!(
        "verified CI certification {} for {} at {}",
        bundle.status, bundle.profile, bundle.plan.source.commit
    );
    Ok(())
}

pub(crate) fn verify_ui_bundle(root: &Path, path: &Path) -> Result<()> {
    let bundle = evidence::read_and_verify_bundle(root, path, None)?;
    evidence::require_ui_evidence(&bundle)
}

pub(crate) fn verify_release_bundle(root: &Path, path: &Path) -> Result<VerifiedCertification> {
    let bundle = evidence::read_and_verify_bundle(root, path, Some("main"))?;
    evidence::require_ui_evidence(&bundle)?;
    let bytes = fs::read(path)?;
    Ok(VerifiedCertification {
        profile: bundle.profile,
        status: bundle.status,
        commit: bundle.plan.source.commit,
        sha256: format!("{:x}", Sha256::digest(&bytes)),
    })
}

fn next_value(args: &[String], index: &mut usize, flag: &str) -> Result<String> {
    *index += 1;
    args.get(*index)
        .cloned()
        .ok_or_else(|| anyhow!("{flag} requires a value"))
}

fn resolve_repo_input(root: &Path, value: &str) -> Result<PathBuf> {
    resolve_repo_path(root, value, false)
}

fn resolve_repo_output(root: &Path, value: &str) -> Result<PathBuf> {
    resolve_repo_path(root, value, true)
}

fn resolve_repo_path(root: &Path, value: &str, output: bool) -> Result<PathBuf> {
    let relative = Path::new(value);
    if value.is_empty()
        || relative.is_absolute()
        || relative.components().any(|component| {
            matches!(
                component,
                Component::ParentDir | Component::RootDir | Component::Prefix(_)
            )
        })
    {
        bail!("CI path must be repository-relative: {value:?}");
    }
    if output && !value.replace('\\', "/").starts_with("target/ci/") {
        bail!("CI outputs must stay under target/ci: {value:?}");
    }
    Ok(root.join(relative))
}

fn usage() -> &'static str {
    "usage:\n  vo-dev ci lint\n  vo-dev ci plan --profile <name> [--base <rev> --head <rev> | --changed-file <path>...] [--output target/ci/plan.json]\n  vo-dev ci explain --base <rev> --head <rev>\n  vo-dev ci record --plan <path> --task <id> --output target/ci/evidence/<id>.evidence.json\n  vo-dev ci certify --plan <path> --evidence-dir <dir> --output target/ci/certification.json\n  vo-dev ci verify --bundle <path> [--profile <name>] [--artifact-task <id> --artifact <path>]"
}
