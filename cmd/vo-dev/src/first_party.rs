use crate::config::{load_project, ProjectFile, ProjectRepo};
use crate::github_output::write_github_output;
use anyhow::{anyhow, bail, Context, Result};
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

pub(crate) fn cmd_first_party(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev first-party path|run|run-workspace|ci-checkout|release-verify ...");
    }
    match args.remove(0).as_str() {
        "path" => {
            if args.is_empty() || args.len() > 2 {
                bail!("usage: vo-dev first-party path <repo> [subdir]");
            }
            let repo = &args[0];
            let mut path = first_party_repo_path(root, repo)?;
            if let Some(subdir) = args.get(1) {
                path = path.join(subdir);
            }
            println!("{}", path.display());
            Ok(())
        }
        "run" => {
            if args.len() < 4 {
                bail!("usage: vo-dev first-party run <repo> <subdir> -- <command...>");
            }
            let repo = args.remove(0);
            let subdir = args.remove(0);
            if args.first().map(|arg| arg == "--").unwrap_or(false) {
                args.remove(0);
            }
            if args.is_empty() {
                bail!("missing command after --");
            }
            let cwd = first_party_repo_path(root, &repo)?.join(subdir);
            if !cwd.is_dir() {
                bail!("missing first-party directory: {}", cwd.display());
            }
            let status = Command::new(&args[0])
                .args(&args[1..])
                .current_dir(&cwd)
                .status()
                .with_context(|| format!("could not run command in {}", cwd.display()))?;
            if !status.success() {
                bail!("first-party command failed in {}", cwd.display());
            }
            Ok(())
        }
        "run-workspace" => {
            if args.len() < 4 {
                bail!("usage: vo-dev first-party run-workspace <repo> <workspace> -- <command...>");
            }
            let repo = args.remove(0);
            let workspace = args.remove(0);
            if args.first().map(|arg| arg == "--").unwrap_or(false) {
                args.remove(0);
            }
            if args.is_empty() {
                bail!("missing command after --");
            }
            let cwd = first_party_workspace_path(root, &repo, &workspace)?;
            let status = Command::new(&args[0])
                .args(&args[1..])
                .current_dir(&cwd)
                .status()
                .with_context(|| {
                    format!("could not run command in {repo} workspace {workspace}")
                })?;
            if !status.success() {
                bail!("first-party command failed in {repo} workspace {workspace}");
            }
            Ok(())
        }
        "ci-checkout" => {
            let mut github_output = false;
            let mut filtered = Vec::new();
            for arg in args {
                if arg == "--github-output" {
                    github_output = true;
                } else {
                    filtered.push(arg);
                }
            }
            if filtered.len() != 1 {
                bail!("usage: vo-dev first-party ci-checkout <repo> [--github-output]");
            }
            let checkout = ci_checkout_for(root, &filtered[0])?;
            if github_output {
                write_github_output(&[
                    ("repo", checkout.repo),
                    ("repository", checkout.repository),
                    ("path", checkout.path),
                    ("enabled", checkout.enabled.to_string()),
                ])
            } else {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&serde_json::json!({
                        "repo": checkout.repo,
                        "repository": checkout.repository,
                        "path": checkout.path,
                        "enabled": checkout.enabled,
                    }))?
                );
                Ok(())
            }
        }
        "release-verify" => {
            if args.len() != 1 {
                bail!("usage: vo-dev first-party release-verify <repo>");
            }
            let repo = &args[0];
            let module = first_party_repo_path(root, repo)?;
            if !module.join("vo.mod").exists() {
                bail!("first-party repo {} does not contain vo.mod", repo);
            }
            println!("release verify {repo}: {}", module.display());
            let status = Command::new("cargo")
                .args(["run", "-p", "vo", "--release", "--", "release", "verify"])
                .arg(&module)
                .current_dir(root)
                .status()
                .context("could not run release verify")?;
            if !status.success() {
                bail!("release verify failed for {repo}");
            }
            Ok(())
        }
        other => bail!("unknown first-party command: {other}"),
    }
}

pub(crate) fn cmd_studio_install_local_vogui(root: &Path) -> Result<()> {
    let package_path = first_party_workspace_path(root, "vogui", "js")?;
    let status = Command::new("npm")
        .args(["install", "--no-save"])
        .arg(&package_path)
        .current_dir(root.join("apps/studio"))
        .status()
        .context("could not install local @vogui/runtime")?;
    if !status.success() {
        bail!("npm install local @vogui/runtime failed");
    }
    Ok(())
}

fn first_party_repo_path(root: &Path, repo: &str) -> Result<PathBuf> {
    let project = load_project(root)?;
    let entry = project
        .first_party
        .iter()
        .find(|item| item.name == repo)
        .ok_or_else(|| anyhow!("unknown first-party repo: {repo}"))?;
    repo_path_from_entry(root, repo, entry, true)
}

pub(crate) fn first_party_workspace_path(
    root: &Path,
    repo: &str,
    workspace: &str,
) -> Result<PathBuf> {
    let project = load_project(root)?;
    let entry = project
        .first_party
        .iter()
        .find(|item| item.name == repo)
        .ok_or_else(|| anyhow!("unknown first-party repo: {repo}"))?;
    let workspace_path = entry
        .workspace
        .iter()
        .find(|item| item.name == workspace)
        .ok_or_else(|| anyhow!("unknown first-party workspace: {repo}:{workspace}"))?
        .path
        .clone();
    let path = first_party_repo_path(root, repo)?.join(&workspace_path);
    if !path.is_dir() {
        bail!(
            "missing first-party workspace directory {repo}:{workspace}: {}",
            path.display()
        );
    }
    Ok(path)
}

#[derive(Debug)]
pub(crate) struct CiCheckout {
    pub(crate) repo: String,
    pub(crate) repository: String,
    pub(crate) path: String,
    pub(crate) enabled: bool,
}

pub(crate) fn ci_checkout_for(root: &Path, repo: &str) -> Result<CiCheckout> {
    let project = load_project(root)?;
    let entry = project_repo_entry(&project, repo)
        .ok_or_else(|| anyhow!("unknown project repo: {repo}"))?;
    if entry.ci_checkout != Some(true) {
        return Ok(CiCheckout {
            repo: repo.to_string(),
            repository: String::new(),
            path: String::new(),
            enabled: false,
        });
    }
    let repository = entry
        .repository
        .clone()
        .ok_or_else(|| anyhow!("project repo {repo} has ci_checkout=true but no repository"))?;
    Ok(CiCheckout {
        repo: repo.to_string(),
        repository,
        path: format!("ci_modules/{repo}"),
        enabled: true,
    })
}

pub(crate) fn ci_checkout_untracked_prefixes(root: &Path) -> Result<Vec<String>> {
    if !root.join("eng/project.toml").exists() {
        return Ok(Vec::new());
    }
    let project = load_project(root)?;
    let mut prefixes = Vec::new();
    for entry in project
        .first_party
        .iter()
        .chain(project.external_project.iter())
    {
        if entry.ci_checkout == Some(true) {
            let prefix = format!("ci_modules/{}/", entry.name);
            prefixes.push(prefix);
        }
    }
    prefixes.sort();
    prefixes.dedup();
    Ok(prefixes)
}

pub(crate) fn project_repo_path(root: &Path, repo: &str) -> Result<PathBuf> {
    let project = load_project(root)?;
    let entry = project_repo_entry(&project, repo)
        .ok_or_else(|| anyhow!("unknown project repo: {repo}"))?;
    repo_path_from_entry(root, repo, entry, entry.ci_checkout == Some(true))
}

pub(crate) fn project_repo_entry<'a>(
    project: &'a ProjectFile,
    repo: &str,
) -> Option<&'a ProjectRepo> {
    project
        .first_party
        .iter()
        .chain(project.external_project.iter())
        .find(|item| item.name == repo)
}

fn repo_path_from_entry(
    root: &Path,
    repo: &str,
    entry: &ProjectRepo,
    include_ci_paths: bool,
) -> Result<PathBuf> {
    let mut candidates = Vec::new();
    if include_ci_paths {
        if let Ok(module_root) = env::var("CI_MODULE_ROOT") {
            if !module_root.trim().is_empty() {
                candidates.push(PathBuf::from(module_root).join(repo));
            }
        }
        candidates.push(root.join("ci_modules").join(repo));
    }
    if let Some(local_hint) = &entry.local_hint {
        candidates.push(root.join(local_hint));
    }

    for candidate in &candidates {
        if candidate.exists() {
            return candidate
                .canonicalize()
                .with_context(|| format!("could not canonicalize {}", candidate.display()));
        }
    }
    bail!(
        "could not find project repo {repo}; searched: {}",
        candidates
            .iter()
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", ")
    )
}
