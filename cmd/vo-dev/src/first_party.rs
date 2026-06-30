use crate::config::{load_project, ProjectFile, ProjectRepo};
use crate::github_output::write_github_output;
use anyhow::{anyhow, bail, Context, Result};
use std::env;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

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
            let repo_root = checked_first_party_repo_path(root, &repo)?;
            let cwd = repo_root.join(subdir);
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
            let cwd = checked_first_party_workspace_path(root, &repo, &workspace)?;
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
            let module = checked_first_party_repo_path(root, repo)?;
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
    let package_path = checked_first_party_workspace_path(root, "vogui", "js")?;
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

fn checked_first_party_repo_path(root: &Path, repo: &str) -> Result<PathBuf> {
    let path = first_party_repo_path(root, repo)?;
    ensure_clean_first_party_root(root, repo, &path)?;
    Ok(path)
}

fn checked_first_party_workspace_path(root: &Path, repo: &str, workspace: &str) -> Result<PathBuf> {
    let repo_root = checked_first_party_repo_path(root, repo)?;
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
    let path = repo_root.join(&workspace_path);
    if !path.is_dir() {
        bail!(
            "missing first-party workspace directory {repo}:{workspace}: {}",
            path.display()
        );
    }
    Ok(path)
}

fn ensure_clean_first_party_root(root: &Path, repo: &str, path: &Path) -> Result<()> {
    let inside = git_output(path, &["rev-parse", "--is-inside-work-tree"]).with_context(|| {
        format!(
            "could not inspect first-party repo {repo}: {}",
            path.display()
        )
    })?;
    if !inside.status.success() || String::from_utf8_lossy(&inside.stdout).trim() != "true" {
        bail!(
            "first-party repo {repo} is not a git worktree: {}",
            path.display()
        );
    }
    if path.join("vo.work").exists() {
        bail!(
            "first-party repo {repo} contains vo.work; release/stage verification requires a root without local workspace overrides: {}",
            path.display()
        );
    }
    let status = git_output(path, &["status", "--porcelain=v1", "--untracked-files=all"])?;
    if !status.status.success() {
        bail!(
            "could not read git status for first-party repo {repo}: {}",
            path.display()
        );
    }
    let dirty = String::from_utf8_lossy(&status.stdout);
    if !dirty.trim().is_empty() {
        bail!(
            "first-party repo {repo} is dirty and cannot be used as release/stage proof: {}\n{}",
            path.display(),
            dirty.trim()
        );
    }
    if first_party_path_is_ci_module(root, repo, path) {
        return Ok(());
    }
    let upstream = git_output(
        path,
        &["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"],
    )?;
    if !upstream.status.success() {
        bail!(
            "first-party repo {repo} uses local_hint {} without an upstream; use a clean ci_modules/{repo} checkout or configure upstream tracking",
            path.display()
        );
    }
    let upstream_name = String::from_utf8_lossy(&upstream.stdout).trim().to_string();
    let counts = git_output(
        path,
        &["rev-list", "--left-right", "--count", "HEAD...@{u}"],
    )?;
    if !counts.status.success() {
        bail!("could not compare first-party repo {repo} to upstream {upstream_name}");
    }
    let counts_text = String::from_utf8_lossy(&counts.stdout);
    let mut parts = counts_text.split_whitespace();
    let ahead = parts
        .next()
        .and_then(|part| part.parse::<u32>().ok())
        .unwrap_or(1);
    let behind = parts
        .next()
        .and_then(|part| part.parse::<u32>().ok())
        .unwrap_or(1);
    if ahead != 0 || behind != 0 {
        bail!(
            "first-party repo {repo} is not aligned with upstream {upstream_name}: ahead {ahead}, behind {behind}; use a clean checkout for release/stage proof"
        );
    }
    Ok(())
}

fn git_output(path: &Path, args: &[&str]) -> Result<Output> {
    Command::new("git")
        .args(args)
        .current_dir(path)
        .output()
        .with_context(|| format!("could not run git {} in {}", args.join(" "), path.display()))
}

fn first_party_path_is_ci_module(root: &Path, repo: &str, path: &Path) -> bool {
    let canonical_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let root_ci = root
        .join("ci_modules")
        .join(repo)
        .canonicalize()
        .unwrap_or_else(|_| root.join("ci_modules").join(repo));
    if canonical_path == root_ci || canonical_path.starts_with(&root_ci) {
        return true;
    }
    if let Ok(module_root) = env::var("CI_MODULE_ROOT") {
        if !module_root.trim().is_empty() {
            let env_ci = PathBuf::from(&module_root)
                .join(repo)
                .canonicalize()
                .unwrap_or_else(|_| PathBuf::from(module_root).join(repo));
            return canonical_path == env_ci || canonical_path.starts_with(&env_ci);
        }
    }
    false
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_root(name: &str) -> PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "volang-first-party-{name}-{stamp}-{}",
            std::process::id()
        ))
    }

    fn run_git(path: &Path, args: &[&str]) {
        let status = Command::new("git")
            .args(args)
            .current_dir(path)
            .status()
            .unwrap_or_else(|error| panic!("could not run git {}: {error}", args.join(" ")));
        assert!(status.success(), "git {} failed", args.join(" "));
    }

    fn init_clean_repo(path: &Path) {
        fs::create_dir_all(path).expect("repo dir");
        run_git(path, &["init", "-q"]);
        fs::write(path.join("vo.mod"), "module github.com/vo-lang/test\n").expect("vo.mod");
        run_git(path, &["add", "."]);
        run_git(
            path,
            &[
                "-c",
                "user.email=test@example.com",
                "-c",
                "user.name=Test",
                "commit",
                "-q",
                "-m",
                "init",
            ],
        );
    }

    #[test]
    fn first_party_clean_root_rejects_dirty_tree_062() {
        let root = temp_root("dirty");
        let repo = root.join("siblings/vogui");
        init_clean_repo(&repo);
        fs::write(repo.join("dirty.txt"), "dirty\n").expect("dirty");

        let err = ensure_clean_first_party_root(&root, "vogui", &repo).unwrap_err();

        assert!(format!("{err:#}").contains("is dirty"));
        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn first_party_clean_root_rejects_local_hint_without_upstream_062() {
        let root = temp_root("no-upstream");
        let repo = root.join("siblings/voplay");
        init_clean_repo(&repo);

        let err = ensure_clean_first_party_root(&root, "voplay", &repo).unwrap_err();

        assert!(format!("{err:#}").contains("without an upstream"));
        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn first_party_clean_root_accepts_clean_ci_module_without_upstream_062() {
        let root = temp_root("ci-module");
        let repo = root.join("ci_modules/vogui");
        init_clean_repo(&repo);

        ensure_clean_first_party_root(&root, "vogui", &repo)
            .expect("clean ci module should not require upstream tracking");

        fs::remove_dir_all(root).ok();
    }
}
