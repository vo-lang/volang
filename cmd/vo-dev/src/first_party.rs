use crate::config::{load_project, ProjectFile, ProjectRepo};
use anyhow::{anyhow, bail, Context, Result};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

pub(crate) fn cmd_first_party(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev first-party path|run|run-workspace|release-verify ...");
    }
    match args.remove(0).as_str() {
        "path" => {
            if args.is_empty() || args.len() > 2 {
                bail!("usage: vo-dev first-party path <repo> [subdir]");
            }
            let mut path = first_party_repo_path(root, &args[0])?;
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
            if args.first().is_some_and(|arg| arg == "--") {
                args.remove(0);
            }
            run_in(
                &first_party_repo_path(root, &repo)?.join(subdir),
                &args,
                &format!("first-party repo {repo}"),
            )
        }
        "run-workspace" => {
            if args.len() < 4 {
                bail!("usage: vo-dev first-party run-workspace <repo> <workspace> -- <command...>");
            }
            let repo = args.remove(0);
            let workspace = args.remove(0);
            if args.first().is_some_and(|arg| arg == "--") {
                args.remove(0);
            }
            run_in(
                &first_party_workspace_path(root, &repo, &workspace)?,
                &args,
                &format!("first-party workspace {repo}:{workspace}"),
            )
        }
        "release-verify" => {
            if args.len() != 1 {
                bail!("usage: vo-dev first-party release-verify <repo>");
            }
            let repo = &args[0];
            let module = checked_first_party_repo_path(root, repo)?;
            if !module.join("vo.mod").is_file() {
                bail!("first-party repo {repo} does not contain vo.mod");
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
            checked_first_party_repo_path(root, repo).with_context(|| {
                format!("first-party repo {repo} changed during release verification")
            })?;
            Ok(())
        }
        other => bail!("unknown first-party command: {other}"),
    }
}

fn run_in(cwd: &Path, args: &[String], label: &str) -> Result<()> {
    if args.is_empty() {
        bail!("missing command after --");
    }
    if !cwd.is_dir() {
        bail!("missing directory for {label}: {}", cwd.display());
    }
    let status = Command::new(&args[0])
        .args(&args[1..])
        .current_dir(cwd)
        .status()
        .with_context(|| format!("could not run command in {label}"))?;
    if !status.success() {
        bail!("command failed in {label}");
    }
    Ok(())
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
        .find(|entry| entry.name == repo)
        .ok_or_else(|| anyhow!("unknown first-party repo: {repo}"))?;
    repo_path_from_entry(root, repo, entry)
}

fn checked_first_party_repo_path(root: &Path, repo: &str) -> Result<PathBuf> {
    let project = load_project(root)?;
    let entry = project
        .first_party
        .iter()
        .find(|entry| entry.name == repo)
        .ok_or_else(|| anyhow!("unknown first-party repo: {repo}"))?;
    checked_repo_path_from_entry(root, repo, entry)
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
        .find(|entry| entry.name == repo)
        .ok_or_else(|| anyhow!("unknown first-party repo: {repo}"))?;
    let workspace_path = entry
        .workspace
        .iter()
        .find(|entry| entry.name == workspace)
        .ok_or_else(|| anyhow!("unknown first-party workspace: {repo}:{workspace}"))?
        .path
        .as_str();
    let path = repo_path_from_entry(root, repo, entry)?.join(workspace_path);
    if !path.is_dir() {
        bail!(
            "missing first-party workspace directory {repo}:{workspace}: {}",
            path.display()
        );
    }
    Ok(path)
}

pub(crate) fn project_repo_entry<'a>(
    project: &'a ProjectFile,
    repo: &str,
) -> Option<&'a ProjectRepo> {
    project
        .first_party
        .iter()
        .chain(project.external_project.iter())
        .find(|entry| entry.name == repo)
}

fn repo_path_from_entry(root: &Path, repo: &str, entry: &ProjectRepo) -> Result<PathBuf> {
    let local_hint = entry
        .local_hint
        .as_deref()
        .ok_or_else(|| anyhow!("project repo {repo} has no local_hint"))?;
    let path = root.join(local_hint);
    if !path.is_dir() {
        bail!("project repo {repo} is missing: {}", path.display());
    }
    path.canonicalize().with_context(|| {
        format!(
            "could not canonicalize project repo {repo}: {}",
            path.display()
        )
    })
}

fn checked_repo_path_from_entry(root: &Path, repo: &str, entry: &ProjectRepo) -> Result<PathBuf> {
    let path = repo_path_from_entry(root, repo, entry)?;
    let expected_commit = entry
        .expected_commit
        .as_deref()
        .ok_or_else(|| anyhow!("project repo {repo} has no expected_commit"))?;
    validate_full_oid(repo, expected_commit)?;
    ensure_clean_pinned_repo(repo, &path, expected_commit)?;
    Ok(path)
}

fn ensure_clean_pinned_repo(repo: &str, path: &Path, expected_commit: &str) -> Result<()> {
    let inside = git_output(path, &["rev-parse", "--is-inside-work-tree"])?;
    if !inside.status.success() || String::from_utf8_lossy(&inside.stdout).trim() != "true" {
        bail!(
            "project repo {repo} is not a git worktree: {}",
            path.display()
        );
    }
    let status = git_output(path, &["status", "--porcelain=v1", "--untracked-files=all"])?;
    if !status.status.success() {
        bail!("could not inspect project repo {repo}: {}", path.display());
    }
    let dirty = String::from_utf8_lossy(&status.stdout);
    if !dirty.trim().is_empty() {
        bail!(
            "project repo {repo} is dirty: {}\n{}",
            path.display(),
            dirty.trim()
        );
    }
    let head = git_output(path, &["rev-parse", "HEAD"])?;
    if !head.status.success() {
        bail!(
            "could not read HEAD for project repo {repo}: {}",
            path.display()
        );
    }
    let actual = String::from_utf8_lossy(&head.stdout);
    let actual = actual.trim();
    if actual != expected_commit {
        bail!(
            "project repo {repo} HEAD {actual} does not match expected_commit {expected_commit}: {}",
            path.display()
        );
    }
    Ok(())
}

fn git_output(path: &Path, args: &[&str]) -> Result<Output> {
    Command::new("git")
        .args(args)
        .env("GIT_TERMINAL_PROMPT", "0")
        .env("GCM_INTERACTIVE", "Never")
        .current_dir(path)
        .output()
        .with_context(|| format!("could not run git {} in {}", args.join(" "), path.display()))
}

fn validate_full_oid(repo: &str, oid: &str) -> Result<()> {
    let valid_length = oid.len() == 40 || oid.len() == 64;
    let valid_digits = oid
        .bytes()
        .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte));
    if !valid_length || !valid_digits {
        bail!(
            "project repo {repo} expected_commit must be a full lowercase Git object ID; found {oid:?}"
        );
    }
    Ok(())
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

    fn init_repo(path: &Path) -> String {
        fs::create_dir_all(path).expect("repo dir");
        run_git(path, &["init", "-q"]);
        fs::write(path.join("source.txt"), "source\n").expect("source");
        run_git(path, &["add", "source.txt"]);
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
        String::from_utf8_lossy(
            &git_output(path, &["rev-parse", "HEAD"])
                .expect("git head")
                .stdout,
        )
        .trim()
        .to_string()
    }

    #[test]
    fn local_hint_resolves_project_repo() {
        let root = temp_root("local-hint");
        let repo = root.join("siblings/voplay");
        fs::create_dir_all(&repo).expect("repo");
        let entry = ProjectRepo {
            name: "voplay".to_string(),
            repository: Some("vo-lang/voplay".to_string()),
            local_hint: Some("siblings/voplay".to_string()),
            expected_commit: None,
            workspace: Vec::new(),
        };

        assert_eq!(
            repo_path_from_entry(&root, "voplay", &entry).expect("resolved repo"),
            repo.canonicalize().expect("canonical repo")
        );
        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn checked_repo_requires_the_clean_expected_commit() {
        let root = temp_root("checked");
        let repo = root.join("siblings/vogui");
        let expected = init_repo(&repo);
        let entry = ProjectRepo {
            name: "vogui".to_string(),
            repository: Some("vo-lang/vogui".to_string()),
            local_hint: Some("siblings/vogui".to_string()),
            expected_commit: Some(expected),
            workspace: Vec::new(),
        };

        checked_repo_path_from_entry(&root, "vogui", &entry).expect("clean pin");
        fs::write(repo.join("dirty.txt"), "dirty\n").expect("dirty");
        assert!(checked_repo_path_from_entry(&root, "vogui", &entry).is_err());
        fs::remove_dir_all(root).ok();
    }
}
