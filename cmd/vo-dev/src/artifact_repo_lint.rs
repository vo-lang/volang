use crate::config::{Artifact, ArtifactFile};
use anyhow::{bail, Context, Result};
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::process::Command;

fn git_lines(root: &Path, args: &[&str]) -> Result<Vec<String>> {
    let output = Command::new("git")
        .args(args)
        .current_dir(root)
        .output()
        .with_context(|| format!("could not run git {}", args.join(" ")))?;
    if !output.status.success() {
        bail!(
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(str::to_owned)
        .collect())
}

pub(crate) fn lint_tracked_artifacts(root: &Path, artifacts: &ArtifactFile) -> Result<()> {
    let mut paths = artifact_policy_paths(root)?;
    let declared_tracked: Vec<_> = artifacts
        .artifacts
        .iter()
        .filter(|artifact| artifact.tracked == Some(true))
        .collect();
    let declared_untracked: Vec<_> = artifacts
        .artifacts
        .iter()
        .filter(|artifact| artifact.tracked == Some(false))
        .collect();
    let mut violations = Vec::new();

    for artifact in &declared_tracked {
        for path in ignored_paths_under(root, &artifact.path)? {
            paths
                .entry(path)
                .or_insert(GitPathState::UntrackedOrIgnored);
        }
    }

    for (path, state) in paths {
        let tracked = state == GitPathState::Tracked;
        if let Some(artifact) = declared_tracked
            .iter()
            .find(|artifact| path_matches_artifact(&path, artifact))
        {
            if !tracked {
                violations.push(format!(
                    "{} is untracked or ignored inside tracked artifact {}",
                    path, artifact.name
                ));
            }
            if !artifact.allowed_extensions.is_empty() {
                let ok = artifact
                    .allowed_extensions
                    .iter()
                    .any(|ext| path.ends_with(ext));
                if !ok {
                    violations.push(format!(
                        "{} is inside tracked artifact {} but extension is not allowed",
                        path, artifact.name
                    ));
                }
            }
            continue;
        }
        if declared_untracked
            .iter()
            .any(|artifact| path_matches_artifact(&path, artifact))
        {
            if tracked {
                violations.push(format!(
                    "{} is tracked inside a declared untracked artifact directory",
                    path
                ));
            }
            continue;
        }
        if suspicious_generated_path(root, &path)? {
            violations.push(format!(
                "{} looks generated but is not declared in eng/artifacts.toml",
                path
            ));
        }
    }
    lint_generated_directories(root, artifacts, &mut violations)?;

    if !violations.is_empty() {
        bail!("artifact policy violations: {}", violations.join("; "));
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GitPathState {
    Tracked,
    UntrackedOrIgnored,
}

fn artifact_policy_paths(root: &Path) -> Result<BTreeMap<String, GitPathState>> {
    let mut paths = BTreeMap::new();
    for path in git_lines(root, &["ls-files"])? {
        if root.join(&path).exists() {
            paths.insert(path, GitPathState::Tracked);
        }
    }
    for args in [["ls-files", "--others", "--exclude-standard"].as_slice()] {
        for path in git_lines(root, args)? {
            paths
                .entry(path)
                .or_insert(GitPathState::UntrackedOrIgnored);
        }
    }
    for path in git_lines(
        root,
        &[
            "ls-files",
            "--others",
            "--ignored",
            "--exclude-standard",
            "--",
            ":(glob)**/*.wasm",
            ":(glob)**/*.vpak",
        ],
    )? {
        paths
            .entry(path)
            .or_insert(GitPathState::UntrackedOrIgnored);
    }
    Ok(paths)
}

fn ignored_paths_under(root: &Path, path: &str) -> Result<Vec<String>> {
    git_lines(
        root,
        &[
            "ls-files",
            "--others",
            "--ignored",
            "--exclude-standard",
            "--",
            path,
        ],
    )
}

fn lint_generated_directories(
    root: &Path,
    artifacts: &ArtifactFile,
    violations: &mut Vec<String>,
) -> Result<()> {
    lint_generated_directories_inner(root, root, artifacts, violations)
}

fn lint_generated_directories_inner(
    root: &Path,
    dir: &Path,
    artifacts: &ArtifactFile,
    violations: &mut Vec<String>,
) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let file_type = entry.file_type()?;
        if file_type.is_symlink() || !file_type.is_dir() {
            continue;
        }
        let rel = match path.strip_prefix(root) {
            Ok(rel) if !rel.as_os_str().is_empty() => rel.to_string_lossy().replace('\\', "/"),
            _ => continue,
        };
        if rel == ".git" || rel.ends_with("/.git") {
            continue;
        }
        if artifacts
            .artifacts
            .iter()
            .any(|artifact| path_matches_artifact(&rel, artifact))
        {
            continue;
        }
        let name = path
            .file_name()
            .map(|name| name.to_string_lossy())
            .unwrap_or_default();
        if matches!(name.as_ref(), "target" | "dist" | "pkg" | "node_modules") {
            violations.push(format!(
                "{} looks generated but is not declared in eng/artifacts.toml",
                rel
            ));
            continue;
        }
        lint_generated_directories_inner(root, &path, artifacts, violations)?;
    }
    Ok(())
}

pub(crate) fn path_matches_artifact(path: &str, artifact: &Artifact) -> bool {
    path == artifact.path
        || path.starts_with(&format!("{}/", artifact.path.trim_end_matches('/')))
        || artifact.provenance.as_deref() == Some(path)
}

fn suspicious_generated_path(root: &Path, path: &str) -> Result<bool> {
    let generated_dir = path.contains("/dist/")
        || path.contains("/pkg/")
        || path.contains("/target/")
        || path.starts_with("dist/")
        || path.starts_with("target/")
        || path.ends_with(".wasm")
        || path.ends_with(".vpak")
        || path.ends_with(".class")
        || path.ends_with("/go_bench")
        || path.ends_with("/c_bench");
    if generated_dir {
        return Ok(true);
    }
    if path.ends_with(".json") {
        let full = root.join(path);
        if full.exists() && fs::metadata(full)?.len() > 5_000_000 {
            return Ok(true);
        }
    }
    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_root(name: &str) -> std::path::PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "volang-vo-dev-artifact-lint-{name}-{stamp}-{}",
            std::process::id()
        ))
    }

    fn run_git(root: &Path, args: &[&str]) {
        let status = Command::new("git")
            .args(args)
            .current_dir(root)
            .status()
            .unwrap_or_else(|error| panic!("could not run git {}: {error}", args.join(" ")));
        assert!(status.success(), "git {} failed", args.join(" "));
    }

    fn init_project(root: &Path, project_toml: &str) {
        fs::create_dir_all(root.join("eng")).expect("eng dir");
        fs::write(root.join("eng/project.toml"), project_toml).expect("project toml");
        run_git(root, &["init", "-q"]);
        run_git(root, &["add", "eng/project.toml"]);
    }

    #[test]
    fn artifact_lint_rejects_undeclared_generated_dirs_062() {
        let root = temp_root("undeclared");
        fs::create_dir_all(&root).expect("root dir");
        init_project(
            &root,
            r#"version = 1

[repo]
name = "volang"
module = "github.com/vo-lang/volang"
"#,
        );
        fs::create_dir_all(root.join("workspace/vogui/js/dist")).expect("dist dir");

        let artifacts = ArtifactFile {
            version: 1,
            artifacts: Vec::new(),
        };

        let err = lint_tracked_artifacts(&root, &artifacts).unwrap_err();
        assert!(format!("{err:#}").contains("workspace/vogui/js/dist looks generated"));
        fs::remove_dir_all(root).ok();
    }
}
