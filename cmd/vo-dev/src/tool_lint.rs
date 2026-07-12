use crate::config::{load_project, RustCacheWorkspace, ToolchainFile};
use crate::first_party::project_repo_entry;
use crate::lint_policy::{
    artifact_path_contains, contains_glob_meta, validate_ascii_slug, validate_repo_path_like,
};
use crate::tool_system::desired_tool_version;
use anyhow::{anyhow, bail, Result};
use std::collections::HashSet;
use std::path::Path;

pub(crate) fn lint_toolchain_file(root: &Path, config: &ToolchainFile) -> Result<()> {
    if config.version != 1 {
        bail!("eng/toolchains.toml version must be 1");
    }
    let project = load_project(root)?;
    for (name, tool) in &config.tools {
        validate_ascii_slug("tool name", name, &['-'])?;
        let has_policy = tool.version.is_some()
            || tool.version_from.is_some()
            || tool.source.is_some()
            || tool.minimum.is_some()
            || tool.usage.is_some()
            || tool.check.is_some()
            || tool.bootstrap.is_some();
        if !has_policy {
            bail!("tool {name} has no version, source, minimum, usage, or check policy");
        }
        if let Some(check) = &tool.check {
            if check.is_empty() {
                bail!("tool {name} has an empty check command");
            }
            if check.iter().any(|arg| arg.trim().is_empty()) {
                bail!("tool {name} check command contains an empty argument");
            }
        }
        if let Some(bootstrap) = &tool.bootstrap {
            if bootstrap.is_empty() {
                bail!("tool {name} has an empty bootstrap command");
            }
            if bootstrap.iter().any(|arg| arg.trim().is_empty()) {
                bail!("tool {name} bootstrap command contains an empty argument");
            }
        }
        if let Some(source) = &tool.version_from {
            if source == name {
                bail!("tool {name} version_from cannot reference itself");
            }
            if !config.tools.contains_key(source) {
                bail!("tool {name} version_from references unknown tool {source}");
            }
            desired_tool_version(config, name)?;
        }
        if let Some(source) = &tool.source {
            validate_repo_path_like("tool", name, "source", source, false)?;
            if !root.join(source).exists() {
                bail!("tool {name} source does not exist: {source}");
            }
        }
        if tool.required == Some(true) && tool.check.is_none() && tool.source.is_none() {
            bail!("required tool {name} must declare check or source");
        }
    }
    if config.rust_cache_workspace.is_empty() {
        bail!("eng/toolchains.toml rust_cache_workspace cannot be empty");
    }
    let mut rust_cache_workspaces = HashSet::new();
    for workspace in &config.rust_cache_workspace {
        validate_rust_cache_workspace(root, workspace)?;
        if !rust_cache_workspaces.insert((
            workspace.repo.clone(),
            workspace.path.clone(),
            workspace.target.clone(),
        )) {
            bail!(
                "duplicate rust cache workspace: {}{} -> {}",
                workspace
                    .repo
                    .as_deref()
                    .map(|repo| format!("{repo}:"))
                    .unwrap_or_default(),
                workspace.path,
                workspace.target
            );
        }
    }
    let mut workspaces = HashSet::new();
    for workspace in &config.node_workspace {
        if !workspaces.insert(workspace.name.clone()) {
            bail!("duplicate node workspace: {}", workspace.name);
        }
        if workspace.name.trim().is_empty() {
            bail!("node workspace name cannot be empty");
        }
        if workspace.name.trim() != workspace.name {
            bail!(
                "node workspace {} name cannot contain surrounding whitespace",
                workspace.name
            );
        }
        if !workspace
            .name
            .chars()
            .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || matches!(ch, '-' | '_'))
        {
            bail!(
                "node workspace {} name must use lowercase ASCII letters, digits, hyphen, or underscore",
                workspace.name
            );
        }
        if workspace.path.trim().is_empty() {
            bail!("node workspace {} path cannot be empty", workspace.name);
        }
        validate_repo_path_like(
            "node workspace",
            &workspace.name,
            "path",
            &workspace.path,
            false,
        )?;
        if workspace.lockfile.trim().is_empty() {
            bail!("node workspace {} lockfile cannot be empty", workspace.name);
        }
        validate_repo_path_like(
            "node workspace",
            &workspace.name,
            "lockfile",
            &workspace.lockfile,
            false,
        )?;
        if !artifact_path_contains(&workspace.path, &workspace.lockfile) {
            bail!(
                "node workspace {} lockfile {} must be inside {}",
                workspace.name,
                workspace.lockfile,
                workspace.path
            );
        }
        if workspace.repo.is_none() {
            let workspace_path = root.join(&workspace.path);
            if !workspace_path.is_dir() {
                bail!(
                    "node workspace {} path is not a directory: {}",
                    workspace.name,
                    workspace.path
                );
            }
            if !workspace_path.join("package.json").is_file() {
                bail!(
                    "node workspace {} is missing package.json under {}",
                    workspace.name,
                    workspace.path
                );
            }
            if !root.join(&workspace.lockfile).is_file() {
                bail!(
                    "node workspace {} lockfile is missing: {}",
                    workspace.name,
                    workspace.lockfile
                );
            }
        }
        if let Some(status) = &workspace.status {
            if status != "legacy" {
                bail!(
                    "node workspace {} has invalid status {}",
                    workspace.name,
                    status
                );
            }
        }
        match workspace.audit.as_deref() {
            Some("current") => {
                if workspace.status.as_deref() == Some("legacy") {
                    bail!(
                        "node workspace {} cannot audit current while status is legacy",
                        workspace.name
                    );
                }
                if workspace
                    .audit_level
                    .as_deref()
                    .unwrap_or_default()
                    .is_empty()
                {
                    bail!(
                        "node workspace {} audit=current must declare audit_level",
                        workspace.name
                    );
                }
                if !matches!(
                    workspace.audit_level.as_deref(),
                    Some("low" | "moderate" | "high" | "critical")
                ) {
                    bail!(
                        "node workspace {} has invalid audit_level {}",
                        workspace.name,
                        workspace.audit_level.as_deref().unwrap_or("missing")
                    );
                }
            }
            Some("legacy-exempt") => {
                if workspace.status.as_deref() != Some("legacy") {
                    bail!(
                        "node workspace {} audit=legacy-exempt requires status=legacy",
                        workspace.name
                    );
                }
                if workspace
                    .audit_reason
                    .as_deref()
                    .unwrap_or_default()
                    .trim()
                    .is_empty()
                {
                    bail!(
                        "node workspace {} audit=legacy-exempt must declare audit_reason",
                        workspace.name
                    );
                }
            }
            Some("first-party-release") => {
                if workspace.repo.is_none() {
                    bail!(
                        "node workspace {} audit=first-party-release requires repo",
                        workspace.name
                    );
                }
                if workspace
                    .audit_reason
                    .as_deref()
                    .unwrap_or_default()
                    .trim()
                    .is_empty()
                {
                    bail!(
                        "node workspace {} audit=first-party-release must declare audit_reason",
                        workspace.name
                    );
                }
            }
            Some(other) => bail!(
                "node workspace {} has invalid audit policy {other}",
                workspace.name
            ),
            None => bail!(
                "node workspace {} must declare audit policy",
                workspace.name
            ),
        }
        if let Some(repo) = &workspace.repo {
            let entry = project_repo_entry(&project, repo).ok_or_else(|| {
                anyhow!(
                    "node workspace {} references unknown repo {}",
                    workspace.name,
                    repo
                )
            })?;
            if !entry
                .workspace
                .iter()
                .any(|item| item.kind == "node" && item.path == workspace.path)
            {
                bail!(
                    "node workspace {} references {}:{}, but eng/project.toml does not declare a matching node workspace",
                    workspace.name,
                    repo,
                    workspace.path
                );
            }
            if entry.ci_checkout != Some(true) {
                bail!(
                    "node workspace {} references repo {} without ci_checkout=true",
                    workspace.name,
                    repo
                );
            }
            if let Some(local_hint) = &entry.local_hint {
                let local_root = root.join(local_hint);
                if local_root.exists() {
                    let workspace_path = local_root.join(&workspace.path);
                    if !workspace_path.is_dir() {
                        bail!(
                            "node workspace {} path is not a directory under {}: {}",
                            workspace.name,
                            repo,
                            workspace.path
                        );
                    }
                    if !workspace_path.join("package.json").is_file() {
                        bail!(
                            "node workspace {} is missing package.json under {}:{}",
                            workspace.name,
                            repo,
                            workspace.path
                        );
                    }
                    if !local_root.join(&workspace.lockfile).is_file() {
                        bail!(
                            "node workspace {} lockfile is missing under {}: {}",
                            workspace.name,
                            repo,
                            workspace.lockfile
                        );
                    }
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn validate_rust_cache_workspace(
    root: &Path,
    workspace: &RustCacheWorkspace,
) -> Result<()> {
    if workspace.path != "." {
        validate_repo_path_like(
            "rust cache workspace",
            &workspace.path,
            "path",
            &workspace.path,
            false,
        )?;
    }
    if let Some(repo) = &workspace.repo {
        let project = load_project(root)?;
        if project_repo_entry(&project, repo).is_none() {
            bail!("rust cache workspace references undeclared repo {repo}");
        }
    } else {
        let workspace_path = root.join(&workspace.path);
        if !workspace_path.is_dir() {
            bail!(
                "rust cache workspace path is not a directory: {}",
                workspace.path
            );
        }
    }
    validate_repo_path_like(
        "rust cache workspace",
        &workspace.path,
        "target",
        &workspace.target,
        false,
    )?;
    if contains_glob_meta(&workspace.target) {
        bail!(
            "rust cache workspace {} target {} must be concrete",
            workspace.path,
            workspace.target
        );
    }
    Ok(())
}
