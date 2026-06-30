use crate::config::{load_toolchains, NodeWorkspace};
use crate::first_party::first_party_workspace_path;
use anyhow::{bail, Context, Result};
use std::path::{Path, PathBuf};
use std::process::Command;

pub(crate) fn cmd_node_audit(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.len() != 1 {
        bail!("usage: vo-dev node-audit current|list");
    }
    match args.remove(0).as_str() {
        "current" => audit_current_workspaces(root),
        "list" => list_workspaces(root),
        other => bail!("unknown node-audit target: {other}"),
    }
}

fn audit_current_workspaces(root: &Path) -> Result<()> {
    let toolchains = load_toolchains(root)?;
    let current: Vec<_> = toolchains
        .node_workspace
        .iter()
        .filter(|workspace| workspace.audit.as_deref() == Some("current"))
        .collect();
    if current.is_empty() {
        bail!("eng/toolchains.toml declares no current node workspaces to audit");
    }
    for workspace in current {
        let cwd = workspace_cwd(root, workspace)?;
        let level = workspace.audit_level.as_deref().unwrap_or("high");
        println!("node audit current {} ({})", workspace.name, cwd.display());
        let status = Command::new("npm")
            .args(["audit", "--audit-level", level])
            .current_dir(&cwd)
            .status()
            .with_context(|| format!("could not run npm audit in {}", cwd.display()))?;
        if !status.success() {
            bail!("npm audit failed for node workspace {}", workspace.name);
        }
    }
    Ok(())
}

fn list_workspaces(root: &Path) -> Result<()> {
    let toolchains = load_toolchains(root)?;
    for workspace in &toolchains.node_workspace {
        println!(
            "{}\t{}\t{}",
            workspace.name,
            workspace.audit.as_deref().unwrap_or("missing"),
            workspace.audit_reason.as_deref().unwrap_or("")
        );
    }
    Ok(())
}

fn workspace_cwd(root: &Path, workspace: &NodeWorkspace) -> Result<PathBuf> {
    if let Some(repo) = &workspace.repo {
        let declared_workspace = workspace
            .name
            .strip_prefix(&format!("{repo}-"))
            .unwrap_or(&workspace.name);
        return first_party_workspace_path(root, repo, declared_workspace);
    }
    Ok(root.join(&workspace.path))
}
