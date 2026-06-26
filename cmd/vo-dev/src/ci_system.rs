use crate::config::{load_tasks, load_toolchains, NodeWorkspace, Task, ToolchainFile};
use crate::first_party::{ci_checkout_for, CiCheckout};
use crate::github_output::write_github_output;
use crate::task_graph::{
    collect_task_node_workspaces, resolve_selector, task_map, task_tools_recursive,
};
use crate::task_planner::{plan_tasks, PlanArgs};
use crate::task_runner::VM_PRODUCTION_FINAL_GATE_SELECTORS;
use crate::tool_lint::validate_rust_cache_workspace;
use crate::tool_system::desired_tool_version;
use anyhow::{anyhow, bail, Result};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::path::Path;

pub(crate) fn cmd_ci(root: &Path, mut args: Vec<String>) -> Result<()> {
    let Some(subcommand) = args.first().cloned() else {
        bail!("usage: vo-dev ci matrix|metadata <selector> [--github-output]\n       vo-dev ci final-matrix [--github-output]");
    };
    args.remove(0);
    let mut github_output = false;
    let mut filtered = Vec::new();
    for arg in args {
        if arg == "--github-output" {
            github_output = true;
        } else if arg == "--format" || arg.starts_with("--format=") {
            bail!("vo-dev ci matrix does not accept --format; it always emits a GitHub matrix");
        } else {
            filtered.push(arg);
        }
    }
    if subcommand == "final-matrix" {
        if !filtered.is_empty() {
            bail!("usage: vo-dev ci final-matrix [--github-output]");
        }
        let task_names = final_gate_task_names(root)?;
        let matrix = matrix_for(root, &task_names)?;
        if github_output {
            write_github_output(&[
                ("tasks", serde_json::to_string(&task_names)?),
                ("matrix", serde_json::to_string(&matrix)?),
                (
                    "has_tasks",
                    if task_names.is_empty() {
                        "false"
                    } else {
                        "true"
                    }
                    .to_string(),
                ),
            ])?;
        } else {
            println!("{}", serde_json::to_string_pretty(&matrix)?);
        }
        return Ok(());
    }
    let mut opts = PlanArgs::parse(filtered)?;
    if !opts.selector_explicit {
        bail!("usage: vo-dev ci {subcommand} <selector> [--github-output]");
    }
    if subcommand == "matrix" {
        opts.changed = true;
    }
    let (task_names, _) = plan_tasks(root, &opts)?;
    match subcommand.as_str() {
        "matrix" => {
            let matrix = matrix_for(root, &task_names)?;
            if github_output {
                write_github_output(&[
                    ("tasks", serde_json::to_string(&task_names)?),
                    ("matrix", serde_json::to_string(&matrix)?),
                    (
                        "has_tasks",
                        if task_names.is_empty() {
                            "false"
                        } else {
                            "true"
                        }
                        .to_string(),
                    ),
                ])?;
            } else {
                println!("{}", serde_json::to_string_pretty(&matrix)?);
            }
        }
        "metadata" => {
            let metadata = ci_metadata_for(root, &task_names)?;
            if github_output {
                write_github_output(&metadata.github_outputs()?)?;
            } else {
                println!("{}", serde_json::to_string_pretty(&metadata)?);
            }
        }
        _ => bail!(
            "usage: vo-dev ci matrix <selector> [--base <sha>] [--head <sha>] [--github-output]\n       vo-dev ci metadata <selector> [--github-output]\n       vo-dev ci final-matrix [--github-output]"
        ),
    }
    Ok(())
}

#[derive(Debug, Serialize)]
pub(crate) struct MatrixOutput {
    include: Vec<MatrixRow>,
}

#[derive(Debug, Serialize)]
struct MatrixRow {
    task: String,
    title: String,
    tier: String,
    repo: String,
    checkout: bool,
    checkout_repository: String,
    checkout_path: String,
    repos: Vec<String>,
    tools: Vec<String>,
    rust: bool,
    python: bool,
    node: bool,
    wasm_pack: bool,
    python_version: String,
    node_version: String,
    node_lockfiles: String,
    rust_cache_workspaces: String,
    wasm_pack_version: String,
}

#[derive(Debug, Serialize)]
pub(crate) struct CiMetadata {
    tasks: Vec<String>,
    tools: Vec<String>,
    rust: bool,
    python: bool,
    node: bool,
    wasm_pack: bool,
    python_version: String,
    node_version: String,
    node_lockfiles: String,
    rust_cache_workspaces: String,
    wasm_pack_version: String,
    checkout: bool,
    checkout_repository: String,
    checkout_path: String,
}

impl CiMetadata {
    pub(crate) fn github_outputs(&self) -> Result<Vec<(&str, String)>> {
        Ok(vec![
            ("tasks", serde_json::to_string(&self.tasks)?),
            ("tools", serde_json::to_string(&self.tools)?),
            ("rust", self.rust.to_string()),
            ("python", self.python.to_string()),
            ("node", self.node.to_string()),
            ("wasm_pack", self.wasm_pack.to_string()),
            ("python_version", self.python_version.clone()),
            ("node_version", self.node_version.clone()),
            ("node_lockfiles", self.node_lockfiles.clone()),
            ("rust_cache_workspaces", self.rust_cache_workspaces.clone()),
            ("wasm_pack_version", self.wasm_pack_version.clone()),
            ("checkout", self.checkout.to_string()),
            ("checkout_repository", self.checkout_repository.clone()),
            ("checkout_path", self.checkout_path.clone()),
        ])
    }
}

fn final_gate_task_names(root: &Path) -> Result<Vec<String>> {
    let config = load_tasks(root)?;
    let mut tasks = Vec::new();
    let mut seen = BTreeSet::new();
    for selector in VM_PRODUCTION_FINAL_GATE_SELECTORS {
        for task in resolve_selector(&config, selector)? {
            if seen.insert(task.clone()) {
                tasks.push(task);
            }
        }
    }
    Ok(tasks)
}

pub(crate) fn matrix_for(root: &Path, task_names: &[String]) -> Result<MatrixOutput> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    let toolchains = load_toolchains(root)?;
    let python_version = desired_tool_version(&toolchains, "python")?;
    let node_version = desired_tool_version(&toolchains, "node")?;
    let wasm_pack_version = desired_tool_version(&toolchains, "wasm-pack")?;
    let mut include = Vec::new();
    for name in task_names {
        let task = task_map
            .get(name)
            .ok_or_else(|| anyhow!("unknown task for matrix: {name}"))?;
        let tools = task_tools_recursive(root, name)?;
        let repos = task_repos_recursive(name, &task_map)?;
        let repo = repos.iter().next().cloned().unwrap_or_default();
        let checkout = matrix_checkout(root, name, &repos)?;
        include.push(MatrixRow {
            task: name.clone(),
            title: task.title.clone(),
            tier: task.tier.clone(),
            repo,
            checkout: checkout.enabled,
            checkout_repository: checkout.repository,
            checkout_path: checkout.path,
            rust: true,
            python: tools.contains("python"),
            node: tools.contains("node"),
            wasm_pack: tools.contains("wasm-pack"),
            python_version: if tools.contains("python") {
                python_version.clone()
            } else {
                String::new()
            },
            node_version: if tools.contains("node") {
                node_version.clone()
            } else {
                String::new()
            },
            node_lockfiles: if tools.contains("node") {
                node_lockfiles_for_tasks(
                    root,
                    &task_map,
                    &toolchains,
                    std::slice::from_ref(name),
                    true,
                )?
            } else {
                String::new()
            },
            rust_cache_workspaces: rust_cache_workspaces_for_workflow(root, &toolchains)?,
            wasm_pack_version: if tools.contains("wasm-pack") {
                wasm_pack_version.clone()
            } else {
                String::new()
            },
            tools: tools.into_iter().collect(),
            repos: repos.into_iter().collect(),
        });
    }
    Ok(MatrixOutput { include })
}

pub(crate) fn ci_metadata_for(root: &Path, task_names: &[String]) -> Result<CiMetadata> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    let toolchains = load_toolchains(root)?;
    let mut tools = BTreeSet::new();
    let mut repos = BTreeSet::new();
    for name in task_names {
        tools.extend(task_tools_recursive(root, name)?);
        repos.extend(task_repos_recursive(name, &task_map)?);
    }
    let checkout = metadata_checkout(root, &repos)?;
    let python = tools.contains("python");
    let node = tools.contains("node");
    let wasm_pack = tools.contains("wasm-pack");
    Ok(CiMetadata {
        tasks: task_names.to_vec(),
        tools: tools.iter().cloned().collect(),
        rust: true,
        python,
        node,
        wasm_pack,
        python_version: if python {
            desired_tool_version(&toolchains, "python")?
        } else {
            String::new()
        },
        node_version: if node {
            desired_tool_version(&toolchains, "node")?
        } else {
            String::new()
        },
        node_lockfiles: if node {
            node_lockfiles_for_tasks(root, &task_map, &toolchains, task_names, false)?
        } else {
            String::new()
        },
        rust_cache_workspaces: rust_cache_workspaces_for_workflow(root, &toolchains)?,
        wasm_pack_version: if wasm_pack {
            desired_tool_version(&toolchains, "wasm-pack")?
        } else {
            String::new()
        },
        checkout: checkout.enabled,
        checkout_repository: checkout.repository,
        checkout_path: checkout.path,
    })
}

fn node_lockfiles_for_tasks(
    root: &Path,
    task_map: &BTreeMap<String, Task>,
    toolchains: &ToolchainFile,
    task_names: &[String],
    include_legacy: bool,
) -> Result<String> {
    let mut workspace_names = BTreeSet::new();
    for task_name in task_names {
        collect_task_node_workspaces(
            task_name.strip_prefix("task:").unwrap_or(task_name),
            task_map,
            &mut workspace_names,
            &mut Vec::new(),
            &mut HashSet::new(),
        )?;
    }
    let mut lockfiles = Vec::new();
    for name in workspace_names {
        let workspace = toolchains
            .node_workspace
            .iter()
            .find(|workspace| workspace.name == name)
            .ok_or_else(|| anyhow!("unknown node workspace for CI metadata: {name}"))?;
        if !include_legacy && workspace.status.as_deref() == Some("legacy") {
            continue;
        }
        lockfiles.push(node_workspace_ci_lockfile(root, workspace)?);
    }
    Ok(lockfiles.join("\n"))
}

fn rust_cache_workspaces_for_workflow(root: &Path, toolchains: &ToolchainFile) -> Result<String> {
    if toolchains.rust_cache_workspace.is_empty() {
        bail!("eng/toolchains.toml must declare at least one rust_cache_workspace");
    }
    let mut lines = Vec::new();
    for workspace in &toolchains.rust_cache_workspace {
        validate_rust_cache_workspace(root, workspace)?;
        lines.push(format!("{} -> {}", workspace.path, workspace.target));
    }
    Ok(lines.join("\n"))
}

fn node_workspace_ci_lockfile(root: &Path, workspace: &NodeWorkspace) -> Result<String> {
    if let Some(repo) = &workspace.repo {
        let checkout = ci_checkout_for(root, repo)?;
        if !checkout.enabled {
            bail!(
                "node workspace {} references repo {} without CI checkout",
                workspace.name,
                repo
            );
        }
        Ok(format!(
            "{}/{}",
            checkout.path.trim_end_matches('/'),
            workspace.lockfile.trim_start_matches('/')
        ))
    } else {
        Ok(workspace.lockfile.clone())
    }
}

fn matrix_checkout(root: &Path, task_name: &str, repos: &BTreeSet<String>) -> Result<CiCheckout> {
    let mut checkouts = Vec::new();
    for repo in repos {
        let checkout = ci_checkout_for(root, repo)?;
        if checkout.enabled {
            checkouts.push(checkout);
        }
    }
    match checkouts.len() {
        0 => Ok(CiCheckout {
            repo: String::new(),
            repository: String::new(),
            path: String::new(),
            enabled: false,
        }),
        1 => Ok(checkouts.remove(0)),
        _ => bail!(
            "task {task_name} requires multiple CI checkouts: {}",
            checkouts
                .iter()
                .map(|item| item.repo.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn metadata_checkout(root: &Path, repos: &BTreeSet<String>) -> Result<CiCheckout> {
    let mut checkouts = Vec::new();
    for repo in repos {
        let checkout = ci_checkout_for(root, repo)?;
        if checkout.enabled {
            checkouts.push(checkout);
        }
    }
    match checkouts.len() {
        0 => Ok(CiCheckout {
            repo: String::new(),
            repository: String::new(),
            path: String::new(),
            enabled: false,
        }),
        1 => Ok(checkouts.remove(0)),
        _ => bail!(
            "CI metadata requires multiple checkouts; use ci matrix instead: {}",
            checkouts
                .iter()
                .map(|item| item.repo.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn task_repos_recursive(name: &str, task_map: &BTreeMap<String, Task>) -> Result<BTreeSet<String>> {
    let mut repos = BTreeSet::new();
    let mut stack = Vec::new();
    let mut seen = HashSet::new();
    collect_task_repos(name, task_map, &mut repos, &mut stack, &mut seen)?;
    Ok(repos)
}

fn collect_task_repos(
    name: &str,
    task_map: &BTreeMap<String, Task>,
    repos: &mut BTreeSet<String>,
    stack: &mut Vec<String>,
    seen: &mut HashSet<String>,
) -> Result<()> {
    if seen.contains(name) {
        return Ok(());
    }
    if stack.iter().any(|item| item == name) {
        stack.push(name.to_string());
        bail!(
            "task dependency cycle while collecting repos: {}",
            stack.join(" -> ")
        );
    }
    stack.push(name.to_string());
    let task = task_map
        .get(name)
        .ok_or_else(|| anyhow!("unknown task for repo collection: {name}"))?;
    for dep in &task.needs {
        collect_task_repos(dep, task_map, repos, stack, seen)?;
    }
    if let Some(repo) = &task.repo {
        repos.insert(repo.clone());
    }
    stack.pop();
    seen.insert(name.to_string());
    Ok(())
}
