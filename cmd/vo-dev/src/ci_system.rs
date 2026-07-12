use crate::config::{
    load_ci, load_tasks, load_toolchains, NodeWorkspace, Task, TaskFile, ToolchainFile,
};
use crate::first_party::{ci_checkout_for, CiCheckout};
use crate::github_output::write_github_output;
use crate::task_graph::{
    collect_task_node_workspaces, resolve_selector, task_map, task_repos_recursive_from_map,
    task_tools_recursive,
};
use crate::task_planner::{plan_tasks, PlanArgs};
use crate::task_runner::final_gate_selectors;
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

#[derive(Debug)]
struct ExecutionUnit {
    selector: String,
    title: String,
    tier: String,
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
    checkout_expected_commit: String,
    checkouts: Vec<CiCheckout>,
    checkout_vogui: bool,
    checkout_vogui_repository: String,
    checkout_vogui_path: String,
    checkout_vogui_expected_commit: String,
    checkout_voplay: bool,
    checkout_voplay_repository: String,
    checkout_voplay_path: String,
    checkout_voplay_expected_commit: String,
    checkout_vopack: bool,
    checkout_vopack_repository: String,
    checkout_vopack_path: String,
    checkout_vopack_expected_commit: String,
    checkout_vostore: bool,
    checkout_vostore_repository: String,
    checkout_vostore_path: String,
    checkout_vostore_expected_commit: String,
    checkout_blockkart: bool,
    checkout_blockkart_repository: String,
    checkout_blockkart_path: String,
    checkout_blockkart_expected_commit: String,
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
    linux_packages: String,
}

#[derive(Debug, Serialize)]
pub(crate) struct CiMetadata {
    tasks: Vec<String>,
    repos: Vec<String>,
    checkouts: Vec<CiCheckout>,
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
    linux_packages: String,
    checkout: bool,
    checkout_repository: String,
    checkout_path: String,
    checkout_expected_commit: String,
    checkout_vogui: bool,
    checkout_vogui_repository: String,
    checkout_vogui_path: String,
    checkout_vogui_expected_commit: String,
    checkout_voplay: bool,
    checkout_voplay_repository: String,
    checkout_voplay_path: String,
    checkout_voplay_expected_commit: String,
    checkout_vopack: bool,
    checkout_vopack_repository: String,
    checkout_vopack_path: String,
    checkout_vopack_expected_commit: String,
    checkout_vostore: bool,
    checkout_vostore_repository: String,
    checkout_vostore_path: String,
    checkout_vostore_expected_commit: String,
    checkout_blockkart: bool,
    checkout_blockkart_repository: String,
    checkout_blockkart_path: String,
    checkout_blockkart_expected_commit: String,
}

impl CiMetadata {
    pub(crate) fn github_outputs(&self) -> Result<Vec<(&str, String)>> {
        Ok(vec![
            ("tasks", serde_json::to_string(&self.tasks)?),
            ("repos", serde_json::to_string(&self.repos)?),
            ("checkouts", serde_json::to_string(&self.checkouts)?),
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
            ("linux_packages", self.linux_packages.clone()),
            ("checkout", self.checkout.to_string()),
            ("checkout_repository", self.checkout_repository.clone()),
            ("checkout_path", self.checkout_path.clone()),
            (
                "checkout_expected_commit",
                self.checkout_expected_commit.clone(),
            ),
            ("checkout_vogui", self.checkout_vogui.to_string()),
            (
                "checkout_vogui_repository",
                self.checkout_vogui_repository.clone(),
            ),
            ("checkout_vogui_path", self.checkout_vogui_path.clone()),
            (
                "checkout_vogui_expected_commit",
                self.checkout_vogui_expected_commit.clone(),
            ),
            ("checkout_voplay", self.checkout_voplay.to_string()),
            (
                "checkout_voplay_repository",
                self.checkout_voplay_repository.clone(),
            ),
            ("checkout_voplay_path", self.checkout_voplay_path.clone()),
            (
                "checkout_voplay_expected_commit",
                self.checkout_voplay_expected_commit.clone(),
            ),
            ("checkout_vopack", self.checkout_vopack.to_string()),
            (
                "checkout_vopack_repository",
                self.checkout_vopack_repository.clone(),
            ),
            ("checkout_vopack_path", self.checkout_vopack_path.clone()),
            (
                "checkout_vopack_expected_commit",
                self.checkout_vopack_expected_commit.clone(),
            ),
            ("checkout_vostore", self.checkout_vostore.to_string()),
            (
                "checkout_vostore_repository",
                self.checkout_vostore_repository.clone(),
            ),
            ("checkout_vostore_path", self.checkout_vostore_path.clone()),
            (
                "checkout_vostore_expected_commit",
                self.checkout_vostore_expected_commit.clone(),
            ),
            ("checkout_blockkart", self.checkout_blockkart.to_string()),
            (
                "checkout_blockkart_repository",
                self.checkout_blockkart_repository.clone(),
            ),
            (
                "checkout_blockkart_path",
                self.checkout_blockkart_path.clone(),
            ),
            (
                "checkout_blockkart_expected_commit",
                self.checkout_blockkart_expected_commit.clone(),
            ),
        ])
    }
}

fn final_gate_task_names(root: &Path) -> Result<Vec<String>> {
    let config = load_tasks(root)?;
    let mut tasks = Vec::new();
    let mut seen = BTreeSet::new();
    for selector in final_gate_selectors(&config)? {
        for task in resolve_selector(&config, &selector)? {
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
    let ci = load_ci(root)?;
    let toolchains = load_toolchains(root)?;
    let python_version = desired_tool_version(&toolchains, "python")?;
    let node_version = desired_tool_version(&toolchains, "node")?;
    let wasm_pack_version = desired_tool_version(&toolchains, "wasm-pack")?;
    let mut include = Vec::new();
    for unit in execution_units(&config, &ci.lanes, task_names)? {
        let selector_tasks = resolve_selector(&config, &unit.selector)?;
        let tools = crate::task_graph::selector_tools_recursive(root, &unit.selector)?;
        let mut repos = BTreeSet::new();
        for task_name in &selector_tasks {
            repos.extend(task_repos_recursive_from_map(&task_map, task_name)?);
        }
        let repo = repos.iter().next().cloned().unwrap_or_default();
        let checkouts = ci_checkouts(root, &repos)?;
        let checkout = legacy_checkout(&checkouts);
        let named_checkouts = NamedCheckoutFields::from_checkouts(&checkouts);
        include.push(MatrixRow {
            task: unit.selector,
            title: unit.title,
            tier: unit.tier,
            repo,
            checkout: checkout.enabled,
            checkout_repository: checkout.repository,
            checkout_path: checkout.path,
            checkout_expected_commit: checkout.expected_commit,
            checkouts,
            checkout_vogui: named_checkouts.vogui.enabled,
            checkout_vogui_repository: named_checkouts.vogui.repository,
            checkout_vogui_path: named_checkouts.vogui.path,
            checkout_vogui_expected_commit: named_checkouts.vogui.expected_commit,
            checkout_voplay: named_checkouts.voplay.enabled,
            checkout_voplay_repository: named_checkouts.voplay.repository,
            checkout_voplay_path: named_checkouts.voplay.path,
            checkout_voplay_expected_commit: named_checkouts.voplay.expected_commit,
            checkout_vopack: named_checkouts.vopack.enabled,
            checkout_vopack_repository: named_checkouts.vopack.repository,
            checkout_vopack_path: named_checkouts.vopack.path,
            checkout_vopack_expected_commit: named_checkouts.vopack.expected_commit,
            checkout_vostore: named_checkouts.vostore.enabled,
            checkout_vostore_repository: named_checkouts.vostore.repository,
            checkout_vostore_path: named_checkouts.vostore.path,
            checkout_vostore_expected_commit: named_checkouts.vostore.expected_commit,
            checkout_blockkart: named_checkouts.blockkart.enabled,
            checkout_blockkart_repository: named_checkouts.blockkart.repository,
            checkout_blockkart_path: named_checkouts.blockkart.path,
            checkout_blockkart_expected_commit: named_checkouts.blockkart.expected_commit,
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
                node_lockfiles_for_tasks(root, &task_map, &toolchains, &selector_tasks, true)?
            } else {
                String::new()
            },
            rust_cache_workspaces: rust_cache_workspaces_for_workflow(root, &toolchains)?,
            wasm_pack_version: if tools.contains("wasm-pack") {
                wasm_pack_version.clone()
            } else {
                String::new()
            },
            linux_packages: selector_tasks
                .iter()
                .filter_map(|name| task_map.get(name))
                .flat_map(|task| task.linux_packages.iter().cloned())
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
                .join(" "),
            tools: tools.into_iter().collect(),
            repos: repos.into_iter().collect(),
        });
    }
    Ok(MatrixOutput { include })
}

fn execution_units(
    config: &TaskFile,
    lanes: &[crate::config::CiLane],
    task_names: &[String],
) -> Result<Vec<ExecutionUnit>> {
    let task_map = task_map(config)?;
    for name in task_names {
        if !task_map.contains_key(name) {
            bail!("unknown task for matrix: {name}");
        }
    }
    let planned = task_names.iter().cloned().collect::<BTreeSet<_>>();
    let mut covered_by = BTreeMap::<String, String>::new();
    let mut units = Vec::new();
    for lane in lanes {
        let lane_tasks = resolve_selector(config, &lane.selector)?;
        let selected = lane_tasks
            .iter()
            .filter(|task| planned.contains(*task))
            .cloned()
            .collect::<Vec<_>>();
        if selected.is_empty() {
            continue;
        }
        for task in selected {
            if let Some(previous) = covered_by.insert(task.clone(), lane.selector.clone()) {
                bail!(
                    "CI task {task} is covered by overlapping lanes {previous} and {}",
                    lane.selector
                );
            }
        }
        units.push(ExecutionUnit {
            selector: lane.selector.clone(),
            title: lane.title.clone(),
            tier: lane.tier.clone(),
        });
    }
    for name in task_names {
        if covered_by.contains_key(name) {
            continue;
        }
        let task = task_map
            .get(name)
            .ok_or_else(|| anyhow!("unknown task for matrix: {name}"))?;
        units.push(ExecutionUnit {
            selector: name.clone(),
            title: task.title.clone(),
            tier: task.tier.clone(),
        });
    }
    Ok(units)
}

pub(crate) fn ci_metadata_for(root: &Path, task_names: &[String]) -> Result<CiMetadata> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    let toolchains = load_toolchains(root)?;
    let mut tools = BTreeSet::new();
    let mut repos = BTreeSet::new();
    for name in task_names {
        tools.extend(task_tools_recursive(root, name)?);
        repos.extend(task_repos_recursive_from_map(&task_map, name)?);
    }
    let checkouts = ci_checkouts(root, &repos)?;
    let checkout = legacy_checkout(&checkouts);
    let named_checkouts = NamedCheckoutFields::from_checkouts(&checkouts);
    let python = tools.contains("python");
    let node = tools.contains("node");
    let wasm_pack = tools.contains("wasm-pack");
    Ok(CiMetadata {
        tasks: task_names.to_vec(),
        repos: repos.iter().cloned().collect(),
        checkouts,
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
        linux_packages: task_names
            .iter()
            .filter_map(|name| task_map.get(name.strip_prefix("task:").unwrap_or(name)))
            .flat_map(|task| task.linux_packages.iter().cloned())
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
            .join(" "),
        checkout: checkout.enabled,
        checkout_repository: checkout.repository,
        checkout_path: checkout.path,
        checkout_expected_commit: checkout.expected_commit,
        checkout_vogui: named_checkouts.vogui.enabled,
        checkout_vogui_repository: named_checkouts.vogui.repository,
        checkout_vogui_path: named_checkouts.vogui.path,
        checkout_vogui_expected_commit: named_checkouts.vogui.expected_commit,
        checkout_voplay: named_checkouts.voplay.enabled,
        checkout_voplay_repository: named_checkouts.voplay.repository,
        checkout_voplay_path: named_checkouts.voplay.path,
        checkout_voplay_expected_commit: named_checkouts.voplay.expected_commit,
        checkout_vopack: named_checkouts.vopack.enabled,
        checkout_vopack_repository: named_checkouts.vopack.repository,
        checkout_vopack_path: named_checkouts.vopack.path,
        checkout_vopack_expected_commit: named_checkouts.vopack.expected_commit,
        checkout_vostore: named_checkouts.vostore.enabled,
        checkout_vostore_repository: named_checkouts.vostore.repository,
        checkout_vostore_path: named_checkouts.vostore.path,
        checkout_vostore_expected_commit: named_checkouts.vostore.expected_commit,
        checkout_blockkart: named_checkouts.blockkart.enabled,
        checkout_blockkart_repository: named_checkouts.blockkart.repository,
        checkout_blockkart_path: named_checkouts.blockkart.path,
        checkout_blockkart_expected_commit: named_checkouts.blockkart.expected_commit,
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

fn ci_checkouts(root: &Path, repos: &BTreeSet<String>) -> Result<Vec<CiCheckout>> {
    let mut checkouts = Vec::new();
    for repo in repos {
        let checkout = ci_checkout_for(root, repo)?;
        if checkout.enabled {
            checkouts.push(checkout);
        }
    }
    Ok(checkouts)
}

fn legacy_checkout(checkouts: &[CiCheckout]) -> CiCheckout {
    checkouts.first().cloned().unwrap_or_else(|| CiCheckout {
        repo: String::new(),
        repository: String::new(),
        path: String::new(),
        expected_commit: String::new(),
        enabled: false,
    })
}

#[derive(Default)]
struct NamedCheckoutFields {
    vogui: CiCheckout,
    voplay: CiCheckout,
    vopack: CiCheckout,
    vostore: CiCheckout,
    blockkart: CiCheckout,
}

impl NamedCheckoutFields {
    fn from_checkouts(checkouts: &[CiCheckout]) -> Self {
        let mut fields = Self::default();
        for checkout in checkouts {
            match checkout.repo.as_str() {
                "vogui" => fields.vogui = checkout.clone(),
                "voplay" => fields.voplay = checkout.clone(),
                "vopack" => fields.vopack = checkout.clone(),
                "vostore" => fields.vostore = checkout.clone(),
                "BlockKart" => fields.blockkart = checkout.clone(),
                _ => {}
            }
        }
        fields
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::CiLane;

    fn task(name: &str) -> Task {
        Task {
            name: name.to_string(),
            title: name.to_string(),
            command: vec!["true".to_string()],
            tools: Vec::new(),
            node_workspaces: Vec::new(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            tier: "contract".to_string(),
            tags: Vec::new(),
            owner: Some("eng".to_string()),
            cwd: None,
            env: BTreeMap::new(),
            needs: Vec::new(),
            repo: None,
            repos: Vec::new(),
            internal: false,
            timeout_sec: Some(60),
            platforms: Vec::new(),
            linux_packages: Vec::new(),
            shell: false,
        }
    }

    fn config(groups: &[(&str, &[&str])], task_names: &[&str]) -> TaskFile {
        TaskFile {
            version: 1,
            final_selectors: Vec::new(),
            groups: groups
                .iter()
                .map(|(name, tasks)| {
                    (
                        (*name).to_string(),
                        tasks.iter().map(|task| (*task).to_string()).collect(),
                    )
                })
                .collect(),
            group_meta: Vec::new(),
            tasks: task_names.iter().map(|name| task(name)).collect(),
        }
    }

    fn lane(selector: &str) -> CiLane {
        CiLane {
            selector: selector.to_string(),
            title: selector.to_string(),
            tier: "contract".to_string(),
        }
    }

    #[test]
    fn execution_units_compact_selected_tasks_into_their_lane() {
        let config = config(&[("lane-a", &["compile", "test"])], &["compile", "test"]);
        let units = execution_units(&config, &[lane("lane-a")], &["compile".to_string()])
            .expect("execution lane");

        assert_eq!(units.len(), 1);
        assert_eq!(units[0].selector, "lane-a");
    }

    #[test]
    fn execution_units_keep_uncovered_final_gate_tasks_standalone() {
        let config = config(&[("lane-a", &["compile"])], &["compile", "signoff"]);
        let units = execution_units(&config, &[lane("lane-a")], &["signoff".to_string()])
            .expect("standalone final gate");

        assert_eq!(units.len(), 1);
        assert_eq!(units[0].selector, "signoff");
    }

    #[test]
    fn execution_units_reject_overlapping_lanes() {
        let config = config(&[("lane-a", &["test"]), ("lane-b", &["test"])], &["test"]);
        let err = execution_units(
            &config,
            &[lane("lane-a"), lane("lane-b")],
            &["test".to_string()],
        )
        .unwrap_err();

        assert!(format!("{err:#}").contains("covered by overlapping lanes"));
    }
}
