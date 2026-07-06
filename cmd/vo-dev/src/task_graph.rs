use crate::config::{load_tasks, Task, TaskFile};
use anyhow::{anyhow, bail, Context, Result};
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::path::Path;

pub(crate) fn task_map(config: &TaskFile) -> Result<BTreeMap<String, Task>> {
    let mut map = BTreeMap::new();
    for task in &config.tasks {
        if map.insert(task.name.clone(), task.clone()).is_some() {
            bail!("duplicate task name: {}", task.name);
        }
    }
    Ok(map)
}

pub(crate) fn resolve_selector(config: &TaskFile, selector: &str) -> Result<Vec<String>> {
    let task_map = task_map(config)?;
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    let mut group_stack = Vec::new();
    let name = selector.strip_prefix("task:").unwrap_or(selector);
    resolve_name(
        name,
        config,
        &task_map,
        &mut out,
        &mut seen,
        &mut group_stack,
    )
    .with_context(|| format!("could not resolve selector {selector}"))?;
    Ok(out)
}

fn resolve_name(
    name: &str,
    config: &TaskFile,
    task_map: &BTreeMap<String, Task>,
    out: &mut Vec<String>,
    seen: &mut HashSet<String>,
    group_stack: &mut Vec<String>,
) -> Result<()> {
    if task_map.contains_key(name) {
        add_task_with_deps(name, task_map, out, seen)?;
        return Ok(());
    }
    let Some(items) = config.groups.get(name) else {
        bail!("unknown task or group: {name}");
    };
    if group_stack.iter().any(|item| item == name) {
        bail!("group cycle: {}", group_stack.join(" -> "));
    }
    group_stack.push(name.to_string());
    for item in items {
        resolve_name(item, config, task_map, out, seen, group_stack)?;
    }
    group_stack.pop();
    Ok(())
}

pub(crate) fn add_task_with_deps(
    name: &str,
    task_map: &BTreeMap<String, Task>,
    out: &mut Vec<String>,
    seen: &mut HashSet<String>,
) -> Result<()> {
    let task = task_map
        .get(name)
        .ok_or_else(|| anyhow!("unknown task dependency: {name}"))?;
    for dep in &task.needs {
        add_task_with_deps(dep, task_map, out, seen)?;
    }
    if seen.insert(name.to_string()) {
        out.push(name.to_string());
    }
    Ok(())
}

pub(crate) fn task_tools_recursive(root: &Path, task_name: &str) -> Result<BTreeSet<String>> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    task_tools_recursive_from_map(&task_map, task_name)
}

pub(crate) fn task_repos_recursive_from_map(
    task_map: &BTreeMap<String, Task>,
    task_name: &str,
) -> Result<BTreeSet<String>> {
    let mut repos = BTreeSet::new();
    let mut stack = Vec::new();
    let mut seen = HashSet::new();
    collect_task_repos(
        task_name.strip_prefix("task:").unwrap_or(task_name),
        task_map,
        &mut repos,
        &mut stack,
        &mut seen,
    )?;
    Ok(repos)
}

pub(crate) fn selector_tools_recursive(root: &Path, selector: &str) -> Result<BTreeSet<String>> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    let task_names = resolve_selector(&config, selector)?;
    let mut tools = BTreeSet::new();
    for task_name in task_names {
        tools.extend(task_tools_recursive_from_map(&task_map, &task_name)?);
    }
    Ok(tools)
}

fn task_tools_recursive_from_map(
    task_map: &BTreeMap<String, Task>,
    task_name: &str,
) -> Result<BTreeSet<String>> {
    let mut tools = BTreeSet::new();
    let mut stack = Vec::new();
    let mut seen = HashSet::new();
    collect_task_tools(
        task_name.strip_prefix("task:").unwrap_or(task_name),
        task_map,
        &mut tools,
        &mut stack,
        &mut seen,
    )?;
    Ok(tools)
}

fn collect_task_tools(
    name: &str,
    task_map: &BTreeMap<String, Task>,
    tools: &mut BTreeSet<String>,
    stack: &mut Vec<String>,
    seen: &mut HashSet<String>,
) -> Result<()> {
    if seen.contains(name) {
        return Ok(());
    }
    if stack.iter().any(|item| item == name) {
        stack.push(name.to_string());
        bail!(
            "task dependency cycle while collecting tools: {}",
            stack.join(" -> ")
        );
    }
    stack.push(name.to_string());
    let task = task_map
        .get(name)
        .ok_or_else(|| anyhow!("unknown task for tool check: {name}"))?;
    for dep in &task.needs {
        collect_task_tools(dep, task_map, tools, stack, seen)?;
    }
    for tool in &task.tools {
        tools.insert(tool.clone());
    }
    stack.pop();
    seen.insert(name.to_string());
    Ok(())
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
    for repo in &task.repos {
        repos.insert(repo.clone());
    }
    for input in &task.inputs {
        if let Some(repo) = input_required_repo(input) {
            repos.insert(repo.to_string());
        }
    }
    stack.pop();
    seen.insert(name.to_string());
    Ok(())
}

fn input_required_repo(input: &str) -> Option<&str> {
    input
        .strip_prefix("external:")
        .or_else(|| input.strip_prefix("module-cache:"))
        .filter(|repo| !repo.trim().is_empty())
}

pub(crate) fn collect_task_node_workspaces(
    name: &str,
    task_map: &BTreeMap<String, Task>,
    workspaces: &mut BTreeSet<String>,
    stack: &mut Vec<String>,
    seen: &mut HashSet<String>,
) -> Result<()> {
    if seen.contains(name) {
        return Ok(());
    }
    if stack.iter().any(|item| item == name) {
        stack.push(name.to_string());
        bail!(
            "task dependency cycle while collecting node workspaces: {}",
            stack.join(" -> ")
        );
    }
    stack.push(name.to_string());
    let task = task_map
        .get(name)
        .ok_or_else(|| anyhow!("unknown task for node workspace collection: {name}"))?;
    for dep in &task.needs {
        collect_task_node_workspaces(dep, task_map, workspaces, stack, seen)?;
    }
    for workspace in &task.node_workspaces {
        workspaces.insert(workspace.clone());
    }
    stack.pop();
    seen.insert(name.to_string());
    Ok(())
}

pub(crate) fn task_to_json(task: &Task) -> serde_json::Value {
    serde_json::json!({
        "name": task.name,
        "title": task.title,
        "command": task.command,
        "tools": task.tools,
        "node_workspaces": task.node_workspaces,
        "inputs": task.inputs,
        "outputs": task.outputs,
        "tier": task.tier,
        "tags": task.tags,
        "owner": task.owner,
        "cwd": task.cwd,
        "env": task.env,
        "needs": task.needs,
        "repo": task.repo,
        "repos": task.repos,
        "internal": task.internal,
        "timeout_sec": task.timeout_sec,
        "platforms": task.platforms,
        "shell": task.shell,
    })
}
