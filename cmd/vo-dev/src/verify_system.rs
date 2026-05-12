use crate::config::{load_tasks, load_toolchains, Task};
use crate::first_party::project_repo_path;
use crate::task_graph::{task_map, task_tools_recursive};
use crate::task_planner::{plan_tasks, PlanArgs};
use crate::task_runner::run_tasks;
use crate::tool_system::{check_tools, ToolStatus};
use anyhow::{anyhow, bail, Result};
use serde::Serialize;
use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};

pub(crate) fn cmd_verify(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev verify plan|run <selector> [--changed] [--base <sha>] [--head <sha>]");
    }
    let command = args.remove(0);
    let opts = PlanArgs::parse(args)?;
    if opts.format != "text" {
        bail!("vo-dev verify only supports text output");
    }
    let (tasks, files) = plan_tasks(root, &opts)?;
    let report = verify_report(root, &tasks)?;
    match command.as_str() {
        "plan" => {
            if !files.is_empty() {
                println!("Changed files:");
                for file in files {
                    println!("  {file}");
                }
                println!();
            }
            print_verify_report(&report);
            if report.iter().any(|task| !task.ready) {
                bail!("verification has blocked tasks");
            }
            Ok(())
        }
        "run" => {
            print_verify_report(&report);
            if report.iter().any(|task| !task.ready) {
                bail!("verification has blocked tasks");
            }
            run_tasks(root, &tasks)
        }
        other => bail!("unknown verify command: {other}"),
    }
}

#[derive(Debug, Serialize)]
struct VerifyTask {
    task: String,
    title: String,
    ready: bool,
    blocked_tools: Vec<ToolStatus>,
    blocked_requirements: Vec<String>,
}

fn verify_report(root: &Path, tasks: &[String]) -> Result<Vec<VerifyTask>> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    let toolchains = load_toolchains(root)?;
    let mut report = Vec::new();
    for name in tasks {
        let task = task_map
            .get(name)
            .ok_or_else(|| anyhow!("unknown task for verify: {name}"))?;
        let statuses = check_tools(&toolchains, task_tools_recursive(root, name)?)?;
        let blocked_tools: Vec<_> = statuses.into_iter().filter(|status| !status.ok).collect();
        let blocked_requirements = task_runtime_requirements(root, name, &task_map)?;
        report.push(VerifyTask {
            task: name.clone(),
            title: task.title.clone(),
            ready: blocked_tools.is_empty() && blocked_requirements.is_empty(),
            blocked_tools,
            blocked_requirements,
        });
    }
    Ok(report)
}

fn print_verify_report(report: &[VerifyTask]) {
    println!("Verification plan:");
    for task in report {
        if task.ready {
            println!("  READY   {}\t{}", task.task, task.title);
        } else {
            println!("  BLOCKED {}\t{}", task.task, task.title);
            for tool in &task.blocked_tools {
                println!("    {}: {}", tool.name, tool.message);
            }
            for requirement in &task.blocked_requirements {
                println!("    {requirement}");
            }
        }
    }
}

fn task_runtime_requirements(
    root: &Path,
    name: &str,
    task_map: &BTreeMap<String, Task>,
) -> Result<Vec<String>> {
    let mut missing = Vec::new();
    let mut seen = HashSet::new();
    collect_task_runtime_requirements(root, name, task_map, &mut seen, &mut missing)?;
    missing.sort();
    missing.dedup();
    Ok(missing)
}

fn collect_task_runtime_requirements(
    root: &Path,
    name: &str,
    task_map: &BTreeMap<String, Task>,
    seen: &mut HashSet<String>,
    missing: &mut Vec<String>,
) -> Result<()> {
    let task = task_map
        .get(name)
        .ok_or_else(|| anyhow!("unknown task for verify: {name}"))?;
    for dep in &task.needs {
        collect_task_runtime_requirements(root, dep, task_map, seen, missing)?;
    }
    if !seen.insert(name.to_string()) {
        return Ok(());
    }

    let cwd = root.join(task.cwd.as_deref().unwrap_or("."));
    if !cwd.exists() {
        missing.push(format!(
            "cwd missing for task {}: {}",
            task.name,
            cwd.display()
        ));
    }
    if let Some(repo) = &task.repo {
        if let Err(err) = project_repo_path(root, repo) {
            missing.push(format!(
                "repo {repo} unavailable for task {}: {err:#}",
                task.name
            ));
        }
    }
    for path in command_local_paths(&task.command, &cwd) {
        if !path.exists() {
            missing.push(format!(
                "command path missing for task {}: {}",
                task.name,
                path.display()
            ));
        }
    }
    Ok(())
}

fn command_local_paths(command: &[String], cwd: &Path) -> Vec<PathBuf> {
    command
        .iter()
        .filter(|arg| {
            !arg.starts_with('-')
                && *arg != "--"
                && (arg.starts_with("./") || arg.starts_with("../") || arg.contains('/'))
        })
        .map(|arg| {
            let path = PathBuf::from(arg.as_str());
            if path.is_absolute() {
                path
            } else {
                cwd.join(path)
            }
        })
        .collect()
}
