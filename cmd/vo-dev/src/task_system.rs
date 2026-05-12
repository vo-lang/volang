use crate::config::load_tasks;
use crate::task_graph::{task_map, task_to_json};
use crate::task_planner::{plan_tasks, PlanArgs};
use crate::task_runner::run_tasks;
use anyhow::{anyhow, bail, Result};
use serde::Serialize;
use std::path::Path;

#[derive(Debug, Serialize)]
struct PlanOutput {
    tasks: Vec<String>,
    changed_files: Vec<String>,
}

pub(crate) fn cmd_task(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev task list|show|plan|run ...");
    }
    match args.remove(0).as_str() {
        "list" => {
            if !args.is_empty() {
                bail!("usage: vo-dev task list");
            }
            let config = load_tasks(root)?;
            for task in &config.tasks {
                println!("{}\t{}\t{}", task.name, task.tier, task.title);
            }
            Ok(())
        }
        "show" => {
            if args.len() != 1 {
                bail!("usage: vo-dev task show <task>");
            }
            let name = &args[0];
            let config = load_tasks(root)?;
            let task = task_map(&config)?
                .get(name)
                .ok_or_else(|| anyhow!("unknown task: {name}"))?
                .clone();
            println!("{}", serde_json::to_string_pretty(&task_to_json(&task))?);
            Ok(())
        }
        "plan" => {
            let opts = PlanArgs::parse(args)?;
            let (tasks, files) = plan_tasks(root, &opts)?;
            if opts.format == "json" {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&PlanOutput {
                        tasks,
                        changed_files: files
                    })?
                );
            } else {
                if !files.is_empty() {
                    println!("Changed files:");
                    for path in &files {
                        println!("  {path}");
                    }
                    println!();
                }
                println!("Planned tasks:");
                for task in tasks {
                    println!("  {task}");
                }
            }
            Ok(())
        }
        "run" => {
            let opts = PlanArgs::parse(args)?;
            if opts.format == "json" {
                bail!("vo-dev task run does not support --format json");
            }
            let (tasks, _) = plan_tasks(root, &opts)?;
            run_tasks(root, &tasks)
        }
        other => bail!("unknown task command: {other}"),
    }
}
