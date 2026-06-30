use crate::config::load_tasks;
use crate::task_graph::{task_map, task_to_json};
use crate::task_planner::{plan_task_details, plan_tasks, PlanArgs};
use crate::task_runner::{final_gate_selectors, run_tasks, write_task_run_evidence};
use anyhow::{anyhow, bail, Result};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

#[derive(Debug, Serialize)]
struct PlanOutput {
    tasks: Vec<String>,
    changed_files: Vec<String>,
}

#[derive(Debug, Serialize)]
struct FinalSelectorsOutput {
    schema: &'static str,
    selectors: Vec<String>,
}

#[derive(Debug, Serialize)]
struct PlanExplainOutput {
    tasks: Vec<PlannedTaskOutput>,
    changed_files: Vec<String>,
    unknown_files: Vec<String>,
}

#[derive(Debug, Serialize)]
struct PlannedTaskOutput {
    name: String,
    reasons: Vec<String>,
}

pub(crate) fn cmd_task(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev task list|show|stats|coverage|plan|run ...");
    }
    match args.remove(0).as_str() {
        "final-selectors" => {
            let format = parse_format_args("task final-selectors", args)?;
            let config = load_tasks(root)?;
            let selectors = final_gate_selectors(&config)?;
            if format == "json" {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&FinalSelectorsOutput {
                        schema: "volang.final-task-selectors.v1",
                        selectors: selectors.clone(),
                    })?
                );
            } else {
                for selector in selectors {
                    println!("{selector}");
                }
            }
            Ok(())
        }
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
        "stats" => {
            let format = parse_format_args("task stats", args)?;
            let config = load_tasks(root)?;
            print_task_stats(&config, &format)?;
            Ok(())
        }
        "coverage" => {
            let format = parse_format_args("task coverage", args)?;
            let config = load_tasks(root)?;
            print_task_coverage(&config, &format)?;
            Ok(())
        }
        "plan" => {
            let opts = PlanArgs::parse(args)?;
            let plan = plan_task_details(root, &opts)?;
            if opts.format == "json" {
                if opts.explain {
                    let tasks = plan
                        .tasks
                        .iter()
                        .map(|name| PlannedTaskOutput {
                            name: name.clone(),
                            reasons: plan.reasons.get(name).cloned().unwrap_or_default(),
                        })
                        .collect();
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&PlanExplainOutput {
                            tasks,
                            changed_files: plan.changed_files,
                            unknown_files: plan.unknown_files,
                        })?
                    );
                } else {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&PlanOutput {
                            tasks: plan.tasks,
                            changed_files: plan.changed_files
                        })?
                    );
                }
            } else {
                if !plan.changed_files.is_empty() {
                    println!("Changed files:");
                    for path in &plan.changed_files {
                        println!("  {path}");
                    }
                    println!();
                }
                println!("Planned tasks:");
                for task in &plan.tasks {
                    println!("  {task}");
                    if opts.explain {
                        for reason in plan.reasons.get(task).into_iter().flatten() {
                            println!("    - {reason}");
                        }
                    }
                }
                if opts.explain && !plan.unknown_files.is_empty() {
                    println!();
                    println!("Unknown changed paths:");
                    for path in &plan.unknown_files {
                        println!("  {path}");
                    }
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
            run_tasks(root, &tasks)?;
            write_task_run_evidence(root, &opts.selector, opts.changed, &tasks)
        }
        other => bail!("unknown task command: {other}"),
    }
}

#[derive(Debug, Serialize)]
struct TaskStats {
    schema: &'static str,
    tasks: usize,
    internal_tasks: usize,
    groups: usize,
    group_metadata: usize,
    tasks_by_tier: BTreeMap<String, usize>,
    tasks_by_owner: BTreeMap<String, usize>,
    tasks_by_tag: BTreeMap<String, usize>,
    group_sizes: BTreeMap<String, usize>,
    groups_by_owner: BTreeMap<String, usize>,
    groups_by_tag: BTreeMap<String, usize>,
}

#[derive(Debug, Serialize)]
struct TaskCoverage {
    schema: &'static str,
    tasks: usize,
    groups: usize,
    missing_task_owner: usize,
    missing_task_tags: usize,
    missing_task_surface: usize,
    missing_group_metadata: usize,
    groups_without_owner: usize,
    groups_without_tags: usize,
    groups_without_inclusion_policy: usize,
    public_tasks_unreachable_from_top_level: usize,
}

const SURFACE_TAGS: &[&str] = &[
    "manifest-lint",
    "lang-case",
    "crate-unit",
    "crate-integration",
    "contract",
    "model",
    "docs-policy",
    "example-smoke",
    "benchmark",
    "app-build",
    "app-smoke",
    "release-verify",
    "legacy-excluded",
    "tooling",
    "repo-policy",
];

const TOP_LEVEL_GROUPS: &[&str] = &[
    "pr",
    "full",
    "quality",
    "test",
    "contract",
    "stress",
    "site",
    "release-verify",
    "legacy-excluded",
];

fn print_task_stats(config: &crate::config::TaskFile, format: &str) -> Result<()> {
    let stats = collect_task_stats(config);
    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&stats)?);
    } else {
        print_task_stats_text(&stats);
    }
    Ok(())
}

fn collect_task_stats(config: &crate::config::TaskFile) -> TaskStats {
    let mut by_tier = BTreeMap::new();
    let mut by_owner = BTreeMap::new();
    let mut by_tag = BTreeMap::new();
    let mut group_sizes = BTreeMap::new();
    let mut groups_by_owner = BTreeMap::new();
    let mut groups_by_tag = BTreeMap::new();
    let mut internal = 0usize;

    for task in &config.tasks {
        increment(&mut by_tier, &task.tier);
        increment(&mut by_owner, task.owner.as_deref().unwrap_or("(missing)"));
        if task.tags.is_empty() {
            increment(&mut by_tag, "(missing)");
        } else {
            for tag in &task.tags {
                increment(&mut by_tag, tag);
            }
        }
        if task.internal {
            internal += 1;
        }
    }
    for (group, items) in &config.groups {
        group_sizes.insert(group.clone(), items.len());
    }
    for group in &config.group_meta {
        increment(&mut groups_by_owner, &group.owner);
        if group.tags.is_empty() {
            increment(&mut groups_by_tag, "(missing)");
        } else {
            for tag in &group.tags {
                increment(&mut groups_by_tag, tag);
            }
        }
    }

    TaskStats {
        schema: "volang.task-stats.v1",
        tasks: config.tasks.len(),
        internal_tasks: internal,
        groups: config.groups.len(),
        group_metadata: config.group_meta.len(),
        tasks_by_tier: by_tier,
        tasks_by_owner: by_owner,
        tasks_by_tag: by_tag,
        group_sizes,
        groups_by_owner,
        groups_by_tag,
    }
}

fn print_task_stats_text(stats: &TaskStats) {
    println!("task stats:");
    println!("  tasks: {}", stats.tasks);
    println!("  internal tasks: {}", stats.internal_tasks);
    println!("  groups: {}", stats.groups);
    println!("  group metadata: {}", stats.group_metadata);
    print_count_map("tasks by tier", &stats.tasks_by_tier);
    print_count_map("tasks by owner", &stats.tasks_by_owner);
    print_count_map("tasks by tag", &stats.tasks_by_tag);
    println!("  group sizes:");
    if stats.group_sizes.is_empty() {
        println!("    (none): 0");
    } else {
        for (group, count) in &stats.group_sizes {
            println!("    {group}: {count}");
        }
    }
    print_count_map("groups by owner", &stats.groups_by_owner);
    print_count_map("groups by tag", &stats.groups_by_tag);
}

fn print_task_coverage(config: &crate::config::TaskFile, format: &str) -> Result<()> {
    let coverage = collect_task_coverage(config)?;
    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&coverage)?);
    } else {
        println!("task metadata coverage:");
        println!("  tasks: {}", coverage.tasks);
        println!("  groups: {}", coverage.groups);
        println!("  missing task owner: {}", coverage.missing_task_owner);
        println!("  missing task tags: {}", coverage.missing_task_tags);
        println!("  missing task surface: {}", coverage.missing_task_surface);
        println!(
            "  missing group metadata: {}",
            coverage.missing_group_metadata
        );
        println!("  groups without owner: {}", coverage.groups_without_owner);
        println!("  groups without tags: {}", coverage.groups_without_tags);
        println!(
            "  groups without inclusion policy: {}",
            coverage.groups_without_inclusion_policy
        );
        println!(
            "  public tasks unreachable from top-level groups: {}",
            coverage.public_tasks_unreachable_from_top_level
        );
    }
    if coverage.missing_task_owner != 0
        || coverage.missing_task_tags != 0
        || coverage.missing_task_surface != 0
        || coverage.missing_group_metadata != 0
        || coverage.groups_without_owner != 0
        || coverage.groups_without_tags != 0
        || coverage.groups_without_inclusion_policy != 0
        || coverage.public_tasks_unreachable_from_top_level != 0
    {
        bail!("task metadata coverage is incomplete");
    }
    Ok(())
}

fn collect_task_coverage(config: &crate::config::TaskFile) -> Result<TaskCoverage> {
    let group_meta: BTreeMap<_, _> = config
        .group_meta
        .iter()
        .map(|group| (group.name.clone(), group))
        .collect();
    let mut missing_task_owner = 0usize;
    let mut missing_task_tags = 0usize;
    let mut missing_task_surface = 0usize;
    for task in &config.tasks {
        if task.owner.as_deref().unwrap_or_default().trim().is_empty() {
            missing_task_owner += 1;
        }
        if task.tags.is_empty() {
            missing_task_tags += 1;
        }
        if !task
            .tags
            .iter()
            .any(|tag| SURFACE_TAGS.contains(&tag.as_str()))
        {
            missing_task_surface += 1;
        }
    }
    let mut groups_without_owner = 0usize;
    let mut groups_without_tags = 0usize;
    let mut groups_without_inclusion_policy = 0usize;
    for group in &config.group_meta {
        if group.owner.trim().is_empty() {
            groups_without_owner += 1;
        }
        if group.tags.is_empty() {
            groups_without_tags += 1;
        }
        if group.selection_policy.trim().is_empty() {
            groups_without_inclusion_policy += 1;
        }
    }

    let reachable = reachable_public_tasks(config)?;
    let mut public_tasks_unreachable_from_top_level = 0usize;
    for task in &config.tasks {
        if !task.internal && !reachable.contains(&task.name) {
            public_tasks_unreachable_from_top_level += 1;
        }
    }

    Ok(TaskCoverage {
        schema: "volang.task-coverage.v1",
        tasks: config.tasks.len(),
        groups: config.groups.len(),
        missing_task_owner,
        missing_task_tags,
        missing_task_surface,
        missing_group_metadata: config
            .groups
            .keys()
            .filter(|group| !group_meta.contains_key(*group))
            .count(),
        groups_without_owner,
        groups_without_tags,
        groups_without_inclusion_policy,
        public_tasks_unreachable_from_top_level,
    })
}

fn reachable_public_tasks(config: &crate::config::TaskFile) -> Result<BTreeSet<String>> {
    let mut reachable = BTreeSet::new();
    for group in TOP_LEVEL_GROUPS {
        if config.groups.contains_key(*group) {
            collect_group_tasks(config, group, &mut reachable, &mut Vec::new())?;
        }
    }
    Ok(reachable)
}

fn collect_group_tasks(
    config: &crate::config::TaskFile,
    group: &str,
    out: &mut BTreeSet<String>,
    stack: &mut Vec<String>,
) -> Result<()> {
    if stack.iter().any(|item| item == group) {
        stack.push(group.to_string());
        bail!(
            "group cycle while computing task coverage: {}",
            stack.join(" -> ")
        );
    }
    stack.push(group.to_string());
    if let Some(items) = config.groups.get(group) {
        for item in items {
            if config.groups.contains_key(item) {
                collect_group_tasks(config, item, out, stack)?;
            } else {
                out.insert(item.clone());
            }
        }
    }
    stack.pop();
    Ok(())
}

fn parse_format_args(command: &str, args: Vec<String>) -> Result<String> {
    let mut format = "text".to_string();
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--format" => {
                i += 1;
                format = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--format requires a value"))?
                    .clone();
            }
            arg if arg.starts_with("--format=") => {
                format = arg["--format=".len()..].to_string();
            }
            other => bail!("unknown {command} argument: {other}"),
        }
        i += 1;
    }
    if format != "text" && format != "json" {
        bail!("--format must be text or json");
    }
    Ok(format)
}

fn increment(counts: &mut BTreeMap<String, usize>, key: &str) {
    *counts.entry(key.to_string()).or_default() += 1;
}

fn print_count_map(title: &str, counts: &BTreeMap<String, usize>) {
    println!("  {title}:");
    if counts.is_empty() {
        println!("    (none): 0");
        return;
    }
    for (key, count) in counts {
        println!("    {key}: {count}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn task_stats_json_schema_is_stable() {
        let stats = TaskStats {
            schema: "volang.task-stats.v1",
            tasks: 1,
            internal_tasks: 0,
            groups: 1,
            group_metadata: 1,
            tasks_by_tier: BTreeMap::new(),
            tasks_by_owner: BTreeMap::new(),
            tasks_by_tag: BTreeMap::new(),
            group_sizes: BTreeMap::new(),
            groups_by_owner: BTreeMap::new(),
            groups_by_tag: BTreeMap::new(),
        };
        let value = serde_json::to_value(stats).unwrap();
        assert_eq!(value["schema"], "volang.task-stats.v1");
        assert!(value.get("group_metadata").is_some());
        assert!(value.get("tasks_by_owner").is_some());
    }

    #[test]
    fn final_selectors_json_schema_is_stable_060() {
        let output = FinalSelectorsOutput {
            schema: "volang.final-task-selectors.v1",
            selectors: vec![
                "contract".to_string(),
                "vm-production".to_string(),
                "site".to_string(),
                "release-verify".to_string(),
            ],
        };
        let value = serde_json::to_value(output).unwrap();
        assert_eq!(value["schema"], "volang.final-task-selectors.v1");
        assert_eq!(
            value["selectors"],
            serde_json::json!(["contract", "vm-production", "site", "release-verify"])
        );
    }
}
