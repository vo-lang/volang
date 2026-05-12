use crate::config::{load_ci, load_tasks, TaskFile};
use crate::glob::path_matches;
use crate::lint_system::lint_task_file;
use crate::task_graph::{add_task_with_deps, resolve_selector, task_map};
use anyhow::{anyhow, bail, Result};
use std::collections::{BTreeSet, HashSet};
use std::path::Path;
use std::process::Command;

#[derive(Debug)]
pub(crate) struct PlanArgs {
    pub(crate) selector: String,
    pub(crate) selector_explicit: bool,
    pub(crate) changed: bool,
    pub(crate) base: Option<String>,
    pub(crate) head: Option<String>,
    pub(crate) format: String,
}

impl PlanArgs {
    pub(crate) fn parse(args: Vec<String>) -> Result<Self> {
        let mut selector = None;
        let mut changed = false;
        let mut base = None;
        let mut head = None;
        let mut format = "text".to_string();
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--changed" => changed = true,
                "--base" => {
                    i += 1;
                    base = Some(
                        args.get(i)
                            .ok_or_else(|| anyhow!("--base requires a value"))?
                            .clone(),
                    );
                }
                "--head" => {
                    i += 1;
                    head = Some(
                        args.get(i)
                            .ok_or_else(|| anyhow!("--head requires a value"))?
                            .clone(),
                    );
                }
                "--format" => {
                    i += 1;
                    format = args
                        .get(i)
                        .ok_or_else(|| anyhow!("--format requires a value"))?
                        .clone();
                    if format != "text" && format != "json" {
                        bail!("--format must be text or json");
                    }
                }
                arg if arg.starts_with('-') => bail!("unknown argument: {arg}"),
                arg => {
                    if selector.is_some() {
                        bail!("multiple selectors provided");
                    }
                    selector = Some(arg.to_string());
                }
            }
            i += 1;
        }
        let selector_explicit = selector.is_some();
        Ok(Self {
            selector: selector.unwrap_or_else(|| "smart".to_string()),
            selector_explicit,
            changed,
            base,
            head,
            format,
        })
    }
}

pub(crate) fn plan_tasks(root: &Path, opts: &PlanArgs) -> Result<(Vec<String>, Vec<String>)> {
    let config = load_tasks(root)?;
    lint_task_file(root, &config)?;
    let task_map = task_map(&config)?;
    let changed_mode = opts.changed || opts.selector == "smart";
    if !changed_mode {
        let tasks = resolve_selector(&config, &opts.selector)?;
        return Ok((tasks, Vec::new()));
    }

    let ci = load_ci(root)?;
    if ci.version != 1 {
        bail!("eng/ci.toml version must be 1");
    }
    let files = changed_files(root, opts.base.as_deref(), opts.head.as_deref())?;
    let scope = if opts.selector == "smart" {
        None
    } else {
        Some(
            resolve_selector(&config, &opts.selector)?
                .into_iter()
                .collect::<HashSet<_>>(),
        )
    };
    let mut selected = Vec::new();
    let mut selected_set = HashSet::new();
    let mut unknown = Vec::new();

    for file in &files {
        let mut matched = false;
        for known in &ci.known_prefix {
            if path_matches(file, &known.path) {
                matched = true;
                for name in &known.tasks {
                    if in_scope(name, &scope) && selected_set.insert(name.clone()) {
                        selected.push(name.clone());
                    }
                }
            }
        }
        for task in &config.tasks {
            if task.internal {
                continue;
            }
            if !in_scope(&task.name, &scope) {
                continue;
            }
            if task
                .inputs
                .iter()
                .any(|pattern| path_matches(file, pattern))
            {
                matched = true;
                if selected_set.insert(task.name.clone()) {
                    selected.push(task.name.clone());
                }
            }
        }
        if !matched {
            unknown.push(file.clone());
        }
    }

    if !unknown.is_empty() {
        match ci
            .changed_files
            .unknown_path_policy
            .as_deref()
            .unwrap_or("fallback")
        {
            "fallback" => {
                for name in &ci.changed_files.fallback {
                    if in_scope(name, &scope) && selected_set.insert(name.clone()) {
                        selected.push(name.clone());
                    }
                }
            }
            "error" => bail!(
                "changed paths matched no task input: {}",
                unknown.join(", ")
            ),
            other => bail!("invalid unknown_path_policy: {other}"),
        }
    }

    add_downstream_dependents(&config, &scope, &mut selected, &mut selected_set);

    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for name in selected {
        add_task_with_deps(&name, &task_map, &mut out, &mut seen)?;
    }
    Ok((out, files))
}

fn add_downstream_dependents(
    config: &TaskFile,
    scope: &Option<HashSet<String>>,
    selected: &mut Vec<String>,
    selected_set: &mut HashSet<String>,
) {
    let mut index = 0;
    while index < selected.len() {
        let selected_name = selected[index].clone();
        index += 1;
        for task in &config.tasks {
            if !in_scope(&task.name, scope) {
                continue;
            }
            if task.needs.iter().any(|dep| dep == &selected_name)
                && selected_set.insert(task.name.clone())
            {
                selected.push(task.name.clone());
            }
        }
    }
}

fn in_scope(name: &str, scope: &Option<HashSet<String>>) -> bool {
    scope.as_ref().is_none_or(|scope| scope.contains(name))
}

fn changed_files(root: &Path, base: Option<&str>, head: Option<&str>) -> Result<Vec<String>> {
    let mut files = BTreeSet::new();
    match (base, head) {
        (Some(base), Some(head)) if !base.chars().all(|ch| ch == '0') => {
            for line in git_lines(root, &["diff", "--name-only", &format!("{base}...{head}")])? {
                files.insert(line);
            }
            return Ok(files.into_iter().collect());
        }
        (Some(_), Some(head)) => {
            for line in git_lines(root, &["ls-tree", "-r", "--name-only", head])? {
                files.insert(line);
            }
            return Ok(files.into_iter().collect());
        }
        (Some(_), None) | (None, Some(_)) => bail!("--base and --head must be provided together"),
        (None, None) => {}
    }

    let upstream = git_lines_optional(
        root,
        &[
            "rev-parse",
            "--abbrev-ref",
            "--symbolic-full-name",
            "@{upstream}",
        ],
    )?;
    if let Some(upstream) = upstream.first() {
        for line in git_lines(
            root,
            &["diff", "--name-only", &format!("{upstream}...HEAD")],
        )? {
            files.insert(line);
        }
    } else {
        for line in git_lines_optional(root, &["diff", "--name-only", "HEAD~1...HEAD"])? {
            files.insert(line);
        }
    }
    for args in [
        ["diff", "--name-only"].as_slice(),
        ["diff", "--cached", "--name-only"].as_slice(),
        ["ls-files", "--others", "--exclude-standard"].as_slice(),
    ] {
        for line in git_lines(root, args)? {
            files.insert(line);
        }
    }
    Ok(files.into_iter().collect())
}

pub(crate) fn git_lines(root: &Path, args: &[&str]) -> Result<Vec<String>> {
    let output = Command::new("git").args(args).current_dir(root).output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stderr = stderr.trim();
        bail!(
            "git {} failed: {}",
            args.join(" "),
            if stderr.is_empty() {
                "no stderr"
            } else {
                stderr
            }
        );
    }
    Ok(parse_lines(&output.stdout))
}

fn git_lines_optional(root: &Path, args: &[&str]) -> Result<Vec<String>> {
    let output = Command::new("git").args(args).current_dir(root).output()?;
    if !output.status.success() {
        return Ok(Vec::new());
    }
    Ok(parse_lines(&output.stdout))
}

fn parse_lines(stdout: &[u8]) -> Vec<String> {
    String::from_utf8_lossy(stdout)
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(ToOwned::to_owned)
        .collect()
}
