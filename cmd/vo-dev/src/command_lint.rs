use crate::config::{NodeWorkspace, ProjectFile, Task};
use crate::test_config;
use anyhow::{anyhow, bail, Result};
use std::collections::BTreeSet;
use std::path::Path;

pub(crate) fn inferred_tools_for_command(command: &[String]) -> BTreeSet<&'static str> {
    let mut tools = BTreeSet::new();
    let Some(binary) = command.first().map(String::as_str) else {
        return tools;
    };
    match binary {
        "cargo" => {
            tools.insert("rust");
            if cargo_run_invokes_vo_dev(command, None) {
                tools.insert("vo-dev");
            }
        }
        "node" => {
            tools.insert("node");
        }
        "npm" => {
            tools.insert("node");
            tools.insert("npm");
        }
        "wasm-pack" => {
            tools.insert("rust");
            tools.insert("wasm-pack");
        }
        "python" | "python3" => {
            tools.insert("python");
        }
        "./d.py" => {
            tools.insert("python");
            tools.insert("rust");
            tools.insert("vo-dev");
        }
        "vo-dev" => {
            tools.insert("vo-dev");
        }
        _ => {}
    }
    if let Some(args) = vo_dev_invocation_args(command) {
        infer_vo_dev_subcommand_tools(args, &mut tools);
    }
    tools
}

pub(crate) fn validate_task_vo_dev_invocation(task: &Task) -> Result<()> {
    if cargo_run_invokes_vo_dev(&task.command, task.cwd.as_deref()) {
        bail!(
            "task {} re-enters vo-dev through cargo; invoke the current binary as [\"vo-dev\", ...]",
            task.name
        );
    }
    Ok(())
}

fn vo_dev_invocation_args(command: &[String]) -> Option<&[String]> {
    let binary = command.first()?.as_str();
    if binary == "vo-dev" {
        return Some(&command[1..]);
    }
    if !cargo_run_invokes_vo_dev(command, None) {
        return None;
    }
    let separator = command.iter().position(|arg| arg == "--")?;
    Some(&command[separator + 1..])
}

fn cargo_run_invokes_vo_dev(command: &[String], cwd: Option<&str>) -> bool {
    if command.first().map(String::as_str) != Some("cargo") {
        return false;
    }
    let cargo_args_end = command
        .iter()
        .position(|arg| arg == "--")
        .unwrap_or(command.len());
    if !command[1..cargo_args_end].iter().any(|arg| arg == "run") {
        return false;
    }
    if cwd.is_some_and(is_vo_dev_manifest_directory) {
        return true;
    }
    let args = &command[1..cargo_args_end];
    args.windows(2).any(|pair| {
        (matches!(pair[0].as_str(), "-p" | "--package" | "--bin") && pair[1] == "vo-dev")
            || (pair[0] == "--manifest-path" && is_vo_dev_manifest(&pair[1]))
    }) || args.iter().any(|arg| {
        matches!(
            arg.as_str(),
            "-pvo-dev" | "-p=vo-dev" | "--package=vo-dev" | "--bin=vo-dev"
        ) || arg
            .strip_prefix("--manifest-path=")
            .is_some_and(is_vo_dev_manifest)
    })
}

fn is_vo_dev_manifest(path: &str) -> bool {
    let path = path.replace('\\', "/");
    path == "cmd/vo-dev/Cargo.toml" || path.ends_with("/cmd/vo-dev/Cargo.toml")
}

fn is_vo_dev_manifest_directory(path: &str) -> bool {
    let path = path.replace('\\', "/");
    let path = path.trim_end_matches('/');
    path == "cmd/vo-dev" || path.ends_with("/cmd/vo-dev")
}

fn infer_vo_dev_subcommand_tools(args: &[String], tools: &mut BTreeSet<&'static str>) {
    match args {
        [command, ..] if command == "studio-install-local-vogui" => {
            tools.insert("node");
            tools.insert("npm");
        }
        [first, second, _repo, _location, rest @ ..]
            if first == "first-party" && matches!(second.as_str(), "run" | "run-workspace") =>
        {
            if let Some(separator) = rest.iter().position(|arg| arg == "--") {
                let nested = &rest[separator + 1..];
                tools.extend(inferred_tools_for_command(nested));
            }
        }
        _ => {}
    }
}

#[derive(Debug, PartialEq, Eq)]
enum FirstPartyRunLocation<'a> {
    Path(&'a str),
    Workspace(&'a str),
}

#[derive(Debug, PartialEq, Eq)]
struct FirstPartyRunParts<'a> {
    repo: &'a str,
    location: FirstPartyRunLocation<'a>,
    nested_command: &'a [String],
}

fn first_party_run_parts(command: &[String]) -> Option<FirstPartyRunParts<'_>> {
    let args = vo_dev_invocation_args(command)?;
    match args {
        [first, second, repo, subdir, rest @ ..] if first == "first-party" && second == "run" => {
            let separator = rest.iter().position(|arg| arg == "--")?;
            Some(FirstPartyRunParts {
                repo,
                location: FirstPartyRunLocation::Path(subdir),
                nested_command: &rest[separator + 1..],
            })
        }
        [first, second, repo, workspace, rest @ ..]
            if first == "first-party" && second == "run-workspace" =>
        {
            let separator = rest.iter().position(|arg| arg == "--")?;
            Some(FirstPartyRunParts {
                repo,
                location: FirstPartyRunLocation::Workspace(workspace),
                nested_command: &rest[separator + 1..],
            })
        }
        _ => None,
    }
}

pub(crate) fn validate_first_party_run_node_workspace(
    task: &Task,
    node_workspaces: &[NodeWorkspace],
    project: &ProjectFile,
) -> Result<()> {
    let Some(parts) = first_party_run_parts(&task.command) else {
        return Ok(());
    };
    if !inferred_tools_for_command(parts.nested_command).contains("node") {
        return Ok(());
    }
    let (workspace_path, workspace_label) = match parts.location {
        FirstPartyRunLocation::Path(subdir) => {
            bail!(
                "task {} runs node in first-party path {}/{subdir}; use first-party run-workspace <repo> <workspace> so eng/project.toml owns the workspace path",
                task.name,
                parts.repo
            );
        }
        FirstPartyRunLocation::Workspace(workspace_name) => {
            let repo = project
                .first_party
                .iter()
                .find(|repo| repo.name == parts.repo)
                .ok_or_else(|| {
                    anyhow!(
                        "task {} references unknown first-party repo {}",
                        task.name,
                        parts.repo
                    )
                })?;
            let workspace = repo
                .workspace
                .iter()
                .find(|workspace| workspace.name == workspace_name)
                .ok_or_else(|| {
                    anyhow!(
                        "task {} references unknown first-party workspace {}:{}",
                        task.name,
                        parts.repo,
                        workspace_name
                    )
                })?;
            if workspace.kind != "node" {
                bail!(
                    "task {} runs node in first-party workspace {}:{}, but its kind is {}",
                    task.name,
                    parts.repo,
                    workspace_name,
                    workspace.kind
                );
            }
            (
                workspace.path.trim_end_matches('/').to_string(),
                format!("{}:{}", parts.repo, workspace_name),
            )
        }
    };
    let workspace = node_workspaces
        .iter()
        .find(|workspace| {
            workspace.repo.as_deref() == Some(parts.repo)
                && workspace.path.trim_end_matches('/') == workspace_path
        })
        .ok_or_else(|| {
            anyhow!(
                "task {} runs node in first-party workspace {}, but eng/toolchains.toml has no matching node_workspace for {}:{}",
                task.name,
                workspace_label,
                parts.repo,
                workspace_path
            )
        })?;
    if !task
        .node_workspaces
        .iter()
        .any(|declared| declared == &workspace.name)
    {
        bail!(
            "task {} runs node in first-party workspace {} but does not declare node_workspaces = [\"{}\"]",
            task.name,
            workspace_label,
            workspace.name
        );
    }
    Ok(())
}

pub(crate) fn validate_embedded_test_task_tools(root: &Path, task: &Task) -> Result<()> {
    let Some(target_specs) = test_run_target_specs(&task.command)? else {
        return Ok(());
    };
    let required_tools = test_config::required_tools_for_test_targets(root, &target_specs)?;
    for tool in required_tools {
        if !task.tools.iter().any(|declared| declared == &tool) {
            bail!(
                "task {} runs vo-dev test targets that require {} from eng/tests.toml, but tools does not declare {}",
                task.name,
                tool,
                tool
            );
        }
    }
    Ok(())
}

fn test_run_target_specs(command: &[String]) -> Result<Option<Vec<String>>> {
    let Some(index) = command
        .windows(2)
        .position(|pair| pair[0] == "test" && pair[1] == "run")
    else {
        return Ok(None);
    };
    if !invokes_vo_dev_test_run(command, index) {
        return Ok(None);
    }
    let args = &command[index + 2..];
    let mut targets = None;
    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];
        if arg == "--targets" {
            let Some(value) = args.get(i + 1) else {
                bail!("vo-dev test run command has --targets without a value");
            };
            targets = Some(split_target_specs(value)?);
            i += 2;
        } else if let Some(value) = arg.strip_prefix("--targets=") {
            targets = Some(split_target_specs(value)?);
            i += 1;
        } else {
            i += 1;
        }
    }
    Ok(Some(targets.unwrap_or_default()))
}

fn invokes_vo_dev_test_run(command: &[String], test_index: usize) -> bool {
    if command.first().is_some_and(|binary| binary == "vo-dev") {
        return test_index == 1;
    }
    cargo_run_invokes_vo_dev(command, None)
        && command
            .get(test_index.saturating_sub(1))
            .is_some_and(|arg| arg == "--")
}

fn split_target_specs(value: &str) -> Result<Vec<String>> {
    let mut out = Vec::new();
    for item in value.split(',') {
        let trimmed = item.trim();
        if trimmed.is_empty() {
            bail!("vo-dev test run command has an empty --targets entry");
        }
        out.push(trimmed.to_string());
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_vo_dev_test_targets_from_cargo_command() {
        let command = vec![
            "cargo".to_string(),
            "run".to_string(),
            "-q".to_string(),
            "-p".to_string(),
            "vo-dev".to_string(),
            "--".to_string(),
            "test".to_string(),
            "run".to_string(),
            "--suite".to_string(),
            "lang".to_string(),
            "--targets".to_string(),
            "wasm,gc-vm".to_string(),
        ];
        let targets = test_run_target_specs(&command).unwrap().unwrap();
        assert_eq!(targets, vec!["wasm".to_string(), "gc-vm".to_string()]);
    }

    #[test]
    fn rejects_task_cargo_reentry_for_vo_dev() {
        let task: Task = toml::from_str(
            r#"
name = "nested"
title = "Nested"
command = ["cargo", "run", "-q", "-p", "vo-dev", "--", "lint", "all"]
tools = ["rust", "vo-dev"]
tier = "contract"
owner = "eng"
            "#,
        )
        .unwrap();
        let error = validate_task_vo_dev_invocation(&task).unwrap_err();
        assert!(error.to_string().contains("re-enters vo-dev through cargo"));
    }

    #[test]
    fn rejects_every_supported_cargo_reentry_spelling_for_vo_dev() {
        for command in [
            r#"["cargo", "+nightly", "run", "--package=vo-dev", "--", "lint", "all"]"#,
            r#"["cargo", "run", "-pvo-dev", "--", "lint", "all"]"#,
            r#"["cargo", "run", "--bin", "vo-dev", "--", "lint", "all"]"#,
            r#"["cargo", "run", "--bin=vo-dev", "--", "lint", "all"]"#,
            r#"["cargo", "run", "--manifest-path", "cmd/vo-dev/Cargo.toml", "--", "lint", "all"]"#,
            r#"["cargo", "run", "--manifest-path=cmd\\vo-dev\\Cargo.toml", "--", "lint", "all"]"#,
        ] {
            let task: Task = toml::from_str(&format!(
                "name = \"nested\"\ntitle = \"Nested\"\ncommand = {command}\ntools = [\"rust\", \"vo-dev\"]\ntier = \"contract\"\nowner = \"eng\"\n"
            ))
            .unwrap();
            validate_task_vo_dev_invocation(&task).unwrap_err();
        }

        let task: Task = toml::from_str(
            r#"
name = "nested-cwd"
title = "Nested cwd"
cwd = "cmd/vo-dev"
command = ["cargo", "run", "--", "lint", "all"]
tools = ["rust", "vo-dev"]
tier = "contract"
owner = "eng"
            "#,
        )
        .unwrap();
        validate_task_vo_dev_invocation(&task).unwrap_err();
    }

    #[test]
    fn accepts_cargo_operations_that_do_not_run_vo_dev() {
        let task: Task = toml::from_str(
            r#"
name = "test-vo-dev"
title = "Test vo-dev"
command = ["cargo", "test", "-p", "vo-dev", "--", "run"]
tools = ["rust", "vo-dev"]
tier = "contract"
owner = "eng"
            "#,
        )
        .unwrap();
        validate_task_vo_dev_invocation(&task).unwrap();
    }

    #[test]
    fn accepts_task_current_vo_dev_invocation() {
        let task: Task = toml::from_str(
            r#"
name = "direct"
title = "Direct"
command = ["vo-dev", "lint", "all"]
tools = ["vo-dev"]
tier = "contract"
owner = "eng"
            "#,
        )
        .unwrap();
        validate_task_vo_dev_invocation(&task).unwrap();
    }

    #[test]
    fn parses_vo_dev_test_targets_with_equals_syntax() {
        let command = vec![
            "vo-dev".to_string(),
            "test".to_string(),
            "run".to_string(),
            "--targets=vm,jit".to_string(),
        ];
        let targets = test_run_target_specs(&command).unwrap().unwrap();
        assert_eq!(targets, vec!["vm".to_string(), "jit".to_string()]);
    }

    #[test]
    fn leaves_non_test_commands_unclassified() {
        let command = vec!["cargo".to_string(), "check".to_string()];
        assert!(test_run_target_specs(&command).unwrap().is_none());
    }

    #[test]
    fn ignores_non_vo_dev_test_run_commands() {
        let command = vec![
            "cargo".to_string(),
            "test".to_string(),
            "run".to_string(),
            "--targets".to_string(),
            "wasm".to_string(),
        ];
        assert!(test_run_target_specs(&command).unwrap().is_none());
    }

    #[test]
    fn infers_tools_from_first_party_nested_command() {
        let command = vec![
            "cargo".to_string(),
            "run".to_string(),
            "-q".to_string(),
            "-p".to_string(),
            "vo-dev".to_string(),
            "--".to_string(),
            "first-party".to_string(),
            "run".to_string(),
            "vogui".to_string(),
            "js".to_string(),
            "--".to_string(),
            "npm".to_string(),
            "ci".to_string(),
        ];
        let tools = inferred_tools_for_command(&command);
        assert!(tools.contains("rust"));
        assert!(tools.contains("vo-dev"));
        assert!(tools.contains("node"));
        assert!(tools.contains("npm"));
    }

    #[test]
    fn parses_first_party_run_path_parts() {
        let command = vec![
            "cargo".to_string(),
            "run".to_string(),
            "-q".to_string(),
            "-p".to_string(),
            "vo-dev".to_string(),
            "--".to_string(),
            "first-party".to_string(),
            "run".to_string(),
            "vogui".to_string(),
            "js".to_string(),
            "--".to_string(),
            "npm".to_string(),
            "run".to_string(),
            "build".to_string(),
        ];
        let parts = first_party_run_parts(&command).unwrap();
        assert_eq!(parts.repo, "vogui");
        assert_eq!(parts.location, FirstPartyRunLocation::Path("js"));
        assert_eq!(
            parts.nested_command,
            ["npm".to_string(), "run".to_string(), "build".to_string()]
        );
    }

    #[test]
    fn parses_first_party_run_named_workspace_parts() {
        let command = vec![
            "cargo".to_string(),
            "run".to_string(),
            "-q".to_string(),
            "-p".to_string(),
            "vo-dev".to_string(),
            "--".to_string(),
            "first-party".to_string(),
            "run-workspace".to_string(),
            "vogui".to_string(),
            "js".to_string(),
            "--".to_string(),
            "npm".to_string(),
            "ci".to_string(),
        ];
        let parts = first_party_run_parts(&command).unwrap();
        assert_eq!(parts.repo, "vogui");
        assert_eq!(parts.location, FirstPartyRunLocation::Workspace("js"));
        assert_eq!(parts.nested_command, ["npm".to_string(), "ci".to_string()]);
    }

    #[test]
    fn infers_tools_from_first_party_named_workspace() {
        let command = vec![
            "vo-dev".to_string(),
            "first-party".to_string(),
            "run-workspace".to_string(),
            "vogui".to_string(),
            "js".to_string(),
            "--".to_string(),
            "npm".to_string(),
            "ci".to_string(),
        ];
        let tools = inferred_tools_for_command(&command);
        assert!(tools.contains("vo-dev"));
        assert!(tools.contains("node"));
        assert!(tools.contains("npm"));
    }

    #[test]
    fn infers_tools_from_studio_install_local_vogui() {
        let command = vec![
            "cargo".to_string(),
            "run".to_string(),
            "-q".to_string(),
            "-p".to_string(),
            "vo-dev".to_string(),
            "--".to_string(),
            "studio-install-local-vogui".to_string(),
        ];
        let tools = inferred_tools_for_command(&command);
        assert!(tools.contains("rust"));
        assert!(tools.contains("vo-dev"));
        assert!(tools.contains("node"));
        assert!(tools.contains("npm"));
    }

    #[test]
    fn infers_tools_from_d_py_dispatch() {
        let command = vec!["./d.py".to_string(), "help".to_string()];
        let tools = inferred_tools_for_command(&command);
        assert!(tools.contains("python"));
        assert!(tools.contains("rust"));
        assert!(tools.contains("vo-dev"));
    }
}
