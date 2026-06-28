use crate::config::{load_toolchains, Tool, ToolchainFile};
use crate::task_graph::selector_tools_recursive;
use anyhow::{anyhow, bail, Result};
use serde::Serialize;
use std::collections::BTreeSet;
use std::path::Path;
use std::process::{Command, Stdio};

pub(crate) fn cmd_tool(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev tool check|bootstrap|version ...");
    }
    let command = args.remove(0);
    if command == "version" {
        if args.len() != 1 {
            bail!("usage: vo-dev tool version <tool>");
        }
        let tools = load_toolchains(root)?;
        println!("{}", desired_tool_version(&tools, &args[0])?);
        return Ok(());
    }
    let mut task_name = None;
    let mut json = false;
    let mut apply = false;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--task" => {
                i += 1;
                task_name = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--task requires a task name"))?
                        .clone(),
                );
            }
            "--json" => json = true,
            "--apply" => apply = true,
            other => bail!("unknown tool check argument: {other}"),
        }
        i += 1;
    }
    let tools = load_toolchains(root)?;
    let task_scoped = task_name.is_some();
    let names = if let Some(task_name) = task_name {
        selector_tools_recursive(root, &task_name)?
            .into_iter()
            .collect()
    } else {
        tools.tools.keys().cloned().collect()
    };
    match command.as_str() {
        "check" => {
            if apply {
                bail!("vo-dev tool check does not accept --apply");
            }
            let statuses = check_tools_with_policy(&tools, names, task_scoped)?;
            let has_missing = statuses.iter().any(|status| !status.ok);
            if json {
                println!("{}", serde_json::to_string_pretty(&statuses)?);
            } else {
                for status in &statuses {
                    println!(
                        "{}\t{}",
                        status.name,
                        if status.ok {
                            "ok"
                        } else {
                            status.message.as_str()
                        }
                    );
                }
            }
            if has_missing {
                bail!("one or more tools are missing or have the wrong version");
            }
            Ok(())
        }
        "bootstrap" => bootstrap_tools(&tools, names, apply, json, task_scoped),
        other => bail!("unknown tool command: {other}"),
    }
}

#[derive(Debug, Serialize)]
pub(crate) struct ToolStatus {
    pub(crate) name: String,
    pub(crate) ok: bool,
    pub(crate) message: String,
}

#[derive(Debug, Serialize)]
struct BootstrapStep {
    tool: String,
    current: String,
    ready: bool,
    action: String,
    command: Option<Vec<String>>,
}

fn bootstrap_tools(
    toolchains: &ToolchainFile,
    names: BTreeSet<String>,
    apply: bool,
    json: bool,
    enforce_optional: bool,
) -> Result<()> {
    let statuses = check_tools_with_policy(toolchains, names, enforce_optional)?;
    let mut steps = Vec::new();
    for status in statuses {
        let Some(tool) = toolchains.tools.get(&status.name) else {
            steps.push(BootstrapStep {
                tool: status.name,
                current: status.message,
                ready: false,
                action: "declare this tool in eng/toolchains.toml".to_string(),
                command: None,
            });
            continue;
        };
        steps.push(bootstrap_step(&status, tool));
    }

    if json {
        println!("{}", serde_json::to_string_pretty(&steps)?);
    } else {
        for step in &steps {
            println!(
                "{}\t{}\t{}",
                step.tool,
                if step.ready { "ready" } else { "needed" },
                step.action
            );
            if let Some(command) = &step.command {
                println!("  {}", command.join(" "));
            }
        }
    }

    if apply {
        for step in &steps {
            if step.ready {
                continue;
            }
            let Some(command) = &step.command else {
                bail!(
                    "cannot apply bootstrap automatically for {}: {}",
                    step.tool,
                    step.action
                );
            };
            println!("==> {}", command.join(" "));
            let status = Command::new(&command[0]).args(&command[1..]).status()?;
            if !status.success() {
                bail!("bootstrap command failed for {}", step.tool);
            }
        }
    }
    if steps.iter().any(|step| !step.ready) && !apply {
        bail!("tool bootstrap plan contains pending tools");
    }
    Ok(())
}

fn bootstrap_step(status: &ToolStatus, tool: &Tool) -> BootstrapStep {
    if status.ok {
        return BootstrapStep {
            tool: status.name.clone(),
            current: status.message.clone(),
            ready: true,
            action: "already satisfies eng/toolchains.toml".to_string(),
            command: None,
        };
    }
    match status.name.as_str() {
        _ if tool.bootstrap.is_some() => BootstrapStep {
            tool: status.name.clone(),
            current: status.message.clone(),
            ready: false,
            action: format!(
                "run bootstrap command for {} from eng/toolchains.toml",
                status.name
            ),
            command: tool.bootstrap.clone(),
        },
        "wasm-pack" => BootstrapStep {
            tool: status.name.clone(),
            current: status.message.clone(),
            ready: false,
            action: format!(
                "install wasm-pack {}",
                tool.version.as_deref().unwrap_or("from eng/toolchains.toml")
            ),
            command: tool.version.as_ref().map(|version| {
                vec![
                    "cargo".to_string(),
                    "install".to_string(),
                    "wasm-pack".to_string(),
                    "--locked".to_string(),
                    "--version".to_string(),
                    version.clone(),
                ]
            }),
        },
        "node" => BootstrapStep {
            tool: status.name.clone(),
            current: status.message.clone(),
            ready: false,
            action: format!(
                "install or activate Node {}; supported examples: fnm install {}, volta install node@{}, mise use node@{}",
                node_tool_requirement(tool),
                node_tool_bootstrap_version(tool),
                node_tool_bootstrap_version(tool),
                node_tool_bootstrap_version(tool)
            ),
            command: None,
        },
        "rust" => BootstrapStep {
            tool: status.name.clone(),
            current: status.message.clone(),
            ready: false,
            action: "install the toolchain declared in rust-toolchain.toml".to_string(),
            command: Some(vec!["rustup".to_string(), "show".to_string()]),
        },
        "npm" => BootstrapStep {
            tool: status.name.clone(),
            current: status.message.clone(),
            ready: false,
            action: "npm is provided by the declared Node toolchain".to_string(),
            command: None,
        },
        _ => BootstrapStep {
            tool: status.name.clone(),
            current: status.message.clone(),
            ready: false,
            action: tool
                .usage
                .clone()
                .or_else(|| tool.source.clone())
                .unwrap_or_else(|| "install according to eng/toolchains.toml".to_string()),
            command: None,
        },
    }
}

pub(crate) fn check_tools(
    toolchains: &ToolchainFile,
    names: BTreeSet<String>,
) -> Result<Vec<ToolStatus>> {
    check_tools_with_policy(toolchains, names, true)
}

fn check_tools_with_policy(
    toolchains: &ToolchainFile,
    names: BTreeSet<String>,
    enforce_optional: bool,
) -> Result<Vec<ToolStatus>> {
    let mut statuses = Vec::new();
    for name in names {
        let Some(tool) = toolchains.tools.get(&name) else {
            statuses.push(ToolStatus {
                name,
                ok: false,
                message: "not declared in eng/toolchains.toml".to_string(),
            });
            continue;
        };
        if name == "rust" {
            statuses.push(validate_tool_status(
                &name,
                tool,
                run_check("rust", &["cargo", "--version"]),
            ));
            continue;
        }
        if name == "vo-dev" {
            statuses.push(ToolStatus {
                name,
                ok: true,
                message: "current binary".to_string(),
            });
            continue;
        }
        if let Some(check) = &tool.check {
            if check.is_empty() {
                statuses.push(ToolStatus {
                    name,
                    ok: false,
                    message: "empty check command".to_string(),
                });
            } else {
                let args: Vec<&str> = check.iter().map(String::as_str).collect();
                statuses.push(validate_tool_status(&name, tool, run_check(&name, &args)));
            }
        } else {
            statuses.push(ToolStatus {
                name,
                ok: true,
                message: "no check command".to_string(),
            });
        }
        if !enforce_optional {
            if let Some(status) = statuses.last_mut() {
                if !status.ok && tool.required == Some(false) {
                    status.ok = true;
                    status.message = format!("optional tool unavailable: {}", status.message);
                }
            }
        }
    }
    Ok(statuses)
}

fn validate_tool_status(name: &str, tool: &Tool, mut status: ToolStatus) -> ToolStatus {
    if !status.ok {
        return with_tool_repair_hint(name, tool, status);
    }
    if let Some(version) = &tool.version {
        let ok = if name == "node" {
            status.message.trim().starts_with(&format!("v{version}."))
                || status.message.trim() == format!("v{version}")
        } else {
            status.message.contains(version)
        };
        if !ok {
            status.ok = false;
            status.message = format!(
                "expected version {}, got {}",
                version,
                status.message.trim()
            );
        }
    }
    if let Some(minimum) = &tool.minimum {
        if let (Some(actual), Some(required)) = (
            parse_major_minor(&status.message),
            parse_major_minor(minimum),
        ) {
            if actual < required {
                status.ok = false;
                status.message = format!("expected >= {}, got {}", minimum, status.message.trim());
            }
        }
    }
    with_tool_repair_hint(name, tool, status)
}

fn with_tool_repair_hint(name: &str, tool: &Tool, mut status: ToolStatus) -> ToolStatus {
    if status.ok || status.message.contains("Repair:") {
        return status;
    }
    let Some(hint) = tool_repair_hint(name, tool) else {
        return status;
    };
    status.message = format!("{}. Repair: {hint}", status.message.trim_end_matches('.'));
    status
}

fn tool_repair_hint(name: &str, tool: &Tool) -> Option<String> {
    if let Some(command) = &tool.bootstrap {
        return Some(format!("run {}", command.join(" ")));
    }
    match name {
        "node" => Some(format!(
            "install or activate Node {}; examples: fnm install {}, volta install node@{}, mise use node@{}",
            node_tool_requirement(tool),
            node_tool_bootstrap_version(tool),
            node_tool_bootstrap_version(tool),
            node_tool_bootstrap_version(tool)
        )),
        "npm" => Some(
            "activate the Node toolchain declared in eng/toolchains.toml; npm is provided by Node"
                .to_string(),
        ),
        "rust" => Some("install the toolchain declared in rust-toolchain.toml".to_string()),
        "wasm-pack" => Some(format!(
            "install wasm-pack {}",
            tool.version
                .as_deref()
                .unwrap_or("from eng/toolchains.toml")
        )),
        _ => tool
            .usage
            .clone()
            .or_else(|| tool.source.clone())
            .map(|text| format!("install according to eng/toolchains.toml ({text})")),
    }
}

fn node_tool_requirement(tool: &Tool) -> String {
    if let Some(version) = &tool.version {
        version.clone()
    } else if let Some(minimum) = &tool.minimum {
        format!(">= {minimum}")
    } else {
        "from eng/toolchains.toml".to_string()
    }
}

fn node_tool_bootstrap_version(tool: &Tool) -> String {
    tool.version
        .as_deref()
        .or(tool.minimum.as_deref())
        .unwrap_or("<version>")
        .to_string()
}

pub(crate) fn desired_tool_version(toolchains: &ToolchainFile, name: &str) -> Result<String> {
    desired_tool_version_inner(toolchains, name, &mut Vec::new())
}

fn desired_tool_version_inner(
    toolchains: &ToolchainFile,
    name: &str,
    stack: &mut Vec<String>,
) -> Result<String> {
    if stack.iter().any(|item| item == name) {
        stack.push(name.to_string());
        bail!("tool version_from cycle: {}", stack.join(" -> "));
    }
    stack.push(name.to_string());
    let tool = toolchains
        .tools
        .get(name)
        .ok_or_else(|| anyhow!("unknown tool in eng/toolchains.toml: {name}"))?;
    let result = if let Some(source) = &tool.version_from {
        desired_tool_version_inner(toolchains, source, stack)
    } else {
        match name {
            "node" => Ok(node_tool_bootstrap_version(tool)),
            _ => tool
                .version
                .clone()
                .or_else(|| tool.minimum.clone())
                .ok_or_else(|| {
                    anyhow!("tool {name} has no version or minimum in eng/toolchains.toml")
                }),
        }
    };
    stack.pop();
    result
}

fn parse_major_minor(value: &str) -> Option<(u64, u64)> {
    let mut nums = Vec::new();
    let mut current = String::new();
    for ch in value.chars() {
        if ch.is_ascii_digit() {
            current.push(ch);
        } else if !current.is_empty() {
            nums.push(current.parse().ok()?);
            current.clear();
            if nums.len() == 2 {
                return Some((nums[0], nums[1]));
            }
        }
    }
    if !current.is_empty() {
        nums.push(current.parse().ok()?);
    }
    if nums.len() >= 2 {
        Some((nums[0], nums[1]))
    } else {
        None
    }
}

fn run_check(name: &str, check: &[&str]) -> ToolStatus {
    let output = Command::new(check[0])
        .args(&check[1..])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output();
    match output {
        Ok(output) if output.status.success() => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            ToolStatus {
                name: name.to_string(),
                ok: true,
                message: or_else_nonempty(stdout.trim(), stderr.trim()).to_string(),
            }
        }
        Ok(output) => ToolStatus {
            name: name.to_string(),
            ok: false,
            message: format!("check exited with {}", output.status),
        },
        Err(err) => ToolStatus {
            name: name.to_string(),
            ok: false,
            message: err.to_string(),
        },
    }
}

fn or_else_nonempty<'a>(value: &'a str, fallback: &'a str) -> &'a str {
    if value.is_empty() {
        fallback
    } else {
        value
    }
}
