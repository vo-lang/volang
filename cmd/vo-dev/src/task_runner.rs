use crate::config::{load_tasks, load_toolchains, Task};
use crate::task_graph::{resolve_selector, task_map, task_tools_recursive};
use crate::tool_lint::lint_toolchain_file;
use crate::tool_system::check_tools;
use anyhow::{anyhow, bail, Context, Result};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::fs;
use std::io;
#[cfg(unix)]
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Child, Command, ExitStatus};
use std::time::{Duration, Instant};

#[cfg(unix)]
const SIGKILL: i32 = 9;

pub(crate) const VM_PRODUCTION_FINAL_GATE_SELECTORS: &[&str] =
    &["contract", "vm-production", "site", "release-verify"];

#[cfg(unix)]
unsafe extern "C" {
    fn kill(pid: i32, sig: i32) -> i32;
    fn setpgid(pid: i32, pgid: i32) -> i32;
}

pub(crate) fn run_tasks(root: &Path, task_names: &[String]) -> Result<()> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    let gate_plan_env = gate_plan_env(&config)?;
    for name in task_names {
        let task = task_map
            .get(name)
            .ok_or_else(|| anyhow!("unknown task: {name}"))?;
        ensure_task_tools(root, name)?;
        let cwd = root.join(task.cwd.as_deref().unwrap_or("."));
        println!("\n==> {}", task.title);
        println!(
            "{}$ {}",
            cwd.strip_prefix(root).unwrap_or(&cwd).display(),
            task.command.join(" ")
        );
        let start = Instant::now();
        let mut command = Command::new(&task.command[0]);
        command.args(&task.command[1..]).current_dir(&cwd);
        for (key, value) in &task.env {
            command.env(key, value);
        }
        for (key, value) in &gate_plan_env {
            command.env(key, value);
        }
        let status = run_command_with_optional_timeout(&mut command, task.timeout_sec)
            .with_context(|| format!("could not run task {name}"))?;
        let elapsed = start.elapsed().as_secs_f32();
        if !status.success() {
            bail!("task {name} failed after {elapsed:.1}s");
        }
        validate_task_outputs(root, task)
            .with_context(|| format!("task {name} did not produce declared outputs"))?;
        println!("ok: {name} ({elapsed:.1}s)");
    }
    Ok(())
}

fn gate_plan_env(config: &crate::config::TaskFile) -> Result<Vec<(String, String)>> {
    VM_PRODUCTION_FINAL_GATE_SELECTORS
        .iter()
        .copied()
        .map(|selector| {
            let tasks = resolve_selector(config, selector)?;
            Ok((
                task_plan_env_name(selector),
                serde_json::to_string(&tasks)
                    .with_context(|| format!("could not encode {selector} task plan"))?,
            ))
        })
        .collect()
}

fn task_plan_env_name(selector: &str) -> String {
    format!(
        "VO_DEV_EXPECTED_TASKS_{}",
        selector.to_ascii_uppercase().replace('-', "_")
    )
}

#[derive(Debug, Serialize)]
struct TaskRunEvidence<'a> {
    schema: &'static str,
    selector: &'a str,
    changed: bool,
    passed: bool,
    source_state: String,
    tasks: &'a [String],
}

pub(crate) fn write_task_run_evidence(
    root: &Path,
    selector: &str,
    changed: bool,
    task_names: &[String],
) -> Result<()> {
    if changed || !VM_PRODUCTION_FINAL_GATE_SELECTORS.contains(&selector) {
        return Ok(());
    }
    let evidence = TaskRunEvidence {
        schema: "vo-dev-task-run-evidence-v1",
        selector,
        changed,
        passed: true,
        source_state: format!(
            "vm-production-current-source:{}",
            current_vm_production_source_state_hash(root)?
        ),
        tasks: task_names,
    };
    let dir = root.join("lang/docs/dev/vm-production-gate-evidence");
    fs::create_dir_all(&dir)?;
    fs::write(
        dir.join(format!("{selector}.json")),
        serde_json::to_string_pretty(&evidence)?,
    )?;
    Ok(())
}

fn current_vm_production_source_state_hash(root: &Path) -> Result<String> {
    ensure_no_untracked_vm_production_source(root)?;
    let mut hasher = Sha256::new();
    hasher.update(b"vm-production-readiness-source-state-v1\0");
    let mut files = git_z(root, &["ls-files", "--cached", "-z"])?;
    files.retain(|path| {
        path != "lang/docs/dev/vm-production-readiness.md"
            && !path.starts_with("lang/docs/dev/vm-production-gate-evidence/")
    });
    files.sort();
    for rel_path in files {
        update_hash_field(&mut hasher, b"path", rel_path.as_bytes());
        match fs::read(root.join(&rel_path)) {
            Ok(content) => update_hash_field(&mut hasher, b"content", &content),
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                update_hash_field(&mut hasher, b"deleted", b"")
            }
            Err(error) => return Err(error).with_context(|| format!("could not read {rel_path}")),
        }
    }
    Ok(hex_encode(&hasher.finalize()))
}

fn ensure_no_untracked_vm_production_source(root: &Path) -> Result<()> {
    let mut files = git_z(
        root,
        &["status", "--porcelain=v1", "-z", "--untracked-files=all"],
    )?;
    files.retain(|entry| {
        let bytes = entry.as_bytes();
        bytes.len() >= 4
            && bytes[2] == b' '
            && ((bytes[0] == b'?' && bytes[1] == b'?') || (bytes[1] == b'A' && bytes[0] != b'A'))
    });
    let mut files: Vec<String> = files
        .into_iter()
        .map(|entry| entry[3..].to_string())
        .collect();
    files.retain(|path| {
        path != "lang/docs/dev/vm-production-readiness.md"
            && !path.starts_with("lang/docs/dev/vm-production-gate-evidence/")
    });
    files.sort();
    if files.is_empty() {
        return Ok(());
    }
    bail!(
        "vm-production final evidence requires tracked source files; untracked source paths: {}",
        files.join(", ")
    )
}

fn update_hash_field(hasher: &mut Sha256, label: &[u8], value: &[u8]) {
    hasher.update(label);
    hasher.update(b"\0");
    hasher.update(value);
    hasher.update(b"\0");
}

fn git_z(root: &Path, args: &[&str]) -> Result<Vec<String>> {
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
    Ok(output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|entry| !entry.is_empty())
        .map(|entry| String::from_utf8_lossy(entry).into_owned())
        .collect())
}

fn hex_encode(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        out.push(HEX[(byte >> 4) as usize] as char);
        out.push(HEX[(byte & 0x0f) as usize] as char);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn gate_plan_env_includes_resolved_task_dependencies_035() {
        let config: crate::config::TaskFile = toml::from_str(
            r#"
version = 1

[groups]
contract = ["main"]
vm-production = ["main", "extra"]
site = ["site-task"]
release-verify = ["release-task"]

[[task]]
name = "setup"
title = "Setup"
command = ["true"]
tier = "contract"

[[task]]
name = "main"
title = "Main"
command = ["true"]
tier = "contract"
needs = ["setup"]

[[task]]
name = "extra"
title = "Extra"
command = ["true"]
tier = "contract"

[[task]]
name = "site-task"
title = "Site"
command = ["true"]
tier = "site"

[[task]]
name = "release-task"
title = "Release"
command = ["true"]
tier = "release"
"#,
        )
        .expect("task config");

        let env: BTreeMap<_, _> = gate_plan_env(&config)
            .expect("gate plan env")
            .into_iter()
            .collect();

        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_CONTRACT")
                .map(String::as_str),
            Some(r#"["setup","main"]"#),
        );
        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_VM_PRODUCTION")
                .map(String::as_str),
            Some(r#"["setup","main","extra"]"#),
        );
        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_SITE").map(String::as_str),
            Some(r#"["site-task"]"#),
        );
        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_RELEASE_VERIFY")
                .map(String::as_str),
            Some(r#"["release-task"]"#),
        );
    }

    #[test]
    fn vm_production_source_state_rejects_untracked_source_039() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-source-state-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("lang/docs/dev")).expect("docs dir");
        fs::write(root.join("src.vo"), "package main\nfunc main() {}\n").expect("src");
        fs::write(
            root.join("lang/docs/dev/vm-production-readiness.md"),
            "# VM Production Readiness Plan\n",
        )
        .expect("readiness");
        run_git(&root, &["init", "-q"]);
        run_git(&root, &["add", "."]);

        fs::create_dir_all(root.join(".github/workflows")).expect("workflow dir");
        fs::write(
            root.join(".github/workflows/production-readiness.yml"),
            "name: Production Readiness\n",
        )
        .expect("workflow");

        let err = current_vm_production_source_state_hash(&root)
            .expect_err("untracked source must not be accepted as final evidence state");
        let msg = err.to_string();
        assert!(msg.contains("requires tracked source files"), "{msg}");
        assert!(
            msg.contains(".github/workflows/production-readiness.yml"),
            "{msg}"
        );

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn vm_production_source_state_rejects_intent_to_add_source_041() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-source-state-intent-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("lang/docs/dev")).expect("docs dir");
        fs::write(root.join("src.vo"), "package main\nfunc main() {}\n").expect("src");
        fs::write(
            root.join("lang/docs/dev/vm-production-readiness.md"),
            "# VM Production Readiness Plan\n",
        )
        .expect("readiness");
        run_git(&root, &["init", "-q"]);
        run_git(&root, &["add", "."]);

        fs::create_dir_all(root.join("src")).expect("src dir");
        fs::write(
            root.join("src/intent.vo"),
            "package main\nfunc intent() {}\n",
        )
        .expect("intent source");
        run_git(&root, &["add", "-N", "src/intent.vo"]);

        let err = current_vm_production_source_state_hash(&root)
            .expect_err("intent-to-add source must not be accepted as final evidence state");
        let msg = err.to_string();
        assert!(msg.contains("requires tracked source files"), "{msg}");
        assert!(msg.contains("src/intent.vo"), "{msg}");

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn final_signoff_evidence_writes_all_required_selectors_047() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-final-evidence-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("lang/docs/dev")).expect("docs dir");
        fs::write(root.join("src.vo"), "package main\nfunc main() {}\n").expect("src");
        fs::write(
            root.join("lang/docs/dev/vm-production-readiness.md"),
            "# VM Production Readiness Plan\n",
        )
        .expect("readiness");
        run_git(&root, &["init", "-q"]);
        run_git(&root, &["add", "."]);

        for selector in ["contract", "vm-production", "site", "release-verify"] {
            write_task_run_evidence(&root, selector, false, &[format!("{selector}-task")])
                .unwrap_or_else(|error| panic!("{selector} evidence failed: {error}"));
            let evidence = root
                .join("lang/docs/dev/vm-production-gate-evidence")
                .join(format!("{selector}.json"));
            assert!(
                evidence.exists(),
                "{selector} final signoff evidence was not written"
            );
            let source = fs::read_to_string(&evidence).expect("evidence JSON");
            assert!(
                source.contains(&format!(r#""selector": "{selector}""#)),
                "{selector} evidence recorded the wrong selector: {source}"
            );
        }

        fs::remove_dir_all(root).ok();
    }

    fn run_git(root: &Path, args: &[&str]) {
        let status = Command::new("git")
            .args(args)
            .current_dir(root)
            .status()
            .expect("git command");
        assert!(status.success(), "git {args:?} failed");
    }
}

fn validate_task_outputs(root: &Path, task: &Task) -> Result<()> {
    for output in &task.outputs {
        let path = root.join(output);
        if !path.exists() {
            bail!("missing output {}", output);
        }
        if path.is_dir() && fs::read_dir(&path)?.next().is_none() {
            bail!("output directory {} is empty", output);
        }
    }
    Ok(())
}

fn run_command_with_optional_timeout(
    command: &mut Command,
    timeout_sec: Option<u64>,
) -> Result<ExitStatus> {
    #[cfg(unix)]
    unsafe {
        command.pre_exec(|| {
            if setpgid(0, 0) == 0 {
                Ok(())
            } else {
                Err(io::Error::last_os_error())
            }
        });
    }

    let mut child = command.spawn().context("could not start process")?;
    let Some(timeout_sec) = timeout_sec else {
        return child.wait().context("could not wait for process");
    };

    let timeout = Duration::from_secs(timeout_sec);
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait().context("could not poll process")? {
            return Ok(status);
        }
        if start.elapsed() >= timeout {
            kill_process_tree(&mut child);
            bail!("timed out after {timeout_sec}s");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn kill_process_tree(child: &mut Child) {
    #[cfg(unix)]
    unsafe {
        let process_group = -(child.id() as i32);
        if kill(process_group, SIGKILL) == 0 {
            let _ = child.wait();
            return;
        }
    }

    let _ = child.kill();
    let _ = child.wait();
}

fn ensure_task_tools(root: &Path, task_name: &str) -> Result<()> {
    let tools = load_toolchains(root)?;
    lint_toolchain_file(root, &tools)?;
    let statuses = check_tools(&tools, task_tools_recursive(root, task_name)?)?;
    let missing: Vec<_> = statuses
        .into_iter()
        .filter(|status| !status.ok)
        .map(|status| format!("{} ({})", status.name, status.message))
        .collect();
    if !missing.is_empty() {
        bail!("missing task tools for {task_name}: {}", missing.join(", "));
    }
    Ok(())
}
