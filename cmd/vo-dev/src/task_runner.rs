use crate::config::{load_tasks, load_toolchains, Task};
use crate::task_graph::{task_map, task_tools_recursive};
use crate::tool_lint::lint_toolchain_file;
use crate::tool_system::check_tools;
use anyhow::{anyhow, bail, Context, Result};
use std::fs;
use std::io;
#[cfg(unix)]
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Child, Command, ExitStatus};
use std::time::{Duration, Instant};

#[cfg(unix)]
const SIGKILL: i32 = 9;

#[cfg(unix)]
unsafe extern "C" {
    fn kill(pid: i32, sig: i32) -> i32;
    fn setpgid(pid: i32, pgid: i32) -> i32;
}

pub(crate) fn run_tasks(root: &Path, task_names: &[String]) -> Result<()> {
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
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
