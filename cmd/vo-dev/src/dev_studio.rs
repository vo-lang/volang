use crate::config::load_tasks;
use crate::dev_common::walk_files;
use crate::glob::path_matches;
use anyhow::{anyhow, bail, Context, Result};
use std::fs;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

const STUDIO_PORT: &str = "5174";

pub(crate) fn cmd_studio(root: &Path, args: Vec<String>, native: bool) -> Result<()> {
    let mut build_wasm = false;
    let mut build_only = false;
    let mut runner = false;
    let mut project = None;
    for arg in args {
        match arg.as_str() {
            "--build-wasm" => build_wasm = true,
            "--build-only" if !native => build_only = true,
            "--runner" => runner = true,
            other if other.starts_with('-') => bail!("unknown studio argument: {other}"),
            other => {
                if project.is_some() {
                    bail!("multiple studio projects provided");
                }
                project = Some(other.to_string());
            }
        }
    }

    if build_wasm || build_only || studio_wasm_needs_build(root)? {
        build_studio_wasm(root)?;
    } else {
        println!("studio WASM up-to-date");
    }

    if build_only {
        return Ok(());
    }

    if native {
        run_studio_native(root, project.as_deref(), runner)
    } else {
        ensure_vo_web_wasm_built(root)?;
        run_studio_web(root, project.as_deref(), runner)
    }
}

pub(crate) fn cmd_studio_stop(root: &Path) -> Result<()> {
    let output = Command::new("lsof")
        .args(["-nP", &format!("-iTCP:{STUDIO_PORT}"), "-sTCP:LISTEN", "-t"])
        .current_dir(root)
        .output()
        .context("could not run lsof")?;
    let pids: Vec<_> = String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(ToOwned::to_owned)
        .collect();
    if pids.is_empty() {
        println!("No Studio dev server is listening on port {STUDIO_PORT}.");
        return Ok(());
    }

    let studio_vite = root.join("apps/studio/node_modules/.bin/vite");
    let mut stopped = 0;
    for pid in pids {
        let ps = match Command::new("ps")
            .args(["-p", &pid, "-o", "command="])
            .current_dir(root)
            .output()
        {
            Ok(output) => output,
            Err(err) => {
                println!(
                    "Refusing to stop uninspectable process on {STUDIO_PORT}: pid={pid} ({err})"
                );
                continue;
            }
        };
        let command = String::from_utf8_lossy(&ps.stdout).trim().to_string();
        if !command.contains(&studio_vite.to_string_lossy().to_string()) {
            println!("Refusing to stop non-Studio process on {STUDIO_PORT}: pid={pid} {command}");
            continue;
        }
        let _ = Command::new("kill").arg(&pid).current_dir(root).status();
        println!("Stopped Studio dev server: pid={pid}");
        stopped += 1;
    }
    if stopped == 0 {
        bail!("no Studio dev server was stopped");
    }
    Ok(())
}

fn run_studio_web(root: &Path, project: Option<&str>, runner: bool) -> Result<()> {
    println!("\nStarting Vibe Studio...");
    println!("Press Ctrl+C to stop\n");
    let mut command = Command::new("npm");
    command
        .args(["run", "dev"])
        .current_dir(root.join("apps/studio"));
    configure_studio_env(&mut command, root, project, runner, false)?;
    let _ = command
        .status()
        .context("could not start Studio dev server")?;
    Ok(())
}

fn run_studio_native(root: &Path, project: Option<&str>, runner: bool) -> Result<()> {
    println!("\nStarting Vibe Studio (native)...");
    println!("Press Ctrl+C to stop\n");
    let debug_log = root.join(".tmp/studio-native-debug.log");
    fs::create_dir_all(debug_log.parent().unwrap())?;
    fs::write(&debug_log, "")?;
    let reuse_existing = studio_dev_server_available();
    if reuse_existing {
        println!("Reusing Studio dev server: http://localhost:{STUDIO_PORT}");
    }
    println!("Native debug log: {}", debug_log.display());

    let mut command = Command::new("npm");
    if reuse_existing {
        let override_json = format!(
            "{{\"build\":{{\"beforeDevCommand\":null,\"devUrl\":\"http://localhost:{STUDIO_PORT}\"}}}}"
        );
        command.args(["run", "tauri", "--", "dev", "--config", &override_json]);
    } else {
        command.args(["run", "tauri", "dev"]);
    }
    command.current_dir(root.join("apps/studio"));
    command.env("VIBE_STUDIO_DEBUG_LOG", debug_log);
    command.env("VIBE_STUDIO_WORKSPACE", root);
    configure_studio_env(&mut command, root, project, runner, true)?;
    let _ = command.status().context("could not start native Studio")?;
    Ok(())
}

fn configure_studio_env(
    command: &mut Command,
    root: &Path,
    project: Option<&str>,
    runner: bool,
    native: bool,
) -> Result<()> {
    let prefix = if native { "VIBE" } else { "VITE" };
    if let Some(project) = project {
        command.env(format!("{prefix}_STUDIO_PROJ"), project);
    }
    if project.is_some() || runner {
        command.env(
            format!("{prefix}_STUDIO_MODE"),
            if runner { "runner" } else { "dev" },
        );
    }
    if !native && project.is_some_and(is_local_studio_project) {
        command.env("VITE_STUDIO_LOCAL_PROJECTS", "1");
        command.env("VITE_STUDIO_LOCAL_PROJECT_MAX_BYTES", "512MiB");
        command.env("VITE_STUDIO_LOCAL_PROJECT_MAX_FILES", "20000");
    }
    command.env("VOLANG_ROOT", root);
    Ok(())
}

fn is_local_studio_project(project: &str) -> bool {
    let raw = project.strip_prefix("file://").unwrap_or(project);
    raw.starts_with('/') || Path::new(raw).exists()
}

fn ensure_vo_web_wasm_built(root: &Path) -> Result<()> {
    if task_needs_build(root, "vo-web-wasm-build")? {
        println!("Building vo-web WASM...");
        crate::task_runner::run_tasks(root, &[String::from("vo-web-wasm-build")])?;
    }
    Ok(())
}

fn task_needs_build(root: &Path, task_name: &str) -> Result<bool> {
    let tasks = load_tasks(root)?;
    let task = tasks
        .tasks
        .iter()
        .find(|task| task.name == task_name)
        .ok_or_else(|| anyhow!("unknown build task: {task_name}"))?;
    if task.outputs.is_empty() {
        bail!("build task {task_name} must declare outputs");
    }

    let Some(oldest_output) = oldest_output_mtime(root, &task.outputs)? else {
        return Ok(true);
    };
    Ok(newest_input_mtime(root, &task.inputs)? > oldest_output)
}

fn oldest_output_mtime(root: &Path, outputs: &[String]) -> Result<Option<f64>> {
    let mut oldest: Option<f64> = None;
    for output in outputs {
        let path = root.join(output);
        if !path.exists() {
            return Ok(None);
        }
        let Some(output_mtime) = output_mtime(&path)? else {
            return Ok(None);
        };
        oldest = Some(oldest.map_or(output_mtime, |current| f64::min(current, output_mtime)));
    }
    Ok(oldest)
}

fn output_mtime(path: &Path) -> Result<Option<f64>> {
    if path.is_file() {
        return Ok(Some(mtime(path)?));
    }
    let files = walk_files(path)?;
    if files.is_empty() {
        return Ok(None);
    }
    let mut newest = 0.0;
    for file in files {
        newest = f64::max(newest, mtime(&file)?);
    }
    Ok(Some(newest))
}

fn newest_input_mtime(root: &Path, inputs: &[String]) -> Result<f64> {
    let mut newest = 0.0;
    let repo_files = walk_source_files(root)?;
    for input in inputs {
        if has_glob_meta(input) {
            for file in &repo_files {
                let rel = file
                    .strip_prefix(root)
                    .unwrap_or(file)
                    .to_string_lossy()
                    .replace('\\', "/");
                if path_matches(&rel, input) {
                    newest = f64::max(newest, mtime(file)?);
                }
            }
            continue;
        }

        let path = root.join(input);
        if path.exists() {
            newest = f64::max(newest, newest_mtime(&path, "*")?);
        }
    }
    Ok(newest)
}

fn has_glob_meta(value: &str) -> bool {
    value.contains('*') || value.contains('?') || value.contains('[') || value.contains(']')
}

fn studio_wasm_needs_build(root: &Path) -> Result<bool> {
    if task_needs_build(root, "studio-wasm-build")? {
        return Ok(true);
    }

    let wasm = root.join("apps/studio/public/wasm/vo_studio_wasm_bg.wasm");
    let build_id = root.join("apps/studio/public/wasm/vo_studio_wasm.build_id");
    let id = fs::read_to_string(&build_id).unwrap_or_default();
    if id.trim().is_empty()
        || !fs::read(&wasm)?
            .windows(id.trim().len())
            .any(|w| w == id.trim().as_bytes())
    {
        return Ok(true);
    }
    Ok(false)
}

fn build_studio_wasm(root: &Path) -> Result<()> {
    println!("Building vo-studio WASM...");
    crate::task_runner::run_tasks(root, &[String::from("studio-wasm-build")])?;
    let wasm = root.join("apps/studio/public/wasm/vo_studio_wasm_bg.wasm");
    if wasm.exists() {
        println!(
            "vo-studio: {:.1} KB",
            wasm.metadata()?.len() as f64 / 1024.0
        );
    }
    Ok(())
}

fn studio_dev_server_available() -> bool {
    let Ok(mut stream) = TcpStream::connect(("127.0.0.1", STUDIO_PORT.parse::<u16>().unwrap()))
    else {
        return false;
    };
    let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
    let request =
        format!("GET / HTTP/1.1\r\nHost: localhost:{STUDIO_PORT}\r\nConnection: close\r\n\r\n");
    if stream.write_all(request.as_bytes()).is_err() {
        return false;
    }
    let mut body = String::new();
    if stream.read_to_string(&mut body).is_err() {
        return false;
    }
    body.contains("<title>Studio</title>") && body.contains("id=\"app\"")
}

fn walk_source_files(root: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    if !root.exists() {
        return Ok(files);
    }
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        for entry in
            fs::read_dir(&dir).with_context(|| format!("could not read {}", dir.display()))?
        {
            let path = entry?.path();
            let name = path
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or_default();
            if path.is_dir() {
                if matches!(
                    name,
                    ".git" | ".tmp" | "target" | "node_modules" | "dist" | "pkg"
                ) {
                    continue;
                }
                stack.push(path);
            } else if path.is_file() {
                files.push(path);
            }
        }
    }
    Ok(files)
}

fn newest_mtime(root: &Path, _pattern: &str) -> Result<f64> {
    let mut newest = 0.0;
    for path in walk_files(root)? {
        newest = f64::max(newest, mtime(&path)?);
    }
    Ok(newest)
}

fn mtime(path: &Path) -> Result<f64> {
    Ok(path
        .metadata()?
        .modified()?
        .duration_since(std::time::UNIX_EPOCH)?
        .as_secs_f64())
}
