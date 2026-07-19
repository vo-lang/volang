use anyhow::{bail, Context, Result};
use std::fs;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

const STUDIO_PORT: &str = "5174";

pub(crate) fn cmd_studio(root: &Path, args: Vec<String>, native: bool) -> Result<()> {
    let mut runner = false;
    let mut project = None;
    for arg in args {
        match arg.as_str() {
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

    if native {
        run_studio_native(root, project.as_deref(), runner)
    } else {
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
    println!("\nStarting Vo Studio...");
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
    println!("\nStarting Vo Studio (native)...");
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
    configure_studio_native_host_env(&mut command, root, &debug_log);
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
    let prefix = if native { "STUDIO" } else { "VITE_STUDIO" };
    if let Some(project) = project {
        command.env(format!("{prefix}_PROJ"), project);
    }
    if project.is_some() || runner {
        command.env(
            format!("{prefix}_MODE"),
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

fn configure_studio_native_host_env(command: &mut Command, root: &Path, debug_log: &Path) {
    command.env("STUDIO_DEBUG_LOG", debug_log);
    command.env("STUDIO_WORKSPACE", root);
}

fn is_local_studio_project(project: &str) -> bool {
    let raw = project.strip_prefix("file://").unwrap_or(project);
    raw.starts_with('/') || Path::new(raw).exists()
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

#[cfg(test)]
mod tests {
    use super::{configure_studio_env, configure_studio_native_host_env};
    use std::ffi::OsStr;
    use std::path::Path;
    use std::process::Command;

    fn env_value<'a>(command: &'a Command, name: &str) -> Option<&'a OsStr> {
        command
            .get_envs()
            .find_map(|(key, value)| (key == name).then_some(value).flatten())
    }

    #[test]
    fn native_studio_uses_only_canonical_host_environment_names() {
        let root = Path::new("/tmp/volang-studio-test");
        let debug_log = root.join("studio.log");
        let mut command = Command::new("studio-test");
        configure_studio_native_host_env(&mut command, root, &debug_log);
        configure_studio_env(&mut command, root, Some("file:///tmp/example"), true, true)
            .expect("configure native Studio environment");

        assert_eq!(
            env_value(&command, "STUDIO_DEBUG_LOG"),
            Some(debug_log.as_os_str())
        );
        assert_eq!(
            env_value(&command, "STUDIO_WORKSPACE"),
            Some(root.as_os_str())
        );
        assert_eq!(
            env_value(&command, "STUDIO_PROJ"),
            Some(OsStr::new("file:///tmp/example"))
        );
        assert_eq!(
            env_value(&command, "STUDIO_MODE"),
            Some(OsStr::new("runner"))
        );
        assert!(command
            .get_envs()
            .all(|(key, _)| !key.to_string_lossy().starts_with("VIBE_STUDIO_")));
    }

    #[test]
    fn web_studio_keeps_the_vite_environment_boundary() {
        let root = Path::new("/tmp/volang-studio-test");
        let mut command = Command::new("studio-test");
        configure_studio_env(
            &mut command,
            root,
            Some("file:///tmp/example"),
            false,
            false,
        )
        .expect("configure web Studio environment");

        assert_eq!(
            env_value(&command, "VITE_STUDIO_PROJ"),
            Some(OsStr::new("file:///tmp/example"))
        );
        assert_eq!(
            env_value(&command, "VITE_STUDIO_MODE"),
            Some(OsStr::new("dev"))
        );
        assert!(env_value(&command, "STUDIO_PROJ").is_none());
        assert!(env_value(&command, "STUDIO_MODE").is_none());
    }
}
