use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};

#[derive(serde::Serialize, serde::Deserialize, Clone)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum ProcEvent {
    Stdout {
        text: String,
    },
    Stderr {
        text: String,
    },
    Done {
        #[serde(rename = "exitCode")]
        exit_code: i32,
    },
    Error {
        message: String,
    },
}

#[tauri::command]
pub async fn cmd_spawn_process(
    program: String,
    args: Vec<String>,
    cwd: Option<String>,
    env: Option<std::collections::HashMap<String, String>>,
    on_event: tauri::ipc::Channel<ProcEvent>,
) -> Result<(), String> {
    std::thread::spawn(move || {
        let mut cmd = Command::new(&program);
        cmd.args(&args);
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        if let Some(cwd) = cwd {
            cmd.current_dir(cwd);
        }
        if let Some(env_map) = env {
            for (key, value) in env_map {
                cmd.env(key, value);
            }
        }
        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(err) => {
                let _ = on_event.send(ProcEvent::Error {
                    message: format!("{}: {}", program, err),
                });
                return;
            }
        };
        let stdout = child.stdout.take().unwrap();
        let stderr = child.stderr.take().unwrap();
        let on_event_out = on_event.clone();
        let on_event_err = on_event.clone();
        let stdout_thread = std::thread::spawn(move || {
            for line in BufReader::new(stdout).lines().map_while(Result::ok) {
                let _ = on_event_out.send(ProcEvent::Stdout { text: line });
            }
        });
        let stderr_thread = std::thread::spawn(move || {
            for line in BufReader::new(stderr).lines().map_while(Result::ok) {
                let _ = on_event_err.send(ProcEvent::Stderr { text: line });
            }
        });
        let _ = stdout_thread.join();
        let _ = stderr_thread.join();
        let exit_code = child.wait().map(|s| s.code().unwrap_or(0)).unwrap_or(1);
        let _ = on_event.send(ProcEvent::Done { exit_code });
    });
    Ok(())
}
