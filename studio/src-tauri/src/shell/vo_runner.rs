use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::thread;

use serde_json::{json, Value};
use vo_vox::{compile, run as run_vox, RunMode};
use vo_runtime::output;

use super::{ShellRequest, ShellResponse, ShellError};

fn emit(app: &tauri::AppHandle, payload: Value) {
    use tauri::Emitter;
    app.emit("shell-event", payload).ok();
}

// =============================================================================
// Global serialization lock — output::start_capture / stop_capture are global
// state, so concurrent Vo executions must be serialized.
// =============================================================================

static VO_EXEC_LOCK: Mutex<()> = Mutex::new(());

// =============================================================================
// VoRunner — compiles the Vo shell handler and runs it per request
// =============================================================================

pub struct VoRunner {
    /// Absolute path to studio/vo/shell/ directory containing the Vo handler.
    handler_dir:    PathBuf,
    workspace_root: PathBuf,
}

impl VoRunner {
    pub fn new(workspace_root: PathBuf) -> Self {
        // Derive handler path relative to the executable's workspace root.
        // studio/vo/shell/ lives two levels above the src-tauri/ directory.
        // At runtime the workspace_root is the project root (where d.py lives).
        let handler_dir = workspace_root.join("studio").join("vo").join("shell");
        Self { handler_dir, workspace_root }
    }

    pub fn handle(&self, req: ShellRequest, app: &tauri::AppHandle) -> ShellResponse {
        match self.run_handler(&req) {
            Ok(resp_val) => self.interpret(resp_val, app),
            Err(e)       => ShellResponse::Error {
                id:      req.id,
                code:    e.code,
                message: e.message,
            },
        }
    }

    // ── Execute the Vo handler, return raw JSON value ─────────────────────────

    fn run_handler(&self, req: &ShellRequest) -> Result<Value, ShellError> {
        let handler_path = self.handler_dir.to_string_lossy().to_string();

        let req_json = serde_json::to_string(&json!({
            "id":  req.id,
            "cwd": req.cwd,
            "op":  req.op,
        })).map_err(|e| ShellError::internal(&e.to_string()))?;

        let workspace_str = self.workspace_root.to_string_lossy().to_string();

        // Compile the handler directory on every request.
        // The Vo compiler is fast for small programs; caching can be added later.
        let compiled = compile(&handler_path)
            .map_err(|e| ShellError::internal(&format!("shell handler compile error: {e}")))?;

        // Serialize Vo execution — output capture is process-global state.
        let _guard = VO_EXEC_LOCK.lock().unwrap();

        output::start_capture();
        let run_result = run_vox(compiled, RunMode::Vm, vec![req_json, workspace_str]);
        let captured = output::stop_capture();
        drop(_guard);

        if let Err(e) = run_result {
            // If the handler panicked but produced partial output, use it;
            // otherwise surface the runtime error.
            if captured.trim().is_empty() {
                return Err(ShellError::internal(&format!("shell handler runtime error: {e:?}")));
            }
        }

        serde_json::from_str(captured.trim())
            .map_err(|e| ShellError::internal(&format!("handler output is not valid JSON: {e}\noutput: {captured}")))
    }

    // ── Interpret the JSON response from the Vo handler ───────────────────────

    fn interpret(&self, val: Value, app: &tauri::AppHandle) -> ShellResponse {
        let id   = val["id"].as_str().unwrap_or("").to_string();
        let kind = val["kind"].as_str().unwrap_or("error");

        match kind {
            "ok" => ShellResponse::Ok {
                id,
                data: val["data"].clone(),
            },

            "stream" => {
                let job_id = val["jobId"].as_str().unwrap_or("").to_string();
                match self.start_stream(job_id.clone(), &val["streamCmd"], app) {
                    Ok(()) => ShellResponse::Stream { id, job_id },
                    Err(e) => ShellResponse::Error  { id, code: e.code, message: e.message },
                }
            }

            "error" => ShellResponse::Error {
                id,
                code:    val["code"].as_str().unwrap_or("ERR_INTERNAL").to_string(),
                message: val["message"].as_str().unwrap_or("unknown error").to_string(),
            },

            _ => ShellResponse::Error {
                id,
                code:    "ERR_INTERNAL".into(),
                message: format!("unexpected handler response kind: {kind}"),
            },
        }
    }

    // ── Spawn a streaming subprocess from a streamCmd object ─────────────────
    //
    // streamCmd shape (produced by the Vo handler):
    //   { program, args: [string], dir, env: null | {k:v}, combined: bool }

    fn start_stream(&self, job_id: String, cmd_val: &Value, app: &tauri::AppHandle) -> Result<(), ShellError> {
        let program = cmd_val["program"].as_str()
            .ok_or_else(|| ShellError::internal("streamCmd missing program"))?
            .to_string();

        let args: Vec<String> = cmd_val["args"].as_array()
            .map(|a| a.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect())
            .unwrap_or_default();

        // Skip argv[0] (program name duplicate) when it equals the program.
        let spawn_args: Vec<String> = if args.first().map(|s| s.as_str()) == Some(&program) {
            args[1..].to_vec()
        } else {
            args
        };

        let dir = cmd_val["dir"].as_str().unwrap_or("").to_string();
        let combined = cmd_val["combined"].as_bool().unwrap_or(false);

        let work_dir: PathBuf = if dir.is_empty() {
            self.workspace_root.clone()
        } else if Path::new(&dir).is_absolute() {
            PathBuf::from(&dir)
        } else {
            self.workspace_root.join(&dir)
        };

        let mut cmd = Command::new(&program);
        cmd.args(&spawn_args).current_dir(&work_dir);

        if combined {
            cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
        } else {
            cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
        }

        // Optional env overrides
        if let Some(env_obj) = cmd_val["env"].as_object() {
            for (k, v) in env_obj {
                cmd.env(k, v.as_str().unwrap_or(""));
            }
        }

        let mut child = cmd.spawn().map_err(|e| {
            ShellError::tool_missing(&format!("failed to spawn '{}': {}", program, e))
        })?;

        let stdout = child.stdout.take();
        let stderr = child.stderr.take();

        let jid_out = job_id.clone();
        let jid_err = job_id.clone();
        let jid_fin = job_id.clone();
        let app_out = app.clone();
        let app_err = app.clone();
        let app_fin = app.clone();

        let out_thread = thread::spawn(move || {
            if let Some(out) = stdout {
                for line in BufReader::new(out).lines().map_while(Result::ok) {
                    emit(&app_out, json!({ "jobId": jid_out, "kind": "stdout", "line": line }));
                }
            }
        });

        let err_thread = thread::spawn(move || {
            if let Some(err) = stderr {
                for line in BufReader::new(err).lines().map_while(Result::ok) {
                    emit(&app_err, json!({ "jobId": jid_err, "kind": "stderr", "line": line }));
                }
            }
        });

        thread::spawn(move || {
            out_thread.join().ok();
            err_thread.join().ok();
            match child.wait() {
                Ok(status) => emit(&app_fin, json!({
                    "jobId":    jid_fin,
                    "kind":     "done",
                    "exitCode": status.code().unwrap_or(-1),
                })),
                Err(e) => emit(&app_fin, json!({
                    "jobId":   jid_fin,
                    "kind":    "fail",
                    "code":    "ERR_INTERNAL",
                    "message": e.to_string(),
                })),
            }
        });

        Ok(())
    }
}
