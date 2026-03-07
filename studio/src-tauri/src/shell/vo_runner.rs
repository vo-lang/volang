use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::thread;

use vo_vox::CompileOutput;

use serde_json::{json, Value};
use vo_vox::{compile, run_with_output as run_vox, RunMode};
use vo_runtime::output::CaptureSink;

use super::{ShellRequest, ShellResponse, ShellError};

fn emit(app: &tauri::AppHandle, payload: Value) {
    use tauri::Emitter;
    app.emit("shell-event", payload).ok();
}

// =============================================================================
// VoRunner — compiles the Vo shell handler and runs it per request
// =============================================================================

pub struct VoRunner {
    /// Absolute path to studio/vo/shell/ directory containing the Vo handler.
    handler_dir:    PathBuf,
    workspace_root: PathBuf,
    /// Cached compiled shell handler bytecode — recompiled only on first use.
    cached:         Mutex<Option<CompileOutput>>,
}

impl VoRunner {
    pub fn new(workspace_root: PathBuf) -> Self {
        let handler_dir = Self::find_handler_dir();
        Self { handler_dir, workspace_root, cached: Mutex::new(None) }
    }

    /// Locate the studio/vo/shell/ handler directory at runtime.
    ///
    /// Strategy (tried in order):
    /// 1. `VIBE_STUDIO_HANDLER_DIR` env var — explicit override.
    /// 2. Relative to the running executable:
    ///    dev layout:  `studio/src-tauri/target/debug/vibe-studio`
    ///                  → exe/../../../..  ==  `studio/`
    ///                  → `studio/vo/shell/`
    /// 3. Relative to the current working directory (`cwd/vo/shell`).
    fn find_handler_dir() -> PathBuf {
        if let Ok(val) = std::env::var("VIBE_STUDIO_HANDLER_DIR") {
            return PathBuf::from(val);
        }

        if let Ok(exe) = std::env::current_exe() {
            // exe  →  debug/  →  target/  →  src-tauri/  →  studio/
            let candidate = exe
                .parent()                     // debug/
                .and_then(|p| p.parent())     // target/
                .and_then(|p| p.parent())     // src-tauri/
                .and_then(|p| p.parent())     // studio/
                .map(|studio| studio.join("vo").join("shell"));

            if let Some(dir) = candidate {
                if dir.exists() {
                    return dir;
                }
            }
        }

        std::env::current_dir()
            .unwrap_or_default()
            .join("vo")
            .join("shell")
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

        let compiled = {
            let mut guard = self.cached.lock().unwrap();
            if let Some(cached) = &*guard {
                cached.clone()
            } else {
                let fresh = compile(&handler_path)
                    .map_err(|e| ShellError::internal(&format!("shell handler compile error: {e}")))?;
                *guard = Some(fresh.clone());
                fresh
            }
        };

        let sink = CaptureSink::new();
        let run_result = run_vox(compiled, RunMode::Vm, vec!["native".into(), req_json, workspace_str], sink.clone());
        let captured = sink.take();

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

        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

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
        // When combined=true, stderr events are emitted as "stdout" (merged stream).
        let stderr_kind = if combined { "stdout" } else { "stderr" };

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
                    emit(&app_err, json!({ "jobId": jid_err, "kind": stderr_kind, "line": line }));
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
