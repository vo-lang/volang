use crate::commands::pathing::{resolve_run_target, resolve_target, ResolvedTarget};
use crate::state::AppState;
use std::path::PathBuf;
use vo_app_runtime::take_captured_stdout;
use vo_engine::{
    check_with_auto_install, compile_with_auto_install, format_text, run_with_output,
    run_with_output_interruptible, CaptureSink, CompileError, CompileOutput, RunError, RunMode,
    RuntimeErrorKind,
};

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DiagnosticError {
    pub file: String,
    pub line: u32,
    pub column: u32,
    pub message: String,
    pub category: String,
    pub module_stage: Option<String>,
    pub module_kind: Option<String>,
    pub module_path: Option<String>,
    pub module_version: Option<String>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CheckResult {
    pub ok: bool,
    pub errors: Vec<DiagnosticError>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompileResult {
    pub ok: bool,
    pub errors: Vec<DiagnosticError>,
    pub output_path: Option<String>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BuildResult {
    pub ok: bool,
    pub errors: Vec<DiagnosticError>,
    pub output_path: Option<String>,
}

#[derive(serde::Serialize, serde::Deserialize, Clone)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum RunEvent {
    Stdout { text: String },
    Stderr { text: String },
    Stopped,
    Done {
        #[serde(rename = "exitCode")]
        exit_code: i32,
        #[serde(rename = "durationMs")]
        duration_ms: u64,
    },
    Error { message: String },
}

pub(crate) fn prepare_and_compile(target: &str) -> Result<CompileOutput, CompileError> {
    compile_with_auto_install(target)
}

fn diagnostic_from_compile_error(default_file: &str, error: &CompileError) -> DiagnosticError {
    let module_error = error.module_system();
    DiagnosticError {
        file: module_error
            .and_then(|module_error| module_error.path().map(str::to_string))
            .unwrap_or_else(|| default_file.to_string()),
        line: 0,
        column: 0,
        message: module_error
            .map(|module_error| module_error.detail().to_string())
            .unwrap_or_else(|| error.to_string()),
        category: error.category().to_string(),
        module_stage: module_error.map(|module_error| module_error.stage().as_str().to_string()),
        module_kind: module_error.map(|module_error| module_error.kind().as_str().to_string()),
        module_path: module_error.and_then(|module_error| module_error.module_path().map(str::to_string)),
        module_version: module_error.and_then(|module_error| module_error.version().map(str::to_string)),
    }
}

fn diagnostic_from_message(file: &str, category: &str, message: String) -> DiagnosticError {
    DiagnosticError {
        file: file.to_string(),
        line: 0,
        column: 0,
        message,
        category: category.to_string(),
        module_stage: None,
        module_kind: None,
        module_path: None,
        module_version: None,
    }
}

fn resolve_command_target(
    state: &tauri::State<'_, AppState>,
    path: &str,
) -> Result<ResolvedTarget, String> {
    resolve_target(
        &state.session_root(),
        state.workspace_root(),
        path,
        state.single_file_run(),
    )
}

fn default_output_path(target: &ResolvedTarget) -> PathBuf {
    target.output_base_path.with_extension("vob")
}

#[tauri::command]
pub fn cmd_check_vo(path: String, state: tauri::State<'_, AppState>) -> Result<CheckResult, String> {
    let target = resolve_command_target(&state, &path)?;
    let compile_str = target.compile_path.to_string_lossy().to_string();
    match check_with_auto_install(&compile_str) {
        Ok(_) => Ok(CheckResult { ok: true, errors: vec![] }),
        Err(error) => Ok(CheckResult {
            ok: false,
            errors: vec![diagnostic_from_compile_error(&compile_str, &error)],
        }),
    }
}

#[tauri::command]
pub fn cmd_compile_vo(path: String, state: tauri::State<'_, AppState>) -> Result<CompileResult, String> {
    let target = resolve_command_target(&state, &path)?;
    let compile_str = target.compile_path.to_string_lossy().to_string();
    match prepare_and_compile(&compile_str) {
        Ok(output) => {
            let output_path = default_output_path(&target);
            if let Err(error) = std::fs::write(&output_path, output.module.serialize()) {
                return Ok(CompileResult {
                    ok: false,
                    errors: vec![diagnostic_from_message(
                        &output_path.to_string_lossy(),
                        "io",
                        format!("failed to write output: {}", error),
                    )],
                    output_path: None,
                });
            }
            Ok(CompileResult { ok: true, errors: vec![], output_path: Some(output_path.to_string_lossy().to_string()) })
        }
        Err(error) => Ok(CompileResult {
            ok: false,
            errors: vec![diagnostic_from_compile_error(&compile_str, &error)],
            output_path: None,
        }),
    }
}

#[tauri::command]
pub fn cmd_format_vo(_path: String, _state: tauri::State<'_, AppState>) -> Result<String, String> {
    Err("vo format is not yet implemented".to_string())
}

#[tauri::command]
pub fn cmd_build_vo(path: String, output: Option<String>, state: tauri::State<'_, AppState>) -> Result<BuildResult, String> {
    let target = resolve_command_target(&state, &path)?;
    let compile_str = target.compile_path.to_string_lossy().to_string();
    match prepare_and_compile(&compile_str) {
        Ok(compiled) => {
            let output_path = output.map(std::path::PathBuf::from).unwrap_or_else(|| default_output_path(&target));
            if let Err(error) = std::fs::write(&output_path, compiled.module.serialize()) {
                return Ok(BuildResult {
                    ok: false,
                    errors: vec![diagnostic_from_message(
                        &output_path.to_string_lossy(),
                        "io",
                        format!("failed to write output: {}", error),
                    )],
                    output_path: None,
                });
            }
            Ok(BuildResult { ok: true, errors: vec![], output_path: Some(output_path.to_string_lossy().to_string()) })
        }
        Err(error) => Ok(BuildResult {
            ok: false,
            errors: vec![diagnostic_from_compile_error(&compile_str, &error)],
            output_path: None,
        }),
    }
}

#[tauri::command]
pub fn cmd_dump_vo(path: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    let target = resolve_command_target(&state, &path)?;
    let output = prepare_and_compile(&target.compile_path.to_string_lossy())
        .map_err(|error| error.to_string())?;
    Ok(format_text(&output.module))
}

#[tauri::command]
pub fn cmd_run_vo(path: String, run_mode: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    let run_target = resolve_run_target(&state.session_root(), state.workspace_root(), &path, state.single_file_run())?;
    let compiled = prepare_and_compile(&run_target.compile_path.to_string_lossy())
        .map_err(|error| error.to_string())?;
    let sink = CaptureSink::new();
    let result = run_with_output(compiled, parse_run_mode(&run_mode)?, Vec::new(), sink.clone());
    let captured = take_captured_stdout(sink.as_ref()).unwrap_or_default();
    match result {
        Ok(()) => Ok(captured),
        Err(err) => {
            if captured.trim().is_empty() {
                Err(err.to_string())
            } else {
                Err(format!("{}\nRuntime error: {}", captured.trim_end(), err))
            }
        }
    }
}

#[tauri::command]
pub async fn cmd_run_vo_stream(
    path: String,
    run_mode: Option<String>,
    state: tauri::State<'_, AppState>,
    on_event: tauri::ipc::Channel<RunEvent>,
) -> Result<(), String> {
    let run_target = resolve_run_target(&state.session_root(), state.workspace_root(), &path, state.single_file_run())?;
    let compile_path: PathBuf = run_target.compile_path;
    let run_mode_str = run_mode.as_deref().unwrap_or("vm").to_string();
    let run_handle = state.begin_console_run();
    std::thread::spawn(move || {
        let compile_str = compile_path.to_string_lossy().to_string();
        let start = std::time::Instant::now();
        let interrupt_flag = run_handle.interrupt_flag();
        if interrupt_flag.load(std::sync::atomic::Ordering::SeqCst) {
            let _ = on_event.send(RunEvent::Stopped);
            run_handle.clear_current();
            return;
        }
        let compiled = match prepare_and_compile(&compile_str) {
            Ok(c) => c,
            Err(err) => {
                let _ = on_event.send(RunEvent::Stderr { text: err.to_string() });
                let _ = on_event.send(RunEvent::Done { exit_code: 1, duration_ms: start.elapsed().as_millis() as u64 });
                run_handle.clear_current();
                return;
            }
        };
        if interrupt_flag.load(std::sync::atomic::Ordering::SeqCst) {
            let _ = on_event.send(RunEvent::Stopped);
            run_handle.clear_current();
            return;
        }
        let sink = CaptureSink::new();
        let mode = match run_mode_str.as_str() {
            "jit" => RunMode::Jit,
            _ => RunMode::Vm,
        };
        let result = run_with_output_interruptible(compiled, mode, Vec::new(), sink.clone(), Some(interrupt_flag));
        if let Some(captured) = take_captured_stdout(sink.as_ref()) {
            let _ = on_event.send(RunEvent::Stdout { text: captured });
        }
        let duration_ms = start.elapsed().as_millis() as u64;
        match result {
            Ok(()) => {
                let _ = on_event.send(RunEvent::Done { exit_code: 0, duration_ms });
            }
            Err(RunError::Runtime(runtime_error)) if runtime_error.kind == RuntimeErrorKind::Interrupted => {
                let _ = on_event.send(RunEvent::Stopped);
            }
            Err(err) => {
                let _ = on_event.send(RunEvent::Stderr { text: err.to_string() });
                let _ = on_event.send(RunEvent::Done { exit_code: 1, duration_ms });
            }
        }
        run_handle.clear_current();
    });
    Ok(())
}

#[tauri::command]
pub fn cmd_stop_vo_run(state: tauri::State<'_, AppState>) -> Result<(), String> {
    state.stop_console_run();
    Ok(())
}

fn parse_run_mode(run_mode: &str) -> Result<RunMode, String> {
    match run_mode {
        "vm" => Ok(RunMode::Vm),
        "jit" => Ok(RunMode::Jit),
        other => Err(format!("Unsupported run mode: {}", other)),
    }
}
