use crate::commands::pathing::{resolve_run_target, resolve_target, ResolvedTarget};
use crate::state::AppState;
use std::path::PathBuf;
use vo_app_runtime::take_captured_stdout;
use vo_engine::{
    compile_with_auto_install_prepared_with_options, format_text, run_with_output_interruptible,
    CaptureSink, CompileError, CompileOutput, PreparedCompileOutput, RunError, RunMode,
    RuntimeErrorKind,
};
use vo_module::project::ProjectContextOptions;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum RunEvent {
    Stdout {
        text: String,
    },
    Stderr {
        text: String,
    },
    Stopped,
    Done {
        #[serde(rename = "exitCode")]
        exit_code: i32,
        #[serde(rename = "durationMs")]
        duration_ms: u64,
    },
    Error {
        message: String,
    },
}

pub(crate) fn prepare_and_compile(
    target: &str,
    options: &ProjectContextOptions,
) -> Result<CompileOutput, CompileError> {
    prepare_and_compile_prepared(target, options)
        .and_then(PreparedCompileOutput::into_validated_output)
}

pub(crate) fn prepare_and_compile_prepared(
    target: &str,
    options: &ProjectContextOptions,
) -> Result<PreparedCompileOutput, CompileError> {
    compile_with_auto_install_prepared_with_options(target, options)
}

fn resolve_command_target(
    state: &tauri::State<'_, AppState>,
    path: &str,
) -> Result<(ResolvedTarget, ProjectContextOptions), String> {
    let session = state.session_snapshot();
    let target = resolve_target(
        session.root(),
        state.workspace_root(),
        path,
        session.single_file_run(),
    )?;
    Ok((target, session.project_context_options()))
}

#[tauri::command]
pub fn cmd_dump_vo(path: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    let (target, options) = resolve_command_target(&state, &path)?;
    let output = prepare_and_compile(&target.compile_path.to_string_lossy(), &options)
        .map_err(|error| error.to_string())?;
    Ok(format_text(&output.module))
}

#[tauri::command]
pub async fn cmd_run_vo_stream(
    path: String,
    run_mode: Option<String>,
    state: tauri::State<'_, AppState>,
    on_event: tauri::ipc::Channel<RunEvent>,
) -> Result<(), String> {
    let session = state.session_snapshot();
    let run_target = resolve_run_target(
        session.root(),
        state.workspace_root(),
        &path,
        session.single_file_run(),
    )?;
    let compile_path: PathBuf = run_target.compile_path;
    let run_mode_str = run_mode.as_deref().unwrap_or("vm").to_string();
    let run_handle = state.begin_console_run();
    let options = session.project_context_options();
    std::thread::spawn(move || {
        let compile_str = compile_path.to_string_lossy().to_string();
        let start = std::time::Instant::now();
        let interrupt_flag = run_handle.interrupt_flag();
        if interrupt_flag.load(std::sync::atomic::Ordering::SeqCst) {
            let _ = on_event.send(RunEvent::Stopped);
            run_handle.clear_current();
            return;
        }
        let compiled = match prepare_and_compile(&compile_str, &options) {
            Ok(c) => c,
            Err(err) => {
                let _ = on_event.send(RunEvent::Stderr {
                    text: err.to_string(),
                });
                let _ = on_event.send(RunEvent::Done {
                    exit_code: 1,
                    duration_ms: start.elapsed().as_millis() as u64,
                });
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
        let result = run_with_output_interruptible(
            compiled,
            mode,
            Vec::new(),
            sink.clone(),
            Some(interrupt_flag),
        );
        if let Some(captured) = take_captured_stdout(sink.as_ref()) {
            let _ = on_event.send(RunEvent::Stdout { text: captured });
        }
        let duration_ms = start.elapsed().as_millis() as u64;
        match result {
            Ok(()) => {
                let _ = on_event.send(RunEvent::Done {
                    exit_code: 0,
                    duration_ms,
                });
            }
            Err(RunError::Runtime(runtime_error))
                if runtime_error.kind == RuntimeErrorKind::Interrupted =>
            {
                let _ = on_event.send(RunEvent::Stopped);
            }
            Err(RunError::Exited(code)) => {
                let _ = on_event.send(RunEvent::Done {
                    exit_code: code,
                    duration_ms,
                });
            }
            Err(err) => {
                let _ = on_event.send(RunEvent::Stderr {
                    text: err.to_string(),
                });
                let _ = on_event.send(RunEvent::Done {
                    exit_code: 1,
                    duration_ms,
                });
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
