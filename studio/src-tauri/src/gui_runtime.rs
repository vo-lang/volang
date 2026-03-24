use std::io::Write;
use std::sync::Arc;

use tauri::{AppHandle, Emitter};
use vo_app_runtime::{NativeGuestHandle, NativeGuiEventLoopConfig, SyncRenderBuffer, spawn_native_gui};
use vo_engine::{with_compile_log_sink, CompileOutput};

pub type GuestHandle = NativeGuestHandle;

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct GuiLogEvent {
    session_id: u64,
    source: String,
    message: String,
}

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct GuiFatalErrorEvent {
    session_id: u64,
    message: String,
}

pub(crate) fn debug_log(message: &str) {
    eprintln!("{message}");
    let log_path = ["STUDIO_DEBUG_LOG", "VIBE_STUDIO_DEBUG_LOG"]
        .iter()
        .find_map(|name| std::env::var(name).ok())
        .filter(|path| !path.trim().is_empty());
    if let Some(path) = log_path {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
        {
            let _ = writeln!(file, "{message}");
        }
    }
}

pub(crate) fn emit_gui_log(app: &AppHandle, session_id: u64, source: &str, message: &str) {
    let _ = app.emit("gui_log", GuiLogEvent {
        session_id,
        source: source.to_string(),
        message: message.to_string(),
    });
}

pub(crate) fn make_gui_log_sink(app: AppHandle, session_id: u64) -> impl Fn(&str, &str) + Send + Sync + 'static {
    move |source, message| {
        emit_gui_log(&app, session_id, source, message);
    }
}

pub fn run_gui(output: CompileOutput, app: AppHandle, session_id: u64) -> Result<(Vec<u8>, GuestHandle, Arc<SyncRenderBuffer>), String> {
    let prepare_extensions_message = if output.extensions.is_empty() {
        "prepare gui extensions none".to_string()
    } else {
        format!(
            "prepare gui extensions {}",
            output
                .extensions
                .iter()
                .map(|m| m.name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    emit_gui_log(&app, session_id, "studio-native", &prepare_extensions_message);
    debug_log(&format!("[studio-native] {}", prepare_extensions_message));
    let error_app = app.clone();
    let config = NativeGuiEventLoopConfig {
        island_sink: Some({
            let app = app.clone();
            Box::new(move |bytes| {
                app.emit("island_data", bytes)
                    .map_err(|e| format!("failed to emit island_data: {}", e))
            })
        }),
        capabilities: vec!["external_island_host".to_string()],
        on_stdout: Some(Box::new({
            let app = app.clone();
            move |label, text| {
                emit_gui_log(&app, session_id, "guest", text);
                debug_log(&format!("[guest-stdout][{}] {}", label, text));
            }
        })),
        on_error: Some(Box::new(move |msg| {
            eprintln!("{}", msg);
            let _ = error_app.emit("gui_fatal_error", GuiFatalErrorEvent {
                session_id,
                message: msg.to_string(),
            });
        })),
    };
    let build_app = app.clone();
    spawn_native_gui(
        move || {
            with_compile_log_sink(make_gui_log_sink(build_app.clone(), session_id), || vo_engine::build_gui_vm(output))
        },
        config,
    )
}
