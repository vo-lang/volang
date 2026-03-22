use std::io::Write;
use std::sync::Arc;

use tauri::{AppHandle, Emitter};
use vo_app_runtime::{NativeGuestHandle, NativeGuiEventLoopConfig, SyncRenderBuffer, spawn_native_gui};
use vo_engine::CompileOutput;

pub type GuestHandle = NativeGuestHandle;

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

pub fn run_gui(output: CompileOutput, app: AppHandle) -> Result<(Vec<u8>, GuestHandle, Arc<SyncRenderBuffer>), String> {
    debug_log(&format!(
        "[studio-native] compile extensions: {:?}",
        output
            .extensions
            .iter()
            .map(|m| format!("{} => {}", m.name, m.native_path.display()))
            .collect::<Vec<_>>()
    ));
    let config = NativeGuiEventLoopConfig {
        island_sink: Some({
            let app = app.clone();
            Box::new(move |bytes| {
                app.emit("island_data", bytes)
                    .map_err(|e| format!("failed to emit island_data: {}", e))
            })
        }),
        capabilities: vec!["external_island_host".to_string()],
        on_stdout: Some(Box::new(|label, text| {
            debug_log(&format!("[guest-stdout][{}] {}", label, text));
        })),
        on_error: Some(Box::new(|msg| {
            eprintln!("{}", msg);
        })),
    };
    spawn_native_gui(move || vo_engine::build_gui_vm(output), config)
}
