//! Tauri app commands for Vibe Studio.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;
use tauri::{AppHandle, Emitter, Manager};
use vogui::{VoguiPlatform, set_platform};
use vo_vox::gui::{run_gui, send_gui_event, store_guest_handle, with_guest_handle};
use vo_engine::compile_from_memory;

// =============================================================================
// TauriPlatform — native timers via AppHandle::emit + background threads
// =============================================================================

struct TauriPlatform {
    app_handle: AppHandle,
    timers: Arc<Mutex<HashMap<i32, Arc<AtomicBool>>>>,
}

impl TauriPlatform {
    fn new(app_handle: AppHandle) -> Self {
        Self { app_handle, timers: Arc::new(Mutex::new(HashMap::new())) }
    }

    fn cancel(&self, id: i32) {
        if let Some(flag) = self.timers.lock().unwrap().remove(&id) {
            flag.store(true, Ordering::Relaxed);
        }
    }
}

impl VoguiPlatform for TauriPlatform {
    fn start_timeout(&self, id: i32, ms: i32) {
        let handle = self.app_handle.clone();
        let flag = Arc::new(AtomicBool::new(false));
        self.timers.lock().unwrap().insert(id, flag.clone());
        let ms = ms.max(0) as u64;
        thread::spawn(move || {
            thread::sleep(Duration::from_millis(ms));
            if !flag.load(Ordering::Relaxed) {
                let _ = handle.emit("vo-timer", id);
            }
        });
    }

    fn clear_timeout(&self, id: i32) { self.cancel(id); }

    fn start_interval(&self, id: i32, ms: i32) {
        let handle = self.app_handle.clone();
        let flag = Arc::new(AtomicBool::new(false));
        self.timers.lock().unwrap().insert(id, flag.clone());
        let ms = ms.max(1) as u64;
        thread::spawn(move || {
            loop {
                thread::sleep(Duration::from_millis(ms));
                if flag.load(Ordering::Relaxed) { break; }
                if handle.emit("vo-timer", id).is_err() { break; }
            }
        });
    }

    fn clear_interval(&self, id: i32) { self.cancel(id); }

    fn navigate(&self, path: &str) {
        let _ = self.app_handle.emit("vo-navigate", path.to_string());
    }

    fn get_current_path(&self) -> String { "/".to_string() }
}

// =============================================================================
// IDE host VM singleton
// =============================================================================

static HOST_HANDLE_ID: Mutex<Option<i64>> = Mutex::new(None);

fn do_init_ide() -> Result<String, String> {
    let fs = studio_core::build_native_fs();
    let output = compile_from_memory(fs, std::path::Path::new("studio"))
        .map_err(|e| format!("IDE compile error: {}", e))?;
    let (json, handle) = run_gui(output)?;
    let id = store_guest_handle(handle);
    *HOST_HANDLE_ID.lock().unwrap() = Some(id);
    Ok(json)
}

fn do_handle_ide_event(handler_id: i32, payload: &str) -> Result<String, String> {
    let id = HOST_HANDLE_ID.lock().unwrap().ok_or("IDE not initialized")?;
    with_guest_handle(id, |handle| send_gui_event(handle, handler_id, payload))
        .ok_or_else(|| "IDE handle not found".to_string())?
}

// =============================================================================
// Tauri commands
// =============================================================================

#[tauri::command]
fn init_ide() -> Result<String, String> {
    do_init_ide()
}

#[tauri::command]
fn handle_ide_event(handler_id: i32, payload: String) -> Result<String, String> {
    do_handle_ide_event(handler_id, &payload)
}

pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_shell::init())
        .setup(|app| {
            set_platform(Box::new(TauriPlatform::new(app.handle().clone())));
            Ok(())
        })
        .invoke_handler(tauri::generate_handler![init_ide, handle_ide_event])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
