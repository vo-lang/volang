//! Tauri commands for Vibe Studio (Svelte-native frontend).
//!
//! The IDE UI is a Svelte app; this backend provides:
//! - Filesystem commands (scoped to a workspace root)
//! - Compile & run user Vo code via vo-engine / vox

use std::path::{Component, Path, PathBuf};
use std::sync::Mutex;
use serde::Serialize;
use vo_vox::gui::GuestHandle;
use vo_vox::{compile, run as run_vox, RunMode};
use vo_runtime::output;

// =============================================================================
// AppState
// =============================================================================

pub struct AppState {
    workspace_root: PathBuf,
    guest: Mutex<Option<GuestHandle>>,
}

fn default_workspace() -> PathBuf {
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".vibe-studio")
        .join("workspace")
}

// =============================================================================
// FS types
// =============================================================================

#[derive(Serialize)]
struct FsEntry {
    name: String,
    path: String,
    #[serde(rename = "isDir")]
    is_dir: bool,
}

// =============================================================================
// FS commands
// =============================================================================

#[tauri::command]
fn cmd_get_workspace_root(state: tauri::State<'_, AppState>) -> String {
    state.workspace_root.to_string_lossy().to_string()
}

#[tauri::command]
fn cmd_fs_list_dir(dir_path: String, state: tauri::State<'_, AppState>) -> Result<Vec<FsEntry>, String> {
    let abs = resolve_path(&state.workspace_root, &dir_path)?;
    let rd = std::fs::read_dir(&abs).map_err(|e| format!("{}: {}", abs.display(), e))?;

    let mut entries: Vec<FsEntry> = Vec::new();
    for item in rd.flatten() {
        let name = item.file_name().to_string_lossy().to_string();
        let is_dir = item.file_type().map(|t| t.is_dir()).unwrap_or(false);
        let child_path = Path::new(&dir_path).join(&name);
        entries.push(FsEntry {
            name,
            path: child_path.to_string_lossy().to_string(),
            is_dir,
        });
    }
    entries.sort_by(|a, b| {
        if a.is_dir != b.is_dir {
            return if a.is_dir { std::cmp::Ordering::Less } else { std::cmp::Ordering::Greater };
        }
        a.name.cmp(&b.name)
    });
    Ok(entries)
}

#[tauri::command]
fn cmd_fs_read_file(path: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    let abs = resolve_path(&state.workspace_root, &path)?;
    std::fs::read_to_string(&abs).map_err(|e| format!("{}: {}", abs.display(), e))
}

#[tauri::command]
fn cmd_fs_write_file(path: String, content: String, state: tauri::State<'_, AppState>) -> Result<(), String> {
    let abs = resolve_path(&state.workspace_root, &path)?;
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
    }
    std::fs::write(&abs, &content).map_err(|e| format!("{}: {}", abs.display(), e))
}

#[tauri::command]
fn cmd_fs_mkdir(path: String, state: tauri::State<'_, AppState>) -> Result<(), String> {
    let abs = resolve_path(&state.workspace_root, &path)?;
    std::fs::create_dir_all(&abs).map_err(|e| format!("{}: {}", abs.display(), e))
}

#[tauri::command]
fn cmd_fs_rename(old_path: String, new_path: String, state: tauri::State<'_, AppState>) -> Result<(), String> {
    let abs_old = resolve_path(&state.workspace_root, &old_path)?;
    let abs_new = resolve_path(&state.workspace_root, &new_path)?;
    std::fs::rename(&abs_old, &abs_new).map_err(|e| e.to_string())
}

#[tauri::command]
fn cmd_fs_remove(path: String, recursive: bool, state: tauri::State<'_, AppState>) -> Result<(), String> {
    let abs = resolve_path(&state.workspace_root, &path)?;
    if recursive {
        std::fs::remove_dir_all(&abs).map_err(|e| e.to_string())
    } else if abs.is_dir() {
        std::fs::remove_dir(&abs).map_err(|e| e.to_string())
    } else {
        std::fs::remove_file(&abs).map_err(|e| e.to_string())
    }
}

/// Resolve a path to an absolute path within the workspace, preventing escape.
/// Accepts both absolute paths (must be under root) and relative paths (joined to root).
fn resolve_path(root: &Path, path: &str) -> Result<PathBuf, String> {
    let canonical_root = root
        .canonicalize()
        .map_err(|e| format!("{}: {}", root.display(), e))?;

    let input = Path::new(path);
    let rel = if input.is_absolute() {
        input
            .strip_prefix(&canonical_root)
            .map_err(|_| format!("path escapes workspace: {}", path))?
    } else {
        input
    };

    let mut normalized_rel = PathBuf::new();
    for comp in rel.components() {
        match comp {
            Component::CurDir => {}
            Component::Normal(seg) => normalized_rel.push(seg),
            Component::ParentDir => {
                if !normalized_rel.pop() {
                    return Err(format!("path escapes workspace: {}", path));
                }
            }
            Component::RootDir | Component::Prefix(_) => {
                return Err(format!("path escapes workspace: {}", path));
            }
        }
    }

    let abs = canonical_root.join(&normalized_rel);

    // Reject symlink-based escapes by validating the closest existing ancestor.
    let mut probe = abs.as_path();
    while !probe.exists() {
        probe = probe
            .parent()
            .ok_or_else(|| format!("path escapes workspace: {}", path))?;
    }
    let canonical_probe = probe
        .canonicalize()
        .map_err(|e| format!("{}: {}", probe.display(), e))?;
    if !canonical_probe.starts_with(&canonical_root) {
        return Err(format!("path escapes workspace: {}", path));
    }

    Ok(abs)
}

// =============================================================================
// Execution commands
// =============================================================================

/// Compile and run user code from an entry path, returning captured stdout.
#[tauri::command]
fn cmd_compile_run(entry_path: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    let abs = resolve_path(&state.workspace_root, &entry_path)?;
    let abs_str = abs.to_string_lossy().to_string();

    let compile_output = compile(&abs_str).map_err(|e| e.to_string())?;
    output::start_capture();
    let result = run_vox(compile_output, RunMode::Vm, Vec::new());
    let captured = output::stop_capture();
    match result {
        Ok(()) => Ok(captured),
        Err(e) => {
            if captured.is_empty() {
                Err(e.to_string())
            } else {
                Err(format!("{}\nRuntime error: {}", captured.trim_end(), e))
            }
        }
    }
}

/// Compile user GUI code from entry path, start a guest VM thread, return initial render JSON.
#[tauri::command]
fn cmd_run_gui(entry_path: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    let abs = resolve_path(&state.workspace_root, &entry_path)?;
    let abs_str = abs.to_string_lossy().to_string();

    let _ = state.guest.lock().unwrap().take();

    let compile_output = compile(&abs_str).map_err(|e| e.to_string())?;
    let (initial_json, handle) = vo_vox::gui::run_gui(compile_output)?;
    *state.guest.lock().unwrap() = Some(handle);
    Ok(initial_json)
}

/// Send an event to the running guest VM and return the new render JSON.
#[tauri::command]
fn cmd_send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let mut guard = state.guest.lock().unwrap();
    let handle = guard.as_mut().ok_or_else(|| "No guest VM running".to_string())?;
    vo_vox::gui::send_gui_event(handle, handler_id, &payload)
}

/// Stop the running guest VM.
#[tauri::command]
fn cmd_stop_gui(state: tauri::State<'_, AppState>) -> Result<(), String> {
    let _ = state.guest.lock().unwrap().take();
    Ok(())
}

// =============================================================================
// App entry
// =============================================================================

pub fn run() {
    let workspace = default_workspace();
    std::fs::create_dir_all(&workspace).ok();

    // Seed a default main.vo if workspace is empty
    let main_dir = workspace.join("main");
    let main_file = main_dir.join("main.vo");
    if !main_file.exists() {
        std::fs::create_dir_all(&main_dir).ok();
        std::fs::write(&main_file, "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, Vo!\")\n}\n").ok();
    }

    tauri::Builder::default()
        .plugin(tauri_plugin_shell::init())
        .manage(AppState {
            workspace_root: workspace,
            guest: Mutex::new(None),
        })
        .invoke_handler(tauri::generate_handler![
            cmd_get_workspace_root,
            cmd_fs_list_dir,
            cmd_fs_read_file,
            cmd_fs_write_file,
            cmd_fs_mkdir,
            cmd_fs_rename,
            cmd_fs_remove,
            cmd_compile_run,
            cmd_run_gui,
            cmd_send_gui_event,
            cmd_stop_gui,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Vibe Studio");
}
