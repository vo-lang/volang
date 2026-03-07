//! Tauri commands for Vibe Studio (Svelte-native frontend).
//!
//! The IDE UI is a Svelte app; this backend provides:
//! - Compile & run user Vo code via vo-engine / vox
//! - Unified shell API via ShellRouter (shell/mod.rs)
//!
//! All filesystem operations are routed through `cmd_shell_exec` → Vo shell handler.

mod shell;

use std::path::{Component, Path, PathBuf};
use std::sync::Mutex;
use vo_vox::gui::GuestHandle;
use vo_vox::{compile, run_with_output as run_vox, RunMode};
use vo_runtime::output::CaptureSink;

// =============================================================================
// AppState
// =============================================================================

pub struct AppState {
    pub workspace_root: PathBuf,
    guest:        Mutex<Option<GuestHandle>>,
    shell_runner: shell::VoRunner,
}

fn default_workspace() -> PathBuf {
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".vibe-studio")
        .join("workspace")
}

// =============================================================================
// Workspace root command
// =============================================================================

#[tauri::command]
fn cmd_get_workspace_root(state: tauri::State<'_, AppState>) -> String {
    state.workspace_root.to_string_lossy().to_string()
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
fn cmd_compile_run(entry_path: String, state: tauri::State<'_, AppState>) -> Result<String, shell::StudioError> {
    let abs = resolve_path(&state.workspace_root, &entry_path)
        .map_err(|e| shell::StudioError::access_denied(&e))?;
    let abs_str = abs.to_string_lossy().to_string();

    let compile_output = compile(&abs_str)
        .map_err(|e| shell::StudioError::vo_compile(&e.to_string()))?;
    let sink = CaptureSink::new();
    let result = run_vox(compile_output, RunMode::Vm, Vec::new(), sink.clone());
    let captured = sink.take();
    match result {
        Ok(()) => Ok(captured),
        Err(e) => {
            let msg = if captured.is_empty() {
                e.to_string()
            } else {
                format!("{}\nRuntime error: {}", captured.trim_end(), e)
            };
            Err(shell::StudioError::vo_runtime(&msg))
        }
    }
}

/// Compile user GUI code from entry path, start a guest VM thread, return initial render bytes.
#[tauri::command]
fn cmd_run_gui(entry_path: String, state: tauri::State<'_, AppState>) -> Result<Vec<u8>, shell::StudioError> {
    let abs = resolve_path(&state.workspace_root, &entry_path)
        .map_err(|e| shell::StudioError::access_denied(&e))?;
    let abs_str = abs.to_string_lossy().to_string();

    let _ = state.guest.lock().unwrap().take();

    let compile_output = compile(&abs_str)
        .map_err(|e| shell::StudioError::vo_compile(&e.to_string()))?;
    let (initial_bytes, handle) = vo_vox::gui::run_gui(compile_output)
        .map_err(|e| shell::StudioError::vo_runtime(&e))?;
    *state.guest.lock().unwrap() = Some(handle);
    Ok(initial_bytes)
}

/// Send an event to the running guest VM and return the new render bytes.
#[tauri::command]
fn cmd_send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<u8>, shell::StudioError> {
    let mut guard = state.guest.lock().unwrap();
    let handle = guard.as_mut()
        .ok_or_else(|| shell::StudioError::vo_runtime("No guest VM running"))?;
    vo_vox::gui::send_gui_event(handle, handler_id, &payload)
        .map_err(|e| shell::StudioError::vo_runtime(&e))
}

/// Stop the running guest VM.
#[tauri::command]
fn cmd_stop_gui(state: tauri::State<'_, AppState>) -> Result<(), shell::StudioError> {
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
            shell_runner:  shell::VoRunner::new(workspace.clone()),
            workspace_root: workspace,
            guest:         Mutex::new(None),
        })
        .invoke_handler(tauri::generate_handler![
            cmd_get_workspace_root,
            cmd_compile_run,
            cmd_run_gui,
            cmd_send_gui_event,
            cmd_stop_gui,
            shell::cmd_shell_init,
            shell::cmd_shell_exec,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Vibe Studio");
}
