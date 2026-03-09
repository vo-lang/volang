//! Tauri commands for Vibe Studio (Svelte-native frontend).
//!
//! The IDE UI is a Svelte app; this backend provides:
//! - Compile & run user Vo code via vo-engine / vox
//! - Unified shell API via ShellRouter (shell/mod.rs)
//!
//! All filesystem operations are routed through `cmd_shell_exec` → Vo shell handler.

mod shell;

use std::path::{Component, Path, PathBuf};
use std::sync::{Arc, Mutex};
use vo_vox::gui::{GuestHandle, PushReceiver};
use vo_vox::{compile_with_auto_install, run_with_output as run_vox, RunMode};
use vo_runtime::output::CaptureSink;

// =============================================================================
// AppState
// =============================================================================

pub struct AppState {
    pub workspace_root: PathBuf,
    launch_url:   Option<String>,
    guest:        Mutex<Option<GuestHandle>>,
    /// Platform-driven render receiver — stored separately so polling
    /// doesn't contend with the guest handle lock.
    push_rx:      Mutex<Option<Arc<PushReceiver>>>,
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

#[tauri::command]
fn cmd_get_launch_url(state: tauri::State<'_, AppState>) -> Option<String> {
    state.launch_url.clone()
}

fn detect_launch_url() -> Option<String> {
    if let Ok(value) = std::env::var("VIBE_STUDIO_LAUNCH_URL") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }
    for arg in std::env::args().skip(1) {
        let trimmed = arg.trim();
        if trimmed.contains("://") {
            return Some(trimmed.to_string());
        }
    }
    None
}

fn sanitize_path_component(component: &str) -> String {
    let mut out = String::with_capacity(component.len());
    for ch in component.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '.' | '_' | '-') {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    out
}

fn find_project_root(path: &Path) -> Option<PathBuf> {
    let mut current = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()?.to_path_buf()
    };

    loop {
        if current.join("vo.mod").is_file() {
            return Some(current);
        }
        if !current.pop() {
            return None;
        }
    }
}

fn copy_path_recursive(src: &Path, dst: &Path) -> Result<(), String> {
    let metadata = std::fs::metadata(src)
        .map_err(|e| format!("{}: {}", src.display(), e))?;

    if metadata.is_dir() {
        std::fs::create_dir_all(dst)
            .map_err(|e| format!("{}: {}", dst.display(), e))?;
        for entry in std::fs::read_dir(src)
            .map_err(|e| format!("{}: {}", src.display(), e))?
        {
            let entry = entry.map_err(|e| format!("{}: {}", src.display(), e))?;
            let dst_path = dst.join(entry.file_name());
            copy_path_recursive(&entry.path(), &dst_path)?;
        }
    } else {
        if let Some(parent) = dst.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("{}: {}", parent.display(), e))?;
        }
        std::fs::copy(src, dst)
            .map_err(|e| format!("{} -> {}: {}", src.display(), dst.display(), e))?;
    }

    Ok(())
}

fn remove_existing_path(path: &Path) -> Result<(), String> {
    if !path.exists() {
        return Ok(());
    }

    let metadata = std::fs::symlink_metadata(path)
        .map_err(|e| format!("{}: {}", path.display(), e))?;
    if metadata.is_dir() {
        std::fs::remove_dir_all(path)
            .map_err(|e| format!("{}: {}", path.display(), e))?;
    } else {
        std::fs::remove_file(path)
            .map_err(|e| format!("{}: {}", path.display(), e))?;
    }

    Ok(())
}

fn imported_local_root(workspace_root: &Path, source_root: &Path) -> PathBuf {
    let mut dst = workspace_root.join(".studio").join("local");
    for component in source_root.components() {
        if let Component::Normal(seg) = component {
            dst.push(sanitize_path_component(&seg.to_string_lossy()));
        }
    }
    dst
}

fn materialize_source_root(target_path: &Path) -> PathBuf {
    if let Some(project_root) = find_project_root(target_path) {
        return project_root;
    }
    if target_path.is_file() {
        return target_path.parent().unwrap_or(target_path).to_path_buf();
    }
    target_path.to_path_buf()
}

fn materialize_local_target(workspace_root: &Path, target_path: &Path) -> Result<PathBuf, shell::StudioError> {
    if !target_path.exists() {
        return Err(shell::StudioError::not_found(&format!("launch target not found: {}", target_path.display())));
    }

    let canonical_workspace = workspace_root
        .canonicalize()
        .map_err(|e| shell::StudioError::internal(&format!("{}: {}", workspace_root.display(), e)))?;
    let canonical_target = target_path
        .canonicalize()
        .map_err(|e| shell::StudioError::internal(&format!("{}: {}", target_path.display(), e)))?;

    if canonical_target.starts_with(&canonical_workspace) {
        return Ok(canonical_target);
    }

    let source_root = materialize_source_root(&canonical_target);
    let import_root = imported_local_root(&canonical_workspace, &source_root);

    remove_existing_path(&import_root)
        .map_err(|e| shell::StudioError::internal(&e))?;
    copy_path_recursive(&source_root, &import_root)
        .map_err(|e| shell::StudioError::internal(&e))?;

    if canonical_target == source_root {
        Ok(import_root)
    } else {
        let rel = canonical_target
            .strip_prefix(&source_root)
            .map_err(|e| shell::StudioError::internal(&e.to_string()))?;
        Ok(import_root.join(rel))
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

#[tauri::command]
fn cmd_materialize_local_launch_target(
    target_path: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, shell::StudioError> {
    let materialized = materialize_local_target(&state.workspace_root, Path::new(&target_path))?;
    Ok(materialized.to_string_lossy().to_string())
}

/// For multi-file projects (vo.mod present in parent dir), return the directory
/// path so the compiler reads all .vo files and resolves module dependencies.
/// For single-file entries, return the file path as-is.
fn resolve_compile_path(entry: &Path) -> PathBuf {
    if let Some(project_root) = find_project_root(entry) {
        return project_root;
    }
    entry.to_path_buf()
}

/// Compile and run user code from an entry path, returning captured stdout.
#[tauri::command]
fn cmd_compile_run(entry_path: String, state: tauri::State<'_, AppState>) -> Result<String, shell::StudioError> {
    let abs = resolve_path(&state.workspace_root, &entry_path)
        .map_err(|e| shell::StudioError::access_denied(&e))?;
    let compile_path = resolve_compile_path(&abs);
    let compile_str = compile_path.to_string_lossy().to_string();

    let compile_output = compile_with_auto_install(&compile_str)
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

    // Drop previous guest (sends Shutdown, cleans up timers).
    let _ = state.guest.lock().unwrap().take();
    let _ = state.push_rx.lock().unwrap().take();

    let compile_path = resolve_compile_path(&abs);
    let compile_str = compile_path.to_string_lossy().to_string();

    let compile_output = compile_with_auto_install(&compile_str)
        .map_err(|e| shell::StudioError::vo_compile(&e.to_string()))?;
    let (initial_bytes, handle, push) = vo_vox::gui::run_gui(compile_output)
        .map_err(|e| shell::StudioError::vo_runtime(&e))?;

    *state.guest.lock().unwrap() = Some(handle);
    *state.push_rx.lock().unwrap() = Some(push);
    Ok(initial_bytes)
}

/// Send an event to the running guest VM and return the new render bytes.
#[tauri::command]
fn cmd_send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<u8>, shell::StudioError> {
    let guard = state.guest.lock().unwrap();
    let handle = guard.as_ref()
        .ok_or_else(|| shell::StudioError::vo_runtime("No guest VM running"))?;
    handle.send_event(handler_id, &payload)
        .map_err(|e| shell::StudioError::vo_runtime(&e))
}

/// Poll for platform-driven render updates (game loop, timers, anim frames).
/// Returns the latest render bytes if available, or empty vec if none.
#[tauri::command]
fn cmd_poll_gui_render(state: tauri::State<'_, AppState>) -> Vec<u8> {
    let guard = state.push_rx.lock().unwrap();
    match guard.as_ref() {
        Some(push) => push.poll().unwrap_or_default(),
        None => Vec::new(),
    }
}

/// Stop the running guest VM.
#[tauri::command]
fn cmd_stop_gui(state: tauri::State<'_, AppState>) -> Result<(), shell::StudioError> {
    let _ = state.guest.lock().unwrap().take();
    let _ = state.push_rx.lock().unwrap().take();
    Ok(())
}

// =============================================================================
// App entry
// =============================================================================

pub fn run() {
    // Force linker to include vogui extern registrations (linkme distributed slices).
    vogui::ensure_linked();

    let workspace = default_workspace();
    let launch_url = detect_launch_url();
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
            launch_url,
            guest:         Mutex::new(None),
            push_rx:       Mutex::new(None),
        })
        .invoke_handler(tauri::generate_handler![
            cmd_get_workspace_root,
            cmd_get_launch_url,
            cmd_materialize_local_launch_target,
            cmd_compile_run,
            cmd_run_gui,
            cmd_send_gui_event,
            cmd_poll_gui_render,
            cmd_stop_gui,
            shell::cmd_shell_init,
            shell::cmd_shell_exec,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Vibe Studio");
}

#[cfg(test)]
mod tests {
    use super::{materialize_local_target, materialize_source_root};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn make_temp_dir(name: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("vibe-studio-{name}-{unique}"));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn materialize_source_root_uses_parent_dir_for_single_file_without_project() {
        let root = make_temp_dir("single-file-root");
        let app_dir = root.join("demo");
        fs::create_dir_all(&app_dir).unwrap();
        let main_file = app_dir.join("main.vo");
        fs::write(&main_file, "package main\n").unwrap();

        assert_eq!(materialize_source_root(&main_file), app_dir);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn materialize_local_target_copies_parent_dir_for_single_file_launch() {
        let workspace_root = make_temp_dir("workspace");
        let source_root = make_temp_dir("external-source");
        let app_dir = source_root.join("demo");
        let codec_dir = app_dir.join("codec");
        fs::create_dir_all(&codec_dir).unwrap();
        let main_file = app_dir.join("main.vo");
        fs::write(&main_file, "package main\nimport \"./codec\"\n").unwrap();
        fs::write(codec_dir.join("codec.vo"), "package codec\n").unwrap();

        let materialized = materialize_local_target(&workspace_root, &main_file).unwrap();
        let materialized_root = materialized.parent().unwrap();

        assert_eq!(materialized.file_name(), Some(std::ffi::OsStr::new("main.vo")));
        assert!(materialized_root.join("codec/codec.vo").is_file());
        assert!(Path::new(&materialized).is_file());

        fs::remove_dir_all(&workspace_root).unwrap();
        fs::remove_dir_all(&source_root).unwrap();
    }
}
