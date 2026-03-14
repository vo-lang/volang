//! Tauri commands for Vibe Studio (Svelte-native frontend).
//!
//! The IDE UI is a Svelte app; this backend provides:
//! - Compile & run user Vo code via vo-engine / vox
//! - Unified shell API via ShellRouter (shell/mod.rs)
//!
//! All filesystem operations are routed through `cmd_shell_exec` → Vo shell handler.
mod gui_runtime;
mod shell;

use std::collections::BTreeMap;
use std::env;
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::sync::{Arc, Mutex};
use gui_runtime::{GuestHandle, PushReceiver};
use vo_vox::{compile_prepared, prepare_with_auto_install, run_with_output as run_vox, RunMode};
use vo_runtime::output::CaptureSink;
use vo_module::ModFile;

static VOWORK_ENV_LOCK: Mutex<()> = Mutex::new(());

pub(crate) fn debug_log(message: &str) {
    eprintln!("{message}");
    if let Ok(path) = std::env::var("VIBE_STUDIO_DEBUG_LOG") {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
        {
            let _ = writeln!(file, "{message}");
        }
    }
}

// =============================================================================
// AppState
// =============================================================================

pub struct AppState {
    pub workspace_root: PathBuf,
    launch_url:   Option<String>,
    session_root: Mutex<PathBuf>,
    guest:        Mutex<Option<GuestHandle>>,
    /// Platform-driven render receiver — stored separately so polling
    /// doesn't contend with the guest handle lock.
    push_rx:      Mutex<Option<Arc<PushReceiver>>>,
    shell_runner: shell::VoRunner,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct GuiRunOutput {
    render_bytes: Vec<u8>,
    module_bytes: Vec<u8>,
    entry_path: String,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RenderIslandVfsFile {
    path:  String,
    bytes: Vec<u8>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RenderIslandVfsSnapshot {
    root_path: String,
    files:     Vec<RenderIslandVfsFile>,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct StudioSyncExampleInput {
    path: String,
    content: String,
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
    debug_log("[studio-native] cmd_get_workspace_root");
    state.workspace_root.to_string_lossy().to_string()
}

#[tauri::command]
fn cmd_get_launch_url(state: tauri::State<'_, AppState>) -> Option<String> {
    debug_log("[studio-native] cmd_get_launch_url");
    state.launch_url.clone()
}

#[tauri::command]
fn cmd_sync_studio_examples(
    examples: Vec<StudioSyncExampleInput>,
    state: tauri::State<'_, AppState>,
) -> Result<(), shell::StudioError> {
    debug_log(&format!(
        "[studio-native] cmd_sync_studio_examples count={}",
        examples.len()
    ));
    for example in examples {
        let path = resolve_path(&state.workspace_root, &example.path)
            .map_err(|e| shell::StudioError::access_denied(&e))?;
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| shell::StudioError::internal(&format!("{}: {}", parent.display(), e)))?;
        }
        std::fs::write(&path, example.content)
            .map_err(|e| shell::StudioError::internal(&format!("{}: {}", path.display(), e)))?;
    }
    Ok(())
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

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct LocalDevLaunchTarget {
    target_path: String,
    session_root: String,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct DevModeModuleFile {
    name: String,
    content: String,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct DevModeLocalModule {
    module_path: String,
    files: Vec<DevModeModuleFile>,
    wasm_bytes: Option<Vec<u8>>,
}

fn resolve_local_dev_target(target_path: &Path) -> Result<LocalDevLaunchTarget, shell::StudioError> {
    if !target_path.exists() {
        return Err(shell::StudioError::not_found(&format!(
            "launch target not found: {}",
            target_path.display()
        )));
    }

    let canonical_target = target_path
        .canonicalize()
        .map_err(|e| shell::StudioError::internal(&format!("{}: {}", target_path.display(), e)))?;
    let session_root = materialize_source_root(&canonical_target)
        .canonicalize()
        .map_err(|e| shell::StudioError::internal(&format!("{}: {}", canonical_target.display(), e)))?;

    Ok(LocalDevLaunchTarget {
        target_path: canonical_target.to_string_lossy().to_string(),
        session_root: session_root.to_string_lossy().to_string(),
    })
}

fn read_mod_file(path: &Path) -> Result<ModFile, shell::StudioError> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| shell::StudioError::internal(&format!("{}: {}", path.display(), e)))?;
    ModFile::parse(&content, path)
        .map_err(|e| shell::StudioError::internal(&e.to_string()))
}

fn read_project_replaces(project_root: &Path) -> Result<BTreeMap<String, PathBuf>, shell::StudioError> {
    let mut map = BTreeMap::new();
    let mod_path = project_root.join("vo.mod");
    if !mod_path.is_file() {
        return Ok(map);
    }
    let mod_file = read_mod_file(&mod_path)?;
    for rep in &mod_file.replaces {
        let local = Path::new(&rep.local_path);
        let abs = if local.is_absolute() {
            local.to_path_buf()
        } else {
            project_root.join(local)
        };
        let abs = abs.canonicalize().unwrap_or(abs);
        map.insert(rep.module.clone(), abs);
    }
    Ok(map)
}

fn collect_module_files(module_root: &Path) -> Result<Vec<DevModeModuleFile>, shell::StudioError> {
    let vo_mod_path = module_root.join("vo.mod");
    let vo_mod_content = std::fs::read_to_string(&vo_mod_path)
        .map_err(|e| shell::StudioError::internal(&format!("{}: {}", vo_mod_path.display(), e)))?;
    let mod_file = ModFile::parse(&vo_mod_content, &vo_mod_path)
        .map_err(|e| shell::StudioError::internal(&e.to_string()))?;

    let mut files = Vec::new();
    files.push(DevModeModuleFile {
        name: "vo.mod".to_string(),
        content: vo_mod_content,
    });

    let ext_path = module_root.join("vo.ext.toml");
    if ext_path.is_file() {
        let content = std::fs::read_to_string(&ext_path)
            .map_err(|e| shell::StudioError::internal(&format!("{}: {}", ext_path.display(), e)))?;
        files.push(DevModeModuleFile {
            name: "vo.ext.toml".to_string(),
            content,
        });
    }

    for rel in &mod_file.files {
        let path = module_root.join(rel);
        let content = std::fs::read_to_string(&path)
            .map_err(|e| shell::StudioError::internal(&format!("{}: {}", path.display(), e)))?;
        files.push(DevModeModuleFile {
            name: rel.clone(),
            content,
        });
    }

    files.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(files)
}

fn read_module_wasm_bytes(module_root: &Path, module_path: &str) -> Result<Option<Vec<u8>>, shell::StudioError> {
    let module_name = module_path.rsplit('/').next().unwrap_or(module_path);
    let wasm_path = module_root.join(format!("{}.wasm", module_name));
    if !wasm_path.is_file() {
        return Ok(None);
    }
    let bytes = std::fs::read(&wasm_path)
        .map_err(|e| shell::StudioError::internal(&format!("{}: {}", wasm_path.display(), e)))?;
    Ok(Some(bytes))
}

fn collect_render_island_vfs_files(root: &Path) -> Result<Vec<RenderIslandVfsFile>, shell::StudioError> {
    fn walk(dir: &Path, out: &mut Vec<RenderIslandVfsFile>) -> Result<(), shell::StudioError> {
        for entry in std::fs::read_dir(dir)
            .map_err(|e| shell::StudioError::internal(&format!("{}: {}", dir.display(), e)))?
        {
            let entry = entry
                .map_err(|e| shell::StudioError::internal(&format!("{}: {}", dir.display(), e)))?;
            let path = entry.path();
            let file_type = entry.file_type()
                .map_err(|e| shell::StudioError::internal(&format!("{}: {}", path.display(), e)))?;
            if file_type.is_dir() {
                walk(&path, out)?;
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            let bytes = std::fs::read(&path)
                .map_err(|e| shell::StudioError::internal(&format!("{}: {}", path.display(), e)))?;
            out.push(RenderIslandVfsFile {
                path: path.to_string_lossy().to_string(),
                bytes,
            });
        }
        Ok(())
    }

    let mut files = Vec::new();
    walk(root, &mut files)?;
    Ok(files)
}

fn build_dev_mode_local_modules(project_root: &Path) -> Result<Vec<DevModeLocalModule>, shell::StudioError> {
    let mut module_roots: BTreeMap<String, PathBuf> = vo_module::find_workspace_replaces(project_root)
        .into_iter()
        .collect();
    for (module, path) in read_project_replaces(project_root)? {
        module_roots.insert(module, path);
    }

    let mut modules = Vec::with_capacity(module_roots.len());
    for (module_path, module_root) in module_roots {
        modules.push(DevModeLocalModule {
            module_path: module_path.clone(),
            files: collect_module_files(&module_root)?,
            wasm_bytes: read_module_wasm_bytes(&module_root, &module_path)?,
        });
    }
    Ok(modules)
}

pub(crate) fn current_session_root(state: &AppState) -> PathBuf {
    state.session_root.lock().unwrap().clone()
}

fn reset_session_root(state: &AppState) {
    *state.session_root.lock().unwrap() = state.workspace_root.clone();
}

fn set_session_root(state: &AppState, session_root: PathBuf) {
    *state.session_root.lock().unwrap() = session_root;
}

fn session_uses_workspace_mode(state: &AppState) -> bool {
    current_session_root(state) != state.workspace_root
}

fn with_session_workspace_mode<T>(state: &AppState, f: impl FnOnce() -> T) -> T {
    let _guard = VOWORK_ENV_LOCK.lock().unwrap();
    let previous = env::var_os("VOWORK");
    if session_uses_workspace_mode(state) {
        env::remove_var("VOWORK");
    } else {
        env::set_var("VOWORK", "off");
    }
    let result = f();
    match previous {
        Some(value) => env::set_var("VOWORK", value),
        None => env::remove_var("VOWORK"),
    }
    result
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

fn resolve_compile_target(
    workspace_root: &Path,
    entry_path: &str,
) -> Result<String, shell::StudioError> {
    let abs = resolve_path(workspace_root, entry_path)
        .map_err(|e| shell::StudioError::access_denied(&e))?;
    let compile_path = resolve_compile_path(&abs);
    Ok(compile_path.to_string_lossy().to_string())
}

fn clear_guest_runtime(state: &AppState) {
    let _ = state.guest.lock().unwrap().take();
    let _ = state.push_rx.lock().unwrap().take();
}

#[tauri::command]
fn cmd_prepare_app(entry_path: String, state: tauri::State<'_, AppState>) -> Result<(), shell::StudioError> {
    let session_root = current_session_root(&state);
    let compile_str = resolve_compile_target(&session_root, &entry_path)?;

    with_session_workspace_mode(&state, || prepare_with_auto_install(&compile_str))
        .map_err(|e| shell::StudioError::vo_compile(&e.to_string()))?;
    Ok(())
}

/// Compile and run a prepared non-GUI app from an entry path, returning captured stdout.
#[tauri::command]
fn cmd_compile_run_app(entry_path: String, state: tauri::State<'_, AppState>) -> Result<String, shell::StudioError> {
    let session_root = current_session_root(&state);
    let compile_str = resolve_compile_target(&session_root, &entry_path)?;

    let compile_output = with_session_workspace_mode(&state, || compile_prepared(&compile_str))
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
fn cmd_run_gui(
    entry_path: String,
    state: tauri::State<'_, AppState>,
    app: tauri::AppHandle,
) -> Result<GuiRunOutput, shell::StudioError> {
    // Drop previous guest (sends Shutdown, cleans up timers).
    clear_guest_runtime(&state);

    let session_root = current_session_root(&state);
    let compile_str = resolve_compile_target(&session_root, &entry_path)?;

    let compile_output = with_session_workspace_mode(&state, || compile_prepared(&compile_str))
        .map_err(|e| shell::StudioError::vo_compile(&e.to_string()))?;
    let module_bytes = compile_output.module.serialize();
    let (initial_bytes, handle, push) = gui_runtime::run_gui(compile_output, app)
        .map_err(|e| shell::StudioError::vo_runtime(&e))?;

    *state.guest.lock().unwrap() = Some(handle);
    *state.push_rx.lock().unwrap() = Some(push);
    Ok(GuiRunOutput {
        render_bytes: initial_bytes,
        module_bytes,
        entry_path,
    })
}

#[tauri::command]
fn cmd_get_render_island_vfs_snapshot(
    entry_path: String,
    state: tauri::State<'_, AppState>,
) -> Result<RenderIslandVfsSnapshot, shell::StudioError> {
    let session_root = current_session_root(&state);
    let compile_target = resolve_compile_target(&session_root, &entry_path)?;
    let root_path = materialize_source_root(Path::new(&compile_target));
    let files = collect_render_island_vfs_files(&root_path)?;
    Ok(RenderIslandVfsSnapshot {
        root_path: root_path.to_string_lossy().to_string(),
        files,
    })
}

#[tauri::command]
fn cmd_send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<u8>, shell::StudioError> {
    let guest = state.guest.lock().unwrap();
    let handle = guest
        .as_ref()
        .ok_or_else(|| shell::StudioError::vo_runtime("guest VM not running"))?;
    handle
        .send_event(handler_id, &payload)
        .map_err(|e| shell::StudioError::vo_runtime(&e))
}

#[tauri::command]
fn cmd_send_gui_event_async(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), shell::StudioError> {
    let guest = state.guest.lock().unwrap();
    let handle = guest
        .as_ref()
        .ok_or_else(|| shell::StudioError::vo_runtime("guest VM not running"))?;
    handle
        .send_event_async(handler_id, &payload)
        .map_err(|e| shell::StudioError::vo_runtime(&e))
}

#[tauri::command]
fn __island_transport_push(
    data: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), shell::StudioError> {
    let guest = state.guest.lock().unwrap();
    let handle = guest
        .as_ref()
        .ok_or_else(|| shell::StudioError::vo_runtime("guest VM not running"))?;
    handle
        .push_island_data(&data)
        .map_err(|e| shell::StudioError::vo_runtime(&e))
}

#[tauri::command]
fn cmd_poll_gui_render(state: tauri::State<'_, AppState>) -> Vec<u8> {
    state
        .push_rx
        .lock()
        .unwrap()
        .as_ref()
        .and_then(|rx| rx.poll())
        .unwrap_or_default()
}

#[tauri::command]
fn cmd_stop_gui(state: tauri::State<'_, AppState>) -> Result<(), shell::StudioError> {
    clear_guest_runtime(&state);
    Ok(())
}

#[tauri::command]
fn cmd_activate_local_dev_mode(
    target_path: String,
    state: tauri::State<'_, AppState>,
) -> Result<LocalDevLaunchTarget, shell::StudioError> {
    let resolved = resolve_local_dev_target(Path::new(&target_path))?;
    set_session_root(&state, PathBuf::from(&resolved.session_root));
    Ok(resolved)
}

#[tauri::command]
fn cmd_get_dev_mode_workspace_closure(
    state: tauri::State<'_, AppState>,
) -> Result<Vec<DevModeLocalModule>, shell::StudioError> {
    if !session_uses_workspace_mode(&state) {
        return Ok(Vec::new());
    }
    let session_root = current_session_root(&state);
    build_dev_mode_local_modules(&session_root)
}

#[tauri::command]
fn cmd_reset_launch_mode(state: tauri::State<'_, AppState>) {
    reset_session_root(&state);
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
            shell_runner:  shell::VoRunner::new(),
            workspace_root: workspace,
            launch_url,
            session_root:  Mutex::new(default_workspace()),
            guest:         Mutex::new(None),
            push_rx:       Mutex::new(None),
        })
        .invoke_handler(tauri::generate_handler![
            cmd_get_workspace_root,
            cmd_get_launch_url,
            cmd_sync_studio_examples,
            cmd_materialize_local_launch_target,
            cmd_activate_local_dev_mode,
            cmd_get_dev_mode_workspace_closure,
            cmd_reset_launch_mode,
            cmd_prepare_app,
            cmd_compile_run_app,
            cmd_run_gui,
            cmd_get_render_island_vfs_snapshot,
            cmd_send_gui_event,
            cmd_send_gui_event_async,
            __island_transport_push,
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
    use crate::{build_dev_mode_local_modules, materialize_local_target, materialize_source_root, resolve_compile_target, resolve_local_dev_target};
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

    #[test]
    fn materialize_local_target_allows_vo_work_projects() {
        let workspace_root = make_temp_dir("workspace-with-work");
        let source_root = make_temp_dir("external-work-project");
        let game_root = source_root.join("MarbleRush");
        fs::create_dir_all(&game_root).unwrap();

        fs::write(game_root.join("vo.work"), "vo 0.1\n\nuse ../voplay\n").unwrap();
        fs::write(game_root.join("vo.mod"), "module marblerush\n\nvo 0.1\n").unwrap();
        fs::write(game_root.join("main.vo"), "package main\n").unwrap();

        let materialized = materialize_local_target(&workspace_root, &game_root).unwrap();
        assert!(materialized.join("main.vo").is_file());
        assert!(materialized.join("vo.work").is_file());

        fs::remove_dir_all(&workspace_root).unwrap();
        fs::remove_dir_all(&source_root).unwrap();
    }

    #[test]
    fn resolve_local_dev_target_preserves_external_vo_work_project_root() {
        let source_root = make_temp_dir("external-dev-work-project");
        let game_root = source_root.join("MarbleRush");
        fs::create_dir_all(game_root.join("src")).unwrap();

        fs::write(game_root.join("vo.work"), "vo 0.1\n\nuse ../voplay\n").unwrap();
        fs::write(game_root.join("vo.mod"), "module marblerush\n\nvo 0.1\n").unwrap();
        fs::write(game_root.join("src/main.vo"), "package main\n").unwrap();

        let resolved = resolve_local_dev_target(&game_root.join("src/main.vo")).unwrap();
        assert_eq!(
            PathBuf::from(&resolved.target_path).canonicalize().unwrap(),
            game_root.join("src/main.vo").canonicalize().unwrap()
        );
        assert_eq!(
            PathBuf::from(&resolved.session_root).canonicalize().unwrap(),
            game_root.canonicalize().unwrap()
        );

        fs::remove_dir_all(&source_root).unwrap();
    }

    #[test]
    fn resolve_compile_target_allows_vo_work_projects_inside_workspace() {
        let workspace_root = make_temp_dir("workspace-compile-work");
        let project_root = workspace_root.join("demo");
        fs::create_dir_all(&project_root).unwrap();

        fs::write(project_root.join("vo.work"), "vo 0.1\n\nuse ../voplay\n").unwrap();
        fs::write(project_root.join("vo.mod"), "module demo\n\nvo 0.1\n").unwrap();
        fs::write(project_root.join("main.vo"), "package main\n").unwrap();

        let compile_target = resolve_compile_target(&workspace_root, "demo/main.vo").unwrap();
        assert_eq!(
            PathBuf::from(&compile_target).canonicalize().unwrap(),
            project_root.canonicalize().unwrap()
        );

        fs::remove_dir_all(&workspace_root).unwrap();
    }

    #[test]
    fn build_dev_mode_local_modules_includes_workspace_module_sources_and_wasm() {
        let workspace_root = make_temp_dir("workspace-dev-closure");
        let project_root = workspace_root.join("demo");
        let voplay_root = workspace_root.join("voplay");
        fs::create_dir_all(&project_root).unwrap();
        fs::create_dir_all(&voplay_root).unwrap();

        fs::write(project_root.join("vo.work"), "vo 0.1\n\nuse ../voplay\n").unwrap();
        fs::write(
            project_root.join("vo.mod"),
            "module demo\n\nvo 0.1\n\nrequire github.com/vo-lang/voplay v0.1.0\n\nfiles (\n    main.vo\n)\n",
        ).unwrap();
        fs::write(project_root.join("main.vo"), "package main\n").unwrap();

        fs::write(
            voplay_root.join("vo.mod"),
            "module github.com/vo-lang/voplay\n\nvo 0.1\n\nfiles (\n    voplay.vo\n)\n",
        ).unwrap();
        fs::write(voplay_root.join("vo.ext.toml"), "[extension]\nname = \"voplay\"\npath = \"rust/target/{profile}/libvo_voplay\"\n").unwrap();
        fs::write(voplay_root.join("voplay.vo"), "package voplay\n").unwrap();
        fs::write(voplay_root.join("voplay.wasm"), [0, 97, 115, 109]).unwrap();

        let modules = build_dev_mode_local_modules(&project_root).unwrap();
        assert_eq!(modules.len(), 1);
        assert_eq!(modules[0].module_path, "github.com/vo-lang/voplay");
        assert!(modules[0].files.iter().any(|f| f.name == "vo.mod"));
        assert!(modules[0].files.iter().any(|f| f.name == "vo.ext.toml"));
        assert!(modules[0].files.iter().any(|f| f.name == "voplay.vo"));
        assert_eq!(modules[0].wasm_bytes.as_deref(), Some(&[0, 97, 115, 109][..]));

        fs::remove_dir_all(&workspace_root).unwrap();
    }

    #[test]
    fn build_dev_mode_local_modules_uses_project_replace_to_override_workspace_entry() {
        let workspace_root = make_temp_dir("workspace-dev-replace");
        let project_root = workspace_root.join("demo");
        let workspace_voplay = workspace_root.join("voplay-workspace");
        let replace_voplay = workspace_root.join("voplay-replace");
        fs::create_dir_all(&project_root).unwrap();
        fs::create_dir_all(&workspace_voplay).unwrap();
        fs::create_dir_all(&replace_voplay).unwrap();

        fs::write(project_root.join("vo.work"), "vo 0.1\n\nuse ../voplay-workspace\n").unwrap();
        fs::write(
            project_root.join("vo.mod"),
            "module demo\n\nvo 0.1\n\nreplace github.com/vo-lang/voplay => ../voplay-replace\n\nfiles (\n    main.vo\n)\n",
        ).unwrap();
        fs::write(project_root.join("main.vo"), "package main\n").unwrap();

        fs::write(
            workspace_voplay.join("vo.mod"),
            "module github.com/vo-lang/voplay\n\nvo 0.1\n\nfiles (\n    workspace.vo\n)\n",
        ).unwrap();
        fs::write(workspace_voplay.join("workspace.vo"), "package workspace\n").unwrap();

        fs::write(
            replace_voplay.join("vo.mod"),
            "module github.com/vo-lang/voplay\n\nvo 0.1\n\nfiles (\n    replace.vo\n)\n",
        ).unwrap();
        fs::write(replace_voplay.join("replace.vo"), "package replace\n").unwrap();

        let modules = build_dev_mode_local_modules(&project_root).unwrap();
        assert_eq!(modules.len(), 1);
        assert!(modules[0].files.iter().any(|f| f.name == "replace.vo"));
        assert!(!modules[0].files.iter().any(|f| f.name == "workspace.vo"));

        fs::remove_dir_all(&workspace_root).unwrap();
    }
}
