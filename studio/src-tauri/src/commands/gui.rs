use std::path::{Path, PathBuf};
use std::time::Instant;

use tauri::AppHandle;
use toml::Value;
use vo_engine::with_compile_log_sink;
use crate::commands::compiler::prepare_and_compile;
use crate::commands::pathing::resolve_run_target;
use crate::gui_runtime;
use crate::state::AppState;

struct StudioManifest {
    entry: String,
    capabilities: Vec<String>,
    renderer_path: Option<PathBuf>,
}

fn parse_studio_manifest(manifest_path: &Path) -> Option<StudioManifest> {
    let content = std::fs::read_to_string(manifest_path).ok()?;
    let value: Value = toml::from_str(&content).ok()?;
    let studio = value.get("studio")?.as_table()?;
    let entry = studio
        .get("entry")
        .and_then(Value::as_str)
        .unwrap_or("Run")
        .to_string();
    let capabilities = studio
        .get("capabilities")
        .and_then(Value::as_array)
        .map(|arr| {
            arr.iter()
                .filter_map(Value::as_str)
                .map(str::to_string)
                .collect()
        })
        .unwrap_or_default();
    let parent = manifest_path.parent()?;
    let renderer_path = studio.get("renderer").and_then(Value::as_str).map(|r| {
        let p = PathBuf::from(r);
        if p.is_absolute() { p } else { parent.join(p) }
    });
    Some(StudioManifest {
        entry,
        capabilities,
        renderer_path,
    })
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FrameworkContract {
    pub name: String,
    pub entry: String,
    pub capabilities: Vec<String>,
    pub renderer_path: Option<String>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GuiRunOutput {
    render_bytes: Vec<u8>,
    module_bytes: Vec<u8>,
    entry_path: String,
    framework: Option<FrameworkContract>,
    external_widget_handler_id: Option<i32>,
}

/// Decode binary render bytes and find the `onWidget` handler ID of a `vo-external-widget` node.
fn find_on_widget_handler_id(bytes: &[u8]) -> Option<i32> {
    let frame = vogui_protocol::decode_binary_render(bytes).ok()?;
    vogui_protocol::query::find_external_widget_handler_id(&frame)
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RenderIslandVfsFile {
    path: String,
    bytes: Vec<u8>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RenderIslandVfsSnapshot {
    root_path: String,
    files: Vec<RenderIslandVfsFile>,
}

#[tauri::command]
pub fn cmd_run_gui(
    entry_path: String,
    session_id: u64,
    state: tauri::State<'_, AppState>,
    app: AppHandle,
) -> Result<GuiRunOutput, String> {
    state.clear_guest_runtime();
    let session_root = state.session_root();
    let run_target = resolve_run_target(&session_root, state.workspace_root(), &entry_path, state.single_file_run())?;
    let compile_path = run_target.compile_path.to_string_lossy().to_string();
    let compile_start = Instant::now();
    let compile_output = with_compile_log_sink(
        gui_runtime::make_gui_log_sink(app.clone(), session_id),
        || prepare_and_compile(&compile_path),
    )?;
    gui_runtime::emit_gui_log(
        &app,
        session_id,
        "studio-native",
        &format!("runGui compile {} {}ms", entry_path, compile_start.elapsed().as_millis()),
    );
    let module_bytes = compile_output.module.serialize();
    let framework = extract_framework_contract(&compile_output.extensions);
    state.set_last_extensions(compile_output.extensions.clone());
    let start_start = Instant::now();
    let (render_bytes, handle, push_rx) = gui_runtime::run_gui(compile_output, app.clone(), session_id)
        .map_err(|error| error.to_string())?;
    gui_runtime::emit_gui_log(
        &app,
        session_id,
        "studio-native",
        &format!("runGui start app {} {}ms", entry_path, start_start.elapsed().as_millis()),
    );
    state.install_guest_runtime(handle, push_rx);
    let external_widget_handler_id = find_on_widget_handler_id(&render_bytes);
    Ok(GuiRunOutput {
        render_bytes,
        module_bytes,
        entry_path,
        framework,
        external_widget_handler_id,
    })
}

#[tauri::command]
pub fn cmd_send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<u8>, String> {
    state.with_guest(|handle| handle.send_event(handler_id, &payload))
}

#[tauri::command]
pub fn cmd_send_gui_event_async(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_guest(|handle| handle.send_event_async(handler_id, &payload))
}

#[tauri::command]
pub fn cmd_debug_log(message: String) -> Result<(), String> {
    crate::gui_runtime::debug_log(&format!("[studio-frontend] {}", message));
    Ok(())
}

#[tauri::command]
pub fn __island_transport_push(
    data: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_guest(|handle| handle.push_island_data(&data))
}

#[tauri::command]
pub fn cmd_poll_gui_render(state: tauri::State<'_, AppState>) -> Vec<u8> {
    state.poll_gui_render()
}

#[tauri::command]
pub fn cmd_stop_gui(state: tauri::State<'_, AppState>) -> Result<(), String> {
    state.clear_guest_runtime();
    Ok(())
}

#[tauri::command]
pub fn cmd_get_render_island_vfs_snapshot(
    entry_path: String,
    state: tauri::State<'_, AppState>,
) -> Result<RenderIslandVfsSnapshot, String> {
    let session_root = state.session_root();
    let run_target = resolve_run_target(&session_root, state.workspace_root(), &entry_path, state.single_file_run())?;
    let root_path = run_target.source_root;
    let mut files = collect_render_island_vfs_files(&root_path)?;

    // Append framework extension files (renderer JS + WASM island) from their package directories.
    for ext in state.last_extensions() {
        let Some(studio) = parse_studio_manifest(&ext.manifest_path) else { continue };
        let ext_root = ext.manifest_path.parent().unwrap_or(ext.manifest_path.as_path());
        ensure_pkg_island_fresh(ext_root, &ext.name)?;

        // Include the renderer directory so relative JS imports resolve.
        if let Some(renderer_path) = studio.renderer_path.as_ref() {
            if renderer_path.is_file() {
                if let Some(renderer_dir) = renderer_path.parent() {
                    match collect_render_island_vfs_files(renderer_dir) {
                        Ok(extra) => files.extend(extra),
                        Err(e) => eprintln!("[studio] failed to read renderer dir {}: {}", renderer_dir.display(), e),
                    }
                    if let Ok(extra) = collect_render_island_vfs_files_virtual(renderer_dir, ext_root, "") {
                        files.extend(extra);
                    }
                }
            }
        }

        // Include all files from <ext_root>/rust/pkg-island/ if it exists (WASM island outputs).
        let pkg_island = ext_root.join("rust").join("pkg-island");
        if pkg_island.is_dir() {
            if let Ok(extra) = collect_render_island_vfs_files(&pkg_island) {
                files.extend(extra);
            }
            if let Ok(extra) = collect_render_island_vfs_files_virtual(&pkg_island, &pkg_island, "wasm") {
                files.extend(extra);
            }
        }
    }

    Ok(RenderIslandVfsSnapshot {
        root_path: root_path.to_string_lossy().to_string(),
        files,
    })
}

fn ensure_pkg_island_fresh(ext_root: &Path, ext_name: &str) -> Result<(), String> {
    let rust_root = ext_root.join("rust");
    let cargo_toml = rust_root.join("Cargo.toml");
    if !cargo_toml.is_file() {
        return Ok(());
    }
    if !pkg_island_needs_build(&rust_root, ext_name)? {
        return Ok(());
    }
    let out_dir = rust_root.join("pkg-island");
    let out_name = format!("{}_island", ext_name);
    eprintln!("[studio] building render-island wasm for {}", ext_name);
    let output = std::process::Command::new("wasm-pack")
        .args([
            "build",
            "--target", "web",
            "--out-dir", &out_dir.to_string_lossy(),
            "--out-name", &out_name,
            "--release",
            &rust_root.to_string_lossy(),
            "--",
            "--no-default-features",
            "--features", "wasm-island",
        ])
        .output()
        .map_err(|error| format!("wasm-pack: {}", error))?;
    if output.status.success() {
        return Ok(());
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    Err(format!(
        "failed to build render-island wasm for {}:\n{}\n{}",
        ext_name,
        stdout.trim_end(),
        stderr.trim_end(),
    ))
}

fn pkg_island_needs_build(rust_root: &Path, ext_name: &str) -> Result<bool, String> {
    let pkg_island = rust_root.join("pkg-island");
    let wasm_path = pkg_island.join(format!("{}_island_bg.wasm", ext_name));
    let js_path = pkg_island.join(format!("{}_island.js", ext_name));
    let Some(wasm_mtime) = file_modified(&wasm_path)? else {
        return Ok(true);
    };
    let Some(js_mtime) = file_modified(&js_path)? else {
        return Ok(true);
    };
    let oldest_output = if wasm_mtime < js_mtime { wasm_mtime } else { js_mtime };

    let mut newest_input = None;
    update_newest_modified(&mut newest_input, file_modified(&rust_root.join("Cargo.toml"))?);
    update_newest_modified(&mut newest_input, file_modified(&rust_root.join("Cargo.lock"))?);
    update_newest_modified(&mut newest_input, newest_modified_in_dir(&rust_root.join("src"))?);

    Ok(newest_input.map(|mtime| mtime > oldest_output).unwrap_or(false))
}

fn update_newest_modified(
    newest: &mut Option<std::time::SystemTime>,
    candidate: Option<std::time::SystemTime>,
) {
    let Some(candidate) = candidate else {
        return;
    };
    match newest {
        Some(current) if *current >= candidate => {}
        _ => *newest = Some(candidate),
    }
}

fn file_modified(path: &Path) -> Result<Option<std::time::SystemTime>, String> {
    if !path.exists() {
        return Ok(None);
    }
    let metadata = std::fs::metadata(path)
        .map_err(|error| format!("{}: {}", path.display(), error))?;
    metadata
        .modified()
        .map(Some)
        .map_err(|error| format!("{}: {}", path.display(), error))
}

fn newest_modified_in_dir(dir: &Path) -> Result<Option<std::time::SystemTime>, String> {
    if !dir.is_dir() {
        return Ok(None);
    }

    fn walk(dir: &Path, newest: &mut Option<std::time::SystemTime>) -> Result<(), String> {
        for entry in std::fs::read_dir(dir)
            .map_err(|error| format!("{}: {}", dir.display(), error))?
        {
            let entry = entry
                .map_err(|error| format!("{}: {}", dir.display(), error))?;
            let path = entry.path();
            let file_type = entry.file_type()
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            if file_type.is_dir() {
                walk(&path, newest)?;
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            update_newest_modified(
                newest,
                Some(
                    entry.metadata()
                        .map_err(|error| format!("{}: {}", path.display(), error))?
                        .modified()
                        .map_err(|error| format!("{}: {}", path.display(), error))?,
                ),
            );
        }
        Ok(())
    }

    let mut newest = None;
    walk(dir, &mut newest)?;
    Ok(newest)
}

fn collect_render_island_vfs_files(root: &Path) -> Result<Vec<RenderIslandVfsFile>, String> {
    fn walk(dir: &Path, out: &mut Vec<RenderIslandVfsFile>) -> Result<(), String> {
        for entry in std::fs::read_dir(dir)
            .map_err(|error| format!("{}: {}", dir.display(), error))?
        {
            let entry = entry
                .map_err(|error| format!("{}: {}", dir.display(), error))?;
            let path = entry.path();
            let file_type = entry.file_type()
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            if file_type.is_dir() {
                walk(&path, out)?;
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            let bytes = std::fs::read(&path)
                .map_err(|error| format!("{}: {}", path.display(), error))?;
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

fn collect_render_island_vfs_files_virtual(
    root: &Path,
    strip_prefix: &Path,
    virtual_prefix: &str,
) -> Result<Vec<RenderIslandVfsFile>, String> {
    fn walk(
        dir: &Path,
        strip_prefix: &Path,
        virtual_prefix: &str,
        out: &mut Vec<RenderIslandVfsFile>,
    ) -> Result<(), String> {
        for entry in std::fs::read_dir(dir)
            .map_err(|error| format!("{}: {}", dir.display(), error))?
        {
            let entry = entry
                .map_err(|error| format!("{}: {}", dir.display(), error))?;
            let path = entry.path();
            let file_type = entry.file_type()
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            if file_type.is_dir() {
                walk(&path, strip_prefix, virtual_prefix, out)?;
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            let rel = path.strip_prefix(strip_prefix)
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            let rel = rel.to_string_lossy().replace('\\', "/");
            let virtual_path = if virtual_prefix.is_empty() {
                rel
            } else {
                format!("{}/{}", virtual_prefix.trim_end_matches('/'), rel)
            };
            let bytes = std::fs::read(&path)
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            out.push(RenderIslandVfsFile {
                path: virtual_path,
                bytes,
            });
        }
        Ok(())
    }

    let mut files = Vec::new();
    walk(root, strip_prefix, virtual_prefix, &mut files)?;
    Ok(files)
}

fn extract_framework_contract(extensions: &[vo_module::ext_manifest::ExtensionManifest]) -> Option<FrameworkContract> {
    for ext in extensions {
        if let Some(studio) = parse_studio_manifest(&ext.manifest_path) {
            return Some(FrameworkContract {
                name: ext.name.clone(),
                entry: studio.entry,
                capabilities: studio.capabilities,
                renderer_path: studio.renderer_path.map(|p| p.to_string_lossy().to_string()),
            });
        }
    }
    None
}
