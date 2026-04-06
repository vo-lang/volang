use std::path::{Path, PathBuf};
use std::time::Instant;

use tauri::ipc::Response;
use tauri::AppHandle;
use toml::Value;
use vo_engine::with_compile_log_sink;
use super::run_blocking;
use crate::commands::compiler::prepare_and_compile;
use crate::commands::pathing::resolve_run_target;
use crate::gui_runtime;
use crate::state::AppState;

struct StudioManifest {
    entry: String,
    capabilities: Vec<String>,
    renderer_path: Option<PathBuf>,
    protocol_path: Option<PathBuf>,
    host_bridge_path: Option<PathBuf>,
}

struct WasmBuildCandidate {
    crate_root: PathBuf,
    package_name: Option<String>,
    lib_name: Option<String>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum WasmExtensionKind {
    Standalone,
    Bindgen,
}

struct WasmExtensionManifestSpec {
    name: String,
    kind: WasmExtensionKind,
    wasm_filename: String,
}

fn inspect_wasm_build_candidate(crate_root: &Path, required_feature: &str) -> Result<Option<WasmBuildCandidate>, String> {
    let cargo_toml = crate_root.join("Cargo.toml");
    if !cargo_toml.is_file() {
        return Ok(None);
    }
    let content = std::fs::read_to_string(&cargo_toml)
        .map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
    let value: Value = toml::from_str(&content)
        .map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
    let package_name = value
        .get("package")
        .and_then(Value::as_table)
        .and_then(|table| table.get("name"))
        .and_then(Value::as_str)
        .map(str::to_string);
    let lib_name = value
        .get("lib")
        .and_then(Value::as_table)
        .and_then(|table| table.get("name"))
        .and_then(Value::as_str)
        .map(str::to_string);
    let has_feature = value
        .get("features")
        .and_then(Value::as_table)
        .map(|table| table.contains_key(required_feature))
        .unwrap_or(false);
    Ok(has_feature.then_some(WasmBuildCandidate {
        crate_root: crate_root.to_path_buf(),
        package_name,
        lib_name,
    }))
}

fn select_wasm_build_candidate(
    rust_root: &Path,
    ext_name: &str,
    required_feature: &str,
) -> Result<WasmBuildCandidate, String> {
    let mut candidates = Vec::new();
    if let Some(candidate) = inspect_wasm_build_candidate(rust_root, required_feature)? {
        candidates.push(candidate);
    }
    for entry in std::fs::read_dir(rust_root)
        .map_err(|error| format!("{}: {}", rust_root.display(), error))?
    {
        let entry = entry
            .map_err(|error| format!("{}: {}", rust_root.display(), error))?;
        let path = entry.path();
        if !entry
            .file_type()
            .map_err(|error| format!("{}: {}", path.display(), error))?
            .is_dir()
        {
            continue;
        }
        if let Some(candidate) = inspect_wasm_build_candidate(&path, required_feature)? {
            candidates.push(candidate);
        }
    }
    if let Some(candidate) = candidates.iter().find(|candidate| {
        candidate.package_name.as_deref() == Some(ext_name)
    }) {
        return Ok(WasmBuildCandidate {
            crate_root: candidate.crate_root.clone(),
            package_name: candidate.package_name.clone(),
            lib_name: candidate.lib_name.clone(),
        });
    }
    if candidates.len() == 1 {
        return Ok(candidates.into_iter().next().unwrap());
    }
    if candidates.is_empty() {
        return Err(format!(
            "no {} crate found under {} for {}",
            required_feature,
            rust_root.display(),
            ext_name,
        ));
    }
    let names = candidates
        .iter()
        .map(|candidate| candidate.crate_root.display().to_string())
        .collect::<Vec<_>>();
    Err(format!(
        "multiple {} crate candidates under {} for {}: {}",
        required_feature,
        rust_root.display(),
        ext_name,
        names.join(", "),
    ))
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
    let resolve_path = |key: &str| -> Option<PathBuf> {
        studio.get(key).and_then(Value::as_str).map(|r| {
            let p = PathBuf::from(r);
            if p.is_absolute() { p } else { parent.join(p) }
        })
    };
    let renderer_path = resolve_path("renderer");
    let protocol_path = resolve_path("protocol");
    let host_bridge_path = resolve_path("host_bridge");
    Some(StudioManifest {
        entry,
        capabilities,
        renderer_path,
        protocol_path,
        host_bridge_path,
    })
}

fn studio_declares_vo_web(studio: &StudioManifest) -> bool {
    studio.capabilities.iter().any(|capability| capability == "vo_web")
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FrameworkContract {
    pub name: String,
    pub entry: String,
    pub capabilities: Vec<String>,
    pub renderer_path: Option<String>,
    pub protocol_path: Option<String>,
    pub host_bridge_path: Option<String>,
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

fn parse_wasm_ext_manifest(manifest_path: &Path) -> Result<Option<WasmExtensionManifestSpec>, String> {
    let content = std::fs::read_to_string(manifest_path)
        .map_err(|error| format!("{}: {}", manifest_path.display(), error))?;
    let value: Value = toml::from_str(&content)
        .map_err(|error| format!("{}: {}", manifest_path.display(), error))?;
    let Some(extension) = value.get("extension").and_then(Value::as_table) else {
        return Ok(None);
    };
    let Some(wasm_table) = extension.get("wasm").and_then(Value::as_table) else {
        return Ok(None);
    };
    let name = extension
        .get("name")
        .and_then(Value::as_str)
        .ok_or_else(|| format!("{}: missing [extension].name", manifest_path.display()))?
        .to_string();
    let kind = match wasm_table.get("type").and_then(Value::as_str) {
        Some("standalone") => WasmExtensionKind::Standalone,
        Some("bindgen") => WasmExtensionKind::Bindgen,
        Some(other) => {
            return Err(format!(
                "{}: unsupported [extension.wasm].type {}",
                manifest_path.display(),
                other,
            ))
        }
        None => {
            return Err(format!(
                "{}: missing [extension.wasm].type",
                manifest_path.display(),
            ))
        }
    };
    let wasm_filename = wasm_table
        .get("wasm")
        .and_then(Value::as_str)
        .ok_or_else(|| format!("{}: missing [extension.wasm].wasm", manifest_path.display()))?
        .to_string();
    let js_glue_filename = wasm_table
        .get("js_glue")
        .and_then(Value::as_str)
        .map(str::to_string);
    match kind {
        WasmExtensionKind::Standalone => {
            if js_glue_filename.is_some() {
                return Err(format!(
                    "{}: standalone [extension.wasm] must not declare js_glue",
                    manifest_path.display(),
                ));
            }
        }
        WasmExtensionKind::Bindgen => {
            if js_glue_filename.as_deref().unwrap_or("").trim().is_empty() {
                return Err(format!(
                    "{}: bindgen [extension.wasm] must declare js_glue",
                    manifest_path.display(),
                ));
            }
        }
    }
    Ok(Some(WasmExtensionManifestSpec {
        name,
        kind,
        wasm_filename,
    }))
}

fn prepare_wasm_extensions(extensions: &[vo_engine::PreparedNativeExtension]) -> Result<(), String> {
    let mut seen = std::collections::HashSet::new();
    for ext in extensions {
        if !seen.insert(ext.manifest_path.clone()) {
            continue;
        }
        let ext_root = ext.manifest_path.parent().unwrap_or(ext.manifest_path.as_path());
        if let Some(spec) = parse_wasm_ext_manifest(&ext.manifest_path)? {
            if spec.kind == WasmExtensionKind::Standalone {
                ensure_standalone_wasm_ext_fresh(ext_root, &spec)?;
            }
        }
        if let Some(studio) = parse_studio_manifest(&ext.manifest_path) {
            if studio_declares_vo_web(&studio) {
                ensure_pkg_island_fresh(ext_root, &ext.name)?;
            }
        }
    }
    Ok(())
}

#[tauri::command]
pub async fn cmd_run_gui(
    entry_path: String,
    session_id: u64,
    state: tauri::State<'_, AppState>,
    app: AppHandle,
) -> Result<GuiRunOutput, String> {
    state.set_gui_session(session_id);
    state.clear_guest_runtime();
    let session_root = state.session_root();
    let workspace_root = state.workspace_root().to_path_buf();
    let single_file_run = state.single_file_run();
    let task_entry_path = entry_path.clone();
    let task_app = app.clone();
    let (run_output, extensions, handle, push_rx) = run_blocking(move || {
        let run_target = resolve_run_target(&session_root, &workspace_root, &task_entry_path, single_file_run)?;
        let compile_path = run_target.compile_path.to_string_lossy().to_string();
        let compile_start = Instant::now();
        let compile_output = with_compile_log_sink(
            gui_runtime::make_studio_log_sink(task_app.clone(), session_id),
            || prepare_and_compile(&compile_path).map_err(|error| error.to_string()),
        )?;
        gui_runtime::emit_studio_log(
            &task_app,
            session_id,
            gui_runtime::StudioLogRecord::new("studio-native", "gui_compile_done", "system")
                .path(task_entry_path.clone())
                .duration_ms(compile_start.elapsed().as_millis()),
        );
        let module_bytes = compile_output.module.serialize();
        let framework = extract_framework_contract(&compile_output.extensions);
        let extensions = compile_output.extensions.clone();
        prepare_wasm_extensions(&compile_output.extensions)?;
        let start_start = Instant::now();
        let (render_bytes, handle, push_rx) = gui_runtime::run_gui(compile_output, task_app.clone(), session_id)
            .map_err(|error| error.to_string())?;
        gui_runtime::emit_studio_log(
            &task_app,
            session_id,
            gui_runtime::StudioLogRecord::new("studio-native", "gui_start_done", "system")
                .path(task_entry_path.clone())
                .duration_ms(start_start.elapsed().as_millis()),
        );
        let external_widget_handler_id = find_on_widget_handler_id(&render_bytes);
        Ok((
            GuiRunOutput {
                render_bytes,
                module_bytes,
                entry_path: task_entry_path,
                framework,
                external_widget_handler_id,
            },
            extensions,
            handle,
            push_rx,
        ))
    }).await?;
    if state.gui_session_id() != session_id {
        return Err("GUI session superseded".to_string());
    }
    state.set_last_extensions(extensions);
    state.install_guest_runtime(session_id, handle, push_rx);
    Ok(run_output)
}

#[tauri::command]
pub fn cmd_send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    let bytes = state.with_guest(|handle| handle.send_event(handler_id, &payload))?;
    Ok(Response::new(bytes))
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
pub fn cmd_push_island_transport(
    data: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_guest(|handle| handle.push_island_data(&data))
}

#[tauri::command]
pub fn cmd_poll_gui_render(state: tauri::State<'_, AppState>) -> Response {
    Response::new(state.poll_gui_render())
}

#[tauri::command]
pub fn cmd_stop_gui(state: tauri::State<'_, AppState>) -> Result<(), String> {
    state.set_gui_session(0);
    state.clear_guest_runtime();
    Ok(())
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RendererBridgeVfsFile {
    path: String,
    bytes: Vec<u8>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RendererBridgeVfsSnapshot {
    root_path: String,
    files: Vec<RendererBridgeVfsFile>,
}

#[tauri::command]
pub fn cmd_get_renderer_bridge_vfs_snapshot(
    entry_path: String,
    state: tauri::State<'_, AppState>,
) -> Result<RendererBridgeVfsSnapshot, String> {
    let session_root = state.session_root();
    let run_target = resolve_run_target(&session_root, state.workspace_root(), &entry_path, state.single_file_run())?;
    let root_path = run_target.source_root;
    let mut files = collect_renderer_bridge_vfs_files(&root_path)?;
    let mut seen_extension_manifests = std::collections::HashSet::new();

    // Append framework extension files (renderer JS + WASM island) from their package directories.
    for ext in state.last_extensions() {
        if !seen_extension_manifests.insert(ext.manifest_path.clone()) {
            continue;
        }
        let Some(studio) = parse_studio_manifest(&ext.manifest_path) else { continue };
        let ext_root = ext.manifest_path.parent().unwrap_or(ext.manifest_path.as_path());

        // Include directories containing framework artifacts (renderer, protocol, host_bridge).
        let artifact_paths: Vec<&PathBuf> = [
            studio.renderer_path.as_ref(),
            studio.protocol_path.as_ref(),
            studio.host_bridge_path.as_ref(),
        ]
        .into_iter()
        .flatten()
        .collect();
        let mut collected_dirs = std::collections::HashSet::new();
        for artifact_path in &artifact_paths {
            if artifact_path.is_file() {
                if let Some(artifact_dir) = artifact_path.parent() {
                    if collected_dirs.insert(artifact_dir.to_path_buf()) {
                        match collect_renderer_bridge_vfs_files(artifact_dir) {
                            Ok(extra) => files.extend(extra),
                            Err(e) => eprintln!("[studio] failed to read artifact dir {}: {}", artifact_dir.display(), e),
                        }
                        if let Ok(extra) = collect_renderer_bridge_vfs_files_virtual(artifact_dir, ext_root, "") {
                            files.extend(extra);
                        }
                    }
                }
            }
        }

        // Include all files from <ext_root>/rust/pkg-island/ if it exists (WASM island outputs).
        let pkg_island = ext_root.join("rust").join("pkg-island");
        if studio_declares_vo_web(&studio) && pkg_island.is_dir() {
            if let Ok(extra) = collect_renderer_bridge_vfs_files(&pkg_island) {
                files.extend(extra);
            }
            if let Ok(extra) = collect_renderer_bridge_vfs_files_virtual(&pkg_island, &pkg_island, "wasm") {
                files.extend(extra);
            }
        }
    }

    Ok(RendererBridgeVfsSnapshot {
        root_path: root_path.to_string_lossy().to_string(),
        files,
    })
}

fn ensure_standalone_wasm_ext_fresh(
    ext_root: &Path,
    spec: &WasmExtensionManifestSpec,
) -> Result<(), String> {
    let rust_root = ext_root.join("rust");
    let cargo_toml = rust_root.join("Cargo.toml");
    if !cargo_toml.is_file() {
        return Ok(());
    }
    let build = select_wasm_build_candidate(&rust_root, &spec.name, "wasm-standalone")?;
    let workspace_root = select_cargo_workspace_root(&rust_root, &build.crate_root)?;
    let build_output = standalone_wasm_build_output_path(&workspace_root, &build)?;
    if standalone_wasm_target_needs_build(&rust_root, &build.crate_root, &build_output)? {
        eprintln!(
            "[studio] building standalone wasm for {} from {}",
            spec.name,
            build.crate_root.display(),
        );
        let output = std::process::Command::new("cargo")
            .current_dir(&build.crate_root)
            .args([
                "build",
                "--target", "wasm32-unknown-unknown",
                "--release",
                "--no-default-features",
                "--features", "wasm-standalone",
            ])
            .output()
            .map_err(|error| format!("cargo: {}", error))?;
        if !output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!(
                "failed to build standalone wasm for {}:\n{}\n{}",
                spec.name,
                stdout.trim_end(),
                stderr.trim_end(),
            ));
        }
    }
    let wasm_path = ext_root.join(&spec.wasm_filename);
    sync_standalone_wasm_output(&build_output, &wasm_path)
}

fn ensure_pkg_island_fresh(ext_root: &Path, ext_name: &str) -> Result<(), String> {
    let rust_root = ext_root.join("rust");
    let cargo_toml = rust_root.join("Cargo.toml");
    if !cargo_toml.is_file() {
        return Ok(());
    }
    let build = select_wasm_build_candidate(&rust_root, ext_name, "wasm-island")?;
    if !pkg_island_needs_build(&rust_root, &build.crate_root, ext_name)? {
        return Ok(());
    }
    let out_dir = rust_root.join("pkg-island");
    let out_name = format!("{}_island", ext_name);
    eprintln!(
        "[studio] building render-island wasm for {} from {}",
        ext_name,
        build.crate_root.display(),
    );
    let output = std::process::Command::new("wasm-pack")
        .args([
            "build",
            "--target", "web",
            "--out-dir", &out_dir.to_string_lossy(),
            "--out-name", &out_name,
            "--release",
            &build.crate_root.to_string_lossy(),
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

fn select_cargo_workspace_root(rust_root: &Path, crate_root: &Path) -> Result<PathBuf, String> {
    let mut current = Some(crate_root);
    while let Some(path) = current {
        if !path.starts_with(rust_root) {
            break;
        }
        let cargo_toml = path.join("Cargo.toml");
        if cargo_toml.is_file() {
            let content = std::fs::read_to_string(&cargo_toml)
                .map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
            let value: Value = toml::from_str(&content)
                .map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
            if value.get("workspace").and_then(Value::as_table).is_some() {
                return Ok(path.to_path_buf());
            }
        }
        if path == rust_root {
            break;
        }
        current = path.parent();
    }
    Ok(rust_root.to_path_buf())
}

fn standalone_wasm_build_output_path(
    workspace_root: &Path,
    build: &WasmBuildCandidate,
) -> Result<PathBuf, String> {
    let stem = build
        .lib_name
        .clone()
        .or_else(|| build.package_name.clone().map(|name| name.replace('-', "_")))
        .ok_or_else(|| {
            format!(
                "{}: missing [package].name and [lib].name",
                build.crate_root.join("Cargo.toml").display(),
            )
        })?;
    Ok(workspace_root
        .join("target")
        .join("wasm32-unknown-unknown")
        .join("release")
        .join(format!("{}.wasm", stem)))
}

fn standalone_wasm_target_needs_build(
    rust_root: &Path,
    crate_root: &Path,
    target_output: &Path,
) -> Result<bool, String> {
    let Some(output_mtime) = file_modified(target_output)? else {
        return Ok(true);
    };
    let mut newest_input = None;
    update_newest_modified(&mut newest_input, file_modified(&rust_root.join("Cargo.toml"))?);
    update_newest_modified(&mut newest_input, file_modified(&rust_root.join("Cargo.lock"))?);
    if crate_root != rust_root {
        update_newest_modified(&mut newest_input, file_modified(&crate_root.join("Cargo.toml"))?);
    }
    update_newest_modified(&mut newest_input, newest_modified_in_dir(&crate_root.join("src"))?);
    Ok(newest_input.map(|mtime| mtime > output_mtime).unwrap_or(false))
}

fn sync_standalone_wasm_output(target_output: &Path, wasm_path: &Path) -> Result<(), String> {
    let Some(target_mtime) = file_modified(target_output)? else {
        return Err(format!("missing standalone wasm build output {}", target_output.display()));
    };
    let wasm_mtime = file_modified(wasm_path)?;
    if wasm_mtime.map(|mtime| mtime >= target_mtime).unwrap_or(false) {
        return Ok(());
    }
    if let Some(parent) = wasm_path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|error| format!("{}: {}", parent.display(), error))?;
    }
    std::fs::copy(target_output, wasm_path)
        .map_err(|error| format!("{} -> {}: {}", target_output.display(), wasm_path.display(), error))?;
    Ok(())
}

fn pkg_island_needs_build(rust_root: &Path, crate_root: &Path, ext_name: &str) -> Result<bool, String> {
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
    update_newest_modified(&mut newest_input, file_modified(&crate_root.join("Cargo.toml"))?);
    update_newest_modified(&mut newest_input, newest_modified_in_dir(&crate_root.join("src"))?);

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

fn collect_renderer_bridge_vfs_files(root: &Path) -> Result<Vec<RendererBridgeVfsFile>, String> {
    fn walk(dir: &Path, out: &mut Vec<RendererBridgeVfsFile>) -> Result<(), String> {
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
            out.push(RendererBridgeVfsFile {
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

fn collect_renderer_bridge_vfs_files_virtual(
    root: &Path,
    strip_prefix: &Path,
    virtual_prefix: &str,
) -> Result<Vec<RendererBridgeVfsFile>, String> {
    fn walk(
        dir: &Path,
        strip_prefix: &Path,
        virtual_prefix: &str,
        out: &mut Vec<RendererBridgeVfsFile>,
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
            out.push(RendererBridgeVfsFile {
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

fn extract_framework_contract(extensions: &[vo_engine::PreparedNativeExtension]) -> Option<FrameworkContract> {
    for ext in extensions {
        if let Some(studio) = parse_studio_manifest(&ext.manifest_path) {
            return Some(FrameworkContract {
                name: ext.name.clone(),
                entry: studio.entry,
                capabilities: studio.capabilities,
                renderer_path: studio.renderer_path.map(|p| p.to_string_lossy().to_string()),
                protocol_path: studio.protocol_path.map(|p| p.to_string_lossy().to_string()),
                host_bridge_path: studio.host_bridge_path.map(|p| p.to_string_lossy().to_string()),
            });
        }
    }
    None
}
