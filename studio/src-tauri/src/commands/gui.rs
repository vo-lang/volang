use std::collections::BTreeMap;
use std::time::Instant;

use tauri::ipc::Response;
use tauri::AppHandle;
use vo_engine::{default_mod_cache_root, with_compile_log_sink};

use super::run_blocking;
use crate::commands::compiler::prepare_and_compile;
use crate::commands::pathing::resolve_run_target;
use crate::gui_runtime;
use crate::state::AppState;

#[derive(serde::Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct FrameworkContract {
    pub name: String,
    pub entry: Option<String>,
    pub capabilities: Vec<String>,
    pub js_modules: BTreeMap<String, String>,
}

fn framework_contract_from_runtime_contract(contract: vo_web::BrowserRuntimeContract) -> FrameworkContract {
    FrameworkContract {
        name: contract.name,
        entry: contract.entry,
        capabilities: contract.capabilities,
        js_modules: contract.js_modules,
    }
}

fn split_framework_contracts(
    runtime: &vo_web::BrowserRuntimePlan,
) -> (Option<FrameworkContract>, Vec<FrameworkContract>) {
    let split = runtime.primary_framework_split();
    (
        split
            .primary_framework
            .map(framework_contract_from_runtime_contract),
        split
            .provider_frameworks
            .into_iter()
            .map(framework_contract_from_runtime_contract)
            .collect(),
    )
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GuiRunOutput {
    render_bytes: Vec<u8>,
    module_bytes: Vec<u8>,
    entry_path: String,
    framework: Option<FrameworkContract>,
    provider_frameworks: Vec<FrameworkContract>,
    host_widget_handler_id: Option<i32>,
}

/// Decode binary render bytes and find the `onWidget` handler ID of a `vo-host-widget` node.
fn find_on_widget_handler_id(bytes: &[u8]) -> Option<i32> {
    let frame = vogui_protocol::decode_binary_render(bytes).ok()?;
    vogui_protocol::query::find_host_widget_handler_id(&frame)
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
    let (run_output, runtime_plan, handle, push_rx) = run_blocking(move || {
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
        let local_extension_manifests = compile_output
            .extensions
            .iter()
            .map(|spec| spec.manifest_path.clone())
            .collect::<Vec<_>>();
        // Native Studio must mirror the actual compiled GUI program. Derive
        // browser runtime contracts from the native extensions that were
        // linked into this build, then merge any remaining published modules.
        let runtime_plan = vo_web::native_gui_browser_runtime_plan_from_fs(
            &local_extension_manifests,
            &compile_output.locked_modules,
            &default_mod_cache_root(),
        )?;
        let artifact_intent = runtime_plan.artifact_intent()?;
        let artifact_plan = vo_web::browser_artifact_plan_from_fs(&artifact_intent, &runtime_plan)?;
        vo_web::execute_browser_artifact_plan(&artifact_plan)?;
        let (framework, provider_frameworks) = split_framework_contracts(&runtime_plan);
        let start_start = Instant::now();
        let (render_bytes, handle, push_rx) = gui_runtime::run_gui(compile_output, task_app.clone(), session_id)
            .map_err(|error| error.to_string())?;
        let host_widget_handler_id = find_on_widget_handler_id(&render_bytes);
        gui_runtime::emit_studio_log(
            &task_app,
            session_id,
            gui_runtime::StudioLogRecord::new("studio-native", "gui_start_done", "system")
                .path(task_entry_path.clone())
                .duration_ms(start_start.elapsed().as_millis()),
        );
        Ok((
            GuiRunOutput {
                render_bytes,
                module_bytes,
                entry_path: task_entry_path,
                framework,
                provider_frameworks,
                host_widget_handler_id,
            },
            runtime_plan,
            handle,
            push_rx,
        ))
    }).await?;
    if state.gui_session_id() != session_id {
        return Err("GUI session superseded".to_string());
    }
    state.set_last_browser_runtime(runtime_plan);
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
    let runtime = state
        .last_browser_runtime()
        .ok_or_else(|| "missing GUI browser runtime plan; call cmd_run_gui first".to_string())?;
    let snapshot = runtime.snapshot_plan(vo_web::BrowserSnapshotRoot::ProjectRoot)?;
    let files = vo_web::materialize_browser_snapshot_from_fs(
        &snapshot,
        &runtime,
        Some(root_path.as_path()),
        &root_path,
    )?
    .into_iter()
    .map(|file| RendererBridgeVfsFile {
        path: file.path,
        bytes: file.bytes,
    })
    .collect();

    Ok(RendererBridgeVfsSnapshot {
        root_path: root_path.to_string_lossy().to_string(),
        files,
    })
}
