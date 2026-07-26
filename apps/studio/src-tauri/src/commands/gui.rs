use std::collections::BTreeMap;
use std::time::Instant;

use tauri::ipc::Response;
use tauri::{AppHandle, Manager};
use vo_engine::{default_mod_cache_root, with_compile_log_sink};

use super::run_blocking;
use crate::app_plan::materialize_native_studio_plan;
use crate::commands::compiler::prepare_and_compile;
use crate::commands::pathing::resolve_run_target;
use crate::gui_runtime;
use crate::state::{AppState, StudioSessionHandle};

#[derive(serde::Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct FrameworkContract {
    pub module_key: String,
    pub name: String,
    pub entry: Option<String>,
    pub provider_role: Option<String>,
    pub provider_roles: Vec<String>,
    pub capabilities: Vec<String>,
    pub roles: Vec<String>,
    pub js_modules: BTreeMap<String, String>,
}

fn framework_contract_from_runtime_contract(
    contract: vo_web::BrowserRuntimeContract,
) -> FrameworkContract {
    FrameworkContract {
        module_key: contract.module_key,
        name: contract.name,
        entry: contract.entry,
        provider_role: contract
            .provider_role
            .map(|provider_role| provider_role.as_str().to_string()),
        provider_roles: contract
            .provider_roles
            .into_iter()
            .map(|provider_role| provider_role.as_str().to_string())
            .collect(),
        capabilities: contract.capabilities,
        roles: contract.roles,
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
    preview_handle: StudioSessionHandle,
    session_epoch: String,
    bridge_epoch: String,
    render_bytes: Vec<u8>,
    module_bytes: Vec<u8>,
    entry_path: String,
    framework: Option<FrameworkContract>,
    provider_frameworks: Vec<FrameworkContract>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FrameworkLaneHandle {
    index: u32,
    generation: u32,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FrameworkLaneCaller {
    session_index: u32,
    session_generation: u32,
    session_epoch: String,
    endpoint_index: u32,
    endpoint_generation: u32,
    endpoint_epoch: String,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FrameworkLaneBinding {
    session: FrameworkLaneHandle,
    session_epoch: String,
    caller: FrameworkLaneCaller,
    channel: FrameworkLaneHandle,
    channel_epoch: String,
    selected_minor: u16,
    selected_exact_fingerprint: Vec<u8>,
    max_packet_bytes: u32,
    max_messages: u32,
    max_bytes: u32,
}

impl From<vo_app_runtime::EndpointChannelBinding> for FrameworkLaneBinding {
    fn from(binding: vo_app_runtime::EndpointChannelBinding) -> Self {
        Self {
            session: FrameworkLaneHandle {
                index: binding.session.index,
                generation: binding.session.generation,
            },
            session_epoch: binding.session_epoch.to_string(),
            caller: FrameworkLaneCaller {
                session_index: binding.caller.session_index,
                session_generation: binding.caller.session_generation,
                session_epoch: binding.caller.session_epoch.to_string(),
                endpoint_index: binding.caller.endpoint_index,
                endpoint_generation: binding.caller.endpoint_generation,
                endpoint_epoch: binding.caller.endpoint_epoch.to_string(),
            },
            channel: FrameworkLaneHandle {
                index: binding.channel.index,
                generation: binding.channel.generation,
            },
            channel_epoch: binding.channel_epoch.to_string(),
            selected_minor: binding.selected_minor,
            selected_exact_fingerprint: binding.selected_exact_fingerprint.to_vec(),
            max_packet_bytes: binding.limits.max_packet_bytes,
            max_messages: binding.limits.max_messages,
            max_bytes: binding.limits.max_bytes,
        }
    }
}

#[derive(Clone, Copy, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DisplayTimingHandle {
    index: u32,
    generation: u32,
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DisplayTimingRequest {
    view: DisplayTimingHandle,
    request_sequence: String,
}

impl From<vo_app_runtime::DisplayTimingRequest> for DisplayTimingRequest {
    fn from(request: vo_app_runtime::DisplayTimingRequest) -> Self {
        Self {
            view: DisplayTimingHandle {
                index: request.view.index,
                generation: request.view.generation,
            },
            request_sequence: request.request_sequence.to_string(),
        }
    }
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DisplayPulseSubmission {
    emitted_domains: usize,
}

#[tauri::command]
pub async fn cmd_run_gui(
    entry_path: String,
    session_id: u64,
    state: tauri::State<'_, AppState>,
    app: AppHandle,
) -> Result<GuiRunOutput, String> {
    let session = state.session_snapshot();
    let session_root = session.root().to_path_buf();
    let workspace_root = state.workspace_root().to_path_buf();
    let single_file_run = session.single_file_run();
    let project_options = session.project_context_options();
    let task_entry_path = entry_path.clone();
    let available_host_probes = if app.webview_windows().is_empty() {
        vo_app_runtime::HostProbeRequirements::NONE
    } else {
        vo_app_runtime::HostProbeRequirements::WEBVIEW_PROCESS
    };
    let task_app = app.clone();
    let hosted_runtime = state.hosted_runtime();
    let (
        run_output,
        runtime_plan,
        materialized_browser_artifacts,
        retained_plan,
        app_session,
        handle,
        push_rx,
    ) = run_blocking(move || {
        let run_target = resolve_run_target(
            &session_root,
            &workspace_root,
            &task_entry_path,
            single_file_run,
        )?;
        let compile_path = run_target.compile_path.to_string_lossy().to_string();
        let compile_start = Instant::now();
        let compile_output = with_compile_log_sink(
            gui_runtime::make_studio_log_sink(task_app.clone(), session_id),
            || {
                prepare_and_compile(&compile_path, &project_options)
                    .map_err(|error| error.to_string())
            },
        )?;
        gui_runtime::emit_studio_log(
            &task_app,
            session_id,
            gui_runtime::StudioLogRecord::new("studio-native", "gui_compile_done", "system")
                .path(task_entry_path.clone())
                .duration_ms(compile_start.elapsed().as_millis()),
        );
        let module_bytes = compile_output
            .module
            .serialize()
            .map_err(|error| format!("failed to serialize compiled GUI bytecode: {error}"))?;
        let local_extension_manifests = compile_output
            .extensions
            .iter()
            .map(|spec| spec.manifest_path.clone())
            .collect::<Vec<_>>();
        // Native Studio must mirror the actual compiled GUI program. Derive
        // browser runtime contracts from the native extensions that were
        // linked into this build, then merge any remaining published modules.
        let mod_cache = default_mod_cache_root().map_err(|error| error.to_string())?;
        let runtime_plan = vo_web::native_gui_browser_runtime_plan_from_fs(
            &local_extension_manifests,
            &compile_output.locked_modules,
            &mod_cache,
        )?;
        let artifact_intent = runtime_plan.artifact_intent()?;
        let artifact_plan = vo_web::browser_artifact_plan_from_fs(&artifact_intent, &runtime_plan)?;
        vo_web::execute_browser_artifact_plan(&artifact_plan)?;
        let materialized_browser_artifacts =
            vo_web::materialized_browser_artifacts_from_fs(&artifact_intent, &runtime_plan)?;
        let plan_generation = session_id
            .checked_add(1)
            .ok_or_else(|| String::from("Studio AppBuildPlan generation exhausted"))?;
        let resolved_plan = materialize_native_studio_plan(
            &module_bytes,
            &runtime_plan,
            &materialized_browser_artifacts,
            &compile_output.extensions,
            &compile_output.locked_modules,
            plan_generation,
            available_host_probes,
        )?;
        let retained_plan = resolved_plan.clone();
        let (framework, provider_frameworks) = split_framework_contracts(&runtime_plan);
        let start_start = Instant::now();
        let (render_bytes, app_session, handle, push_rx) = gui_runtime::run_gui(
            compile_output,
            task_app.clone(),
            session_id,
            hosted_runtime,
            resolved_plan,
        )
        .map_err(|error| error.to_string())?;
        gui_runtime::emit_studio_log(
            &task_app,
            session_id,
            gui_runtime::StudioLogRecord::new("studio-native", "gui_start_done", "system")
                .path(task_entry_path.clone())
                .duration_ms(start_start.elapsed().as_millis()),
        );
        Ok((
            (
                render_bytes,
                module_bytes,
                task_entry_path,
                framework,
                provider_frameworks,
            ),
            runtime_plan,
            materialized_browser_artifacts,
            retained_plan,
            app_session,
            handle,
            push_rx,
        ))
    })
    .await?;
    let expected_plan_generation = retained_plan.plan_generation;
    let preview_handle = state.install_guest_runtime(
        app_session,
        handle,
        push_rx,
        runtime_plan,
        materialized_browser_artifacts,
        retained_plan,
    )?;
    let retained_artifact_count = state.browser_artifacts(preview_handle)?.len();
    if retained_artifact_count > vo_web::MAX_BROWSER_RUNTIME_ITEMS.saturating_mul(2) {
        state.close_guest_runtime(preview_handle)?;
        return Err(String::from(
            "Studio retained an invalid number of materialized browser artifacts",
        ));
    }
    if state.resolved_app_plan(preview_handle)?.plan_generation != expected_plan_generation {
        state.close_guest_runtime(preview_handle)?;
        return Err(String::from(
            "Studio retained a different resolved AppRuntimePlan generation",
        ));
    }
    let (session_epoch, bridge_epoch) = state.webview_bridge_identity(preview_handle)?;
    let (render_bytes, module_bytes, entry_path, framework, provider_frameworks) = run_output;
    Ok(GuiRunOutput {
        preview_handle,
        session_epoch: session_epoch.to_string(),
        bridge_epoch: bridge_epoch.to_string(),
        render_bytes,
        module_bytes,
        entry_path,
        framework,
        provider_frameworks,
    })
}

#[derive(Clone, Copy, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum WebviewBridgeLane {
    Control,
    Completion,
    ReliableInput,
    Framework,
    Presentation,
    Diagnostics,
}

impl From<WebviewBridgeLane> for vo_app_runtime::BridgeLane {
    fn from(lane: WebviewBridgeLane) -> Self {
        match lane {
            WebviewBridgeLane::Control => Self::Control,
            WebviewBridgeLane::Completion => Self::Completion,
            WebviewBridgeLane::ReliableInput => Self::ReliableInput,
            WebviewBridgeLane::Framework => Self::Framework,
            WebviewBridgeLane::Presentation => Self::Presentation,
            WebviewBridgeLane::Diagnostics => Self::Diagnostics,
        }
    }
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct WebviewBridgeRestart {
    old_epoch: String,
    new_epoch: String,
    discarded_to_webview: usize,
    discarded_from_webview: usize,
}

#[tauri::command]
pub fn cmd_attach_webview_bridge(
    preview_handle: StudioSessionHandle,
    bridge_epoch: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let bridge_epoch = bridge_epoch
        .parse::<u64>()
        .map_err(|_| String::from("bridgeEpoch must be an unsigned 64-bit decimal string"))?;
    state.attach_webview_bridge(preview_handle, bridge_epoch)
}

#[tauri::command]
pub fn cmd_enqueue_webview_bridge(
    preview_handle: StudioSessionHandle,
    lane: WebviewBridgeLane,
    coalesce_key: String,
    payload: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let coalesce_key = coalesce_key
        .parse::<u64>()
        .map_err(|_| String::from("coalesceKey must be an unsigned 64-bit decimal string"))?;
    state.enqueue_webview_bridge(preview_handle, lane.into(), coalesce_key, payload)
}

#[tauri::command]
pub fn cmd_stage_webview_restart_snapshot(
    preview_handle: StudioSessionHandle,
    snapshot_key: String,
    payload: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let snapshot_key = snapshot_key
        .parse::<u64>()
        .map_err(|_| String::from("snapshotKey must be an unsigned 64-bit decimal string"))?;
    state.stage_webview_restart_snapshot(preview_handle, snapshot_key, payload)
}

#[tauri::command]
pub fn cmd_poll_webview_bridge(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    Ok(Response::new(state.poll_webview_bridge(preview_handle)?))
}

#[tauri::command]
pub fn cmd_submit_webview_bridge(
    preview_handle: StudioSessionHandle,
    frame: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.submit_webview_bridge(preview_handle, &frame)
}

#[tauri::command]
pub fn cmd_take_webview_bridge_input(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    Ok(Response::new(
        state.take_webview_bridge_input(preview_handle)?,
    ))
}

#[tauri::command]
pub fn cmd_restart_webview_bridge(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<WebviewBridgeRestart, String> {
    state
        .restart_webview_bridge(preview_handle)
        .map(|report| WebviewBridgeRestart {
            old_epoch: report.old_epoch.to_string(),
            new_epoch: report.new_epoch.to_string(),
            discarded_to_webview: report.discarded_to_webview,
            discarded_from_webview: report.discarded_from_webview,
        })
}

#[derive(Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WebviewBridgeRestartSnapshot {
    key: String,
    payload: Vec<u8>,
}

#[tauri::command]
pub fn cmd_restart_webview_bridge_with_snapshots(
    preview_handle: StudioSessionHandle,
    snapshots: Vec<WebviewBridgeRestartSnapshot>,
    state: tauri::State<'_, AppState>,
) -> Result<WebviewBridgeRestart, String> {
    let mut decoded = Vec::with_capacity(snapshots.len());
    for snapshot in snapshots {
        let key = snapshot
            .key
            .parse::<u64>()
            .map_err(|_| String::from("snapshot key must be an unsigned 64-bit decimal string"))?;
        decoded.push((key, snapshot.payload));
    }
    state
        .restart_webview_bridge_with_snapshots(preview_handle, decoded)
        .map(|report| WebviewBridgeRestart {
            old_epoch: report.old_epoch.to_string(),
            new_epoch: report.new_epoch.to_string(),
            discarded_to_webview: report.discarded_to_webview,
            discarded_from_webview: report.discarded_from_webview,
        })
}

#[derive(Clone, Copy, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformHandle {
    index: u32,
    generation: u32,
}

impl From<PlatformHandle> for vo_app_runtime::GenerationalHandle {
    fn from(handle: PlatformHandle) -> Self {
        Self {
            index: handle.index,
            generation: handle.generation,
        }
    }
}

impl From<vo_app_runtime::GenerationalHandle> for PlatformHandle {
    fn from(handle: vo_app_runtime::GenerationalHandle) -> Self {
        Self {
            index: handle.index,
            generation: handle.generation,
        }
    }
}

#[derive(Clone, Copy, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformViewInsets {
    top_milli: u32,
    right_milli: u32,
    bottom_milli: u32,
    left_milli: u32,
}

#[derive(Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformViewMetricsUpdate {
    expected_revision: String,
    origin_x_milli: i32,
    origin_y_milli: i32,
    width_milli: u32,
    height_milli: u32,
    framebuffer_width: u32,
    framebuffer_height: u32,
    scale_q16: u32,
    safe_area: PlatformViewInsets,
    visibility: String,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformViewMetricsResult {
    revision: String,
    scale_q16: u32,
    framebuffer_width: u32,
    framebuffer_height: u32,
}

#[derive(Clone, Copy, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformRect {
    x_milli: i32,
    y_milli: i32,
    width_milli: u32,
    height_milli: u32,
}

#[derive(Clone, Copy, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformTransform {
    m11_q16: i32,
    m12_q16: i32,
    m21_q16: i32,
    m22_q16: i32,
    translate_x_milli: i32,
    translate_y_milli: i32,
}

#[derive(Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformSurfaceDescriptor {
    view: PlatformHandle,
    kind: String,
    z_order: i32,
    input_policy: String,
    accepts_text: bool,
    bounds: Option<PlatformRect>,
    clip: Option<PlatformRect>,
    transform: PlatformTransform,
    opacity_q16: u16,
    hit_test_enabled: bool,
}

#[derive(Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformSurfaceGeometryUpdate {
    expected_revision: String,
    bounds: Option<PlatformRect>,
    clip: Option<PlatformRect>,
    transform: PlatformTransform,
    opacity_q16: u16,
    hit_test_enabled: bool,
}

#[derive(Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformInputHeader {
    sequence: String,
    timestamp_micros: String,
    metrics_revision: String,
    window: PlatformHandle,
    view: PlatformHandle,
    device_id: String,
    device_generation: u32,
    device_kind: String,
    modifier_flags: u32,
}

#[derive(Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformGamepadButton {
    value_q15: u16,
    pressed: bool,
    touched: bool,
}

#[derive(Clone, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum PlatformInputPayload {
    Pointer {
        contact: u32,
        phase: String,
        x_milli: i32,
        y_milli: i32,
        delta_x_milli: i32,
        delta_y_milli: i32,
        pressure_q15: u16,
        tilt_x_degrees: i16,
        tilt_y_degrees: i16,
        buttons: u32,
        changed_button: Option<u8>,
    },
    Wheel {
        contact: u32,
        x_milli: i32,
        y_milli: i32,
        delta_x_milli: i32,
        delta_y_milli: i32,
        unit: String,
    },
    Key {
        phase: String,
        physical_key: u32,
        logical_key: String,
        repeat: bool,
    },
    Shortcut {
        class_mask: String,
        system: bool,
    },
    Text {
        text: String,
    },
    Composition {
        phase: String,
        text: String,
        selection_start: u32,
        selection_end: u32,
    },
    GamepadSnapshot {
        connected: bool,
        mapping: String,
        axes_q15: Vec<i16>,
        buttons: Vec<PlatformGamepadButton>,
    },
    FocusChanged {
        focused: bool,
    },
    VisibilityChanged {
        visible: bool,
    },
    DeviceDisconnected,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformInputResult {
    composition_revision: String,
    synthesized_release_count: usize,
    arbitrated: bool,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformSurfaceStatus {
    surface: PlatformHandle,
    surface_generation: String,
    state: String,
    last_outcome: Option<String>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformSurfaceRoute {
    session: PlatformHandle,
    session_epoch: String,
    window: PlatformHandle,
    view: PlatformHandle,
    surface: PlatformHandle,
    kind: String,
    z_order: i32,
    input_policy: String,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformSystemShortcutRegistration {
    class_mask: String,
    scope: String,
    priority: i16,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformSurfaceRecoveryTicket {
    surface: PlatformHandle,
    old_generation: String,
    new_generation: String,
}

fn parse_runtime_u64(value: &str, field: &str) -> Result<u64, String> {
    value
        .parse::<u64>()
        .map_err(|_| format!("{field} must be an unsigned 64-bit decimal string"))
}

fn platform_rect(rect: PlatformRect) -> vo_app_runtime::SurfaceRect {
    vo_app_runtime::SurfaceRect {
        x_milli: rect.x_milli,
        y_milli: rect.y_milli,
        width_milli: rect.width_milli,
        height_milli: rect.height_milli,
    }
}

fn platform_transform(transform: PlatformTransform) -> vo_app_runtime::SurfaceTransform {
    vo_app_runtime::SurfaceTransform {
        m11_q16: transform.m11_q16,
        m12_q16: transform.m12_q16,
        m21_q16: transform.m21_q16,
        m22_q16: transform.m22_q16,
        translate_x_milli: transform.translate_x_milli,
        translate_y_milli: transform.translate_y_milli,
    }
}

fn platform_geometry(
    bounds: Option<PlatformRect>,
    clip: Option<PlatformRect>,
    transform: PlatformTransform,
    opacity_q16: u16,
    hit_test_enabled: bool,
) -> vo_app_runtime::SurfaceGeometry {
    vo_app_runtime::SurfaceGeometry {
        bounds: bounds.map(platform_rect),
        clip: clip.map(platform_rect),
        transform: platform_transform(transform),
        opacity_q16,
        hit_test_enabled,
    }
}

fn parse_platform_input(
    header: PlatformInputHeader,
    payload: PlatformInputPayload,
) -> Result<vo_app_runtime::PlatformInputEvent, String> {
    let device_kind = match header.device_kind.as_str() {
        "mouse" => vo_app_runtime::InputDeviceKind::Mouse,
        "touch" => vo_app_runtime::InputDeviceKind::Touch,
        "pen" => vo_app_runtime::InputDeviceKind::Pen,
        "keyboard" => vo_app_runtime::InputDeviceKind::Keyboard,
        "gamepad" => vo_app_runtime::InputDeviceKind::Gamepad,
        value => return Err(format!("unknown input device kind '{value}'")),
    };
    let payload = match payload {
        PlatformInputPayload::Pointer {
            contact,
            phase,
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            pressure_q15,
            tilt_x_degrees,
            tilt_y_degrees,
            buttons,
            changed_button,
        } => vo_app_runtime::PlatformInputPayload::Pointer {
            contact,
            phase: match phase.as_str() {
                "down" => vo_app_runtime::PointerPhase::Down,
                "move" => vo_app_runtime::PointerPhase::Move,
                "up" => vo_app_runtime::PointerPhase::Up,
                "cancel" => vo_app_runtime::PointerPhase::Cancel,
                value => return Err(format!("unknown pointer phase '{value}'")),
            },
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            pressure_q15,
            tilt_x_degrees,
            tilt_y_degrees,
            buttons,
            changed_button,
        },
        PlatformInputPayload::Wheel {
            contact,
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            unit,
        } => vo_app_runtime::PlatformInputPayload::Wheel {
            contact,
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            unit: match unit.as_str() {
                "pixel" => vo_app_runtime::WheelUnit::Pixel,
                "line" => vo_app_runtime::WheelUnit::Line,
                "page" => vo_app_runtime::WheelUnit::Page,
                value => return Err(format!("unknown wheel unit '{value}'")),
            },
        },
        PlatformInputPayload::Key {
            phase,
            physical_key,
            logical_key,
            repeat,
        } => vo_app_runtime::PlatformInputPayload::Key {
            phase: match phase.as_str() {
                "down" => vo_app_runtime::KeyPhase::Down,
                "up" => vo_app_runtime::KeyPhase::Up,
                value => return Err(format!("unknown key phase '{value}'")),
            },
            physical_key,
            logical_key,
            repeat,
        },
        PlatformInputPayload::Shortcut { class_mask, system } => {
            vo_app_runtime::PlatformInputPayload::Shortcut {
                class_mask: parse_runtime_u64(&class_mask, "classMask")?,
                system,
            }
        }
        PlatformInputPayload::Text { text } => vo_app_runtime::PlatformInputPayload::Text { text },
        PlatformInputPayload::Composition {
            phase,
            text,
            selection_start,
            selection_end,
        } => vo_app_runtime::PlatformInputPayload::Composition {
            phase: match phase.as_str() {
                "start" => vo_app_runtime::CompositionPhase::Start,
                "update" => vo_app_runtime::CompositionPhase::Update,
                "end" => vo_app_runtime::CompositionPhase::End,
                "cancel" => vo_app_runtime::CompositionPhase::Cancel,
                value => return Err(format!("unknown composition phase '{value}'")),
            },
            text,
            selection_start,
            selection_end,
        },
        PlatformInputPayload::GamepadSnapshot {
            connected,
            mapping,
            axes_q15,
            buttons,
        } => vo_app_runtime::PlatformInputPayload::GamepadSnapshot {
            connected,
            mapping: match mapping.as_str() {
                "standard" => vo_app_runtime::GamepadMapping::Standard,
                "raw" => vo_app_runtime::GamepadMapping::Raw,
                value => return Err(format!("unknown gamepad mapping '{value}'")),
            },
            axes_q15,
            buttons: buttons
                .into_iter()
                .map(|button| vo_app_runtime::GamepadButton {
                    value_q15: button.value_q15,
                    pressed: button.pressed,
                    touched: button.touched,
                })
                .collect(),
        },
        PlatformInputPayload::FocusChanged { focused } => {
            vo_app_runtime::PlatformInputPayload::FocusChanged { focused }
        }
        PlatformInputPayload::VisibilityChanged { visible } => {
            vo_app_runtime::PlatformInputPayload::VisibilityChanged { visible }
        }
        PlatformInputPayload::DeviceDisconnected => {
            vo_app_runtime::PlatformInputPayload::DeviceDisconnected
        }
    };
    Ok(vo_app_runtime::PlatformInputEvent {
        header: vo_app_runtime::PlatformInputHeader {
            sequence: parse_runtime_u64(&header.sequence, "sequence")?,
            timestamp_micros: parse_runtime_u64(&header.timestamp_micros, "timestampMicros")?,
            metrics_revision: parse_runtime_u64(&header.metrics_revision, "metricsRevision")?,
            window: header.window.into(),
            view: header.view.into(),
            device: vo_app_runtime::InputDeviceId {
                value: parse_runtime_u64(&header.device_id, "deviceId")?,
                generation: header.device_generation,
            },
            device_kind,
            modifiers: vo_app_runtime::InputModifiers {
                shift: header.modifier_flags & 1 != 0,
                control: header.modifier_flags & 2 != 0,
                alt: header.modifier_flags & 4 != 0,
                meta: header.modifier_flags & 8 != 0,
                caps_lock: header.modifier_flags & 16 != 0,
                num_lock: header.modifier_flags & 32 != 0,
            },
        },
        payload,
    })
}

fn parse_surface_outcome(
    value: &str,
) -> Result<vo_app_runtime::SurfacePresentationOutcome, String> {
    match value {
        "presented" => Ok(vo_app_runtime::SurfacePresentationOutcome::Presented),
        "deadline-missed" => Ok(vo_app_runtime::SurfacePresentationOutcome::DeadlineMissed),
        "zero-sized" => Ok(vo_app_runtime::SurfacePresentationOutcome::ZeroSized),
        "suspended" => Ok(vo_app_runtime::SurfacePresentationOutcome::Suspended),
        "timed-out" => Ok(vo_app_runtime::SurfacePresentationOutcome::TimedOut),
        "surface-lost" => Ok(vo_app_runtime::SurfacePresentationOutcome::SurfaceLost),
        "device-lost" => Ok(vo_app_runtime::SurfacePresentationOutcome::DeviceLost),
        _ => Err(format!("unknown Surface presentation outcome '{value}'")),
    }
}

fn surface_outcome_name(outcome: vo_app_runtime::SurfacePresentationOutcome) -> String {
    match outcome {
        vo_app_runtime::SurfacePresentationOutcome::Presented => "presented",
        vo_app_runtime::SurfacePresentationOutcome::DeadlineMissed => "deadline-missed",
        vo_app_runtime::SurfacePresentationOutcome::ZeroSized => "zero-sized",
        vo_app_runtime::SurfacePresentationOutcome::Suspended => "suspended",
        vo_app_runtime::SurfacePresentationOutcome::TimedOut => "timed-out",
        vo_app_runtime::SurfacePresentationOutcome::SurfaceLost => "surface-lost",
        vo_app_runtime::SurfacePresentationOutcome::DeviceLost => "device-lost",
    }
    .to_string()
}

fn platform_surface_status(status: vo_app_runtime::SurfaceStatus) -> PlatformSurfaceStatus {
    PlatformSurfaceStatus {
        surface: status.surface.into(),
        surface_generation: status.generation.to_string(),
        state: match status.state {
            vo_app_runtime::SurfaceRuntimeState::Active => "active",
            vo_app_runtime::SurfaceRuntimeState::Suspended => "suspended",
            vo_app_runtime::SurfaceRuntimeState::Lost => "lost",
            vo_app_runtime::SurfaceRuntimeState::Recovering => "recovering",
        }
        .to_string(),
        last_outcome: status.last_outcome.map(surface_outcome_name),
    }
}

#[tauri::command]
pub fn cmd_create_platform_window(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformHandle, String> {
    state
        .with_guest(preview_handle, |handle| handle.create_window())
        .map(PlatformHandle::from)
}

#[tauri::command]
pub fn cmd_close_platform_window(
    preview_handle: StudioSessionHandle,
    window: PlatformHandle,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_guest(preview_handle, |handle| handle.close_window(window.into()))
}

#[tauri::command]
pub fn cmd_create_platform_view(
    preview_handle: StudioSessionHandle,
    window: PlatformHandle,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformHandle, String> {
    state
        .with_guest(preview_handle, |handle| handle.create_view(window.into()))
        .map(PlatformHandle::from)
}

#[tauri::command]
pub fn cmd_update_platform_view_metrics(
    preview_handle: StudioSessionHandle,
    view: PlatformHandle,
    update: PlatformViewMetricsUpdate,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformViewMetricsResult, String> {
    let visibility = match update.visibility.as_str() {
        "visible" => vo_app_runtime::ViewVisibility::Visible,
        "hidden" => vo_app_runtime::ViewVisibility::Hidden,
        "suspended" => vo_app_runtime::ViewVisibility::Suspended,
        value => return Err(format!("unknown View visibility '{value}'")),
    };
    let expected_revision = parse_runtime_u64(&update.expected_revision, "expectedRevision")?;
    state
        .with_guest(preview_handle, |handle| {
            handle.update_view_metrics(
                view.into(),
                vo_app_runtime::ViewMetricsUpdate {
                    origin_x_milli: update.origin_x_milli,
                    origin_y_milli: update.origin_y_milli,
                    width_milli: update.width_milli,
                    height_milli: update.height_milli,
                    framebuffer_width: update.framebuffer_width,
                    framebuffer_height: update.framebuffer_height,
                    scale_q16: update.scale_q16,
                    safe_area: vo_app_runtime::ViewInsets {
                        top_milli: update.safe_area.top_milli,
                        right_milli: update.safe_area.right_milli,
                        bottom_milli: update.safe_area.bottom_milli,
                        left_milli: update.safe_area.left_milli,
                    },
                    visibility,
                },
                expected_revision,
            )
        })
        .map(|metrics| PlatformViewMetricsResult {
            revision: metrics.revision.to_string(),
            scale_q16: metrics.scale_q16,
            framebuffer_width: metrics.framebuffer_width,
            framebuffer_height: metrics.framebuffer_height,
        })
}

#[tauri::command]
pub fn cmd_close_platform_view(
    preview_handle: StudioSessionHandle,
    view: PlatformHandle,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_guest(preview_handle, |handle| handle.close_view(view.into()))
}

#[tauri::command]
pub fn cmd_attach_platform_surface(
    preview_handle: StudioSessionHandle,
    descriptor: PlatformSurfaceDescriptor,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformHandle, String> {
    let kind = match descriptor.kind.as_str() {
        "game" => vo_app_runtime::SurfaceKind::Game,
        "ui" => vo_app_runtime::SurfaceKind::Ui,
        "diagnostics" => vo_app_runtime::SurfaceKind::Diagnostics,
        value => return Err(format!("unknown Surface kind '{value}'")),
    };
    let input = match descriptor.input_policy.as_str() {
        "observe" => vo_app_runtime::SurfaceInputPolicy::Observe,
        "passthrough" => vo_app_runtime::SurfaceInputPolicy::Passthrough,
        "interactive" => vo_app_runtime::SurfaceInputPolicy::Interactive,
        "exclusive" => vo_app_runtime::SurfaceInputPolicy::Exclusive,
        value => return Err(format!("unknown Surface input policy '{value}'")),
    };
    state
        .with_guest(preview_handle, |handle| {
            handle.attach_surface(vo_app_runtime::SurfaceDescriptor {
                view: descriptor.view.into(),
                kind,
                z_order: descriptor.z_order,
                input,
                accepts_text: descriptor.accepts_text,
                geometry: platform_geometry(
                    descriptor.bounds,
                    descriptor.clip,
                    descriptor.transform,
                    descriptor.opacity_q16,
                    descriptor.hit_test_enabled,
                ),
            })
        })
        .map(PlatformHandle::from)
}

#[tauri::command]
pub fn cmd_resolve_platform_surface(
    preview_handle: StudioSessionHandle,
    surface: PlatformHandle,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformSurfaceRoute, String> {
    state
        .with_guest(preview_handle, |handle| {
            handle.surface_route(surface.into())
        })
        .map(|route| PlatformSurfaceRoute {
            session: route.session.into(),
            session_epoch: route.session_epoch.to_string(),
            window: route.window.into(),
            view: route.view.into(),
            surface: route.surface.into(),
            kind: match route.kind {
                vo_app_runtime::SurfaceKind::Game => "game",
                vo_app_runtime::SurfaceKind::Ui => "ui",
                vo_app_runtime::SurfaceKind::Diagnostics => "diagnostics",
            }
            .to_string(),
            z_order: route.z_order,
            input_policy: match route.input {
                vo_app_runtime::SurfaceInputPolicy::Observe => "observe",
                vo_app_runtime::SurfaceInputPolicy::Passthrough => "passthrough",
                vo_app_runtime::SurfaceInputPolicy::Interactive => "interactive",
                vo_app_runtime::SurfaceInputPolicy::Exclusive => "exclusive",
            }
            .to_string(),
        })
}

#[tauri::command]
pub fn cmd_register_platform_surface_shortcuts(
    preview_handle: StudioSessionHandle,
    surface: PlatformHandle,
    registrations: Vec<PlatformSystemShortcutRegistration>,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let registrations = registrations
        .into_iter()
        .map(|registration| {
            let class_mask = parse_runtime_u64(&registration.class_mask, "classMask")?;
            let scope = match registration.scope.as_str() {
                "view" => vo_app_runtime::ShortcutScope::View,
                "window" => vo_app_runtime::ShortcutScope::Window,
                "session" => vo_app_runtime::ShortcutScope::Session,
                value => return Err(format!("unknown system shortcut scope '{value}'")),
            };
            Ok(vo_app_runtime::SurfaceShortcutRegistration {
                class_mask,
                scope,
                priority: registration.priority,
            })
        })
        .collect::<Result<Vec<_>, String>>()?;
    state
        .with_guest(preview_handle, |handle| {
            handle.register_surface_shortcuts(surface.into(), registrations)
        })
        .map(|revision| revision.to_string())
}

#[tauri::command]
pub fn cmd_update_platform_surface_geometry(
    preview_handle: StudioSessionHandle,
    surface: PlatformHandle,
    update: PlatformSurfaceGeometryUpdate,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let expected_revision = parse_runtime_u64(&update.expected_revision, "expectedRevision")?;
    state
        .with_guest(preview_handle, |handle| {
            handle.update_surface_geometry(
                surface.into(),
                platform_geometry(
                    update.bounds,
                    update.clip,
                    update.transform,
                    update.opacity_q16,
                    update.hit_test_enabled,
                ),
                expected_revision,
            )
        })
        .map(|revision| revision.to_string())
}

#[tauri::command]
pub fn cmd_close_platform_surface(
    preview_handle: StudioSessionHandle,
    surface: PlatformHandle,
    state: tauri::State<'_, AppState>,
) -> Result<usize, String> {
    state
        .with_guest(preview_handle, |handle| {
            handle.close_surface(surface.into())
        })
        .map(|report| report.synthesized_releases.len())
}

#[tauri::command]
pub fn cmd_report_platform_surface_outcome(
    preview_handle: StudioSessionHandle,
    surface: PlatformHandle,
    surface_generation: String,
    outcome: String,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformSurfaceStatus, String> {
    let surface_generation = parse_runtime_u64(&surface_generation, "surfaceGeneration")?;
    let outcome = parse_surface_outcome(&outcome)?;
    state
        .with_guest(preview_handle, |handle| {
            handle.report_surface_outcome(surface.into(), surface_generation, outcome)
        })
        .map(platform_surface_status)
}

#[tauri::command]
pub fn cmd_begin_platform_surface_recovery(
    preview_handle: StudioSessionHandle,
    surface: PlatformHandle,
    expected_generation: String,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformSurfaceRecoveryTicket, String> {
    let expected_generation = parse_runtime_u64(&expected_generation, "expectedGeneration")?;
    state
        .with_guest(preview_handle, |handle| {
            handle.begin_surface_recovery(surface.into(), expected_generation)
        })
        .map(|ticket| PlatformSurfaceRecoveryTicket {
            surface: ticket.surface.into(),
            old_generation: ticket.old_generation.to_string(),
            new_generation: ticket.new_generation.to_string(),
        })
}

#[tauri::command]
pub fn cmd_complete_platform_surface_recovery(
    preview_handle: StudioSessionHandle,
    ticket: PlatformSurfaceRecoveryTicketInput,
    suspended: bool,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformSurfaceStatus, String> {
    let ticket = vo_app_runtime::SurfaceRecoveryTicket {
        surface: ticket.surface.into(),
        old_generation: parse_runtime_u64(&ticket.old_generation, "oldGeneration")?,
        new_generation: parse_runtime_u64(&ticket.new_generation, "newGeneration")?,
    };
    state
        .with_guest(preview_handle, |handle| {
            handle.complete_surface_recovery(ticket, suspended)
        })
        .map(platform_surface_status)
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlatformSurfaceRecoveryTicketInput {
    surface: PlatformHandle,
    old_generation: String,
    new_generation: String,
}

#[tauri::command]
pub fn cmd_route_platform_input(
    preview_handle: StudioSessionHandle,
    header: PlatformInputHeader,
    payload: PlatformInputPayload,
    state: tauri::State<'_, AppState>,
) -> Result<PlatformInputResult, String> {
    let event = parse_platform_input(header, payload)?;
    state
        .route_platform_input(preview_handle, event)
        .map(|report| PlatformInputResult {
            composition_revision: report.composition_revision.to_string(),
            synthesized_release_count: report.synthesized_releases.len(),
            arbitrated: report.arbitration.is_some(),
        })
}

#[tauri::command]
pub fn cmd_send_gui_event(
    preview_handle: StudioSessionHandle,
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    let bytes = state.with_guest(preview_handle, |handle| {
        handle.send_event(handler_id, &payload)
    })?;
    Ok(Response::new(bytes))
}

#[tauri::command]
pub fn cmd_send_gui_event_async(
    preview_handle: StudioSessionHandle,
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_guest(preview_handle, |handle| {
        handle.send_event_async(handler_id, &payload)
    })
}

#[tauri::command]
pub fn cmd_push_island_transport(
    preview_handle: StudioSessionHandle,
    data: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_guest(preview_handle, |handle| handle.push_island_data(&data))
}

#[tauri::command]
pub fn cmd_open_framework_lane(
    preview_handle: StudioSessionHandle,
    owner: String,
    state: tauri::State<'_, AppState>,
) -> Result<FrameworkLaneBinding, String> {
    state
        .with_guest(preview_handle, |handle| {
            handle.open_framework_channel(
                owner,
                vo_app_runtime::LaneLimits {
                    max_packet_bytes: vo_app_runtime::MAX_PACKET_BYTES as u32,
                    max_messages: 256,
                    max_bytes: 32 * 1024 * 1024,
                },
            )
        })
        .map(FrameworkLaneBinding::from)
}

#[tauri::command]
pub fn cmd_begin_framework_provider(
    preview_handle: StudioSessionHandle,
    module_key: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_framework_provider(preview_handle, &module_key, |handle, binding| {
        handle.begin_framework_provider(
            module_key.clone(),
            vo_app_runtime::DynamicInstanceGroupPlan {
                instances: binding
                    .providers
                    .iter()
                    .map(|provider| vo_app_runtime::InitialProviderInstancePlan {
                        template_id: provider.template_id,
                        capabilities: binding.capabilities.clone(),
                    })
                    .collect(),
            },
        )
    })
}

#[tauri::command]
pub fn cmd_load_framework_provider(
    preview_handle: StudioSessionHandle,
    module_key: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_framework_provider(preview_handle, &module_key, |handle, binding| {
        let mut loaded = Vec::new();
        for provider in &binding.providers {
            if let Err(error) = handle.load_framework_provider(
                module_key.clone(),
                provider.template_id,
                provider.loaded,
            ) {
                for template_id in loaded.into_iter().rev() {
                    let _ = handle.unload_framework_provider(module_key.clone(), template_id);
                }
                return Err(error);
            }
            loaded.push(provider.template_id);
        }
        Ok(())
    })
}

#[tauri::command]
pub fn cmd_unload_framework_provider(
    preview_handle: StudioSessionHandle,
    module_key: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_framework_provider(preview_handle, &module_key, |handle, binding| {
        for provider in binding.providers.iter().rev() {
            handle.unload_framework_provider(module_key.clone(), provider.template_id)?;
        }
        Ok(())
    })
}

#[tauri::command]
pub fn cmd_ready_framework_provider(
    preview_handle: StudioSessionHandle,
    module_key: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_framework_provider(preview_handle, &module_key, |handle, _| {
        handle.ready_framework_provider(module_key.clone())
    })
}

#[tauri::command]
pub fn cmd_abort_framework_provider(
    preview_handle: StudioSessionHandle,
    module_key: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_framework_provider(preview_handle, &module_key, |handle, _| {
        handle.abort_framework_provider(module_key.clone())
    })
}

#[tauri::command]
pub fn cmd_close_framework_provider(
    preview_handle: StudioSessionHandle,
    module_key: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.with_framework_provider(preview_handle, &module_key, |handle, _| {
        handle.close_framework_provider(module_key.clone())
    })
}

#[tauri::command]
pub fn cmd_poll_framework_lane(
    preview_handle: StudioSessionHandle,
    channel_index: u32,
    channel_generation: u32,
    channel_epoch: String,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    let channel_epoch = channel_epoch
        .parse::<u64>()
        .map_err(|_| String::from("channelEpoch must be an unsigned 64-bit decimal string"))?;
    let packet = state.with_guest(preview_handle, |handle| {
        handle.poll_endpoint_packet(
            vo_app_runtime::ChannelHandle {
                index: channel_index,
                generation: channel_generation,
            },
            channel_epoch,
        )
    })?;
    Ok(Response::new(
        packet.map(|packet| packet.bytes).unwrap_or_default(),
    ))
}

#[tauri::command]
pub fn cmd_submit_framework_lane(
    preview_handle: StudioSessionHandle,
    channel_index: u32,
    channel_generation: u32,
    channel_epoch: String,
    packet: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let channel_epoch = channel_epoch
        .parse::<u64>()
        .map_err(|_| String::from("channelEpoch must be an unsigned 64-bit decimal string"))?;
    state.with_guest(preview_handle, |handle| {
        handle.submit_endpoint_packet(
            vo_app_runtime::ChannelHandle {
                index: channel_index,
                generation: channel_generation,
            },
            channel_epoch,
            &packet,
        )
    })
}

#[tauri::command]
pub fn cmd_submit_framework_lane_batch(
    preview_handle: StudioSessionHandle,
    channel_index: u32,
    channel_generation: u32,
    channel_epoch: String,
    packets: Vec<Vec<u8>>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let channel_epoch = channel_epoch
        .parse::<u64>()
        .map_err(|_| String::from("channelEpoch must be an unsigned 64-bit decimal string"))?;
    if packets.is_empty() || packets.len() > 4096 || packets.iter().any(Vec::is_empty) {
        return Err(String::from("framework lane packet batch is invalid"));
    }
    state.with_guest(preview_handle, |handle| {
        handle.submit_endpoint_packet_batch(
            vo_app_runtime::ChannelHandle {
                index: channel_index,
                generation: channel_generation,
            },
            channel_epoch,
            packets,
        )
    })
}

#[tauri::command]
pub fn cmd_poll_display_timing_request(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<Option<DisplayTimingRequest>, String> {
    state
        .with_guest(preview_handle, |handle| {
            handle.poll_display_timing_request()
        })
        .map(|request| request.map(DisplayTimingRequest::from))
}

#[tauri::command]
pub fn cmd_submit_display_pulse(
    preview_handle: StudioSessionHandle,
    request: DisplayTimingRequest,
    observed_micros: String,
    interval_micros: String,
    state: tauri::State<'_, AppState>,
) -> Result<DisplayPulseSubmission, String> {
    let request_sequence = request
        .request_sequence
        .parse::<u64>()
        .map_err(|_| String::from("requestSequence must be an unsigned 64-bit decimal string"))?;
    let observed_micros = observed_micros
        .parse::<u64>()
        .map_err(|_| String::from("observedMicros must be an unsigned 64-bit decimal string"))?;
    let interval_micros = interval_micros
        .parse::<u64>()
        .map_err(|_| String::from("intervalMicros must be an unsigned 64-bit decimal string"))?;
    state
        .with_guest(preview_handle, |handle| {
            handle.submit_display_pulse(
                vo_app_runtime::DisplayTimingRequest {
                    view: vo_app_runtime::ViewHandle {
                        index: request.view.index,
                        generation: request.view.generation,
                    },
                    request_sequence,
                },
                observed_micros,
                interval_micros,
            )
        })
        .map(|submission| DisplayPulseSubmission {
            emitted_domains: submission.emitted_domains,
        })
}

#[tauri::command]
pub fn cmd_poll_gui_render(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    Ok(Response::new(state.poll_gui_render(preview_handle)?))
}

#[tauri::command]
pub fn cmd_poll_game_render(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    Ok(Response::new(state.poll_game_render(preview_handle)?))
}

#[tauri::command]
pub fn cmd_submit_game_render_result(
    preview_handle: StudioSessionHandle,
    result: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.submit_game_render_result(preview_handle, &result)
}

#[tauri::command]
pub fn cmd_poll_platform_request(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    Ok(Response::new(state.poll_platform_request(preview_handle)?))
}

#[tauri::command]
pub fn cmd_poll_vogui_subscriptions(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<Response, String> {
    Ok(Response::new(
        state.poll_vogui_subscriptions(preview_handle)?,
    ))
}

#[tauri::command]
pub fn cmd_submit_vogui_subscription_event(
    preview_handle: StudioSessionHandle,
    caller: Vec<u8>,
    handle_index: u32,
    handle_generation: u32,
    payload: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.submit_vogui_subscription_event(
        preview_handle,
        &caller,
        handle_index,
        handle_generation,
        payload,
    )
}

#[tauri::command]
pub fn cmd_complete_platform_request(
    preview_handle: StudioSessionHandle,
    request_id: String,
    outcome: String,
    payload: Vec<u8>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let request_id = request_id
        .parse::<u64>()
        .map_err(|_| String::from("requestId must be an unsigned 64-bit decimal string"))?;
    let outcome = match outcome.as_str() {
        "completed" => vo_app_runtime::PlatformCompletionOutcome::Completed,
        "denied" => vo_app_runtime::PlatformCompletionOutcome::Denied,
        "unsupported" => vo_app_runtime::PlatformCompletionOutcome::Unsupported,
        "cancelled" => vo_app_runtime::PlatformCompletionOutcome::Cancelled,
        "timed_out" => vo_app_runtime::PlatformCompletionOutcome::TimedOut,
        "failed" => vo_app_runtime::PlatformCompletionOutcome::Failed,
        value => return Err(format!("unknown platform completion outcome '{value}'")),
    };
    state.complete_platform_request(preview_handle, request_id, outcome, payload)
}

#[tauri::command]
pub fn cmd_stop_gui(
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.close_guest_runtime(preview_handle)
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
    preview_handle: StudioSessionHandle,
    state: tauri::State<'_, AppState>,
) -> Result<RendererBridgeVfsSnapshot, String> {
    let session = state.session_snapshot();
    let run_target = resolve_run_target(
        session.root(),
        state.workspace_root(),
        &entry_path,
        session.single_file_run(),
    )?;
    let root_path = run_target.source_root;
    let single_file_entry = run_target.compile_path.is_file();
    let runtime = state.browser_runtime(preview_handle)?;
    // A standalone entry owns exactly one project source file. Mounting its
    // parent directory would disclose unrelated siblings to the renderer and
    // make the host snapshot depend on files the compiler never selected.
    let snapshot_root = if single_file_entry {
        vo_web::BrowserSnapshotRoot::EntryFile
    } else {
        vo_web::BrowserSnapshotRoot::ProjectRoot
    };
    let snapshot = runtime.snapshot_plan(snapshot_root)?;
    let snapshot_root_path = vo_web::browser_snapshot_vfs_path_from_fs(&root_path)?;
    let project_root = (!single_file_entry).then_some(root_path.as_path());
    let entry_path = if single_file_entry {
        run_target.compile_path.as_path()
    } else {
        root_path.as_path()
    };
    let files = vo_web::materialize_browser_snapshot_from_fs(
        &snapshot,
        &runtime,
        project_root,
        entry_path,
    )?
    .into_iter()
    .map(|file| RendererBridgeVfsFile {
        path: file.path,
        bytes: file.bytes,
    })
    .collect();

    Ok(RendererBridgeVfsSnapshot {
        root_path: snapshot_root_path,
        files,
    })
}
