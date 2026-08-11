use std::collections::BTreeMap;
use std::time::Instant;

use tauri::ipc::Response;
use tauri::{AppHandle, Manager};
use vo_engine::{default_mod_cache_root, with_compile_log_sink};

use super::run_blocking;
use crate::app_plan::materialize_native_studio_plan;
use crate::commands::compiler::prepare_and_compile_prepared;
use crate::commands::pathing::{resolve_run_target, ResolvedTarget};
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
    vfs_snapshot: RendererBridgeVfsSnapshot,
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

fn materialize_renderer_bridge_vfs_snapshot(
    run_target: &ResolvedTarget,
    runtime: &vo_web::BrowserRuntimePlan,
    artifact_intent: &vo_web::BrowserArtifactIntent,
) -> Result<
    (
        RendererBridgeVfsSnapshot,
        Vec<vo_web::MaterializedBrowserArtifact>,
    ),
    String,
> {
    let single_file_entry = run_target.compile_path_is_file;
    // A standalone entry owns exactly one project source file. Mounting its
    // parent directory would disclose unrelated siblings to the renderer and
    // make the host snapshot depend on files the compiler never selected.
    let snapshot_root = if single_file_entry {
        vo_web::BrowserSnapshotRoot::EntryFile
    } else {
        vo_web::BrowserSnapshotRoot::ProjectRoot
    };
    let snapshot = runtime.snapshot_plan(snapshot_root)?;
    let root_path = vo_web::browser_snapshot_vfs_path_from_fs(&run_target.source_root)?;
    let project_root = (!single_file_entry).then_some(run_target.source_root.as_path());
    let entry_path = if single_file_entry {
        run_target.compile_path.as_path()
    } else {
        run_target.source_root.as_path()
    };
    let materialized = vo_web::materialize_browser_runtime_snapshot_from_fs(
        artifact_intent,
        runtime,
        &snapshot,
        project_root,
        entry_path,
    )?;
    let files = materialized
        .files
        .into_iter()
        .map(|file| RendererBridgeVfsFile {
            path: file.path,
            bytes: file.bytes,
        })
        .collect();
    Ok((
        RendererBridgeVfsSnapshot { root_path, files },
        materialized.artifacts,
    ))
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
    let (run_output, framework_provider_bindings, app_session, handle, push_rx) =
        run_blocking(move || {
            let run_target = resolve_run_target(
                &session_root,
                &workspace_root,
                &task_entry_path,
                single_file_run,
            )?;
            let compile_path = run_target.compile_path.to_string_lossy().to_string();
            let compile_start = Instant::now();
            let prepared_compile = with_compile_log_sink(
                gui_runtime::make_studio_log_sink(task_app.clone(), session_id),
                || {
                    prepare_and_compile_prepared(&compile_path, &project_options)
                        .map_err(|error| error.to_string())
                },
            )?;
            let compile_output = prepared_compile.output();
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
            let artifact_plan =
                vo_web::browser_artifact_plan_from_fs(&artifact_intent, &runtime_plan)?;
            vo_web::execute_browser_artifact_plan(&artifact_plan)?;
            let (vfs_snapshot, materialized_browser_artifacts) =
                materialize_renderer_bridge_vfs_snapshot(
                    &run_target,
                    &runtime_plan,
                    &artifact_intent,
                )?;
            if materialized_browser_artifacts.len()
                > vo_web::MAX_BROWSER_RUNTIME_ITEMS.saturating_mul(2)
            {
                return Err(String::from(
                    "Studio materialized an invalid number of browser artifacts",
                ));
            }
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
            let framework_provider_bindings =
                crate::app_plan::framework_provider_bindings(&runtime_plan, &resolved_plan)?;
            let (framework, provider_frameworks) = split_framework_contracts(&runtime_plan);
            let compile_output = prepared_compile
                .into_validated_output()
                .map_err(|error| error.to_string())?;
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
                    vfs_snapshot,
                ),
                framework_provider_bindings,
                app_session,
                handle,
                push_rx,
            ))
        })
        .await?;
    let preview_handle =
        state.install_guest_runtime(app_session, handle, push_rx, framework_provider_bindings)?;
    let (session_epoch, bridge_epoch) = state.webview_bridge_identity(preview_handle)?;
    let (render_bytes, module_bytes, entry_path, framework, provider_frameworks, vfs_snapshot) =
        run_output;
    Ok(GuiRunOutput {
        preview_handle,
        session_epoch: session_epoch.to_string(),
        bridge_epoch: bridge_epoch.to_string(),
        render_bytes,
        module_bytes,
        entry_path,
        framework,
        provider_frameworks,
        vfs_snapshot,
    })
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

fn parse_runtime_u64(value: &str, field: &str) -> Result<u64, String> {
    value
        .parse::<u64>()
        .map_err(|_| format!("{field} must be an unsigned 64-bit decimal string"))
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
