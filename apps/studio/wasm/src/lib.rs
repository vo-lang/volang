//! Vo Studio WASM entry point.
//!
//! Exposes compile_run_entry / run_gui_entry / send_gui_event / stop_gui
//! to the Svelte frontend.
//!
//! The IDE UI is Svelte; this module compiles and runs user Vo code.
//! Source files are read from the JS VirtualFS (via vo_web_runtime_wasm::vfs).

mod app_plan;

use futures_util::future::{AbortHandle, Abortable};
use js_sys::{Function, Object, Reflect};
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::future::Future;
use std::path::{Path, PathBuf};
use vo_app_runtime::{
    DynamicInstanceGroupPlan, EntryIslandConstructCommand, EntryLaunchSupervisor,
    EntryLaunchSupervisorConfig, GuestRuntime, HostRequestCommand, HostedAppRuntime,
    HostedInstanceGroup, InitialProviderInstancePlan, PendingHostEvent, PendingHostedInstanceGroup,
    RenderBuffer, RenderIslandRuntime, RequestOutcome, SessionError, SessionHandle, SessionHostMap,
    StepResult,
};
use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{FileSystem, MemoryFs};
use vo_module::project::ProjectContextOptions;
use vo_module::workspace::WorkspaceDiscovery;
use vo_vm::scheduler::HostWaitKey;
use wasm_bindgen::prelude::*;
use wasm_bindgen::{closure::Closure, JsCast, JsValue};

fn session_error_to_js(error: SessionError) -> JsValue {
    let message = error.to_string();
    let js_error = js_sys::Error::new(&message);
    if let SessionError::Exited(code) = error {
        js_error.set_name("VoGuestExitError");
        let _ = Reflect::set(
            js_error.as_ref(),
            &JsValue::from_str("exitCode"),
            &JsValue::from_f64(code as f64),
        );
    }
    js_error.into()
}

fn ensure_panic_hook() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(console_error_panic_hook::set_once);
}

const MAX_STUDIO_OPERATION_ID_BYTES: usize = 128;

fn validate_studio_operation_id(operation_id: &str) -> Result<(), JsValue> {
    if operation_id.is_empty()
        || operation_id.len() > MAX_STUDIO_OPERATION_ID_BYTES
        || !operation_id
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b':' | b'-' | b'_'))
    {
        return Err(js_sys::Error::new("invalid Studio operation ID").into());
    }
    Ok(())
}

fn cancellable_studio_promise<F>(operation_id: &str, future: F) -> js_sys::Promise
where
    F: Future<Output = Result<JsValue, JsValue>> + 'static,
{
    if let Err(error) = validate_studio_operation_id(operation_id) {
        return js_sys::Promise::reject(&error);
    }
    let operation_id = operation_id.to_string();
    let (abort, registration) = AbortHandle::new_pair();
    let generation = match NEXT_STUDIO_OPERATION_GENERATION.with(|next| {
        let generation = next.get();
        let following = generation
            .checked_add(1)
            .ok_or_else(|| js_sys::Error::new("Studio operation generation exhausted"))?;
        next.set(following);
        Ok::<_, js_sys::Error>(generation)
    }) {
        Ok(generation) => generation,
        Err(error) => return js_sys::Promise::reject(error.as_ref()),
    };
    ACTIVE_STUDIO_OPERATIONS.with(|operations| {
        if let Some((_, previous)) = operations
            .borrow_mut()
            .insert(operation_id.clone(), (generation, abort))
        {
            previous.abort();
        }
    });
    wasm_bindgen_futures::future_to_promise(async move {
        let result = Abortable::new(future, registration).await;
        ACTIVE_STUDIO_OPERATIONS.with(|operations| {
            let mut operations = operations.borrow_mut();
            if operations
                .get(&operation_id)
                .is_some_and(|(current, _)| *current == generation)
            {
                operations.remove(&operation_id);
            }
        });
        result.unwrap_or_else(|_| Err(js_sys::Error::new("Studio operation cancelled").into()))
    })
}

#[wasm_bindgen(js_name = "cancelStudioOperation")]
pub fn cancel_studio_operation(operation_id: &str) -> Result<bool, JsValue> {
    validate_studio_operation_id(operation_id)?;
    Ok(ACTIVE_STUDIO_OPERATIONS.with(|operations| {
        let Some((_, operation)) = operations.borrow_mut().remove(operation_id) else {
            return false;
        };
        operation.abort();
        true
    }))
}

/// Synchronize one JavaScript-side extension disposal with Rust routing state.
#[wasm_bindgen(js_name = "forgetWasmExtModuleOwner")]
pub fn forget_wasm_ext_module_owner(owner: &str) -> Result<(), JsValue> {
    vo_web::ext_bridge::forget_wasm_ext_module_owner(owner)
        .map(|_| ())
        .map_err(|error| js_sys::Error::new(&error).into())
}

/// Synchronize a JavaScript-side extension reset with Rust routing state.
#[wasm_bindgen(js_name = "clearWasmExtModuleOwners")]
pub fn clear_wasm_ext_module_owners() -> Result<(), JsValue> {
    vo_web::ext_bridge::clear_wasm_ext_state().map_err(|error| js_sys::Error::new(&error).into())
}

#[wasm_bindgen(js_name = "activateWasmExtScope")]
pub fn activate_wasm_ext_scope(scope: u64) -> Result<(), JsValue> {
    vo_web::ext_bridge::activate_wasm_ext_scope(scope)
        .map_err(|error| js_sys::Error::new(&error).into())
}

#[wasm_bindgen(js_name = "forgetWasmExtScope")]
pub fn forget_wasm_ext_scope(scope: u64) -> Result<(), JsValue> {
    vo_web::ext_bridge::forget_wasm_ext_scope(scope)
        .map_err(|error| js_sys::Error::new(&error).into())
}

/// Result of compiling and running a console entry in Studio.
///
/// A completed program reports `exit_code == 0`; an explicit `os.Exit` keeps
/// the exact VM exit code so the Studio frontend can surface process status.
struct StudioRunResult {
    output: String,
    exit_code: i32,
}

fn pending_host_event_to_js(event: &PendingHostEvent) -> Object {
    let obj = Object::new();
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("key"),
        &JsValue::from_str(&event.key.encode()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("source"),
        &JsValue::from_str(event.source.as_str()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("token"),
        &JsValue::from_str(&event.token.to_string()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("delayMs"),
        &JsValue::from_f64(event.delay_ms as f64),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("replay"),
        &JsValue::from_bool(event.replay),
    );
    obj
}

fn diagnostic_record_to_js(record: &vo_app_runtime::DiagnosticRecord) -> Object {
    let obj = Object::new();
    let severity = match record.severity {
        vo_app_runtime::DiagnosticSeverity::Trace => "trace",
        vo_app_runtime::DiagnosticSeverity::Info => "info",
        vo_app_runtime::DiagnosticSeverity::Warning => "warning",
        vo_app_runtime::DiagnosticSeverity::Error => "error",
        vo_app_runtime::DiagnosticSeverity::Fatal => "fatal",
    };
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("sequence"),
        &JsValue::from_str(&record.sequence.to_string()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("droppedBefore"),
        &JsValue::from_str(&record.dropped_before.to_string()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("severity"),
        &JsValue::from_str(severity),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("source"),
        &JsValue::from_str(&String::from_utf8_lossy(&record.source)),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("code"),
        &JsValue::from_str(&String::from_utf8_lossy(&record.code)),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("message"),
        &JsValue::from_str(&String::from_utf8_lossy(&record.message)),
    );
    obj
}

fn endpoint_channel_binding_to_js(binding: &vo_app_runtime::EndpointChannelBinding) -> Object {
    let obj = Object::new();
    let session = Object::new();
    let channel = Object::new();
    let caller = Object::new();
    let _ = Reflect::set(
        &session,
        &JsValue::from_str("index"),
        &JsValue::from_f64(binding.session.index as f64),
    );
    let _ = Reflect::set(
        &session,
        &JsValue::from_str("generation"),
        &JsValue::from_f64(binding.session.generation as f64),
    );
    let _ = Reflect::set(
        &channel,
        &JsValue::from_str("index"),
        &JsValue::from_f64(binding.channel.index as f64),
    );
    let _ = Reflect::set(
        &channel,
        &JsValue::from_str("generation"),
        &JsValue::from_f64(binding.channel.generation as f64),
    );
    for (name, value) in [
        ("sessionIndex", binding.caller.session_index),
        ("sessionGeneration", binding.caller.session_generation),
        ("endpointIndex", binding.caller.endpoint_index),
        ("endpointGeneration", binding.caller.endpoint_generation),
    ] {
        let _ = Reflect::set(
            &caller,
            &JsValue::from_str(name),
            &JsValue::from_f64(value as f64),
        );
    }
    for (name, value) in [
        ("sessionEpoch", binding.caller.session_epoch),
        ("endpointEpoch", binding.caller.endpoint_epoch),
    ] {
        let _ = Reflect::set(
            &caller,
            &JsValue::from_str(name),
            &JsValue::from_str(&value.to_string()),
        );
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("session"), &session);
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("sessionEpoch"),
        &JsValue::from_str(&binding.session_epoch.to_string()),
    );
    let _ = Reflect::set(&obj, &JsValue::from_str("caller"), &caller);
    let _ = Reflect::set(&obj, &JsValue::from_str("channel"), &channel);
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("channelEpoch"),
        &JsValue::from_str(&binding.channel_epoch.to_string()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("selectedMinor"),
        &JsValue::from_f64(binding.selected_minor as f64),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("selectedExactFingerprint"),
        &js_sys::Uint8Array::from(binding.selected_exact_fingerprint.as_slice()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("maxPacketBytes"),
        &JsValue::from_f64(binding.limits.max_packet_bytes as f64),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("maxMessages"),
        &JsValue::from_f64(binding.limits.max_messages as f64),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("maxBytes"),
        &JsValue::from_f64(binding.limits.max_bytes as f64),
    );
    obj
}

fn host_request_command_to_js(command: &HostRequestCommand) -> Object {
    let obj = Object::new();
    match command {
        HostRequestCommand::Begin {
            request_id,
            capability_name,
            deadline,
            payload,
            ..
        } => {
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("kind"),
                &JsValue::from_str("begin"),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("requestId"),
                &JsValue::from_str(&request_id.to_string()),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("capability"),
                &JsValue::from_str(&String::from_utf8_lossy(capability_name)),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("deadline"),
                &JsValue::from_str(&deadline.to_string()),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("payload"),
                &js_sys::Uint8Array::from(payload.as_slice()),
            );
        }
        HostRequestCommand::Cancel { request_id, .. } => {
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("kind"),
                &JsValue::from_str("cancel"),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("requestId"),
                &JsValue::from_str(&request_id.to_string()),
            );
        }
    }
    obj
}

fn entry_launch_command_to_js(command: &EntryIslandConstructCommand) -> Object {
    let obj = Object::new();
    let framework = match command.framework {
        vo_app_runtime::EntryFramework::Vogui => "vogui",
        vo_app_runtime::EntryFramework::Voplay => "voplay",
    };
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("launchId"),
        &JsValue::from_str(&command.launch_id.to_string()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("framework"),
        &JsValue::from_str(framework),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("factoryId"),
        &JsValue::from_str(&command.descriptor.factory_id().to_string()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("functionId"),
        &JsValue::from_f64(f64::from(command.function_id)),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("artifactIdentity"),
        &js_sys::Uint8Array::from(command.descriptor.artifact_identity().as_slice()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("entryArtifactDigest"),
        &js_sys::Uint8Array::from(command.entry_artifact_digest.as_slice()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("planIdentity"),
        &js_sys::Uint8Array::from(command.plan_identity.as_slice()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("planGeneration"),
        &JsValue::from_str(&command.plan_generation.to_string()),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("init"),
        &js_sys::Uint8Array::from(command.init.as_slice()),
    );
    obj
}

fn parse_request_outcome(outcome: &str) -> Result<RequestOutcome, JsValue> {
    match outcome {
        "success" => Ok(RequestOutcome::Success),
        "denied" => Ok(RequestOutcome::Denied),
        "unsupported" => Ok(RequestOutcome::Unsupported),
        "cancelled" => Ok(RequestOutcome::Cancelled),
        "timeout" => Ok(RequestOutcome::Timeout),
        "provider_error" => Ok(RequestOutcome::ProviderError),
        "session_closed" => Ok(RequestOutcome::SessionClosed),
        value => Err(JsValue::from_str(&format!(
            "unknown host request outcome '{value}'"
        ))),
    }
}

fn parse_platform_completion_outcome(
    outcome: &str,
) -> Result<vo_app_runtime::PlatformCompletionOutcome, JsValue> {
    match outcome {
        "completed" => Ok(vo_app_runtime::PlatformCompletionOutcome::Completed),
        "denied" => Ok(vo_app_runtime::PlatformCompletionOutcome::Denied),
        "unsupported" => Ok(vo_app_runtime::PlatformCompletionOutcome::Unsupported),
        "cancelled" => Ok(vo_app_runtime::PlatformCompletionOutcome::Cancelled),
        "timed_out" => Ok(vo_app_runtime::PlatformCompletionOutcome::TimedOut),
        "failed" => Ok(vo_app_runtime::PlatformCompletionOutcome::Failed),
        value => Err(JsValue::from_str(&format!(
            "unknown platform completion outcome '{value}'"
        ))),
    }
}

include!(concat!(env!("OUT_DIR"), "/studio_build_info.rs"));

#[path = "../../../../lang/crates/vo-engine/src/format.rs"]
mod bytecode_text_format;

const STUDIO_PACKAGE_SNAPSHOT_MAX_BYTES: usize = 256 * 1024 * 1024;
const STUDIO_CACHE_METADATA_MAX_BYTES: usize = 1024;

fn project_context_options_from_workspace_discovery(
    workspace_discovery: &str,
) -> Result<ProjectContextOptions, String> {
    let workspace = match workspace_discovery {
        "auto" => WorkspaceDiscovery::Auto,
        "disabled" => WorkspaceDiscovery::Disabled,
        other => return Err(format!("unsupported workspace discovery mode '{}'", other)),
    };
    Ok(ProjectContextOptions::new(workspace))
}

fn workspace_discovery_reads_workfile(options: &ProjectContextOptions) -> bool {
    !matches!(options.workspace, WorkspaceDiscovery::Disabled)
}

fn emit_host_log(record: vo_web::HostLogRecord) {
    let source = record.core.source.clone();
    let code = record.core.code.clone();
    let text = record.text.clone();
    vo_web::emit_host_log(record);
    if code == "stdout" || code == "voplay_perf_report" {
        return;
    }
    match text {
        Some(text) => web_sys::console::log_1(&format!("[{}:{}] {}", source, code, text).into()),
        None => web_sys::console::log_1(&format!("[{}:{}]", source, code).into()),
    }
}

fn flush_stdout(label: &str, stdout: Option<&str>) {
    if let Some(s) = stdout {
        let trimmed = s.trim();
        if trimmed.is_empty() {
            return;
        }
        if !trimmed.contains("__VOPLAY_PERF_REPORT__") {
            emit_host_log(vo_web::HostLogRecord::new(label, "stdout", "stdout").text(trimmed));
            return;
        }
        let mut stdout_lines = Vec::new();
        for line in s.lines() {
            let line_trimmed = line.trim();
            if line_trimmed.is_empty() {
                continue;
            }
            if let Some(payload) = line_trimmed.strip_prefix("__VOPLAY_PERF_REPORT__") {
                vo_web::emit_host_log(
                    vo_web::HostLogRecord::new("voplay-perf", "voplay_perf_report", "system")
                        .text(payload.trim()),
                );
                continue;
            }
            stdout_lines.push(line);
        }
        let stdout_text = stdout_lines.join("\n");
        let stdout_text = stdout_text.trim();
        if !stdout_text.is_empty() {
            emit_host_log(vo_web::HostLogRecord::new(label, "stdout", "stdout").text(stdout_text));
        }
    }
}

fn publish_guest_stdout(
    guest: &GuestRuntime,
    label: &str,
    stdout: Option<&str>,
) -> Result<(), JsValue> {
    let Some(trimmed) = stdout.map(str::trim).filter(|text| !text.is_empty()) else {
        return Ok(());
    };
    guest
        .publish_diagnostic(
            vo_app_runtime::DiagnosticSeverity::Info,
            label.as_bytes(),
            b"stdout",
            trimmed.as_bytes(),
        )
        .map(|_| ())
        .map_err(|error| JsValue::from_str(&error))
}

fn log_wasm_path(code: &str, path: &str, level: &str, start_ms: Option<f64>) {
    let mut record = vo_web::HostLogRecord::new("studio-wasm", code, level).path(path);
    if let Some(start_ms) = start_ms {
        record = record.duration_ms(js_sys::Date::now() - start_ms);
    }
    emit_host_log(record);
}

fn log_wasm_module(code: &str, module: &str, start_ms: f64) {
    emit_host_log(
        vo_web::HostLogRecord::new("studio-wasm", code, "system")
            .module(module)
            .duration_ms(js_sys::Date::now() - start_ms),
    );
}

fn guest_stdout_source() -> Box<dyn Fn() -> String> {
    Box::new(vo_web::take_output)
}

// =============================================================================
// Guest state (for a running vogui app)
// =============================================================================

thread_local! {
    static HOSTED_RUNTIME: HostedAppRuntime = HostedAppRuntime::new(MAX_GUI_PREVIEWS)
        .expect("valid browser App Runtime capacity");
    static GUESTS: RefCell<SessionHostMap<Option<BrowserSessionHost>>> = RefCell::new(
        SessionHostMap::new(MAX_GUI_PREVIEWS).expect("valid browser preview capacity")
    );
    static BROWSER_RUNTIME_HOST_DIGEST: RefCell<Option<[u8; 32]>> = const { RefCell::new(None) };
    static PREPARED_GUI_LAUNCHES: RefCell<BTreeMap<u64, PreparedGuiLaunch>> =
        const { RefCell::new(BTreeMap::new()) };
    static NEXT_PREPARED_GUI_LAUNCH: Cell<u64> = const { Cell::new(1) };
    static NEXT_APP_PLAN_GENERATION: Cell<u64> = const { Cell::new(1) };
    static GC_STRESS_EVERY_STEP: Cell<bool> = const { Cell::new(false) };
    static GC_STRESS_HOST_STEP: Cell<bool> = const { Cell::new(false) };
    static ACTIVE_STUDIO_OPERATIONS: RefCell<BTreeMap<String, (u64, AbortHandle)>> =
        const { RefCell::new(BTreeMap::new()) };
    static NEXT_STUDIO_OPERATION_GENERATION: Cell<u64> = const { Cell::new(1) };
}

const MAX_GUI_PREVIEWS: usize = 16;
const MAX_BROWSER_HOST_ARTIFACT_BYTES: usize = 256 * 1024 * 1024;

struct PreparedGuiLaunch {
    entry_path: String,
    bytecode_digest: [u8; 32],
    runtime_plan: vo_web::BrowserRuntimePlan,
    browser_artifacts: Vec<vo_web::MaterializedBrowserArtifact>,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
    extensions: Vec<vo_web::ReadyWasmExtensionBytes>,
}

impl PreparedGuiLaunch {
    fn extension_payload_bytes(&self) -> Result<usize, String> {
        self.extensions.iter().try_fold(0usize, |total, extension| {
            total
                .checked_add(extension.wasm_bytes.len())
                .and_then(|total| {
                    total.checked_add(
                        extension
                            .js_glue_bytes
                            .as_ref()
                            .map_or(0, |bytes| bytes.len()),
                    )
                })
                .ok_or_else(|| String::from("prepared GUI extension byte count overflow"))
        })
    }
}

struct BrowserEntryVm {
    vm: vo_vm::vm::Vm,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    framework: vo_app_runtime::EntryFramework,
    startup_bound: bool,
    awaiting_ready: bool,
    pending_vogui_turn: Option<u64>,
    pending_voplay_tick_turn: Option<u64>,
}

struct BrowserHostTimer {
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    handle: vo_app_runtime::TimerHandle,
    timeout_id: i32,
}

#[derive(Clone)]
struct BrowserFrameworkLane {
    module_key: String,
    owner: String,
    role: vo_app_runtime::ProviderRole,
}

struct PendingBrowserVoguiCommit {
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    request_id: u64,
    module_key: String,
    commit: vo_app_runtime::VoguiTargetCommit,
}

struct BrowserSessionHost {
    guest: GuestRuntime,
    entry_bytecode: Vec<u8>,
    entry_vms: BTreeMap<u64, BrowserEntryVm>,
    started: bool,
    closed: bool,
    render: RenderBuffer,
    host_requests: VecDeque<HostRequestCommand>,
    external_request_callers: BTreeMap<u64, vo_runtime::host_services_v2::CallerEndpointHandle>,
    entry_launches: VecDeque<EntryIslandConstructCommand>,
    entry_supervisor: EntryLaunchSupervisor,
    voplay_engines: vo_app_runtime::VoplayEngineControlStore,
    voplay_engine_launches:
        BTreeMap<vo_app_runtime::VoplayPublicEngineRef, vo_app_runtime::EntryLaunchId>,
    timer_ids: BTreeMap<u64, BrowserHostTimer>,
    framework_clock_timeout_id: Option<i32>,
    framework_clock_deadline: Option<u64>,
    resolved_plan: vo_app_runtime::ResolvedAppRuntimePlan,
    _browser_artifacts: Vec<vo_web::MaterializedBrowserArtifact>,
    framework_provider_bindings: BTreeMap<String, app_plan::FrameworkProviderBinding>,
    loaded_framework_providers: BTreeSet<String>,
    pending_framework_providers: BTreeMap<String, PendingHostedInstanceGroup>,
    active_framework_providers: BTreeMap<String, HostedInstanceGroup>,
    framework_lanes: BTreeMap<(u32, u32, u64), BrowserFrameworkLane>,
    pending_vogui_commit: Option<PendingBrowserVoguiCommit>,
    voplay_render_features_initialized: BTreeSet<(String, u32, u32)>,
    voplay_role_engines_initialized: BTreeSet<(String, u32, u32, u32)>,
    voplay_role_engine_epochs: BTreeMap<(String, u32, u32, u32), u64>,
}

impl BrowserSessionHost {
    fn new(
        guest: GuestRuntime,
        session: SessionHandle,
        session_epoch: u64,
        entry_bytecode: Vec<u8>,
        resolved_plan: vo_app_runtime::ResolvedAppRuntimePlan,
        browser_artifacts: Vec<vo_web::MaterializedBrowserArtifact>,
        framework_provider_bindings: Vec<app_plan::FrameworkProviderBinding>,
    ) -> Self {
        Self {
            guest,
            entry_bytecode,
            entry_vms: BTreeMap::new(),
            started: false,
            closed: false,
            render: RenderBuffer::new(),
            host_requests: VecDeque::new(),
            external_request_callers: BTreeMap::new(),
            entry_launches: VecDeque::new(),
            entry_supervisor: EntryLaunchSupervisor::new(EntryLaunchSupervisorConfig::default())
                .expect("valid browser entry launch supervisor limits"),
            voplay_engines: vo_app_runtime::VoplayEngineControlStore::new(
                session.index.saturating_add(1),
                session.generation,
                session_epoch,
                vo_app_runtime::VoplayEngineControlConfig::default(),
            )
            .expect("valid browser Voplay Engine control limits"),
            voplay_engine_launches: BTreeMap::new(),
            timer_ids: BTreeMap::new(),
            framework_clock_timeout_id: None,
            framework_clock_deadline: None,
            resolved_plan,
            _browser_artifacts: browser_artifacts,
            framework_provider_bindings: framework_provider_bindings
                .into_iter()
                .map(|binding| (binding.module_key.clone(), binding))
                .collect(),
            loaded_framework_providers: BTreeSet::new(),
            pending_framework_providers: BTreeMap::new(),
            active_framework_providers: BTreeMap::new(),
            framework_lanes: BTreeMap::new(),
            pending_vogui_commit: None,
            voplay_render_features_initialized: BTreeSet::new(),
            voplay_role_engines_initialized: BTreeSet::new(),
            voplay_role_engine_epochs: BTreeMap::new(),
        }
    }
}

impl Drop for BrowserSessionHost {
    fn drop(&mut self) {
        if !self.closed {
            let _ = clear_browser_host_state(self);
            self.guest.shutdown();
            let _ = self.render.poll();
            self.closed = true;
        }
    }
}

fn apply_gc_stress_config(vm: &mut vo_vm::vm::Vm) {
    GC_STRESS_EVERY_STEP.with(|enabled| {
        vm.set_gc_stress_every_step(enabled.get());
    });
}

fn gc_stress_host_step_enabled() -> bool {
    GC_STRESS_HOST_STEP.with(|enabled| enabled.get())
}

fn run_gc_stress_render_step(runtime: &mut RenderIslandRuntime) {
    if gc_stress_host_step_enabled() {
        runtime.gc_step();
    }
}

fn run_gc_stress_guest_step(guest: &mut GuestRuntime) {
    if gc_stress_host_step_enabled() {
        guest.gc_step();
    }
}

fn with_guest_mut<T>(
    handle: SessionHandle,
    f: impl FnOnce(&mut BrowserSessionHost) -> Result<T, JsValue>,
) -> Result<T, JsValue> {
    let mut host = GUESTS.with(|guests| {
        guests
            .borrow_mut()
            .get_mut(handle)
            .map_err(|error| JsValue::from_str(&format!("invalid preview handle: {error:?}")))?
            .take()
            .ok_or_else(|| JsValue::from_str("preview host is already handling an operation"))
    })?;
    let result = f(&mut host);
    let dispatch_result = if result.is_ok() {
        dispatch_browser_host_requests(handle, &mut host)
    } else {
        Ok(())
    };
    GUESTS.with(|guests| {
        if let Ok(slot) = guests.borrow_mut().get_mut(handle) {
            *slot = Some(host);
        }
    });
    match result {
        Ok(value) => dispatch_result.map(|()| value),
        Err(error) => Err(error),
    }
}

fn browser_global_function(name: &str) -> Result<(Object, Function), JsValue> {
    let global = js_sys::global();
    let function = Reflect::get(&global, &JsValue::from_str(name))?
        .dyn_into::<Function>()
        .map_err(|_| JsValue::from_str(&format!("browser host lacks {name}")))?;
    Ok((global, function))
}

fn clear_browser_timer(host: &mut BrowserSessionHost, request_id: u64) -> Result<bool, JsValue> {
    let Some(timer) = host.timer_ids.remove(&request_id) else {
        return Ok(false);
    };
    let (global, clear_timeout) = browser_global_function("clearTimeout")?;
    clear_timeout.call1(&global, &JsValue::from_f64(timer.timeout_id as f64))?;
    Ok(true)
}

fn clear_browser_framework_clock_wake(host: &mut BrowserSessionHost) -> Result<bool, JsValue> {
    let Some(timeout_id) = host.framework_clock_timeout_id.take() else {
        host.framework_clock_deadline = None;
        return Ok(false);
    };
    host.framework_clock_deadline = None;
    let (global, clear_timeout) = browser_global_function("clearTimeout")?;
    clear_timeout.call1(&global, &JsValue::from_f64(timeout_id as f64))?;
    Ok(true)
}

fn cancel_browser_timer(
    host: &mut BrowserSessionHost,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    request_id: u64,
) -> Result<bool, JsValue> {
    let Some(timer) = host.timer_ids.get(&request_id) else {
        return Ok(false);
    };
    if timer.caller != caller {
        return Err(JsValue::from_str(
            "browser timer cancellation caller identity mismatch",
        ));
    }
    let owner = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser timer has no HostServices V2 owner"))?;
    owner
        .cancel_request_timer(caller, timer.handle)
        .map_err(|status| {
            JsValue::from_str(&format!("cancel browser host timer: status {status}"))
        })?;
    clear_browser_timer(host, request_id)
}

fn schedule_browser_timer_chunk(
    handle: SessionHandle,
    host: &mut BrowserSessionHost,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    request_id: u64,
    timer_handle: vo_app_runtime::TimerHandle,
    remaining: u64,
) -> Result<(), JsValue> {
    const MAX_TIMEOUT_MILLIS: u64 = i32::MAX as u64;
    let chunk = remaining.min(MAX_TIMEOUT_MILLIS);
    let callback = Closure::once(move || {
        let result = with_guest_mut(handle, |host| {
            if remaining > chunk {
                schedule_browser_timer_chunk(
                    handle,
                    host,
                    caller,
                    request_id,
                    timer_handle,
                    remaining - chunk,
                )
            } else {
                fire_browser_timers(handle, host, request_id)
            }
        });
        if let Err(error) = result {
            web_sys::console::error_1(&error);
        }
    });
    let (global, set_timeout) = browser_global_function("setTimeout")?;
    let timeout_id = set_timeout
        .call2(
            &global,
            callback.as_ref().unchecked_ref(),
            &JsValue::from_f64(chunk as f64),
        )?
        .as_f64()
        .ok_or_else(|| JsValue::from_str("setTimeout returned a non-numeric handle"))?
        as i32;
    callback.forget();
    host.timer_ids.insert(
        request_id,
        BrowserHostTimer {
            caller,
            handle: timer_handle,
            timeout_id,
        },
    );
    Ok(())
}

fn finish_browser_host_request_with_data(
    host: &mut BrowserSessionHost,
    request_id: u64,
    outcome: RequestOutcome,
    response: Vec<u8>,
) -> Result<(), JsValue> {
    host.guest
        .complete_host_request_with_data(request_id, outcome, response)
        .map_err(|status| JsValue::from_str(&format!("host completion failed: status {status}")))?;
    host.guest
        .try_take_and_apply_host_wake_signal()
        .map_err(|error| JsValue::from_str(&error))?
        .ok_or_else(|| JsValue::from_str("host completion produced no wake signal"))?;
    let step = host.guest.run_scheduled().map_err(session_error_to_js)?;
    run_gc_stress_guest_step(&mut host.guest);
    publish_guest_stdout(&host.guest, "host-request", step.stdout.as_deref())?;
    if let Some(render_output) = step.render_output {
        host.render.push(render_output);
    }
    Ok(())
}

fn finish_browser_host_request_for(
    host: &mut BrowserSessionHost,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    request_id: u64,
    outcome: RequestOutcome,
    response: Vec<u8>,
) -> Result<(), JsValue> {
    if host.guest.host_caller() == Some(caller) {
        return finish_browser_host_request_with_data(host, request_id, outcome, response);
    }
    let launch_id = host
        .entry_vms
        .iter()
        .find_map(|(launch_id, entry)| (entry.caller == caller).then_some(*launch_id))
        .ok_or_else(|| {
            JsValue::from_str("browser host completion targeted unknown entry caller")
        })?;
    let owner = host.guest.host_services_v2().cloned().ok_or_else(|| {
        JsValue::from_str("browser child completion has no HostServices V2 owner")
    })?;
    owner
        .complete_request_with_data(caller, request_id, outcome, response)
        .map_err(|status| {
            JsValue::from_str(&format!(
                "browser child completion failed with status {status}"
            ))
        })?;
    let signal = owner
        .try_take_wake_signal()
        .map_err(|status| {
            JsValue::from_str(&format!(
                "take browser child wake signal failed with status {status}"
            ))
        })?
        .ok_or_else(|| JsValue::from_str("browser child completion produced no wake signal"))?;
    if signal.caller != caller || signal.request_id != request_id {
        return Err(JsValue::from_str(
            "browser child completion wake order mismatch",
        ));
    }
    let table = owner.provider_abi_table();
    let release_status = unsafe {
        (table
            .release_wake_registration
            .expect("validated HostServices V2 wake release"))(
            table.context,
            caller,
            signal.registration,
        )
    };
    if release_status != vo_runtime::host_services_v2::HOST_SERVICE_STATUS_OK {
        return Err(JsValue::from_str(&format!(
            "release browser child host wait failed with status {release_status}"
        )));
    }
    let (scheduling, became_ready) = {
        let entry = host
            .entry_vms
            .get_mut(&launch_id)
            .ok_or_else(|| JsValue::from_str("browser entry VM disappeared before wake"))?;
        let key = entry
            .vm
            .host_event_key_for_token(signal.wake_key)
            .ok_or_else(|| JsValue::from_str("browser child wake token has no VM waiter"))?;
        if !entry.vm.wake_host_event_with_data(key, signal.response) {
            return Err(JsValue::from_str("browser child VM rejected host wake"));
        }
        let scheduling = entry.vm.run_scheduled().map_err(|error| {
            JsValue::from_str(&format!("run browser child after wake: {error:?}"))
        })?;
        if entry.awaiting_ready
            && matches!(scheduling, vo_vm::vm::SchedulingOutcome::Blocked)
            && !entry.startup_bound
        {
            return Err(JsValue::from_str(
                "browser entry reached its lifecycle without startup state",
            ));
        }
        let became_ready =
            entry.awaiting_ready && matches!(scheduling, vo_vm::vm::SchedulingOutcome::Blocked);
        if became_ready {
            entry.awaiting_ready = false;
        }
        (scheduling, became_ready)
    };
    match scheduling {
        vo_vm::vm::SchedulingOutcome::Blocked
        | vo_vm::vm::SchedulingOutcome::Suspended
        | vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents => {
            if became_ready {
                host.entry_supervisor
                    .mark_running(launch_id)
                    .map_err(|error| {
                        JsValue::from_str(&format!(
                            "ready browser entry after child wake: {error:?}"
                        ))
                    })?;
                finish_browser_entry_launches(host)?;
            }
            Ok(())
        }
        outcome => {
            let message = format!("browser child ended after host wake: {outcome:?}");
            if let Some(engine) = host
                .voplay_engine_launches
                .iter()
                .find_map(|(engine, launch)| (*launch == launch_id).then_some(*engine))
            {
                let _ = host.voplay_engines.fail(engine);
                host.voplay_engine_launches.remove(&engine);
            }
            if host
                .entry_supervisor
                .record(launch_id)
                .is_some_and(|record| {
                    record.state == vo_app_runtime::EntryLaunchState::Constructing
                })
            {
                host.entry_supervisor
                    .fail(launch_id, message.as_bytes())
                    .map_err(|error| {
                        JsValue::from_str(&format!(
                            "fail browser entry after child wake: {error:?}"
                        ))
                    })?;
                if let Some(entry) = host.entry_vms.remove(&launch_id) {
                    release_browser_target_startup(host, entry.framework, entry.caller);
                    close_browser_entry_endpoint(host, entry.caller)?;
                }
                finish_browser_entry_launches(host)?;
                Ok(())
            } else {
                Err(JsValue::from_str(&message))
            }
        }
    }
}

fn finish_browser_entry_launches(host: &mut BrowserSessionHost) -> Result<(), JsValue> {
    while let Some(completion) = host.entry_supervisor.take_completion() {
        finish_browser_host_request_for(
            host,
            completion.caller,
            completion.request_id,
            completion.outcome,
            completion.response,
        )?;
    }
    Ok(())
}

fn browser_framework_module_matches(
    module_key: &str,
    framework: vo_app_runtime::EntryFramework,
) -> bool {
    let expected = match framework {
        vo_app_runtime::EntryFramework::Vogui => "vogui",
        vo_app_runtime::EntryFramework::Voplay => "voplay",
    };
    module_key
        .rsplit('/')
        .next()
        .is_some_and(|name| name == expected)
}

fn bind_browser_target_startup(
    host: &mut BrowserSessionHost,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    startup: vo_app_runtime::TargetStartup,
) -> Result<(), String> {
    let framework = startup.framework();
    let keys = host
        .active_framework_providers
        .keys()
        .filter(|module_key| browser_framework_module_matches(module_key, framework))
        .take(2)
        .cloned()
        .collect::<Vec<_>>();
    if keys.len() != 1 {
        return Err(format!(
            "expected one active {framework:?} provider, found {} among {:?}",
            keys.len(),
            host.active_framework_providers.keys().collect::<Vec<_>>()
        ));
    }
    host.active_framework_providers
        .get_mut(&keys[0])
        .ok_or_else(|| format!("active {framework:?} provider disappeared"))?
        .bind_target_startup(caller, startup)
        .map_err(|error| format!("bind {framework:?} target startup: {error}"))
}

fn release_browser_target_startup(
    host: &mut BrowserSessionHost,
    framework: vo_app_runtime::EntryFramework,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
) {
    let key = host
        .active_framework_providers
        .keys()
        .find(|module_key| browser_framework_module_matches(module_key, framework))
        .cloned();
    if let Some(group) = key.and_then(|key| host.active_framework_providers.get_mut(&key)) {
        group.release_target_startup(caller);
    }
}

fn encode_browser_vogui_target_turn(
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    source_root: Option<(u32, u32)>,
    source_view: Option<(u32, u32)>,
    event_sequence: Option<u64>,
    event_revision: Option<u64>,
    mapper_id: u32,
    monotonic_millis: u64,
    payload: &[u8],
) -> Result<Vec<u8>, JsValue> {
    if mapper_id == 0 || payload.len() > vo_app_runtime::MAX_TARGET_STARTUP_BYTES - 52 {
        return Err(JsValue::from_str(
            "browser Vogui target turn exceeds provider limits",
        ));
    }
    let mut turn = Vec::with_capacity(52 + payload.len());
    turn.extend_from_slice(&3_u32.to_le_bytes());
    turn.extend_from_slice(&mapper_id.to_le_bytes());
    turn.extend_from_slice(&monotonic_millis.to_le_bytes());
    let source_root = source_root.unwrap_or((caller.endpoint_index, caller.endpoint_generation));
    let source_view = source_view.unwrap_or(source_root);
    turn.extend_from_slice(&source_root.0.to_le_bytes());
    turn.extend_from_slice(&source_root.1.to_le_bytes());
    turn.extend_from_slice(&source_view.0.to_le_bytes());
    turn.extend_from_slice(&source_view.1.to_le_bytes());
    turn.extend_from_slice(&event_sequence.unwrap_or(0).to_le_bytes());
    turn.extend_from_slice(&event_revision.unwrap_or(0).to_le_bytes());
    turn.extend_from_slice(&(payload.len() as u32).to_le_bytes());
    turn.extend_from_slice(payload);
    Ok(turn)
}

struct DecodedBrowserProviderTurn<'a> {
    mapper_id: u32,
    source_root: Option<(u32, u32)>,
    source_view: Option<(u32, u32)>,
    event_sequence: Option<u64>,
    event_revision: Option<u64>,
    payload: &'a [u8],
}

fn decode_browser_provider_turn(
    packet: &[u8],
) -> Result<Option<DecodedBrowserProviderTurn<'_>>, String> {
    if let Some(turn) = packet.strip_prefix(b"vogui-target-turn-v3\0") {
        if turn.len() < 40 {
            return Err(String::from("sequenced browser Vogui turn is truncated"));
        }
        let mapper_id = u32::from_le_bytes(turn[..4].try_into().unwrap());
        let source_root = (
            u32::from_le_bytes(turn[4..8].try_into().unwrap()),
            u32::from_le_bytes(turn[8..12].try_into().unwrap()),
        );
        let source_view = (
            u32::from_le_bytes(turn[12..16].try_into().unwrap()),
            u32::from_le_bytes(turn[16..20].try_into().unwrap()),
        );
        let event_sequence = u64::from_le_bytes(turn[20..28].try_into().unwrap());
        let event_revision = u64::from_le_bytes(turn[28..36].try_into().unwrap());
        let payload_len = u32::from_le_bytes(turn[36..40].try_into().unwrap()) as usize;
        if mapper_id == 0
            || source_root.1 == 0
            || source_view.1 == 0
            || event_sequence == 0
            || event_revision == 0
            || payload_len != turn.len() - 40
        {
            return Err(String::from("sequenced browser Vogui turn is malformed"));
        }
        return Ok(Some(DecodedBrowserProviderTurn {
            mapper_id,
            source_root: Some(source_root),
            source_view: Some(source_view),
            event_sequence: Some(event_sequence),
            event_revision: Some(event_revision),
            payload: &turn[40..],
        }));
    }
    if let Some(turn) = packet.strip_prefix(b"vogui-target-turn-v2\0") {
        if turn.len() < 24 {
            return Err(String::from("qualified browser Vogui turn is truncated"));
        }
        let mapper_id = u32::from_le_bytes(turn[..4].try_into().unwrap());
        let source_root = (
            u32::from_le_bytes(turn[4..8].try_into().unwrap()),
            u32::from_le_bytes(turn[8..12].try_into().unwrap()),
        );
        let source_view = (
            u32::from_le_bytes(turn[12..16].try_into().unwrap()),
            u32::from_le_bytes(turn[16..20].try_into().unwrap()),
        );
        let payload_len = u32::from_le_bytes(turn[20..24].try_into().unwrap()) as usize;
        if mapper_id == 0
            || source_root.1 == 0
            || source_view.1 == 0
            || payload_len != turn.len() - 24
        {
            return Err(String::from("qualified browser Vogui turn is malformed"));
        }
        return Ok(Some(DecodedBrowserProviderTurn {
            mapper_id,
            source_root: Some(source_root),
            source_view: Some(source_view),
            event_sequence: None,
            event_revision: None,
            payload: &turn[24..],
        }));
    }
    if let Some(turn) = packet.strip_prefix(b"vogui-target-turn-v1\0") {
        if turn.len() < 8 {
            return Err(String::from("browser Vogui turn is truncated"));
        }
        let mapper_id = u32::from_le_bytes(turn[..4].try_into().unwrap());
        let payload_len = u32::from_le_bytes(turn[4..8].try_into().unwrap()) as usize;
        if mapper_id == 0 || payload_len != turn.len() - 8 {
            return Err(String::from("browser Vogui turn is malformed"));
        }
        return Ok(Some(DecodedBrowserProviderTurn {
            mapper_id,
            source_root: None,
            source_view: None,
            event_sequence: None,
            event_revision: None,
            payload: &turn[8..],
        }));
    }
    Ok(None)
}

fn enqueue_browser_vogui_target_turn(
    host: &mut BrowserSessionHost,
    mapper_id: i32,
    payload: &[u8],
    source_root: Option<(u32, u32)>,
    source_view: Option<(u32, u32)>,
    event_sequence: Option<u64>,
    event_revision: Option<u64>,
) -> Result<bool, JsValue> {
    let launch_ids = host
        .entry_vms
        .iter()
        .filter(|(_, entry)| {
            entry.framework == vo_app_runtime::EntryFramework::Vogui && entry.startup_bound
        })
        .take(2)
        .map(|(launch_id, _)| *launch_id)
        .collect::<Vec<_>>();
    if launch_ids.is_empty() {
        return Ok(false);
    }
    if launch_ids.len() != 1 {
        return Err(JsValue::from_str(
            "unqualified browser Vogui event is ambiguous",
        ));
    }
    let caller = host
        .entry_vms
        .get(&launch_ids[0])
        .map(|entry| entry.caller)
        .ok_or_else(|| JsValue::from_str("browser Vogui target disappeared"))?;
    let mapper_id = u32::try_from(mapper_id)
        .map_err(|_| JsValue::from_str("Vogui mapper identity is negative"))?;
    enqueue_browser_vogui_target_turn_for(
        host,
        caller,
        source_root,
        source_view,
        event_sequence,
        event_revision,
        mapper_id,
        payload,
        browser_monotonic_millis()?,
    )?;
    Ok(true)
}

fn enqueue_browser_vogui_target_turn_for(
    host: &mut BrowserSessionHost,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    source_root: Option<(u32, u32)>,
    source_view: Option<(u32, u32)>,
    event_sequence: Option<u64>,
    event_revision: Option<u64>,
    mapper_id: u32,
    payload: &[u8],
    monotonic_millis: u64,
) -> Result<(), JsValue> {
    let launch_id = host
        .entry_vms
        .iter()
        .find_map(|(launch_id, entry)| {
            (entry.caller == caller
                && entry.framework == vo_app_runtime::EntryFramework::Vogui
                && entry.startup_bound)
                .then_some(*launch_id)
        })
        .ok_or_else(|| JsValue::from_str("browser Vogui subscription caller is not active"))?;
    let turn = encode_browser_vogui_target_turn(
        caller,
        source_root,
        source_view,
        event_sequence,
        event_revision,
        mapper_id,
        monotonic_millis,
        payload,
    )?;
    let key = host
        .active_framework_providers
        .keys()
        .find(|module_key| {
            browser_framework_module_matches(module_key, vo_app_runtime::EntryFramework::Vogui)
        })
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser Vogui provider is not active"))?;
    host.active_framework_providers
        .get_mut(&key)
        .ok_or_else(|| JsValue::from_str("browser Vogui provider disappeared"))?
        .enqueue_vogui_target_turn(caller, turn)
        .map_err(|error| JsValue::from_str(&error))?;
    let pending = host
        .entry_vms
        .get_mut(&launch_id)
        .and_then(|entry| entry.pending_vogui_turn.take());
    if let Some(request_id) = pending {
        let turn = host
            .active_framework_providers
            .get_mut(&key)
            .ok_or_else(|| JsValue::from_str("browser Vogui provider disappeared"))?
            .take_vogui_target_turn(caller)
            .map_err(|error| JsValue::from_str(&error))?
            .ok_or_else(|| JsValue::from_str("browser Vogui target turn disappeared"))?;
        let mut response = Vec::with_capacity(1 + turn.len());
        response.push(0);
        response.extend_from_slice(&turn);
        finish_browser_host_request_for(
            host,
            caller,
            request_id,
            RequestOutcome::Success,
            response,
        )?;
    }
    Ok(())
}

fn take_browser_vogui_effect(host: &mut BrowserSessionHost) -> Result<Option<Vec<u8>>, JsValue> {
    let callers = host
        .entry_vms
        .values()
        .filter(|entry| {
            entry.framework == vo_app_runtime::EntryFramework::Vogui && entry.startup_bound
        })
        .take(2)
        .map(|entry| entry.caller)
        .collect::<Vec<_>>();
    if callers.is_empty() {
        return Ok(None);
    }
    if callers.len() != 1 {
        return Err(JsValue::from_str("browser Vogui effect poll is ambiguous"));
    }
    let key = host
        .active_framework_providers
        .keys()
        .find(|module_key| {
            browser_framework_module_matches(module_key, vo_app_runtime::EntryFramework::Vogui)
        })
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser Vogui provider is not active"))?;
    host.active_framework_providers
        .get_mut(&key)
        .ok_or_else(|| JsValue::from_str("browser Vogui provider disappeared"))?
        .take_vogui_effect(callers[0])
        .map_err(|error| JsValue::from_str(&error))
}

fn complete_browser_voplay_tick_turn(
    host: &mut BrowserSessionHost,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
) -> Result<(), JsValue> {
    let launch_id = host
        .entry_vms
        .iter()
        .find_map(|(launch_id, entry)| {
            (entry.caller == caller
                && entry.framework == vo_app_runtime::EntryFramework::Voplay
                && entry.startup_bound)
                .then_some(*launch_id)
        })
        .ok_or_else(|| JsValue::from_str("browser Voplay tick caller is not active"))?;
    let pending = host
        .entry_vms
        .get_mut(&launch_id)
        .and_then(|entry| entry.pending_voplay_tick_turn.take());
    let Some(request_id) = pending else {
        return Ok(());
    };
    let key = host
        .active_framework_providers
        .keys()
        .find(|module_key| {
            browser_framework_module_matches(module_key, vo_app_runtime::EntryFramework::Voplay)
        })
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser Voplay provider is not active"))?;
    let turn = host
        .active_framework_providers
        .get_mut(&key)
        .ok_or_else(|| JsValue::from_str("browser Voplay provider disappeared"))?
        .take_voplay_tick_turn(caller)
        .map_err(|error| JsValue::from_str(&error))?
        .ok_or_else(|| JsValue::from_str("browser Voplay target tick turn disappeared"))?;
    let mut response = Vec::with_capacity(1 + turn.len());
    response.push(0);
    response.extend_from_slice(&turn);
    finish_browser_host_request_for(host, caller, request_id, RequestOutcome::Success, response)
}

fn browser_monotonic_millis() -> Result<u64, JsValue> {
    let global = js_sys::global();
    let performance = Reflect::get(&global, &JsValue::from_str("performance"))?;
    let now = Reflect::get(&performance, &JsValue::from_str("now"))?
        .dyn_into::<Function>()
        .map_err(|_| JsValue::from_str("browser host lacks performance.now"))?
        .call0(&performance)?
        .as_f64()
        .filter(|value| value.is_finite() && *value >= 0.0)
        .ok_or_else(|| JsValue::from_str("performance.now returned an invalid timestamp"))?;
    Ok(now.min(u64::MAX as f64) as u64)
}

fn drive_browser_framework_clocks(host: &mut BrowserSessionHost) -> Result<(), JsValue> {
    let now_millis = browser_monotonic_millis()?;
    let now_nanos = now_millis.saturating_mul(1_000_000);
    let mut subscription_events = Vec::new();
    let mut effect_completions = Vec::new();
    let mut advanced_voplay_callers = Vec::new();
    for group in host.active_framework_providers.values_mut() {
        advanced_voplay_callers.extend(
            group
                .drive_voplay_browser_clock(now_nanos)
                .map_err(|error| JsValue::from_str(&error))?
                .into_iter()
                .map(|(caller, _)| caller),
        );
        subscription_events.extend(
            group
                .drive_vogui_subscriptions(now_millis)
                .map_err(|error| JsValue::from_str(&error))?,
        );
        effect_completions.extend(
            group
                .drive_vogui_task_effects(now_millis)
                .map_err(|error| JsValue::from_str(&error))?,
        );
        effect_completions.extend(
            group
                .drive_vogui_platform_effects(now_millis)
                .map_err(|error| JsValue::from_str(&error))?,
        );
        effect_completions.extend(
            group
                .take_vogui_platform_completions()
                .map_err(|error| JsValue::from_str(&error))?,
        );
    }
    for caller in advanced_voplay_callers {
        complete_browser_voplay_tick_turn(host, caller)?;
    }
    for event in subscription_events {
        submit_browser_vogui_subscription_event(host, event)?;
    }
    for completion in effect_completions {
        submit_browser_vogui_effect_completion(host, completion)?;
    }
    Ok(())
}

fn submit_browser_vogui_subscription_event(
    host: &mut BrowserSessionHost,
    event: vo_app_runtime::HostedVoguiSubscriptionEvent,
) -> Result<(), JsValue> {
    let owners = host
        .framework_lanes
        .values()
        .filter(|lane| {
            lane.role == vo_app_runtime::ProviderRole::UiLogic
                && browser_framework_module_matches(
                    &lane.module_key,
                    vo_app_runtime::EntryFramework::Vogui,
                )
        })
        .map(|lane| lane.owner.clone())
        .take(2)
        .collect::<Vec<_>>();
    if owners.len() != 1 {
        return Err(JsValue::from_str(
            "browser Vogui subscription has no unique UiLogic lane",
        ));
    }
    let endpoint = host
        .guest
        .host_caller()
        .ok_or_else(|| JsValue::from_str("browser runtime has no hosted endpoint"))?;
    let services = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser runtime has no HostServices V2 owner"))?;
    let payload_len = u32::try_from(event.payload.len())
        .map_err(|_| JsValue::from_str("browser Vogui subscription payload is too large"))?;
    let mut packet = Vec::with_capacity(45 + event.payload.len());
    packet.extend_from_slice(b"vogui-host-subscription-event-v1\0");
    packet.extend_from_slice(&event.handle.index.to_le_bytes());
    packet.extend_from_slice(&event.handle.generation.to_le_bytes());
    packet.extend_from_slice(&payload_len.to_le_bytes());
    packet.extend_from_slice(&event.payload);
    services
        .publish_named_endpoint_payload(endpoint, owners[0].as_bytes(), &packet)
        .map_err(|status| {
            JsValue::from_str(&format!(
                "publish browser Vogui subscription event failed: status {status}"
            ))
        })
}

fn submit_browser_vogui_effect_completion(
    host: &mut BrowserSessionHost,
    completion: vo_app_runtime::HostedVoguiEffectCompletion,
) -> Result<(), JsValue> {
    let owners = host
        .framework_lanes
        .values()
        .filter(|lane| {
            lane.role == vo_app_runtime::ProviderRole::UiLogic
                && browser_framework_module_matches(
                    &lane.module_key,
                    vo_app_runtime::EntryFramework::Vogui,
                )
        })
        .map(|lane| lane.owner.clone())
        .take(2)
        .collect::<Vec<_>>();
    if owners.len() != 1 {
        return Err(JsValue::from_str(
            "browser Vogui effect has no unique UiLogic lane",
        ));
    }
    let endpoint = host
        .guest
        .host_caller()
        .ok_or_else(|| JsValue::from_str("browser runtime has no hosted endpoint"))?;
    let services = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser runtime has no HostServices V2 owner"))?;
    let payload_len = u32::try_from(completion.payload.len())
        .map_err(|_| JsValue::from_str("browser Vogui effect result is too large"))?;
    let mut packet = Vec::with_capacity(50 + completion.payload.len());
    packet.extend_from_slice(b"vogui-host-effect-result-v1\0");
    packet.extend_from_slice(&completion.effect_id.to_le_bytes());
    packet.extend_from_slice(&completion.app_code_epoch.to_le_bytes());
    packet.push(completion.outcome);
    packet.extend_from_slice(&payload_len.to_le_bytes());
    packet.extend_from_slice(&completion.payload);
    services
        .publish_named_endpoint_payload(endpoint, owners[0].as_bytes(), &packet)
        .map_err(|status| {
            JsValue::from_str(&format!(
                "publish browser Vogui effect completion failed: status {status}"
            ))
        })
}

fn schedule_browser_framework_clock_wake(
    handle: SessionHandle,
    host: &mut BrowserSessionHost,
) -> Result<(), JsValue> {
    if host.closed {
        clear_browser_framework_clock_wake(host)?;
        return Ok(());
    }
    let now_millis = browser_monotonic_millis()?;
    let mut next_deadline = None;
    for group in host.active_framework_providers.values() {
        if let Some(deadline) = group
            .next_vogui_subscription_wake(now_millis)
            .map_err(|error| JsValue::from_str(&error))?
        {
            next_deadline =
                Some(next_deadline.map_or(deadline, |current: u64| current.min(deadline)));
        }
        if let Some(deadline) = group.next_vogui_task_wake() {
            next_deadline =
                Some(next_deadline.map_or(deadline, |current: u64| current.min(deadline)));
        }
        if let Some(deadline) = group.next_vogui_platform_deadline() {
            next_deadline =
                Some(next_deadline.map_or(deadline, |current: u64| current.min(deadline)));
        }
        if let Some(deadline_nanos) = group
            .next_voplay_tick_wake_nanos(now_millis.saturating_mul(1_000_000))
            .map_err(|error| JsValue::from_str(&error))?
        {
            let deadline = (deadline_nanos / 1_000_000
                + u64::from(deadline_nanos % 1_000_000 != 0))
            // A target tick may take longer than its nominal fixed interval
            // in the browser VM. Never reschedule an overdue tick at 0 ms:
            // reserve a bounded event-loop window for provider replies,
            // painting, browser input and Studio controls.
            .max(now_millis.saturating_add(16));
            next_deadline =
                Some(next_deadline.map_or(deadline, |current: u64| current.min(deadline)));
        }
    }
    if host.framework_clock_timeout_id.is_some() && host.framework_clock_deadline == next_deadline {
        return Ok(());
    }
    clear_browser_framework_clock_wake(host)?;
    let Some(deadline) = next_deadline else {
        return Ok(());
    };
    let delay = deadline.saturating_sub(now_millis).min(i32::MAX as u64);
    let callback = Closure::once(move || {
        let result = with_guest_mut(handle, |host| {
            host.framework_clock_timeout_id = None;
            host.framework_clock_deadline = None;
            drive_browser_framework_clocks(host)?;
            schedule_browser_framework_clock_wake(handle, host)
        });
        if let Err(error) = result {
            web_sys::console::error_1(&error);
        }
    });
    let (global, set_timeout) = browser_global_function("setTimeout")?;
    let timeout_id = set_timeout
        .call2(
            &global,
            callback.as_ref().unchecked_ref(),
            &JsValue::from_f64(delay as f64),
        )?
        .as_f64()
        .ok_or_else(|| JsValue::from_str("setTimeout returned a non-numeric handle"))?
        as i32;
    callback.forget();
    host.framework_clock_timeout_id = Some(timeout_id);
    host.framework_clock_deadline = Some(deadline);
    Ok(())
}

fn fire_browser_timers(
    handle: SessionHandle,
    host: &mut BrowserSessionHost,
    trigger_request_id: u64,
) -> Result<(), JsValue> {
    let (caller, timer_handle) = host
        .timer_ids
        .get(&trigger_request_id)
        .map(|timer| (timer.caller, timer.handle))
        .ok_or_else(|| JsValue::from_str("browser timer fired after it was released"))?;
    let now = browser_monotonic_millis()?;
    let owner = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser timer has no HostServices V2 owner"))?;
    owner.set_monotonic_time(now);
    let expired = owner
        .take_expired_request_timers(caller, now)
        .map_err(|status| {
            JsValue::from_str(&format!(
                "failed to advance browser host timers: status {status}"
            ))
        })?;
    if !expired
        .iter()
        .any(|timer| timer.payload == trigger_request_id)
    {
        schedule_browser_timer_chunk(handle, host, caller, trigger_request_id, timer_handle, 1)?;
    }
    for timer in expired {
        let request_id = timer.payload;
        if let Some(scheduled) = host.timer_ids.get(&request_id) {
            if scheduled.caller != timer.caller {
                return Err(JsValue::from_str(
                    "expired browser timer caller identity mismatch",
                ));
            }
        }
        clear_browser_timer(host, request_id)?;
        finish_browser_host_request_for(
            host,
            timer.caller,
            request_id,
            RequestOutcome::Success,
            Vec::new(),
        )?;
    }
    Ok(())
}

fn browser_voplay_group_mut(
    host: &mut BrowserSessionHost,
) -> Result<&mut HostedInstanceGroup, JsValue> {
    let keys = host
        .active_framework_providers
        .keys()
        .filter(|module_key| {
            browser_framework_module_matches(module_key, vo_app_runtime::EntryFramework::Voplay)
        })
        .take(2)
        .cloned()
        .collect::<Vec<_>>();
    if keys.len() != 1 {
        return Err(JsValue::from_str(
            "browser Voplay Engine has no unique provider group",
        ));
    }
    host.active_framework_providers
        .get_mut(&keys[0])
        .ok_or_else(|| JsValue::from_str("browser Voplay provider group disappeared"))
}

fn dispatch_browser_voplay_engine_command(
    host: &mut BrowserSessionHost,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    request_id: u64,
    host_wait_key: u64,
    command: vo_app_runtime::VoplayEngineCommand,
) -> Result<Option<Vec<u8>>, JsValue> {
    match command {
        vo_app_runtime::VoplayEngineCommand::New {
            session_index,
            session_generation,
            session_epoch,
            descriptor,
        } => {
            let engine = host
                .voplay_engines
                .create(session_index, session_generation, session_epoch, descriptor)
                .map_err(|error| {
                    JsValue::from_str(&format!("create browser Voplay Engine: {error:?}"))
                })?;
            let mut response = vec![0];
            response.extend_from_slice(&engine.engine_index.to_le_bytes());
            response.extend_from_slice(&engine.engine_generation.to_le_bytes());
            Ok(Some(response))
        }
        vo_app_runtime::VoplayEngineCommand::Install { engine, entry } => {
            host.voplay_engines
                .install(engine, entry)
                .map_err(|error| {
                    JsValue::from_str(&format!("install browser Voplay entry: {error:?}"))
                })?;
            Ok(Some(vec![0]))
        }
        vo_app_runtime::VoplayEngineCommand::Start(engine) => {
            host.voplay_engines.begin_start(engine).map_err(|error| {
                JsValue::from_str(&format!("start browser Voplay Engine: {error:?}"))
            })?;
            let launch = host
                .voplay_engines
                .start_entry(engine)
                .map_err(|error| {
                    JsValue::from_str(&format!("read browser Voplay entry: {error:?}"))
                })?
                .clone();
            let certified = vo_app_runtime::certify_entry_launch(&host.resolved_plan, launch)
                .map_err(|error| {
                    let _ = host.voplay_engines.fail(engine);
                    JsValue::from_str(&format!("certify browser Voplay entry: {error:?}"))
                })?;
            let launch_id = host
                .entry_supervisor
                .enqueue(caller, request_id, host_wait_key, certified)
                .map_err(|error| {
                    let _ = host.voplay_engines.fail(engine);
                    JsValue::from_str(&format!("queue browser Voplay entry: {error:?}"))
                })?;
            let launch = host
                .entry_supervisor
                .take_construct_command()
                .map_err(|error| {
                    JsValue::from_str(&format!("take browser Voplay entry: {error:?}"))
                })?
                .ok_or_else(|| JsValue::from_str("browser Voplay entry was not queued"))?;
            if launch.launch_id != launch_id {
                let _ = host.voplay_engines.fail(engine);
                return Err(JsValue::from_str(
                    "browser Voplay entry launch identity changed",
                ));
            }
            match launch_browser_entry_island(host, &launch) {
                Ok(entry) => {
                    let ready = !entry.awaiting_ready;
                    host.entry_vms.insert(launch_id, entry);
                    host.voplay_engine_launches.insert(engine, launch_id);
                    if ready {
                        host.entry_supervisor
                            .mark_running(launch_id)
                            .map_err(|error| {
                                JsValue::from_str(&format!("ready browser Voplay entry: {error:?}"))
                            })?;
                        host.voplay_engines.mark_running(engine).map_err(|error| {
                            JsValue::from_str(&format!("ready browser Voplay Engine: {error:?}"))
                        })?;
                        finish_browser_entry_launches(host)?;
                    }
                }
                Err(error) => {
                    let _ = host.voplay_engines.fail(engine);
                    host.entry_supervisor
                        .fail(launch_id, error.as_bytes())
                        .map_err(|failure| {
                            JsValue::from_str(&format!("fail browser Voplay entry: {failure:?}"))
                        })?;
                    finish_browser_entry_launches(host)?;
                }
            }
            Ok(None)
        }
        vo_app_runtime::VoplayEngineCommand::Step { engine, count } => {
            host.voplay_engines
                .queue_manual_ticks(engine, count)
                .map_err(|error| {
                    JsValue::from_str(&format!("queue browser Voplay ticks: {error:?}"))
                })?;
            let launch = *host
                .voplay_engine_launches
                .get(&engine)
                .ok_or_else(|| JsValue::from_str("browser Voplay Engine has no entry"))?;
            let target = host
                .entry_vms
                .get(&launch)
                .ok_or_else(|| JsValue::from_str("browser Voplay target is not active"))?;
            let target_caller = target.caller;
            let queued = host
                .voplay_engines
                .take_manual_ticks(engine, count)
                .map_err(|error| {
                    JsValue::from_str(&format!("consume browser Voplay ticks: {error:?}"))
                })?;
            browser_voplay_group_mut(host)?
                .advance_voplay_fixed_ticks(target_caller, queued)
                .map_err(|error| JsValue::from_str(&error))?;
            complete_browser_voplay_tick_turn(host, target_caller)?;
            Ok(Some(vec![0]))
        }
        vo_app_runtime::VoplayEngineCommand::Pause(engine) => {
            host.voplay_engines.pause(engine).map_err(|error| {
                JsValue::from_str(&format!("pause browser Voplay Engine: {error:?}"))
            })?;
            dispatch_browser_voplay_engine_lifecycle(host, engine, 14)?;
            set_browser_voplay_clock(host, engine, true)?;
            Ok(Some(vec![0]))
        }
        vo_app_runtime::VoplayEngineCommand::Resume(engine) => {
            host.voplay_engines.resume(engine).map_err(|error| {
                JsValue::from_str(&format!("resume browser Voplay Engine: {error:?}"))
            })?;
            dispatch_browser_voplay_engine_lifecycle(host, engine, 15)?;
            let manual = host
                .voplay_engines
                .descriptor(engine)
                .map_err(|error| {
                    JsValue::from_str(&format!("read browser Voplay Engine: {error:?}"))
                })?
                .headless;
            set_browser_voplay_clock(host, engine, manual)?;
            Ok(Some(vec![0]))
        }
        vo_app_runtime::VoplayEngineCommand::Shutdown(engine) => {
            host.voplay_engines
                .begin_shutdown(engine)
                .map_err(|error| {
                    JsValue::from_str(&format!("shutdown browser Voplay Engine: {error:?}"))
                })?;
            dispatch_browser_voplay_engine_lifecycle(host, engine, 16)?;
            if let Some(launch_id) = host.voplay_engine_launches.remove(&engine) {
                if let Some(entry) = host.entry_vms.remove(&launch_id) {
                    release_browser_target_startup(host, entry.framework, entry.caller);
                    close_browser_entry_endpoint(host, entry.caller)?;
                }
                host.entry_supervisor
                    .close_launch(launch_id)
                    .map_err(|error| {
                        JsValue::from_str(&format!("close browser Voplay entry: {error:?}"))
                    })?;
            }
            host.voplay_engines
                .mark_stopped(engine)
                .and_then(|_| host.voplay_engines.release(engine))
                .map_err(|error| {
                    JsValue::from_str(&format!("release browser Voplay Engine: {error:?}"))
                })?;
            Ok(Some(vec![0]))
        }
    }
}

fn dispatch_browser_voplay_engine_lifecycle(
    host: &mut BrowserSessionHost,
    engine: vo_app_runtime::VoplayPublicEngineRef,
    kind: u16,
) -> Result<(), JsValue> {
    let launch = *host
        .voplay_engine_launches
        .get(&engine)
        .ok_or_else(|| JsValue::from_str("browser Voplay Engine has no entry"))?;
    let caller = host
        .entry_vms
        .get(&launch)
        .ok_or_else(|| JsValue::from_str("browser Voplay target is not active"))?
        .caller;
    let module_key = host
        .active_framework_providers
        .keys()
        .find(|module_key| {
            browser_framework_module_matches(module_key, vo_app_runtime::EntryFramework::Voplay)
        })
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser Voplay provider disappeared"))?;
    let services = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| JsValue::from_str("browser runtime has no HostServices V2 owner"))?;
    let endpoint = host
        .guest
        .host_caller()
        .ok_or_else(|| JsValue::from_str("browser runtime has no hosted endpoint"))?;
    let roles = [
        vo_app_runtime::ProviderRole::GameLogic,
        vo_app_runtime::ProviderRole::GameAsset,
        vo_app_runtime::ProviderRole::GameRenderer,
        vo_app_runtime::ProviderRole::GameAudio,
    ];
    for role in roles {
        let role_tag = match role {
            vo_app_runtime::ProviderRole::GameAsset => 1,
            vo_app_runtime::ProviderRole::GameRenderer => 2,
            vo_app_runtime::ProviderRole::GameAudio => 3,
            vo_app_runtime::ProviderRole::GameLogic => 4,
            _ => unreachable!(),
        };
        let initialized = (
            module_key.clone(),
            caller.endpoint_index,
            caller.endpoint_generation,
            role_tag,
        );
        let Some(channel_epoch) = host.voplay_role_engine_epochs.get(&initialized).copied() else {
            continue;
        };
        let lane = host
            .framework_lanes
            .values()
            .find(|lane| lane.module_key == module_key && lane.role == role)
            .ok_or_else(|| JsValue::from_str("browser Voplay lifecycle lane disappeared"))?;
        let packet = encode_browser_voplay_engine_lifecycle_packet(
            kind,
            (caller.endpoint_index, caller.endpoint_generation),
            channel_epoch,
        )
        .map_err(|error| JsValue::from_str(&error))?;
        services
            .publish_named_endpoint_payload(endpoint, lane.owner.as_bytes(), &packet)
            .map_err(|status| {
                JsValue::from_str(&format!(
                    "publish Voplay lifecycle to browser lane: status {status}"
                ))
            })?;
        if kind == 16 {
            host.voplay_role_engines_initialized.remove(&initialized);
            host.voplay_role_engine_epochs.remove(&initialized);
        }
    }
    if kind == 16 {
        host.voplay_render_features_initialized.remove(&(
            module_key,
            caller.endpoint_index,
            caller.endpoint_generation,
        ));
    }
    Ok(())
}

fn set_browser_voplay_clock(
    host: &mut BrowserSessionHost,
    engine: vo_app_runtime::VoplayPublicEngineRef,
    paused: bool,
) -> Result<(), JsValue> {
    let launch = *host
        .voplay_engine_launches
        .get(&engine)
        .ok_or_else(|| JsValue::from_str("browser Voplay Engine has no entry"))?;
    let caller = host
        .entry_vms
        .get(&launch)
        .ok_or_else(|| JsValue::from_str("browser Voplay target is not active"))?
        .caller;
    browser_voplay_group_mut(host)?
        .set_voplay_clock_paused(caller, paused)
        .map_err(|error| JsValue::from_str(&error))
}

fn dispatch_browser_host_requests(
    handle: SessionHandle,
    host: &mut BrowserSessionHost,
) -> Result<(), JsValue> {
    while let Some(command) = host
        .guest
        .try_take_host_request_command()
        .map_err(|error| JsValue::from_str(&error))?
    {
        let (caller, request_id) = match &command {
            HostRequestCommand::Begin {
                caller, request_id, ..
            }
            | HostRequestCommand::Cancel { caller, request_id } => (*caller, *request_id),
        };
        if let HostRequestCommand::Begin {
            capability_name,
            payload,
            host_wait_key,
            ..
        } = &command
        {
            let public_engine_capability = matches!(
                capability_name.as_slice(),
                value if value == vo_app_runtime::CAPABILITY_VOPLAY_NEW_ENGINE.as_bytes()
                    || value == vo_app_runtime::CAPABILITY_VOPLAY_INSTALL_ENTRY.as_bytes()
                    || value == vo_app_runtime::CAPABILITY_VOPLAY_ENGINE_START.as_bytes()
                    || value == vo_app_runtime::CAPABILITY_VOPLAY_ENGINE_STEP.as_bytes()
                    || value == vo_app_runtime::CAPABILITY_VOPLAY_ENGINE_PAUSE.as_bytes()
                    || value == vo_app_runtime::CAPABILITY_VOPLAY_ENGINE_RESUME.as_bytes()
                    || value == vo_app_runtime::CAPABILITY_VOPLAY_ENGINE_SHUTDOWN.as_bytes()
            );
            if public_engine_capability {
                let result = vo_app_runtime::decode_voplay_engine_command(capability_name, payload)
                    .map_err(|error| format!("decode browser Voplay Engine command: {error:?}"))
                    .and_then(|engine_command| {
                        dispatch_browser_voplay_engine_command(
                            host,
                            caller,
                            request_id,
                            *host_wait_key,
                            engine_command,
                        )
                        .map_err(|error| {
                            error
                                .as_string()
                                .unwrap_or_else(|| String::from("browser Voplay Engine failed"))
                        })
                    });
                match result {
                    Ok(Some(response)) => finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::Success,
                        response,
                    )?,
                    Ok(None) => {}
                    Err(error) => {
                        let mut response = vec![1];
                        response.extend_from_slice(error.as_bytes());
                        finish_browser_host_request_for(
                            host,
                            caller,
                            request_id,
                            RequestOutcome::ProviderError,
                            response,
                        )?;
                    }
                }
                continue;
            }
        }
        let is_entry_begin = matches!(
            &command,
            HostRequestCommand::Begin {
                capability_name,
                ..
            } if capability_name.as_slice()
                == vo_app_runtime::CAPABILITY_VOGUI_RUN_ENTRY.as_bytes()
                || capability_name.as_slice()
                    == vo_app_runtime::CAPABILITY_VOPLAY_RUN_ENTRY.as_bytes()
        );
        let is_cancel = matches!(&command, HostRequestCommand::Cancel { .. });
        if is_entry_begin || is_cancel {
            match command.enqueue_entry_launch(&host.resolved_plan, &mut host.entry_supervisor) {
                Ok(Some(launch_id)) if is_entry_begin => {
                    let launch = host
                        .entry_supervisor
                        .take_construct_command()
                        .map_err(|error| {
                            JsValue::from_str(&format!(
                                "take browser entry launch command: {error:?}"
                            ))
                        })?
                        .ok_or_else(|| {
                            JsValue::from_str("browser entry launch command was not queued")
                        })?;
                    if launch.launch_id != launch_id {
                        return Err(JsValue::from_str(
                            "browser entry launch queue identity mismatch",
                        ));
                    }
                    match launch_browser_entry_island(host, &launch) {
                        Ok(entry) => {
                            let ready = !entry.awaiting_ready;
                            host.entry_vms.insert(launch_id, entry);
                            if ready {
                                host.entry_supervisor
                                    .mark_running(launch_id)
                                    .map_err(|error| {
                                        JsValue::from_str(&format!(
                                            "ready browser entry launch: {error:?}"
                                        ))
                                    })?;
                            }
                        }
                        Err(error) => {
                            host.entry_supervisor
                                .fail(launch_id, error.as_bytes())
                                .map_err(|error| {
                                    JsValue::from_str(&format!(
                                        "fail browser entry launch: {error:?}"
                                    ))
                                })?;
                        }
                    }
                    finish_browser_entry_launches(host)?;
                    continue;
                }
                Ok(Some(launch_id)) if is_cancel => {
                    if let Some(entry) = host.entry_vms.remove(&launch_id) {
                        release_browser_target_startup(host, entry.framework, entry.caller);
                        close_browser_entry_endpoint(host, entry.caller)?;
                    }
                    finish_browser_entry_launches(host)?;
                    continue;
                }
                Ok(Some(_)) => {
                    return Err(JsValue::from_str(
                        "browser entry launch classification changed during dispatch",
                    ));
                }
                Ok(None) if is_entry_begin => {
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::ProviderError,
                        vec![1, b'i', b'n', b'v', b'a', b'l', b'i', b'd'],
                    )?;
                    continue;
                }
                Ok(None) => {}
                Err(error) if is_entry_begin => {
                    let mut response = vec![1];
                    response
                        .extend_from_slice(format!("entry launch rejected: {error:?}").as_bytes());
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::ProviderError,
                        response,
                    )?;
                    continue;
                }
                Err(error) => {
                    return Err(JsValue::from_str(&format!(
                        "cancel browser entry launch: {error:?}"
                    )));
                }
            }
        }
        match command {
            HostRequestCommand::Begin {
                request_id,
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == vo_app_runtime::CAPABILITY_VOGUI_TARGET_NEXT_TURN.as_bytes() =>
            {
                if !payload.is_empty() {
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::ProviderError,
                        vec![1, b'i', b'n', b'v', b'a', b'l', b'i', b'd'],
                    )?;
                    continue;
                }
                let launch_id = host
                    .entry_vms
                    .iter()
                    .find_map(|(launch_id, entry)| (entry.caller == caller).then_some(*launch_id))
                    .ok_or_else(|| JsValue::from_str("browser Vogui turn caller is not active"))?;
                let entry = host
                    .entry_vms
                    .get(&launch_id)
                    .ok_or_else(|| JsValue::from_str("browser Vogui target disappeared"))?;
                if entry.framework != vo_app_runtime::EntryFramework::Vogui
                    || !entry.startup_bound
                    || entry.pending_vogui_turn.is_some()
                {
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::ProviderError,
                        vec![1, b'i', b'n', b'v', b'a', b'l', b'i', b'd'],
                    )?;
                    continue;
                }
                let key = host
                    .active_framework_providers
                    .keys()
                    .find(|module_key| {
                        browser_framework_module_matches(
                            module_key,
                            vo_app_runtime::EntryFramework::Vogui,
                        )
                    })
                    .cloned()
                    .ok_or_else(|| JsValue::from_str("browser Vogui provider is not active"))?;
                let turn = host
                    .active_framework_providers
                    .get_mut(&key)
                    .ok_or_else(|| JsValue::from_str("browser Vogui provider disappeared"))?
                    .take_vogui_target_turn(caller)
                    .map_err(|error| JsValue::from_str(&error))?;
                if let Some(turn) = turn {
                    let mut response = Vec::with_capacity(1 + turn.len());
                    response.push(0);
                    response.extend_from_slice(&turn);
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::Success,
                        response,
                    )?;
                } else if let Some(entry) = host.entry_vms.get_mut(&launch_id) {
                    entry.pending_vogui_turn = Some(request_id);
                }
            }
            HostRequestCommand::Begin {
                request_id,
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == vo_app_runtime::CAPABILITY_VOPLAY_TARGET_NEXT_TICKS.as_bytes() =>
            {
                if !payload.is_empty() {
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::ProviderError,
                        vec![1, b'i', b'n', b'v', b'a', b'l', b'i', b'd'],
                    )?;
                    continue;
                }
                let launch_id = host
                    .entry_vms
                    .iter()
                    .find_map(|(launch_id, entry)| (entry.caller == caller).then_some(*launch_id))
                    .ok_or_else(|| JsValue::from_str("browser Voplay tick caller is not active"))?;
                let entry = host
                    .entry_vms
                    .get(&launch_id)
                    .ok_or_else(|| JsValue::from_str("browser Voplay target disappeared"))?;
                if entry.framework != vo_app_runtime::EntryFramework::Voplay
                    || !entry.startup_bound
                    || entry.pending_voplay_tick_turn.is_some()
                {
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::ProviderError,
                        vec![1, b'i', b'n', b'v', b'a', b'l', b'i', b'd'],
                    )?;
                    continue;
                }
                let key = host
                    .active_framework_providers
                    .keys()
                    .find(|module_key| {
                        browser_framework_module_matches(
                            module_key,
                            vo_app_runtime::EntryFramework::Voplay,
                        )
                    })
                    .cloned()
                    .ok_or_else(|| JsValue::from_str("browser Voplay provider is not active"))?;
                let turn = host
                    .active_framework_providers
                    .get_mut(&key)
                    .ok_or_else(|| JsValue::from_str("browser Voplay provider disappeared"))?
                    .take_voplay_tick_turn(caller)
                    .map_err(|error| JsValue::from_str(&error))?;
                if let Some(turn) = turn {
                    let mut response = Vec::with_capacity(1 + turn.len());
                    response.push(0);
                    response.extend_from_slice(&turn);
                    finish_browser_host_request_for(
                        host,
                        caller,
                        request_id,
                        RequestOutcome::Success,
                        response,
                    )?;
                } else if let Some(entry) = host.entry_vms.get_mut(&launch_id) {
                    entry.pending_voplay_tick_turn = Some(request_id);
                }
            }
            HostRequestCommand::Begin {
                request_id,
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == vo_app_runtime::CAPABILITY_VOPLAY_TARGET_COMMIT_TICKS.as_bytes() =>
            {
                let result = vo_app_runtime::decode_voplay_tick_commit(&payload)
                    .map_err(|error| format!("decode Voplay target tick commit: {error:?}"))
                    .and_then(|commit| {
                        let key = host
                            .active_framework_providers
                            .keys()
                            .find(|module_key| {
                                browser_framework_module_matches(
                                    module_key,
                                    vo_app_runtime::EntryFramework::Voplay,
                                )
                            })
                            .cloned()
                            .ok_or_else(|| String::from("browser Voplay provider is not active"))?;
                        host.active_framework_providers
                            .get_mut(&key)
                            .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                            .commit_voplay_tick(
                                caller,
                                commit.first_tick,
                                commit.count,
                                commit.result,
                            )?;
                        dispatch_browser_voplay_outboxes(host, &key, caller)
                    });
                let (outcome, response) = match result {
                    Ok(_) => (RequestOutcome::Success, vec![0]),
                    Err(error) => {
                        let mut response = vec![1];
                        response.extend_from_slice(error.as_bytes());
                        (RequestOutcome::ProviderError, response)
                    }
                };
                finish_browser_host_request_for(host, caller, request_id, outcome, response)?;
            }
            HostRequestCommand::Begin {
                request_id,
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == vo_app_runtime::CAPABILITY_VOGUI_TARGET_COMMIT.as_bytes() =>
            {
                let provider_ingress = payload.clone();
                let result = vo_app_runtime::decode_vogui_target_commit(&payload)
                    .map_err(|error| format!("decode Vogui target commit: {error:?}"))
                    .and_then(|commit| {
                        let key = host
                            .active_framework_providers
                            .keys()
                            .find(|module_key| {
                                browser_framework_module_matches(
                                    module_key,
                                    vo_app_runtime::EntryFramework::Vogui,
                                )
                            })
                            .cloned()
                            .ok_or_else(|| String::from("browser Vogui provider is not active"))?;
                        host.active_framework_providers
                            .get(&key)
                            .ok_or_else(|| String::from("browser Vogui provider disappeared"))?
                            .preflight_vogui_target_state(
                                caller,
                                &commit.model,
                                &commit.update_result,
                                &commit.effects,
                                &commit.presentation,
                                &commit.subscriptions,
                            )?;
                        Ok((key, commit, provider_ingress))
                    });
                match result {
                    Ok((key, commit, provider_ingress)) => {
                        if host.pending_vogui_commit.is_some() {
                            finish_browser_host_request_for(
                                host,
                                caller,
                                request_id,
                                RequestOutcome::ProviderError,
                                b"browser Vogui provider already has a pending commit".to_vec(),
                            )?;
                            continue;
                        }
                        host.render.push(provider_ingress);
                        host.pending_vogui_commit = Some(PendingBrowserVoguiCommit {
                            caller,
                            request_id,
                            module_key: key,
                            commit,
                        });
                    }
                    Err(error) => {
                        let mut response = vec![1];
                        response.extend_from_slice(error.as_bytes());
                        finish_browser_host_request_for(
                            host,
                            caller,
                            request_id,
                            RequestOutcome::ProviderError,
                            response,
                        )?;
                    }
                }
            }
            HostRequestCommand::Cancel { request_id, .. }
                if host.entry_vms.values().any(|entry| {
                    entry.caller == caller && entry.pending_vogui_turn == Some(request_id)
                }) =>
            {
                let entry = host
                    .entry_vms
                    .values_mut()
                    .find(|entry| {
                        entry.caller == caller && entry.pending_vogui_turn == Some(request_id)
                    })
                    .expect("guard certified pending browser Vogui turn");
                entry.pending_vogui_turn = None;
                if host.pending_vogui_commit.as_ref().is_some_and(|pending| {
                    pending.caller == caller && pending.request_id == request_id
                }) {
                    host.pending_vogui_commit = None;
                }
                finish_browser_host_request_for(
                    host,
                    caller,
                    request_id,
                    RequestOutcome::Cancelled,
                    Vec::new(),
                )?;
            }
            HostRequestCommand::Cancel { request_id, .. }
                if host.entry_vms.values().any(|entry| {
                    entry.caller == caller && entry.pending_voplay_tick_turn == Some(request_id)
                }) =>
            {
                let entry = host
                    .entry_vms
                    .values_mut()
                    .find(|entry| {
                        entry.caller == caller && entry.pending_voplay_tick_turn == Some(request_id)
                    })
                    .expect("guard certified pending browser Voplay tick");
                entry.pending_voplay_tick_turn = None;
                finish_browser_host_request_for(
                    host,
                    caller,
                    request_id,
                    RequestOutcome::Cancelled,
                    Vec::new(),
                )?;
            }
            HostRequestCommand::Begin {
                request_id,
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == vo_app_runtime::CAPABILITY_VOGUI_TARGET_INIT.as_bytes()
                || capability_name.as_slice()
                    == vo_app_runtime::CAPABILITY_VOPLAY_TARGET_START.as_bytes() =>
            {
                let vogui_provider_ingress = (capability_name.as_slice()
                    == vo_app_runtime::CAPABILITY_VOGUI_TARGET_INIT.as_bytes())
                .then(|| payload.clone());
                let result = vo_app_runtime::decode_target_startup(&capability_name, &payload)
                    .map_err(|error| format!("decode target startup: {error:?}"))
                    .and_then(|startup| {
                        let entry = host
                            .entry_vms
                            .values_mut()
                            .find(|entry| entry.caller == caller)
                            .ok_or_else(|| {
                                String::from("target startup caller has no browser entry")
                            })?;
                        if entry.framework != startup.framework() || entry.startup_bound {
                            return Err(format!(
                                "target startup state mismatch: entry={:?} startup={:?} bound={}",
                                entry.framework,
                                startup.framework(),
                                entry.startup_bound
                            ));
                        }
                        bind_browser_target_startup(host, caller, startup)?;
                        let entry = host
                            .entry_vms
                            .values_mut()
                            .find(|entry| entry.caller == caller)
                            .ok_or_else(|| {
                                String::from("bound target startup browser entry disappeared")
                            })?;
                        entry.startup_bound = true;
                        Ok(())
                    });
                let (outcome, response) = match result {
                    Ok(()) => {
                        if let Some(provider_ingress) = vogui_provider_ingress {
                            host.render.push(provider_ingress);
                        }
                        let launch_id = host
                            .entry_vms
                            .iter()
                            .find_map(|(launch_id, entry)| {
                                (entry.caller == caller).then_some(*launch_id)
                            })
                            .ok_or_else(|| {
                                JsValue::from_str(
                                    "initialized browser target has no entry launch identity",
                                )
                            })?;
                        if let Some(entry) = host.entry_vms.get_mut(&launch_id) {
                            entry.awaiting_ready = false;
                        }
                        host.entry_supervisor
                            .mark_running(launch_id)
                            .map_err(|error| {
                                JsValue::from_str(&format!(
                                    "ready initialized browser target: {error:?}"
                                ))
                            })?;
                        if let Some(engine) = host.voplay_engine_launches.iter().find_map(
                            |(engine, mapped_launch)| {
                                (*mapped_launch == launch_id).then_some(*engine)
                            },
                        ) {
                            host.voplay_engines.mark_running(engine).map_err(|error| {
                                JsValue::from_str(&format!(
                                    "ready browser Voplay Engine: {error:?}"
                                ))
                            })?;
                            if host
                                .voplay_engines
                                .descriptor(engine)
                                .map_err(|error| {
                                    JsValue::from_str(&format!(
                                        "read browser Voplay Engine: {error:?}"
                                    ))
                                })?
                                .headless
                            {
                                browser_voplay_group_mut(host)?
                                    .set_voplay_clock_paused(caller, true)
                                    .map_err(|error| JsValue::from_str(&error))?;
                            }
                        }
                        finish_browser_host_request_for(
                            host,
                            caller,
                            request_id,
                            RequestOutcome::Success,
                            vec![0],
                        )?;
                        finish_browser_entry_launches(host)?;
                        continue;
                    }
                    Err(error) => {
                        let mut response = vec![1];
                        response.extend_from_slice(
                            format!("target startup rejected: {error}").as_bytes(),
                        );
                        (RequestOutcome::ProviderError, response)
                    }
                };
                finish_browser_host_request_for(host, caller, request_id, outcome, response)?;
            }
            HostRequestCommand::Begin {
                request_id,
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == vo_app_runtime::CAPABILITY_APP_TIMER_ONCE.as_bytes() =>
            {
                let delay = payload
                    .as_slice()
                    .try_into()
                    .ok()
                    .map(u64::from_le_bytes)
                    .filter(|delay| *delay > 0);
                if let Some(delay) = delay {
                    let now = browser_monotonic_millis()?;
                    let owner = host.guest.host_services_v2().cloned().ok_or_else(|| {
                        JsValue::from_str("browser timer has no HostServices V2 owner")
                    })?;
                    owner.set_monotonic_time(now);
                    if let Ok(timer_handle) =
                        owner.schedule_request_timer(caller, request_id, delay)
                    {
                        if schedule_browser_timer_chunk(
                            handle,
                            host,
                            caller,
                            request_id,
                            timer_handle,
                            delay,
                        )
                        .is_ok()
                        {
                            continue;
                        }
                        let _ = owner.cancel_request_timer(caller, timer_handle);
                    }
                }
                finish_browser_host_request_for(
                    host,
                    caller,
                    request_id,
                    RequestOutcome::ProviderError,
                    Vec::new(),
                )?;
            }
            HostRequestCommand::Cancel { request_id, .. }
                if cancel_browser_timer(host, caller, request_id)? =>
            {
                finish_browser_host_request_for(
                    host,
                    caller,
                    request_id,
                    RequestOutcome::Cancelled,
                    Vec::new(),
                )?;
            }
            command => {
                host.external_request_callers.insert(request_id, caller);
                host.host_requests.push_back(command);
            }
        }
    }
    schedule_browser_framework_clock_wake(handle, host)?;
    Ok(())
}

fn launch_browser_entry_island(
    host: &BrowserSessionHost,
    launch: &EntryIslandConstructCommand,
) -> Result<BrowserEntryVm, String> {
    let mut vm = vo_web::create_loaded_vm(
        &host.entry_bytecode,
        vo_web::ext_bridge::register_wasm_ext_bridges,
    )?;
    apply_gc_stress_config(&mut vm);
    let services = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("browser entry island has no HostServices V2 owner"))?;
    let parent = host
        .guest
        .host_caller()
        .ok_or_else(|| String::from("browser entry island has no caller endpoint"))?;
    let role = match launch.framework {
        vo_app_runtime::EntryFramework::Vogui => vo_app_runtime::EndpointRole::UiExecutor,
        vo_app_runtime::EntryFramework::Voplay => vo_app_runtime::EndpointRole::EngineLogic,
    };
    let caller = services
        .register_child_endpoint(
            parent,
            role,
            vo_app_runtime::PlacementDomain::WasmMain,
            host.resolved_plan.granted_capabilities.clone(),
        )
        .map_err(|status| format!("register browser entry endpoint: status {status}"))?;
    let result = (|| {
        let binding_owner: vo_runtime::host_services_v2::SharedHostServicesV2 = services.clone();
        vm.set_host_services_v2(binding_owner, caller)
            .map_err(|error| format!("install browser entry HostServices V2: {error:?}"))?;
        match vm
            .run_init()
            .map_err(|error| format!("initialize browser entry island: {error:?}"))?
        {
            vo_vm::vm::SchedulingOutcome::Completed => {}
            outcome => {
                return Err(format!(
                    "browser entry island initialization ended with {outcome:?}"
                ));
            }
        }
        vm.spawn_entry_factory(launch.function_id, &launch.init)
            .map_err(|error| format!("spawn browser entry factory: {error:?}"))?;
        let scheduling = vm
            .run_scheduled()
            .map_err(|error| format!("run browser entry factory: {error:?}"))?;
        match scheduling {
            vo_vm::vm::SchedulingOutcome::Blocked => Err(String::from(
                "browser entry factory entered its lifecycle without startup state",
            )),
            vo_vm::vm::SchedulingOutcome::Suspended
            | vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents => Ok(BrowserEntryVm {
                vm,
                caller,
                framework: launch.framework,
                startup_bound: false,
                awaiting_ready: true,
                pending_vogui_turn: None,
                pending_voplay_tick_turn: None,
            }),
            outcome => Err(format!(
                "browser entry factory ended before entering its owned lifecycle: {outcome:?}"
            )),
        }
    })();
    if result.is_err() {
        let _ = services.close_child_endpoint(parent, caller);
    }
    result
}

fn close_browser_entry_endpoint(
    host: &BrowserSessionHost,
    child: vo_runtime::host_services_v2::CallerEndpointHandle,
) -> Result<(), JsValue> {
    let owner = host
        .guest
        .host_services_v2()
        .ok_or_else(|| JsValue::from_str("browser entry runtime has no HostServices V2 owner"))?;
    let parent = host
        .guest
        .host_caller()
        .ok_or_else(|| JsValue::from_str("browser entry runtime has no bootstrap caller"))?;
    owner.close_child_endpoint(parent, child).map_err(|status| {
        JsValue::from_str(&format!(
            "close browser entry endpoint failed with status {status}"
        ))
    })
}

fn clear_browser_host_state(host: &mut BrowserSessionHost) -> Result<(), String> {
    let mut failures = Vec::new();
    host.framework_lanes.clear();
    host.pending_vogui_commit = None;
    if let Err(error) = clear_browser_framework_clock_wake(host) {
        failures.push(format!(
            "clear framework clock wake: {}",
            error
                .as_string()
                .unwrap_or_else(|| String::from("unknown error"))
        ));
    }
    let pending = core::mem::take(&mut host.pending_framework_providers);
    for (module_key, group) in pending {
        if let Err(error) = group.rollback() {
            failures.push(format!("rollback {module_key}: {error}"));
        }
    }
    let active = core::mem::take(&mut host.active_framework_providers);
    for (module_key, group) in active {
        if let Err(error) = group.close() {
            failures.push(format!("close {module_key}: {error}"));
        }
    }
    let loaded = core::mem::take(&mut host.loaded_framework_providers);
    for module_key in loaded {
        if let Some(binding) = host.framework_provider_bindings.get(&module_key) {
            for provider in binding.providers.iter().rev() {
                if let Err(error) = host.guest.unload_provider_factory(provider.template_id) {
                    failures.push(format!(
                        "unload {module_key} role {:?}: {error}",
                        provider.loaded.role
                    ));
                }
            }
        }
    }
    match host.guest.host_provider_live_counts() {
        Ok((1, 1)) => {}
        Ok((groups, instances)) => failures.push(format!(
            "preview retained provider groups={groups} instances={instances} before Session close"
        )),
        Err(error) => failures.push(format!("inspect provider live counts: {error}")),
    }
    let request_ids = host.timer_ids.keys().copied().collect::<Vec<_>>();
    for request_id in request_ids {
        let _ = clear_browser_timer(host, request_id);
    }
    host.host_requests.clear();
    host.external_request_callers.clear();
    host.entry_launches.clear();
    let entries = core::mem::take(&mut host.entry_vms);
    for (_, entry) in entries {
        let _ = &entry.vm;
        release_browser_target_startup(host, entry.framework, entry.caller);
        if let Err(error) = close_browser_entry_endpoint(host, entry.caller) {
            failures.push(format!(
                "close browser entry endpoint: {}",
                error
                    .as_string()
                    .unwrap_or_else(|| String::from("unknown error"))
            ));
        }
    }
    if let Err(error) = host.entry_supervisor.close() {
        failures.push(format!("close entry launch supervisor: {error:?}"));
    }
    if failures.is_empty() {
        Ok(())
    } else {
        Err(failures.join("; "))
    }
}

#[wasm_bindgen(js_name = "registerBrowserRuntimeHostArtifact")]
pub fn register_browser_runtime_host_artifact(bytes: &[u8]) -> Result<(), JsValue> {
    if bytes.is_empty() || bytes.len() > MAX_BROWSER_HOST_ARTIFACT_BYTES {
        return Err(JsValue::from_str(
            "browser runtime host artifact must be non-empty and at most 256 MiB",
        ));
    }
    let digest = app_plan::sha256_bytes(bytes);
    BROWSER_RUNTIME_HOST_DIGEST.with(|current| {
        let mut current = current.borrow_mut();
        if current.is_some_and(|existing| existing != digest) {
            return Err(JsValue::from_str(
                "browser runtime host artifact changed within one Studio WASM instance",
            ));
        }
        *current = Some(digest);
        Ok(())
    })
}

fn next_browser_identity(
    cell: &'static std::thread::LocalKey<Cell<u64>>,
    label: &str,
) -> Result<u64, String> {
    cell.with(|next| {
        let value = next.get();
        if value == 0 || value == u64::MAX {
            return Err(format!("{label} exhausted"));
        }
        next.set(value + 1);
        Ok(value)
    })
}

fn build_prepared_gui_launch(
    entry_path: &str,
    bytecode: &[u8],
    runtime_plan: vo_web::BrowserRuntimePlan,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
) -> Result<PreparedGuiLaunch, String> {
    let intent = runtime_plan.artifact_intent()?;
    let mut extension_indices = BTreeMap::new();
    let mut extensions = Vec::<vo_web::ReadyWasmExtensionBytes>::new();
    let browser_artifacts = vo_web::materialized_browser_artifacts_from_vfs(
        &intent,
        &runtime_plan,
        |artifact, bytes| match artifact.role {
            vo_web::MaterializedBrowserArtifactRole::WasmModule => {
                if extension_indices.contains_key(&artifact.module_key) {
                    return Err(format!(
                        "browser GUI launch contains duplicate WASM owner {}",
                        artifact.module_key
                    ));
                }
                extension_indices.insert(artifact.module_key.clone(), extensions.len());
                extensions.push(vo_web::ReadyWasmExtensionBytes {
                    name: artifact.extension_name.clone(),
                    module_key: artifact.module_key.clone(),
                    wasm_bytes: bytes,
                    js_glue_bytes: None,
                });
                Ok(())
            }
            vo_web::MaterializedBrowserArtifactRole::JavaScriptGlue => {
                let index = extension_indices.get(&artifact.module_key).ok_or_else(|| {
                    format!(
                        "browser GUI launch contains JavaScript glue without WASM for {}",
                        artifact.module_key
                    )
                })?;
                let extension = &mut extensions[*index];
                if extension.name != artifact.extension_name || extension.js_glue_bytes.is_some() {
                    return Err(format!(
                        "browser GUI launch contains conflicting JavaScript glue for {}",
                        artifact.module_key
                    ));
                }
                extension.js_glue_bytes = Some(bytes);
                Ok(())
            }
            vo_web::MaterializedBrowserArtifactRole::JavaScriptModule => Ok(()),
        },
    )?;
    if browser_artifacts.len() > vo_app_runtime::MAX_RUNTIME_PLAN_ARTIFACTS.saturating_sub(3) {
        return Err(format!(
            "browser GUI launch materialized {} artifacts, exceeding the AppBuildPlan budget",
            browser_artifacts.len()
        ));
    }
    Ok(PreparedGuiLaunch {
        entry_path: entry_path.to_string(),
        bytecode_digest: app_plan::sha256_bytes(bytecode),
        runtime_plan,
        browser_artifacts,
        locked_modules,
        extensions,
    })
}

fn prepare_gui_launch(
    entry_path: &str,
    bytecode: &[u8],
    runtime_plan: vo_web::BrowserRuntimePlan,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
) -> Result<u64, String> {
    ensure_prepared_gui_launch_capacity()?;
    let launch = build_prepared_gui_launch(entry_path, bytecode, runtime_plan, locked_modules)?;
    let launch_bytes = launch.extension_payload_bytes()?;
    let token = next_browser_identity(&NEXT_PREPARED_GUI_LAUNCH, "prepared GUI launch identity")?;
    PREPARED_GUI_LAUNCHES.with(|launches| {
        let mut launches = launches.borrow_mut();
        if launches.len() >= MAX_GUI_PREVIEWS {
            return Err(String::from(
                "cannot prepare GUI launch: launch capacity reached",
            ));
        }
        let retained_bytes = launches.values().try_fold(launch_bytes, |total, launch| {
            total
                .checked_add(launch.extension_payload_bytes()?)
                .ok_or_else(|| String::from("prepared GUI launch byte count overflow"))
        })?;
        if retained_bytes > vo_web::MAX_BROWSER_SNAPSHOT_BYTES {
            return Err(format!(
                "prepared GUI launches retain more than {} extension bytes",
                vo_web::MAX_BROWSER_SNAPSHOT_BYTES
            ));
        }
        launches.insert(token, launch);
        Ok(token)
    })
}

fn ensure_prepared_gui_launch_capacity() -> Result<(), String> {
    PREPARED_GUI_LAUNCHES.with(|launches| {
        if launches.borrow().len() >= MAX_GUI_PREVIEWS {
            Err(String::from(
                "cannot prepare GUI launch: launch capacity reached",
            ))
        } else {
            Ok(())
        }
    })
}

fn parse_prepared_gui_launch_token(token: &str) -> Result<u64, JsValue> {
    let parsed = token
        .parse::<u64>()
        .map_err(|_| JsValue::from_str("prepared GUI launch token is invalid"))?;
    if parsed == 0 || parsed.to_string() != token {
        return Err(JsValue::from_str(
            "prepared GUI launch token is not canonical",
        ));
    }
    Ok(parsed)
}

fn take_prepared_gui_launch(
    token: &str,
    entry_path: &str,
    bytecode: &[u8],
) -> Result<PreparedGuiLaunch, JsValue> {
    let token = parse_prepared_gui_launch_token(token)?;
    let launch = PREPARED_GUI_LAUNCHES.with(|launches| {
        launches
            .borrow_mut()
            .remove(&token)
            .ok_or_else(|| JsValue::from_str("prepared GUI launch token is unknown or consumed"))
    })?;
    if launch.entry_path != entry_path || launch.bytecode_digest != app_plan::sha256_bytes(bytecode)
    {
        return Err(JsValue::from_str(
            "prepared GUI launch does not match entry path and bytecode",
        ));
    }
    Ok(launch)
}

#[wasm_bindgen(js_name = "discardPreparedGuiLaunch")]
pub fn discard_prepared_gui_launch(token: &str) -> Result<(), JsValue> {
    let token = parse_prepared_gui_launch_token(token)?;
    PREPARED_GUI_LAUNCHES.with(|launches| {
        launches.borrow_mut().remove(&token);
        Ok(())
    })
}

struct LoadedBrowserGuest {
    guest: GuestRuntime,
    resolved_plan: vo_app_runtime::ResolvedAppRuntimePlan,
    browser_artifacts: Vec<vo_web::MaterializedBrowserArtifact>,
    framework_provider_bindings: Vec<app_plan::FrameworkProviderBinding>,
}

fn load_gui_app_from_bytecode(
    bytecode: &[u8],
    prepared: PreparedGuiLaunch,
) -> Result<LoadedBrowserGuest, JsValue> {
    let mut vm = vo_web::create_loaded_vm(bytecode, vo_web::ext_bridge::register_wasm_ext_bridges)
        .map_err(|e| JsValue::from_str(&e))?;
    apply_gc_stress_config(&mut vm);
    let host_digest = BROWSER_RUNTIME_HOST_DIGEST.with(|digest| {
        digest
            .borrow()
            .as_ref()
            .copied()
            .ok_or_else(|| JsValue::from_str("browser runtime host artifact is not registered"))
    })?;
    let plan_generation =
        next_browser_identity(&NEXT_APP_PLAN_GENERATION, "browser AppBuildPlan generation")
            .map_err(|error| JsValue::from_str(&error))?;
    let resolved_plan = app_plan::materialize_browser_studio_plan(
        bytecode,
        &prepared.runtime_plan,
        &prepared.browser_artifacts,
        &prepared.locked_modules,
        host_digest,
        plan_generation,
    )
    .map_err(|error| JsValue::from_str(&error))?;
    let framework_provider_bindings =
        app_plan::framework_provider_bindings(&prepared.runtime_plan, &resolved_plan)
            .map_err(|error| JsValue::from_str(&error))?;
    let retained_plan = resolved_plan.clone();
    let browser_artifacts = prepared.browser_artifacts;
    HOSTED_RUNTIME.with(|runtime| {
        GuestRuntime::new_gui_app_planned_in(runtime, vm, guest_stdout_source(), resolved_plan)
            .map(|guest| LoadedBrowserGuest {
                guest,
                resolved_plan: retained_plan,
                browser_artifacts,
                framework_provider_bindings,
            })
            .map_err(|error| JsValue::from_str(&error))
    })
}

fn close_browser_host(handle: SessionHandle) -> Result<(), JsValue> {
    let mut host = GUESTS.with(|guests| {
        guests
            .borrow_mut()
            .remove(handle)
            .map_err(|error| JsValue::from_str(&format!("invalid preview handle: {error:?}")))
    })?;
    if let Some(host) = host.as_mut() {
        let clear_result = clear_browser_host_state(host);
        host.guest.shutdown();
        let _ = host.render.poll();
        host.closed = true;
        clear_result.map_err(|error| JsValue::from_str(&error))?;
    }
    Ok(())
}

fn preview_handle(index: u32, generation: u32) -> SessionHandle {
    SessionHandle { index, generation }
}

fn preview_handle_to_js(handle: SessionHandle) -> JsValue {
    let value = Object::new();
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("index"),
        &JsValue::from_f64(handle.index as f64),
    );
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("generation"),
        &JsValue::from_f64(handle.generation as f64),
    );
    value.into()
}

fn take_guest_step_render(guest: &GuestRuntime, step: StepResult) -> Result<Vec<u8>, JsValue> {
    publish_guest_stdout(guest, "guest", step.stdout.as_deref())?;
    Ok(step.render_output.unwrap_or_default())
}

fn prepare_gui_from_bytecode_with(
    bytecode: &[u8],
    path_label: &str,
    prepared: PreparedGuiLaunch,
) -> Result<SessionHandle, JsValue> {
    ensure_panic_hook();
    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(vec![path_label.to_string()]);
    });
    let result = (|| {
        GUESTS.with(|guests| {
            let guests = guests.borrow();
            if guests.len() >= guests.capacity() {
                Err(JsValue::from_str(
                    "cannot start preview: session capacity reached",
                ))
            } else {
                Ok(())
            }
        })?;
        let load_start = js_sys::Date::now();
        let LoadedBrowserGuest {
            guest,
            resolved_plan,
            browser_artifacts,
            framework_provider_bindings,
        } = load_gui_app_from_bytecode(bytecode, prepared)?;
        let app_session = guest
            .host_session_handle()
            .ok_or_else(|| JsValue::from_str("browser GUI guest has no App Session identity"))?;
        let app_session_epoch = guest
            .host_session_epoch()
            .map_err(|error| JsValue::from_str(&error))?;
        log_wasm_path("gui_load_vm_done", path_label, "system", Some(load_start));
        let handle = GUESTS.with(|guests| {
            guests
                .borrow_mut()
                .bind(
                    app_session,
                    Some(BrowserSessionHost::new(
                        guest,
                        app_session,
                        app_session_epoch,
                        bytecode.to_vec(),
                        resolved_plan,
                        browser_artifacts,
                        framework_provider_bindings,
                    )),
                )
                .map_err(|error| JsValue::from_str(&format!("cannot start preview: {error:?}")))
        })?;
        if let Err(error) = with_guest_mut(handle, |_| Ok(())) {
            let _ = close_browser_host(handle);
            return Err(error);
        }
        Ok(handle)
    })();
    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = None;
    });
    result
}

fn start_prepared_gui_with<F>(
    handle: SessionHandle,
    path_label: &str,
    start_guest: F,
) -> Result<Vec<u8>, JsValue>
where
    F: FnOnce(&mut GuestRuntime) -> Result<StepResult, SessionError>,
{
    ensure_panic_hook();
    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(vec![path_label.to_string()]);
    });
    let start_start = js_sys::Date::now();
    let result = with_guest_mut(handle, |host| {
        if host.started {
            return Err(JsValue::from_str("browser GUI guest is already started"));
        }
        let step = start_guest(&mut host.guest).map_err(session_error_to_js)?;
        run_gc_stress_guest_step(&mut host.guest);
        let render_output = take_guest_step_render(&host.guest, step)?;
        host.started = true;
        Ok(render_output)
    });
    if result.is_ok() {
        log_wasm_path("gui_start_done", path_label, "system", Some(start_start));
    }
    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = None;
    });
    result
}

// =============================================================================
// VoVm — instance-based VM for render islands (framework-neutral)
// =============================================================================

/// A Vo VM instance with ext_bridge externs registered.
/// Exposes the VoWebModule.VoVm interface expected by render island bootstrappers.
#[wasm_bindgen(js_name = "StudioVoVm")]
pub struct StudioVoVm {
    runtime: RenderIslandRuntime,
    bytecode_dump: String,
}

#[wasm_bindgen(js_class = "StudioVoVm")]
impl StudioVoVm {
    /// Create a VM from bytecode with ext_bridge externs registered.
    /// Corresponds to VoWebModule.VoVm.withExterns(bytecode).
    #[wasm_bindgen(js_name = "withExterns")]
    pub fn with_externs(bytecode: &[u8]) -> Result<StudioVoVm, JsValue> {
        ensure_panic_hook();
        let module =
            decode_verified_module(bytecode, "Studio VM").map_err(|e| JsValue::from_str(&e))?;
        let bytecode_dump = bytecode_text_format::format_text(&module);
        let mut vm = vo_web::create_loaded_vm_from_module(
            module,
            vo_web::ext_bridge::register_wasm_ext_bridges,
        )
        .map_err(|e| JsValue::from_str(&e))?;
        apply_gc_stress_config(&mut vm);
        Ok(StudioVoVm {
            runtime: RenderIslandRuntime::new(vm, guest_stdout_source()),
            bytecode_dump,
        })
    }

    #[wasm_bindgen(js_name = "dumpBytecode")]
    pub fn dump_bytecode(&self) -> String {
        self.bytecode_dump.clone()
    }

    #[wasm_bindgen(js_name = "setGcStressEveryStep")]
    pub fn set_gc_stress_every_step(&mut self, enabled: bool) {
        self.runtime.set_gc_stress_every_step(enabled);
    }

    pub fn run(&mut self) -> Result<String, JsValue> {
        let step = self.runtime.run().map_err(session_error_to_js)?;
        run_gc_stress_render_step(&mut self.runtime);
        flush_stdout("render-island", step.stdout.as_deref());
        Ok(format!("{:?}", step.outcome))
    }

    /// Process exit status supplied by `os.Exit`, or `undefined` while the
    /// render-island guest remains active.
    #[wasm_bindgen(getter, js_name = "exitCode")]
    pub fn exit_code(&self) -> Option<i32> {
        self.runtime.exit_code()
    }

    #[wasm_bindgen(js_name = "runInit")]
    pub fn run_init(&mut self) -> Result<String, JsValue> {
        let step = self.runtime.run_init().map_err(session_error_to_js)?;
        run_gc_stress_render_step(&mut self.runtime);
        flush_stdout("render-island", step.stdout.as_deref());
        Ok(format!("{:?}", step.outcome))
    }

    #[wasm_bindgen(js_name = "runScheduled")]
    pub fn run_scheduled(&mut self) -> Result<String, JsValue> {
        let step = self.runtime.run_scheduled().map_err(session_error_to_js)?;
        run_gc_stress_render_step(&mut self.runtime);
        flush_stdout("render-island", step.stdout.as_deref());
        Ok(format!("{:?}", step.outcome))
    }

    /// Push a frame received from Studio's certified island transport into the
    /// VM command queue (does not run the VM).
    #[wasm_bindgen(js_name = "pushIslandCommand")]
    pub fn push_island_command(&mut self, frame: &[u8]) -> Result<(), JsValue> {
        self.runtime
            .push_inbound_island_frame(frame)
            .map_err(session_error_to_js)?;
        Ok(())
    }

    /// Drain all outbound island transport frames queued since the last call.
    #[wasm_bindgen(js_name = "takeOutboundCommands")]
    pub fn take_outbound_commands(&mut self) -> js_sys::Array {
        let arr = js_sys::Array::new();
        for frame in self.runtime.take_outbound_frames() {
            arr.push(&js_sys::Uint8Array::from(frame.as_slice()));
        }
        arr
    }

    /// Drain pending host events (timers) that JS must schedule.
    /// Each element is { key: string, source: string, token: string, delayMs: number, replay: boolean }.
    #[wasm_bindgen(js_name = "takePendingHostEvents")]
    pub fn take_pending_host_events(&mut self) -> js_sys::Array {
        let arr = js_sys::Array::new();
        for event in self.runtime.take_pending_host_events() {
            arr.push(&pending_host_event_to_js(&event));
        }
        arr
    }

    /// Wake a suspended host event fiber and run scheduled work.
    #[wasm_bindgen(js_name = "wakeHostEvent")]
    pub fn wake_host_event_vm(&mut self, key: &str) -> Result<(), JsValue> {
        let key = HostWaitKey::decode(key).map_err(|e| JsValue::from_str(&e))?;
        self.runtime
            .wake_host_event(key)
            .map_err(session_error_to_js)?;
        Ok(())
    }

    /// Take any stdout produced by the last VM run.
    #[wasm_bindgen(js_name = "takeOutput")]
    pub fn take_output(&self) -> String {
        vo_web::take_output()
    }
}

// =============================================================================
// VoWebModule exports — initVFS
// preloadExtModule is provided by vo-web (3-param version with optional JS glue source).
// =============================================================================

#[wasm_bindgen(js_name = "getBuildId")]
pub fn get_build_id() -> String {
    STUDIO_WASM_BUILD_ID.to_string()
}

/// Render a new module manifest through the same parser and schema used by
/// the native CLI. The browser host owns VFS mutation; Rust owns semantics.
#[wasm_bindgen(js_name = "renderInitialModuleManifest")]
pub fn render_initial_module_manifest(module: &str) -> Result<String, JsValue> {
    vo_module::ops::render_initial_mod_file(module)
        .map_err(|error| js_sys::Error::new(&error.to_string()).into())
}

#[wasm_bindgen(js_name = "initVFS")]
pub fn init_vfs() -> js_sys::Promise {
    ensure_panic_hook();
    wasm_bindgen_futures::future_to_promise(async move { Ok(JsValue::UNDEFINED) })
}

/// Atomically bind a complete host-authenticated static release batch to the
/// browser registry. Merely placing files in the shared VFS never grants
/// package trust.
#[wasm_bindgen(js_name = "registerBrowserReleaseCapabilities")]
pub fn register_browser_release_capabilities(
    modules: Box<[JsValue]>,
    versions: Box<[JsValue]>,
    release_digests: Box<[JsValue]>,
    roots: Box<[JsValue]>,
) -> Result<(), JsValue> {
    let count = modules.len();
    if versions.len() != count || release_digests.len() != count || roots.len() != count {
        return Err(js_sys::Error::new(
            "browser release capability columns must have identical lengths",
        )
        .into());
    }
    if count > vo_web::MAX_PACKAGED_RELEASE_CAPABILITIES {
        return Err(js_sys::Error::new(&format!(
            "browser release capability batch exceeds {} entries",
            vo_web::MAX_PACKAGED_RELEASE_CAPABILITIES,
        ))
        .into());
    }
    let mut specs = Vec::new();
    specs.try_reserve_exact(count).map_err(|_| {
        js_sys::Error::new("failed to reserve the browser release capability batch")
    })?;
    for index in 0..count {
        let string_at = |column: &[JsValue], name: &str| {
            column[index].as_string().ok_or_else(|| {
                js_sys::Error::new(&format!(
                    "browser release capability {name}[{index}] must be a string",
                ))
            })
        };
        specs.push(vo_web::PackagedReleaseCapabilitySpec {
            module: string_at(&modules, "modules")?,
            version: string_at(&versions, "versions")?,
            release_digest: string_at(&release_digests, "releaseDigests")?,
            root: string_at(&roots, "roots")?,
        });
    }
    vo_web::register_packaged_release_capabilities(&specs)
        .map_err(|error| js_sys::Error::new(&error.to_string()).into())
}

// =============================================================================
// FS helpers
// =============================================================================

const VFS_MOD_ROOT: &str = "";
const STUDIO_HOST_PRIVATE_VFS_ROOT: &str = "/__volang_studio_host";
const STUDIO_VFS_COMPILE_CACHE_SCHEMA_VERSION: &str = "5";
const STUDIO_VFS_COMPILE_CACHE_SLOT_NAMESPACE: &str = "studio-vfs-compile-cache-slot";
const STUDIO_VFS_COMPILE_CACHE_NAMESPACE: &str = "studio-vfs-compile-cache";

#[derive(Clone)]
struct ResolvedVfsCompileTarget {
    entry_path: String,
    project_root: Option<String>,
}

struct PreparedVfsCompile {
    target: ResolvedVfsCompileTarget,
    local_fs: MemoryFs,
    authority: VfsCompileAuthority,
    project_context: Option<vo_module::project::ProjectContext>,
}

struct ReadyVfsCompile {
    prepared: PreparedVfsCompile,
    ready_modules: Vec<vo_module::readiness::ReadyModule>,
}

impl PreparedVfsCompile {
    fn locked_modules(&self) -> &[vo_module::schema::lockfile::LockedModule] {
        self.project_context
            .as_ref()
            .map(|context| context.project_plan().locked_modules())
            .unwrap_or_default()
    }
}

impl ReadyVfsCompile {
    fn locked_modules(&self) -> &[vo_module::schema::lockfile::LockedModule] {
        self.prepared.locked_modules()
    }

    fn browser_runtime_plan(&self) -> Result<vo_web::BrowserRuntimePlan, String> {
        match self.prepared.project_context.as_ref() {
            Some(context) => {
                browser_runtime_plan_for_context_with_ready(context, &self.ready_modules)
            }
            None => Ok(vo_web::BrowserRuntimePlan::default()),
        }
    }
}

struct VfsCompileCacheSlot {
    metadata_path: String,
    module_path: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VfsCompileAuthority {
    Project,
    EphemeralSingleFile,
    AdHocSingleFile,
}

struct SingleFileEntry {
    entry_clean: String,
    content: String,
    external_modules: Vec<String>,
    inline_mod: Option<vo_module::inline_mod::InlineMod>,
}

#[derive(Clone)]
struct FrameworkContract {
    module_key: String,
    name: String,
    entry: Option<String>,
    provider_role: Option<String>,
    provider_roles: Vec<String>,
    capabilities: Vec<String>,
    roles: Vec<String>,
    js_modules: BTreeMap<String, String>,
}

fn normalize_vfs_path(path: &str) -> String {
    let trimmed = path.trim();
    if trimmed.is_empty() || trimmed == "/" {
        "/".to_string()
    } else {
        let normalized = trimmed.trim_end_matches('/');
        if normalized.is_empty() {
            "/".to_string()
        } else {
            normalized.to_string()
        }
    }
}

fn vfs_parent_dir(path: &str) -> Option<String> {
    std::path::Path::new(path).parent().map(|p| {
        let value = p.to_string_lossy().to_string();
        if value.is_empty() {
            "/".to_string()
        } else {
            value
        }
    })
}

fn join_vfs_path(base: &str, child: &str) -> String {
    let normalized_child = normalize_vfs_path(child);
    if child.trim().starts_with('/') {
        return normalized_child;
    }
    let normalized_base = normalize_vfs_path(base);
    if normalized_child == "/" {
        return normalized_base;
    }
    let child = normalized_child.trim_start_matches('/');
    if normalized_base == "/" {
        format!("/{}", child)
    } else {
        format!("{}/{}", normalized_base, child)
    }
}

fn normalize_vfs_dot_segments(path: &str) -> String {
    let mut parts = Vec::new();
    for part in path.split('/') {
        match part {
            "" | "." => {}
            ".." => {
                parts.pop();
            }
            value => parts.push(value),
        }
    }
    if parts.is_empty() {
        "/".to_string()
    } else {
        format!("/{}", parts.join("/"))
    }
}

fn single_file_project_dir(entry_clean: &str) -> PathBuf {
    let parent = Path::new(entry_clean)
        .parent()
        .unwrap_or_else(|| Path::new("."));
    if parent.as_os_str().is_empty() {
        PathBuf::from(".")
    } else {
        parent.to_path_buf()
    }
}

fn parse_single_file_inline_mod(
    entry_clean: &str,
    content: &str,
) -> Result<Option<vo_module::inline_mod::InlineMod>, String> {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(PathBuf::from(entry_clean), content.to_string());
    match vo_module::project::load_single_file_context(&local_fs, Path::new(entry_clean)) {
        Ok(vo_module::project::SingleFileContext::EphemeralInlineMod { inline_mod, .. }) => {
            Ok(Some(inline_mod))
        }
        Ok(vo_module::project::SingleFileContext::AdHoc { .. }) => Ok(None),
        Ok(vo_module::project::SingleFileContext::Project(_)) => Err(format!(
            "single-file target {} unexpectedly resolved as a project",
            entry_clean
        )),
        Err(error) => Err(error.to_string()),
    }
}

fn is_persistent_vfs_project_root(dir: &str) -> bool {
    let vo_mod_path = join_vfs_path(dir, "vo.mod");
    vfs_exists(&vo_mod_path)
}

fn is_vfs_dir(path: &str) -> bool {
    let normalized = normalize_vfs_path(path);
    let (_, _, _, _, is_dir, error) = vo_web_runtime_wasm::vfs::stat(&normalized);
    error.is_none() && is_dir
}

fn find_vfs_project_root(entry_path: &str) -> Option<String> {
    let normalized = normalize_vfs_path(entry_path);
    let mut current = if is_vfs_dir(&normalized) {
        normalized
    } else {
        vfs_parent_dir(&normalized).unwrap_or_else(|| "/".to_string())
    };

    loop {
        if is_persistent_vfs_project_root(&current) {
            return Some(current);
        }

        let parent = vfs_parent_dir(&current)?;
        if parent == current {
            return None;
        }
        current = parent;
    }
}

fn resolve_vfs_compile_target(entry_path: &str) -> Result<ResolvedVfsCompileTarget, String> {
    let normalized = normalize_vfs_dot_segments(&normalize_vfs_path(entry_path));
    let resolved_entry_path = if is_vfs_dir(&normalized) {
        let main_path = join_vfs_path(&normalized, "main.vo");
        if !vfs_exists(&main_path) {
            return Err(format!("missing Studio entry file '{}'", main_path));
        }
        main_path
    } else {
        normalized
    };

    Ok(ResolvedVfsCompileTarget {
        project_root: find_vfs_project_root(&resolved_entry_path),
        entry_path: resolved_entry_path,
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VfsPackageCopyPolicy {
    Project { include_workfile: bool },
    WorkspaceMember,
}

impl VfsPackageCopyPolicy {
    fn should_keep_file(self, name: &str) -> bool {
        if name.ends_with(".vo") || name == "vo.mod" {
            return true;
        }
        match self {
            Self::Project { include_workfile } => {
                matches!(name, "vo.lock" | "vo.release.json" | "vo.tree.json")
                    || (include_workfile && name == "vo.work")
            }
            Self::WorkspaceMember => false,
        }
    }
}

fn read_vfs_package(
    project_root: &str,
    local_fs: &mut MemoryFs,
    policy: VfsPackageCopyPolicy,
    excluded_roots: &BTreeSet<String>,
    budget: &mut VfsPackageReadBudget,
) -> Result<(), String> {
    #[derive(Clone, Copy)]
    enum KeptFileKind {
        Source,
        Metadata { limit: usize },
    }

    fn kept_file_kind(name: &str) -> KeptFileKind {
        if name.ends_with(".vo") {
            KeptFileKind::Source
        } else if name == "vo.lock" {
            KeptFileKind::Metadata {
                limit: vo_module::MAX_LOCK_FILE_BYTES,
            }
        } else {
            KeptFileKind::Metadata {
                limit: vo_common::vfs::MAX_TEXT_FILE_BYTES,
            }
        }
    }

    let root = normalize_vfs_dot_segments(&normalize_vfs_path(project_root));
    if !is_vfs_dir(&root) {
        return Ok(());
    }
    let mut pending = vec![root];
    while let Some(dir) = pending.pop() {
        if !budget.visited_directories.insert(dir.clone()) {
            continue;
        }
        let (mut entries, err) = vo_web_runtime_wasm::vfs::read_dir(&dir);
        if let Some(error) = err {
            return Err(format!("read dir '{}': {}", dir, error));
        }
        entries.sort_by(|left, right| left.0.cmp(&right.0));
        for (name, is_dir, _mode) in entries {
            budget.charge_directory_entry(&dir)?;
            let full = join_vfs_path(&dir, &name);
            if is_dir {
                if excluded_roots.contains(&full) {
                    continue;
                }
                pending.push(full);
                continue;
            }
            if !policy.should_keep_file(&name) {
                continue;
            }
            if budget.kept_files.contains(&full) {
                continue;
            }
            let kind = kept_file_kind(&name);
            let limit = match kind {
                KeptFileKind::Source => vo_common::vfs::MAX_TEXT_FILE_BYTES,
                KeptFileKind::Metadata { limit } => limit,
            };
            let data = read_vfs_bytes_limited(&full, limit, "Studio package file")?;
            budget.charge_kept_file(&full, data.len(), matches!(kind, KeptFileKind::Source))?;
            let content =
                String::from_utf8(data).map_err(|error| format!("utf8 '{}': {}", full, error))?;
            local_fs.add_file(PathBuf::from(full.trim_start_matches('/')), content);
        }
    }
    Ok(())
}

fn copy_vfs_metadata_file(
    path: &Path,
    local_fs: &mut MemoryFs,
    budget: &mut VfsPackageReadBudget,
    label: &str,
) -> Result<(), String> {
    let vfs_path = vfs_path_from_fs_path(path);
    if budget.kept_files.contains(&vfs_path) {
        return Ok(());
    }
    let content = read_vfs_text_limited(&vfs_path, vo_common::vfs::MAX_TEXT_FILE_BYTES, label)?;
    budget.charge_kept_file(&vfs_path, content.len(), false)?;
    local_fs.add_file(PathBuf::from(vfs_path.trim_start_matches('/')), content);
    Ok(())
}

#[derive(Default)]
struct VfsPackageReadBudget {
    visited_directories: BTreeSet<String>,
    kept_files: BTreeSet<String>,
    directory_entries: usize,
    source_files: usize,
    source_bytes: usize,
    snapshot_bytes: usize,
}

impl VfsPackageReadBudget {
    fn charge_directory_entry(&mut self, dir: &str) -> Result<(), String> {
        let directory_entries = self
            .directory_entries
            .checked_add(1)
            .ok_or_else(|| "Studio package directory-entry count overflow".to_string())?;
        if directory_entries > vo_common::vfs::MAX_DIRECTORY_ENTRIES {
            return Err(format!(
                "Studio package tree rooted near '{}' contains more than {} entries",
                dir,
                vo_common::vfs::MAX_DIRECTORY_ENTRIES
            ));
        }
        self.directory_entries = directory_entries;
        Ok(())
    }

    fn charge_kept_file(&mut self, path: &str, bytes: usize, source: bool) -> Result<(), String> {
        let snapshot_bytes = self
            .snapshot_bytes
            .checked_add(bytes)
            .ok_or_else(|| "Studio package snapshot byte count overflow".to_string())?;
        if snapshot_bytes > STUDIO_PACKAGE_SNAPSHOT_MAX_BYTES {
            return Err(format!(
                "Studio package snapshot exceeds the {}-byte limit while reading '{}'",
                STUDIO_PACKAGE_SNAPSHOT_MAX_BYTES, path
            ));
        }
        if !source {
            self.snapshot_bytes = snapshot_bytes;
            self.kept_files.insert(path.to_string());
            return Ok(());
        }
        let source_files = self
            .source_files
            .checked_add(1)
            .ok_or_else(|| "Studio source-file count overflow".to_string())?;
        if source_files > vo_common::vfs::MAX_PACKAGE_SOURCE_FILES {
            return Err(format!(
                "Studio package snapshot contains more than {} source files",
                vo_common::vfs::MAX_PACKAGE_SOURCE_FILES
            ));
        }
        let source_bytes = self
            .source_bytes
            .checked_add(bytes)
            .ok_or_else(|| "Studio source byte count overflow".to_string())?;
        if source_bytes > vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES {
            return Err(format!(
                "Studio package snapshot exceeds the {}-byte source limit while reading '{}'",
                vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES,
                path
            ));
        }
        self.snapshot_bytes = snapshot_bytes;
        self.source_files = source_files;
        self.source_bytes = source_bytes;
        self.kept_files.insert(path.to_string());
        Ok(())
    }
}

fn vfs_path_from_fs_path(path: &Path) -> String {
    normalize_vfs_path(&format!("/{}", path.to_string_lossy()))
}

fn validate_project_context_authority(
    expected: &vo_module::project::ProjectContext,
    context: &vo_module::project::ProjectContext,
) -> Result<(), String> {
    if !expected.has_same_root_authority(context) {
        return Err(
            "Studio ProjectContext changed the exact authoritative root manifest, lock graph, or graph authority"
                .to_string(),
        );
    }
    Ok(())
}

fn select_authorized_workspace_source_roots(
    discovered_roots: &BTreeSet<String>,
    authorized_local_dirs: impl IntoIterator<Item = PathBuf>,
) -> Result<BTreeSet<String>, String> {
    let authorized_roots = authorized_local_dirs
        .into_iter()
        .map(|local_dir| vfs_path_from_fs_path(&local_dir))
        .collect::<BTreeSet<_>>();
    if let Some(unexpected) = authorized_roots
        .iter()
        .find(|root| !discovered_roots.contains(*root))
    {
        return Err(format!(
            "Studio ProjectContext authorized undiscovered workspace source {unexpected}"
        ));
    }
    Ok(authorized_roots)
}

fn read_workspace_vfs_packages(
    project_root: &str,
    local_fs: &mut MemoryFs,
    options: &ProjectContextOptions,
) -> Result<vo_module::project::ProjectContext, String> {
    let include_workfile = workspace_discovery_reads_workfile(options);
    let mut budget = VfsPackageReadBudget::default();
    let project_dir = Path::new(project_root.trim_start_matches('/'));
    let vfs = vo_web::WasmVfs::new("");
    let (workspace_file, discovered_candidates) = if include_workfile {
        vo_module::workspace::discover_workspace_candidates_in_with_provenance(
            &vfs,
            project_dir,
            None,
            &options.workspace,
        )
        .map_err(|error| error.to_string())?
    } else {
        (None, Vec::new())
    };
    let discovered_roots = discovered_candidates
        .iter()
        .map(|entry| vfs_path_from_fs_path(&entry.local_dir))
        .collect::<BTreeSet<_>>();

    // Workspace discovery is metadata only at this point. Keep every member's
    // identity available to ProjectContext while preventing an untrusted
    // workspace entry from smuggling source into the compile snapshot.
    read_vfs_package(
        project_root,
        local_fs,
        VfsPackageCopyPolicy::Project { include_workfile },
        &discovered_roots,
        &mut budget,
    )?;
    if let Some(workspace_file) = workspace_file.as_deref() {
        copy_vfs_metadata_file(
            workspace_file,
            local_fs,
            &mut budget,
            "Studio workspace file",
        )?;
    }
    for candidate in &discovered_candidates {
        copy_vfs_metadata_file(
            &candidate.local_dir.join("vo.mod"),
            local_fs,
            &mut budget,
            "Studio workspace member manifest",
        )?;
    }

    // ProjectContext is the single authority gate for the format-1 selection
    // lock. Dependency-free roots omit the lock; workspace sources must have
    // matching workspace-origin records. Capture its exact metadata generation
    // and selected graph authority before copying any sources.
    let discovery_context =
        vo_module::project::load_project_context_with_options(local_fs, project_dir, options)
            .map_err(|error| error.to_string())?;
    let authorized_sources = discovery_context.workspace_sources().clone();
    let authorized_roots = select_authorized_workspace_source_roots(
        &discovered_roots,
        authorized_sources.values().cloned(),
    )?;
    for authorized_root in authorized_roots {
        let mut excluded_member_roots = discovered_roots.clone();
        excluded_member_roots.remove(&authorized_root);
        read_vfs_package(
            &authorized_root,
            local_fs,
            VfsPackageCopyPolicy::WorkspaceMember,
            &excluded_member_roots,
            &mut budget,
        )?;
    }

    // Rebuild after copying the authorized source closure. This second pass
    // validates imports from the bytes that compilation will actually see.
    let context =
        vo_module::project::load_project_context_with_options(local_fs, project_dir, options)
            .map_err(|error| error.to_string())?;
    validate_project_context_authority(&discovery_context, &context)?;
    if context.workspace_sources() != &authorized_sources {
        return Err(
            "Studio workspace authorization changed while constructing the compile snapshot"
                .to_string(),
        );
    }
    Ok(context)
}

fn build_workspace_project_from_vfs(
    project_root: &str,
    options: &ProjectContextOptions,
) -> Result<(MemoryFs, vo_module::project::ProjectContext), String> {
    let mut local_fs = MemoryFs::new();
    let context = read_workspace_vfs_packages(project_root, &mut local_fs, options)?;
    Ok((local_fs, context))
}

fn browser_runtime_plan_for_context_with_ready(
    context: &vo_module::project::ProjectContext,
    ready: &[vo_module::readiness::ReadyModule],
) -> Result<vo_web::BrowserRuntimePlan, String> {
    merge_browser_runtime_plan_for_context(context, vo_web::plan_ready_browser_runtime(ready)?)
}

fn merge_browser_runtime_plan_for_context(
    context: &vo_module::project::ProjectContext,
    published: vo_web::BrowserRuntimePlan,
) -> Result<vo_web::BrowserRuntimePlan, String> {
    let mut plans = Vec::new();
    for local_dir in context.workspace_sources().values() {
        let local_root = vfs_path_from_fs_path(local_dir);
        plans.push(vo_web::debug_local_project_browser_runtime_plan_from_vfs(
            &local_root,
        )?);
    }
    plans.push(published);
    vo_web::merge_browser_runtime_plans(plans)
}

fn read_vfs_text_limited(path: &str, max_bytes: usize, label: &str) -> Result<String, String> {
    let data = read_vfs_bytes_limited(path, max_bytes, label)?;
    String::from_utf8(data).map_err(|error| format!("utf8 decode '{}': {}", path, error))
}

fn read_vfs_bytes_limited(path: &str, max_bytes: usize, label: &str) -> Result<Vec<u8>, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file_limited(path, max_bytes);
    if let Some(error) = err {
        return Err(format!(
            "read {label} '{}' with a {max_bytes}-byte limit: {error}",
            path
        ));
    }
    if data.len() > max_bytes {
        return Err(format!(
            "read {label} '{}' returned {} bytes, exceeding the {max_bytes}-byte limit",
            path,
            data.len()
        ));
    }
    Ok(data)
}

const WASM_INSTALL_TARGET: &str = "wasm32-unknown-unknown";

async fn ensure_project_plan_for_studio(
    project_plan: &vo_module::project::ProjectPlan,
) -> Result<Vec<vo_module::readiness::ReadyModule>, String> {
    let registry = vo_web::BrowserRegistry;
    let surface = vo_web::WasmVfs::new("");
    vo_module::async_install::ensure_project_plan(
        &surface,
        &registry,
        project_plan,
        WASM_INSTALL_TARGET,
    )
    .await
    .map_err(|error| error.to_string())
}

fn log_prepare_entry_resolve_install_done<'a>(modules: impl IntoIterator<Item = &'a str>) {
    for module in modules {
        log_wasm_module(
            "prepare_entry_resolve_install_done",
            module,
            js_sys::Date::now(),
        );
    }
}

async fn prepare_ready_vfs_compile(
    entry_path: &str,
    options: &ProjectContextOptions,
) -> Result<ReadyVfsCompile, String> {
    let total_start = js_sys::Date::now();
    let mut prepared = prepare_vfs_compile(entry_path, options)?;
    let ready_modules = match prepared.project_context.as_ref() {
        Some(context) if context.project_plan().has_mod_file() => {
            let deps_start = js_sys::Date::now();
            let ready = ensure_project_plan_for_studio(context.project_plan()).await?;
            log_prepare_entry_resolve_install_done(
                context
                    .project_plan()
                    .locked_modules()
                    .iter()
                    .map(|module| module.path.as_str()),
            );
            log_wasm_path(
                "prepare_entry_ensure_deps_done",
                &prepared.target.entry_path,
                "system",
                Some(deps_start),
            );
            let refreshed = prepare_vfs_compile(entry_path, options)?;
            if refreshed.target.entry_path != prepared.target.entry_path
                || refreshed.target.project_root != prepared.target.project_root
            {
                return Err(String::from(
                    "Studio compile target changed while dependencies were being prepared",
                ));
            }
            let refreshed_context = refreshed.project_context.as_ref().ok_or_else(|| {
                String::from("Studio project disappeared while dependencies were being prepared")
            })?;
            validate_project_context_authority(context, refreshed_context)?;
            if context.workspace_sources() != refreshed_context.workspace_sources() {
                return Err(String::from(
                    "Studio workspace selection changed while dependencies were being prepared",
                ));
            }
            prepared = refreshed;
            ready
        }
        _ => Vec::new(),
    };
    log_wasm_path(
        "prepare_entry_done",
        &prepared.target.entry_path,
        "system",
        Some(total_start),
    );
    Ok(ReadyVfsCompile {
        prepared,
        ready_modules,
    })
}

async fn prepare_ready_vfs_compile_from_discovery(
    entry_path: &str,
    workspace_discovery: &str,
) -> Result<(ReadyVfsCompile, ProjectContextOptions), String> {
    let options = project_context_options_from_workspace_discovery(workspace_discovery)?;
    let ready = prepare_ready_vfs_compile(entry_path, &options).await?;
    Ok((ready, options))
}

impl SingleFileEntry {
    fn load(target: &ResolvedVfsCompileTarget) -> Result<Self, String> {
        let entry_clean = target.entry_path.trim_start_matches('/').to_string();
        let content = read_vfs_text_limited(
            &target.entry_path,
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "Studio source file",
        )?;
        let external_modules = vo_web::extract_external_module_paths(&content);
        if external_modules.len() > vo_module::MAX_MODULE_DEPENDENCIES {
            return Err(format!(
                "single-file entry {} imports more than {} external modules",
                target.entry_path,
                vo_module::MAX_MODULE_DEPENDENCIES
            ));
        }
        let inline_mod = parse_single_file_inline_mod(&entry_clean, &content)?;
        Ok(Self {
            entry_clean,
            content,
            external_modules,
            inline_mod,
        })
    }

    fn validate_dependency_authority(&self) -> Result<(), String> {
        if let Some(module) = self.external_modules.first() {
            return Err(format!(
                "single-file entry /{} imports third-party module {module}; single files support only the standard library, so create a project with vo.mod and commit its generated vo.lock",
                self.entry_clean,
            ));
        }
        Ok(())
    }

    fn populate_compile_fs(&self, local_fs: &mut MemoryFs) -> Result<(), String> {
        self.validate_dependency_authority()?;
        local_fs.add_file(PathBuf::from(&self.entry_clean), self.content.clone());
        let Some(inline_mod) = self.inline_mod.as_ref() else {
            return Ok(());
        };
        let mod_file = vo_module::inline_mod::synthesize_mod_file(inline_mod);
        let project_dir = single_file_project_dir(&self.entry_clean);
        let mod_path = if project_dir == Path::new(".") {
            PathBuf::from("vo.mod")
        } else {
            project_dir.join("vo.mod")
        };
        let mod_content = mod_file.render().map_err(|error| error.to_string())?;
        local_fs.add_file(mod_path, mod_content);
        Ok(())
    }
}

fn prepare_vfs_compile(
    entry_path: &str,
    options: &ProjectContextOptions,
) -> Result<PreparedVfsCompile, String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let (local_fs, authority, project_context) = if let Some(project_root) = &target.project_root {
        let (local_fs, context) = build_workspace_project_from_vfs(project_root, options)?;
        (local_fs, VfsCompileAuthority::Project, Some(context))
    } else {
        let single_file = SingleFileEntry::load(&target)?;
        let authority = if single_file.inline_mod.is_some() {
            VfsCompileAuthority::EphemeralSingleFile
        } else {
            VfsCompileAuthority::AdHocSingleFile
        };
        single_file.validate_dependency_authority()?;
        let mut local_fs = MemoryFs::new();
        single_file.populate_compile_fs(&mut local_fs)?;
        (local_fs, authority, None)
    };

    Ok(PreparedVfsCompile {
        target,
        local_fs,
        authority,
        project_context,
    })
}

fn validate_materialized_modules_with_fs<F: FileSystem>(
    module_fs: &F,
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<Vec<vo_module::readiness::ReadyModule>, String> {
    vo_module::readiness::check_materialized_modules_readiness(
        module_fs,
        locked_modules,
        WASM_INSTALL_TARGET,
    )
    .map_err(|error| format!("Studio module cache does not match the authorized graph: {error}"))
}

fn validate_vfs_materialized_modules(
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<Vec<vo_module::readiness::ReadyModule>, String> {
    validate_materialized_modules_with_fs(&vo_web::WasmVfs::new(""), locked_modules)
}

fn ensure_vfs_parent_dir(path: &str) -> Result<(), String> {
    if let Some(parent) = Path::new(path).parent() {
        let parent = normalize_vfs_path(&parent.to_string_lossy());
        if parent != "/" && !parent.is_empty() {
            if let Some(error) = vo_web_runtime_wasm::vfs::mkdir_all(&parent, 0o755) {
                return Err(format!("mkdir {}: {}", parent, error));
            }
        }
    }
    Ok(())
}

fn write_vfs_bytes(path: &str, bytes: &[u8]) -> Result<(), String> {
    ensure_vfs_parent_dir(path)?;
    if let Some(error) = vo_web_runtime_wasm::vfs::write_file(path, bytes, 0o644) {
        return Err(format!("write {}: {}", path, error));
    }
    Ok(())
}

fn write_vfs_text(path: &str, content: &str) -> Result<(), String> {
    write_vfs_bytes(path, content.as_bytes())
}

fn vfs_compile_cache_slot(target: &ResolvedVfsCompileTarget) -> VfsCompileCacheSlot {
    let mut slot_hasher = StableHasher::new(STUDIO_VFS_COMPILE_CACHE_SLOT_NAMESPACE);
    slot_hasher.update_str("entry_path", &target.entry_path);
    slot_hasher.update_str("project_root", target.project_root.as_deref().unwrap_or(""));
    let slot_id = slot_hasher.finish_suffix();
    let cache_dir = join_vfs_path(
        &join_vfs_path(STUDIO_HOST_PRIVATE_VFS_ROOT, "compile-cache"),
        "studio-wasm",
    );
    let slot_dir = join_vfs_path(&cache_dir, &slot_id);
    VfsCompileCacheSlot {
        metadata_path: join_vfs_path(&slot_dir, "metadata"),
        module_path: join_vfs_path(&slot_dir, "module.voc"),
    }
}

fn collect_memory_fs_files(
    fs: &MemoryFs,
    dir: &Path,
    out: &mut Vec<PathBuf>,
) -> Result<(), String> {
    let entries = fs
        .read_dir(dir)
        .map_err(|error| format!("read local fs dir {:?}: {}", dir, error))?;
    for entry in entries {
        if fs.is_dir(&entry) {
            collect_memory_fs_files(fs, &entry, out)?;
            continue;
        }
        out.push(entry);
    }
    Ok(())
}

fn compute_vfs_compile_cache_fingerprint(
    target: &ResolvedVfsCompileTarget,
    local_fs: &MemoryFs,
) -> Result<String, String> {
    let mut hasher = StableHasher::new(STUDIO_VFS_COMPILE_CACHE_NAMESPACE);
    hasher.update_str("schema", STUDIO_VFS_COMPILE_CACHE_SCHEMA_VERSION);
    hasher.update_str("compiler_version", vo_module::TOOLCHAIN_VERSION);
    hasher.update_str("compiler_build_id", STUDIO_WASM_BUILD_ID);
    hasher.update_str("entry_path", &target.entry_path);
    hasher.update_str("project_root", target.project_root.as_deref().unwrap_or(""));
    let mut files = Vec::new();
    collect_memory_fs_files(local_fs, Path::new("."), &mut files)?;
    files.sort();
    for file in files {
        let content = local_fs
            .read_file(&file)
            .map_err(|e| format!("read local fs file {:?}: {}", file, e))?;
        hasher.update_path("file_path", &file);
        hasher.update_bytes("file_bytes", content.as_bytes());
    }
    Ok(hasher.finish())
}

fn try_load_vfs_compile_cache(
    slot: &VfsCompileCacheSlot,
    fingerprint: &str,
) -> Result<Option<Vec<u8>>, String> {
    if !vfs_exists(&slot.metadata_path) || !vfs_exists(&slot.module_path) {
        discard_vfs_compile_cache(slot);
        return Ok(None);
    }
    let metadata = match read_vfs_text_limited(
        &slot.metadata_path,
        STUDIO_CACHE_METADATA_MAX_BYTES,
        "Studio compile-cache metadata",
    ) {
        Ok(value) => value,
        Err(_) => {
            discard_vfs_compile_cache(slot);
            return Ok(None);
        }
    };
    let expected_digest = match parse_vfs_compile_cache_metadata(&metadata, fingerprint) {
        Ok(digest) => digest,
        Err(_) => {
            discard_vfs_compile_cache(slot);
            return Ok(None);
        }
    };
    let bytecode = match read_vfs_bytes_limited(
        &slot.module_path,
        vo_common_core::serialize::MAX_VOB_BYTES,
        "Studio compile-cache bytecode",
    ) {
        Ok(value) => value,
        Err(_) => {
            discard_vfs_compile_cache(slot);
            return Ok(None);
        }
    };
    if validate_vfs_compile_cache_module_binding(&expected_digest, &bytecode).is_err() {
        discard_vfs_compile_cache(slot);
        return Ok(None);
    }
    if decode_verified_module(&bytecode, "Studio compile cache").is_err() {
        discard_vfs_compile_cache(slot);
        return Ok(None);
    }
    Ok(Some(bytecode))
}

fn discard_vfs_compile_cache(slot: &VfsCompileCacheSlot) {
    let _ = vo_web_runtime_wasm::vfs::remove(&slot.metadata_path);
    let _ = vo_web_runtime_wasm::vfs::remove(&slot.module_path);
}

fn encode_vfs_compile_cache_metadata(fingerprint: &str, bytecode: &[u8]) -> String {
    let module_digest = vo_module::digest::Digest::from_sha256(bytecode);
    format!("fingerprint={fingerprint}\nmodule_digest={module_digest}\n")
}

fn parse_vfs_compile_cache_metadata(
    metadata: &str,
    expected_fingerprint: &str,
) -> Result<vo_module::digest::Digest, String> {
    let mut lines = metadata.lines();
    let fingerprint = lines
        .next()
        .and_then(|line| line.strip_prefix("fingerprint="))
        .ok_or_else(|| "Studio compile-cache metadata is missing fingerprint".to_string())?;
    if fingerprint != expected_fingerprint {
        return Err("Studio compile-cache fingerprint does not match source snapshot".to_string());
    }
    let module_digest = lines
        .next()
        .and_then(|line| line.strip_prefix("module_digest="))
        .ok_or_else(|| "Studio compile-cache metadata is missing module digest".to_string())?;
    if lines.next().is_some() {
        return Err("Studio compile-cache metadata has unexpected fields".to_string());
    }
    vo_module::digest::Digest::parse(module_digest)
        .map_err(|error| format!("Studio compile-cache module digest is invalid: {error}"))
}

fn validate_vfs_compile_cache_module_binding(
    expected_digest: &vo_module::digest::Digest,
    bytecode: &[u8],
) -> Result<(), String> {
    let actual_digest = vo_module::digest::Digest::from_sha256(bytecode);
    if &actual_digest != expected_digest {
        return Err(format!(
            "Studio compile-cache bytecode digest mismatch: expected {expected_digest}, found {actual_digest}",
        ));
    }
    Ok(())
}

fn save_vfs_compile_cache(
    slot: &VfsCompileCacheSlot,
    fingerprint: &str,
    bytecode: &[u8],
) -> Result<(), String> {
    decode_verified_module(bytecode, "Studio compile cache")?;
    write_vfs_bytes(&slot.module_path, bytecode)?;
    write_vfs_text(
        &slot.metadata_path,
        &encode_vfs_compile_cache_metadata(fingerprint, bytecode),
    )
}

fn validate_studio_bytecode_size(len: usize, label: &str) -> Result<(), String> {
    vo_common_core::serialize::validate_vob_input_size(len)
        .map_err(|e| format!("failed to decode {label} bytecode: {e}"))?;
    Ok(())
}

fn decode_verified_module(bytecode: &[u8], label: &str) -> Result<vo_vm::bytecode::Module, String> {
    validate_studio_bytecode_size(bytecode.len(), label)?;
    let module = vo_vm::bytecode::Module::deserialize(bytecode)
        .map_err(|e| format!("failed to decode {label} bytecode: {e:?}"))?;
    vo_common_core::verifier::verify_module(&module)
        .map_err(|err| format!("invalid {label} bytecode: {err}"))?;
    Ok(module)
}

fn vfs_exists(path: &str) -> bool {
    let (_, _, _, _, _, err) = vo_web_runtime_wasm::vfs::stat(path);
    err.is_none()
}

fn framework_contract_from_vo_web(contract: vo_web::BrowserRuntimeContract) -> FrameworkContract {
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

fn materialize_render_island_snapshot(
    target: &ResolvedVfsCompileTarget,
    plan: &vo_web::BrowserRuntimePlan,
) -> Result<(String, Vec<(String, Vec<u8>)>), String> {
    let root_path = target
        .project_root
        .clone()
        .unwrap_or_else(|| vfs_parent_dir(&target.entry_path).unwrap_or_else(|| "/".to_string()));
    let snapshot = if target.project_root.is_some() {
        plan.snapshot_plan(vo_web::BrowserSnapshotRoot::ProjectRoot)
    } else {
        plan.snapshot_plan(vo_web::BrowserSnapshotRoot::EntryFile)
    }?;
    let files = vo_web::materialize_browser_snapshot_from_vfs(
        &snapshot,
        &plan,
        target.project_root.as_deref(),
        &target.entry_path,
    )
    .map_err(|error| error.to_string())?
    .into_iter()
    .map(|file| (file.path, file.bytes))
    .collect();
    Ok((root_path, files))
}

fn render_island_snapshot_to_js(root_path: &str, files: Vec<(String, Vec<u8>)>) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("rootPath"),
        &JsValue::from_str(root_path),
    );
    let js_files = js_sys::Array::new();
    for (path, bytes) in files {
        let file = Object::new();
        let _ = Reflect::set(&file, &JsValue::from_str("path"), &JsValue::from_str(&path));
        let bytes = js_sys::Uint8Array::from(bytes.as_slice());
        let _ = Reflect::set(&file, &JsValue::from_str("bytes"), &bytes);
        js_files.push(&file);
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("files"), &js_files);
    obj.into()
}

fn compile_prepared_vfs(
    prepared: PreparedVfsCompile,
    options: &ProjectContextOptions,
) -> Result<Vec<u8>, String> {
    let ready_modules = validate_vfs_materialized_modules(prepared.locked_modules())?;
    compile_authenticated_vfs(prepared, options, &ready_modules)
}

fn compile_ready_vfs(
    ready: ReadyVfsCompile,
    options: &ProjectContextOptions,
) -> Result<Vec<u8>, String> {
    let ReadyVfsCompile {
        prepared,
        ready_modules,
    } = ready;
    compile_authenticated_vfs(prepared, options, &ready_modules)
}

fn compile_authenticated_vfs(
    prepared: PreparedVfsCompile,
    options: &ProjectContextOptions,
    ready_modules: &[vo_module::readiness::ReadyModule],
) -> Result<Vec<u8>, String> {
    let PreparedVfsCompile {
        target,
        local_fs,
        authority,
        project_context: _,
    } = prepared;
    let cache_slot = vfs_compile_cache_slot(&target);
    let fingerprint = compute_vfs_compile_cache_fingerprint(&target, &local_fs)?;
    if let Some(bytecode) = try_load_vfs_compile_cache(&cache_slot, &fingerprint)? {
        log_wasm_path("compile_cache_hit", &target.entry_path, "success", None);
        return Ok(bytecode);
    }
    let entry_clean = target.entry_path.trim_start_matches('/');
    let bytecode = match authority {
        VfsCompileAuthority::EphemeralSingleFile => {
            vo_web::compile_ephemeral_entry_with_vfs(entry_clean, local_fs, VFS_MOD_ROOT)
        }
        VfsCompileAuthority::Project | VfsCompileAuthority::AdHocSingleFile => {
            vo_web::compile_ready_entry_with_vfs(
                entry_clean,
                local_fs,
                VFS_MOD_ROOT,
                options,
                ready_modules,
            )
        }
    }
    .map_err(|e| format!("compile error: {}", e))?;
    save_vfs_compile_cache(&cache_slot, &fingerprint, &bytecode)?;
    log_wasm_path("compile_cache_store", &target.entry_path, "system", None);
    Ok(bytecode)
}

fn compile_from_vfs(entry_path: &str, options: &ProjectContextOptions) -> Result<Vec<u8>, String> {
    compile_prepared_vfs(prepare_vfs_compile(entry_path, options)?, options)
}

struct GuiCompileOutput {
    target: ResolvedVfsCompileTarget,
    bytecode: Vec<u8>,
    framework: Option<FrameworkContract>,
    provider_frameworks: Vec<FrameworkContract>,
    runtime_plan: vo_web::BrowserRuntimePlan,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
}

fn compile_gui_run_output(
    ready: ReadyVfsCompile,
    options: &ProjectContextOptions,
) -> Result<GuiCompileOutput, String> {
    let target = ready.prepared.target.clone();
    let plan = ready.browser_runtime_plan()?;
    let locked_modules = ready.locked_modules().to_vec();
    let bytecode = compile_ready_vfs(ready, options)?;
    let split = plan.primary_framework_split();
    let framework = split.primary_framework.map(framework_contract_from_vo_web);
    let provider_frameworks = split
        .provider_frameworks
        .into_iter()
        .map(framework_contract_from_vo_web)
        .collect();
    Ok(GuiCompileOutput {
        target,
        bytecode,
        framework,
        provider_frameworks,
        runtime_plan: plan,
        locked_modules,
    })
}

fn framework_contract_to_js(contract: &FrameworkContract) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("moduleKey"),
        &JsValue::from_str(&contract.module_key),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("name"),
        &JsValue::from_str(&contract.name),
    );
    let entry = contract
        .entry
        .as_ref()
        .map(|value| JsValue::from_str(value))
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("entry"), &entry);
    let provider_role = contract
        .provider_role
        .as_ref()
        .map(|value| JsValue::from_str(value))
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("providerRole"), &provider_role);
    let provider_roles = js_sys::Array::new();
    for provider_role in &contract.provider_roles {
        provider_roles.push(&JsValue::from_str(provider_role));
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("providerRoles"), &provider_roles);
    let capabilities = js_sys::Array::new();
    for capability in &contract.capabilities {
        capabilities.push(&JsValue::from_str(capability));
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("capabilities"), &capabilities);
    let roles = js_sys::Array::new();
    for role in &contract.roles {
        roles.push(&JsValue::from_str(role));
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("roles"), &roles);
    let js_modules = Object::new();
    for (name, path) in &contract.js_modules {
        let _ = Reflect::set(
            &js_modules,
            &JsValue::from_str(name),
            &JsValue::from_str(path),
        );
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("jsModules"), &js_modules);
    obj.into()
}

#[wasm_bindgen(js_name = "compileRunEntry")]
pub fn compile_run_entry(
    entry_path: &str,
    workspace_discovery: &str,
    operation_id: &str,
) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    let workspace_discovery = workspace_discovery.to_string();
    cancellable_studio_promise(operation_id, async move {
        let (ready, options) =
            prepare_ready_vfs_compile_from_discovery(&entry_path, &workspace_discovery)
                .await
                .map_err(|error| JsValue::from_str(&error))?;
        let runtime_plan = ready
            .browser_runtime_plan()
            .map_err(|error| JsValue::from_str(&error))?;
        let bytecode =
            compile_ready_vfs(ready, &options).map_err(|error| JsValue::from_str(&error))?;
        let extensions = vo_web::collect_browser_wasm_extensions_from_vfs(&runtime_plan)
            .map_err(|error| JsValue::from_str(&error))?;
        vo_web::load_wasm_extensions(&extensions)
            .await
            .map_err(|error| JsValue::from_str(&error))?;
        let result = run_console_bytecode(&bytecode).map_err(|error| JsValue::from_str(&error))?;
        Ok(studio_run_result_to_js(&result))
    })
}

fn studio_run_result_to_js(result: &StudioRunResult) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("output"),
        &JsValue::from_str(&result.output),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("exitCode"),
        &JsValue::from_f64(result.exit_code.into()),
    );
    obj.into()
}

fn run_console_bytecode(bytecode: &[u8]) -> Result<StudioRunResult, String> {
    vo_web::take_output();

    let saved = vo_web::ext_bridge::save_extern_state();
    let run_result = (|| {
        let mut vm =
            vo_web::create_loaded_vm(bytecode, vo_web::ext_bridge::register_wasm_ext_bridges)?;
        match vm.run().map_err(|error| format!("{error:?}"))? {
            vo_vm::vm::SchedulingOutcome::Completed => Ok(0),
            vo_vm::vm::SchedulingOutcome::Exited(code) => Ok(code),
            vo_vm::vm::SchedulingOutcome::Blocked => Err(format!("{:?}", vm.deadlock_err())),
            vo_vm::vm::SchedulingOutcome::Suspended => Err(String::from(
                "console VM suspended before completion; asynchronous replay is unavailable",
            )),
            vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents => Err(String::from(
                "console VM is waiting for host events; asynchronous replay is unavailable",
            )),
            vo_vm::vm::SchedulingOutcome::Panicked => Err(String::from(
                "console VM terminated with an unexpected panic",
            )),
        }
    })();
    vo_web::ext_bridge::restore_extern_state(saved)?;

    Ok(StudioRunResult {
        output: vo_web::take_output(),
        exit_code: run_result?,
    })
}

#[wasm_bindgen(js_name = "dumpEntry")]
pub fn dump_entry(entry_path: &str, workspace_discovery: &str) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    let workspace_discovery = workspace_discovery.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let (ready, options) =
            prepare_ready_vfs_compile_from_discovery(&entry_path, &workspace_discovery)
                .await
                .map_err(|error| JsValue::from_str(&error))?;
        let bytecode =
            compile_ready_vfs(ready, &options).map_err(|error| JsValue::from_str(&error))?;
        let module = decode_verified_module(&bytecode, "Studio dump")
            .map_err(|error| JsValue::from_str(&error))?;
        Ok(JsValue::from_str(&bytecode_text_format::format_text(
            &module,
        )))
    })
}

#[wasm_bindgen(js_name = "dumpGuiEntry")]
pub fn dump_gui_entry(entry_path: &str, workspace_discovery: &str) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    let workspace_discovery = workspace_discovery.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let (ready, options) =
            prepare_ready_vfs_compile_from_discovery(&entry_path, &workspace_discovery)
                .await
                .map_err(|error| JsValue::from_str(&error))?;
        let GuiCompileOutput { bytecode, .. } =
            compile_gui_run_output(ready, &options).map_err(|error| JsValue::from_str(&error))?;
        let module = decode_verified_module(&bytecode, "Studio GUI dump")
            .map_err(|error| JsValue::from_str(&error))?;
        Ok(JsValue::from_str(&bytecode_text_format::format_text(
            &module,
        )))
    })
}

#[wasm_bindgen(js_name = "dumpBytecode")]
pub fn dump_bytecode(bytecode: &[u8]) -> Result<String, JsValue> {
    ensure_panic_hook();
    let module = decode_verified_module(bytecode, "Studio bytecode dump")
        .map_err(|e| JsValue::from_str(&e))?;
    Ok(bytecode_text_format::format_text(&module))
}

/// Compile a GUI entry point without running it.
/// Returns `{ bytecode: Uint8Array, entryPath: string, framework: FrameworkContract | null }`.
/// Intended for the web backend unified compile path, followed by host-provider
/// setup and prepared guest startup.
#[wasm_bindgen(js_name = "compileGui")]
pub fn compile_gui(
    entry_path: &str,
    workspace_discovery: &str,
    operation_id: &str,
) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    let workspace_discovery = workspace_discovery.to_string();
    cancellable_studio_promise(operation_id, async move {
        ensure_prepared_gui_launch_capacity().map_err(|error| JsValue::from_str(&error))?;
        let compile_start = js_sys::Date::now();
        let (ready, options) =
            prepare_ready_vfs_compile_from_discovery(&entry_path, &workspace_discovery)
                .await
                .map_err(|error| JsValue::from_str(&error))?;
        let GuiCompileOutput {
            target,
            bytecode,
            framework,
            provider_frameworks,
            runtime_plan,
            locked_modules,
        } = compile_gui_run_output(ready, &options).map_err(|error| JsValue::from_str(&error))?;
        let (snapshot_root, snapshot_files) =
            materialize_render_island_snapshot(&target, &runtime_plan)
                .map_err(|error| JsValue::from_str(&error))?;
        log_wasm_path(
            "gui_compile_done",
            &target.entry_path,
            "system",
            Some(compile_start),
        );
        let launch_token =
            prepare_gui_launch(&target.entry_path, &bytecode, runtime_plan, locked_modules)
                .map_err(|error| JsValue::from_str(&error))?;
        let obj = Object::new();
        let bytes = js_sys::Uint8Array::from(bytecode.as_slice());
        let _ = Reflect::set(&obj, &JsValue::from_str("bytecode"), &bytes);
        let _ = Reflect::set(
            &obj,
            &JsValue::from_str("entryPath"),
            &JsValue::from_str(&target.entry_path),
        );
        let framework_value = framework
            .as_ref()
            .map(framework_contract_to_js)
            .unwrap_or(JsValue::NULL);
        let _ = Reflect::set(&obj, &JsValue::from_str("framework"), &framework_value);
        let provider_frameworks_value = js_sys::Array::new();
        for provider in &provider_frameworks {
            provider_frameworks_value.push(&framework_contract_to_js(provider));
        }
        let _ = Reflect::set(
            &obj,
            &JsValue::from_str("providerFrameworks"),
            &provider_frameworks_value,
        );
        let snapshot = render_island_snapshot_to_js(&snapshot_root, snapshot_files);
        let _ = Reflect::set(&obj, &JsValue::from_str("vfsSnapshot"), &snapshot);
        let _ = Reflect::set(
            &obj,
            &JsValue::from_str("launchToken"),
            &JsValue::from_str(&launch_token.to_string()),
        );
        Ok(obj.into())
    })
}

/// Run a GUI app from pre-compiled bytecode (compiled by the native Rust backend via cmd_compile_gui).
/// Returns the initial render bytes. Framework metadata is provided separately by the caller.
#[wasm_bindgen(js_name = "prepareGuiFromBytecode")]
pub fn prepare_gui_from_bytecode(
    bytecode: &[u8],
    entry_path: &str,
    launch_token: &str,
    operation_id: &str,
) -> js_sys::Promise {
    let bytecode = bytecode.to_vec();
    let entry_path = entry_path.to_string();
    let launch_token = launch_token.to_string();
    cancellable_studio_promise(operation_id, async move {
        let prepared = take_prepared_gui_launch(&launch_token, &entry_path, &bytecode)?;
        vo_web::load_wasm_extensions(&prepared.extensions)
            .await
            .map_err(|error| JsValue::from_str(&error))?;
        let handle = prepare_gui_from_bytecode_with(&bytecode, &entry_path, prepared)?;
        Ok(preview_handle_to_js(handle))
    })
}

#[wasm_bindgen(js_name = "startPreparedGui")]
pub fn start_prepared_gui(
    preview_index: u32,
    preview_generation: u32,
    entry_path: &str,
) -> Result<Vec<u8>, JsValue> {
    let handle = preview_handle(preview_index, preview_generation);
    match start_prepared_gui_with(handle, entry_path, |guest| guest.start_gui_app_step()) {
        Ok(render_output) => Ok(render_output),
        Err(error) => {
            let _ = close_browser_host(handle);
            Err(error)
        }
    }
}

/// Send an event to the running guest app, returning the new render bytes.
///
/// Stores event data and wakes the main fiber (blocked on waitForEvent).
/// The fiber processes the event inline and blocks again on waitForEvent.
/// No new fiber is created — zero allocation per event.
#[wasm_bindgen(js_name = "sendGuiEvent")]
pub fn send_gui_event(
    preview_index: u32,
    preview_generation: u32,
    handler_id: i32,
    payload: &str,
) -> Result<Vec<u8>, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let _ = host.render.poll();
        if enqueue_browser_vogui_target_turn(
            host,
            handler_id,
            payload.as_bytes(),
            None,
            None,
            None,
            None,
        )? {
            return Ok(Vec::new());
        }
        let step = host
            .guest
            .dispatch_gui_event(handler_id, payload)
            .map_err(session_error_to_js)?;
        run_gc_stress_guest_step(&mut host.guest);
        publish_guest_stdout(&host.guest, "guest", step.stdout.as_deref())?;
        Ok(step.render_output.unwrap_or_default())
    })
}

#[wasm_bindgen(js_name = "sendGuiEventAsync")]
pub fn send_gui_event_async(
    preview_index: u32,
    preview_generation: u32,
    handler_id: i32,
    payload: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        if enqueue_browser_vogui_target_turn(
            host,
            handler_id,
            payload.as_bytes(),
            None,
            None,
            None,
            None,
        )? {
            return Ok(());
        }
        let step = host
            .guest
            .try_dispatch_gui_event(handler_id, payload)
            .map_err(session_error_to_js)?;
        if let Some(step) = step {
            run_gc_stress_guest_step(&mut host.guest);
            publish_guest_stdout(&host.guest, "guest", step.stdout.as_deref())?;
            if let Some(render_output) = step.render_output {
                host.render.push(render_output);
            }
        }
        Ok(())
    })
}

#[wasm_bindgen(js_name = "setGcStressEveryStep")]
pub fn set_gc_stress_every_step(enabled: bool) {
    GC_STRESS_EVERY_STEP.with(|cell| cell.set(enabled));
    let handles = GUESTS.with(|guests| guests.borrow().handles().collect::<Vec<_>>());
    for handle in handles {
        let _ = with_guest_mut(handle, |host| {
            host.guest.set_gc_stress_every_step(enabled);
            Ok(())
        });
    }
}

#[wasm_bindgen(js_name = "setGcStressHostStep")]
pub fn set_gc_stress_host_step(enabled: bool) {
    GC_STRESS_HOST_STEP.with(|cell| cell.set(enabled));
}

#[wasm_bindgen(js_name = "pushIslandData")]
pub fn push_island_data(
    preview_index: u32,
    preview_generation: u32,
    data: &[u8],
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let step = host
            .guest
            .push_island_frame(data)
            .map_err(session_error_to_js)?;
        run_gc_stress_guest_step(&mut host.guest);
        publish_guest_stdout(&host.guest, "guest", step.stdout.as_deref())?;
        if let Some(render_output) = step.render_output {
            host.render.push(render_output);
        }
        Ok(())
    })
}

#[wasm_bindgen(js_name = "pollGuiRender")]
pub fn poll_gui_render(preview_index: u32, preview_generation: u32) -> Result<Vec<u8>, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        drive_browser_framework_clocks(host)?;
        Ok(host.render.poll().unwrap_or_default())
    })
}

#[wasm_bindgen(js_name = "pollGameRender")]
pub fn poll_game_render(preview_index: u32, preview_generation: u32) -> Result<Vec<u8>, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        drive_browser_framework_clocks(host)?;
        Ok(host.render.poll_game().unwrap_or_default())
    })
}

#[wasm_bindgen(js_name = "completeVoguiTargetCommit")]
pub fn complete_vogui_target_commit(
    preview_index: u32,
    preview_generation: u32,
    accepted: bool,
    provider_error: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let pending = host
            .pending_vogui_commit
            .take()
            .ok_or_else(|| JsValue::from_str("browser Vogui provider has no pending commit"))?;
        let result = if accepted {
            host.active_framework_providers
                .get_mut(&pending.module_key)
                .ok_or_else(|| String::from("browser Vogui provider disappeared"))
                .and_then(|group| {
                    group.commit_vogui_target_state(
                        pending.caller,
                        pending.commit.model,
                        pending.commit.update_result,
                        pending.commit.effects,
                        pending.commit.presentation,
                        pending.commit.subscriptions,
                    )
                })
        } else {
            Err(if provider_error.is_empty() {
                String::from("browser Vogui logic provider rejected the commit")
            } else {
                provider_error.to_owned()
            })
        };
        let (outcome, response) = match result {
            Ok(_) => (RequestOutcome::Success, vec![0]),
            Err(error) => {
                let mut response = vec![1];
                response.extend_from_slice(error.as_bytes());
                (RequestOutcome::ProviderError, response)
            }
        };
        finish_browser_host_request_for(host, pending.caller, pending.request_id, outcome, response)
    })
}

#[wasm_bindgen(js_name = "pollVoguiEffect")]
pub fn poll_vogui_effect(preview_index: u32, preview_generation: u32) -> Result<Vec<u8>, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        Ok(take_browser_vogui_effect(host)?.unwrap_or_default())
    })
}

#[wasm_bindgen(js_name = "pollPlatformRequest")]
pub fn poll_platform_request(
    preview_index: u32,
    preview_generation: u32,
) -> Result<Vec<u8>, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        drive_browser_framework_clocks(host)?;
        let now_millis = browser_monotonic_millis()?;
        host.guest
            .poll_host_platform_request(now_millis)
            .map_err(|error| JsValue::from_str(&error))
            .map(|request| {
                request.map_or_else(Vec::new, |value| {
                    vo_app_runtime::encode_platform_request_frame(&value)
                })
            })
    })
}

#[wasm_bindgen(js_name = "pollVoguiSubscriptions")]
pub fn poll_vogui_subscriptions(
    preview_index: u32,
    preview_generation: u32,
) -> Result<Vec<u8>, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let bindings = host
            .active_framework_providers
            .values()
            .flat_map(vo_app_runtime::HostedInstanceGroup::active_vogui_subscriptions)
            .collect::<Vec<_>>();
        vo_app_runtime::encode_vogui_subscription_bindings(&bindings)
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "submitVoguiSubscriptionEvent")]
pub fn submit_vogui_subscription_event(
    preview_index: u32,
    preview_generation: u32,
    caller: &[u8],
    handle_index: u32,
    handle_generation: u32,
    payload: &[u8],
) -> Result<(), JsValue> {
    let caller = decode_vogui_subscription_caller(caller)?;
    let handle = vo_runtime::host_services_v2::HostResourceHandle {
        index: handle_index,
        generation: handle_generation,
    };
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let event = host
            .active_framework_providers
            .values()
            .find(|group| group.vogui_subscription_records(caller).is_some())
            .ok_or_else(|| JsValue::from_str("browser Vogui subscription caller is not active"))?
            .emit_vogui_subscription_event(caller, handle, payload.to_vec())
            .map_err(|error| JsValue::from_str(&error))?;
        submit_browser_vogui_subscription_event(host, event)?;
        drive_browser_framework_clocks(host)
    })
}

fn decode_vogui_subscription_caller(
    bytes: &[u8],
) -> Result<vo_runtime::host_services_v2::CallerEndpointHandle, JsValue> {
    if bytes.len() != 32 {
        return Err(JsValue::from_str(
            "Vogui subscription caller token must contain 32 bytes",
        ));
    }
    Ok(vo_runtime::host_services_v2::CallerEndpointHandle {
        session_index: u32::from_le_bytes(bytes[0..4].try_into().unwrap()),
        session_generation: u32::from_le_bytes(bytes[4..8].try_into().unwrap()),
        session_epoch: u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
        endpoint_index: u32::from_le_bytes(bytes[16..20].try_into().unwrap()),
        endpoint_generation: u32::from_le_bytes(bytes[20..24].try_into().unwrap()),
        endpoint_epoch: u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
    })
}

#[wasm_bindgen(js_name = "completePlatformRequest")]
pub fn complete_platform_request(
    preview_index: u32,
    preview_generation: u32,
    request_id: &str,
    outcome: &str,
    payload: &[u8],
) -> Result<(), JsValue> {
    let request_id = request_id
        .parse::<u64>()
        .map_err(|_| JsValue::from_str("requestId must be an unsigned 64-bit decimal string"))?;
    let outcome = parse_platform_completion_outcome(outcome)?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .complete_host_platform_request(request_id, outcome, payload.to_vec())
            .map_err(|error| JsValue::from_str(&error))?;
        drive_browser_framework_clocks(host)
    })
}

fn parse_u64_field(value: &str, name: &str) -> Result<u64, JsValue> {
    value.parse::<u64>().map_err(|_| {
        JsValue::from_str(&format!("{name} must be an unsigned 64-bit decimal string"))
    })
}

fn platform_handle(index: u32, generation: u32) -> vo_app_runtime::GenerationalHandle {
    vo_app_runtime::GenerationalHandle { index, generation }
}

fn platform_handle_to_js(handle: vo_app_runtime::GenerationalHandle) -> Object {
    let value = Object::new();
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("index"),
        &JsValue::from_f64(handle.index as f64),
    );
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("generation"),
        &JsValue::from_f64(handle.generation as f64),
    );
    value
}

fn parse_view_visibility(value: &str) -> Result<vo_app_runtime::ViewVisibility, JsValue> {
    match value {
        "visible" => Ok(vo_app_runtime::ViewVisibility::Visible),
        "hidden" => Ok(vo_app_runtime::ViewVisibility::Hidden),
        "suspended" => Ok(vo_app_runtime::ViewVisibility::Suspended),
        _ => Err(JsValue::from_str("unknown View visibility")),
    }
}

fn parse_device_kind(value: &str) -> Result<vo_app_runtime::InputDeviceKind, JsValue> {
    match value {
        "mouse" => Ok(vo_app_runtime::InputDeviceKind::Mouse),
        "touch" => Ok(vo_app_runtime::InputDeviceKind::Touch),
        "pen" => Ok(vo_app_runtime::InputDeviceKind::Pen),
        "keyboard" => Ok(vo_app_runtime::InputDeviceKind::Keyboard),
        "gamepad" => Ok(vo_app_runtime::InputDeviceKind::Gamepad),
        _ => Err(JsValue::from_str("unknown input device kind")),
    }
}

fn input_header(
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    device_kind: &str,
    modifier_flags: u32,
) -> Result<vo_app_runtime::PlatformInputHeader, JsValue> {
    Ok(vo_app_runtime::PlatformInputHeader {
        sequence: parse_u64_field(sequence, "sequence")?,
        timestamp_micros: parse_u64_field(timestamp_micros, "timestampMicros")?,
        metrics_revision: parse_u64_field(metrics_revision, "metricsRevision")?,
        window: platform_handle(window_index, window_generation),
        view: platform_handle(view_index, view_generation),
        device: vo_app_runtime::InputDeviceId {
            value: parse_u64_field(device_id, "deviceId")?,
            generation: device_generation,
        },
        device_kind: parse_device_kind(device_kind)?,
        modifiers: vo_app_runtime::InputModifiers {
            shift: modifier_flags & 1 != 0,
            control: modifier_flags & 2 != 0,
            alt: modifier_flags & 4 != 0,
            meta: modifier_flags & 8 != 0,
            caps_lock: modifier_flags & 16 != 0,
            num_lock: modifier_flags & 32 != 0,
        },
    })
}

fn route_browser_platform_input(
    preview_index: u32,
    preview_generation: u32,
    header: vo_app_runtime::PlatformInputHeader,
    payload: vo_app_runtime::PlatformInputPayload,
) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let report = host
            .guest
            .route_host_platform_input(vo_app_runtime::PlatformInputEvent { header, payload })
            .map_err(|error| JsValue::from_str(&error))?;
        let value = Object::new();
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("compositionRevision"),
            &JsValue::from_str(&report.composition_revision.to_string()),
        );
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("synthesizedReleaseCount"),
            &JsValue::from_f64(report.synthesized_releases.len() as f64),
        );
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("arbitrated"),
            &JsValue::from_bool(report.arbitration.is_some()),
        );
        Ok(value.into())
    })
}

#[wasm_bindgen(js_name = "createPlatformWindow")]
pub fn create_platform_window(
    preview_index: u32,
    preview_generation: u32,
) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .create_host_window()
            .map(platform_handle_to_js)
            .map(Into::into)
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "closePlatformWindow")]
pub fn close_platform_window(
    preview_index: u32,
    preview_generation: u32,
    window_index: u32,
    window_generation: u32,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .close_host_window(platform_handle(window_index, window_generation))
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "createPlatformView")]
pub fn create_platform_view(
    preview_index: u32,
    preview_generation: u32,
    window_index: u32,
    window_generation: u32,
) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .create_host_view(platform_handle(window_index, window_generation))
            .map(platform_handle_to_js)
            .map(Into::into)
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "updatePlatformViewMetrics")]
#[allow(clippy::too_many_arguments)]
pub fn update_platform_view_metrics(
    preview_index: u32,
    preview_generation: u32,
    view_index: u32,
    view_generation: u32,
    expected_revision: &str,
    origin_x_milli: i32,
    origin_y_milli: i32,
    width_milli: u32,
    height_milli: u32,
    framebuffer_width: u32,
    framebuffer_height: u32,
    scale_q16: u32,
    safe_top_milli: u32,
    safe_right_milli: u32,
    safe_bottom_milli: u32,
    safe_left_milli: u32,
    visibility: &str,
) -> Result<JsValue, JsValue> {
    let expected_revision = parse_u64_field(expected_revision, "expectedRevision")?;
    let update = vo_app_runtime::ViewMetricsUpdate {
        origin_x_milli,
        origin_y_milli,
        width_milli,
        height_milli,
        framebuffer_width,
        framebuffer_height,
        scale_q16,
        safe_area: vo_app_runtime::ViewInsets {
            top_milli: safe_top_milli,
            right_milli: safe_right_milli,
            bottom_milli: safe_bottom_milli,
            left_milli: safe_left_milli,
        },
        visibility: parse_view_visibility(visibility)?,
    };
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let metrics = host
            .guest
            .update_host_view_metrics(
                platform_handle(view_index, view_generation),
                update,
                expected_revision,
            )
            .map_err(|error| JsValue::from_str(&error))?;
        let value = Object::new();
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("revision"),
            &JsValue::from_str(&metrics.revision.to_string()),
        );
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("scaleQ16"),
            &JsValue::from_f64(metrics.scale_q16 as f64),
        );
        Ok(value.into())
    })
}

#[wasm_bindgen(js_name = "closePlatformView")]
pub fn close_platform_view(
    preview_index: u32,
    preview_generation: u32,
    view_index: u32,
    view_generation: u32,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .close_host_view(platform_handle(view_index, view_generation))
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "attachPlatformSurface")]
pub fn attach_platform_surface(
    preview_index: u32,
    preview_generation: u32,
    view_index: u32,
    view_generation: u32,
    kind: &str,
    z_order: i32,
    input_policy: &str,
    accepts_text: bool,
) -> Result<JsValue, JsValue> {
    let kind = match kind {
        "game" => vo_app_runtime::SurfaceKind::Game,
        "ui" => vo_app_runtime::SurfaceKind::Ui,
        "diagnostics" => vo_app_runtime::SurfaceKind::Diagnostics,
        _ => return Err(JsValue::from_str("unknown Surface kind")),
    };
    let input = match input_policy {
        "observe" => vo_app_runtime::SurfaceInputPolicy::Observe,
        "passthrough" => vo_app_runtime::SurfaceInputPolicy::Passthrough,
        "interactive" => vo_app_runtime::SurfaceInputPolicy::Interactive,
        "exclusive" => vo_app_runtime::SurfaceInputPolicy::Exclusive,
        _ => return Err(JsValue::from_str("unknown Surface input policy")),
    };
    let descriptor = vo_app_runtime::SurfaceDescriptor {
        view: platform_handle(view_index, view_generation),
        kind,
        z_order,
        input,
        accepts_text,
        geometry: vo_app_runtime::SurfaceGeometry::default(),
    };
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .attach_host_surface(descriptor)
            .map(platform_handle_to_js)
            .map(Into::into)
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "updatePlatformSurfaceGeometry")]
#[allow(clippy::too_many_arguments)]
pub fn update_platform_surface_geometry(
    preview_index: u32,
    preview_generation: u32,
    surface_index: u32,
    surface_generation: u32,
    expected_revision: &str,
    has_bounds: bool,
    x_milli: i32,
    y_milli: i32,
    width_milli: u32,
    height_milli: u32,
    opacity_q16: u16,
    hit_test_enabled: bool,
) -> Result<String, JsValue> {
    let geometry = vo_app_runtime::SurfaceGeometry {
        bounds: has_bounds.then_some(vo_app_runtime::SurfaceRect {
            x_milli,
            y_milli,
            width_milli,
            height_milli,
        }),
        opacity_q16,
        hit_test_enabled,
        ..vo_app_runtime::SurfaceGeometry::default()
    };
    let expected_revision = parse_u64_field(expected_revision, "expectedRevision")?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .update_host_surface_geometry(
                platform_handle(surface_index, surface_generation),
                geometry,
                expected_revision,
            )
            .map(|revision| revision.to_string())
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "closePlatformSurface")]
pub fn close_platform_surface(
    preview_index: u32,
    preview_generation: u32,
    surface_index: u32,
    surface_generation: u32,
) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let report = host
            .guest
            .close_host_surface(platform_handle(surface_index, surface_generation))
            .map_err(|error| JsValue::from_str(&error))?;
        let value = Object::new();
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("synthesizedReleaseCount"),
            &JsValue::from_f64(report.synthesized_releases.len() as f64),
        );
        Ok(value.into())
    })
}

fn parse_surface_presentation_outcome(
    value: &str,
) -> Result<vo_app_runtime::SurfacePresentationOutcome, JsValue> {
    match value {
        "presented" => Ok(vo_app_runtime::SurfacePresentationOutcome::Presented),
        "deadline-missed" => Ok(vo_app_runtime::SurfacePresentationOutcome::DeadlineMissed),
        "zero-sized" => Ok(vo_app_runtime::SurfacePresentationOutcome::ZeroSized),
        "suspended" => Ok(vo_app_runtime::SurfacePresentationOutcome::Suspended),
        "timed-out" => Ok(vo_app_runtime::SurfacePresentationOutcome::TimedOut),
        "surface-lost" => Ok(vo_app_runtime::SurfacePresentationOutcome::SurfaceLost),
        "device-lost" => Ok(vo_app_runtime::SurfacePresentationOutcome::DeviceLost),
        _ => Err(JsValue::from_str("unknown Surface presentation outcome")),
    }
}

fn platform_surface_status_to_js(status: vo_app_runtime::SurfaceStatus) -> JsValue {
    let value = Object::new();
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("surface"),
        &platform_handle_to_js(status.surface),
    );
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("surfaceGeneration"),
        &JsValue::from_str(&status.generation.to_string()),
    );
    let state = match status.state {
        vo_app_runtime::SurfaceRuntimeState::Active => "active",
        vo_app_runtime::SurfaceRuntimeState::Suspended => "suspended",
        vo_app_runtime::SurfaceRuntimeState::Lost => "lost",
        vo_app_runtime::SurfaceRuntimeState::Recovering => "recovering",
    };
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("state"),
        &JsValue::from_str(state),
    );
    let outcome = status.last_outcome.map(|outcome| match outcome {
        vo_app_runtime::SurfacePresentationOutcome::Presented => "presented",
        vo_app_runtime::SurfacePresentationOutcome::DeadlineMissed => "deadline-missed",
        vo_app_runtime::SurfacePresentationOutcome::ZeroSized => "zero-sized",
        vo_app_runtime::SurfacePresentationOutcome::Suspended => "suspended",
        vo_app_runtime::SurfacePresentationOutcome::TimedOut => "timed-out",
        vo_app_runtime::SurfacePresentationOutcome::SurfaceLost => "surface-lost",
        vo_app_runtime::SurfacePresentationOutcome::DeviceLost => "device-lost",
    });
    let _ = Reflect::set(
        &value,
        &JsValue::from_str("lastOutcome"),
        &outcome.map_or(JsValue::NULL, JsValue::from_str),
    );
    value.into()
}

#[wasm_bindgen(js_name = "reportPlatformSurfaceOutcome")]
pub fn report_platform_surface_outcome(
    preview_index: u32,
    preview_generation: u32,
    surface_index: u32,
    surface_handle_generation: u32,
    surface_generation: &str,
    outcome: &str,
) -> Result<JsValue, JsValue> {
    let surface_generation = parse_u64_field(surface_generation, "surfaceGeneration")?;
    let outcome = parse_surface_presentation_outcome(outcome)?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .report_host_surface_outcome(
                platform_handle(surface_index, surface_handle_generation),
                surface_generation,
                outcome,
            )
            .map(platform_surface_status_to_js)
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "beginPlatformSurfaceRecovery")]
pub fn begin_platform_surface_recovery(
    preview_index: u32,
    preview_generation: u32,
    surface_index: u32,
    surface_handle_generation: u32,
    expected_generation: &str,
) -> Result<JsValue, JsValue> {
    let expected_generation = parse_u64_field(expected_generation, "expectedGeneration")?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let ticket = host
            .guest
            .begin_host_surface_recovery(
                platform_handle(surface_index, surface_handle_generation),
                expected_generation,
            )
            .map_err(|error| JsValue::from_str(&error))?;
        let value = Object::new();
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("surface"),
            &platform_handle_to_js(ticket.surface),
        );
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("oldGeneration"),
            &JsValue::from_str(&ticket.old_generation.to_string()),
        );
        let _ = Reflect::set(
            &value,
            &JsValue::from_str("newGeneration"),
            &JsValue::from_str(&ticket.new_generation.to_string()),
        );
        Ok(value.into())
    })
}

#[wasm_bindgen(js_name = "completePlatformSurfaceRecovery")]
#[allow(clippy::too_many_arguments)]
pub fn complete_platform_surface_recovery(
    preview_index: u32,
    preview_generation: u32,
    surface_index: u32,
    surface_handle_generation: u32,
    old_generation: &str,
    new_generation: &str,
    suspended: bool,
) -> Result<JsValue, JsValue> {
    let ticket = vo_app_runtime::SurfaceRecoveryTicket {
        surface: platform_handle(surface_index, surface_handle_generation),
        old_generation: parse_u64_field(old_generation, "oldGeneration")?,
        new_generation: parse_u64_field(new_generation, "newGeneration")?,
    };
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .complete_host_surface_recovery(ticket, suspended)
            .map(platform_surface_status_to_js)
            .map_err(|error| JsValue::from_str(&error))
    })
}

#[wasm_bindgen(js_name = "routePlatformPointerInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_pointer_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    device_kind: &str,
    modifier_flags: u32,
    contact: u32,
    phase: &str,
    x_milli: i32,
    y_milli: i32,
    delta_x_milli: i32,
    delta_y_milli: i32,
    pressure_q15: u16,
    tilt_x_degrees: i16,
    tilt_y_degrees: i16,
    buttons: u32,
    changed_button: i16,
) -> Result<JsValue, JsValue> {
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        device_kind,
        modifier_flags,
    )?;
    let phase = match phase {
        "down" => vo_app_runtime::PointerPhase::Down,
        "move" => vo_app_runtime::PointerPhase::Move,
        "up" => vo_app_runtime::PointerPhase::Up,
        "cancel" => vo_app_runtime::PointerPhase::Cancel,
        _ => return Err(JsValue::from_str("unknown pointer phase")),
    };
    let changed_button = if changed_button < 0 {
        None
    } else {
        Some(
            u8::try_from(changed_button)
                .map_err(|_| JsValue::from_str("changedButton exceeds u8"))?,
        )
    };
    route_browser_platform_input(
        preview_index,
        preview_generation,
        header,
        vo_app_runtime::PlatformInputPayload::Pointer {
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
        },
    )
}

#[wasm_bindgen(js_name = "routePlatformWheelInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_wheel_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    modifier_flags: u32,
    contact: u32,
    x_milli: i32,
    y_milli: i32,
    delta_x_milli: i32,
    delta_y_milli: i32,
    unit: &str,
) -> Result<JsValue, JsValue> {
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        "mouse",
        modifier_flags,
    )?;
    let unit = match unit {
        "pixel" => vo_app_runtime::WheelUnit::Pixel,
        "line" => vo_app_runtime::WheelUnit::Line,
        "page" => vo_app_runtime::WheelUnit::Page,
        _ => return Err(JsValue::from_str("unknown wheel unit")),
    };
    route_browser_platform_input(
        preview_index,
        preview_generation,
        header,
        vo_app_runtime::PlatformInputPayload::Wheel {
            contact,
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            unit,
        },
    )
}

#[wasm_bindgen(js_name = "routePlatformKeyInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_key_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    modifier_flags: u32,
    phase: &str,
    physical_key: u32,
    logical_key: &str,
    repeat: bool,
) -> Result<JsValue, JsValue> {
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        "keyboard",
        modifier_flags,
    )?;
    let phase = match phase {
        "down" => vo_app_runtime::KeyPhase::Down,
        "up" => vo_app_runtime::KeyPhase::Up,
        _ => return Err(JsValue::from_str("unknown key phase")),
    };
    route_browser_platform_input(
        preview_index,
        preview_generation,
        header,
        vo_app_runtime::PlatformInputPayload::Key {
            phase,
            physical_key,
            logical_key: logical_key.to_owned(),
            repeat,
        },
    )
}

#[wasm_bindgen(js_name = "routePlatformShortcutInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_shortcut_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    modifier_flags: u32,
    class_mask: &str,
    system: bool,
) -> Result<JsValue, JsValue> {
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        "keyboard",
        modifier_flags,
    )?;
    route_browser_platform_input(
        preview_index,
        preview_generation,
        header,
        vo_app_runtime::PlatformInputPayload::Shortcut {
            class_mask: parse_u64_field(class_mask, "classMask")?,
            system,
        },
    )
}

#[wasm_bindgen(js_name = "routePlatformTextInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_text_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    modifier_flags: u32,
    text: &str,
) -> Result<JsValue, JsValue> {
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        "keyboard",
        modifier_flags,
    )?;
    route_browser_platform_input(
        preview_index,
        preview_generation,
        header,
        vo_app_runtime::PlatformInputPayload::Text {
            text: text.to_owned(),
        },
    )
}

#[wasm_bindgen(js_name = "routePlatformCompositionInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_composition_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    modifier_flags: u32,
    phase: &str,
    text: &str,
    selection_start: u32,
    selection_end: u32,
) -> Result<JsValue, JsValue> {
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        "keyboard",
        modifier_flags,
    )?;
    let phase = match phase {
        "start" => vo_app_runtime::CompositionPhase::Start,
        "update" => vo_app_runtime::CompositionPhase::Update,
        "end" => vo_app_runtime::CompositionPhase::End,
        "cancel" => vo_app_runtime::CompositionPhase::Cancel,
        _ => return Err(JsValue::from_str("unknown composition phase")),
    };
    route_browser_platform_input(
        preview_index,
        preview_generation,
        header,
        vo_app_runtime::PlatformInputPayload::Composition {
            phase,
            text: text.to_owned(),
            selection_start,
            selection_end,
        },
    )
}

#[wasm_bindgen(js_name = "routePlatformGamepadInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_gamepad_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    connected: bool,
    mapping: &str,
    axes_q15: &[i16],
    button_values_q15: &[u16],
    button_flags: &[u8],
) -> Result<JsValue, JsValue> {
    if button_values_q15.len() != button_flags.len() {
        return Err(JsValue::from_str(
            "gamepad button values and flags must have equal lengths",
        ));
    }
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        "gamepad",
        0,
    )?;
    let mapping = match mapping {
        "standard" => vo_app_runtime::GamepadMapping::Standard,
        "raw" => vo_app_runtime::GamepadMapping::Raw,
        _ => return Err(JsValue::from_str("unknown gamepad mapping")),
    };
    let buttons = button_values_q15
        .iter()
        .zip(button_flags)
        .map(|(value_q15, flags)| vo_app_runtime::GamepadButton {
            value_q15: *value_q15,
            pressed: flags & 1 != 0,
            touched: flags & 2 != 0,
        })
        .collect();
    route_browser_platform_input(
        preview_index,
        preview_generation,
        header,
        vo_app_runtime::PlatformInputPayload::GamepadSnapshot {
            connected,
            mapping,
            axes_q15: axes_q15.to_vec(),
            buttons,
        },
    )
}

#[wasm_bindgen(js_name = "routePlatformLifecycleInput")]
#[allow(clippy::too_many_arguments)]
pub fn route_platform_lifecycle_input(
    preview_index: u32,
    preview_generation: u32,
    sequence: &str,
    timestamp_micros: &str,
    metrics_revision: &str,
    window_index: u32,
    window_generation: u32,
    view_index: u32,
    view_generation: u32,
    device_id: &str,
    device_generation: u32,
    device_kind: &str,
    modifier_flags: u32,
    event: &str,
) -> Result<JsValue, JsValue> {
    let header = input_header(
        sequence,
        timestamp_micros,
        metrics_revision,
        window_index,
        window_generation,
        view_index,
        view_generation,
        device_id,
        device_generation,
        device_kind,
        modifier_flags,
    )?;
    let payload = match event {
        "focus-gained" => vo_app_runtime::PlatformInputPayload::FocusChanged { focused: true },
        "focus-lost" => vo_app_runtime::PlatformInputPayload::FocusChanged { focused: false },
        "visible" => vo_app_runtime::PlatformInputPayload::VisibilityChanged { visible: true },
        "hidden" => vo_app_runtime::PlatformInputPayload::VisibilityChanged { visible: false },
        "device-disconnected" => vo_app_runtime::PlatformInputPayload::DeviceDisconnected,
        _ => return Err(JsValue::from_str("unknown platform lifecycle input")),
    };
    route_browser_platform_input(preview_index, preview_generation, header, payload)
}

fn resolve_browser_framework_lane(
    host: &BrowserSessionHost,
    owner: &str,
) -> Result<BrowserFrameworkLane, String> {
    for binding in host.framework_provider_bindings.values() {
        let role =
            if owner == binding.lane_owner {
                if binding.providers.iter().any(|provider| {
                    provider.loaded.role == vo_app_runtime::ProviderRole::GameRenderer
                }) {
                    vo_app_runtime::ProviderRole::GameRenderer
                } else {
                    vo_app_runtime::ProviderRole::UiRenderer
                }
            } else {
                let Some(suffix) = owner
                    .strip_prefix(&binding.lane_owner)
                    .and_then(|suffix| suffix.strip_prefix('/'))
                else {
                    continue;
                };
                match suffix {
                    "asset" => vo_app_runtime::ProviderRole::GameAsset,
                    "render" => vo_app_runtime::ProviderRole::GameRenderer,
                    "audio" => vo_app_runtime::ProviderRole::GameAudio,
                    "logic" => vo_app_runtime::ProviderRole::GameLogic,
                    "ui-logic" => vo_app_runtime::ProviderRole::UiLogic,
                    "ui-renderer" => vo_app_runtime::ProviderRole::UiRenderer,
                    "surface-host" => vo_app_runtime::ProviderRole::SurfaceHost,
                    "accessibility" => vo_app_runtime::ProviderRole::Accessibility,
                    "diagnostics" => vo_app_runtime::ProviderRole::Diagnostics,
                    _ => continue,
                }
            };
        if !binding
            .providers
            .iter()
            .any(|provider| provider.loaded.role == role)
        {
            return Err(format!(
                "framework lane {owner} selects a role absent from the resolved provider set"
            ));
        }
        if !host
            .pending_framework_providers
            .contains_key(&binding.module_key)
            && !host
                .active_framework_providers
                .contains_key(&binding.module_key)
        {
            return Err(format!(
                "framework lane {owner} provider group is not pending or active"
            ));
        }
        return Ok(BrowserFrameworkLane {
            module_key: binding.module_key.clone(),
            owner: owner.to_string(),
            role,
        });
    }
    Err(format!("framework lane owner {owner:?} is not resolved"))
}

fn dispatch_browser_voplay_outboxes(
    host: &mut BrowserSessionHost,
    module_key: &str,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
) -> Result<u64, String> {
    let roles = [
        vo_app_runtime::ProviderRole::GameLogic,
        vo_app_runtime::ProviderRole::GameAsset,
        vo_app_runtime::ProviderRole::GameRenderer,
        vo_app_runtime::ProviderRole::GameAudio,
    ];
    let mut lanes = Vec::new();
    for role in roles {
        let has_packet = host
            .active_framework_providers
            .get(module_key)
            .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
            .has_voplay_role_packet(caller, role)?;
        let matching = host
            .framework_lanes
            .iter()
            .filter(|(_, lane)| lane.module_key == module_key && lane.role == role)
            .collect::<Vec<_>>();
        if matching.is_empty() {
            continue;
        }
        if matching.len() != 1 {
            return Err(format!(
                "Voplay output role {role:?} has {} browser provider lanes",
                matching.len()
            ));
        }
        lanes.push((
            role,
            matching[0].1.owner.clone(),
            has_packet,
            matching[0].0 .2,
        ));
    }
    let services = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("browser runtime has no HostServices V2 owner"))?;
    let endpoint = host
        .guest
        .host_caller()
        .ok_or_else(|| String::from("browser runtime has no hosted endpoint"))?;
    for (role, owner, has_packet, lane_channel_epoch) in lanes {
        if role == vo_app_runtime::ProviderRole::GameRenderer
            && !host.voplay_render_features_initialized.contains(&(
                module_key.to_owned(),
                caller.endpoint_index,
                caller.endpoint_generation,
            ))
        {
            let features = host
                .active_framework_providers
                .get(module_key)
                .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                .voplay_render_feature_descriptors(caller)?;
            let bootstrap = encode_browser_voplay_render_feature_bootstrap(
                (caller.endpoint_index, caller.endpoint_generation),
                &features,
            )?;
            services
                .publish_named_endpoint_payload(endpoint, owner.as_bytes(), &bootstrap)
                .map_err(|status| {
                    format!(
                        "publish Voplay RenderFeature bootstrap to browser lane: status {status}"
                    )
                })?;
            host.voplay_render_features_initialized.insert((
                module_key.to_owned(),
                caller.endpoint_index,
                caller.endpoint_generation,
            ));
        }
        let role_tag = match role {
            vo_app_runtime::ProviderRole::GameAsset => 1,
            vo_app_runtime::ProviderRole::GameRenderer => 2,
            vo_app_runtime::ProviderRole::GameAudio => 3,
            vo_app_runtime::ProviderRole::GameLogic => 4,
            _ => unreachable!(),
        };
        let initialized = (
            module_key.to_owned(),
            caller.endpoint_index,
            caller.endpoint_generation,
            role_tag,
        );
        let initialized_now = !host.voplay_role_engines_initialized.contains(&initialized);
        if initialized_now {
            let channel_epoch = lane_channel_epoch;
            let start = encode_browser_voplay_engine_lifecycle_packet(
                12,
                (caller.endpoint_index, caller.endpoint_generation),
                lane_channel_epoch,
            )?;
            services
                .publish_named_endpoint_payload(endpoint, owner.as_bytes(), &start)
                .map_err(|status| {
                    format!("publish Voplay EngineStart to browser lane: status {status}")
                })?;
            host.voplay_role_engines_initialized
                .insert(initialized.clone());
            host.voplay_role_engine_epochs
                .insert(initialized, lane_channel_epoch);
            let replay_roles: &[vo_app_runtime::ProviderRole] = match role {
                vo_app_runtime::ProviderRole::GameLogic => &[
                    vo_app_runtime::ProviderRole::GameRenderer,
                    vo_app_runtime::ProviderRole::GameAudio,
                ],
                vo_app_runtime::ProviderRole::GameRenderer => {
                    &[vo_app_runtime::ProviderRole::GameRenderer]
                }
                vo_app_runtime::ProviderRole::GameAudio => {
                    &[vo_app_runtime::ProviderRole::GameAudio]
                }
                _ => &[],
            };
            for replay_role in replay_roles {
                let snapshot = host
                    .active_framework_providers
                    .get(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .voplay_control_snapshot(caller, *replay_role)?;
                if let Some(snapshot) = snapshot {
                    let snapshot = if role == vo_app_runtime::ProviderRole::GameLogic {
                        retarget_browser_voplay_control_adoption(snapshot, channel_epoch)?
                    } else {
                        retarget_browser_voplay_packet_epoch(snapshot, channel_epoch)?
                    };
                    services
                        .publish_named_endpoint_payload(endpoint, owner.as_bytes(), &snapshot)
                        .map_err(|status| {
                            format!(
                                "publish Voplay retained control snapshot to browser lane: status {status}"
                            )
                        })?;
                }
            }
            if role == vo_app_runtime::ProviderRole::GameRenderer && channel_epoch > 1 {
                let snapshot = host
                    .active_framework_providers
                    .get(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .voplay_render_state_snapshot(caller)?;
                if let Some(snapshot) = snapshot {
                    let snapshot = retarget_browser_voplay_packet_epoch(snapshot, channel_epoch)?;
                    services
                        .publish_named_endpoint_payload(endpoint, owner.as_bytes(), &snapshot)
                        .map_err(|status| {
                            format!(
                                "publish Voplay retained render state snapshot to browser lane: status {status}"
                            )
                        })?;
                }
                let packets = host
                    .active_framework_providers
                    .get(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .voplay_render_asset_rebind_packets(caller)?;
                for packet in packets {
                    let packet = retarget_browser_voplay_packet_epoch(packet, channel_epoch)?;
                    services
                        .publish_named_endpoint_payload(endpoint, owner.as_bytes(), &packet)
                        .map_err(|status| {
                            format!(
                                "publish Voplay retained render asset to browser lane: status {status}"
                            )
                        })?;
                }
                host.active_framework_providers
                    .get_mut(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .prune_voplay_replayed_role_packets(
                        caller,
                        vo_app_runtime::ProviderRole::GameRenderer,
                    )?;
            }
            if role == vo_app_runtime::ProviderRole::GameAudio && channel_epoch > 1 {
                let packets = host
                    .active_framework_providers
                    .get(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .voplay_audio_asset_rebind_packets(caller)?;
                for packet in packets {
                    let packet = retarget_browser_voplay_packet_epoch(packet, channel_epoch)?;
                    services
                        .publish_named_endpoint_payload(endpoint, owner.as_bytes(), &packet)
                        .map_err(|status| {
                            format!(
                                "publish Voplay retained audio asset to browser lane: status {status}"
                            )
                        })?;
                }
                host.active_framework_providers
                    .get_mut(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .prune_voplay_replayed_role_packets(
                        caller,
                        vo_app_runtime::ProviderRole::GameAudio,
                    )?;
            }
            if role == vo_app_runtime::ProviderRole::GameLogic {
                host.active_framework_providers
                    .get_mut(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .replay_voplay_unobserved_control_commits(caller)?;
            }
        }
        if initialized_now {
            continue;
        }
        let channel_epoch = host
            .voplay_role_engine_epochs
            .get(&(
                module_key.to_owned(),
                caller.endpoint_index,
                caller.endpoint_generation,
                role_tag,
            ))
            .copied()
            .ok_or_else(|| String::from("browser Voplay role channel is not initialized"))?;
        if !has_packet {
            continue;
        }
        let packet = {
            let group = host
                .active_framework_providers
                .get_mut(module_key)
                .ok_or_else(|| String::from("browser Voplay provider disappeared"))?;
            match role {
                vo_app_runtime::ProviderRole::GameLogic => {
                    group.take_voplay_logic_packet(caller)?
                }
                vo_app_runtime::ProviderRole::GameAsset => {
                    group.take_voplay_asset_packet(caller)?
                }
                vo_app_runtime::ProviderRole::GameRenderer => {
                    group.take_voplay_render_packet(caller)?
                }
                vo_app_runtime::ProviderRole::GameAudio => {
                    group.take_voplay_audio_packet(caller)?
                }
                _ => unreachable!(),
            }
        };
        let Some(packet) = packet else {
            continue;
        };
        let routed_packet = retarget_browser_voplay_packet_epoch(packet.clone(), channel_epoch)?;
        match services.publish_named_endpoint_payload(endpoint, owner.as_bytes(), &routed_packet) {
            Ok(()) => {}
            Err(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_WOULD_BLOCK) => {
                host.active_framework_providers
                    .get_mut(module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .requeue_voplay_role_packet(caller, role, packet)?;
            }
            Err(status) => {
                return Err(format!(
                    "publish Voplay role {role:?} to browser lane: status {status}"
                ));
            }
        }
    }
    Ok(0)
}

fn encode_browser_voplay_render_feature_bootstrap(
    engine: (u32, u32),
    features: &[Vec<u8>],
) -> Result<Vec<u8>, String> {
    if features.len() > 4096 || features.iter().any(Vec::is_empty) {
        return Err(String::from(
            "browser Voplay RenderFeature bootstrap is invalid",
        ));
    }
    let capacity = features
        .iter()
        .try_fold(20_usize, |total, feature| {
            total.checked_add(4)?.checked_add(feature.len())
        })
        .filter(|bytes| *bytes <= vo_app_runtime::MAX_PACKET_BYTES)
        .ok_or_else(|| {
            String::from("browser Voplay RenderFeature bootstrap exceeds packet limit")
        })?;
    let mut bytes = Vec::with_capacity(capacity);
    bytes.extend_from_slice(b"VFRB2\0\0\0");
    bytes.extend_from_slice(&engine.0.to_le_bytes());
    bytes.extend_from_slice(&engine.1.to_le_bytes());
    bytes.extend_from_slice(&(features.len() as u32).to_le_bytes());
    for feature in features {
        bytes.extend_from_slice(&(feature.len() as u32).to_le_bytes());
        bytes.extend_from_slice(feature);
    }
    Ok(bytes)
}

fn encode_browser_voplay_engine_lifecycle_packet(
    kind: u16,
    engine: (u32, u32),
    channel_epoch: u64,
) -> Result<Vec<u8>, String> {
    if !matches!(kind, 12 | 14 | 15 | 16)
        || engine.1 == 0
        || engine.0 == u32::MAX
        || channel_epoch == 0
    {
        return Err(String::from(
            "browser Voplay Engine lifecycle packet is invalid",
        ));
    }
    let mut packet = vec![0_u8; 80];
    packet[0..2].copy_from_slice(&kind.to_le_bytes());
    packet[4..8].copy_from_slice(&engine.0.to_le_bytes());
    packet[8..12].copy_from_slice(&engine.1.to_le_bytes());
    packet[12..20].copy_from_slice(&channel_epoch.to_le_bytes());
    Ok(packet)
}

fn retarget_browser_voplay_packet_epoch(
    mut packet: Vec<u8>,
    channel_epoch: u64,
) -> Result<Vec<u8>, String> {
    if packet.len() < 80 || channel_epoch == 0 {
        return Err(String::from(
            "browser Voplay framework packet cannot be retargeted",
        ));
    }
    packet[12..20].copy_from_slice(&channel_epoch.to_le_bytes());
    Ok(packet)
}

fn retarget_browser_voplay_control_adoption(
    mut packet: Vec<u8>,
    channel_epoch: u64,
) -> Result<Vec<u8>, String> {
    packet = retarget_browser_voplay_packet_epoch(packet, channel_epoch)?;
    let kind = u16::from_le_bytes(packet[0..2].try_into().unwrap());
    if !matches!(kind, 6 | 8) {
        return Err(String::from(
            "browser Voplay control adoption source kind is invalid",
        ));
    }
    packet[0..2].copy_from_slice(&49_u16.to_le_bytes());
    Ok(packet)
}

fn canonicalize_browser_voplay_input(packet: &[u8]) -> Result<Vec<u8>, String> {
    if packet.len() < 160 {
        return Err(String::from(
            "browser Voplay platform input packet is truncated",
        ));
    }
    let framework_payload_bytes = u32::from_le_bytes(packet[76..80].try_into().unwrap()) as usize;
    if framework_payload_bytes != packet.len() - 80 {
        return Err(String::from(
            "browser Voplay platform input packet length is invalid",
        ));
    }
    let input = &packet[80..];
    let detail_bytes = u32::from_le_bytes(input[76..80].try_into().unwrap()) as usize;
    if input[0] != 1 || detail_bytes != input.len() - 80 || input[1] == 0 {
        return Err(String::from(
            "browser Voplay platform input payload is invalid",
        ));
    }
    let mut canonical = Vec::with_capacity(73 + detail_bytes);
    canonical.extend_from_slice(b"voplay-input-v1\0");
    canonical.extend_from_slice(&input[52..60]);
    canonical.extend_from_slice(&input[44..52]);
    canonical.extend_from_slice(&packet[4..12]);
    canonical.extend_from_slice(&input[36..44]);
    canonical.extend_from_slice(&input[60..68]);
    canonical.extend_from_slice(&u16::from(input[1]).to_le_bytes());
    canonical.extend_from_slice(&input[68..72]);
    canonical.extend_from_slice(&input[72..76]);
    canonical.extend_from_slice(&input[2..4]);
    canonical.push(0);
    canonical.extend_from_slice(&(detail_bytes as u32).to_le_bytes());
    canonical.extend_from_slice(&input[80..]);
    Ok(canonical)
}

fn collect_browser_voplay_lane_returns(
    host: &mut BrowserSessionHost,
    lane: &BrowserFrameworkLane,
) -> Result<(), String> {
    let (render, asset, audio, logic) = match lane.role {
        vo_app_runtime::ProviderRole::GameRenderer => (true, false, false, false),
        vo_app_runtime::ProviderRole::GameAsset => (false, true, false, false),
        vo_app_runtime::ProviderRole::GameAudio => (false, false, true, false),
        vo_app_runtime::ProviderRole::GameLogic => (false, false, false, true),
        _ => return Ok(()),
    };
    let endpoint = host
        .guest
        .host_caller()
        .ok_or_else(|| String::from("browser runtime has no hosted endpoint"))?;
    let services = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("browser runtime has no HostServices V2 owner"))?;
    let target_callers = host
        .entry_vms
        .values()
        .filter(|entry| {
            entry.framework == vo_app_runtime::EntryFramework::Voplay && entry.startup_bound
        })
        .map(|entry| entry.caller)
        .collect::<Vec<_>>();
    if target_callers.is_empty() {
        return Err(String::from(
            "browser Voplay return lane has no active target caller",
        ));
    }
    let mut returned =
        BTreeMap::<vo_runtime::host_services_v2::CallerEndpointHandle, Vec<Vec<u8>>>::new();
    while let Some(packet) = services
        .try_take_named_inbound_endpoint_packet(endpoint, lane.owner.as_bytes())
        .map_err(|status| format!("poll browser Voplay return lane: status {status}"))?
    {
        let (envelope, payload) = vo_app_runtime::decode_envelope(&packet.bytes)
            .map_err(|error| format!("decode browser Voplay return envelope: {error:?}"))?;
        if envelope.message_kind != vo_app_runtime::AppMessageKind::FrameworkPayload {
            return Err(String::from(
                "browser Voplay return used a non-framework message kind",
            ));
        }
        if render && (payload.starts_with(b"VHR3") || payload.starts_with(b"VHR1")) {
            if payload.starts_with(b"VHR1") && target_callers.len() != 1 {
                return Err(String::from(
                    "legacy browser Voplay host-render command is ambiguous across target callers",
                ));
            }
            if !host.render.push_game(payload.to_vec()) {
                return Err(String::from(
                    "browser Voplay host-render command queue exhausted",
                ));
            }
        } else if logic
            && payload
                .get(..2)
                .and_then(|kind| kind.try_into().ok())
                .map(u16::from_le_bytes)
                .is_some_and(|kind| matches!(kind, 6 | 8))
        {
            let kind = u16::from_le_bytes(payload[..2].try_into().unwrap());
            let target_role = if kind == 6 {
                vo_app_runtime::ProviderRole::GameRenderer
            } else {
                vo_app_runtime::ProviderRole::GameAudio
            };
            let engine_index = u32::from_le_bytes(payload[4..8].try_into().unwrap());
            let engine_generation = u32::from_le_bytes(payload[8..12].try_into().unwrap());
            let caller = target_callers
                .iter()
                .copied()
                .find(|caller| {
                    caller.endpoint_index == engine_index
                        && caller.endpoint_generation == engine_generation
                })
                .ok_or_else(|| {
                    String::from("browser Voplay control belongs to an unknown target engine")
                })?;
            host.active_framework_providers
                .get_mut(&lane.module_key)
                .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                .retain_voplay_control_snapshot(caller, target_role, payload)?;
            let matching = host
                .framework_lanes
                .values()
                .filter(|candidate| {
                    candidate.module_key == lane.module_key && candidate.role == target_role
                })
                .collect::<Vec<_>>();
            if matching.len() != 1 {
                return Err(format!(
                    "browser Voplay control forward has {} {target_role:?} lanes",
                    matching.len()
                ));
            }
            let role_tag = match target_role {
                vo_app_runtime::ProviderRole::GameRenderer => 2,
                vo_app_runtime::ProviderRole::GameAudio => 3,
                _ => unreachable!(),
            };
            let channel_epoch = host
                .voplay_role_engine_epochs
                .get(&(
                    lane.module_key.clone(),
                    caller.endpoint_index,
                    caller.endpoint_generation,
                    role_tag,
                ))
                .copied()
                .ok_or_else(|| {
                    String::from("browser Voplay control destination is not initialized")
                })?;
            let forwarded = retarget_browser_voplay_packet_epoch(payload.to_vec(), channel_epoch)?;
            services
                .publish_named_endpoint_payload(endpoint, matching[0].owner.as_bytes(), &forwarded)
                .map_err(|status| {
                    format!(
                        "publish browser Voplay control forward to {target_role:?}: status {status}"
                    )
                })?;
        } else {
            if payload.len() < 12 {
                return Err(String::from(
                    "browser Voplay role return lacks framework packet identity",
                ));
            }
            let engine_index = u32::from_le_bytes(payload[4..8].try_into().unwrap());
            let engine_generation = u32::from_le_bytes(payload[8..12].try_into().unwrap());
            let caller = target_callers
                .iter()
                .copied()
                .find(|caller| {
                    caller.endpoint_index == engine_index
                        && caller.endpoint_generation == engine_generation
                })
                .ok_or_else(|| {
                    String::from("browser Voplay role return belongs to an unknown target engine")
                })?;
            let kind = u16::from_le_bytes(payload[0..2].try_into().unwrap());
            if matches!(kind, 13 | 17) {
                continue;
            }
            if render && kind == 33 {
                let input = canonicalize_browser_voplay_input(payload)?;
                host.active_framework_providers
                    .get_mut(&lane.module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?
                    .enqueue_voplay_input_frames(caller, vec![input])?;
                continue;
            }
            if asset && kind != 22 {
                continue;
            }
            if logic && kind == 50 {
                continue;
            }
            if (render || audio)
                && payload
                    .get(..2)
                    .and_then(|kind| kind.try_into().ok())
                    .map(u16::from_le_bytes)
                    == Some(46)
            {
                let logic_lane = host.framework_lanes.values().find(|candidate| {
                    candidate.module_key == lane.module_key
                        && candidate.role == vo_app_runtime::ProviderRole::GameLogic
                });
                if let Some(logic_lane) = logic_lane {
                    let logic_epoch = host
                        .voplay_role_engine_epochs
                        .get(&(
                            lane.module_key.clone(),
                            caller.endpoint_index,
                            caller.endpoint_generation,
                            4,
                        ))
                        .copied()
                        .ok_or_else(|| {
                            String::from("browser Voplay GameLogic authority is not initialized")
                        })?;
                    let feedback =
                        retarget_browser_voplay_packet_epoch(payload.to_vec(), logic_epoch)?;
                    services
                        .publish_named_endpoint_payload(
                            endpoint,
                            logic_lane.owner.as_bytes(),
                            &feedback,
                        )
                        .map_err(|status| {
                            format!("publish browser Voplay realization feedback: status {status}")
                        })?;
                }
            }
            if logic {
                let group = host
                    .active_framework_providers
                    .get_mut(&lane.module_key)
                    .ok_or_else(|| String::from("browser Voplay provider disappeared"))?;
                match kind {
                    45 => group.retain_voplay_unobserved_control_commit(caller, payload)?,
                    48 => {
                        group.observe_voplay_control_commit(caller, payload)?;
                    }
                    _ => {}
                }
            }
            returned.entry(caller).or_default().push(payload.to_vec());
        }
    }
    if returned.is_empty() {
        return Ok(());
    }
    let group = host
        .active_framework_providers
        .get_mut(&lane.module_key)
        .ok_or_else(|| String::from("browser Voplay provider disappeared"))?;
    for (caller, packets) in returned {
        group.enqueue_voplay_returns(
            caller,
            if render { packets.clone() } else { Vec::new() },
            if asset { packets.clone() } else { Vec::new() },
            if audio { packets.clone() } else { Vec::new() },
            if logic { packets } else { Vec::new() },
        )?;
    }
    Ok(())
}

fn route_browser_vogui_lane_packets(
    host: &mut BrowserSessionHost,
    lane: &BrowserFrameworkLane,
) -> Result<(), String> {
    let target_role = match lane.role {
        vo_app_runtime::ProviderRole::UiLogic => vo_app_runtime::ProviderRole::UiRenderer,
        vo_app_runtime::ProviderRole::UiRenderer => vo_app_runtime::ProviderRole::UiLogic,
        _ => return Ok(()),
    };
    let matching = host
        .framework_lanes
        .values()
        .filter(|candidate| {
            candidate.module_key == lane.module_key && candidate.role == target_role
        })
        .collect::<Vec<_>>();
    if matching.len() != 1 {
        return Err(format!(
            "Vogui route {:?} -> {target_role:?} has {} target lanes",
            lane.role,
            matching.len()
        ));
    }
    let target_owner = matching[0].owner.clone();
    let endpoint = host
        .guest
        .host_caller()
        .ok_or_else(|| String::from("browser runtime has no hosted endpoint"))?;
    let services = host
        .guest
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("browser runtime has no HostServices V2 owner"))?;
    while let Some(packet) = services
        .try_take_named_inbound_endpoint_packet(endpoint, lane.owner.as_bytes())
        .map_err(|status| format!("poll browser Vogui provider lane: status {status}"))?
    {
        let (envelope, payload) = vo_app_runtime::decode_envelope(&packet.bytes)
            .map_err(|error| format!("decode browser Vogui provider packet: {error:?}"))?;
        if envelope.message_kind != vo_app_runtime::AppMessageKind::FrameworkPayload {
            return Err(String::from(
                "browser Vogui provider submitted a non-framework packet",
            ));
        }
        if lane.role == vo_app_runtime::ProviderRole::UiLogic {
            if let Some(turn) = decode_browser_provider_turn(payload)? {
                let mapper_id = i32::try_from(turn.mapper_id)
                    .map_err(|_| String::from("browser Vogui mapper identity exceeds i32"))?;
                enqueue_browser_vogui_target_turn(
                    host,
                    mapper_id,
                    turn.payload,
                    turn.source_root,
                    turn.source_view,
                    turn.event_sequence,
                    turn.event_revision,
                )
                .map_err(|error| error.as_string().unwrap_or_else(|| format!("{error:?}")))?;
                continue;
            }
            if payload.starts_with(b"vogui-host-effect-cancel-v1\0") {
                let callers = host
                    .entry_vms
                    .values()
                    .filter(|entry| {
                        entry.framework == vo_app_runtime::EntryFramework::Vogui
                            && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "browser Vogui effect cancellation has {} candidate callers",
                        callers.len()
                    ));
                }
                host.active_framework_providers
                    .get_mut(&lane.module_key)
                    .ok_or_else(|| String::from("browser Vogui provider group disappeared"))?
                    .apply_vogui_provider_effect_cancel(callers[0], payload)?;
                continue;
            }
            if payload.starts_with(b"vogui-host-effect-v1\0") {
                let callers = host
                    .entry_vms
                    .values()
                    .filter(|entry| {
                        entry.framework == vo_app_runtime::EntryFramework::Vogui
                            && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "browser Vogui effect has {} candidate callers",
                        callers.len()
                    ));
                }
                host.active_framework_providers
                    .get_mut(&lane.module_key)
                    .ok_or_else(|| String::from("browser Vogui provider group disappeared"))?
                    .enqueue_vogui_provider_effect(callers[0], payload.to_vec())?;
                continue;
            }
            if payload.starts_with(b"vogui-host-subscription-v1\0") {
                let callers = host
                    .entry_vms
                    .values()
                    .filter(|entry| {
                        entry.framework == vo_app_runtime::EntryFramework::Vogui
                            && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "browser Vogui subscription has {} candidate callers",
                        callers.len()
                    ));
                }
                host.active_framework_providers
                    .get_mut(&lane.module_key)
                    .ok_or_else(|| String::from("browser Vogui provider group disappeared"))?
                    .apply_vogui_provider_subscription(callers[0], payload)?;
                continue;
            }
        }
        services
            .publish_named_endpoint_payload(endpoint, target_owner.as_bytes(), payload)
            .map_err(|status| {
                format!(
                    "route browser Vogui {:?} packet to {target_role:?}: status {status}",
                    lane.role
                )
            })?;
    }
    Ok(())
}

#[wasm_bindgen(js_name = "loadFrameworkProvider")]
pub fn load_framework_provider(
    preview_index: u32,
    preview_generation: u32,
    module_key: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        if host.loaded_framework_providers.contains(module_key) {
            return Err(JsValue::from_str(
                "framework provider factory is already loaded",
            ));
        }
        let binding = host
            .framework_provider_bindings
            .get(module_key)
            .cloned()
            .ok_or_else(|| JsValue::from_str("framework provider is absent from resolved plan"))?;
        let mut loaded_templates = Vec::new();
        for provider in &binding.providers {
            if let Err(error) = host
                .guest
                .validate_loaded_provider_factory(provider.template_id, provider.loaded)
            {
                for template_id in loaded_templates.into_iter().rev() {
                    let _ = host.guest.unload_provider_factory(template_id);
                }
                return Err(JsValue::from_str(&error));
            }
            loaded_templates.push(provider.template_id);
        }
        host.loaded_framework_providers
            .insert(module_key.to_string());
        Ok(())
    })
}

#[wasm_bindgen(js_name = "unloadFrameworkProvider")]
pub fn unload_framework_provider(
    preview_index: u32,
    preview_generation: u32,
    module_key: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        if host.pending_framework_providers.contains_key(module_key)
            || host.active_framework_providers.contains_key(module_key)
        {
            return Err(JsValue::from_str(
                "framework provider factory is pinned by an instance group",
            ));
        }
        let binding = host
            .framework_provider_bindings
            .get(module_key)
            .ok_or_else(|| JsValue::from_str("framework provider is absent from resolved plan"))?;
        if !host.loaded_framework_providers.remove(module_key) {
            return Err(JsValue::from_str(
                "framework provider factory is not loaded",
            ));
        }
        let mut unloaded = Vec::new();
        for provider in binding.providers.iter().rev() {
            if let Err(error) = host.guest.unload_provider_factory(provider.template_id) {
                for provider in unloaded.into_iter().rev() {
                    if let Some(provider) = binding
                        .providers
                        .iter()
                        .find(|candidate| candidate.template_id == provider)
                    {
                        let _ = host.guest.validate_loaded_provider_factory(
                            provider.template_id,
                            provider.loaded,
                        );
                    }
                }
                host.loaded_framework_providers
                    .insert(module_key.to_string());
                return Err(JsValue::from_str(&error));
            }
            unloaded.push(provider.template_id);
        }
        Ok(())
    })
}

#[wasm_bindgen(js_name = "beginFrameworkProvider")]
pub fn begin_framework_provider(
    preview_index: u32,
    preview_generation: u32,
    module_key: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        if host.pending_framework_providers.contains_key(module_key)
            || host.active_framework_providers.contains_key(module_key)
        {
            return Err(JsValue::from_str(
                "framework provider is already pending or active",
            ));
        }
        if !host.loaded_framework_providers.contains(module_key) {
            return Err(JsValue::from_str(
                "framework provider factory has not been loaded",
            ));
        }
        let binding = host
            .framework_provider_bindings
            .get(module_key)
            .cloned()
            .ok_or_else(|| JsValue::from_str("framework provider is absent from resolved plan"))?;
        let pending = host
            .guest
            .begin_dynamic_instance_group(DynamicInstanceGroupPlan {
                instances: binding
                    .providers
                    .iter()
                    .map(|provider| InitialProviderInstancePlan {
                        template_id: provider.template_id,
                        capabilities: binding.capabilities.clone(),
                    })
                    .collect(),
            })
            .map_err(|error| JsValue::from_str(&error))?;
        let providers = pending.providers().to_vec();
        if providers.len() != binding.providers.len() {
            let _ = pending.rollback();
            return Err(JsValue::from_str(
                "framework provider group installed an incomplete role set",
            ));
        }
        let now = browser_monotonic_millis()?;
        for provider in providers {
            if let Err(error) = pending
                .prepare_provider(provider.instance, now)
                .and_then(|()| pending.start_provider(provider.instance, now))
            {
                let _ = pending.rollback();
                return Err(JsValue::from_str(&error));
            }
        }
        host.pending_framework_providers
            .insert(module_key.to_string(), pending);
        Ok(())
    })
}

#[wasm_bindgen(js_name = "readyFrameworkProvider")]
pub fn ready_framework_provider(
    preview_index: u32,
    preview_generation: u32,
    module_key: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let pending = host
            .pending_framework_providers
            .remove(module_key)
            .ok_or_else(|| JsValue::from_str("framework provider is not pending"))?;
        let providers = pending.providers().to_vec();
        if providers.is_empty() {
            let _ = pending.rollback();
            host.framework_lanes
                .retain(|_, lane| lane.module_key != module_key);
            return Err(JsValue::from_str(
                "framework provider group has no instances",
            ));
        }
        let now = browser_monotonic_millis()?;
        for provider in providers {
            if let Err(error) = pending.mark_provider_ready(provider.instance, now) {
                let _ = pending.rollback();
                host.framework_lanes
                    .retain(|_, lane| lane.module_key != module_key);
                return Err(JsValue::from_str(&error));
            }
        }
        let active = match pending.finalize() {
            Ok(active) => active,
            Err(error) => {
                host.framework_lanes
                    .retain(|_, lane| lane.module_key != module_key);
                return Err(JsValue::from_str(&error));
            }
        };
        host.active_framework_providers
            .insert(module_key.to_string(), active);
        Ok(())
    })
}

#[wasm_bindgen(js_name = "abortFrameworkProvider")]
pub fn abort_framework_provider(
    preview_index: u32,
    preview_generation: u32,
    module_key: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.framework_lanes
            .retain(|_, lane| lane.module_key != module_key);
        host.pending_framework_providers
            .remove(module_key)
            .ok_or_else(|| JsValue::from_str("framework provider is not pending"))
            .and_then(|group| {
                group
                    .rollback()
                    .map(|_| ())
                    .map_err(|error| JsValue::from_str(&error))
            })
    })
}

#[wasm_bindgen(js_name = "closeFrameworkProvider")]
pub fn close_framework_provider(
    preview_index: u32,
    preview_generation: u32,
    module_key: &str,
) -> Result<(), JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.framework_lanes
            .retain(|_, lane| lane.module_key != module_key);
        host.voplay_render_features_initialized
            .retain(|(module, _, _)| module != module_key);
        host.voplay_role_engines_initialized
            .retain(|(module, _, _, _)| module != module_key);
        let group = host
            .active_framework_providers
            .remove(module_key)
            .ok_or_else(|| JsValue::from_str("framework provider is not active"))?;
        group.close().map_err(|error| JsValue::from_str(&error))?;
        Ok(())
    })
}

#[wasm_bindgen(js_name = "openFrameworkLane")]
pub fn open_framework_lane(
    preview_index: u32,
    preview_generation: u32,
    owner: String,
) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let lane = resolve_browser_framework_lane(host, &owner)
            .map_err(|error| JsValue::from_str(&error))?;
        let binding = host
            .guest
            .open_host_framework_channel_for(
                &owner,
                vo_app_runtime::LaneLimits {
                    max_packet_bytes: vo_app_runtime::MAX_PACKET_BYTES as u32,
                    max_messages: 256,
                    max_bytes: 32 * 1024 * 1024,
                },
            )
            .map_err(|error| JsValue::from_str(&error))?;
        let module_key = lane.module_key.clone();
        host.framework_lanes.insert(
            (
                binding.channel.index,
                binding.channel.generation,
                binding.channel_epoch,
            ),
            lane,
        );
        if host.active_framework_providers.contains_key(&module_key)
            && browser_framework_module_matches(&module_key, vo_app_runtime::EntryFramework::Voplay)
        {
            let callers = host
                .active_framework_providers
                .get(&module_key)
                .ok_or_else(|| JsValue::from_str("browser Voplay provider disappeared"))?
                .voplay_target_callers();
            for caller in callers {
                dispatch_browser_voplay_outboxes(host, &module_key, caller)
                    .map_err(|error| JsValue::from_str(&error))?;
            }
        }
        Ok(endpoint_channel_binding_to_js(&binding).into())
    })
}

#[wasm_bindgen(js_name = "pollFrameworkLane")]
pub fn poll_framework_lane(
    preview_index: u32,
    preview_generation: u32,
    channel_index: u32,
    channel_generation: u32,
    channel_epoch: &str,
) -> Result<JsValue, JsValue> {
    let channel_epoch = channel_epoch
        .parse::<u64>()
        .map_err(|_| JsValue::from_str("channelEpoch must be an unsigned 64-bit decimal string"))?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let lane = host
            .framework_lanes
            .get(&(channel_index, channel_generation, channel_epoch))
            .cloned()
            .ok_or_else(|| JsValue::from_str("framework lane binding is not registered"))?;
        if browser_framework_module_matches(
            &lane.module_key,
            vo_app_runtime::EntryFramework::Voplay,
        ) {
            let callers = host
                .active_framework_providers
                .get(&lane.module_key)
                .ok_or_else(|| JsValue::from_str("browser Voplay provider disappeared"))?
                .voplay_target_callers();
            for caller in callers {
                dispatch_browser_voplay_outboxes(host, &lane.module_key, caller)
                    .map_err(|error| JsValue::from_str(&error))?;
            }
        }
        Ok(host
            .guest
            .take_host_endpoint_packet(
                vo_app_runtime::ChannelHandle {
                    index: channel_index,
                    generation: channel_generation,
                },
                channel_epoch,
            )
            .map_err(|error| JsValue::from_str(&error))?
            .map(|packet| js_sys::Uint8Array::from(packet.bytes.as_slice()).into())
            .unwrap_or(JsValue::NULL))
    })
}

#[wasm_bindgen(js_name = "submitFrameworkLane")]
pub fn submit_framework_lane(
    preview_index: u32,
    preview_generation: u32,
    channel_index: u32,
    channel_generation: u32,
    channel_epoch: &str,
    packet: &[u8],
) -> Result<(), JsValue> {
    let preview = preview_handle(preview_index, preview_generation);
    let channel_epoch = channel_epoch
        .parse::<u64>()
        .map_err(|_| JsValue::from_str("channelEpoch must be an unsigned 64-bit decimal string"))?;
    with_guest_mut(preview, |host| {
        host.guest
            .submit_host_endpoint_packet(
                vo_app_runtime::ChannelHandle {
                    index: channel_index,
                    generation: channel_generation,
                },
                channel_epoch,
                packet,
            )
            .map_err(|error| JsValue::from_str(&error))?;
        let lane = host
            .framework_lanes
            .get(&(channel_index, channel_generation, channel_epoch))
            .cloned()
            .ok_or_else(|| JsValue::from_str("framework lane binding is not registered"))?;
        collect_browser_voplay_lane_returns(host, &lane)
            .and_then(|()| route_browser_vogui_lane_packets(host, &lane))
            .map_err(|error| JsValue::from_str(&error))?;
        drive_browser_framework_clocks(host)?;
        schedule_browser_framework_clock_wake(preview, host)
    })
}

#[wasm_bindgen(js_name = "submitFrameworkLaneBatch")]
pub fn submit_framework_lane_batch(
    preview_index: u32,
    preview_generation: u32,
    channel_index: u32,
    channel_generation: u32,
    channel_epoch: &str,
    packet_batch: &[u8],
) -> Result<(), JsValue> {
    let preview = preview_handle(preview_index, preview_generation);
    let channel_epoch = channel_epoch
        .parse::<u64>()
        .map_err(|_| JsValue::from_str("channelEpoch must be an unsigned 64-bit decimal string"))?;
    if packet_batch.len() < 4 {
        return Err(JsValue::from_str(
            "framework lane packet batch is truncated",
        ));
    }
    let count = u32::from_le_bytes(packet_batch[..4].try_into().unwrap()) as usize;
    if count == 0 || count > 4096 {
        return Err(JsValue::from_str(
            "framework lane packet batch count is invalid",
        ));
    }
    let mut cursor = 4_usize;
    let mut packets = Vec::with_capacity(count);
    for _ in 0..count {
        let length_end = cursor
            .checked_add(4)
            .filter(|end| *end <= packet_batch.len())
            .ok_or_else(|| JsValue::from_str("framework lane packet batch is truncated"))?;
        let length =
            u32::from_le_bytes(packet_batch[cursor..length_end].try_into().unwrap()) as usize;
        cursor = length_end;
        let packet_end = cursor
            .checked_add(length)
            .filter(|end| *end <= packet_batch.len())
            .ok_or_else(|| JsValue::from_str("framework lane packet batch is truncated"))?;
        if length == 0 {
            return Err(JsValue::from_str(
                "framework lane packet batch contains an empty packet",
            ));
        }
        packets.push(packet_batch[cursor..packet_end].to_vec());
        cursor = packet_end;
    }
    if cursor != packet_batch.len() {
        return Err(JsValue::from_str(
            "framework lane packet batch has trailing bytes",
        ));
    }
    with_guest_mut(preview, |host| {
        host.guest
            .submit_host_endpoint_packet_batch(
                vo_app_runtime::ChannelHandle {
                    index: channel_index,
                    generation: channel_generation,
                },
                channel_epoch,
                &packets,
            )
            .map_err(|error| JsValue::from_str(&error))?;
        let lane = host
            .framework_lanes
            .get(&(channel_index, channel_generation, channel_epoch))
            .cloned()
            .ok_or_else(|| JsValue::from_str("framework lane binding is not registered"))?;
        collect_browser_voplay_lane_returns(host, &lane)
            .and_then(|()| route_browser_vogui_lane_packets(host, &lane))
            .map_err(|error| JsValue::from_str(&error))?;
        drive_browser_framework_clocks(host)?;
        schedule_browser_framework_clock_wake(preview, host)
    })
}

#[wasm_bindgen(js_name = "pollDisplayTimingRequest")]
pub fn poll_display_timing_request(
    preview_index: u32,
    preview_generation: u32,
) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let request = host
            .guest
            .take_host_display_timing_request()
            .map_err(|error| JsValue::from_str(&error))?;
        let Some(request) = request else {
            return Ok(JsValue::NULL);
        };
        let value = Object::new();
        let view = Object::new();
        Reflect::set(
            &view,
            &JsValue::from_str("index"),
            &JsValue::from_f64(f64::from(request.view.index)),
        )?;
        Reflect::set(
            &view,
            &JsValue::from_str("generation"),
            &JsValue::from_f64(f64::from(request.view.generation)),
        )?;
        Reflect::set(&value, &JsValue::from_str("view"), &view)?;
        Reflect::set(
            &value,
            &JsValue::from_str("requestSequence"),
            &JsValue::from_str(&request.request_sequence.to_string()),
        )?;
        Ok(value.into())
    })
}

#[wasm_bindgen(js_name = "submitDisplayPulse")]
pub fn submit_display_pulse(
    preview_index: u32,
    preview_generation: u32,
    view_index: u32,
    view_generation: u32,
    request_sequence: &str,
    observed_micros: &str,
    interval_micros: &str,
) -> Result<JsValue, JsValue> {
    let request_sequence = request_sequence.parse::<u64>().map_err(|_| {
        JsValue::from_str("requestSequence must be an unsigned 64-bit decimal string")
    })?;
    let observed_micros = observed_micros.parse::<u64>().map_err(|_| {
        JsValue::from_str("observedMicros must be an unsigned 64-bit decimal string")
    })?;
    let interval_micros = interval_micros.parse::<u64>().map_err(|_| {
        JsValue::from_str("intervalMicros must be an unsigned 64-bit decimal string")
    })?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let submission = host
            .guest
            .submit_host_display_pulse(
                vo_app_runtime::DisplayTimingRequest {
                    view: vo_app_runtime::ViewHandle {
                        index: view_index,
                        generation: view_generation,
                    },
                    request_sequence,
                },
                observed_micros,
                interval_micros,
            )
            .map_err(|error| JsValue::from_str(&error))?;
        let value = Object::new();
        Reflect::set(
            &value,
            &JsValue::from_str("emittedDomains"),
            &JsValue::from_f64(submission.emitted_domains as f64),
        )?;
        Ok(value.into())
    })
}

#[wasm_bindgen(js_name = "pollIslandData")]
pub fn poll_island_data(preview_index: u32, preview_generation: u32) -> Result<Vec<u8>, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        Ok(host.guest.poll_outbound_frame().unwrap_or_default())
    })
}

#[wasm_bindgen(js_name = "pollPendingHostEvent")]
pub fn poll_pending_host_event(
    preview_index: u32,
    preview_generation: u32,
) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        Ok(host
            .guest
            .poll_pending_host_event()
            .map(|event| pending_host_event_to_js(&event).into())
            .unwrap_or(JsValue::NULL))
    })
}

#[wasm_bindgen(js_name = "pollDiagnostic")]
pub fn poll_diagnostic(preview_index: u32, preview_generation: u32) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        Ok(host
            .guest
            .poll_diagnostic()
            .map_err(|error| JsValue::from_str(&error))?
            .map(|record| diagnostic_record_to_js(&record).into())
            .unwrap_or(JsValue::NULL))
    })
}

#[wasm_bindgen(js_name = "pollHostRequest")]
pub fn poll_host_request(preview_index: u32, preview_generation: u32) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        Ok(host
            .host_requests
            .pop_front()
            .map(|command| host_request_command_to_js(&command).into())
            .unwrap_or(JsValue::NULL))
    })
}

#[wasm_bindgen(js_name = "pollEntryLaunch")]
pub fn poll_entry_launch(preview_index: u32, preview_generation: u32) -> Result<JsValue, JsValue> {
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        Ok(host
            .entry_launches
            .pop_front()
            .map(|command| entry_launch_command_to_js(&command).into())
            .unwrap_or(JsValue::NULL))
    })
}

#[wasm_bindgen(js_name = "completeEntryLaunch")]
pub fn complete_entry_launch(
    preview_index: u32,
    preview_generation: u32,
    launch_id: &str,
    error: Option<String>,
) -> Result<(), JsValue> {
    let launch_id = launch_id
        .parse::<u64>()
        .map_err(|_| JsValue::from_str("launchId must be an unsigned 64-bit decimal string"))?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        match error {
            Some(message) => host.entry_supervisor.fail(launch_id, message.as_bytes()),
            None => host.entry_supervisor.mark_running(launch_id),
        }
        .map_err(|error| JsValue::from_str(&format!("complete browser entry launch: {error:?}")))?;
        finish_browser_entry_launches(host)
    })
}

#[wasm_bindgen(js_name = "completeHostRequest")]
pub fn complete_host_request(
    preview_index: u32,
    preview_generation: u32,
    request_id: &str,
    outcome: &str,
) -> Result<(), JsValue> {
    let request_id = request_id
        .parse::<u64>()
        .map_err(|_| JsValue::from_str("requestId must be an unsigned 64-bit decimal string"))?;
    let outcome = parse_request_outcome(outcome)?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        let caller = host
            .external_request_callers
            .remove(&request_id)
            .ok_or_else(|| JsValue::from_str("requestId has no pending browser host request"))?;
        finish_browser_host_request_for(host, caller, request_id, outcome, Vec::new())
    })
}

#[wasm_bindgen(js_name = "wakeHostEvent")]
pub fn wake_host_event(
    preview_index: u32,
    preview_generation: u32,
    key: &str,
) -> Result<(), JsValue> {
    let key = HostWaitKey::decode(key).map_err(|e| JsValue::from_str(&e))?;
    with_guest_mut(preview_handle(preview_index, preview_generation), |host| {
        host.guest
            .wake_host_event(key)
            .map_err(session_error_to_js)?;
        let step = host.guest.run_scheduled().map_err(session_error_to_js)?;
        run_gc_stress_guest_step(&mut host.guest);
        publish_guest_stdout(&host.guest, "guest", step.stdout.as_deref())?;
        if let Some(render_output) = step.render_output {
            host.render.push(render_output);
        }
        Ok(())
    })
}

/// Stop the running guest app (clears state).
#[wasm_bindgen(js_name = "stopGui")]
pub fn stop_gui(preview_index: u32, preview_generation: u32) -> Result<(), JsValue> {
    close_browser_host(preview_handle(preview_index, preview_generation))
}

// =============================================================================
// Host bridge exports for vox standalone WASM module
//
// The vox.wasm module (WasmHostBackend) calls window.voHost* JS globals which
// delegate to these functions.  This lets vox reuse the host's compiler and VM
// instead of bundling its own.
// =============================================================================

/// Compile a single .vo file (or directory with vo.mod) from VFS.
/// Returns serialised bytecode on success.
#[wasm_bindgen(js_name = "voHostCompileFile")]
pub fn vo_host_compile_file(path: &str) -> Result<Vec<u8>, JsValue> {
    let options = ProjectContextOptions::default();
    compile_from_vfs(path, &options).map_err(|e| JsValue::from_str(&e))
}

/// Compile a directory (entry = dir/main.vo) from VFS.
/// Returns serialised bytecode on success.
#[wasm_bindgen(js_name = "voHostCompileDir")]
pub fn vo_host_compile_dir(path: &str) -> Result<Vec<u8>, JsValue> {
    let options = ProjectContextOptions::default();
    compile_from_vfs(path, &options).map_err(|e| JsValue::from_str(&e))
}

/// Compile source code string. Returns serialised bytecode.
#[wasm_bindgen(js_name = "voHostCompileString")]
pub fn vo_host_compile_string(code: &str) -> Result<Vec<u8>, JsValue> {
    vo_web::compile_source_with_std_fs(code, "main.vo", vo_web::build_stdlib_fs())
        .map_err(|e| JsValue::from_str(&format!("compile error: {}", e)))
}

/// Type-check source code. Returns empty string on success, error message on failure.
#[wasm_bindgen(js_name = "voHostCompileCheck")]
pub fn vo_host_compile_check(code: &str) -> String {
    match vo_web::compile_source_with_std_fs(code, "main.vo", vo_web::build_stdlib_fs()) {
        Ok(_) => String::new(),
        Err(e) => e.to_string(),
    }
}

/// Run bytecode (VM mode).
#[wasm_bindgen(js_name = "voHostRunBytecode")]
pub fn vo_host_run_bytecode(bytecode: &[u8]) -> Result<(), JsValue> {
    vo_runtime::output::clear_output();
    let saved = vo_web::ext_bridge::save_extern_state();
    let result =
        vo_web::create_vm(bytecode, vo_web::ext_bridge::register_wasm_ext_bridges).map(|_| ());
    vo_web::ext_bridge::restore_extern_state(saved).map_err(|e| JsValue::from_str(&e))?;
    result.map_err(|e| JsValue::from_str(&e))
}

/// Run bytecode and capture stdout. Returns captured output.
#[wasm_bindgen(js_name = "voHostRunBytecodeCapture")]
pub fn vo_host_run_bytecode_capture(bytecode: &[u8]) -> Result<String, JsValue> {
    vo_runtime::output::clear_output();
    let saved = vo_web::ext_bridge::save_extern_state();
    let result = vo_web::create_vm(bytecode, vo_web::ext_bridge::register_wasm_ext_bridges);
    let captured = vo_web::take_output();
    vo_web::ext_bridge::restore_extern_state(saved).map_err(|e| JsValue::from_str(&e))?;
    match result {
        Ok(_) => Ok(captured),
        Err(e) => {
            if captured.trim().is_empty() {
                Err(JsValue::from_str(&e))
            } else {
                Err(JsValue::from_str(&format!(
                    "{}\nRuntime error: {}",
                    captured.trim_end(),
                    e
                )))
            }
        }
    }
}

#[cfg(test)]
fn empty_return_test_module(name: &str) -> vo_common_core::bytecode::Module {
    use vo_common_core::bytecode::{FunctionDef, InstructionMetadata, Module};
    use vo_common_core::instruction::{Instruction, Opcode};
    use vo_common_core::types::SlotType;

    let slot_types = Vec::<SlotType>::new();
    let code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    let mut module = Module::new(name.to_string());
    module.functions.push(FunctionDef {
        name: "main".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        gc_scan_slots: 0,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        code,
        instruction_metadata: vec![InstructionMetadata::None],
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
        slot_types,
    });
    module
}

#[cfg(all(test, target_arch = "wasm32"))]
mod cache_metadata_tests {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn compile_cache_metadata_rejects_valid_bytecode_substitution() {
        let fingerprint = "sha256:source-snapshot";
        let trusted = empty_return_test_module("trusted-cache-module")
            .serialize()
            .expect("serialize trusted cache fixture");
        let substituted = empty_return_test_module("substituted-cache-module")
            .serialize()
            .expect("serialize substituted cache fixture");
        decode_verified_module(&trusted, "trusted cache fixture")
            .expect("trusted fixture must be valid bytecode");
        decode_verified_module(&substituted, "substituted cache fixture")
            .expect("substituted fixture must be valid bytecode");
        let metadata = encode_vfs_compile_cache_metadata(fingerprint, &trusted);
        let expected_digest = parse_vfs_compile_cache_metadata(&metadata, fingerprint)
            .expect("canonical cache metadata");

        validate_vfs_compile_cache_module_binding(&expected_digest, &trusted)
            .expect("metadata must accept the bytecode it commits to");
        let error = validate_vfs_compile_cache_module_binding(&expected_digest, &substituted)
            .expect_err("a different module must never reuse the source fingerprint");
        assert!(error.contains("bytecode digest mismatch"), "{error}");
    }

    #[wasm_bindgen_test]
    fn compile_cache_metadata_rejects_source_fingerprint_reuse() {
        let metadata = encode_vfs_compile_cache_metadata("fingerprint-a", b"module");
        let error = parse_vfs_compile_cache_metadata(&metadata, "fingerprint-b")
            .expect_err("metadata belongs to exactly one source snapshot");
        assert!(error.contains("fingerprint does not match"), "{error}");
    }

    #[wasm_bindgen_test]
    fn compile_cache_lookup_requires_the_current_materialized_dependency_generation() {
        let locked = vo_module::schema::lockfile::LockedModule {
            path: vo_module::identity::ModulePath::parse("github.com/acme/lib").unwrap(),
            version: vo_module::version::ExactVersion::parse("0.2.0").unwrap(),
            origin: vo_module::schema::lockfile::LockOrigin::Registry,
            release: Some(vo_module::digest::Digest::from_sha256(b"release")),
            intent: None,
            selection: None,
        };

        let error = validate_materialized_modules_with_fs(&MemoryFs::new(), &[locked])
            .expect_err("a bytecode cache hit must not bypass a missing module-cache generation");
        assert!(error.contains("authorized graph"), "{error}");
        assert!(error.contains("github.com/acme/lib"), "{error}");
    }
}

#[cfg(all(test, target_arch = "wasm32"))]
mod tests {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn studio_serialized_module_gate_rejects_invalid_bytecode() {
        let invalid = vo_common_core::bytecode::Module::new("invalid-cache".to_string())
            .serialize()
            .expect("serialize invalid cache fixture");
        let err = decode_verified_module(&invalid, "Studio compile cache").unwrap_err();
        assert!(err.contains("invalid Studio compile cache bytecode"));

        let valid = empty_return_test_module("valid-cache")
            .serialize()
            .expect("serialize valid cache fixture");
        decode_verified_module(&valid, "Studio compile cache")
            .expect("valid serialized module verifies");
    }

    #[wasm_bindgen_test]
    fn studio_bytecode_gate_uses_canonical_size_boundary() {
        let max = vo_common_core::serialize::MAX_VOB_BYTES;
        assert!(validate_studio_bytecode_size(max, "Studio boundary").is_ok());
        assert!(validate_studio_bytecode_size(max + 1, "Studio boundary").is_err());
    }

    #[wasm_bindgen_test]
    fn single_file_external_imports_require_a_project() {
        let entry = SingleFileEntry {
            entry_clean: "main.vo".to_string(),
            content: "package main\n".to_string(),
            external_modules: vec!["github.com/acme/lib".to_string()],
            inline_mod: None,
        };

        let error = entry.validate_dependency_authority().unwrap_err();

        assert!(error.contains("single files support only the standard library"));
        assert!(error.contains("create a project with vo.mod"));
        assert!(error.contains("commit its generated vo.lock"));
    }

    #[wasm_bindgen_test]
    fn workspace_snapshot_copies_source_only_for_context_authorized_members() {
        let project = VfsPackageCopyPolicy::Project {
            include_workfile: true,
        };
        assert!(project.should_keep_file("main.vo"));
        assert!(project.should_keep_file("vo.mod"));
        assert!(project.should_keep_file("vo.lock"));
        assert!(project.should_keep_file("vo.work"));

        let member = VfsPackageCopyPolicy::WorkspaceMember;
        assert!(member.should_keep_file("src.vo"));
        assert!(member.should_keep_file("vo.mod"));
        assert!(!member.should_keep_file("vo.lock"));
        assert!(!member.should_keep_file("vo.work"));
        assert!(!member.should_keep_file("vo.release.json"));
        assert!(!member.should_keep_file("vo.tree.json"));

        let discovered = BTreeSet::from([
            "/workspace/lib-a".to_string(),
            "/workspace/lib-b".to_string(),
        ]);
        let authorized = select_authorized_workspace_source_roots(
            &discovered,
            [PathBuf::from("workspace/lib-b")],
        )
        .unwrap();
        assert_eq!(authorized, BTreeSet::from(["/workspace/lib-b".to_string()]));
        assert!(select_authorized_workspace_source_roots(
            &discovered,
            [PathBuf::from("workspace/unlocked")],
        )
        .is_err());
    }

    #[wasm_bindgen_test]
    fn single_file_project_plan_uses_the_ephemeral_identity_entry_point() {
        let lockless = vo_module::schema::modfile::ModFile::parse_ephemeral(
            "format = 1\nmodule = \"local/lockless\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        let lockless_content = lockless.render().unwrap();
        let deps = vo_module::project::read_inline_ephemeral_project_plan(&lockless_content, None)
            .unwrap();
        assert!(deps.lock_file().is_none());
        assert!(
            vo_module::project::read_inline_project_plan(&lockless_content, None)
                .unwrap()
                .lock_file()
                .is_none()
        );

        let error = vo_module::schema::modfile::ModFile::parse_ephemeral(
            "format = 1\nmodule = \"local/locked\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/acme/lib\" = \"0.2.0\"\n",
        )
        .unwrap_err();
        assert!(error.to_string().contains("unknown key 'dependencies'"));
    }

    #[wasm_bindgen_test]
    fn studio_package_budget_enforces_source_and_snapshot_boundaries() {
        let mut source_budget = VfsPackageReadBudget::default();
        source_budget
            .charge_kept_file("/main.vo", vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES, true)
            .unwrap();
        assert!(source_budget
            .charge_kept_file("/extra.vo", 1, true)
            .is_err());

        let mut snapshot_budget = VfsPackageReadBudget::default();
        snapshot_budget
            .charge_kept_file("/vo.lock", STUDIO_PACKAGE_SNAPSHOT_MAX_BYTES, false)
            .unwrap();
        assert!(snapshot_budget
            .charge_kept_file("/vo.mod", 1, false)
            .is_err());
    }

    #[wasm_bindgen_test]
    fn console_run_preserves_explicit_exit_code() {
        let source = r#"
            package main

            import (
                "fmt"
                "os"
            )

            func main() {
                fmt.Println("before")
                os.Exit(37)
                fmt.Println("after")
            }
        "#;
        let bytecode =
            vo_web::compile_source_with_std_fs(source, "main.vo", vo_web::build_stdlib_fs())
                .unwrap_or_else(|error| panic!("os.Exit fixture should compile: {error}"));

        let result = run_console_bytecode(&bytecode).expect("console run should terminate cleanly");

        assert_eq!(result.output, "before\n");
        assert_eq!(result.exit_code, 37);
    }

    #[wasm_bindgen_test]
    fn studio_vfs_compile_cache_epoch_rejects_pre_capability_validation_artifacts() {
        assert_eq!(STUDIO_VFS_COMPILE_CACHE_SCHEMA_VERSION, "5");
    }

    #[wasm_bindgen_test]
    fn browser_extension_protocol_uses_shared_canonical_identity() {
        use vo_common_core::extern_key::{decode_extern_name, deepest_owning_module, ExternKeyRef};

        let encoded = ExternKeyRef::new("github.com/acme/graphics/render/图形/V2", "绘制")
            .encode()
            .expect("canonical Unicode extern");
        let key = decode_extern_name(&encoded).expect("decode canonical Unicode extern");
        let owners = BTreeSet::from([
            "github.com/acme/graphics".to_string(),
            "github.com/acme/graphics/render".to_string(),
            "github.com/acme/graphics-vector".to_string(),
        ]);
        assert_eq!(
            deepest_owning_module(key, &owners),
            Some("github.com/acme/graphics/render")
        );
        assert!(!key.is_owned_by_module("github.com/acme/graphic"));

        for key in [
            ExternKeyRef::new("\u{feff}github.com/acme/graphics", "Draw"),
            ExternKeyRef::new("github.com/acme/graphics", "\u{feff}Draw"),
        ] {
            let encoded = key.encode().expect("BOM-bearing wire identity");
            assert_eq!(decode_extern_name(&encoded), Ok(key));
        }

        for package in [
            "github.com/acme/graphics/图形/é",
            "github.com/acme/graphics/Render/V2",
            "github.com/acme/graphics/数据.json",
        ] {
            assert!(
                ExternKeyRef::new(package, "绘制").is_owned_by_module("github.com/acme/graphics"),
                "portable descendant package lost ownership: {package:?}"
            );
        }

        for package in [
            "github.com/acme/graphics/../escape",
            "github.com/acme/graphics/./render",
            "github.com/acme/graphics//render",
            "github.com/acme/graphics/render/",
            "github.com/acme/graphics/render\\alias",
            "github.com/acme/graphics/render\0alias",
            "github.com/acme/graphics/pkg@v2",
            "github.com/acme/graphics/e\u{301}",
            "github.com/acme/graphics/COM¹.txt",
            "github.com/acme/graphics/trailing.",
            "github.com/acme/graphics/ leading",
            "github.com/acme/graphics/a:b",
        ] {
            assert!(
                !ExternKeyRef::new(package, "绘制").is_owned_by_module("github.com/acme/graphics")
            );
        }

        assert!(decode_extern_name("github_com_acme_graphics_Draw").is_err());
        assert!(decode_extern_name("vo1:01:x:1:F").is_err());
        assert_ne!(
            ExternKeyRef::new("x/a/b", "F").encode().unwrap(),
            ExternKeyRef::new("x/a_b", "F").encode().unwrap()
        );
    }
}
