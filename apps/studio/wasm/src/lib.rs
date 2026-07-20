//! Vo Studio WASM entry point.
//!
//! Exposes compile_run_entry / run_gui_entry / send_gui_event / stop_gui
//! to the Svelte frontend.
//!
//! The IDE UI is Svelte; this module compiles and runs user Vo code.
//! Source files are read from the JS VirtualFS (via vo_web_runtime_wasm::vfs).

use js_sys::{Object, Reflect};
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use vo_app_runtime::{
    GuestRuntime, PendingHostEvent, RenderBuffer, RenderIslandRuntime, SessionError, StepResult,
};
use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{FileSystem, MemoryFs};
use vo_module::project::ProjectContextOptions;
use vo_module::workspace::WorkspaceDiscovery;
use vo_vm::scheduler::HostWaitKey;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsValue;

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

/// Result of compiling and running a console entry in Studio.
///
/// A completed program reports `exit_code == 0`; an explicit `os.Exit` keeps
/// the exact VM exit code so the Studio frontend can surface process status.
#[wasm_bindgen]
pub struct StudioRunResult {
    output: String,
    exit_code: i32,
}

#[wasm_bindgen]
impl StudioRunResult {
    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }

    #[wasm_bindgen(getter, js_name = "exitCode")]
    pub fn exit_code(&self) -> i32 {
        self.exit_code
    }
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
    static GUEST: RefCell<Option<GuestRuntime>> = const { RefCell::new(None) };
    static GUI_RENDER: RefCell<RenderBuffer> = RefCell::new(RenderBuffer::new());
    static GC_STRESS_EVERY_STEP: Cell<bool> = const { Cell::new(false) };
    static GC_STRESS_HOST_STEP: Cell<bool> = const { Cell::new(false) };
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
    f: impl FnOnce(&mut GuestRuntime) -> Result<T, JsValue>,
) -> Result<T, JsValue> {
    let mut guest = GUEST
        .with(|g| g.borrow_mut().take())
        .ok_or_else(|| JsValue::from_str("No guest app running"))?;
    let result = f(&mut guest);
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    result
}

fn load_gui_app_from_bytecode(bytecode: &[u8]) -> Result<GuestRuntime, JsValue> {
    let mut vm = vo_web::create_loaded_vm(bytecode, vo_web::ext_bridge::register_wasm_ext_bridges)
        .map_err(|e| JsValue::from_str(&e))?;
    apply_gc_stress_config(&mut vm);
    Ok(GuestRuntime::new_gui_app(vm, guest_stdout_source()))
}

fn load_render_island_from_bytecode(bytecode: &[u8]) -> Result<GuestRuntime, JsValue> {
    let mut vm = vo_web::create_loaded_vm(bytecode, vo_web::ext_bridge::register_wasm_ext_bridges)
        .map_err(|e| JsValue::from_str(&e))?;
    apply_gc_stress_config(&mut vm);
    Ok(GuestRuntime::new_render_island(vm, guest_stdout_source()))
}

fn clear_gui_state() {
    GUEST.with(|g| *g.borrow_mut() = None);
    GUI_RENDER.with(|r| {
        r.borrow_mut().poll();
    });
}

fn take_guest_step_render(step: StepResult) -> Vec<u8> {
    flush_stdout("guest", step.stdout.as_deref());
    step.render_output.unwrap_or_default()
}

fn start_gui_from_bytecode_with<F>(
    bytecode: &[u8],
    path_label: &str,
    start_guest: F,
) -> Result<Vec<u8>, JsValue>
where
    F: FnOnce(&mut GuestRuntime) -> Result<StepResult, SessionError>,
{
    ensure_panic_hook();
    clear_gui_state();
    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(vec![path_label.to_string()]);
    });
    let result = (|| {
        let load_start = js_sys::Date::now();
        let mut guest = load_gui_app_from_bytecode(bytecode)?;
        log_wasm_path("gui_load_vm_done", path_label, "system", Some(load_start));
        let start_start = js_sys::Date::now();
        let step = start_guest(&mut guest).map_err(session_error_to_js)?;
        run_gc_stress_guest_step(&mut guest);
        log_wasm_path("gui_start_done", path_label, "system", Some(start_start));
        let render_output = take_guest_step_render(step);
        GUEST.with(|g| *g.borrow_mut() = Some(guest));
        Ok(render_output)
    })();
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

    /// Push an island transport frame into the VM command queue (does not run the VM).
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
// preloadExtModule is provided by vo-web (3-param version with optional jsGlueUrl).
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

#[wasm_bindgen(js_name = "voVersion")]
pub fn vo_version() -> String {
    vo_module::TOOLCHAIN_VERSION.to_string()
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
const STUDIO_VFS_COMPILE_CACHE_SCHEMA_VERSION: &str = "4";
const STUDIO_VFS_COMPILE_CACHE_SLOT_NAMESPACE: &str = "studio-vfs-compile-cache-slot";
const STUDIO_VFS_COMPILE_CACHE_NAMESPACE: &str = "studio-vfs-compile-cache";

struct ResolvedVfsCompileTarget {
    entry_path: String,
    project_root: Option<String>,
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
    name: String,
    entry: Option<String>,
    capabilities: Vec<String>,
    js_modules: BTreeMap<String, String>,
}

type WasmExtensionCompileSpec = vo_web::ReadyWasmExtensionBytes;

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

fn target_locked_modules(
    target: &ResolvedVfsCompileTarget,
    options: &ProjectContextOptions,
) -> Result<Vec<vo_module::schema::lockfile::LockedModule>, String> {
    if let Some(project_root) = &target.project_root {
        let (_, context) = build_workspace_project_from_vfs(project_root, options)?;
        return Ok(context.project_plan().locked_modules().to_vec());
    }
    let single_file = SingleFileEntry::load(target)?;
    single_file.validate_dependency_authority()?;
    Ok(Vec::new())
}

fn browser_runtime_plan_for_context(
    context: &vo_module::project::ProjectContext,
) -> Result<vo_web::BrowserRuntimePlan, String> {
    let mut plans = Vec::new();
    for local_dir in context.workspace_sources().values() {
        let local_root = vfs_path_from_fs_path(local_dir);
        plans.push(vo_web::debug_local_project_browser_runtime_plan_from_vfs(
            &local_root,
        )?);
    }
    plans.push(vo_web::published_browser_runtime_plan_from_vfs(
        context.project_plan().locked_modules(),
        "",
    )?);
    vo_web::merge_browser_runtime_plans(plans)
}

fn browser_runtime_plan_for_target(
    target: &ResolvedVfsCompileTarget,
    options: &ProjectContextOptions,
) -> Result<vo_web::BrowserRuntimePlan, String> {
    if let Some(project_root) = &target.project_root {
        let (_, context) = build_workspace_project_from_vfs(project_root, options)?;
        return browser_runtime_plan_for_context(&context);
    }
    let locked_modules = target_locked_modules(target, options)?;
    vo_web::published_browser_runtime_plan_from_vfs(&locked_modules, "")
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

async fn load_ready_wasm_extensions_for_studio(
    ready: &[vo_module::readiness::ReadyModule],
) -> Result<(), String> {
    vo_web::load_ready_wasm_extensions_from_vfs(ready)
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

fn build_compile_fs_from_vfs(
    entry_path: &str,
    options: &ProjectContextOptions,
) -> Result<
    (
        ResolvedVfsCompileTarget,
        MemoryFs,
        VfsCompileAuthority,
        Vec<vo_module::schema::lockfile::LockedModule>,
    ),
    String,
> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let (local_fs, authority, locked_modules) = if let Some(project_root) = &target.project_root {
        let (local_fs, context) = build_workspace_project_from_vfs(project_root, options)?;
        let locked_modules = context.project_plan().locked_modules().to_vec();
        (local_fs, VfsCompileAuthority::Project, locked_modules)
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
        (local_fs, authority, Vec::new())
    };

    Ok((target, local_fs, authority, locked_modules))
}

fn validate_materialized_modules_with_fs<F: FileSystem>(
    module_fs: &F,
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<(), String> {
    vo_module::readiness::check_materialized_modules_readiness(
        module_fs,
        locked_modules,
        WASM_INSTALL_TARGET,
    )
    .map(|_| ())
    .map_err(|error| format!("Studio module cache does not match the authorized graph: {error}"))
}

fn validate_vfs_materialized_modules(
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<(), String> {
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
        name: contract.name,
        entry: contract.entry,
        capabilities: contract.capabilities,
        js_modules: contract.js_modules,
    }
}

fn build_wasm_extension_compile_specs(
    plan: &vo_web::BrowserRuntimePlan,
) -> Result<Vec<WasmExtensionCompileSpec>, String> {
    vo_web::collect_browser_wasm_extensions_from_vfs(&plan.wasm_extensions)
        .map_err(|error| error.to_string())
}

fn collect_render_island_snapshot(
    entry_path: &str,
    options: &ProjectContextOptions,
) -> Result<JsValue, String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let root_path = target
        .project_root
        .clone()
        .unwrap_or_else(|| vfs_parent_dir(&target.entry_path).unwrap_or_else(|| "/".to_string()));
    let plan = browser_runtime_plan_for_target(&target, options)?;
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
    Ok(render_island_snapshot_to_js(&root_path, files))
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

fn gui_run_output_to_js(
    render_bytes: Vec<u8>,
    module_bytes: Vec<u8>,
    entry_path: &str,
    framework: Option<&FrameworkContract>,
    provider_frameworks: &[FrameworkContract],
) -> JsValue {
    let obj = Object::new();
    let render = js_sys::Uint8Array::from(render_bytes.as_slice());
    let module = js_sys::Uint8Array::from(module_bytes.as_slice());
    let _ = Reflect::set(&obj, &JsValue::from_str("renderBytes"), &render);
    let _ = Reflect::set(&obj, &JsValue::from_str("moduleBytes"), &module);
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("entryPath"),
        &JsValue::from_str(entry_path),
    );
    let framework_value = framework
        .map(framework_contract_to_js)
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("framework"), &framework_value);
    let provider_frameworks_value = js_sys::Array::new();
    for provider in provider_frameworks {
        provider_frameworks_value.push(&framework_contract_to_js(provider));
    }
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("providerFrameworks"),
        &provider_frameworks_value,
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("hostWidgetHandlerId"),
        &JsValue::NULL,
    );
    obj.into()
}

fn compile_from_vfs(entry_path: &str, options: &ProjectContextOptions) -> Result<Vec<u8>, String> {
    let (target, local_fs, authority, locked_modules) =
        build_compile_fs_from_vfs(entry_path, options)?;
    // Cache hits are executable compiler outputs, so they carry the same
    // authenticated dependency-readiness precondition as cache misses. This
    // check binds the current VFS release/package/source/artifact closure to
    // the materialized subset selected by ProjectContext before any bytecode
    // cache lookup can bypass that boundary.
    validate_vfs_materialized_modules(&locked_modules)?;
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
            vo_web::compile_entry_with_vfs_with_options(
                entry_clean,
                local_fs,
                VFS_MOD_ROOT,
                options,
            )
        }
    }
    .map_err(|e| format!("compile error: {}", e))?;
    save_vfs_compile_cache(&cache_slot, &fingerprint, &bytecode)?;
    log_wasm_path("compile_cache_store", &target.entry_path, "system", None);
    Ok(bytecode)
}

fn diagnostic_error_to_js(file: &str, category: &str, message: &str) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(&obj, &JsValue::from_str("file"), &JsValue::from_str(file));
    let _ = Reflect::set(&obj, &JsValue::from_str("line"), &JsValue::from_f64(0.0));
    let _ = Reflect::set(&obj, &JsValue::from_str("column"), &JsValue::from_f64(0.0));
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("message"),
        &JsValue::from_str(message),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("category"),
        &JsValue::from_str(category),
    );
    let _ = Reflect::set(&obj, &JsValue::from_str("moduleStage"), &JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("moduleKind"), &JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("modulePath"), &JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("moduleVersion"), &JsValue::NULL);
    obj.into()
}

fn compiler_result_to_js(ok: bool, errors: js_sys::Array, bytecode: Option<&[u8]>) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(&obj, &JsValue::from_str("ok"), &JsValue::from_bool(ok));
    let _ = Reflect::set(&obj, &JsValue::from_str("errors"), &errors);
    let bytecode_value = bytecode
        .map(|bytes| js_sys::Uint8Array::from(bytes).into())
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("bytecode"), &bytecode_value);
    obj.into()
}

fn compiler_success_to_js(bytecode: Option<&[u8]>) -> JsValue {
    compiler_result_to_js(true, js_sys::Array::new(), bytecode)
}

fn compiler_error_to_js(entry_path: &str, category: &str, message: String) -> JsValue {
    let errors = js_sys::Array::new();
    errors.push(&diagnostic_error_to_js(entry_path, category, &message));
    compiler_result_to_js(false, errors, None)
}

struct GuiCompileOutput {
    target: ResolvedVfsCompileTarget,
    bytecode: Vec<u8>,
    framework: Option<FrameworkContract>,
    provider_frameworks: Vec<FrameworkContract>,
    wasm_extensions: Vec<WasmExtensionCompileSpec>,
}

fn compile_gui_run_output(
    entry_path: &str,
    options: &ProjectContextOptions,
) -> Result<GuiCompileOutput, String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let bytecode = compile_from_vfs(entry_path, options)?;
    let plan = browser_runtime_plan_for_target(&target, options)?;
    let split = plan.primary_framework_split();
    let wasm_extensions = build_wasm_extension_compile_specs(&plan)?;
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
        wasm_extensions,
    })
}

fn framework_contract_to_js(contract: &FrameworkContract) -> JsValue {
    let obj = Object::new();
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
    let capabilities = js_sys::Array::new();
    for capability in &contract.capabilities {
        capabilities.push(&JsValue::from_str(capability));
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("capabilities"), &capabilities);
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

fn wasm_extension_compile_spec_to_js(spec: &WasmExtensionCompileSpec) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("name"),
        &JsValue::from_str(&spec.name),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("moduleKey"),
        &JsValue::from_str(&spec.module_key),
    );
    let wasm_bytes = js_sys::Uint8Array::from(spec.wasm_bytes.as_slice());
    let _ = Reflect::set(&obj, &JsValue::from_str("wasmBytes"), &wasm_bytes);
    let js_glue_value = spec
        .js_glue_bytes
        .as_ref()
        .map(|bytes| js_sys::Uint8Array::from(bytes.as_slice()).into())
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("jsGlueBytes"), &js_glue_value);
    obj.into()
}

#[wasm_bindgen(js_name = "prepareEntry")]
pub fn prepare_entry(entry_path: &str, workspace_discovery: &str) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    let workspace_discovery = workspace_discovery.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let total_start = js_sys::Date::now();
        let options = project_context_options_from_workspace_discovery(&workspace_discovery)
            .map_err(|e| JsValue::from_str(&e))?;
        let target = resolve_vfs_compile_target(&entry_path).map_err(|e| JsValue::from_str(&e))?;

        if let Some(project_root) = &target.project_root {
            let read_start = js_sys::Date::now();
            let (_local_fs, context) = build_workspace_project_from_vfs(project_root, &options)
                .map_err(|e| JsValue::from_str(&e))?;
            log_wasm_path(
                "prepare_entry_read_package_done",
                project_root,
                "system",
                Some(read_start),
            );
            let deps_start = js_sys::Date::now();
            let ready = if context.project_plan().has_mod_file() {
                ensure_project_plan_for_studio(context.project_plan())
                    .await
                    .map_err(|e| JsValue::from_str(&e))?
            } else {
                Vec::new()
            };
            if !ready.is_empty() {
                load_ready_wasm_extensions_for_studio(&ready)
                    .await
                    .map_err(|e| JsValue::from_str(&e))?;
            }
            log_prepare_entry_resolve_install_done(
                context
                    .project_plan()
                    .locked_modules()
                    .iter()
                    .map(|module| module.path.as_str()),
            );
            log_wasm_path(
                "prepare_entry_ensure_deps_done",
                &target.entry_path,
                "system",
                Some(deps_start),
            );
        } else {
            let single_file_start = js_sys::Date::now();
            let single_file = SingleFileEntry::load(&target).map_err(|e| JsValue::from_str(&e))?;
            log_wasm_path(
                "prepare_entry_load_single_file_done",
                &target.entry_path,
                "system",
                Some(single_file_start),
            );
            single_file
                .validate_dependency_authority()
                .map_err(|e| JsValue::from_str(&e))?;
        }

        log_wasm_path(
            "prepare_entry_done",
            &target.entry_path,
            "system",
            Some(total_start),
        );

        Ok(JsValue::NULL)
    })
}

#[wasm_bindgen(js_name = "compileRunEntry")]
pub fn compile_run_entry(
    entry_path: &str,
    workspace_discovery: &str,
) -> Result<StudioRunResult, JsValue> {
    ensure_panic_hook();
    let options = project_context_options_from_workspace_discovery(workspace_discovery)
        .map_err(|e| JsValue::from_str(&e))?;
    let bytecode = compile_from_vfs(entry_path, &options).map_err(|e| JsValue::from_str(&e))?;
    run_console_bytecode(&bytecode).map_err(|e| JsValue::from_str(&e))
}

fn run_console_bytecode(bytecode: &[u8]) -> Result<StudioRunResult, String> {
    vo_web::take_output();

    let saved = vo_web::ext_bridge::save_extern_state();
    let run_result = vo_web::create_vm(bytecode, vo_web::ext_bridge::register_wasm_ext_bridges);
    vo_web::ext_bridge::restore_extern_state(saved)?;
    let vm = run_result?;

    Ok(StudioRunResult {
        output: vo_web::take_output(),
        exit_code: vm.exit_code().unwrap_or(0),
    })
}

///
/// The Vo app's `Run()` does initial render then blocks on `waitForEvent()`.
/// `vm.run()` returns `SuspendedForHostEvents` once the main fiber blocks.
#[wasm_bindgen(js_name = "runGuiEntry")]
pub fn run_gui_entry(entry_path: &str) -> Result<Vec<u8>, JsValue> {
    let options = ProjectContextOptions::default();
    let GuiCompileOutput {
        target, bytecode, ..
    } = compile_gui_run_output(entry_path, &options).map_err(|e| JsValue::from_str(&e))?;
    start_gui_from_bytecode_with(&bytecode, &target.entry_path, |guest| guest.start_gui_app())
}

#[wasm_bindgen(js_name = "runGui")]
pub fn run_gui(entry_path: &str) -> Result<JsValue, JsValue> {
    let total_start = js_sys::Date::now();
    let compile_start = js_sys::Date::now();
    let options = ProjectContextOptions::default();
    let GuiCompileOutput {
        target,
        bytecode,
        framework,
        provider_frameworks,
        wasm_extensions: _,
    } = compile_gui_run_output(entry_path, &options).map_err(|e| JsValue::from_str(&e))?;
    log_wasm_path(
        "gui_compile_done",
        &target.entry_path,
        "system",
        Some(compile_start),
    );
    let render_bytes =
        start_gui_from_bytecode_with(&bytecode, &target.entry_path, |guest| guest.start_gui_app())?;
    log_wasm_path(
        "gui_total_done",
        &target.entry_path,
        "system",
        Some(total_start),
    );
    Ok(gui_run_output_to_js(
        render_bytes,
        bytecode,
        &target.entry_path,
        framework.as_ref(),
        &provider_frameworks,
    ))
}

#[wasm_bindgen(js_name = "checkEntry")]
pub fn check_entry(entry_path: &str, workspace_discovery: &str) -> Result<JsValue, JsValue> {
    ensure_panic_hook();
    let options = project_context_options_from_workspace_discovery(workspace_discovery)
        .map_err(|e| JsValue::from_str(&e))?;
    match compile_from_vfs(entry_path, &options) {
        Ok(_) => Ok(compiler_success_to_js(None)),
        Err(error) => Ok(compiler_error_to_js(entry_path, "compile", error)),
    }
}

#[wasm_bindgen(js_name = "compileEntry")]
pub fn compile_entry(entry_path: &str, workspace_discovery: &str) -> Result<JsValue, JsValue> {
    ensure_panic_hook();
    let options = project_context_options_from_workspace_discovery(workspace_discovery)
        .map_err(|e| JsValue::from_str(&e))?;
    match compile_from_vfs(entry_path, &options) {
        Ok(bytecode) => Ok(compiler_success_to_js(Some(&bytecode))),
        Err(error) => Ok(compiler_error_to_js(entry_path, "compile", error)),
    }
}

#[wasm_bindgen(js_name = "dumpEntry")]
pub fn dump_entry(entry_path: &str, workspace_discovery: &str) -> Result<String, JsValue> {
    ensure_panic_hook();
    let options = project_context_options_from_workspace_discovery(workspace_discovery)
        .map_err(|e| JsValue::from_str(&e))?;
    let bytecode = compile_from_vfs(entry_path, &options).map_err(|e| JsValue::from_str(&e))?;
    let module =
        decode_verified_module(&bytecode, "Studio dump").map_err(|e| JsValue::from_str(&e))?;
    Ok(bytecode_text_format::format_text(&module))
}

#[wasm_bindgen(js_name = "dumpGuiEntry")]
pub fn dump_gui_entry(entry_path: &str, workspace_discovery: &str) -> Result<String, JsValue> {
    ensure_panic_hook();
    let options = project_context_options_from_workspace_discovery(workspace_discovery)
        .map_err(|e| JsValue::from_str(&e))?;
    let GuiCompileOutput { bytecode, .. } =
        compile_gui_run_output(entry_path, &options).map_err(|e| JsValue::from_str(&e))?;
    let module =
        decode_verified_module(&bytecode, "Studio GUI dump").map_err(|e| JsValue::from_str(&e))?;
    Ok(bytecode_text_format::format_text(&module))
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
/// Intended for the web backend unified compile path: call prepareEntry first, then compileGui,
/// then use the shared post-compile pipeline (preload exts, load host bridge, runGuiFromBytecode).
#[wasm_bindgen(js_name = "compileGui")]
pub fn compile_gui(entry_path: &str, workspace_discovery: &str) -> Result<JsValue, JsValue> {
    let compile_start = js_sys::Date::now();
    let options = project_context_options_from_workspace_discovery(workspace_discovery)
        .map_err(|e| JsValue::from_str(&e))?;
    let GuiCompileOutput {
        target,
        bytecode,
        framework,
        provider_frameworks,
        wasm_extensions,
    } = compile_gui_run_output(entry_path, &options).map_err(|e| JsValue::from_str(&e))?;
    log_wasm_path(
        "gui_compile_done",
        &target.entry_path,
        "system",
        Some(compile_start),
    );
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
    let wasm_extensions_value = js_sys::Array::new();
    for spec in &wasm_extensions {
        wasm_extensions_value.push(&wasm_extension_compile_spec_to_js(spec));
    }
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("wasmExtensions"),
        &wasm_extensions_value,
    );
    Ok(obj.into())
}

/// Run a GUI app from pre-compiled bytecode (compiled by the native Rust backend via cmd_compile_gui).
/// Returns the initial render bytes. Framework metadata is provided separately by the caller.
#[wasm_bindgen(js_name = "runGuiFromBytecode")]
pub fn run_gui_from_bytecode(bytecode: &[u8]) -> Result<Vec<u8>, JsValue> {
    start_gui_from_bytecode_with(bytecode, "native-bytecode", |guest| guest.start_gui_app())
}

/// Run a GUI app from pre-compiled bytecode (compiled by the native Rust backend via cmd_compile_gui).
/// Returns the initial render bytes. Framework metadata is provided separately by the caller.
#[wasm_bindgen(js_name = "startGuiFromBytecode")]
pub fn start_gui_from_bytecode(
    bytecode: &[u8],
    entry_path: Option<String>,
) -> Result<Vec<u8>, JsValue> {
    let path_label = entry_path.as_deref().unwrap_or("native-bytecode");
    start_gui_from_bytecode_with(bytecode, path_label, |guest| guest.start_gui_app_step())
}

/// Send an event to the running guest app, returning the new render bytes.
///
/// Stores event data and wakes the main fiber (blocked on waitForEvent).
/// The fiber processes the event inline and blocks again on waitForEvent.
/// No new fiber is created — zero allocation per event.
#[wasm_bindgen(js_name = "sendGuiEvent")]
pub fn send_gui_event(handler_id: i32, payload: &str) -> Result<Vec<u8>, JsValue> {
    GUI_RENDER.with(|r| {
        r.borrow_mut().poll();
    });
    with_guest_mut(|guest| {
        let step = guest
            .dispatch_gui_event(handler_id, payload)
            .map_err(session_error_to_js)?;
        run_gc_stress_guest_step(guest);
        flush_stdout("guest", step.stdout.as_deref());
        Ok(step.render_output.unwrap_or_default())
    })
}

#[wasm_bindgen(js_name = "sendGuiEventAsync")]
pub fn send_gui_event_async(handler_id: i32, payload: &str) -> Result<(), JsValue> {
    with_guest_mut(|guest| {
        let step = guest
            .try_dispatch_gui_event(handler_id, payload)
            .map_err(session_error_to_js)?;
        if let Some(step) = step {
            run_gc_stress_guest_step(guest);
            flush_stdout("guest", step.stdout.as_deref());
            if let Some(render_output) = step.render_output {
                GUI_RENDER.with(|r| r.borrow_mut().push(render_output));
            }
        }
        Ok(())
    })
}

#[wasm_bindgen(js_name = "setGcStressEveryStep")]
pub fn set_gc_stress_every_step(enabled: bool) {
    GC_STRESS_EVERY_STEP.with(|cell| cell.set(enabled));
    GUEST.with(|g| {
        if let Some(guest) = g.borrow_mut().as_mut() {
            guest.set_gc_stress_every_step(enabled);
        }
    });
}

#[wasm_bindgen(js_name = "setGcStressHostStep")]
pub fn set_gc_stress_host_step(enabled: bool) {
    GC_STRESS_HOST_STEP.with(|cell| cell.set(enabled));
}

#[wasm_bindgen(js_name = "startRenderIsland")]
pub fn start_render_island(bytecode: &[u8]) -> Result<(), JsValue> {
    ensure_panic_hook();
    GUEST.with(|g| *g.borrow_mut() = None);
    let guest = load_render_island_from_bytecode(bytecode)?;
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(())
}

#[wasm_bindgen(js_name = "pushIslandData")]
pub fn push_island_data(data: &[u8]) -> Result<(), JsValue> {
    with_guest_mut(|guest| {
        let step = guest.push_island_frame(data).map_err(session_error_to_js)?;
        run_gc_stress_guest_step(guest);
        flush_stdout("guest", step.stdout.as_deref());
        if let Some(render_output) = step.render_output {
            GUI_RENDER.with(|r| r.borrow_mut().push(render_output));
        }
        Ok(())
    })
}

#[wasm_bindgen(js_name = "pollGuiRender")]
pub fn poll_gui_render() -> Vec<u8> {
    GUI_RENDER.with(|r| r.borrow_mut().poll().unwrap_or_default())
}

#[wasm_bindgen(js_name = "getRenderIslandVfsSnapshot")]
pub fn get_render_island_vfs_snapshot(
    entry_path: &str,
    workspace_discovery: &str,
) -> Result<JsValue, JsValue> {
    let options = project_context_options_from_workspace_discovery(workspace_discovery)
        .map_err(|e| JsValue::from_str(&e))?;
    collect_render_island_snapshot(entry_path, &options).map_err(|e| JsValue::from_str(&e))
}

#[wasm_bindgen(js_name = "pollIslandData")]
pub fn poll_island_data() -> Vec<u8> {
    GUEST.with(|g| {
        g.borrow_mut()
            .as_mut()
            .and_then(|guest| guest.poll_outbound_frame())
            .unwrap_or_default()
    })
}

#[wasm_bindgen(js_name = "pollPendingHostEvent")]
pub fn poll_pending_host_event() -> JsValue {
    GUEST.with(|g| {
        let mut guest = g.borrow_mut();
        let Some(guest) = guest.as_mut() else {
            return JsValue::NULL;
        };
        let Some(event) = guest.poll_pending_host_event() else {
            return JsValue::NULL;
        };
        pending_host_event_to_js(&event).into()
    })
}

#[wasm_bindgen(js_name = "wakeHostEvent")]
pub fn wake_host_event(key: &str) -> Result<(), JsValue> {
    let key = HostWaitKey::decode(key).map_err(|e| JsValue::from_str(&e))?;
    with_guest_mut(|guest| {
        guest.wake_host_event(key).map_err(session_error_to_js)?;
        let step = guest.run_scheduled().map_err(session_error_to_js)?;
        run_gc_stress_guest_step(guest);
        flush_stdout("guest", step.stdout.as_deref());
        if let Some(render_output) = step.render_output {
            GUI_RENDER.with(|r| r.borrow_mut().push(render_output));
        }
        Ok(())
    })
}

/// Stop the running guest app (clears state).
#[wasm_bindgen(js_name = "stopGui")]
pub fn stop_gui() {
    GUEST.with(|g| {
        if let Some(guest) = g.borrow_mut().as_mut() {
            guest.shutdown();
        }
        *g.borrow_mut() = None;
    });
    GUI_RENDER.with(|r| {
        r.borrow_mut().poll();
    });
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
    use vo_common_core::bytecode::{FunctionDef, JitInstructionMetadata, Module};
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
        jit_metadata: vec![JitInstructionMetadata::None],
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
        assert!(vo_module::project::read_inline_project_plan(&lockless_content, None)
            .unwrap()
            .lock_file()
            .is_none());

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
    fn studio_vfs_compile_cache_epoch_tracks_extern_protocol_v3() {
        assert_eq!(STUDIO_VFS_COMPILE_CACHE_SCHEMA_VERSION, "4");
        assert_eq!(
            vo_web_runtime_wasm::ext_bridge::WASM_EXTENSION_PROTOCOL_VERSION,
            3
        );
    }

    #[wasm_bindgen_test]
    fn browser_extension_bridges_use_strict_tuple_routing_source_contract() {
        let sources = [("Studio", include_str!("../../src/lib/studio_wasm.ts"))];
        for (label, source) in sources {
            for required in [
                "export function decodeVoExternName(",
                "new TextDecoder('utf-8', { fatal: true, ignoreBOM: true })",
                "parseExternByteLength(",
                "export function validateCanonicalModuleOwner(",
                "function isCanonicalPortableModuleSegment(",
                "function isPortablePackageSegment(",
                "segments[0] !== 'github.com'",
                "MAX_CANONICAL_MODULE_OWNER_BYTES = 255",
                "MAX_PORTABLE_PACKAGE_COMPONENT_BYTES = 255",
                "const UTF8_ENCODER = new TextEncoder()",
                "export function selectVoExternModuleOwner(",
                "packageName.startsWith(`${owner}/`)",
                "suffix.split('/').every(isPortablePackageSegment)",
                "UTF8_ENCODER.encode(packageName).length > MAX_EXTERN_NAME_BYTES",
                "owner.length > selected.length",
                "vo_ext_protocol_version",
                "WASM_EXTENSION_PROTOCOL_VERSION = 3",
                "WASM_EXTENSION_EXPORT_PREFIX = '__vo_ext_'",
                "export function voExternExportKey(",
                "VO_EXTERN_BOM_CONTRACT_VECTORS",
                "VO_PACKAGE_OWNER_NFC_CONTRACT_VECTORS",
                "segment.normalize('NFC') !== segment",
                "'github.com/acme/graphics/é', true",
                "'github.com/acme/graphics/e\\u0301', false",
                "'vo1:27:\\uFEFFgithub.com/acme/graphics:4:Draw'",
                "'vo1:24:github.com/acme/graphics:7:\\uFEFFDraw'",
                "byte.toString(16).padStart(2, '0')",
                "const exportKey = wasmExtensionExportKeyFromCanonical(externName)",
                "bindgenModule[exportKey]",
                "exp[exportKey]",
                "bindgenProtocolExports(",
                "bindgen initializer did not return raw WebAssembly instance exports",
                "Always import a fresh Blob URL",
                "const extArtifacts = new Map",
                "const extLoadOperations = new Map",
                "const extExhaustedOwnerLoads = new Set",
                "type ExtensionLoadHandle",
                "artifactToken: string",
                "leaseToken: string",
                "ready: Promise<void>",
                "voCommitExtModule",
                "voAbortExtModuleLoad",
                "voAbortExtModuleLoadHandle",
                "const extLoadHandleLeases = new WeakMap",
                "const handle = Object.freeze(",
                "extensionLoadGenerationToken(",
                "voIsExtModuleLoadCurrent",
                "forgetWasmExtModuleOwner",
                "clearWasmExtModuleOwners",
                "pendingLoad.jsGlueSourcePromise",
                "pendingLoad.hasJsGlue !== hasJsGlue",
                "existingArtifact.jsGlueSource === jsGlueSource",
                "bytesEqual(existingArtifact.bytes, moduleBytes)",
                "await pendingLoad.promise",
                "assertExtensionLoadActive(",
                "extLoadOperations.get(key) !== currentOperation",
                "is already loaded with a different artifact",
                "voDisposeExtModule =",
                "voDisposeAllExtModules =",
                "function wasmU32(",
                "function validateWasmRange(",
                "function wasmRangesOverlap(",
                "function bestEffortDealloc(",
                "if (outPtr === 0)",
                "if (outLen !== 0)",
                "inputAllocated = inputPtr !== 0",
                "Input must be a Uint8Array",
            ] {
                assert!(
                    source.contains(required),
                    "{label} extension bridge is missing strict contract marker {required:?}"
                );
            }
            for forbidden in [
                "externName.startsWith(",
                "externName.substring(",
                "voRegisterExtModuleAlias",
                "voCallExtReplay",
                "exp[externName]",
                "bindgenModule[externName]",
                "bindgenModule[decoded.functionName]",
                "exp[decoded.functionName]",
                "endsWith('waitForEvent')",
                "return glue;",
                "typeof result === 'string'",
            ] {
                assert!(
                    !source.contains(forbidden),
                    "{label} extension bridge retains legacy routing heuristic {forbidden:?}"
                );
            }

            use vo_common_core::extern_key::ExternKeyRef;
            for key in [
                ExternKeyRef::new("github.com/acme/graphics", "Draw"),
                ExternKeyRef::new("github.com/acme/图形", "绘制"),
                ExternKeyRef::new("github.com/acme/graphics/render", "Draw"),
            ] {
                let encoded = key.encode().expect("contract extern must encode");
                let export = key
                    .wasm_extension_export_key()
                    .expect("contract export key must encode");
                assert!(
                    source.contains(&format!("'{encoded}'")),
                    "{label} is missing shared encoded-extern vector {encoded:?}"
                );
                assert!(
                    source.contains(&format!("'{export}'")),
                    "{label} is missing shared WASM export-key vector {export:?}"
                );
            }

            let setup = source
                .split("voSetupExtModule =")
                .nth(1)
                .expect("extension setup function")
                .split("voIsExtModuleLoadCurrent =")
                .next()
                .expect("extension setup body");
            assert!(setup.contains("const existingArtifact = extArtifacts.get(key)"));
            assert!(
                setup
                    .find("const pendingLoad = extLoadOperations.get(key)")
                    .expect("pending owner transaction")
                    < setup
                        .find("const jsGlueSourcePromise =")
                        .expect("glue source fetch transaction"),
                "pending owner state must be checked before preparing glue identity"
            );
            assert!(
                setup
                    .find("extLoadOperations.set(key, operation)")
                    .expect("publish pending owner transaction")
                    < setup.find("await loadPromise;").expect("await owner load"),
                "the owner transaction must be published before the setup call yields"
            );
            assert!(
                setup.contains("return;"),
                "identical reload must be idempotent"
            );
            assert!(
                !setup.contains("unloadExtModule(key)"),
                "failed or conflicting reload must preserve the live artifact"
            );
            for forbidden_publish in [
                "extArtifacts.set(",
                "extInstances.set(",
                "extBindgenModules.set(",
                "extStandaloneRefs.set(",
            ] {
                assert!(
                    !setup.contains(forbidden_publish),
                    "{label} setup must keep prepared artifacts outside active dispatch maps: {forbidden_publish}"
                );
            }

            let allocate_lease = source
                .split("function allocateExtensionLoadLease(")
                .nth(1)
                .expect("lease allocator")
                .split("function extensionLoadHandle(")
                .next()
                .expect("lease allocator body");
            assert!(
                allocate_lease
                    .find("extLoadLeases.set(leaseToken")
                    .expect("lease-map publication")
                    < allocate_lease
                        .find("nextExtLoadLease = nextLease")
                        .expect("lease-generation commit"),
                "{label} must not consume a lease generation before its map entry is published"
            );
            let load_handle_tail = source
                .split("function extensionLoadHandle(")
                .nth(1)
                .expect("load-handle constructor");
            let load_handle = if label == "Studio" {
                load_handle_tail
                    .split("let extBridgeInstalled")
                    .next()
                    .expect("Studio load-handle body")
            } else {
                load_handle_tail
                    .split("function requireExtensionProtocolV3(")
                    .next()
                    .expect("legacy load-handle body")
            };
            assert!(load_handle.contains("extLoadHandleLeases.set(handle"));
            assert!(
                load_handle.contains("abortExtensionLoadLease(owner, artifactToken, leaseToken)")
            );

            let commit = source
                .split("function commitExtModule(")
                .nth(1)
                .expect("extension commit function")
                .split("function ")
                .next()
                .expect("extension commit body");
            assert!(commit.contains("extArtifacts.set(key, prepared.artifact)"));
            assert!(commit.contains("extLoadOperations.delete(key)"));
            assert!(commit.contains("extArtifacts.delete(key)"));
            assert!(commit.contains("disposePreparedExtensionArtifact(prepared)"));
            let abort = source
                .split("function abortExtensionLoadLease(")
                .nth(1)
                .expect("extension abort transaction")
                .split("function unloadExtModule(")
                .next()
                .expect("extension abort transaction body");
            assert!(abort.contains("hasAnotherLease"));
            assert!(abort.contains("extExhaustedOwnerLoads.add(key)"));
            assert!(abort.contains("throw error;"));
            assert!(abort.contains("cancelPendingExtensionLoad(key, artifactToken)"));

            let unload = source
                .split("function unloadExtModule(")
                .nth(1)
                .expect("single-owner unload function")
                .split("function bytesEqual(")
                .next()
                .expect("single-owner unload body");
            assert!(
                unload
                    .find("forgetWasmExtModuleOwner(")
                    .expect("Rust owner forget")
                    < unload
                        .find("extArtifacts.delete(")
                        .expect("artifact dispatch removal"),
                "{label} must preserve active JS dispatch state when Rust owner disposal throws"
            );
            for js_mutation in [
                "extOwnerLoadGenerations.set(",
                "extLoadOperations.delete(",
                "removeExtensionLoadLeases(",
                "extBindgenModules.delete(",
                "extInstances.delete(",
                "extArtifacts.delete(",
            ] {
                assert!(
                    unload
                        .find("forgetWasmExtModuleOwner(")
                        .expect("Rust owner forget")
                        < unload.find(js_mutation).unwrap_or_else(|| {
                            panic!("{label} unload is missing JavaScript mutation {js_mutation}")
                        }),
                    "{label} must leave the complete JavaScript owner transaction unchanged when Rust owner disposal throws: {js_mutation}"
                );
            }
            assert!(
                unload
                    .find("extArtifacts.delete(")
                    .expect("artifact dispatch removal")
                    < unload
                        .find("disposePreparedExtensionArtifact(prepared)")
                        .expect("prepared artifact cleanup"),
                "{label} prepared cleanup must observe every JavaScript dispatch map as absent"
            );
            let cleanup = if label == "Studio" {
                unload.find("disposeStandaloneRef(standaloneRef")
            } else {
                unload.find("typeof bindgen.__voDispose")
            }
            .expect("extension cleanup hook");
            assert!(
                unload
                    .find("forgetWasmExtModuleOwner(")
                    .expect("Rust owner forget")
                    < cleanup,
                "{label} cleanup must observe the owner as absent in both routing layers"
            );

            let unload_all = source
                .split("function unloadAllExtModules(")
                .nth(1)
                .expect("all-owner unload function")
                .split("function throwVoCallExtFailure(")
                .next()
                .unwrap_or_else(|| {
                    source
                        .split("function unloadAllExtModules(")
                        .nth(1)
                        .expect("legacy all-owner unload function")
                        .split("function wasmU32(")
                        .next()
                        .expect("legacy all-owner unload body")
                });
            assert!(
                unload_all
                    .find("clearWasmExtModuleOwners(")
                    .expect("Rust owner catalog clear")
                    < unload_all
                        .find("extArtifacts.clear()")
                        .expect("active artifact map clear"),
                "{label} must preserve all active JS dispatch maps when Rust owner reset throws"
            );
            for js_mutation in [
                "extResetGeneration = nextResetGeneration",
                "extOwnerLoadGenerations.clear()",
                "extLoadOperations.clear()",
                "extLoadLeases.clear()",
                "extBindgenModules.clear()",
                "extInstances.clear()",
                "extArtifacts.clear()",
            ] {
                assert!(
                    unload_all
                        .find("clearWasmExtModuleOwners(")
                        .expect("Rust owner catalog clear")
                        < unload_all.find(js_mutation).unwrap_or_else(|| {
                            panic!("{label} reset is missing JavaScript mutation {js_mutation}")
                        }),
                    "{label} must leave the complete JavaScript reset transaction unchanged when Rust owner reset throws: {js_mutation}"
                );
            }
            assert!(
                unload_all
                    .find("extArtifacts.clear()")
                    .expect("active artifact map clear")
                    < unload_all
                        .find("disposePreparedExtensionArtifact(prepared)")
                        .expect("prepared artifact cleanup"),
                "{label} cleanup hooks must run after both routing layers are absent"
            );
        }
        let studio = sources[0].1;
        assert!(studio.contains("const extStandaloneRefs = new Map"));
        assert!(studio.contains("const standaloneHostStates = new Set"));
        assert!(studio.contains("disposeStandaloneRef(standaloneRef"));

        let loader = studio
            .split("export async function loadStudioWasm()")
            .nth(1)
            .expect("Studio WASM loader")
            .split("export function resetStudioWasmInstance()")
            .next()
            .expect("Studio WASM loader body");
        assert!(
            loader
                .find("if (generation !== loadGeneration)")
                .expect("superseded-load generation guard")
                < loader
                    .find("installExtBridgeGlobals(normalized)")
                    .expect("global bridge publication"),
            "a superseded Studio WASM initializer must not publish a stale owner-state bridge"
        );
    }

    #[wasm_bindgen_test]
    fn browser_extension_protocol_v3_is_normative_in_both_specs() {
        let normalize_markdown =
            |source: &str| source.split_whitespace().collect::<Vec<_>>().join(" ");
        let native_ffi =
            normalize_markdown(include_str!("../../../../lang/docs/spec/native-ffi.md"));
        for required in [
            "## 6. Browser WASM Extension Protocol v3",
            "vo_ext_protocol_version(void)",
            "vo_ext::export_wasm_extension_protocol!()",
            "case-sensitive and may contain portable Unicode",
            "__vo_ext_ + lowercase_hex(UTF-8(canonical_encoded_extern_name))",
            "no hash and no truncation",
            "MUST NOT retry a less-specific owner",
            "UTF-8 BOM bytes at the beginning of a field are ordinary U+FEFF data",
            "(output_ptr=0, output_len=0)",
            "pairwise disjoint",
            "exactly 3 bytes",
            "Function-name suffixes and JS-side semantic guesses MUST NOT",
            "Only one load transaction may be pending for an owner",
            "invalidated asynchronous result MUST NOT publish",
            "Strings, promises, and other JavaScript values do not satisfy",
            "host timers, intervals, animation frames, and game",
            "monotonically increasing generation",
            "Setup synchronously returns an opaque artifact token",
            "validates that binding both immediately before and immediately after",
            "last uncommitted lease destroys the prepared artifact",
            "owner lifecycle epoch and active artifact generations",
            "Studio VFS compile cache epoch for protocol v3 is `4`",
        ] {
            assert!(
                native_ffi.contains(required),
                "native FFI spec is missing browser-v3 contract marker {required:?}"
            );
        }

        let module = normalize_markdown(include_str!("../../../../lang/docs/spec/module.md"));
        for required in [
            "The resolved canonical module path is the artifact's extern-owner identity.",
            "Every extension backend selects the longest loaded canonical module owner",
            "case-sensitive and may contain portable Unicode",
            "lowercase hexadecimal form of every UTF-8 byte",
            "Decoded-function, full-wire-name",
            "MUST NOT fall back to a parent owner",
            "freezes the selected `(owner, generation)`",
            "vo_ext_protocol_version()",
            "return browser protocol version `3`",
            "explicit disposal is required before intentional replacement",
            "Concurrent identical loads join one transaction",
            "prepared artifact remains outside active dispatch maps",
            "validates the frozen binding before and after every JavaScript export",
            "browser WASM protocol v3 in `native-ffi.md` section 6",
        ] {
            assert!(
                module.contains(required),
                "module spec is missing browser-v3 contract marker {required:?}"
            );
        }
    }

    #[wasm_bindgen_test]
    fn canonical_extern_owner_selection_handles_unicode_functions_and_portable_nesting() {
        use vo_common_core::extern_key::{decode_extern_name, ExternKeyRef};

        let encoded = ExternKeyRef::new("github.com/acme/graphics/图形/Render/V2", "绘制")
            .encode()
            .expect("canonical Unicode extern");
        let key = decode_extern_name(&encoded).expect("decode canonical Unicode extern");
        let owners = [
            "github.com/acme/graphics",
            "github.com/acme/graphics-vector",
            "github.com/acme/graphic",
        ];
        let selected = owners
            .into_iter()
            .filter(|owner| key.is_owned_by_module(owner))
            .max_by_key(|owner| owner.len());
        assert_eq!(selected, Some("github.com/acme/graphics"));
        assert!(!key.is_owned_by_module("github.com/acme/graphic"));

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
