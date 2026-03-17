//! Vibe Studio WASM entry point.
//!
//! Exposes compile_run_entry / run_gui_entry / send_gui_event / stop_gui
//! to the Svelte frontend.
//!
//! The IDE UI is Svelte; this module compiles and runs user Vo code.
//! Source files are read from the JS VirtualFS (via vo_web_runtime_wasm::vfs).

use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use std::path::PathBuf;
use js_sys::{Object, Reflect};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsValue;
use vo_common::vfs::{FileSystem, MemoryFs};
use vo_runtime::island::{EndpointRequestKind, EndpointResponseKind, IslandCommand};
use vo_runtime::island_msg::{decode_island_transport_frame, encode_island_transport_frame};
use vo_vm::vm::SchedulingOutcome;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = globalThis, js_name = __voStudioDebugLog)]
    fn vo_studio_debug_log(message: &str);
}

fn studio_debug_log(message: &str) {
    vo_studio_debug_log(message);
}

fn hex_bytes(data: &[u8]) -> String {
    let mut out = String::new();
    for (i, byte) in data.iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        out.push_str(&format!("{:02x}", byte));
    }
    out
}

fn describe_island_command(cmd: &IslandCommand) -> String {
    match cmd {
        IslandCommand::SpawnFiber { closure_data } => {
            format!("SpawnFiber(data_len={})", closure_data.data().len())
        }
        IslandCommand::WakeFiber { fiber_id } => {
            format!("WakeFiber(fiber_id={fiber_id})")
        }
        IslandCommand::Shutdown => "Shutdown".to_string(),
        IslandCommand::EndpointRequest { endpoint_id, kind, from_island, fiber_id } => {
            let kind_desc = match kind {
                EndpointRequestKind::Send { data } => format!("Send(len={})", data.len()),
                EndpointRequestKind::Recv => "Recv".to_string(),
                EndpointRequestKind::Close => "Close".to_string(),
                EndpointRequestKind::Transfer { new_peer } => format!("Transfer(new_peer={new_peer})"),
            };
            format!(
                "EndpointRequest(endpoint_id={endpoint_id}, from_island={from_island}, fiber_id={fiber_id}, kind={kind_desc})"
            )
        }
        IslandCommand::EndpointResponse { endpoint_id, kind, fiber_id } => {
            let kind_desc = match kind {
                EndpointResponseKind::SendAck { closed } => format!("SendAck(closed={closed})"),
                EndpointResponseKind::RecvData { data, closed } => {
                    let preview = if data.len() <= 64 {
                        hex_bytes(data)
                    } else {
                        format!("{} ...", hex_bytes(&data[..64]))
                    };
                    format!("RecvData(len={}, closed={closed}, data=[{}])", data.len(), preview)
                }
                EndpointResponseKind::Closed => "Closed".to_string(),
            };
            format!(
                "EndpointResponse(endpoint_id={endpoint_id}, fiber_id={fiber_id}, kind={kind_desc})"
            )
        }
    }
}

fn ensure_panic_hook() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| console_error_panic_hook::set_once());
}

// Embed shell handler source files for runShellHandler.
include!(concat!(env!("OUT_DIR"), "/shell_embedded.rs"));


// =============================================================================
// Guest state (for a running vogui app)
// =============================================================================

struct GuestState {
    vm: vo_web::Vm,
    event_wait_token: Option<u64>,
    outbound_frames: VecDeque<Vec<u8>>,
    pending_host_events: VecDeque<(u64, u32)>,
    pending_host_event_tokens: HashSet<u64>,
}

thread_local! {
    static GUEST: RefCell<Option<GuestState>> = RefCell::new(None);
}

fn load_guest_from_bytecode(bytecode: &[u8]) -> Result<GuestState, JsValue> {
    let vm = vo_web::create_loaded_vm(bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    }).map_err(|e| JsValue::from_str(&e))?;
    Ok(GuestState {
        vm,
        event_wait_token: None,
        outbound_frames: VecDeque::new(),
        pending_host_events: VecDeque::new(),
        pending_host_event_tokens: HashSet::new(),
    })
}

// =============================================================================
// VoVm — instance-based VM for render islands (framework-neutral)
// =============================================================================

/// A Vo VM instance with ext_bridge externs registered.
/// Exposes the VoWebModule.VoVm interface expected by render island bootstrappers.
#[wasm_bindgen(js_name = "StudioVoVm")]
pub struct StudioVoVm {
    vm: vo_web::Vm,
    outbound_frames: VecDeque<Vec<u8>>,
    pending_host_events: VecDeque<(u64, u32)>,
    pending_host_event_tokens: HashSet<u64>,
}

#[wasm_bindgen(js_class = "StudioVoVm")]
impl StudioVoVm {
    /// Create a VM from bytecode with ext_bridge externs registered.
    /// Corresponds to VoWebModule.VoVm.withExterns(bytecode).
    #[wasm_bindgen(js_name = "withExterns")]
    pub fn with_externs(bytecode: &[u8]) -> Result<StudioVoVm, JsValue> {
        ensure_panic_hook();
        let module = vo_vm::bytecode::Module::deserialize(bytecode)
            .map_err(|e| JsValue::from_str(&format!("Failed to load bytecode: {:?}", e)))?;
        let vm = vo_web::create_loaded_vm_from_module(module, |reg, exts| {
            vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
        }).map_err(|e| JsValue::from_str(&e))?;
        Ok(StudioVoVm {
            vm,
            outbound_frames: VecDeque::new(),
            pending_host_events: VecDeque::new(),
            pending_host_event_tokens: HashSet::new(),
        })
    }

    /// Run the VM entry point (cold start).
    pub fn run(&mut self) -> Result<String, JsValue> {
        self.vm.clear_host_output();
        vo_runtime::output::clear_output();
        let outcome = self.vm.run()
            .map_err(|e| JsValue::from_str(&format!("{:?}", e)))?;
        self.process_outcome(&outcome)?;
        let _ = self.vm.take_host_output();
        Ok(format!("{:?}", outcome))
    }

    /// Run island initialization only (global vars + user init functions, no main).
    /// Must be called on island VMs before processing SpawnFiber commands.
    #[wasm_bindgen(js_name = "runInit")]
    pub fn run_init(&mut self) -> Result<String, JsValue> {
        self.vm.clear_host_output();
        vo_runtime::output::clear_output();
        let outcome = self.vm.run_init()
            .map_err(|e| JsValue::from_str(&format!("{:?}", e)))?;
        self.process_outcome(&outcome)?;
        let _ = self.vm.take_host_output();
        Ok(format!("{:?}", outcome))
    }

    /// Run already-scheduled fibers (used after island commands or host events).
    #[wasm_bindgen(js_name = "runScheduled")]
    pub fn run_scheduled(&mut self) -> Result<String, JsValue> {
        self.vm.clear_host_output();
        vo_runtime::output::clear_output();
        let outcome = self.vm.run_scheduled()
            .map_err(|e| JsValue::from_str(&format!("{:?}", e)))?;
        self.process_outcome(&outcome)?;
        let _ = self.vm.take_host_output();
        Ok(format!("{:?}", outcome))
    }

    /// Push an island transport frame into the VM command queue (does not run the VM).
    #[wasm_bindgen(js_name = "pushIslandCommand")]
    pub fn push_island_command(&mut self, frame: &[u8]) -> Result<(), JsValue> {
        let (target_island_id, cmd) = decode_island_transport_frame(frame)
            .map_err(|e| JsValue::from_str(&format!("invalid island frame: {:?}", e)))?;
        let current_island_id = self.vm.current_island_id();
        if current_island_id == 0 {
            self.vm.set_island_id(target_island_id);
        } else if current_island_id != target_island_id {
            return Err(JsValue::from_str(&format!(
                "render island id mismatch: have {}, got {}",
                current_island_id, target_island_id
            )));
        }
        self.vm.push_island_command(cmd);
        Ok(())
    }

    /// Drain all outbound island transport frames queued since the last call.
    #[wasm_bindgen(js_name = "takeOutboundCommands")]
    pub fn take_outbound_commands(&mut self) -> js_sys::Array {
        let arr = js_sys::Array::new();
        while let Some(frame) = self.outbound_frames.pop_front() {
            arr.push(&js_sys::Uint8Array::from(frame.as_slice()));
        }
        arr
    }

    /// Drain pending host events (timers) that JS must schedule.
    /// Each element is { token: string, delayMs: number, replay: boolean }.
    #[wasm_bindgen(js_name = "takePendingHostEvents")]
    pub fn take_pending_host_events(&mut self) -> js_sys::Array {
        let arr = js_sys::Array::new();
        while let Some((token, delay_ms)) = self.pending_host_events.pop_front() {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("token"), &JsValue::from_str(&token.to_string()));
            let _ = Reflect::set(&obj, &JsValue::from_str("delayMs"), &JsValue::from_f64(delay_ms as f64));
            let _ = Reflect::set(&obj, &JsValue::from_str("replay"), &JsValue::from_bool(false));
            arr.push(&obj);
        }
        arr
    }

    /// Wake a suspended host event fiber and run scheduled work.
    #[wasm_bindgen(js_name = "wakeHostEvent")]
    pub fn wake_host_event_vm(&mut self, token: &str) -> Result<(), JsValue> {
        let token = token.parse::<u64>()
            .map_err(|e| JsValue::from_str(&format!("invalid token: {}", e)))?;
        self.pending_host_event_tokens.remove(&token);
        self.vm.wake_host_event(token);
        Ok(())
    }

    /// Take any stdout produced by the last VM run.
    #[wasm_bindgen(js_name = "takeOutput")]
    pub fn take_output(&self) -> String {
        vo_web::take_output()
    }

    fn process_outcome(&mut self, outcome: &SchedulingOutcome) -> Result<(), JsValue> {
        match outcome {
            SchedulingOutcome::Completed
            | SchedulingOutcome::Suspended
            | SchedulingOutcome::SuspendedForHostEvents => {}
            SchedulingOutcome::Blocked => {
                return Err(JsValue::from_str(&format!("{:?}", self.vm.deadlock_err())));
            }
            SchedulingOutcome::Panicked => {
                return Err(JsValue::from_str("unexpected bounded panic outcome in VoVm"));
            }
        }

        let pending = self.vm.scheduler.take_pending_host_events();
        for event in pending.iter().filter(|e| !e.replay) {
            if self.pending_host_event_tokens.insert(event.token) {
                self.pending_host_events.push_back((event.token, event.delay_ms));
            }
        }

        for (target_island_id, cmd) in self.vm.take_outbound_commands() {
            self.outbound_frames.push_back(encode_island_transport_frame(target_island_id, &cmd));
        }

        let stdout = vo_web::take_output();
        if !stdout.is_empty() {
            studio_debug_log(&format!("[render-stdout] {}", stdout.trim_end()));
            web_sys::console::log_1(&format!("[render-island] {}", stdout.trim_end()).into());
        }

        Ok(())
    }
}

// =============================================================================
// VoWebModule exports — initVFS
// preloadExtModule is provided by vo-web (3-param version with optional jsGlueUrl).
// =============================================================================

/// No-op: VFS is initialized as part of the Studio frontend startup.
#[wasm_bindgen(js_name = "initVFS")]
pub fn init_vfs() -> js_sys::Promise {
    js_sys::Promise::resolve(&JsValue::UNDEFINED)
}

// =============================================================================
// FS helpers
// =============================================================================

/// VFS root for third-party modules.
/// Modules are installed at `/<module_path>/...` in the JS VFS
/// (e.g. `/github.com/vo-lang/vox/vox.vo`), so the root is empty.
const VFS_MOD_ROOT: &str = "";

struct ResolvedVfsCompileTarget {
    entry_path: String,
    project_root: Option<String>,
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
    std::path::Path::new(path)
        .parent()
        .map(|p| {
            let value = p.to_string_lossy().to_string();
            if value.is_empty() { "/".to_string() } else { value }
        })
}

fn join_vfs_path(base: &str, child: &str) -> String {
    let normalized_base = normalize_vfs_path(base);
    if normalized_base == "/" {
        format!("/{}", child)
    } else {
        format!("{}/{}", normalized_base, child)
    }
}

fn is_vfs_dir(path: &str) -> bool {
    let normalized = normalize_vfs_path(path);
    let (_, err) = vo_web_runtime_wasm::vfs::read_dir(&normalized);
    err.is_none()
}

fn find_vfs_project_root(entry_path: &str) -> Option<String> {
    let normalized = normalize_vfs_path(entry_path);
    let mut current = if is_vfs_dir(&normalized) {
        normalized
    } else {
        vfs_parent_dir(&normalized).unwrap_or_else(|| "/".to_string())
    };

    loop {
        let vo_mod_path = join_vfs_path(&current, "vo.mod");
        let (_, vo_mod_err) = vo_web_runtime_wasm::vfs::read_file(&vo_mod_path);
        if vo_mod_err.is_none() {
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
    let normalized = normalize_vfs_path(entry_path);
    let resolved_entry_path = if is_vfs_dir(&normalized) {
        let main_path = join_vfs_path(&normalized, "main.vo");
        let (_, err) = vo_web_runtime_wasm::vfs::read_file(&main_path);
        if let Some(e) = err {
            return Err(format!("read file '{}': {}", main_path, e));
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

/// Read all .vo files from a JS VFS directory (recursively) into a MemoryFs.
fn read_vfs_package(pkg_dir: &str, local_fs: &mut MemoryFs) -> Result<(), String> {
    let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(pkg_dir);
    if let Some(e) = err {
        return Err(format!("read dir '{}': {}", pkg_dir, e));
    }
    for (name, is_dir, _mode) in entries {
        let full = if pkg_dir == "/" {
            format!("/{}", name)
        } else {
            format!("{}/{}", pkg_dir, name)
        };
        if is_dir {
            read_vfs_package(&full, local_fs)?;
        } else if name.ends_with(".vo") {
            let (data, err) = vo_web_runtime_wasm::vfs::read_file(&full);
            if let Some(e) = err {
                return Err(format!("read file '{}': {}", full, e));
            }
            let content = String::from_utf8(data)
                .map_err(|e| format!("utf8 '{}': {}", full, e))?;
            // Store with a path relative to the VFS root (strip leading /)
            let rel = full.trim_start_matches('/');
            local_fs.add_file(PathBuf::from(rel), content);
        }
    }
    Ok(())
}

/// Compile user code from a VFS entry path (e.g. "/workspace/main/main.vo").
///
/// Single-file mode (no vo.mod in package dir): only the entry file is compiled,
/// so other .vo files in the same directory don't cause "duplicate main" errors.
/// Multi-file mode (vo.mod present): all .vo files in the package directory are read.
fn compile_from_vfs(entry_path: &str) -> Result<Vec<u8>, String> {
    let (target, local_fs) = prepare_from_vfs(entry_path)?;
    let entry_clean = target.entry_path.trim_start_matches('/');

    vo_web::compile_entry_with_vfs(entry_clean, local_fs, VFS_MOD_ROOT)
        .map_err(|e| format!("compile error: {}", e))
}

fn prepare_from_vfs(entry_path: &str) -> Result<(ResolvedVfsCompileTarget, MemoryFs), String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let entry_clean = target.entry_path.trim_start_matches('/');

    let mut local_fs = MemoryFs::new();

    if let Some(project_root) = &target.project_root {
        read_vfs_package(project_root, &mut local_fs)?;
    } else {
        let (data, err) = vo_web_runtime_wasm::vfs::read_file(&target.entry_path);
        if let Some(e) = err {
            return Err(format!("read file '{}': {}", target.entry_path, e));
        }
        let content = String::from_utf8(data)
            .map_err(|e| format!("utf8 '{}': {}", target.entry_path, e))?;
        local_fs.add_file(PathBuf::from(entry_clean), content);
    }

    Ok((target, local_fs))
}

// =============================================================================
// WASM exports
// =============================================================================

#[wasm_bindgen(js_name = "prepareEntry")]
pub fn prepare_entry(entry_path: &str) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let (target, local_fs) = prepare_from_vfs(&entry_path)
            .map_err(|e| JsValue::from_str(&e))?;
        let entry_clean = target.entry_path.trim_start_matches('/').to_string();

        if target.project_root.is_some() {
            vo_web::ensure_vfs_deps_from_fs(&local_fs, &entry_clean)
                .await
                .map_err(|e| JsValue::from_str(&e))?;
        } else {
            let content = local_fs.read_file(std::path::Path::new(&entry_clean))
                .map_err(|e| JsValue::from_str(&format!("read file '{}': {}", target.entry_path, e)))?;
            vo_web::ensure_vfs_versioned_imports(&content)
                .await
                .map_err(|e| JsValue::from_str(&e))?;
        }

        Ok(JsValue::NULL)
    })
}

/// Compile and run user Vo code (console app) from VFS entry path, returning captured stdout.
#[wasm_bindgen(js_name = "compileRunEntry")]
pub fn compile_run_entry(entry_path: &str) -> Result<String, JsValue> {
    ensure_panic_hook();
    let bytecode = compile_from_vfs(entry_path).map_err(|e| JsValue::from_str(&e))?;
    vo_web::take_output();

    let saved = vo_web::ext_bridge::save_extern_state();
    let run_result = vo_web::create_vm(&bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    });
    vo_web::ext_bridge::restore_extern_state(saved);
    run_result.map_err(|e| JsValue::from_str(&e))?;

    let output = vo_web::take_output();
    Ok(output)
}

/// Compile and start a guest vogui app from VFS entry path, returning initial render bytes.
///
/// The Vo app's `Run()` does initial render then blocks on `waitForEvent()`.
/// `vm.run()` returns `SuspendedForHostEvents` once the main fiber blocks.
#[wasm_bindgen(js_name = "runGuiEntry")]
pub fn run_gui_entry(entry_path: &str) -> Result<Vec<u8>, JsValue> {
    ensure_panic_hook();
    GUEST.with(|g| *g.borrow_mut() = None);

    let bytecode = compile_from_vfs(entry_path).map_err(|e| JsValue::from_str(&e))?;
    let mut guest = load_guest_from_bytecode(&bytecode)?;

    let outcome = guest.vm.run()
        .map_err(|e| JsValue::from_str(&format!("{:?}", e)))?;
    handle_guest_outcome(&mut guest, outcome)?;

    let render_bytes = guest.vm.take_host_output()
        .ok_or_else(|| JsValue::from_str("guest app did not emit a render"))?;
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(render_bytes)
}

/// Send an event to the running guest app, returning the new render bytes.
///
/// Stores event data and wakes the main fiber (blocked on waitForEvent).
/// The fiber processes the event inline and blocks again on waitForEvent.
/// No new fiber is created — zero allocation per event.
#[wasm_bindgen(js_name = "sendGuiEvent")]
pub fn send_gui_event(handler_id: i32, payload: &str) -> Result<Vec<u8>, JsValue> {
    let mut guest = GUEST.with(|g| g.borrow_mut().take())
        .ok_or_else(|| JsValue::from_str("No guest app running"))?;

    guest.vm.clear_host_output();
    vo_runtime::output::clear_output();

    // Encode event data: [i32 handler_id LE][UTF-8 payload]
    let token = guest.event_wait_token
        .ok_or_else(|| JsValue::from_str("Main fiber not waiting for events"))?;
    let mut event_data = Vec::with_capacity(4 + payload.len());
    event_data.extend_from_slice(&handler_id.to_le_bytes());
    event_data.extend_from_slice(payload.as_bytes());

    guest.vm.wake_host_event_with_data(token, event_data);
    let outcome = guest.vm.run_scheduled()
        .map_err(|e| JsValue::from_str(&format!("{:?}", e)))?;
    handle_guest_outcome(&mut guest, outcome)?;
    let render_bytes = guest.vm.take_host_output().unwrap_or_default();
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(render_bytes)
}

#[wasm_bindgen(js_name = "startRenderIsland")]
pub fn start_render_island(bytecode: &[u8]) -> Result<(), JsValue> {
    ensure_panic_hook();
    GUEST.with(|g| *g.borrow_mut() = None);
    let guest = load_guest_from_bytecode(bytecode)?;
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(())
}

#[wasm_bindgen(js_name = "pushIslandData")]
pub fn push_island_data(data: &[u8]) -> Result<(), JsValue> {
    let mut guest = GUEST.with(|g| g.borrow_mut().take())
        .ok_or_else(|| JsValue::from_str("No guest app running"))?;
    guest.vm.clear_host_output();
    vo_runtime::output::clear_output();
    let (target_island_id, cmd) = decode_island_transport_frame(data)
        .map_err(|e| JsValue::from_str(&format!("invalid island transport frame: {:?}", e)))?;
    let current_island_id = guest.vm.current_island_id();
    if current_island_id == 0 {
        guest.vm.set_island_id(target_island_id);
    } else if current_island_id != target_island_id {
        return Err(JsValue::from_str(&format!(
            "render island id mismatch: have {}, got {}",
            current_island_id,
            target_island_id
        )));
    }
    guest.vm.push_island_command(cmd);
    let outcome = guest.vm.run_scheduled()
        .map_err(|e| JsValue::from_str(&format!("{:?}", e)))?;
    handle_guest_outcome(&mut guest, outcome)?;
    let _ = guest.vm.take_host_output();
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(())
}

#[wasm_bindgen(js_name = "pollIslandData")]
pub fn poll_island_data() -> Vec<u8> {
    GUEST.with(|g| {
        g.borrow_mut()
            .as_mut()
            .and_then(|guest| guest.outbound_frames.pop_front())
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
        let Some((token, delay_ms)) = guest.pending_host_events.pop_front() else {
            return JsValue::NULL;
        };
        let obj = Object::new();
        let _ = Reflect::set(&obj, &JsValue::from_str("token"), &JsValue::from_str(&token.to_string()));
        let _ = Reflect::set(&obj, &JsValue::from_str("delayMs"), &JsValue::from_f64(delay_ms as f64));
        obj.into()
    })
}

#[wasm_bindgen(js_name = "wakeHostEvent")]
pub fn wake_host_event(token: &str) -> Result<(), JsValue> {
    let token = token
        .parse::<u64>()
        .map_err(|e| JsValue::from_str(&format!("invalid host event token '{}': {}", token, e)))?;
    let mut guest = GUEST.with(|g| g.borrow_mut().take())
        .ok_or_else(|| JsValue::from_str("No guest app running"))?;
    guest.pending_host_event_tokens.remove(&token);
    guest.vm.wake_host_event(token);
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(())
}

/// Stop the running guest app (clears state).
#[wasm_bindgen(js_name = "stopGui")]
pub fn stop_gui() {
    GUEST.with(|g| *g.borrow_mut() = None);
}

fn handle_guest_outcome(guest: &mut GuestState, outcome: SchedulingOutcome) -> Result<(), JsValue> {
    match outcome {
        SchedulingOutcome::Completed
        | SchedulingOutcome::Suspended
        | SchedulingOutcome::SuspendedForHostEvents => {}
        SchedulingOutcome::Blocked => {
            return Err(JsValue::from_str(&format!("{:?}", guest.vm.deadlock_err())));
        }
        SchedulingOutcome::Panicked => {
            return Err(JsValue::from_str("unexpected bounded panic outcome"));
        }
    }

    let pending_host_events = guest.vm.scheduler.take_pending_host_events();
    guest.event_wait_token = pending_host_events
        .iter()
        .find(|e| e.replay)
        .map(|e| e.token);

    for event in pending_host_events.iter().filter(|e| !e.replay) {
        if guest.pending_host_event_tokens.insert(event.token) {
            guest.pending_host_events.push_back((event.token, event.delay_ms));
        }
    }

    for (target_island_id, cmd) in guest.vm.take_outbound_commands() {
        guest.outbound_frames.push_back(encode_island_transport_frame(target_island_id, &cmd));
    }

    let stdout = vo_web::take_output();
    if !stdout.is_empty() {
        web_sys::console::log_1(&format!("[guest] {}", stdout.trim_end()).into());
    }

    Ok(())
}

// =============================================================================
// Shell handler runner
// =============================================================================

// Cache the compiled shell handler bytecode — compilation is expensive inside
// WASM (~2-5s), so we compile once and reuse across all runShellHandler calls.
thread_local! {
    static SHELL_HANDLER_BYTECODE: std::cell::RefCell<Option<Vec<u8>>> =
        std::cell::RefCell::new(None);
}

fn build_shell_handler_bytecode() -> Result<Vec<u8>, String> {
    let mut local_fs = MemoryFs::new();

    // Only the shell handler's own source files are embedded at build time.
    // Third-party deps (vox, git2, zip) are resolved from JS VFS via WasmVfs.
    for (vfs_path, bytes) in SHELL_HANDLER_FILES {
        let Ok(content) = std::str::from_utf8(bytes) else { continue };
        local_fs.add_file(std::path::PathBuf::from(*vfs_path), content.to_string());
    }

    vo_web::compile_entry_with_vfs(
        "studio/vo/shell/main.vo",
        local_fs,
        VFS_MOD_ROOT,
    ).map_err(|e| format!("error:{}", e))
}

fn get_shell_handler_bytecode() -> Result<Vec<u8>, String> {
    SHELL_HANDLER_BYTECODE.with(|cell| {
        let cached = cell.borrow();
        if let Some(bc) = cached.as_ref() {
            return Ok(bc.clone());
        }
        drop(cached);

        let bc = build_shell_handler_bytecode()?;
        *cell.borrow_mut() = Some(bc.clone());
        Ok(bc)
    })
}

/// Read the shell handler's embedded `vo.mod` content.
///
/// Returns `Err` if the file is missing or not valid UTF-8.
fn shell_vo_mod_content() -> Result<String, String> {
    let bytes = SHELL_HANDLER_FILES
        .iter()
        .find(|(path, _)| *path == "studio/vo/shell/vo.mod")
        .map(|(_, bytes)| *bytes)
        .ok_or_else(|| "no embedded vo.mod found".to_string())?;
    std::str::from_utf8(bytes)
        .map(|s| s.to_string())
        .map_err(|e| format!("vo.mod utf8: {}", e))
}

/// Return the module paths declared in the shell handler's embedded `vo.mod`.
///
/// Used by the JS bridge to derive the VFS purge list dynamically rather than
/// hardcoding module paths.  Returns a JS `Array<string>`.
#[wasm_bindgen(js_name = "getShellDepModules")]
pub fn get_shell_dep_modules() -> js_sys::Array {
    let arr = js_sys::Array::new();
    if let Ok(content) = shell_vo_mod_content() {
        if let Ok(mod_file) = vo_module::ModFile::parse(&content, std::path::Path::new("vo.mod")) {
            for req in &mod_file.requires {
                arr.push(&JsValue::from_str(&req.module));
            }
        }
    }
    arr
}

/// Install all dependencies declared in the shell handler's embedded `vo.mod`.
///
/// Extracts the `vo.mod` that was embedded at build time and delegates to
/// `vo_web::ensure_vfs_deps` — the module system handles parsing, checking
/// whether each module is already in VFS, and fetching missing ones.
///
/// Adding a new shell dependency is a one-line change to `studio/vo/shell/vo.mod`.
#[wasm_bindgen(js_name = "preloadShellDeps")]
pub fn preload_shell_deps() -> js_sys::Promise {
    wasm_bindgen_futures::future_to_promise(async move {
        let content = shell_vo_mod_content()
            .map_err(|e| JsValue::from_str(&e))?;

        vo_web::ensure_vfs_deps(&content)
            .await
            .map_err(|e| JsValue::from_str(&e))?;

        Ok(JsValue::from_str("ok"))
    })
}

/// Download a module from GitHub, write its source files to the JS VFS,
/// and load the pre-built WASM extension (if any).
///
/// `spec` is `"<module>@<version>"`, e.g. `"github.com/vo-lang/zip@v0.1.0"`.
/// Returns the VFS install path (`"/<module>"`) on success.
/// On error the Promise is rejected with a plain string describing the failure.
///
/// All GitHub fetch + VFS write + ext-WASM load logic lives in `vo_web::install_module_to_vfs`.
/// TypeScript is a thin bridge that just awaits this Promise.
#[wasm_bindgen(js_name = "preloadModule")]
pub fn preload_module(spec: &str) -> js_sys::Promise {
    let spec = spec.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let path = vo_web::install_module_to_vfs(&spec)
            .await
            .map_err(|e| wasm_bindgen::JsValue::from_str(&e))?;
        Ok(wasm_bindgen::JsValue::from_str(&path))
    })
}

/// Return a content hash of all embedded shell handler source files.
///
/// The JS bridge uses this as an IndexedDB cache key so it can skip
/// recompilation when the sources haven't changed between page loads.
#[wasm_bindgen(js_name = "shellHandlerSourceHash")]
pub fn shell_handler_source_hash() -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    for (path, bytes) in SHELL_HANDLER_FILES {
        path.hash(&mut hasher);
        bytes.hash(&mut hasher);
    }
    format!("{:016x}", hasher.finish())
}

/// Accept pre-compiled shell handler bytecode from the JS-side IndexedDB cache.
///
/// If the bytecode is valid, it's stored in the thread-local cache so
/// `runShellHandler` never needs to recompile.  Returns `true` on success.
#[wasm_bindgen(js_name = "loadCachedShellHandler")]
pub fn load_cached_shell_handler(bytes: &[u8]) -> bool {
    ensure_panic_hook();
    if bytes.is_empty() {
        return false;
    }
    SHELL_HANDLER_BYTECODE.with(|cell| {
        *cell.borrow_mut() = Some(bytes.to_vec());
    });
    true
}

/// Compile the shell handler and return the bytecode for JS-side caching.
///
/// Returns the compiled bytecode on success, or an error string.
/// The JS bridge stores this in IndexedDB keyed by `shellHandlerSourceHash()`.
#[wasm_bindgen(js_name = "buildShellHandler")]
pub fn build_shell_handler_export() -> Result<Vec<u8>, JsValue> {
    ensure_panic_hook();
    let bc = build_shell_handler_bytecode()
        .map_err(|e| JsValue::from_str(&e))?;
    SHELL_HANDLER_BYTECODE.with(|cell| {
        *cell.borrow_mut() = Some(bc.clone());
    });
    Ok(bc)
}

/// Pre-warm the shell handler bytecode cache during bridge initialization.
/// Call this once after WASM module load so the first shell op is fast.
#[wasm_bindgen(js_name = "initShellHandler")]
pub fn init_shell_handler() -> Option<String> {
    ensure_panic_hook();
    match get_shell_handler_bytecode() {
        Ok(_) => None,
        Err(e) => Some(e),
    }
}

/// Compile and run the embedded shell handler with the given os.Args.
///
/// `args` is a JS `Array<string>` that becomes `os.Args` inside the Vo program.
/// Conventionally args = ["wasm", <req_json>, <workspace>].
/// Returns stdout from the Vo program (a JSON-encoded ShellResponse).
/// Returns an error string (prefixed with "error:") on compile/runtime failure.
#[wasm_bindgen(js_name = "runShellHandler")]
pub fn run_shell_handler(args: js_sys::Array) -> String {
    ensure_panic_hook();

    let bytecode = match get_shell_handler_bytecode() {
        Ok(b) => b,
        Err(e) => return e,
    };

    let args_vec: Vec<String> = args.iter().filter_map(|v: JsValue| v.as_string()).collect();

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(args_vec);
    });

    vo_runtime::output::clear_output();
    // Save and restore ext state so the shell handler doesn't clobber
    // EXTERN_ID_TO_INFO while a guest GUI VM (e.g. game loop) is live.
    let saved = vo_web::ext_bridge::save_extern_state();
    let run_result = vo_web::create_vm(&bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    });
    vo_web::ext_bridge::restore_extern_state(saved);

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = None;
    });

    let stdout = vo_web::take_output();

    match run_result {
        Ok(_) => stdout,
        Err(e) => {
            if stdout.trim().is_empty() {
                format!("error:{}", e)
            } else {
                stdout
            }
        }
    }
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
    compile_from_vfs(path).map_err(|e| JsValue::from_str(&e))
}

/// Compile a directory (entry = dir/main.vo) from VFS.
/// Returns serialised bytecode on success.
#[wasm_bindgen(js_name = "voHostCompileDir")]
pub fn vo_host_compile_dir(path: &str) -> Result<Vec<u8>, JsValue> {
    compile_from_vfs(path).map_err(|e| JsValue::from_str(&e))
}

/// Compile source code string. Returns serialised bytecode.
#[wasm_bindgen(js_name = "voHostCompileString")]
pub fn vo_host_compile_string(code: &str) -> Result<Vec<u8>, JsValue> {
    vo_web::compile_source_with_vfs(code, "main.vo", VFS_MOD_ROOT)
        .map_err(|e| JsValue::from_str(&format!("compile error: {}", e)))
}

/// Type-check source code. Returns empty string on success, error message on failure.
#[wasm_bindgen(js_name = "voHostCompileCheck")]
pub fn vo_host_compile_check(code: &str) -> String {
    match vo_web::compile_source_with_vfs(code, "main.vo", VFS_MOD_ROOT) {
        Ok(_) => String::new(),
        Err(e) => e.to_string(),
    }
}

/// Run bytecode (VM mode).
#[wasm_bindgen(js_name = "voHostRunBytecode")]
pub fn vo_host_run_bytecode(bytecode: &[u8]) -> Result<(), JsValue> {
    vo_runtime::output::clear_output();
    let saved = vo_web::ext_bridge::save_extern_state();
    let result = vo_web::create_vm(bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    })
    .map(|_| ());
    vo_web::ext_bridge::restore_extern_state(saved);
    result.map_err(|e| JsValue::from_str(&e))
}

/// Run bytecode and capture stdout. Returns captured output.
#[wasm_bindgen(js_name = "voHostRunBytecodeCapture")]
pub fn vo_host_run_bytecode_capture(bytecode: &[u8]) -> Result<String, JsValue> {
    vo_runtime::output::clear_output();
    let saved = vo_web::ext_bridge::save_extern_state();
    let result = vo_web::create_vm(bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    });
    let captured = vo_web::take_output();
    vo_web::ext_bridge::restore_extern_state(saved);
    match result {
        Ok(_) => Ok(captured),
        Err(e) => {
            if captured.trim().is_empty() {
                Err(JsValue::from_str(&e))
            } else {
                Err(JsValue::from_str(&format!("{}\nRuntime error: {}", captured.trim_end(), e)))
            }
        }
    }
}
