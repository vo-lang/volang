//! Guest GUI VM management: RunGui, SendGuiEvent, StopGui.
//!
//! Each guest GUI app runs on a dedicated OS thread with its own TLS-isolated
//! PENDING_RENDER / PENDING_HANDLER state, so multiple guest VMs never interfere.
//!
//! This module is only available on non-WASM targets; WASM builds return errors from the externs.

#![cfg(not(target_arch = "wasm32"))]

use std::sync::{mpsc, Mutex};
use vo_engine::CompileOutput;
use vo_vm::vm::Vm;
use vo_vm::vm::helpers::build_closure_args;
use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

// =============================================================================
// GuestHandle
// =============================================================================

pub struct GuestHandle {
    pub event_tx: mpsc::SyncSender<(i32, String)>,
    pub render_rx: mpsc::Receiver<Result<String, String>>,
}

// =============================================================================
// Guest Handle Storage (indexed by module id)
// =============================================================================

static GUEST_HANDLES: Mutex<Vec<Option<GuestHandle>>> = Mutex::new(Vec::new());

pub fn store_guest_handle(handle: GuestHandle) -> i64 {
    let mut handles = GUEST_HANDLES.lock().unwrap();
    for (i, slot) in handles.iter_mut().enumerate() {
        if slot.is_none() {
            *slot = Some(handle);
            return i as i64;
        }
    }
    let id = handles.len();
    handles.push(Some(handle));
    id as i64
}

pub fn with_guest_handle<F, R>(id: i64, f: F) -> Option<R>
where
    F: FnOnce(&mut GuestHandle) -> R,
{
    let mut handles = GUEST_HANDLES.lock().unwrap();
    let idx = id as usize;
    handles.get_mut(idx).and_then(|s| s.as_mut()).map(f)
}

pub fn take_guest_handle(id: i64) -> Option<GuestHandle> {
    let mut handles = GUEST_HANDLES.lock().unwrap();
    let idx = id as usize;
    if idx < handles.len() {
        handles[idx].take()
    } else {
        None
    }
}

// =============================================================================
// Module id -> guest id mapping
// =============================================================================

static MODULE_GUEST_MAP: Mutex<Vec<Option<i64>>> = Mutex::new(Vec::new());

pub fn set_module_guest(module_id: i64, guest_id: i64) {
    let mut map = MODULE_GUEST_MAP.lock().unwrap();
    let idx = module_id as usize;
    while map.len() <= idx {
        map.push(None);
    }
    map[idx] = Some(guest_id);
}

pub fn get_module_guest(module_id: i64) -> Option<i64> {
    let map = MODULE_GUEST_MAP.lock().unwrap();
    let idx = module_id as usize;
    map.get(idx).and_then(|v| *v)
}

pub fn clear_module_guest(module_id: i64) {
    let mut map = MODULE_GUEST_MAP.lock().unwrap();
    let idx = module_id as usize;
    if idx < map.len() {
        map[idx] = None;
    }
}

// =============================================================================
// run_gui: start a guest VM thread and return the initial render JSON
// =============================================================================

pub fn run_gui(output: CompileOutput) -> Result<(String, GuestHandle), String> {
    let (event_tx, event_rx) = mpsc::sync_channel::<(i32, String)>(0);
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<String, String>>(1);

    std::thread::spawn(move || {
        run_gui_thread(output, render_tx, event_rx);
    });

    // Wait for initial render
    let initial = render_rx
        .recv()
        .map_err(|e| format!("guest thread died: {}", e))?
        .map_err(|e| format!("guest init failed: {}", e))?;

    Ok((initial, GuestHandle { event_tx, render_rx }))
}

// =============================================================================
// send_gui_event: post an event and wait for new render JSON
// =============================================================================

pub fn send_gui_event(handle: &mut GuestHandle, handler_id: i32, payload: &str) -> Result<String, String> {
    handle.event_tx
        .send((handler_id, payload.to_string()))
        .map_err(|_| "guest VM stopped".to_string())?;

    handle.render_rx
        .recv()
        .map_err(|_| "guest VM stopped".to_string())?
        .map_err(|e| format!("guest event failed: {}", e))
}

// =============================================================================
// Guest thread body
// =============================================================================

fn build_gui_vm(output: CompileOutput) -> Result<Vm, String> {
    let ext_loader = if output.extensions.is_empty() {
        None
    } else {
        use vo_runtime::ext_loader::ExtensionLoader;
        let mut loader = ExtensionLoader::new();
        for manifest in &output.extensions {
            loader.load(&manifest.native_path, &manifest.name)
                .map_err(|e| format!("failed to load extension '{}': {}", manifest.name, e))?;
        }
        Some(loader)
    };

    let mut vm = Vm::new();
    vm.load_with_extensions(output.module, ext_loader.as_ref());
    Ok(vm)
}

fn invoke_gui_event(vm: &mut Vm, handler: GcRef, handler_id: i32, payload: &str) -> Result<(), String> {
    let func_id = closure::func_id(handler);

    // Clone func_def before mutably borrowing vm.state.gc (borrow checker requires separation)
    let func_def = vm.module().expect("module not set").functions[func_id as usize].clone();

    // Allocate payload string in GC
    let payload_ref = vo_runtime::objects::string::from_rust_str(&mut vm.state.gc, payload);

    // Build args: handler_id (int, 1 slot), payload (string/GcRef, 1 slot)
    let raw_args: [u64; 2] = [handler_id as u64, payload_ref as u64];
    // SAFETY: raw_args is stack-allocated and lives for the duration of this call.
    let full_args = unsafe {
        build_closure_args(
            handler as u64,
            handler,
            &func_def,
            raw_args.as_ptr(),
            raw_args.len() as u32,
        )
    };

    vm.spawn_call(func_id, &full_args);
    // Blocked is expected: GUI goroutines wait on channels between events.
    vm.run_scheduled()
        .map(|_| ())
        .map_err(|e| format!("{:?}", e))
}

fn run_gui_thread(
    output: CompileOutput,
    render_tx: mpsc::SyncSender<Result<String, String>>,
    event_rx: mpsc::Receiver<(i32, String)>,
) {
    let mut vm = match build_gui_vm(output) {
        Ok(vm) => vm,
        Err(e) => {
            let _ = render_tx.send(Err(e));
            return;
        }
    };

    // Run until vogui app calls emitRender + blocks on <-doneChan.
    // `Blocked` is expected here: all goroutines park on channels awaiting events.
    vogui::clear_pending_render();
    vogui::clear_pending_handler();
    if let Err(e) = vm.run() {
        let _ = render_tx.send(Err(format!("{:?}", e)));
        return;
    }

    let json = vogui::take_pending_render().unwrap_or_default();
    let _ = render_tx.send(Ok(json));

    // Persist the event handler: registerEventHandler is called once by vogui::Run.
    let handler = match vogui::take_pending_handler() {
        Some(h) => h,
        None => {
            let _ = render_tx.send(Err("vogui app did not register event handler".to_string()));
            return;
        }
    };

    // Event loop: blocked waiting for events from the IDE thread.
    while let Ok((handler_id, payload)) = event_rx.recv() {
        vogui::clear_pending_render();

        if let Err(e) = invoke_gui_event(&mut vm, handler, handler_id, &payload) {
            let _ = render_tx.send(Err(e));
            return;
        }

        let json = vogui::take_pending_render().unwrap_or_default();
        let _ = render_tx.send(Ok(json));
    }
    // event_rx closed (StopGui dropped the sender) — thread exits cleanly.
}
