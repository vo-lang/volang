//! Guest GUI VM management: RunGui, SendGuiEvent, StopGui.
//!
//! Each guest GUI app runs on a dedicated OS thread with its own TLS-isolated
//! PENDING_RENDER / PENDING_EVENT state, so multiple guest VMs never interfere.
//!
//! This module is only available on non-WASM targets; WASM builds return errors from the externs.

#![cfg(not(target_arch = "wasm32"))]

use std::sync::{mpsc, Mutex};
use vo_engine::CompileOutput;
use vo_vm::vm::Vm;

// =============================================================================
// GuestHandle
// =============================================================================

pub struct GuestHandle {
    pub event_tx: mpsc::SyncSender<(i32, String)>,
    pub render_rx: mpsc::Receiver<Result<Vec<u8>, String>>,
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

pub fn run_gui(output: CompileOutput) -> Result<(Vec<u8>, GuestHandle), String> {
    let (event_tx, event_rx) = mpsc::sync_channel::<(i32, String)>(0);
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<Vec<u8>, String>>(1);

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

pub fn send_gui_event(handle: &mut GuestHandle, handler_id: i32, payload: &str) -> Result<Vec<u8>, String> {
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

fn run_gui_thread(
    output: CompileOutput,
    render_tx: mpsc::SyncSender<Result<Vec<u8>, String>>,
    event_rx: mpsc::Receiver<(i32, String)>,
) {
    let mut vm = match build_gui_vm(output) {
        Ok(vm) => vm,
        Err(e) => {
            let _ = render_tx.send(Err(e));
            return;
        }
    };

    // Run until vogui app blocks on waitForEvent().
    // Returns SuspendedForHostEvents once the main fiber blocks.
    vogui::clear_pending_render();
    vogui::clear_event_state();
    if let Err(e) = vm.run() {
        let _ = render_tx.send(Err(format!("{:?}", e)));
        return;
    }

    let bytes = vogui::take_pending_render_bytes().unwrap_or_default();
    let _ = render_tx.send(Ok(bytes));

    // Event loop: blocked waiting for events from the IDE thread.
    // Each event wakes the main fiber (blocked on waitForEvent), which processes
    // the event and blocks again. No new fiber is spawned per event.
    while let Ok((handler_id, payload)) = event_rx.recv() {
        vogui::clear_pending_render();
        vo_runtime::output::clear_output();

        let token = match vogui::send_event(handler_id, payload) {
            Some(t) => t,
            None => {
                let _ = render_tx.send(Err("Main fiber not waiting for events".to_string()));
                return;
            }
        };

        vm.scheduler.wake_host_event(token);
        if let Err(e) = vm.run_scheduled() {
            let _ = render_tx.send(Err(format!("{:?}", e)));
            return;
        }

        let bytes = vogui::take_pending_render_bytes().unwrap_or_default();
        let _ = render_tx.send(Ok(bytes));
    }
    // event_rx closed (StopGui dropped the sender) — thread exits cleanly.
}
