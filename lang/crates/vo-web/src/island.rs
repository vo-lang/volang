//! Island transport VM wrapper for WASM targets.
//!
//! Provides `VoVmIsland` — a wasm_bindgen class for render islands
//! that communicate with native logic islands via transport frames.

use vo_runtime::island_msg::{decode_island_transport_frame, encode_island_transport_frame};
use vo_vm::vm::SchedulingOutcome;
use wasm_bindgen::prelude::*;

use crate::vm::Vm;

fn outcome_to_string(outcome: SchedulingOutcome) -> String {
    match outcome {
        SchedulingOutcome::Completed => "completed".into(),
        SchedulingOutcome::Suspended => "suspended".into(),
        SchedulingOutcome::SuspendedForHostEvents => "suspended_for_host_events".into(),
        SchedulingOutcome::Blocked => "blocked".into(),
        SchedulingOutcome::Panicked => "panicked".into(),
    }
}

/// A VM instance for JS interop with island transport support.
/// Used by render islands to communicate with native logic islands.
/// JS name is VoVmIsland to avoid conflict with studio-wasm's VoVm class.
#[wasm_bindgen(js_name = "VoVmIsland")]
pub struct VoVm {
    inner: Vm,
}

#[wasm_bindgen(js_class = "VoVmIsland")]
impl VoVm {
    /// Create a new VM from bytecode with stdlib + wasm platform externs.
    /// Does NOT run initialization — call `run` after setup.
    #[wasm_bindgen(constructor)]
    pub fn new(bytecode: &[u8]) -> Result<VoVm, JsValue> {
        let vm =
            crate::vm::create_loaded_vm(bytecode, |_, _| {}).map_err(|e| JsValue::from_str(&e))?;
        Ok(VoVm { inner: vm })
    }

    /// Create a VM with extension support.
    /// `register_ext_bridges` must have been called before this.
    #[wasm_bindgen(js_name = "withExterns")]
    pub fn with_externs(bytecode: &[u8]) -> Result<VoVm, JsValue> {
        let vm =
            crate::vm::create_loaded_vm(bytecode, crate::vm::ext_bridge::register_wasm_ext_bridges)
                .map_err(|e| JsValue::from_str(&e))?;
        Ok(VoVm { inner: vm })
    }

    /// Run the VM until suspended or completed.
    /// Returns: "completed", "suspended", "suspended_for_host_events", "blocked", or "error".
    pub fn run(&mut self) -> String {
        match self.inner.run() {
            Ok(outcome) => outcome_to_string(outcome),
            Err(e) => format!("error: {:?}", e),
        }
    }

    /// Run scheduled fibers until suspended or completed.
    #[wasm_bindgen(js_name = "runScheduled")]
    pub fn run_scheduled(&mut self) -> String {
        match self.inner.run_scheduled() {
            Ok(outcome) => outcome_to_string(outcome),
            Err(e) => format!("error: {:?}", e),
        }
    }

    /// Push an island command (encoded as transport frame bytes) into the VM.
    /// The frame is decoded and queued for processing.
    #[wasm_bindgen(js_name = "pushIslandCommand")]
    pub fn push_island_command(&mut self, frame: &[u8]) -> Result<(), JsValue> {
        let (target_island_id, cmd) = decode_island_transport_frame(frame)
            .map_err(|e| JsValue::from_str(&format!("invalid island transport frame: {:?}", e)))?;
        let current_island_id = self.inner.current_island_id();
        if current_island_id == 0 {
            self.inner.set_island_id(target_island_id);
        } else if current_island_id != target_island_id {
            return Err(JsValue::from_str(&format!(
                "render island id mismatch: have {}, got {}",
                current_island_id, target_island_id
            )));
        }
        self.inner.push_island_command(cmd);
        Ok(())
    }

    /// Take all pending outbound island commands.
    /// Returns an array of transport frame bytes (each frame includes target island ID).
    #[wasm_bindgen(js_name = "takeOutboundCommands")]
    pub fn take_outbound_commands(&mut self) -> js_sys::Array {
        let commands = self.inner.take_outbound_commands();
        let arr = js_sys::Array::new();
        for (target_island_id, cmd) in commands {
            let bytes = encode_island_transport_frame(target_island_id, &cmd);
            let uint8 = js_sys::Uint8Array::from(bytes.as_slice());
            arr.push(&uint8);
        }
        arr
    }

    /// Take captured stdout output.
    #[wasm_bindgen(js_name = "takeOutput")]
    pub fn take_output(&self) -> String {
        vo_runtime::output::take_output()
    }

    /// Check if VM has pending outbound commands.
    #[wasm_bindgen(js_name = "hasOutboundCommands")]
    pub fn has_outbound_commands(&self) -> bool {
        !self.inner.state.outbound_commands.is_empty()
    }

    /// Take pending host events (timers, async callbacks).
    /// Returns array of {token, delayMs, replay} objects.
    #[wasm_bindgen(js_name = "takePendingHostEvents")]
    pub fn take_pending_host_events(&mut self) -> js_sys::Array {
        let events = self.inner.scheduler.take_pending_host_events();
        let arr = js_sys::Array::new();
        for ev in events {
            let obj = js_sys::Object::new();
            let _ = js_sys::Reflect::set(&obj, &"token".into(), &ev.token.to_string().into());
            let _ = js_sys::Reflect::set(&obj, &"delayMs".into(), &(ev.delay_ms as f64).into());
            let _ = js_sys::Reflect::set(&obj, &"replay".into(), &ev.replay.into());
            arr.push(&obj);
        }
        arr
    }

    /// Wake a fiber blocked on host event.
    #[wasm_bindgen(js_name = "wakeHostEvent")]
    pub fn wake_host_event(&mut self, token: &str) {
        if let Ok(t) = token.parse::<u64>() {
            self.inner.wake_host_event(t);
        }
    }
}
