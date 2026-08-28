//! Island transport VM wrapper for WASM targets.
//!
//! Provides `VoVmIsland` — a wasm_bindgen class for render islands
//! that communicate with native logic islands via transport frames.

use vo_runtime::island_msg::decode_island_transport_frame;
use vo_vm::vm::SchedulingOutcome;
use wasm_bindgen::prelude::*;

use crate::vm::Vm;

fn outcome_to_string(outcome: SchedulingOutcome) -> String {
    match outcome {
        SchedulingOutcome::Completed => "completed".into(),
        SchedulingOutcome::Exited(_) => "exited".into(),
        SchedulingOutcome::Suspended => "suspended".into(),
        SchedulingOutcome::SuspendedForHostEvents => "suspended_for_host_events".into(),
        SchedulingOutcome::Blocked => "blocked".into(),
        SchedulingOutcome::Panicked => "panicked".into(),
    }
}

/// A VM instance for JS interop with island transport support.
/// Used by render islands to communicate with native logic islands.
/// The stable host name keeps existing browser embedders source-compatible.
#[wasm_bindgen(js_name = "VoVmIsland")]
pub struct VoVm {
    inner: Vm,
}

#[wasm_bindgen(js_class = "VoVmIsland")]
impl VoVm {
    fn memory_admission(
        reserve_bytes: u64,
        hard_limit_bytes: u64,
        maximum_pages: u32,
        growth_allowed: bool,
    ) -> crate::vm::WasmMemoryAdmission {
        crate::vm::WasmMemoryAdmission {
            reserve_bytes,
            hard_limit_bytes: (hard_limit_bytes != 0).then_some(hard_limit_bytes),
            maximum_pages: (maximum_pages != 0).then_some(u64::from(maximum_pages)),
            growth_allowed,
            allocation_allowed: true,
            gc_mode: vo_runtime::gc::GcMode::Generational,
            automatic_gc: true,
            oom_policy: if growth_allowed {
                vo_runtime::gc::OomPolicy::CollectThenTerminateIsland
            } else {
                vo_runtime::gc::OomPolicy::TerminateIsland
            },
        }
    }

    /// Create a new VM from bytecode with stdlib + wasm platform externs.
    /// Does NOT run initialization — call `run` after setup.
    #[wasm_bindgen(constructor)]
    pub fn new(bytecode: &[u8]) -> Result<VoVm, JsValue> {
        let vm = crate::vm::create_loaded_vm(bytecode, |_, _| Ok(()))
            .map_err(|e| JsValue::from_str(&e))?;
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

    /// Create a VM after validating and pre-growing WebAssembly linear memory.
    /// A zero hard limit or maximum means that bound was not supplied.
    #[wasm_bindgen(js_name = "withMemory")]
    pub fn with_memory(
        bytecode: &[u8],
        reserve_bytes: u64,
        hard_limit_bytes: u64,
        maximum_pages: u32,
        growth_allowed: bool,
    ) -> Result<VoVm, JsValue> {
        let admission = Self::memory_admission(
            reserve_bytes,
            hard_limit_bytes,
            maximum_pages,
            growth_allowed,
        );
        let vm = crate::vm::create_loaded_vm_with_memory(bytecode, |_, _| Ok(()), admission)
            .map_err(|error| JsValue::from_str(&error))?;
        Ok(VoVm { inner: vm })
    }

    /// Memory-admitted constructor with generic WASM extension bridges.
    #[wasm_bindgen(js_name = "withExternsAndMemory")]
    pub fn with_externs_and_memory(
        bytecode: &[u8],
        reserve_bytes: u64,
        hard_limit_bytes: u64,
        maximum_pages: u32,
        growth_allowed: bool,
    ) -> Result<VoVm, JsValue> {
        let admission = Self::memory_admission(
            reserve_bytes,
            hard_limit_bytes,
            maximum_pages,
            growth_allowed,
        );
        let vm = crate::vm::create_loaded_vm_with_memory(
            bytecode,
            crate::vm::ext_bridge::register_wasm_ext_bridges,
            admission,
        )
        .map_err(|error| JsValue::from_str(&error))?;
        Ok(VoVm { inner: vm })
    }

    /// Run the VM until suspended or completed.
    /// Returns: "completed", "exited", "suspended", "suspended_for_host_events", "blocked", or "error".
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

    /// Replace the development bytecode module while preserving compatible
    /// component state. The old UI arena remains restorable until the new
    /// module reaches its initial host-event suspension successfully.
    pub fn reload(&mut self, bytecode: &[u8]) -> Result<String, JsValue> {
        let (mut replacement, prepared) = crate::vm::create_loaded_ui_reload_vm(bytecode)
            .map_err(|error| JsValue::from_str(&error))?;
        let checkpoint =
            vo_ui_vm::begin_reload_with_bundle(prepared.component, prepared.component_bundle)
                .map_err(|error| JsValue::from_str(&error.to_string()))?;
        let outcome = replacement.run().map_err(|error| {
            JsValue::from_str(&format!("UI replacement VM failed to start: {error:?}"))
        })?;
        if outcome != SchedulingOutcome::SuspendedForHostEvents {
            return Err(JsValue::from_str(&format!(
                "UI replacement stopped before mounting: {}",
                outcome_to_string(outcome)
            )));
        }
        checkpoint.commit();
        self.inner = replacement;
        Ok(outcome_to_string(outcome))
    }

    /// Push a frame received from the host's trusted island transport into the
    /// VM. Decoding alone does not authenticate the frame source.
    #[wasm_bindgen(js_name = "pushIslandCommand")]
    pub fn push_island_command(&mut self, frame: &[u8]) -> Result<(), JsValue> {
        let (target_island_id, source_island_id, cmd) = decode_island_transport_frame(frame)
            .map_err(|e| JsValue::from_str(&format!("invalid island transport frame: {e}")))?;
        self.inner
            .push_targeted_island_command_from(source_island_id, target_island_id, cmd)
            .map_err(|error| {
                JsValue::from_str(&format!("render island command rejected: {error}"))
            })?;
        Ok(())
    }

    /// Take all pending outbound island commands.
    /// Returns transport frames containing target and source island IDs.
    #[wasm_bindgen(js_name = "takeOutboundCommands")]
    pub fn take_outbound_commands(&mut self) -> Result<js_sys::Array, JsValue> {
        let frames = self
            .inner
            .try_take_outbound_transport_frames()
            .map_err(|error| {
                JsValue::from_str(&format!(
                    "failed to encode outbound island transport frame: {error}"
                ))
            })?;
        let arr = js_sys::Array::new();
        for frame in frames {
            let uint8 = js_sys::Uint8Array::from(frame.as_slice());
            arr.push(&uint8);
        }
        Ok(arr)
    }

    /// Take captured stdout output.
    #[wasm_bindgen(js_name = "takeOutput")]
    pub fn take_output(&self) -> String {
        vo_runtime::output::take_output()
    }

    /// Take the latest versioned UI mutation frame emitted by `ui.Mount`.
    #[wasm_bindgen(js_name = "takeHostOutput")]
    pub fn take_host_output(&mut self) -> Option<js_sys::Uint8Array> {
        self.inner
            .take_host_output()
            .map(|frame| js_sys::Uint8Array::from(frame.as_slice()))
    }

    /// Take one coalesced UI invalidation requested by worker goroutines.
    #[wasm_bindgen(js_name = "takeUiInvalidation")]
    pub fn take_ui_invalidation(&mut self) -> bool {
        vo_ui_vm::take_invalidation_request()
    }

    /// Synchronize browser history into the renderer-neutral UI provider.
    #[wasm_bindgen(js_name = "setUiLocation")]
    pub fn set_ui_location(&mut self, path: &str, invalidate: bool) -> Result<bool, JsValue> {
        vo_ui_vm::set_location_from_host(path, invalidate).map_err(JsValue::from_str)
    }

    /// Synchronize the browser's logical viewport into the renderer-neutral
    /// environment before a render turn. Resize invalidation is coalesced by
    /// the UI VM so a burst of browser events publishes one fresh tree.
    #[wasm_bindgen(js_name = "setUiViewport")]
    pub fn set_ui_viewport(
        &mut self,
        width: f64,
        height: f64,
        scale_factor: f64,
        invalidate: bool,
    ) -> Result<bool, JsValue> {
        vo_ui_vm::set_platform_viewport(width, height, scale_factor, invalidate)
            .map_err(JsValue::from_str)
    }

    /// Drain bounded navigation commands issued by Volang handlers.
    #[wasm_bindgen(js_name = "takeUiNavigationRequests")]
    pub fn take_ui_navigation_requests(&mut self) -> js_sys::Array {
        let requests = vo_ui_vm::take_navigation_requests();
        let result = js_sys::Array::new();
        for request in requests {
            let object = js_sys::Object::new();
            let (kind, path) = match request {
                vo_ui_vm::NavigationRequest::Push(path) => ("push", Some(path)),
                vo_ui_vm::NavigationRequest::Replace(path) => ("replace", Some(path)),
                vo_ui_vm::NavigationRequest::Back => ("back", None),
                vo_ui_vm::NavigationRequest::Forward => ("forward", None),
            };
            let _ = js_sys::Reflect::set(&object, &"kind".into(), &kind.into());
            if let Some(path) = path {
                let _ = js_sys::Reflect::set(&object, &"path".into(), &path.into());
            }
            result.push(&object);
        }
        result
    }

    /// Drain renderer-independent VUS1 requests issued by UI goroutines.
    #[wasm_bindgen(js_name = "takeUiSystemRequests")]
    pub fn take_ui_system_requests(&mut self) -> js_sys::Array {
        let result = js_sys::Array::new();
        for request in vo_ui_vm::take_system_requests() {
            let object = js_sys::Object::new();
            let _ = js_sys::Reflect::set(
                &object,
                &"requestId".into(),
                &request.request_id.to_string().into(),
            );
            let frame = js_sys::Uint8Array::from(request.frame.as_slice());
            let _ = js_sys::Reflect::set(&object, &"frame".into(), &frame);
            result.push(&object);
        }
        result
    }

    /// Process exit status supplied by `os.Exit`, or `undefined` when the VM has
    /// not exited explicitly.
    #[wasm_bindgen(getter, js_name = "exitCode")]
    pub fn exit_code(&self) -> Option<i32> {
        self.inner.exit_code()
    }

    /// Check if VM has pending outbound commands.
    #[wasm_bindgen(js_name = "hasOutboundCommands")]
    pub fn has_outbound_commands(&self) -> bool {
        self.inner.has_outbound_commands()
    }

    /// Take pending host events (timers, async callbacks).
    /// Returns array of {key, source, token, delayMs, replay} objects.
    #[wasm_bindgen(js_name = "takePendingHostEvents")]
    pub fn take_pending_host_events(&mut self) -> js_sys::Array {
        let events = self.inner.take_pending_host_events();
        let arr = js_sys::Array::new();
        for ev in events {
            let obj = js_sys::Object::new();
            let _ = js_sys::Reflect::set(&obj, &"key".into(), &ev.key.encode().into());
            let _ = js_sys::Reflect::set(&obj, &"source".into(), &ev.source.as_str().into());
            let _ = js_sys::Reflect::set(&obj, &"token".into(), &ev.token.to_string().into());
            let _ = js_sys::Reflect::set(&obj, &"delayMs".into(), &(ev.delay_ms as f64).into());
            let _ = js_sys::Reflect::set(&obj, &"replay".into(), &ev.replay.into());
            arr.push(&obj);
        }
        arr
    }

    /// Wake a fiber blocked on host event.
    #[wasm_bindgen(js_name = "wakeHostEvent")]
    pub fn wake_host_event(&mut self, key: &str) -> bool {
        let Ok(key) = vo_vm::scheduler::HostWaitKey::decode(key) else {
            return false;
        };
        self.inner.wake_host_event(key)
    }

    /// Wake a replaying host event and attach a bounded protocol frame.
    #[wasm_bindgen(js_name = "wakeHostEventWithData")]
    pub fn wake_host_event_with_data(&mut self, key: &str, data: &[u8]) -> bool {
        let Ok(key) = vo_vm::scheduler::HostWaitKey::decode(key) else {
            return false;
        };
        self.inner.wake_host_event_with_data(key, data.to_vec())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::island::IslandCommand;
    use vo_runtime::island_msg::encode_island_transport_frame;

    #[test]
    fn inbound_transport_distinguishes_source_from_target() {
        const TARGET_ISLAND_ID: u32 = 17;
        const SOURCE_ISLAND_ID: u32 = 29;

        let adopt_target = encode_island_transport_frame(
            TARGET_ISLAND_ID,
            TARGET_ISLAND_ID,
            &IslandCommand::Shutdown,
        )
        .expect("target-adoption frame must encode");
        let distinct_source = encode_island_transport_frame(
            TARGET_ISLAND_ID,
            SOURCE_ISLAND_ID,
            &IslandCommand::Shutdown,
        )
        .expect("test transport frame must encode");
        let mut vm = VoVm { inner: Vm::new() };

        vm.push_island_command(&adopt_target)
            .expect("first frame must establish the target island");
        vm.push_island_command(&distinct_source)
            .expect("source identity must not be mistaken for the established target");
        assert_eq!(vm.inner.current_island_id(), TARGET_ISLAND_ID);
    }
}
