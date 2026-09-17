//! Compatibility methods for the previous UI kernel. The core Island transport
//! and replacement UI do not depend on these methods or their state arena.

use super::{outcome_to_string, VoVm};
use vo_vm::vm::SchedulingOutcome;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(js_class = "VoVmIsland")]
impl VoVm {
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
}
