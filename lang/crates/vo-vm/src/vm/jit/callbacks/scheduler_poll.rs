//! Cooperative scheduler poll for an exhausted native execution lease.

use vo_runtime::jit_api::JitContext;

use super::helpers::extract_vm;

/// Return a renewed budget while no scheduler-owned work is waiting. A zero
/// result tells generated code to materialize its exact frame state and yield.
pub extern "C" fn jit_refill_execution_budget(ctx: *mut JitContext, required_budget: u32) -> u32 {
    let Some(ctx_ref) = (unsafe { ctx.as_ref() }) else {
        return 0;
    };
    if ctx_ref.callback_state.is_null() {
        return 0;
    }
    let vm = unsafe { extract_vm(ctx) };
    vm.refill_execution_budget(required_budget)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_context_requests_scheduler_boundary() {
        assert_eq!(jit_refill_execution_budget(core::ptr::null_mut(), 1), 0);
    }
}
