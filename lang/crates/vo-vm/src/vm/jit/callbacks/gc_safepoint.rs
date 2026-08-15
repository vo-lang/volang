//! Managed-heap poll at allocation-capable JIT helper boundaries.

use vo_runtime::jit_api::{
    set_jit_infra_error_with_message, JitContext, JitResult, JIT_CALLBACK_GC_SAFEPOINT,
};

use super::helpers::{extract_context, validate_vm_callback_context};

pub extern "C" fn jit_gc_safepoint(ctx: *mut JitContext) -> JitResult {
    if let Err(result) = validate_vm_callback_context(ctx, JIT_CALLBACK_GC_SAFEPOINT, 0) {
        return result;
    }
    let (mut vm, fiber) = unsafe { extract_context(ctx) };
    if !vm.gc_should_step() {
        return JitResult::Ok;
    }

    let native_frame = unsafe { (*ctx).native_frame };
    if let Err(error) = unsafe { vm.gc_step_while_native(fiber, native_frame, ctx) } {
        return set_jit_infra_error_with_message(
            ctx,
            JIT_CALLBACK_GC_SAFEPOINT,
            1,
            error.to_string(),
        );
    }
    JitResult::Ok
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_context_fails_closed() {
        assert_eq!(jit_gc_safepoint(core::ptr::null_mut()), JitResult::JitError);
    }
}
