//! Managed-heap poll at allocation-capable JIT helper boundaries.

use vo_runtime::jit_api::{set_jit_infra_error, JitContext, JitResult, JIT_CALLBACK_GC_SAFEPOINT};

use super::helpers::{extract_vm, validate_vm_callback_context};

pub extern "C" fn jit_gc_safepoint(ctx: *mut JitContext) -> JitResult {
    if let Err(result) = validate_vm_callback_context(ctx, JIT_CALLBACK_GC_SAFEPOINT, 0) {
        return result;
    }
    let mut vm = unsafe { extract_vm(ctx) };
    if !vm.gc_should_step() {
        return JitResult::Ok;
    }

    // Keep callback work bounded. A complete walk validates every active map;
    // a budget-limited walk is also safe because GcSafepoint first materializes
    // the VM frame chain and collection runs through the resumable VM scanner.
    const MAX_NATIVE_FRAMES_PER_POLL: usize = 256;
    const MAX_NATIVE_ROOTS_PER_POLL: usize = 16 * 1024;
    let native_frame = unsafe { (*ctx).native_frame };
    let scan = unsafe {
        vm.visit_native_roots(
            native_frame,
            ctx,
            MAX_NATIVE_FRAMES_PER_POLL,
            MAX_NATIVE_ROOTS_PER_POLL,
            |root| {
                // Read each mapped slot while native execution is paused. This
                // is the same location a future moving collector would update.
                core::hint::black_box(root.read());
            },
        )
    };
    if scan.is_err() {
        return set_jit_infra_error(ctx, JIT_CALLBACK_GC_SAFEPOINT, 1);
    }
    // The VM will materialize the exact current instruction and run a GC
    // slice. Preserve its identity before direct-call unwinding restores the
    // caller's current_func_id. The resumed JIT invocation may consume this
    // once to make forward progress under stress_every_step.
    unsafe {
        (*ctx).gc_poll_resume_func_id = (*ctx).current_func_id;
        (*ctx).gc_poll_resume_pc = (*ctx).call_resume_pc;
        (*ctx).gc_poll_resume_armed = 1;
    }
    JitResult::GcSafepoint
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_context_fails_closed() {
        assert_eq!(jit_gc_safepoint(core::ptr::null_mut()), JitResult::JitError);
    }
}
