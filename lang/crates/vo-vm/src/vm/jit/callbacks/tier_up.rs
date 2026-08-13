//! Optimizing-tier publication at a baseline function-entry safe point.

use vo_runtime::jit_api::{set_jit_infra_error, JitContext, JitResult, JIT_CALLBACK_TIER_UP};

use super::helpers::{extract_vm, validate_vm_callback_context};

pub extern "C" fn jit_tier_up(ctx: *mut JitContext, func_id: u32) -> JitResult {
    if let Err(result) = validate_vm_callback_context(ctx, JIT_CALLBACK_TIER_UP, func_id as u64) {
        return result;
    }
    let mut vm = unsafe { extract_vm(ctx) };
    match vm.tier_up(func_id) {
        Ok(()) => JitResult::Ok,
        Err(_) => set_jit_infra_error(ctx, JIT_CALLBACK_TIER_UP, func_id as u64),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_context_fails_closed() {
        assert_eq!(jit_tier_up(core::ptr::null_mut(), 0), JitResult::JitError);
    }
}
