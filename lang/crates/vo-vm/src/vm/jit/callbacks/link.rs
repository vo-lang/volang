//! Lazy native linking for cold call targets.

use vo_runtime::jit_api::{set_jit_infra_error, JitContext, JitResult};

use super::helpers::{extract_vm, validate_vm_callback_context};

const JIT_LINK_FUNCTION_ERROR: u64 = 19;

/// Compile and publish a baseline function without unwinding the active native
/// caller chain. The generated caller reloads the dispatch entry afterwards;
/// an unavailable entry selects the established VM materialization path.
pub extern "C" fn jit_link_function(ctx: *mut JitContext, func_id: u32) -> JitResult {
    if let Err(result) =
        validate_vm_callback_context(ctx, JIT_LINK_FUNCTION_ERROR, u64::from(func_id))
    {
        return result;
    }
    let mut vm = unsafe { extract_vm(ctx) };
    match vm.link_function(func_id) {
        Ok(()) => JitResult::Ok,
        Err(_) => set_jit_infra_error(ctx, JIT_LINK_FUNCTION_ERROR, u64::from(func_id)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_context_fails_closed() {
        assert_eq!(
            jit_link_function(core::ptr::null_mut(), 0),
            JitResult::JitError
        );
    }
}
