//! JIT callback for island creation.

use vo_runtime::jit_api::{
    set_jit_infra_error_with_message, JitContext, JitResult, JIT_CALLBACK_CREATE_ISLAND,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::vm::jit::callbacks::helpers::{
    extract_vm, validate_callback_raw_slot_span, validate_vm_callback_context,
};

/// JIT callback to create a new island.
pub extern "C" fn jit_create_island(ctx: *mut JitContext, out: *mut u64) -> JitResult {
    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_CALLBACK_CREATE_ISLAND,
    ) {
        return result;
    }
    if let Err(result) = validate_callback_raw_slot_span(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_CALLBACK_CREATE_ISLAND,
        out,
        1,
    ) {
        return result;
    }
    let ctx_ptr = ctx;
    let mut vm = unsafe { extract_vm(ctx_ptr) };
    let handle = match vm
        .create_island()
        .map_err(|error| format!("JIT island creation failed: {error:?}"))
    {
        Ok(handle) => handle,
        Err(message) => {
            return set_jit_infra_error_with_message(
                ctx_ptr,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_CALLBACK_CREATE_ISLAND,
                message,
            );
        }
    };
    unsafe {
        *out = handle as u64;
    }
    JitResult::Ok
}
