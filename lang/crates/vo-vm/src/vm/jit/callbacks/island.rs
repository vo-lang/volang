//! JIT callback for island creation.

use vo_runtime::jit_api::{
    set_jit_infra_error_with_message, JitContext, JIT_CALLBACK_CREATE_ISLAND,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::vm::jit::callbacks::helpers::validate_vm_callback_context;
use crate::vm::Vm;

/// JIT callback to create a new island.
/// Returns the island handle as u64.
pub extern "C" fn jit_create_island(ctx: *mut JitContext) -> u64 {
    if validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_CALLBACK_CREATE_ISLAND,
    )
    .is_err()
    {
        return 0;
    }
    let ctx_ptr = ctx;
    let ctx = unsafe { &mut *ctx_ptr };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    #[cfg(feature = "std")]
    let handle = match unsafe { ctx.module.as_ref() }
        .ok_or_else(|| "JIT island creation missing active module".to_string())
        .and_then(|module| {
            vm.create_island_for_execution(module)
                .map_err(|error| format!("JIT island creation failed: {error:?}"))
        }) {
        Ok(handle) => handle,
        Err(message) => {
            let _ = set_jit_infra_error_with_message(
                ctx_ptr,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_CALLBACK_CREATE_ISLAND,
                message,
            );
            return 0;
        }
    };
    #[cfg(not(feature = "std"))]
    let handle = {
        let island_id = match vm.state.allocate_island_id() {
            Ok(island_id) => island_id,
            Err(error) => {
                let _ = set_jit_infra_error_with_message(
                    ctx_ptr,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_CALLBACK_CREATE_ISLAND,
                    error.to_string(),
                );
                return 0;
            }
        };
        vo_runtime::island::create(&mut vm.state.gc, island_id)
    };
    handle as u64
}
