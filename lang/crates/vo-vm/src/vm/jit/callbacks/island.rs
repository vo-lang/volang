//! JIT callback for island creation.

use vo_runtime::jit_api::{
    set_jit_infra_error, JitContext, JIT_CALLBACK_CREATE_ISLAND,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::vm::Vm;

/// JIT callback to create a new island.
/// Returns the island handle as u64.
pub extern "C" fn jit_create_island(ctx: *mut JitContext) -> u64 {
    let ctx_ptr = ctx;
    let ctx = unsafe { &mut *ctx_ptr };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    #[cfg(feature = "std")]
    let handle = match unsafe { ctx.module.as_ref() }
        .ok_or(())
        .and_then(|module| vm.create_island_for_execution(module).map_err(|_| ()))
    {
        Ok(handle) => handle,
        Err(_) => {
            let _ = set_jit_infra_error(
                ctx_ptr,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_CALLBACK_CREATE_ISLAND,
            );
            return 0;
        }
    };
    #[cfg(not(feature = "std"))]
    let handle = {
        let island_id = vm.state.next_island_id;
        vm.state.next_island_id += 1;
        vo_runtime::island::create(&mut vm.state.gc, island_id)
    };
    handle as u64
}
