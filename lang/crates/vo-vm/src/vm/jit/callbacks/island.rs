//! JIT callback for island creation.

use vo_runtime::jit_api::JitContext;

use crate::vm::Vm;

/// JIT callback to create a new island.
/// Returns the island handle as u64.
#[cfg(feature = "std")]
pub extern "C" fn jit_create_island(ctx: *mut JitContext) -> u64 {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    
    let handle = vm.create_island();
    handle as u64
}

#[cfg(not(feature = "std"))]
pub extern "C" fn jit_create_island(ctx: *mut JitContext) -> u64 {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    
    // Create dummy main island handle for no_std
    let handle = vo_runtime::island::create_main(&mut vm.state.gc);
    handle as u64
}
