//! JIT callback for island creation.

use vo_runtime::jit_api::JitContext;

use crate::vm::Vm;

/// JIT callback to create a new island.
/// Returns the island handle as u64.
pub extern "C" fn jit_create_island(ctx: *mut JitContext) -> u64 {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    #[cfg(feature = "std")]
    let handle = vm.create_island();
    #[cfg(not(feature = "std"))]
    let handle = {
        let island_id = vm.state.next_island_id;
        vm.state.next_island_id += 1;
        vo_runtime::island::create(&mut vm.state.gc, island_id)
    };
    handle as u64
}
