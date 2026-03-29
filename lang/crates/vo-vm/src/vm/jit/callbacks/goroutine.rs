//! JIT callbacks for goroutine spawning.

use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::JitContext;
use vo_runtime::objects::closure;

use crate::fiber::Fiber;
use crate::vm::{helpers, Vm};

/// Create a new fiber from a closure and spawn it on the scheduler.
///
/// Handles: func_id resolution, Fiber creation, push_frame, call_layout arg copy, and spawn.
unsafe fn spawn_closure_fiber(
    vm: &mut Vm,
    module: &vo_runtime::bytecode::Module,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) {
    let new_fiber = helpers::build_closure_fiber_from_args_ptr(
        &module.functions,
        vm.scheduler.fibers.len() as u32,
        closure_ref,
        args_ptr,
        arg_slots,
    );
    vm.scheduler.spawn(new_fiber);
}

/// JIT callback to spawn a new goroutine.
/// This is fire-and-forget - creates a new fiber and adds it to the scheduler.
pub extern "C" fn jit_go_start(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure_call: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };

    if is_closure_call != 0 {
        unsafe { spawn_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) };
    } else {
        // Regular function: args start at reg[0]
        let func = &module.functions[func_id as usize];
        let next_id = vm.scheduler.fibers.len() as u32;
        let mut new_fiber = Fiber::new(next_id);
        new_fiber.push_frame(func_id, func.local_slots, func.gc_scan_slots, 0, 0);
        let new_stack = new_fiber.stack_ptr();
        for i in 0..arg_slots as usize {
            unsafe { *new_stack.add(i) = *args_ptr.add(i) };
        }
        vm.scheduler.spawn(new_fiber);
    }
}

/// JIT callback to spawn a goroutine on a specific island.
/// If island_id == 0, spawns locally. Otherwise sends to remote island via command channel.
pub extern "C" fn jit_go_island(
    ctx: *mut JitContext,
    island: u64,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };

    let island_handle = island as GcRef;
    let closure = closure_ref as GcRef;
    let island_id = vo_runtime::island::id(island_handle);

    if island_id == vm.state.current_island_id {
        unsafe { spawn_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) };
    } else {
        let func_id = closure::func_id(closure);
        let capture_count = closure::capture_count(closure);

        let mut capture_data = Vec::with_capacity(capture_count);
        for i in 0..capture_count {
            capture_data.push(closure::get_capture(closure, i));
        }

        let mut arg_data = Vec::with_capacity(arg_slots as usize);
        for i in 0..arg_slots as usize {
            arg_data.push(unsafe { *args_ptr.add(i) });
        }

        let func_def = &module.functions[func_id as usize];
        let result = crate::exec::GoIslandResult {
            island: island_handle,
            func_id,
            capture_data,
            arg_data,
        };

        crate::exec::prepare_queue_handles_for_transfer(
            &result,
            island_id,
            &func_def.capture_types,
            &func_def.param_types,
            &module.struct_metas,
            &module.runtime_types,
            &mut vm.state,
        );
        let data = crate::exec::pack_closure_for_island(
            &vm.state.gc,
            &result,
            &func_def.capture_types,
            &func_def.param_types,
            &module.struct_metas,
            &module.runtime_types,
        );
        let closure_data = vo_runtime::pack::PackedValue::from_data(data);
        vm.state.send_spawn_fiber_to_island(island_id, closure_data);
    }
}
