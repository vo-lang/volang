//! JIT callbacks for goroutine spawning.

use vo_runtime::jit_api::JitContext;
use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

use crate::fiber::Fiber;
use crate::vm::Vm;

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
    let closure_gcref = closure_ref as GcRef;
    let func_id = closure::func_id(closure_gcref);
    let func = &module.functions[func_id as usize];

    let next_id = vm.scheduler.fibers.len() as u32;
    let mut new_fiber = Fiber::new(next_id);
    new_fiber.push_frame(func_id, func.local_slots, 0, 0);

    let layout = closure::call_layout(
        closure_ref,
        closure_gcref,
        func.recv_slots as usize,
        func.is_closure,
    );

    let new_stack = new_fiber.stack_ptr();
    if let Some(slot0_val) = layout.slot0 {
        *new_stack = slot0_val;
    }
    for i in 0..arg_slots as usize {
        *new_stack.add(layout.arg_offset + i) = *args_ptr.add(i);
    }

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
        new_fiber.push_frame(func_id, func.local_slots, 0, 0);
        let new_stack = new_fiber.stack_ptr();
        for i in 0..arg_slots as usize {
            unsafe { *new_stack.add(i) = *args_ptr.add(i) };
        }
        vm.scheduler.spawn(new_fiber);
    }
}

/// JIT callback to spawn a goroutine on a specific island.
/// If island_id == 0, spawns locally. Otherwise sends to remote island via command channel.
#[cfg(feature = "std")]
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
    
    if island_id == 0 {
        unsafe { spawn_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) };
    } else {
        // Remote spawn - pack closure and send to island
        let func_id = closure::func_id(closure);
        let capture_count = closure::capture_count(closure);
        
        // Read capture data from closure
        let mut capture_data = Vec::with_capacity(capture_count);
        for i in 0..capture_count {
            capture_data.push(closure::get_capture(closure, i));
        }
        
        // Read arg data from args_ptr
        let mut arg_data = Vec::with_capacity(arg_slots as usize);
        for i in 0..arg_slots as usize {
            arg_data.push(unsafe { *args_ptr.add(i) });
        }
        
        let func_def = &module.functions[func_id as usize];
        let result = crate::exec::GoIslandResult {
            island: island_handle,
            func_id,
            capture_data,
            capture_slots: capture_count as u16,
            arg_data,
            arg_slots: arg_slots as u16,
        };
        
        let data = crate::exec::pack_closure_for_island(
            &vm.state.gc, &result, &func_def.capture_types, &func_def.param_types,
            &module.struct_metas, &module.runtime_types,
        );
        let closure_data = vo_runtime::pack::PackedValue::from_data(data);
        let cmd = vo_runtime::island::IslandCommand::SpawnFiber { 
            closure_data, 
            capture_slots: capture_count as u16 
        };
        
        if let Some(ref registry) = vm.state.island_registry {
            if let Ok(guard) = registry.lock() {
                if let Some(tx) = guard.get(&island_id) { 
                    let _ = tx.send(cmd); 
                }
            }
        }
    }
}

#[cfg(not(feature = "std"))]
pub extern "C" fn jit_go_island(
    ctx: *mut JitContext,
    island: u64,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) {
    // In no_std mode, always spawn locally - match VM behavior
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    
    unsafe { spawn_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) };
    let _ = island; // suppress unused warning
}
