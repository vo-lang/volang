//! JIT callbacks for goroutine spawning.

use vo_runtime::jit_api::JitContext;

use crate::fiber::Fiber;
use crate::vm::Vm;

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
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::closure;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    
    let is_closure_call = is_closure_call != 0;
    
    // Get func_id: from closure if is_closure_call, otherwise from parameter
    let actual_func_id = if is_closure_call {
        closure::func_id(closure_ref as GcRef)
    } else {
        func_id
    };
    
    let func = &module.functions[actual_func_id as usize];
    
    // Create new fiber
    let next_id = vm.scheduler.fibers.len() as u32;
    let mut new_fiber = Fiber::new(next_id);
    new_fiber.push_frame(actual_func_id, func.local_slots, 0, 0);
    
    // Copy args to new fiber's stack
    let new_stack = new_fiber.stack_ptr();
    if is_closure_call {
        // Use call_layout for consistent argument placement
        let closure_gcref = closure_ref as GcRef;
        let layout = closure::call_layout(
            closure_ref,
            closure_gcref,
            func.recv_slots as usize,
            func.is_closure,
        );
        
        if let Some(slot0_val) = layout.slot0 {
            unsafe { *new_stack = slot0_val };
        }
        
        for i in 0..arg_slots as usize {
            unsafe { *new_stack.add(layout.arg_offset + i) = *args_ptr.add(i) };
        }
    } else {
        // Regular function: args start at reg[0]
        for i in 0..arg_slots as usize {
            unsafe { *new_stack.add(i) = *args_ptr.add(i) };
        }
    }
    
    // Add to scheduler
    vm.scheduler.spawn(new_fiber);
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
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::closure;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    
    let island_handle = island as GcRef;
    let closure = closure_ref as GcRef;
    let island_id = vo_runtime::island::id(island_handle);
    
    if island_id == 0 {
        // Local spawn - use call_layout for consistent argument placement
        let func_id = closure::func_id(closure);
        let func = &module.functions[func_id as usize];
        
        let next_id = vm.scheduler.fibers.len() as u32;
        let mut new_fiber = Fiber::new(next_id);
        new_fiber.push_frame(func_id, func.local_slots, 0, 0);
        
        let layout = closure::call_layout(
            closure_ref,
            closure,
            func.recv_slots as usize,
            func.is_closure,
        );
        
        let new_stack = new_fiber.stack_ptr();
        if let Some(slot0_val) = layout.slot0 {
            unsafe { *new_stack = slot0_val };
        }
        for i in 0..arg_slots as usize {
            unsafe { *new_stack.add(layout.arg_offset + i) = *args_ptr.add(i) };
        }
        
        vm.scheduler.spawn(new_fiber);
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
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::closure;
    
    // In no_std mode, always spawn locally - match VM behavior
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    
    let closure_gcref = closure_ref as GcRef;
    let func_id = closure::func_id(closure_gcref);
    let func = &module.functions[func_id as usize];
    
    let next_id = vm.scheduler.fibers.len() as u32;
    let mut new_fiber = Fiber::new(next_id);
    new_fiber.push_frame(func_id, func.local_slots, 0, 0);
    
    // Use call_layout for consistent argument placement
    let layout = closure::call_layout(
        closure_ref,
        closure_gcref,
        func.recv_slots as usize,
        func.is_closure,
    );
    
    let new_stack = new_fiber.stack_ptr();
    if let Some(slot0_val) = layout.slot0 {
        unsafe { *new_stack = slot0_val };
    }
    for i in 0..arg_slots as usize {
        unsafe { *new_stack.add(layout.arg_offset + i) = *args_ptr.add(i) };
    }
    
    vm.scheduler.spawn(new_fiber);
    let _ = island; // suppress unused warning
}
