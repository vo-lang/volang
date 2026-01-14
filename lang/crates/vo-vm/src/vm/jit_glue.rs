//! JIT trampolines, context building, and JIT call implementations.

use vo_runtime::jit_api::{JitResult, JitContext};
use vo_jit::JitFunc;

use crate::bytecode::Module;
use crate::fiber::Fiber;

use super::{Vm, VmState, ExecResult};

// =============================================================================
// JIT Panic Handling
// =============================================================================

/// Set recoverable panic state on fiber when JIT triggers a runtime panic.
/// This allows defer/recover to work correctly.
#[inline]
fn set_jit_runtime_panic(gc: &mut vo_runtime::gc::Gc, fiber: &mut Fiber) {
    let msg = vo_runtime::objects::string::new_from_string(
        gc,
        "runtime error: nil pointer dereference".to_string()
    );
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    let slot1 = msg as u64;
    fiber.set_recoverable_panic(slot0, slot1);
}

// =============================================================================
// JIT Trampolines
// =============================================================================

pub extern "C" fn call_extern_trampoline(
    ctx: *mut vo_runtime::jit_api::JitContext,
    registry: *const std::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    module: *const std::ffi::c_void,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_slots: u32,
) -> JitResult {
    use vo_runtime::ffi::{ExternResult, ExternRegistry};
    use crate::bytecode::Module;
    
    let registry = unsafe { &*(registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    let module = unsafe { &*(module as *const Module) };
    let ctx = unsafe { &mut *ctx };
    let itab_cache = unsafe { &mut *ctx.itab_cache };
    
    // Buffer must be large enough for both args and return values
    let buffer_size = (arg_count as usize).max(ret_slots as usize).max(1);
    let mut temp_stack: Vec<u64> = vec![0; buffer_size];
    for i in 0..arg_count as usize {
        temp_stack[i] = unsafe { *args.add(i) };
    }
    
    let result = registry.call(
        extern_id,
        &mut temp_stack,
        0,
        0,
        arg_count as u16,
        0,
        gc,
        &module.struct_metas,
        &module.interface_metas,
        &module.named_type_metas,
        &module.runtime_types,
        &module.well_known,
        itab_cache,
        &module.functions,
        module,
        ctx.vm,
        ctx.fiber,
        Some(closure_call_trampoline),
    );
    
    match result {
        ExternResult::Ok => {
            // Copy back ret_slots (not arg_count) since extern may return more than args
            for i in 0..ret_slots as usize {
                unsafe { *ret.add(i) = temp_stack[i] };
            }
            JitResult::Ok
        }
        ExternResult::Yield => {
            // JIT doesn't support yield (async operations). This is a fatal error.
            let fiber = unsafe { &mut *(ctx.fiber as *mut crate::fiber::Fiber) };
            fiber.set_fatal_panic("extern function returned Yield, not supported in JIT".to_string());
            JitResult::Panic
        }
        ExternResult::Panic(msg) => {
            // Extern panics are recoverable - convert to Recoverable panic
            // Pack as interface{} with string value
            let fiber = unsafe { &mut *(ctx.fiber as *mut crate::fiber::Fiber) };
            let gc = unsafe { &mut *ctx.gc };
            let panic_str = vo_runtime::objects::string::new_from_string(gc, msg);
            let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
            let slot1 = panic_str as u64;
            fiber.set_recoverable_panic(slot0, slot1);
            JitResult::Panic
        }
    }
}

pub extern "C" fn vm_call_trampoline(
    vm: *mut std::ffi::c_void,
    caller_fiber: *mut std::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    // Catch any panics to prevent unwinding through extern "C" boundary
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let vm = unsafe { &mut *(vm as *mut Vm) };
        vm.execute_jit_call_with_caller(func_id, args, arg_count, ret, ret_count, caller_fiber)
    }));
    
    match result {
        Ok(r) => r,
        Err(_) => JitResult::Panic,
    }
}

/// Trampoline for calling closures from extern functions.
/// This allows extern functions like dyn_call_closure to execute closures
/// and get results without needing a fixed-size buffer at compile time.
pub extern "C" fn closure_call_trampoline(
    vm: *mut std::ffi::c_void,
    caller_fiber: *mut std::ffi::c_void,
    closure_ref: u64,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> vo_runtime::ffi::ClosureCallResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::closure;
    
    // Catch any panics to prevent unwinding through extern "C" boundary
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let vm = unsafe { &mut *(vm as *mut Vm) };
        let closure_gcref = closure_ref as GcRef;
        let func_id = closure::func_id(closure_gcref);
        
        // Get func_def to check recv_slots (for method closures)
        let module = vm.module().expect("closure_call_trampoline: module not set");
        let func_def = &module.functions[func_id as usize];
        let recv_slots = func_def.recv_slots as usize;
        let capture_count = closure::capture_count(closure_gcref);
        
        // Build full_args based on closure type (matches VM's exec_call_closure logic)
        let slot0 = if recv_slots > 0 && capture_count > 0 {
            Some(closure::get_capture(closure_gcref, 0))
        } else if capture_count > 0 || func_def.is_closure {
            Some(closure_ref)
        } else {
            None
        };
        
        let mut full_args = Vec::with_capacity(slot0.is_some() as usize + arg_count as usize);
        full_args.extend(slot0);
        for i in 0..arg_count {
            full_args.push(unsafe { *args.add(i as usize) });
        }
        
        vm.execute_jit_call_with_caller(func_id, full_args.as_ptr(), full_args.len() as u32, ret, ret_count, caller_fiber)
    }));
    
    match result {
        Ok(JitResult::Ok) => vo_runtime::ffi::ClosureCallResult::Ok,
        _ => vo_runtime::ffi::ClosureCallResult::Panic,
    }
}

// =============================================================================
// JitContext Builder
// =============================================================================

pub fn build_jit_ctx(
    state: &mut VmState,
    jit_func_table: *const *const u8,
    jit_func_count: u32,
    vm_ptr: *mut std::ffi::c_void,
    fiber_ptr: *mut std::ffi::c_void,
    module_ptr: *const Module,
    safepoint_flag: *const bool,
    panic_flag: *mut bool,
) -> JitContext {
    JitContext {
        gc: &mut state.gc as *mut _,
        globals: state.globals.as_mut_ptr(),
        safepoint_flag,
        panic_flag,
        vm: vm_ptr,
        fiber: fiber_ptr,
        call_vm_fn: Some(vm_call_trampoline),
        itab_cache: &mut state.itab_cache as *mut _,
        extern_registry: &state.extern_registry as *const _ as *const std::ffi::c_void,
        call_extern_fn: Some(call_extern_trampoline),
        module: module_ptr,
        jit_func_table,
        jit_func_count,
    }
}

// =============================================================================
// JIT Call Methods for Vm
// =============================================================================

impl Vm {
    /// Call a JIT function with a fresh context (raw pointer version).
    pub(super) fn call_jit_direct(
        &mut self,
        jit_func: JitFunc,
        fiber_ptr: *mut std::ffi::c_void,
        args: *mut u64,
        ret: *mut u64,
    ) -> JitResult {
        let jit_mgr = self.jit_mgr.as_ref().unwrap();
        let func_table_ptr = jit_mgr.func_table_ptr();
        let func_table_len = jit_mgr.func_table_len() as u32;
        let safepoint_flag = false;
        let mut panic_flag = false;
        let vm_ptr = self as *mut _ as *mut std::ffi::c_void;
        let module_ptr = self.module.as_ref()
            .map(|m| m as *const _)
            .unwrap_or(std::ptr::null());
        
        let mut ctx = build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, fiber_ptr, module_ptr,
            &safepoint_flag, &mut panic_flag,
        );
        let result = jit_func(&mut ctx, args, ret);
        
        // Set recoverable panic state if JIT triggered panic (e.g., nil pointer)
        if result == JitResult::Panic && panic_flag {
            let fiber = unsafe { &mut *(fiber_ptr as *mut Fiber) };
            set_jit_runtime_panic(&mut self.state.gc, fiber);
        }
        result
    }

    /// Execute a JIT->VM call. JitContext.fiber always points to caller_fiber (the original fiber
    /// that owns defer/recover), not the trampoline fiber. Trampoline fiber is only used as an
    /// execution container for VM interpretation.
    ///
    /// This is the core logic for vm_call_trampoline.
    pub fn execute_jit_call_with_caller(
        &mut self,
        func_id: u32,
        args: *const u64,
        arg_count: u32,
        ret: *mut u64,
        ret_count: u32,
        caller_fiber_ptr: *mut std::ffi::c_void,
    ) -> JitResult {
        let module = match &self.module {
            Some(m) => m as *const Module,
            None => return JitResult::Panic,
        };
        let module = unsafe { &*module };
        
        // Try JIT compilation/execution first
        // IMPORTANT: Use caller_fiber_ptr for JitContext.fiber so panic goes to the right fiber
        if let Some(jit_mgr) = self.jit_mgr.as_mut() {
            if let Some(jit_func) = jit_mgr.get_entry(func_id) {
                // JIT function exists - call directly with caller fiber as JitContext.fiber
                return self.call_jit_direct(jit_func, caller_fiber_ptr, args as *mut u64, ret);
            }
            
            if jit_mgr.record_call(func_id) {
                let func_def = &module.functions[func_id as usize];
                if jit_mgr.compile_full(func_id, func_def, module).is_ok() {
                    if let Some(jit_func) = jit_mgr.get_entry(func_id) {
                        return self.call_jit_direct(jit_func, caller_fiber_ptr, args as *mut u64, ret);
                    }
                }
            }
        }
        
        // Fall back to VM interpretation using trampoline fiber
        // Trampoline fiber provides stack/frames for VM execution, but panic_state
        // will be propagated back to caller_fiber at the end.
        let trampoline_id = self.scheduler.acquire_trampoline_fiber();
        
        let func_def = &module.functions[func_id as usize];
        let local_slots = func_def.local_slots;
        let param_slots = func_def.param_slots as usize;
        let func_ret_slots = func_def.ret_slots as usize;
        
        {
            let fiber = self.scheduler.trampoline_fiber_mut(trampoline_id);
            fiber.push_frame(func_id, local_slots, 0, func_ret_slots as u16);
            let bp = fiber.frames.last().unwrap().bp;
            for i in 0..param_slots.min(arg_count as usize) {
                let arg_val = unsafe { *args.add(i) };
                fiber.stack[bp + i] = arg_val;
            }
        }
        
        let result = loop {
            let exec_result = self.run_fiber(crate::scheduler::FiberId::from_raw(trampoline_id));
            match exec_result {
                ExecResult::Done => break JitResult::Ok,
                ExecResult::Panic => break JitResult::Panic,
                ExecResult::Continue => {}
                ExecResult::Return => break JitResult::Ok,
                ExecResult::Osr(_, _, _) => {}
                ExecResult::Yield | ExecResult::Block => {
                    if !self.scheduler.ready_queue.is_empty() {
                        self.run_scheduler_round();
                    } else {
                        let fiber = self.scheduler.trampoline_fiber_mut(trampoline_id);
                        fiber.set_fatal_panic("deadlock: channel blocked with no other runnable fibers".to_string());
                        break JitResult::Panic;
                    }
                }
            }
        };
        
        if result == JitResult::Ok {
            let fiber = self.scheduler.trampoline_fiber(trampoline_id);
            for i in 0..(ret_count as usize) {
                if i < fiber.stack.len() {
                    unsafe { *ret.add(i) = fiber.stack[i] };
                }
            }
        } else if result == JitResult::Panic && !caller_fiber_ptr.is_null() {
            // VM execution panicked - propagate panic_state to caller fiber
            let trampoline_fiber = self.scheduler.trampoline_fiber_mut(trampoline_id);
            if let Some(panic_state) = trampoline_fiber.panic_state.take() {
                let caller_fiber = unsafe { &mut *(caller_fiber_ptr as *mut Fiber) };
                caller_fiber.panic_state = Some(panic_state);
            }
        }
        
        self.scheduler.release_trampoline_fiber(trampoline_id);
        result
    }

    /// Call a JIT function inline from VM execution (resolve_call path).
    /// JitContext.fiber points to the current fiber for correct panic handling.
    pub(super) fn call_jit_inline(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        jit_func: JitFunc,
        arg_start: u16,
        arg_slots: usize,
        func_ret_slots: usize,
        call_ret_slots: usize,
    ) -> ExecResult {
        // Read args from fiber stack
        let fiber = self.scheduler.get_fiber_mut(fiber_id);
        let mut args: Vec<u64> = (0..arg_slots)
            .map(|i| fiber.read_reg(arg_start + i as u16))
            .collect();
        let fiber_ptr = fiber as *mut Fiber as *mut std::ffi::c_void;
        
        let mut ret_buf = vec![0u64; func_ret_slots.max(1)];
        let result = self.call_jit_direct(jit_func, fiber_ptr, args.as_mut_ptr(), ret_buf.as_mut_ptr());
        
        if result == JitResult::Ok {
            // Write returns back to fiber stack
            let fiber = self.scheduler.get_fiber_mut(fiber_id);
            for i in 0..call_ret_slots.min(ret_buf.len()) {
                fiber.write_reg(arg_start + i as u16, ret_buf[i]);
            }
            ExecResult::Continue
        } else {
            // panic_state already set by call_jit_direct
            ExecResult::Panic
        }
    }

    /// Try to perform loop OSR at a back-edge.
    /// 
    /// Called when VM detects a HINT_LOOP_BEGIN instruction.
    /// If the loop is hot and compiled, executes it via JIT and returns the new PC.
    pub(super) fn try_osr(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        func_id: u32,
        loop_begin_pc: usize,
        bp: usize,
    ) -> Option<usize> {
        let module = self.module.as_ref()?;
        let func_def = &module.functions[func_id as usize];
        let jit_mgr = self.jit_mgr.as_mut()?;
        
        // 1. Check if loop is already compiled
        let loop_func = unsafe { jit_mgr.get_loop_func(func_id, loop_begin_pc) };
        
        if let Some(loop_func) = loop_func {
            // Loop is compiled - execute it
            return self.execute_loop_osr(fiber_id, loop_func, bp);
        }
        
        // 2. Check if loop already failed compilation
        if jit_mgr.is_loop_failed(func_id, loop_begin_pc) {
            return None;
        }
        
        // 3. Record backedge hit for hot tracking
        if jit_mgr.record_backedge(func_id, loop_begin_pc) {
            // Loop is hot - try to compile
            if let Some(loop_info) = jit_mgr.find_loop(func_id, func_def, loop_begin_pc) {
                if loop_info.is_jittable() {
                    match jit_mgr.compile_loop(func_id, func_def, module, &loop_info) {
                        Ok(_) => {
                            // Compilation succeeded - execute the loop
                            if let Some(loop_func) = unsafe { jit_mgr.get_loop_func(func_id, loop_begin_pc) } {
                                return self.execute_loop_osr(fiber_id, loop_func, bp);
                            }
                        }
                        Err(e) => {
                            // Mark as failed and report error
                            jit_mgr.mark_loop_failed(func_id, loop_begin_pc);
                            eprintln!("[JIT] Loop compilation failed (func={}, pc={}): {:?}", func_id, loop_begin_pc, e);
                        }
                    }
                }
            }
        }
        
        // Continue VM execution
        None
    }
    
    /// Execute a compiled loop function via OSR.
    /// Returns the new PC to resume at, or None on panic.
    fn execute_loop_osr(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        loop_func: vo_jit::LoopFunc,
        bp: usize,
    ) -> Option<usize> {
        use vo_jit::LOOP_RESULT_PANIC;
        
        let jit_mgr = self.jit_mgr.as_ref().unwrap();
        let func_table_ptr = jit_mgr.func_table_ptr();
        let func_table_len = jit_mgr.func_table_len() as u32;
        let safepoint_flag = false;
        let mut panic_flag = false;
        let vm_ptr = self as *mut _ as *mut std::ffi::c_void;
        let module_ptr = self.module.as_ref()
            .map(|m| m as *const _)
            .unwrap_or(std::ptr::null());
        
        let fiber = self.scheduler.get_fiber_mut(fiber_id);
        let fiber_ptr = fiber as *mut Fiber as *mut std::ffi::c_void;
        let locals_ptr = fiber.stack[bp..].as_mut_ptr();
        
        let mut ctx = build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, fiber_ptr, module_ptr,
            &safepoint_flag, &mut panic_flag,
        );
        
        let exit_pc = loop_func(&mut ctx, locals_ptr);
        
        if exit_pc == LOOP_RESULT_PANIC {
            // Set panic state so defer/recover can work
            if panic_flag {
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                set_jit_runtime_panic(&mut self.state.gc, fiber);
            }
            None
        } else {
            Some(exit_pc as usize)
        }
    }
}