//! JIT trampolines, context building, and JIT call implementations.

use vo_runtime::jit_api::{JitResult, JitContext};
use vo_runtime::InterfaceSlot;
use vo_jit::JitFunc;

use crate::bytecode::Module;
use crate::fiber::Fiber;

use super::{Vm, VmState, ExecResult};

// =============================================================================
// JIT Panic Handling
// =============================================================================

/// Set recoverable panic state on fiber when JIT triggers a runtime panic.
/// This is only called for runtime panics (nil pointer, bounds check, etc.)
/// when vo_panic hasn't already set the panic message.
#[inline]
fn set_jit_runtime_panic(gc: &mut vo_runtime::gc::Gc, fiber: &mut Fiber) {
    // Only set if panic_state is not already set (vo_panic may have set it for user panics)
    if fiber.panic_state.is_some() {
        return;
    }
    let msg = vo_runtime::objects::string::new_from_string(
        gc,
        "runtime error: nil pointer dereference".to_string()
    );
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, msg as u64));
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
    
    let program_args = unsafe { &*ctx.program_args };
    let sentinel_errors = unsafe { &mut *ctx.sentinel_errors };
    #[cfg(feature = "std")]
    let io = unsafe { &mut *ctx.io };
    
    // Resume token for WaitIo - initially None
    #[cfg(feature = "std")]
    let mut resume_token: Option<vo_runtime::io::IoToken> = None;
    
    loop {
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
            itab_cache,
            &module.functions,
            module,
            ctx.vm,
            ctx.fiber,
            Some(super::closure_call_trampoline),
            &module.well_known,
            program_args,
            sentinel_errors,
            #[cfg(feature = "std")]
            io,
            #[cfg(feature = "std")]
            resume_token.take(),
        );
        
        match result {
            ExternResult::Ok => {
                // Copy back ret_slots (not arg_count) since extern may return more than args
                for i in 0..ret_slots as usize {
                    unsafe { *ret.add(i) = temp_stack[i] };
                }
                return JitResult::Ok;
            }
            ExternResult::Yield => {
                // JIT doesn't support yield (async operations). This is a fatal error.
                let fiber = unsafe { &mut *(ctx.fiber as *mut crate::fiber::Fiber) };
                fiber.set_fatal_panic();
                return JitResult::Panic;
            }
            ExternResult::Block => {
                // Blocking operation - need to return to VM scheduler.
                // This shouldn't normally happen with extern functions.
                let fiber = unsafe { &mut *(ctx.fiber as *mut crate::fiber::Fiber) };
                fiber.set_fatal_panic();
                return JitResult::Panic;
            }
            #[cfg(feature = "std")]
            ExternResult::WaitIo { token } => {
                // Synchronously wait for I/O completion using shared IoRuntime, then resume.
                // Poll until the token's completion appears in the cache.
                // Note: Don't consume the completion here - extern will call take_completion.
                while !io.has_completion(token) {
                    // poll() returns Vec<Completion> for completions that just finished.
                    // Safe to ignore: completions are cached in io.completion_cache, and
                    // the extern will retrieve ours via take_completion(token) when resumed.
                    let _ = io.poll();
                    std::thread::sleep(std::time::Duration::from_micros(100));
                }
                resume_token = Some(token);
                // Loop back to call the extern again with the resume token
            }
            ExternResult::Panic(msg) => {
                // Extern panics are recoverable - convert to Recoverable panic
                // Pack as interface{} with string value
                let fiber = unsafe { &mut *(ctx.fiber as *mut crate::fiber::Fiber) };
                let gc = unsafe { &mut *ctx.gc };
                let panic_str = vo_runtime::objects::string::new_from_string(gc, msg);
                let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
                fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
                return JitResult::Panic;
            }
        }
    }
}


// =============================================================================
// JitContext Builder
// =============================================================================

/// Synchronous VM call from JIT code.
/// This uses execute_closure_sync to run VM code in a temporary fiber.
pub extern "C" fn vm_call_sync(
    vm: *mut std::ffi::c_void,
    fiber: *mut std::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let vm = unsafe { &mut *(vm as *mut super::Vm) };
        
        // Copy args to a Vec
        let args_vec: Vec<u64> = (0..arg_count as usize)
            .map(|i| unsafe { *args.add(i) })
            .collect();
        
        // Use execute_closure_sync which handles all the complexity
        let (success, panic_state) = vm.execute_closure_sync(func_id, &args_vec, ret, ret_count);
        
        if success {
            JitResult::Ok
        } else {
            // Transfer panic state to caller's fiber so defer/recover can work
            if let Some(ps) = panic_state {
                let caller_fiber = unsafe { &mut *(fiber as *mut Fiber) };
                caller_fiber.panic_state = Some(ps);
            }
            JitResult::Panic
        }
    }));
    
    match result {
        Ok(r) => r,
        Err(_) => JitResult::Panic,
    }
}

pub fn build_jit_ctx(
    state: &mut VmState,
    jit_func_table: *const *const u8,
    jit_func_count: u32,
    vm_ptr: *mut std::ffi::c_void,
    fiber_ptr: *mut std::ffi::c_void,
    module_ptr: *const Module,
    safepoint_flag: *const bool,
    panic_flag: *mut bool,
    panic_msg: *mut InterfaceSlot,
) -> JitContext {
    JitContext {
        gc: &mut state.gc as *mut _,
        globals: state.globals.as_mut_ptr(),
        safepoint_flag,
        panic_flag,
        panic_msg,
        vm: vm_ptr,
        fiber: fiber_ptr,
        call_vm_fn: Some(vm_call_sync),
        itab_cache: &mut state.itab_cache as *mut _,
        extern_registry: &state.extern_registry as *const _ as *const std::ffi::c_void,
        call_extern_fn: Some(call_extern_trampoline),
        module: module_ptr,
        jit_func_table,
        jit_func_count,
        program_args: &state.program_args as *const _,
        sentinel_errors: &mut state.sentinel_errors as *mut _,
        #[cfg(feature = "std")]
        io: &mut state.io as *mut _,
        need_vm_entry_pc: 0,
        need_vm_resume_pc: 0,
    }
}

// =============================================================================
// JIT Call Methods for Vm
// =============================================================================

/// Result from JIT execution with NeedVm context.
pub struct JitExecResult {
    pub result: JitResult,
    pub need_vm_entry_pc: u32,
    pub need_vm_resume_pc: u32,
}

impl Vm {
    /// Call a JIT function with a fresh context (raw pointer version).
    /// Returns JitExecResult which includes NeedVm context if applicable.
    pub(super) fn call_jit_direct(
        &mut self,
        jit_func: JitFunc,
        fiber_ptr: *mut std::ffi::c_void,
        args: *mut u64,
        ret: *mut u64,
    ) -> JitExecResult {
        let jit_mgr = self.jit_mgr.as_ref().unwrap();
        let func_table_ptr = jit_mgr.func_table_ptr();
        let func_table_len = jit_mgr.func_table_len() as u32;
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut panic_msg = InterfaceSlot::default();
        let vm_ptr = self as *mut _ as *mut std::ffi::c_void;
        let module_ptr = self.module.as_ref()
            .map(|m| m as *const _)
            .unwrap_or(std::ptr::null());
        
        let mut ctx = build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, fiber_ptr, module_ptr,
            &safepoint_flag, &mut panic_flag,
            &mut panic_msg,
        );
        let result = jit_func(&mut ctx, args, ret);
        
        // Set recoverable panic state if JIT triggered panic
        if result == JitResult::Panic {
            let fiber = unsafe { &mut *(fiber_ptr as *mut Fiber) };
            // Only set panic_state if not already set (vm_call_sync may have set it)
            if fiber.panic_state.is_none() {
                if panic_flag && !panic_msg.is_nil() {
                    // Use actual panic message from vo_panic
                    fiber.set_recoverable_panic(panic_msg);
                } else {
                    // Fallback: use generic runtime error
                    set_jit_runtime_panic(&mut self.state.gc, fiber);
                }
            }
        }
        
        JitExecResult {
            result,
            need_vm_entry_pc: ctx.need_vm_entry_pc,
            need_vm_resume_pc: ctx.need_vm_resume_pc,
        }
    }


    /// Call a JIT function inline from VM execution (resolve_call path).
    /// JitContext.fiber points to the current fiber for correct panic handling.
    /// 
    /// Returns (ExecResult, Option<(entry_pc, resume_pc)>) where the second element
    /// contains NeedVm context if JIT returned NeedVm.
    pub(super) fn call_jit_inline(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        jit_func: JitFunc,
        arg_start: u16,
        arg_slots: usize,
        func_ret_slots: usize,
        call_ret_slots: usize,
    ) -> (ExecResult, Option<(u32, u32)>) {
        // Use stack-allocated buffers to avoid heap allocation on every call
        // Most functions have <= 32 args and <= 16 returns
        const MAX_STACK_SLOTS: usize = 32;
        
        // Read args from fiber stack
        let fiber = self.scheduler.get_fiber_mut(fiber_id);
        let mut args_buf = [0u64; MAX_STACK_SLOTS];
        let mut args_heap: Option<Vec<u64>> = None;
        
        let args_ptr = if arg_slots <= MAX_STACK_SLOTS {
            for i in 0..arg_slots {
                args_buf[i] = fiber.read_reg(arg_start + i as u16);
            }
            args_buf.as_mut_ptr()
        } else {
            let mut v: Vec<u64> = (0..arg_slots)
                .map(|i| fiber.read_reg(arg_start + i as u16))
                .collect();
            let ptr = v.as_mut_ptr();
            args_heap = Some(v);
            ptr
        };
        
        let fiber_ptr = fiber as *mut Fiber as *mut std::ffi::c_void;
        
        let mut ret_buf = [0u64; MAX_STACK_SLOTS];
        let mut ret_heap: Option<Vec<u64>> = None;
        
        let ret_ptr = if func_ret_slots <= MAX_STACK_SLOTS {
            ret_buf.as_mut_ptr()
        } else {
            let mut v = vec![0u64; func_ret_slots];
            let ptr = v.as_mut_ptr();
            ret_heap = Some(v);
            ptr
        };
        
        let jit_result = self.call_jit_direct(jit_func, fiber_ptr, args_ptr, ret_ptr);
        
        // Keep heap allocations alive until after call
        drop(args_heap);
        
        match jit_result.result {
            JitResult::Ok => {
                // Write returns back to fiber stack
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                let ret_slice = if let Some(ref v) = ret_heap { v.as_slice() } else { &ret_buf[..] };
                for i in 0..call_ret_slots.min(func_ret_slots) {
                    fiber.write_reg(arg_start + i as u16, ret_slice[i]);
                }
                drop(ret_heap);
                (ExecResult::FrameChanged, None)
            }
            JitResult::NeedVm => {
                drop(ret_heap);
                (ExecResult::FrameChanged, Some((jit_result.need_vm_entry_pc, jit_result.need_vm_resume_pc)))
            }
            JitResult::Panic => {
                drop(ret_heap);
                (ExecResult::Panic, None)
            }
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
                        Err(_e) => {
                            // Mark as failed
                            jit_mgr.mark_loop_failed(func_id, loop_begin_pc);
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
        let mut panic_msg = InterfaceSlot::default();
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
            &mut panic_msg,
        );
        
        let exit_pc = loop_func(&mut ctx, locals_ptr);
        
        if exit_pc == LOOP_RESULT_PANIC {
            // Set panic state so defer/recover can work
            if panic_flag {
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                if !panic_msg.is_nil() {
                    fiber.set_recoverable_panic(panic_msg);
                } else {
                    set_jit_runtime_panic(&mut self.state.gc, fiber);
                }
            }
            None
        } else {
            Some(exit_pc as usize)
        }
    }
}