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
        need_vm_func_id: 0,
        need_vm_arg_start: 0,
        need_vm_entry_pc: 0,
        need_vm_resume_pc: 0,
    }
}

// =============================================================================
// NeedVm Info
// =============================================================================

/// Info for NeedVm handoff from JIT to VM.
pub struct NeedVmInfo {
    pub func_id: u32,
    pub arg_start: usize,
    pub entry_pc: usize,
    pub resume_pc: u32,
}

impl NeedVmInfo {
    /// Push callee frame for VM to execute.
    pub fn push_callee_frame(&self, fiber: &mut Fiber, jit_bp: usize, module: &Module) {
        let callee_func = &module.functions[self.func_id as usize];
        let callee_bp = fiber.sp;
        fiber.push_frame(self.func_id, callee_func.local_slots, self.arg_start as u16, callee_func.ret_slots as u16);
        for i in 0..callee_func.param_slots as usize {
            fiber.stack[callee_bp + i] = fiber.stack[jit_bp + self.arg_start + i];
        }
        fiber.frames.last_mut().unwrap().pc = self.entry_pc;
    }
}

// =============================================================================
// JIT Call Methods for Vm
// =============================================================================

impl Vm {
    /// VM-led JIT call: VM has already pushed frame, JIT operates on fiber.stack[jit_bp..].
    /// start_pc: 0 for normal entry, resume_pc for NeedVm continuation.
    /// Returns (ExecResult, Option<NeedVmInfo>) for NeedVm handling.
    pub(super) fn call_jit_with_frame(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        jit_func: JitFunc,
        jit_bp: usize,
        start_pc: u32,
    ) -> (ExecResult, Option<NeedVmInfo>) {
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
        let args_ptr = fiber.stack[jit_bp..].as_mut_ptr();
        
        let mut ctx = build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, fiber_ptr, module_ptr,
            &safepoint_flag, &mut panic_flag,
            &mut panic_msg,
        );
        
        let result = jit_func(&mut ctx, args_ptr, args_ptr, start_pc);
        
        match result {
            JitResult::Ok => (ExecResult::FrameChanged, None),
            JitResult::NeedVm => {
                let info = NeedVmInfo {
                    func_id: ctx.need_vm_func_id,
                    arg_start: ctx.need_vm_arg_start as usize,
                    entry_pc: ctx.need_vm_entry_pc as usize,
                    resume_pc: ctx.need_vm_resume_pc,
                };
                (ExecResult::FrameChanged, Some(info))
            }
            JitResult::Panic => {
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                if fiber.panic_state.is_none() {
                    if panic_flag && !panic_msg.is_nil() {
                        fiber.set_recoverable_panic(panic_msg);
                    } else {
                        set_jit_runtime_panic(&mut self.state.gc, fiber);
                    }
                }
                (ExecResult::Panic, None)
            }
        }
    }
    
    /// Handle JIT re-entry after a NeedVm callee returns.
    /// Called from Return when caller frame is a JIT frame with pc > 0.
    pub(super) fn handle_jit_reentry(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        module: &Module,
    ) -> ExecResult {
        loop {
            let (is_jit_frame, pc, func_id, jit_bp, call_ret_slots) = {
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                match fiber.frames.last() {
                    Some(f) => (f.is_jit_frame, f.pc, f.func_id, f.bp, f.ret_count as usize),
                    None => return ExecResult::Done,
                }
            };
            
            if !is_jit_frame || pc == 0 {
                return ExecResult::FrameChanged;
            }
            
            let resume_pc = pc as u32;
            let jit_func = match self.jit_mgr.as_ref().and_then(|mgr| mgr.get_entry(func_id)) {
                Some(f) => f,
                None => return ExecResult::FrameChanged,
            };
            
            let target_func = &module.functions[func_id as usize];
            let func_ret_slots = target_func.ret_slots as usize;
            
            let (result, need_vm) = self.call_jit_with_frame(fiber_id, jit_func, jit_bp, resume_pc);
            
            let fiber = self.scheduler.get_fiber_mut(fiber_id);
            
            if let Some(need_vm_info) = need_vm {
                fiber.frames.last_mut().unwrap().pc = need_vm_info.resume_pc as usize;
                need_vm_info.push_callee_frame(fiber, jit_bp, module);
                return ExecResult::FrameChanged;
            } else if matches!(result, ExecResult::Panic) {
                return ExecResult::Panic;
            } else if matches!(result, ExecResult::FrameChanged) {
                // JIT returned OK - copy returns and pop frame
                let caller_bp = fiber.frames.get(fiber.frames.len().saturating_sub(2))
                    .map(|f| f.bp).unwrap_or(0);
                let ret_reg = fiber.frames.last().unwrap().ret_reg as usize;
                for i in 0..call_ret_slots.min(func_ret_slots) {
                    fiber.stack[caller_bp + ret_reg + i] = fiber.stack[jit_bp + i];
                }
                fiber.pop_frame();
                continue; // Check if caller is also JIT frame
            } else {
                return result;
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