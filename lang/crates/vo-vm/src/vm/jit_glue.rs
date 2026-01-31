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

const PANIC_MSG_NIL_DEREF: &str = "runtime error: invalid memory address or nil pointer dereference (JIT)";
const PANIC_MSG_BLOCKING_OP: &str = "runtime error: extern function returned Yield/Block in JIT context (channel operations are not supported in JIT-compiled functions)";

/// Set recoverable panic with a message.
#[inline]
fn set_recoverable_panic_msg(gc: &mut vo_runtime::gc::Gc, fiber: &mut Fiber, msg: &str) {
    if fiber.panic_state.is_some() {
        return;
    }
    let panic_str = vo_runtime::objects::string::new_from_string(gc, msg.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
}

// =============================================================================
// JIT Helper Functions
// =============================================================================

/// Handle extern function call from JIT code.
/// 
/// Directly operates on the fiber.stack region pointed to by `args`/`ret`,
/// avoiding unnecessary copies. The JIT passes the same pointer for both args and ret,
/// which points to fiber.stack[jit_bp + arg_start].
///
/// For waitio_ extern functions that return WaitIo:
/// - First call: extern starts I/O, returns WaitIo, we return JitResult::WaitIo
/// - VM blocks fiber, waits for I/O completion
/// - Resume call: ctx.wait_io_token contains the token, passed to extern as resume_token
pub extern "C" fn jit_call_extern(
    ctx: *mut vo_runtime::jit_api::JitContext,
    registry: *const std::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    module: *const std::ffi::c_void,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    _ret: *mut u64,  // Same as args in current JIT impl
    ret_slots: u32,
) -> JitResult {
    use vo_runtime::ffi::{ExternResult, ExternRegistry};
    use crate::bytecode::Module;
    
    let registry = unsafe { &*(registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    let module = unsafe { &*(module as *const Module) };
    let ctx = unsafe { &mut *ctx };
    let itab_cache = unsafe { &mut *ctx.itab_cache };
    
    // Directly use the fiber.stack region - no temp buffer needed.
    let buffer_size = (arg_count as usize).max(ret_slots as usize).max(1);
    let stack_slice = unsafe { std::slice::from_raw_parts_mut(args as *mut u64, buffer_size) };
    
    let program_args = unsafe { &*ctx.program_args };
    let sentinel_errors = unsafe { &mut *ctx.sentinel_errors };
    #[cfg(feature = "std")]
    let io = unsafe { &mut *ctx.io };
    
    // Resume token: if wait_io_token is set, this is a resume after WaitIo
    #[cfg(feature = "std")]
    let resume_token = if ctx.wait_io_token != 0 {
        let token = ctx.wait_io_token;  // IoToken is just u64
        ctx.wait_io_token = 0;  // Clear for next call
        Some(token)
    } else {
        None
    };
    #[cfg(not(feature = "std"))]
    let resume_token: Option<()> = None;
    
    let result = registry.call(
        extern_id,
        stack_slice,
        0,  // bp=0: slice already starts at the right position
        0,  // arg_start=0: args are at slice[0..]
        arg_count as u16,
        0,  // ret_start=0: ret goes to slice[0..]
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
        resume_token,
    );
    
    match result {
        ExternResult::Ok => JitResult::Ok,
        ExternResult::Yield | ExternResult::Block => {
            let fiber = unsafe { &mut *(ctx.fiber as *mut crate::fiber::Fiber) };
            let gc = unsafe { &mut *ctx.gc };
            set_recoverable_panic_msg(gc, fiber, PANIC_MSG_BLOCKING_OP);
            JitResult::Panic
        }
        #[cfg(feature = "std")]
        ExternResult::WaitIo { token } => {
            // Return WaitIo to VM scheduler for proper fiber blocking.
            // JIT compiler emits spill before calling waitio_ extern functions,
            // so variables are safely saved to fiber.stack before we yield.
            ctx.wait_io_token = token;  // IoToken is just u64
            JitResult::WaitIo
        }
        ExternResult::Panic(msg) => {
            // Extern panics are recoverable - convert to Recoverable panic
            let fiber = unsafe { &mut *(ctx.fiber as *mut crate::fiber::Fiber) };
            let gc = unsafe { &mut *ctx.gc };
            let panic_str = vo_runtime::objects::string::new_from_string(gc, msg);
            let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
            fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
            JitResult::Panic
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
        call_extern_fn: Some(jit_call_extern),
        module: module_ptr,
        jit_func_table,
        jit_func_count,
        program_args: &state.program_args as *const _,
        sentinel_errors: &mut state.sentinel_errors as *mut _,
        #[cfg(feature = "std")]
        io: &mut state.io as *mut _,
        call_func_id: 0,
        call_arg_start: 0,
        call_entry_pc: 0,
        call_resume_pc: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
    }
}

// =============================================================================
// Call Request Info
// =============================================================================

/// Info for Call request from JIT to VM (non-jittable callee).
pub struct CallRequestInfo {
    pub func_id: u32,
    pub arg_start: usize,
    pub entry_pc: usize,
    pub resume_pc: u32,
}

impl CallRequestInfo {
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
    /// start_pc: 0 for normal entry, resume_pc for Call continuation.
    /// Returns (ExecResult, Option<CallRequestInfo>) for Call handling.
    pub(super) fn call_jit_with_frame(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        jit_func: JitFunc,
        jit_bp: usize,
        start_pc: u32,
    ) -> (ExecResult, Option<CallRequestInfo>) {
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
        
        // Get wait_io_token from frame for I/O resume, then clear it
        #[cfg(feature = "std")]
        let wait_io_token = {
            let frame = fiber.current_frame_mut().unwrap();
            let token = frame.wait_io_token;
            frame.wait_io_token = 0;  // Clear after reading
            token
        };
        
        let mut ctx = build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, fiber_ptr, module_ptr,
            &safepoint_flag, &mut panic_flag,
            &mut panic_msg,
        );
        
        // Set wait_io_token for I/O resume (JIT will pass it to jit_call_extern)
        #[cfg(feature = "std")]
        { ctx.wait_io_token = wait_io_token; }
        
        let result = jit_func(&mut ctx, args_ptr, args_ptr, start_pc);
        
        match result {
            JitResult::Ok => (ExecResult::FrameChanged, None),
            JitResult::Call => {
                let info = CallRequestInfo {
                    func_id: ctx.call_func_id,
                    arg_start: ctx.call_arg_start as usize,
                    entry_pc: ctx.call_entry_pc as usize,
                    resume_pc: ctx.call_resume_pc,
                };
                (ExecResult::FrameChanged, Some(info))
            }
            #[cfg(feature = "std")]
            JitResult::WaitIo => {
                // JIT code wants to yield for I/O - block the fiber
                let token = ctx.wait_io_token;  // IoToken is just u64
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                // Set resume PC and save token so JIT can continue after I/O completes
                let frame = fiber.current_frame_mut().unwrap();
                frame.pc = ctx.call_resume_pc as usize;
                frame.wait_io_token = ctx.wait_io_token;
                self.scheduler.block_for_io(token);
                (ExecResult::Block(crate::fiber::BlockReason::Io(token)), None)
            }
            JitResult::Panic => {
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                if fiber.panic_state.is_none() {
                    if panic_flag && !panic_msg.is_nil() {
                        fiber.set_recoverable_panic(panic_msg);
                    } else {
                        set_recoverable_panic_msg(&mut self.state.gc, fiber, PANIC_MSG_NIL_DEREF);
                    }
                }
                (ExecResult::Panic, None)
            }
        }
    }
    
    /// Handle JIT re-entry after a Call request callee returns.
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
            
            let (result, call_request) = self.call_jit_with_frame(fiber_id, jit_func, jit_bp, resume_pc);
            
            let fiber = self.scheduler.get_fiber_mut(fiber_id);
            
            if let Some(call_info) = call_request {
                fiber.frames.last_mut().unwrap().pc = call_info.resume_pc as usize;
                call_info.push_callee_frame(fiber, jit_bp, module);
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
                    set_recoverable_panic_msg(&mut self.state.gc, fiber, PANIC_MSG_NIL_DEREF);
                }
            }
            None
        } else {
            Some(exit_pc as usize)
        }
    }
}