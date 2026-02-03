//! JIT dispatch: bridge between VM interpreter and JIT-compiled code.
//!
//! ## Overview
//!
//! When VM encounters a `Call` instruction for a JIT-compiled function:
//! 1. `dispatch_jit_call` allocates frame in fiber.stack and prepares JitContext
//! 2. JIT function executes natively, using fiber.stack directly
//! 3. Results (Ok/Panic/Call/WaitIo) are translated back to VM state
//!
//! ## fiber.stack ABI
//!
//! JIT functions store all locals in `fiber.stack[jit_bp..]`, not in separate buffers.
//! This enables seamless continuation when JIT returns `Call` (VM fallback needed).
//!
//! ## Shadow Frame Design
//!
//! JIT-to-JIT calls use lightweight `resume_stack` (shadow frames) instead of `fiber.frames`:
//! - `jit_push_frame`: Push ResumePoint to resume_stack (not fiber.frames)
//! - `jit_pop_frame`: Pop ResumePoint and restore caller's bp
//! - On Call/WaitIo: `convert_resume_stack_to_frames` converts shadow frames to real CallFrames
//!
//! This avoids redundant frame management during pure JIT execution.

use vo_runtime::bytecode::Module;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContext, JitResult};
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::{CallFrame, Fiber};
use crate::vm::{helpers, ExecResult, Vm};

/// Execute a JIT-compiled function call.
///
/// This function:
/// 1. Prepares args/ret buffers from caller's stack
/// 2. Pushes a temporary VM frame (for panic/defer support)
/// 3. Calls the JIT function
/// 4. Handles the result (Ok/Panic/Call/WaitIo)
///
/// Returns `ExecResult::FrameChanged` on success (return values written to stack).
pub fn dispatch_jit_call(
    vm: &mut Vm,
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
    jit_func: vo_jit::JitFunc,
    func_id: u32,
) -> ExecResult {
    let caller_bp = fiber.frames.last().map_or(0, |f| f.bp);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;

    let func_def = &module.functions[func_id as usize];
    let local_slots = func_def.local_slots as usize;
    
    // Allocate JIT frame directly in fiber.stack (not a separate Vec)
    let jit_bp = fiber.sp;
    fiber.ensure_capacity(jit_bp + local_slots);
    
    // Zero the frame region first
    let stack_ptr = fiber.stack_ptr();
    unsafe {
        core::ptr::write_bytes(stack_ptr.add(jit_bp), 0, local_slots);
    }
    
    // Copy caller args directly to fiber.stack[jit_bp..]
    for i in 0..arg_slots {
        fiber.stack[jit_bp + i] = fiber.stack[caller_bp + arg_start + i];
    }
    
    fiber.sp = jit_bp + local_slots;
    
    // Push frame so panic_unwind has correct frame info
    // ret_reg = arg_start so return values go to correct location in caller's stack
    fiber.frames.push(CallFrame::new(func_id, jit_bp, arg_start as u16, ret_slots as u16));

    // Build JitContext with fiber stack access
    let mut ctx = build_jit_context(vm, fiber, module);
    
    // Update ctx with current frame info
    ctx.ctx.stack_ptr = fiber.stack_ptr();
    ctx.ctx.stack_cap = fiber.stack.len() as u32;
    ctx.ctx.jit_bp = jit_bp as u32;

    // Prepare ret buffer (still separate - return values copied back after call)
    let mut ret: Vec<u64> = vec![0u64; ret_slots.max(1)];

    // args_ptr points directly to fiber.stack[jit_bp]
    let args_ptr = unsafe { fiber.stack_ptr().add(jit_bp) };
    
    // Call JIT function
    let result = jit_func(ctx.as_ptr(), args_ptr, ret.as_mut_ptr());

    handle_jit_result(vm, fiber, module, result, ctx, caller_bp, arg_start, ret_slots, jit_bp, &ret)
}

/// Execute a JIT callee when frame is already set up (from Call request).
///
/// This is called recursively from handle_jit_result when the callee can be JIT-executed.
/// The callee's frame has already been pushed to fiber.frames.
fn execute_jit_callee(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    jit_func: vo_jit::JitFunc,
    callee_func_id: u32,
    callee_bp: usize,
    callee_ret_slots: usize,
    caller_bp: usize,
    caller_arg_start: usize,
) -> ExecResult {
    // Build JitContext
    let mut ctx = build_jit_context(vm, fiber, module);
    ctx.ctx.stack_ptr = fiber.stack_ptr();
    ctx.ctx.stack_cap = fiber.stack.len() as u32;
    ctx.ctx.jit_bp = callee_bp as u32;
    
    // Prepare ret buffer
    let mut ret: Vec<u64> = vec![0u64; callee_ret_slots.max(1)];
    
    // args_ptr points to callee's frame
    let args_ptr = unsafe { fiber.stack_ptr().add(callee_bp) };
    
    // Call JIT function
    let result = jit_func(ctx.as_ptr(), args_ptr, ret.as_mut_ptr());
    
    // Handle result (may recurse again)
    handle_jit_result(vm, fiber, module, result, ctx, caller_bp, caller_arg_start, callee_ret_slots, callee_bp, &ret)
}

/// JIT context with owned storage for mutable fields.
///
/// The JitContext contains pointers to mutable state (panic_flag, panic_msg).
/// This wrapper owns those values to ensure they outlive the JIT call.
struct JitContextWrapper {
    ctx: JitContext,
    // Owned storage - pointers in ctx point to these
    _panic_flag: Box<bool>,
    _panic_msg: Box<InterfaceSlot>,
    _safepoint_flag: Box<bool>,
}

impl JitContextWrapper {
    fn as_ptr(&mut self) -> *mut JitContext {
        &mut self.ctx
    }

    fn panic_msg(&self) -> InterfaceSlot {
        *self._panic_msg
    }

    fn call_func_id(&self) -> u32 {
        self.ctx.call_func_id
    }

    fn call_arg_start(&self) -> u16 {
        self.ctx.call_arg_start
    }

    fn call_resume_pc(&self) -> u32 {
        self.ctx.call_resume_pc
    }

    fn call_ret_slots(&self) -> u16 {
        self.ctx.call_ret_slots
    }

    #[cfg(feature = "std")]
    fn wait_io_token(&self) -> u64 {
        self.ctx.wait_io_token
    }
}

fn build_jit_context(vm: &mut Vm, fiber: &mut Fiber, module: &Module) -> JitContextWrapper {
    // Extract jit_mgr values first to avoid borrow conflicts
    let (jit_func_table, jit_func_count) = {
        let jit_mgr = vm.jit_mgr.as_ref().unwrap();
        (jit_mgr.func_table_ptr(), jit_mgr.func_table_len() as u32)
    };

    let mut panic_flag = Box::new(false);
    let mut panic_msg = Box::new(InterfaceSlot::default());
    let mut safepoint_flag = Box::new(false);

    let ctx = JitContext {
        gc: &mut vm.state.gc as *mut _,
        globals: vm.state.globals.as_mut_ptr(),
        safepoint_flag: &*safepoint_flag as *const bool,
        panic_flag: &mut *panic_flag as *mut bool,
        panic_msg: &mut *panic_msg as *mut InterfaceSlot,
        vm: vm as *mut Vm as *mut core::ffi::c_void,
        fiber: fiber as *mut Fiber as *mut core::ffi::c_void,
        call_vm_fn: Some(jit_call_vm_trampoline),
        itab_cache: &mut vm.state.itab_cache as *mut _,
        extern_registry: &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
        #[cfg(feature = "std")]
        call_extern_fn: Some(jit_call_extern),
        #[cfg(not(feature = "std"))]
        call_extern_fn: None,
        module: module as *const Module as *const vo_runtime::bytecode::Module,
        jit_func_table,
        jit_func_count,
        program_args: &vm.state.program_args as *const Vec<String>,
        sentinel_errors: &mut vm.state.sentinel_errors as *mut _,
        #[cfg(feature = "std")]
        io: &mut vm.state.io as *mut _,
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_kind: 0,
        call_arg_slots: 0,
        call_closure_ref: 0,
        call_iface_recv: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        // Fiber stack access fields - will be updated before JIT call
        stack_ptr: fiber.stack_ptr(),
        stack_cap: fiber.stack.len() as u32,
        jit_bp: 0, // Will be set in dispatch_jit_call
        fiber_sp: fiber.sp as u32,
        push_frame_fn: Some(jit_push_frame),
        pop_frame_fn: Some(jit_pop_frame),
        push_resume_point_fn: Some(jit_push_resume_point),
    };

    JitContextWrapper {
        ctx,
        _panic_flag: panic_flag,
        _panic_msg: panic_msg,
        _safepoint_flag: safepoint_flag,
    }
}

fn handle_jit_result(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    result: JitResult,
    ctx: JitContextWrapper,
    caller_bp: usize,
    arg_start: usize,
    ret_slots: usize,
    jit_bp: usize,
    ret: &[u64],
) -> ExecResult {
    match result {
        JitResult::Ok => {
            // Pop the JIT frame
            fiber.frames.pop();
            fiber.sp = jit_bp;
            
            // Clear resume_stack - JIT completed normally, no shadow frames should remain
            #[cfg(feature = "jit")]
            fiber.resume_stack.clear();

            // Copy return values back to caller's stack
            for i in 0..ret_slots {
                fiber.stack[caller_bp + arg_start + i] = ret[i];
            }
            ExecResult::FrameChanged
        }
        JitResult::Panic => {
            let mut panic_msg = ctx.panic_msg();
            
            // Check if panic_msg is nil (runtime panic like nil pointer)
            if panic_msg.slot0 == 0 {
                // Create default nil pointer message
                let msg_str = vo_runtime::objects::string::new_from_string(
                    &mut vm.state.gc,
                    helpers::ERR_NIL_POINTER.to_string(),
                );
                let slot0 = vo_runtime::objects::interface::pack_slot0(
                    0, 0, vo_runtime::ValueKind::String
                );
                panic_msg = InterfaceSlot::new(slot0, msg_str as u64);
            }
            
            // Set panic state on fiber
            fiber.set_recoverable_panic(panic_msg);
            let stack_ptr = fiber.stack_ptr();
            helpers::panic_unwind(fiber, stack_ptr, module)
        }
        JitResult::Call => {
            // Check for special call_kind values that don't need frame setup
            let call_kind = ctx.ctx.call_kind;
            match call_kind {
                JitContext::CALL_KIND_YIELD => return ExecResult::TimesliceExpired,
                JitContext::CALL_KIND_BLOCK => return ExecResult::Block(crate::fiber::BlockReason::Queue),
                _ => {} // Regular, closure, or iface call - continue to frame setup
            }
            
            // JIT requests VM to execute a non-JIT function.
            // JIT locals are already in fiber.stack[jit_bp..] - no copy needed!
            // 
            // IMPORTANT: Use ctx.jit_bp, not the passed jit_bp parameter!
            // If nested JIT calls happened (via emit_jit_call_with_fallback), 
            // ctx.jit_bp points to the actual caller's frame that returned Call,
            // while the passed jit_bp is the original dispatch_jit_call's frame.
            let actual_jit_bp = ctx.ctx.jit_bp as usize;
            
            let callee_func_id = ctx.call_func_id();
            let call_arg_start = ctx.call_arg_start() as usize;
            let resume_pc = ctx.call_resume_pc();
            let callee_ret_slots = ctx.call_ret_slots() as usize;
            let callee_func_def = &module.functions[callee_func_id as usize];

            // Convert shadow frames to real CallFrames before VM takes over
            #[cfg(feature = "jit")]
            materialize_jit_frames(fiber, resume_pc);
            
            // Set up callee frame like exec_call does:
            // callee_bp = current sp (which is actual_jit_bp + jit's local_slots)
            let callee_bp = fiber.sp;
            let callee_local_slots = callee_func_def.local_slots as usize;
            let new_sp = callee_bp + callee_local_slots;
            
            fiber.ensure_capacity(new_sp);
            
            // Zero callee's local slots
            let stack = fiber.stack_ptr();
            unsafe { core::ptr::write_bytes(stack.add(callee_bp), 0, callee_local_slots) };
            
            // Copy args based on call kind
            match call_kind {
                0 => {
                    // Regular call: direct copy
                    let arg_slots = callee_func_def.param_slots as usize;
                    for i in 0..arg_slots {
                        fiber.stack[callee_bp + i] = fiber.stack[actual_jit_bp + call_arg_start + i];
                    }
                }
                1 => {
                    // Closure call: use call_layout for slot0 and arg_offset
                    use vo_runtime::objects::closure;
                    use vo_runtime::gc::GcRef;
                    
                    let closure_ref = ctx.ctx.call_closure_ref;
                    let closure_gcref = closure_ref as GcRef;
                    let recv_slots = callee_func_def.recv_slots as usize;
                    let is_closure = callee_func_def.is_closure;
                    let user_arg_slots = ctx.ctx.call_arg_slots as usize;
                    
                    let layout = closure::call_layout(closure_ref, closure_gcref, recv_slots, is_closure);
                    
                    if let Some(slot0_val) = layout.slot0 {
                        fiber.stack[callee_bp] = slot0_val;
                    }
                    
                    // Copy user args at arg_offset
                    for i in 0..user_arg_slots {
                        fiber.stack[callee_bp + layout.arg_offset + i] = fiber.stack[actual_jit_bp + call_arg_start + i];
                    }
                }
                2 => {
                    // Interface call: set receiver in slot0, copy args at recv_slots
                    let recv = ctx.ctx.call_iface_recv;
                    let recv_slots = callee_func_def.recv_slots as usize;
                    let user_arg_slots = ctx.ctx.call_arg_slots as usize;
                    
                    fiber.stack[callee_bp] = recv;
                    
                    for i in 0..user_arg_slots {
                        fiber.stack[callee_bp + recv_slots + i] = fiber.stack[actual_jit_bp + call_arg_start + i];
                    }
                }
                _ => unreachable!("invalid call_kind"),
            }
            
            fiber.sp = new_sp;
            fiber.frames.push(CallFrame::new(
                callee_func_id,
                callee_bp,
                call_arg_start as u16, // ret_reg: where caller expects returns (relative to caller_bp)
                callee_ret_slots as u16,
            ));

            // Check if callee can be JIT-executed instead of interpreter fallback
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                if let Some(jit_func) = jit_mgr.resolve_call(callee_func_id, callee_func_def, module) {
                    // Execute callee via JIT recursively
                    // Pass caller's bp and arg_start so return values go to the right place
                    return execute_jit_callee(vm, fiber, module, jit_func, callee_func_id, callee_bp, callee_ret_slots, actual_jit_bp, call_arg_start);
                }
            }

            ExecResult::FrameChanged
        }
        #[cfg(feature = "std")]
        JitResult::WaitIo => {
            // JIT hit a blocking I/O operation.
            // JIT locals are already in fiber.stack[jit_bp..] - no copy needed!
            
            // Update frame PC to resume_pc for when we come back
            let resume_pc = ctx.call_resume_pc();
            let io_token = ctx.wait_io_token();
            
            // Convert shadow frames to real CallFrames before VM parks fiber
            #[cfg(feature = "jit")]
            materialize_jit_frames(fiber, resume_pc);
            
            // Store IO token for scheduler
            fiber.resume_io_token = Some(io_token);
            
            ExecResult::Block(crate::fiber::BlockReason::Io(io_token))
        }
        #[cfg(not(feature = "std"))]
        JitResult::WaitIo => {
            panic!("JIT returned WaitIo but std feature not enabled")
        }
    }
}

/// Convert resume_stack to fiber.frames when VM takes over from JIT.
///
/// This is called when JIT returns Call/WaitIo. The resume_stack contains
/// shadow frames for the JIT call chain. We convert them to real CallFrames
/// so the VM can continue execution and GC can scan them.
///
/// With lazy push, resume_stack is built in REVERSE order (callee first, then caller):
/// - JIT A calls B, B calls C, C returns Call
/// - C's frame info is pushed first (by B's non-OK handler)
/// - B's frame info is pushed next (by A's non-OK handler)
/// - resume_stack = [C_info, B_info] (reverse order!)
///
/// We need to reverse it to get [B, C] order for fiber.frames.
///
/// Also updates the entry frame's pc (fiber.frames.last()) when resume_stack is empty,
/// ensuring the caller can resume from the correct pc.
#[cfg(feature = "jit")]
fn materialize_jit_frames(fiber: &mut Fiber, resume_pc: u32) {
    let len = fiber.resume_stack.len();
    
    if len == 0 {
        // No shadow frames, but still update entry frame's pc for resume
        if let Some(frame) = fiber.frames.last_mut() {
            frame.pc = resume_pc as usize;
        }
        return;
    }
    
    // resume_stack is in reverse order (callee first), we need caller first.
    // Iterate in reverse to get correct order.
    // 
    // Entry frame's pc should be set to the first (last in reversed order) resume_pc.
    if let Some(entry_frame) = fiber.frames.last_mut() {
        // The last element in resume_stack is the outermost caller's info
        entry_frame.pc = fiber.resume_stack[len - 1].resume_pc as usize;
    }
    
    // Convert in reverse order (from outermost caller to innermost callee)
    for i in (0..len).rev() {
        let rp = &fiber.resume_stack[i];
        
        // pc: for innermost frame (i=0), use resume_pc
        // for others, use the previous frame's (i-1) resume_pc
        let pc = if i == 0 {
            resume_pc as usize
        } else {
            fiber.resume_stack[i - 1].resume_pc as usize
        };
        
        let mut frame = CallFrame::new(
            rp.func_id,
            rp.bp as usize,
            rp.ret_reg,
            rp.ret_slots,
        );
        frame.pc = pc;
        
        fiber.frames.push(frame);
    }
    
    // Clear resume_stack since VM now owns the frames
    fiber.resume_stack.clear();
}

/// Trampoline for JIT code to call VM-interpreted functions.
///
/// This is used when a JIT-compiled function calls another function
/// that isn't JIT-compiled (VM fallback path in JIT-to-JIT calls).
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_call_vm_trampoline(
    vm: *mut core::ffi::c_void,
    fiber: *mut core::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    let vm = unsafe { &mut *(vm as *mut Vm) };
    let args_slice = unsafe { std::slice::from_raw_parts(args, arg_count as usize) };

    // Execute using callback fiber
    let (success, _panic_state) = vm.execute_closure_sync(func_id, args_slice, ret, ret_count);

    if success {
        JitResult::Ok
    } else {
        JitResult::Panic
    }
}

/// Callback for JIT code to call extern functions.
///
/// This is set as `call_extern_fn` in JitContext and invoked by `vo_call_extern`.
/// Returns JitResult::WaitIo if extern function blocks on I/O.
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
#[cfg(feature = "std")]
pub extern "C" fn jit_call_extern(
    ctx: *mut JitContext,
    extern_registry: *const core::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    module: *const core::ffi::c_void,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_slots: u32,
) -> JitResult {
    use vo_runtime::ffi::{ExternRegistry, ExternResult};
    let ctx_ref = unsafe { &mut *ctx };
    let registry = unsafe { &*(extern_registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    let module = unsafe { &*(module as *const Module) };
    
    // JIT passes same buffer for args and ret (args_ptr used twice in call_helpers.rs)
    // buffer_size = max(arg_count, ret_slots)
    let buffer_size = (arg_count as usize).max(ret_slots as usize).max(1);
    
    // Use the args buffer directly as our temp_stack (it's the same as ret buffer)
    let buffer = unsafe { std::slice::from_raw_parts_mut(args as *mut u64, buffer_size) };
    
    // Get additional context needed for extern calls
    let itab_cache = unsafe { &mut *ctx_ref.itab_cache };
    let program_args = unsafe { &*ctx_ref.program_args };
    let sentinel_errors = unsafe { &mut *ctx_ref.sentinel_errors };
    let io = unsafe { &mut *ctx_ref.io };
    
    // Get resume_io_token from fiber (for replay-at-PC semantics)
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let resume_io_token = fiber.resume_io_token.take();
    
    let result = registry.call(
        extern_id,
        buffer,
        0, // bp = 0 (start of buffer)
        0, // arg_start = 0
        arg_count as u16,
        0, // ret_start = 0 (returns overwrite args in same buffer)
        gc,
        &module.struct_metas,
        &module.interface_metas,
        &module.named_type_metas,
        &module.runtime_types,
        itab_cache,
        &module.functions,
        module,
        ctx_ref.vm,
        ctx_ref.fiber,
        Some(super::closure_call_trampoline), // call_closure_fn for dyn_call_closure etc
        &module.well_known,
        program_args,
        sentinel_errors,
        io,
        resume_io_token, // Pass fiber's resume_io_token for replay-at-PC semantics
    );
    
    // Return values already in buffer (same as ret pointer), no copy needed
    
    let jit_result = match result {
        ExternResult::Ok => JitResult::Ok,
        ExternResult::Panic(_msg) => {
            // Set panic message in context
            unsafe {
                *ctx_ref.panic_flag = true;
            }
            JitResult::Panic
        }
        ExternResult::Yield => {
            ctx_ref.call_kind = JitContext::CALL_KIND_YIELD;
            JitResult::Call
        }
        ExternResult::Block => {
            ctx_ref.call_kind = JitContext::CALL_KIND_BLOCK;
            JitResult::Call
        }
        ExternResult::WaitIo { token } => {
            // Store IO token in context for VM to handle
            ctx_ref.wait_io_token = token;
            JitResult::WaitIo
        }
        ExternResult::NotRegistered(_) => {
            unsafe {
                *ctx_ref.panic_flag = true;
            }
            JitResult::Panic
        }
    };
    jit_result
}

/// Push a new frame for JIT-to-JIT call (fast path).
///
/// This is the optimized version that does NOT push to resume_stack.
/// Resume points are only pushed lazily via jit_push_resume_point when
/// callee returns Call/WaitIo (slow path).
///
/// Called by JIT code for JIT-to-JIT calls:
/// 1. Ensures fiber.stack has capacity for local_slots
/// 2. Zeros the new frame region
/// 3. Updates fiber.sp
/// 4. Updates ctx.jit_bp and ctx.stack_ptr (in case of reallocation)
///
/// # Returns
/// args_ptr for the new frame (fiber.stack_ptr + new_bp)
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
#[allow(unused_variables)]
pub extern "C" fn jit_push_frame(
    ctx: *mut JitContext,
    func_id: u32,
    local_slots: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
) -> *mut u64 {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    // New frame base is current sp
    let new_bp = fiber.sp;
    let new_sp = new_bp + local_slots as usize;
    
    // Ensure capacity (may reallocate fiber.stack)
    fiber.ensure_capacity(new_sp);
    
    // NOTE: No zeroing here! With mixed stack, callee initializes its own
    // locals_slot in prologue. fiber.stack is only used for parameter passing
    // and spilling on slow path.
    
    // Update fiber.sp
    fiber.sp = new_sp;
    
    // NOTE: No resume_stack.push here! This is the fast path.
    // Resume points are pushed lazily via jit_push_resume_point only when
    // callee returns Call/WaitIo.
    
    // Update ctx fields (stack_ptr may have changed due to reallocation)
    ctx_ref.stack_ptr = fiber.stack_ptr();
    ctx_ref.stack_cap = fiber.stack.len() as u32;
    ctx_ref.jit_bp = new_bp as u32;
    ctx_ref.fiber_sp = new_sp as u32;
    
    // Return args_ptr for the new frame
    unsafe { ctx_ref.stack_ptr.add(new_bp) }
}

/// Pop the current JIT frame after callee returns (fast path).
///
/// Restores fiber.sp and ctx.jit_bp to caller's state.
/// The caller passes its saved bp directly since we don't use resume_stack anymore.
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_pop_frame(ctx: *mut JitContext, caller_bp: u32) {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    // Restore fiber.sp to caller's sp (which is callee's bp, stored in ctx.jit_bp)
    fiber.sp = ctx_ref.jit_bp as usize;
    ctx_ref.fiber_sp = ctx_ref.jit_bp;
    
    // Restore jit_bp to caller's bp (passed as parameter)
    ctx_ref.jit_bp = caller_bp;
    
    // NOTE: No resume_stack.pop here! This is the fast path.
    // Resume points are pushed lazily via jit_push_resume_point only when
    // callee returns Call/WaitIo.
}

/// Push a resume point on side-exit (Call/WaitIo) - slow path.
///
/// Called by JIT code when callee returns non-OK, before propagating the result.
/// This builds the call chain in reverse order (callee-to-caller).
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_push_resume_point(
    ctx: *mut JitContext,
    func_id: u32,
    resume_pc: u32,
    bp: u32,
    caller_bp: u32,
    ret_reg: u32,
    ret_slots: u32,
) {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    // Push to resume_stack (builds chain in reverse: callee first, then caller)
    #[cfg(feature = "jit")]
    fiber.resume_stack.push(crate::fiber::ResumePoint {
        func_id,
        resume_pc,
        bp: bp as usize,
        caller_bp: caller_bp as usize,
        ret_reg: ret_reg as u16,
        ret_slots: ret_slots as u16,
    });
}

// =============================================================================
// Loop OSR Dispatch
// =============================================================================

/// Special return values from loop OSR execution.
pub const OSR_RESULT_FRAME_CHANGED: usize = usize::MAX;
pub const OSR_RESULT_WAITIO: usize = usize::MAX - 1;

/// Execute a compiled loop via OSR.
/// Returns:
/// - Some(exit_pc) for normal exit
/// - Some(OSR_RESULT_FRAME_CHANGED) if loop made a Call (VM should continue)
/// - Some(OSR_RESULT_WAITIO) if loop needs WaitIo
/// - None for panic
pub fn dispatch_loop_osr(
    vm: &mut Vm,
    fiber_id: crate::scheduler::FiberId,
    loop_func: vo_jit::LoopFunc,
    bp: usize,
) -> Option<usize> {
    // Use raw pointers to avoid borrow conflicts
    let module_ptr = vm.module.as_ref().unwrap() as *const Module;
    let fiber_ptr = vm.scheduler.get_fiber_mut(fiber_id) as *mut Fiber;
    
    let (result, ctx) = unsafe {
        let module = &*module_ptr;
        let fiber = &mut *fiber_ptr;
        
        // Build JitContext
        let mut ctx = build_jit_context(vm, fiber, module);
        ctx.ctx.stack_ptr = fiber.stack_ptr();
        ctx.ctx.stack_cap = fiber.stack.len() as u32;
        ctx.ctx.jit_bp = bp as u32;
        
        // locals_ptr points to fiber.stack[bp..]
        let locals_ptr = fiber.stack_ptr().add(bp);
        
        // Call loop function
        let result = loop_func(ctx.as_ptr(), locals_ptr);
        (result, ctx)
    };
    
    let fiber = unsafe { &mut *fiber_ptr };
    let module = unsafe { &*module_ptr };
    
    match result {
        JitResult::Ok => {
            // Normal exit - exit_pc is in ctx.loop_exit_pc
            Some(ctx.ctx.loop_exit_pc as usize)
        }
        JitResult::Panic => {
            let mut panic_msg = ctx.panic_msg();
            
            // Check if panic_msg is nil (runtime panic like nil pointer)
            if panic_msg.slot0 == 0 {
                let msg_str = vo_runtime::objects::string::new_from_string(
                    &mut vm.state.gc,
                    helpers::ERR_NIL_POINTER.to_string(),
                );
                let slot0 = vo_runtime::objects::interface::pack_slot0(
                    0, 0, vo_runtime::ValueKind::String
                );
                panic_msg = InterfaceSlot::new(slot0, msg_str as u64);
            }
            
            fiber.set_recoverable_panic(panic_msg);
            None
        }
        JitResult::Call => {
            // Loop wants to make a VM call
            let callee_func_id = ctx.call_func_id();
            let call_arg_start = ctx.call_arg_start() as usize;
            let resume_pc = ctx.call_resume_pc();
            let callee_ret_slots = ctx.call_ret_slots();
            let call_kind = ctx.ctx.call_kind;
            
            // Check for special call_kind values (Yield/Block)
            match call_kind {
                JitContext::CALL_KIND_YIELD | JitContext::CALL_KIND_BLOCK => {
                    // Update frame PC for resume
                    if let Some(frame) = fiber.current_frame_mut() {
                        frame.pc = resume_pc as usize;
                    }
                    return Some(OSR_RESULT_FRAME_CHANGED);
                }
                _ => {}
            }
            
            let callee_func_def = &module.functions[callee_func_id as usize];
            
            // Update current frame PC for resume after callee returns
            if let Some(frame) = fiber.current_frame_mut() {
                frame.pc = resume_pc as usize;
            }
            
            // Set up callee frame
            let callee_bp = fiber.sp;
            let callee_local_slots = callee_func_def.local_slots as usize;
            let new_sp = callee_bp + callee_local_slots;
            
            fiber.ensure_capacity(new_sp);
            
            // Zero callee's local slots
            let stack = fiber.stack_ptr();
            unsafe { core::ptr::write_bytes(stack.add(callee_bp), 0, callee_local_slots) };
            
            // Copy args based on call kind
            match call_kind {
                0 => {
                    // Regular call
                    let arg_slots = callee_func_def.param_slots as usize;
                    for i in 0..arg_slots {
                        fiber.stack[callee_bp + i] = fiber.stack[bp + call_arg_start + i];
                    }
                }
                1 => {
                    // Closure call
                    use vo_runtime::objects::closure;
                    use vo_runtime::gc::GcRef;
                    
                    let closure_ref = ctx.ctx.call_closure_ref;
                    let closure_gcref = closure_ref as GcRef;
                    let recv_slots = callee_func_def.recv_slots as usize;
                    let is_closure = callee_func_def.is_closure;
                    let user_arg_slots = ctx.ctx.call_arg_slots as usize;
                    
                    let layout = closure::call_layout(closure_ref, closure_gcref, recv_slots, is_closure);
                    
                    if let Some(slot0_val) = layout.slot0 {
                        fiber.stack[callee_bp] = slot0_val;
                    }
                    
                    for i in 0..user_arg_slots {
                        fiber.stack[callee_bp + layout.arg_offset + i] = fiber.stack[bp + call_arg_start + i];
                    }
                }
                2 => {
                    // Interface call
                    let recv = ctx.ctx.call_iface_recv;
                    let recv_slots = callee_func_def.recv_slots as usize;
                    let user_arg_slots = ctx.ctx.call_arg_slots as usize;
                    
                    fiber.stack[callee_bp] = recv;
                    
                    for i in 0..user_arg_slots {
                        fiber.stack[callee_bp + recv_slots + i] = fiber.stack[bp + call_arg_start + i];
                    }
                }
                _ => unreachable!("invalid call_kind"),
            }
            
            fiber.sp = new_sp;
            fiber.frames.push(CallFrame::new(
                callee_func_id,
                callee_bp,
                call_arg_start as u16,
                callee_ret_slots,
            ));
            
            Some(OSR_RESULT_FRAME_CHANGED)
        }
        #[cfg(feature = "std")]
        JitResult::WaitIo => {
            let token = ctx.wait_io_token();
            let resume_pc = ctx.call_resume_pc();
            
            // Update frame for resume
            if let Some(frame) = fiber.current_frame_mut() {
                frame.pc = resume_pc as usize;
            }
            
            // Store token for scheduler
            fiber.resume_io_token = Some(token);
            
            Some(OSR_RESULT_WAITIO)
        }
        #[cfg(not(feature = "std"))]
        JitResult::WaitIo => {
            panic!("Loop OSR returned WaitIo but std feature not enabled")
        }
    }
}

