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
        // Batch 1 callbacks
        create_island_fn: Some(jit_create_island),
        chan_close_fn: Some(jit_chan_close),
        port_close_fn: Some(jit_port_close),
        // Batch 2 callbacks
        chan_send_fn: Some(jit_chan_send),
        chan_recv_fn: Some(jit_chan_recv),
        // Batch 3 callbacks
        port_send_fn: Some(jit_port_send),
        port_recv_fn: Some(jit_port_recv),
        // Batch 4 callbacks
        go_start_fn: Some(jit_go_start),
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
            // JIT hit a blocking operation (channel or I/O).
            // JIT locals are already in fiber.stack[jit_bp..] - no copy needed!
            
            let resume_pc = ctx.call_resume_pc();
            let io_token = ctx.wait_io_token();
            
            // Convert shadow frames to real CallFrames before VM parks fiber
            #[cfg(feature = "jit")]
            materialize_jit_frames(fiber, resume_pc);
            
            // Distinguish channel ops (Queue) vs real I/O (Io):
            // - Channel ops: wait_io_token == 0, use Queue (no pc -= 1 in scheduler)
            // - I/O ops: wait_io_token != 0, use Io (scheduler does pc -= 1)
            if io_token == 0 {
                ExecResult::Block(crate::fiber::BlockReason::Queue)
            } else {
                fiber.resume_io_token = Some(io_token);
                ExecResult::Block(crate::fiber::BlockReason::Io(io_token))
            }
        }
        #[cfg(not(feature = "std"))]
        JitResult::WaitIo => {
            // In no_std, only Queue-based blocking is supported
            let resume_pc = ctx.call_resume_pc();
            #[cfg(feature = "jit")]
            materialize_jit_frames(fiber, resume_pc);
            ExecResult::Block(crate::fiber::BlockReason::Queue)
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

    // Trigger func JIT compilation for the target function if not already compiled.
    // This ensures that future JIT-to-JIT calls can use the compiled version.
    if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
        if !jit_mgr.is_compiled(func_id) && !jit_mgr.is_unsupported(func_id) {
            let module = vm.module.as_ref().unwrap();
            let func_def = &module.functions[func_id as usize];
            let _ = jit_mgr.compile_function(func_id, func_def, module);
        }
    }

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

// =============================================================================
// Batch 1: JIT Callbacks for Island/Channel/Port operations
// =============================================================================

/// JIT callback to create a new island.
/// Returns the island handle as u64.
#[cfg(feature = "std")]
extern "C" fn jit_create_island(ctx: *mut JitContext) -> u64 {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const Module) };
    
    let next_id = vm.state.next_island_id;
    vm.state.next_island_id += 1;
    
    // Create island with proper registration
    let (tx, rx) = std::sync::mpsc::channel::<vo_runtime::island::IslandCommand>();
    let handle = vo_runtime::island::create(&mut vm.state.gc, next_id);
    
    // Initialize island registry if needed
    if vm.state.island_registry.is_none() {
        let (main_tx, main_rx) = std::sync::mpsc::channel::<vo_runtime::island::IslandCommand>();
        let mut registry = std::collections::HashMap::new();
        registry.insert(0u32, main_tx);
        vm.state.island_registry = Some(std::sync::Arc::new(std::sync::Mutex::new(registry)));
        vm.state.main_cmd_rx = Some(main_rx);
    }
    
    // Register the island's command channel
    let registry = vm.state.island_registry.as_ref().unwrap().clone();
    { let mut guard = registry.lock().unwrap(); guard.insert(next_id, tx.clone()); }
    
    // Spawn island thread
    let module_arc = std::sync::Arc::new(module.clone());
    let registry_clone = registry.clone();
    let join_handle = std::thread::spawn(move || {
        crate::vm::island_thread::run_island_thread(next_id, module_arc, rx, registry_clone);
    });
    
    // Save thread handle
    vm.state.island_threads.push(crate::vm::types::IslandThread {
        handle, command_tx: tx, join_handle: Some(join_handle),
    });
    
    handle as u64
}

#[cfg(not(feature = "std"))]
extern "C" fn jit_create_island(ctx: *mut JitContext) -> u64 {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    
    // Create dummy main island handle for no_std
    let handle = vo_runtime::island::create_main(&mut vm.state.gc);
    handle as u64
}

/// Helper: set panic message on fiber and return JitResult::Panic.
fn set_jit_panic(gc: &mut vo_runtime::gc::Gc, fiber: &mut Fiber, msg: &str) -> JitResult {
    let panic_str = vo_runtime::objects::string::new_from_string(gc, msg.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
    JitResult::Panic
}

/// JIT callback to close a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel.
extern "C" fn jit_chan_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::channel;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let ch = chan as GcRef;
    
    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_NIL_CHANNEL);
    }
    
    let state = channel::get_state(ch);
    if state.is_closed() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_CLOSED_CHANNEL);
    }
    
    state.close();
    
    // Wake all waiting fibers
    let mut wake_ids: Vec<u32> = state.take_waiting_receivers().into_iter().map(|id| id as u32).collect();
    wake_ids.extend(state.take_waiting_senders().into_iter().map(|(id, _)| id as u32));
    
    for id in wake_ids {
        let fiber_id = crate::scheduler::FiberId::from_raw(id);
        vm.scheduler.wake_fiber(fiber_id);
    }
    
    JitResult::Ok
}

/// JIT callback to close a port.
/// Returns JitResult::Ok on success, JitResult::Panic on nil port.
#[cfg(feature = "std")]
extern "C" fn jit_port_close(ctx: *mut JitContext, port: u64) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::port;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let p = port as GcRef;
    
    if p.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_NIL_CHANNEL);
    }
    
    // Port doesn't panic on double close
    if port::is_closed(p) {
        return JitResult::Ok;
    }
    
    port::close(p);
    
    // Wake all waiting fibers (remote waiters)
    let mut waiters = port::take_waiting_receivers(p);
    for (sender, _) in port::take_waiting_senders(p) {
        waiters.push(sender);
    }
    
    for waiter in &waiters {
        vm.state.wake_waiter(waiter, &mut vm.scheduler);
    }
    
    JitResult::Ok
}

#[cfg(not(feature = "std"))]
extern "C" fn jit_port_close(_ctx: *mut JitContext, _port: u64) -> JitResult {
    // Ports not supported in no_std
    JitResult::Ok
}

// =============================================================================
// Batch 2: JIT Callbacks for Channel Send/Recv
// =============================================================================

/// JIT callback to send on a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel,
/// or JitResult::WaitIo if the send would block.
extern "C" fn jit_chan_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::channel::{self, SendResult};
    use vo_runtime::objects::queue_state;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let ch = chan as GcRef;
    
    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_NIL);
    }
    
    // Read value slots from val_ptr
    let value: Box<[u64]> = (0..val_slots as usize)
        .map(|i| unsafe { *val_ptr.add(i) })
        .collect();
    
    let cap = queue_state::capacity(ch);
    let state = channel::get_state(ch);
    
    match state.try_send(value, cap) {
        SendResult::DirectSend(receiver_id) => {
            // Wake the receiver
            let fiber_id = crate::scheduler::FiberId::from_raw(receiver_id as u32);
            vm.scheduler.wake_fiber(fiber_id);
            JitResult::Ok
        }
        SendResult::Buffered => JitResult::Ok,
        SendResult::WouldBlock(value) => {
            // Register sender and block
            let fiber_id = fiber.id as u64;
            state.register_sender(fiber_id, value);
            JitResult::WaitIo
        }
        SendResult::Closed => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
    }
}

/// JIT callback to receive from a channel.
/// Returns JitResult::Ok on success (including closed channel case),
/// JitResult::Panic on nil channel, or JitResult::WaitIo if would block.
/// 
/// dst_ptr points to where the received value should be written.
/// If has_ok is true, writes 1/0 to dst_ptr[elem_slots] indicating success.
extern "C" fn jit_chan_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::channel::{self, RecvResult};
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let ch = chan as GcRef;
    let has_ok = has_ok != 0;
    
    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_RECV_ON_NIL);
    }
    
    let state = channel::get_state(ch);
    let (result, value) = state.try_recv();
    
    match result {
        RecvResult::Success(woke_sender) => {
            // Write received value to dst_ptr
            if let Some(val) = value {
                for (i, &v) in val.iter().enumerate() {
                    if i < elem_slots as usize {
                        unsafe { *dst_ptr.add(i) = v; }
                    }
                }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 1; }
            }
            // Wake sender if any
            if let Some(id) = woke_sender {
                let fiber_id = crate::scheduler::FiberId::from_raw(id as u32);
                vm.scheduler.wake_fiber(fiber_id);
            }
            JitResult::Ok
        }
        RecvResult::WouldBlock => {
            // Register receiver and block
            let fiber_id = fiber.id as u64;
            state.register_receiver(fiber_id);
            JitResult::WaitIo
        }
        RecvResult::Closed => {
            // Zero out the destination and set ok=false
            for i in 0..elem_slots as usize {
                unsafe { *dst_ptr.add(i) = 0; }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 0; }
            }
            JitResult::Ok
        }
    }
}


// =============================================================================
// Batch 3: JIT Callbacks for Port Send/Recv
// =============================================================================

/// JIT callback to send on a port.
/// Returns JitResult::Ok on success, JitResult::Panic on closed port,
/// or JitResult::WaitIo if the send would block.
#[cfg(feature = "std")]
extern "C" fn jit_port_send(
    ctx: *mut JitContext,
    port: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::port::{self, SendResult};
    use vo_runtime::objects::queue_state;
    use vo_runtime::pack::pack_slots;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    let p = port as GcRef;
    
    if port::is_closed(p) {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED);
    }
    
    // Read value slots from val_ptr
    let src: Vec<u64> = (0..val_slots as usize)
        .map(|i| unsafe { *val_ptr.add(i) })
        .collect();
    
    // Pack the value for cross-island transfer
    let elem_meta = queue_state::elem_meta(p);
    let packed = pack_slots(&vm.state.gc, &src, elem_meta, &module.struct_metas, &module.runtime_types);
    
    let cap = queue_state::capacity(p);
    match port::try_send(p, packed) {
        SendResult::DirectSend(receiver) => {
            // Wake the receiver on remote island
            vm.state.wake_waiter(&receiver, &mut vm.scheduler);
            JitResult::Ok
        }
        SendResult::Buffered => JitResult::Ok,
        SendResult::WouldBlock(value) => {
            // Register sender and block
            let island_id = vm.state.current_island_id;
            let waiter = vo_runtime::objects::port::WaiterInfo { island_id, fiber_id: fiber.id as u64 };
            port::register_sender(p, waiter, value);
            JitResult::WaitIo
        }
        SendResult::Closed => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
    }
}

#[cfg(not(feature = "std"))]
extern "C" fn jit_port_send(_ctx: *mut JitContext, _port: u64, _val_ptr: *const u64, _val_slots: u32) -> JitResult {
    panic!("Port not supported in no_std mode")
}

/// JIT callback to receive from a port.
/// Returns JitResult::Ok on success (including closed port),
/// or JitResult::WaitIo if would block.
#[cfg(feature = "std")]
extern "C" fn jit_port_recv(
    ctx: *mut JitContext,
    port: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::port::{self, RecvResult};
    use vo_runtime::pack::unpack_slots;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    let p = port as GcRef;
    let has_ok = has_ok != 0;
    
    let (result, packed_opt) = port::try_recv(p);
    
    match result {
        RecvResult::Success(woke_sender) => {
            // Unpack the value into destination
            if let Some(packed) = packed_opt {
                let mut dst: Vec<u64> = vec![0; elem_slots as usize];
                unpack_slots(&mut vm.state.gc, &packed, &mut dst, &module.struct_metas, &module.runtime_types);
                for i in 0..elem_slots as usize {
                    unsafe { *dst_ptr.add(i) = dst[i]; }
                }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 1; }
            }
            // Wake sender if any
            if let Some(sender) = woke_sender {
                vm.state.wake_waiter(&sender, &mut vm.scheduler);
            }
            JitResult::Ok
        }
        RecvResult::WouldBlock => {
            // Register receiver and block
            let island_id = vm.state.current_island_id;
            let waiter = vo_runtime::objects::port::WaiterInfo { island_id, fiber_id: fiber.id as u64 };
            port::register_receiver(p, waiter);
            JitResult::WaitIo
        }
        RecvResult::Closed => {
            // Zero out the destination and set ok=false
            for i in 0..elem_slots as usize {
                unsafe { *dst_ptr.add(i) = 0; }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 0; }
            }
            JitResult::Ok
        }
    }
}

#[cfg(not(feature = "std"))]
extern "C" fn jit_port_recv(_ctx: *mut JitContext, _port: u64, _dst_ptr: *mut u64, _elem_slots: u32, _has_ok: u32) -> JitResult {
    panic!("Port not supported in no_std mode")
}

// =============================================================================
// Batch 4: JIT Callback for GoStart
// =============================================================================

/// JIT callback to spawn a new goroutine.
/// This is fire-and-forget - creates a new fiber and adds it to the scheduler.
extern "C" fn jit_go_start(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::closure;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    
    let is_closure = is_closure != 0;
    
    // Get func_id: from closure if is_closure, otherwise from parameter
    let actual_func_id = if is_closure {
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
    if is_closure {
        // Closure goes in reg[0], args start at reg[1]
        unsafe { *new_stack = closure_ref };
        for i in 0..arg_slots as usize {
            unsafe { *new_stack.add(1 + i) = *args_ptr.add(i) };
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
