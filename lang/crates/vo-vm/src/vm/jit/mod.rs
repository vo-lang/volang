//! JIT dispatch: bridge between VM interpreter and JIT-compiled code.
//!
//! ## Overview
//!
//! When VM encounters a `Call` instruction for a JIT-compiled function:
//! 1. `dispatch_jit_call` allocates frame in fiber.stack and prepares JitContext
//! 2. JIT function executes natively, using fiber.stack directly
//! 3. Results (Ok/Panic/Call/WaitIo/WaitQueue) are translated back to VM state
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
//! - On Call/WaitIo/WaitQueue: `materialize_jit_frames` converts shadow frames to real CallFrames
//!
//! This avoids redundant frame management during pure JIT execution.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::bytecode::Module;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContext, JitResult};
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::{CallFrame, Fiber};
use crate::vm::{helpers, ExecResult, Vm};

pub mod callbacks;
mod context;
mod frame;

pub use context::{JitContextWrapper, build_jit_context};

/// Create default panic message for runtime errors (nil deref, bounds check, etc).
fn create_default_panic_msg(gc: &mut vo_runtime::gc::Gc) -> InterfaceSlot {
    let msg_str = vo_runtime::objects::string::new_from_string(gc, helpers::ERR_NIL_POINTER.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    InterfaceSlot::new(slot0, msg_str as u64)
}

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

    invoke_jit_and_handle(vm, fiber, module, jit_func, jit_bp, ret_slots, caller_bp, arg_start)
}

/// Execute a JIT callee when frame is already set up (from Call request).
///
/// Called recursively from handle_jit_result when the callee can be JIT-executed.
/// The callee's frame has already been pushed to fiber.frames.
fn execute_jit_callee(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    jit_func: vo_jit::JitFunc,
    _callee_func_id: u32,
    callee_bp: usize,
    callee_ret_slots: usize,
    caller_bp: usize,
    caller_arg_start: usize,
) -> ExecResult {
    invoke_jit_and_handle(vm, fiber, module, jit_func, callee_bp, callee_ret_slots, caller_bp, caller_arg_start)
}

/// Shared JIT invocation: build context, call function, handle result.
fn invoke_jit_and_handle(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    jit_func: vo_jit::JitFunc,
    jit_bp: usize,
    ret_slots: usize,
    caller_bp: usize,
    caller_arg_start: usize,
) -> ExecResult {
    let mut ctx = build_jit_context(vm, fiber, module);
    ctx.ctx.stack_ptr = fiber.stack_ptr();
    ctx.ctx.stack_cap = fiber.stack.len() as u32;
    ctx.ctx.jit_bp = jit_bp as u32;

    let mut ret: Vec<u64> = vec![0u64; ret_slots.max(1)];
    let args_ptr = unsafe { fiber.stack_ptr().add(jit_bp) };
    let result = jit_func(ctx.as_ptr(), args_ptr, ret.as_mut_ptr());

    handle_jit_result(vm, fiber, module, result, ctx, caller_bp, caller_arg_start, ret_slots, jit_bp, &ret)
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
            fiber.resume_stack.clear();

            let frame_depth = fiber.frames.len();
            let has_defers = fiber.defer_stack.last()
                .map_or(false, |e| e.frame_depth == frame_depth);

            let frame = fiber.frames.last().unwrap();
            let func = &module.functions[frame.func_id as usize];

            // Guard ctx metadata with function's own attributes.
            // Nested JIT callees may leave stale values in ctx; the function's
            // static properties are the ground truth.
            let heap_returns = func.heap_ret_gcref_count > 0 && ctx.ctx.ret_is_heap != 0;
            let ret_gcref_start = ctx.ctx.ret_gcref_start as usize;

            // errdefer should run if:
            // - explicit fail return (set by JIT), OR
            // - function has error return and error is non-nil
            let include_errdefers = if func.error_ret_slot < 0 {
                false
            } else if ctx.ctx.is_error_return != 0 {
                true
            } else if heap_returns {
                // error_ret_slot is the index within heap returns, read from GcRef
                let gcref_count = func.heap_ret_gcref_count as usize;
                let error_gcref_idx = func.error_ret_slot as usize;
                if gcref_count == 0 || error_gcref_idx >= gcref_count {
                    false
                } else {
                    let bp = frame.bp;
                    let gcref_raw = fiber.stack[bp + ret_gcref_start + error_gcref_idx];
                    if gcref_raw == 0 {
                        false
                    } else {
                        // Read slot0 of the error interface from heap
                        let slot0 = unsafe { *(gcref_raw as vo_runtime::gc::GcRef) };
                        (slot0 & 0xFF) != 0
                    }
                }
            } else {
                let idx = func.error_ret_slot as usize;
                if idx < ret.len() {
                    (ret[idx] & 0xFF) != 0
                } else {
                    false
                }
            };

            // When there are defers, or when heap returns are enabled, we must
            // delegate to the unified unwinding engine.
            if has_defers || heap_returns {
                let ret_start = ctx.ret_start() as usize;
                return crate::exec::handle_jit_ok_return(
                    fiber,
                    func,
                    module,
                    &ret[..ret_slots],
                    heap_returns,
                    ret_gcref_start,
                    ret_start,
                    include_errdefers,
                );
            }

            // No defers, stack returns: fast path
            fiber.frames.pop();
            fiber.sp = jit_bp;

            for i in 0..ret_slots {
                fiber.stack[caller_bp + arg_start + i] = ret[i];
            }
            ExecResult::FrameChanged
        }
        JitResult::Panic => {
            // Unified panic handling for JIT and VM:
            // - User panic (via panic() call): is_user_panic=true, use ctx.panic_msg (may be nil)
            // - Runtime error (nil deref, bounds): is_user_panic=false, create default message
            // - VM fallback panic: fiber.panic_state already set
            
            let is_user_panic = ctx.is_user_panic();
            if is_user_panic {
                // User panic: use the value they passed (even if nil interface)
                fiber.set_recoverable_panic(ctx.panic_msg());
            }
            // else: VM fallback may have set fiber.panic_state, or it's a runtime error
            
            // Get panic message from fiber, or create default for runtime errors
            let panic_msg = fiber.take_recoverable_panic()
                .unwrap_or_else(|| create_default_panic_msg(&mut vm.state.gc));
            
            materialize_jit_frames(fiber, module, 0);
            
            // Set panic state and start unwinding
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
                _ => {} // Regular or Prepared call - continue to frame setup
            }
            
            let callee_func_id = ctx.call_func_id();
            let call_arg_start = ctx.call_arg_start() as usize;
            let callee_ret_slots = ctx.call_ret_slots() as usize;

            if call_kind == JitContext::CALL_KIND_PREPARED {
                let callee_bp = ctx.call_resume_pc() as usize;
                let caller_resume_pc = ctx.call_arg_start() as u32;
                let call_ret_reg = ctx.call_ret_reg();
                setup_prepared_call(
                    fiber, module, callee_func_id,
                    callee_ret_slots as u16, call_ret_reg,
                    callee_bp, caller_resume_pc,
                );
                // Trigger JIT compilation for callee so future dynamic calls
                // can use the JIT-to-JIT fast path.
                if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                    let callee_func_def = &module.functions[callee_func_id as usize];
                    let _ = jit_mgr.resolve_call(callee_func_id, callee_func_def, module);
                }
                return ExecResult::FrameChanged;
            }
            
            // Regular call: JIT requests VM to execute a non-JIT function.
            let resume_pc = ctx.call_resume_pc();
            let call_ret_reg = ctx.call_ret_reg();
            let (callee_bp, actual_caller_bp) = setup_regular_call(
                fiber, module, callee_func_id,
                callee_ret_slots as u16, call_ret_reg,
                call_arg_start, resume_pc,
            );

            // Check if callee can be JIT-executed instead of interpreter fallback
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                let callee_func_def = &module.functions[callee_func_id as usize];
                if let Some(jit_func) = jit_mgr.resolve_call(callee_func_id, callee_func_def, module) {
                    return execute_jit_callee(vm, fiber, module, jit_func, callee_func_id, callee_bp, callee_ret_slots, actual_caller_bp, call_arg_start);
                }
            }

            ExecResult::FrameChanged
        }
        JitResult::WaitIo => {
            let resume_pc = ctx.call_resume_pc();
            materialize_jit_frames(fiber, module, resume_pc);
            
            #[cfg(feature = "std")]
            {
                let io_token = ctx.wait_io_token();
                fiber.resume_io_token = Some(io_token);
                ExecResult::Block(crate::fiber::BlockReason::Io(io_token))
            }
            #[cfg(not(feature = "std"))]
            {
                panic!("JIT returned WaitIo but std feature not enabled")
            }
        }
        JitResult::WaitQueue => {
            let resume_pc = ctx.call_resume_pc();
            materialize_jit_frames(fiber, module, resume_pc);
            ExecResult::Block(crate::fiber::BlockReason::Queue)
        }
        JitResult::Replay => {
            // Extern returned CallClosure — exit JIT and let VM handle it.
            // resume_pc points at the CallExtern instruction itself, so VM will
            // re-execute it and go through the suspend/replay path.
            let resume_pc = ctx.call_resume_pc();
            materialize_jit_frames(fiber, module, resume_pc);
            ExecResult::FrameChanged
        }
    }
}

// =============================================================================
// Shared call frame setup helpers (used by both handle_jit_result and dispatch_loop_osr)
// =============================================================================

/// Set up a prepared call frame (closure/interface call where args are already in place).
///
/// Materializes JIT frames, zeros non-arg local slots, sets fiber.sp, and pushes
/// the callee's CallFrame. Returns the callee_bp for the caller.
fn setup_prepared_call(
    fiber: &mut Fiber,
    module: &Module,
    callee_func_id: u32,
    callee_ret_slots: u16,
    call_ret_reg: u16,
    callee_bp: usize,
    caller_resume_pc: u32,
) {
    let callee_func_def = &module.functions[callee_func_id as usize];
    let param_slots = callee_func_def.param_slots as usize;
    let local_slots = callee_func_def.local_slots as usize;
    let new_sp = callee_bp + local_slots;
    
    // Materialize any intermediate JIT frames from non-OK propagation
    if !fiber.resume_stack.is_empty() {
        materialize_jit_frames(fiber, module, caller_resume_pc);
        // resume_stack already cleared by materialize_jit_frames
    } else {
        if let Some(frame) = fiber.frames.last_mut() {
            frame.pc = caller_resume_pc as usize;
        }
    }
    
    // Zero non-arg local slots for VM interpreter
    if local_slots > param_slots {
        let stack = fiber.stack_ptr();
        unsafe {
            core::ptr::write_bytes(
                stack.add(callee_bp + param_slots), 0,
                local_slots - param_slots
            );
        };
    }
    
    fiber.sp = new_sp;
    fiber.frames.push(CallFrame::new(
        callee_func_id,
        callee_bp,
        call_ret_reg,
        callee_ret_slots,
    ));
}

/// Set up a regular call frame (JIT requests VM to execute a non-JIT function).
///
/// Materializes JIT frames, recomputes caller bp/sp, allocates callee frame,
/// zeros locals, copies args, and pushes the callee's CallFrame.
/// Returns (callee_bp, actual_caller_bp, call_arg_start) for potential JIT re-dispatch.
fn setup_regular_call(
    fiber: &mut Fiber,
    module: &Module,
    callee_func_id: u32,
    callee_ret_slots: u16,
    call_ret_reg: u16,
    call_arg_start: usize,
    resume_pc: u32,
) -> (usize, usize) {
    materialize_jit_frames(fiber, module, resume_pc);
    
    // After materialize_jit_frames, the last frame is the immediate caller.
    let caller_frame = fiber.frames.last().unwrap();
    let actual_caller_bp = caller_frame.bp;
    let caller_func = &module.functions[caller_frame.func_id as usize];
    
    // Recompute fiber.sp from the materialized caller frame
    fiber.sp = actual_caller_bp + caller_func.local_slots as usize;
    
    let callee_func_def = &module.functions[callee_func_id as usize];
    let callee_bp = fiber.sp;
    let callee_local_slots = callee_func_def.local_slots as usize;
    let new_sp = callee_bp + callee_local_slots;
    
    fiber.ensure_capacity(new_sp);
    
    // Zero callee's local slots
    let stack = fiber.stack_ptr();
    unsafe { core::ptr::write_bytes(stack.add(callee_bp), 0, callee_local_slots) };
    
    // Copy args from caller's locals area
    let arg_slots = callee_func_def.param_slots as usize;
    for i in 0..arg_slots {
        fiber.stack[callee_bp + i] = fiber.stack[actual_caller_bp + call_arg_start + i];
    }
    
    fiber.sp = new_sp;
    fiber.frames.push(CallFrame::new(
        callee_func_id,
        callee_bp,
        call_ret_reg,
        callee_ret_slots,
    ));
    
    (callee_bp, actual_caller_bp)
}

/// Convert resume_stack to fiber.frames when VM takes over from JIT.
///
/// Called when JIT returns Call/WaitIo/Panic. The resume_stack contains
/// shadow frames for the JIT call chain. We convert them to real CallFrames
/// so the VM can continue execution, GC can scan them, and panic can unwind.
///
/// # Resume Stack Structure
///
/// resume_stack is built in REVERSE order (innermost callee first, outermost last):
/// - JIT A calls B, B calls C, C returns non-OK (e.g. WaitIo)
/// - B's non-OK handler pushes (C_func_id, B_resume_pc, C_bp, B_bp)
/// - A's non-OK handler pushes (B_func_id, A_resume_pc, B_bp, A_bp)
/// - resume_stack = [C_entry, B_entry]
///
/// We iterate in reverse to get [B, C] order for fiber.frames.
///
/// # ResumePoint Semantics
///
/// Each entry represents a CALLEE frame with the CALLER's resume info:
/// - `func_id`: CALLEE's func_id (the function whose frame is at `bp`)
/// - `bp`: callee's base pointer (where callee's frame lives in fiber.stack)
/// - `resume_pc`: CALLER's resume pc (becomes the previous frame's pc)
/// - `caller_bp`: caller's base pointer
/// - `ret_reg`/`ret_slots`: caller's return destination
///
/// The entry frame (fiber.frames.last()) is the JIT dispatch entry function.
/// Step 1 updates its pc to the outermost entry's resume_pc.
/// Step 2 creates CallFrame(callee_func_id, callee_bp) for each entry.
///
/// # OSR Deduplication
///
/// If a frame with same func_id AND bp already exists, just update pc.
fn materialize_jit_frames(fiber: &mut Fiber, module: &Module, resume_pc: u32) {
    let len = fiber.resume_stack.len();
    
    if len == 0 {
        // No shadow frames, just update entry frame's pc for resume
        if let Some(frame) = fiber.frames.last_mut() {
            frame.pc = resume_pc as usize;
        }
        return;
    }
    
    // Step 1: Update entry frame's pc (the frame that was in fiber.frames before JIT ran)
    // The last element in resume_stack is the outermost caller's info, containing
    // the resume_pc for the entry frame.
    if let Some(entry_frame) = fiber.frames.last_mut() {
        entry_frame.pc = fiber.resume_stack[len - 1].resume_pc as usize;
    }
    
    // Step 2: Convert resume_stack entries to frames (reverse order: outermost first)
    for i in (0..len).rev() {
        let rp = &fiber.resume_stack[i];
        
        // Calculate pc for this frame:
        // - innermost (i=0): use resume_pc parameter (where VM should continue)
        // - others: use resume_stack[i-1].resume_pc (the next inner frame's caller resume pc)
        let pc = if i == 0 {
            resume_pc as usize
        } else {
            fiber.resume_stack[i - 1].resume_pc as usize
        };
        
        // rp.func_id is the CALLEE's func_id, rp.bp is the callee's bp.
        // This creates a frame for the callee at the correct location.
        // rp.caller_bp is the caller's bp (used for ret_reg destination).
        // rp.resume_pc is the CALLER's resume pc (used for the previous frame's pc).
        
        // Check for OSR duplicate: same func_id AND same bp means same frame
        let existing = fiber.frames.iter_mut().find(|f| 
            f.func_id == rp.func_id && f.bp == rp.bp as usize
        );
        
        if let Some(frame) = existing {
            // OSR case: frame already exists, just update pc
            frame.pc = pc;
        } else {
            // Normal case: create new frame
            let mut frame = CallFrame::new(
                rp.func_id,
                rp.bp as usize,
                rp.ret_reg,
                rp.ret_slots,
            );
            frame.pc = pc;
            fiber.frames.push(frame);
        }
    }
    
    // Step 3: Fix fiber.sp — must cover the innermost frame.
    // The non-OK propagation chain's push_frame calls overwrote fiber.sp with
    // progressively lower values. Restore it to the innermost callee's sp.
    let innermost = &fiber.resume_stack[0];
    let innermost_local_slots = module.functions[innermost.func_id as usize].local_slots as usize;
    fiber.sp = innermost.bp + innermost_local_slots;
    
    // Clear resume_stack since VM now owns the frames
    fiber.resume_stack.clear();
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
    _ret: *mut u64,
    ret_slots: u32,
) -> JitResult {
    use vo_runtime::ffi::{ExternRegistry, ExternResult, ExternInvoke, ExternWorld, ExternFiberInputs};
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
    
    // Take closure replay state from fiber (populated by VM suspend/replay on re-entry)
    let (closure_replay_results, closure_replay_panicked) = fiber.closure_replay.take_for_extern();
    
    let invoke = ExternInvoke {
        extern_id,
        bp: 0,        // start of buffer
        arg_start: 0,
        arg_slots: arg_count as u16,
        ret_start: 0, // returns overwrite args in same buffer
        ret_slots: ret_slots as u16,
    };
    let world = ExternWorld {
        gc,
        module,
        itab_cache,
        vm_opaque: ctx_ref.vm,
        program_args,
        sentinel_errors,
        io,
    };
    let fiber_inputs = ExternFiberInputs {
        fiber_opaque: ctx_ref.fiber,
        resume_io_token,
        resume_host_event_token: fiber.resume_host_event_token.take(),
        replay_results: closure_replay_results,
        replay_panicked: closure_replay_panicked,
    };
    let result = registry.call(buffer, invoke, world, fiber_inputs);
    
    match result {
        ExternResult::Ok => JitResult::Ok,
        ExternResult::CallClosure { .. } => {
            // Exit JIT — VM will re-execute CallExtern and handle suspend/replay
            JitResult::Replay
        }
        ExternResult::Panic(msg) => {
            let msg_str = vo_runtime::objects::string::new_from_string(gc, msg);
            let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
            unsafe {
                *ctx_ref.panic_flag = true;
                *ctx_ref.is_user_panic = true;
                (*ctx_ref.panic_msg).slot0 = slot0;
                (*ctx_ref.panic_msg).slot1 = msg_str as u64;
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
        #[cfg(feature = "std")]
        ExternResult::WaitIo { token } => {
            ctx_ref.wait_io_token = token;
            JitResult::WaitIo
        }
        ExternResult::HostEventWait { .. } | ExternResult::HostEventWaitAndReplay { .. } => {
            // Exit JIT — VM will handle host event suspension
            JitResult::Replay
        }
        ExternResult::NotRegistered(_) => {
            unsafe { *ctx_ref.panic_flag = true; }
            JitResult::Panic
        }
    }
}

// =============================================================================
// Loop OSR Dispatch
// =============================================================================

/// Result of loop OSR execution.
pub enum OsrResult {
    /// Loop exited normally at exit_pc.
    ExitPc(usize),
    /// Loop made a Call — VM should refetch and continue.
    FrameChanged,
    /// Loop blocks on I/O (token stored in fiber.resume_io_token).
    #[cfg(feature = "std")]
    WaitIo,
    /// Loop blocks on channel/port.
    WaitQueue,
    /// Panic occurred during loop execution.
    Panic,
}

/// Execute a compiled loop via OSR.
pub fn dispatch_loop_osr(
    vm: &mut Vm,
    fiber_id: crate::scheduler::FiberId,
    loop_func: vo_jit::LoopFunc,
    bp: usize,
    local_slots: usize,
) -> OsrResult {
    // Use raw pointers to avoid borrow conflicts
    let module_ptr = vm.module.as_ref().unwrap() as *const Module;
    let fiber_ptr = vm.scheduler.get_fiber_mut(fiber_id) as *mut Fiber;
    
    let (result, ctx) = unsafe {
        let module = &*module_ptr;
        let fiber = &mut *fiber_ptr;
        
        // Sync fiber.sp to the correct value for this frame.
        // After a WaitIo cycle, fiber.sp may be stale (left at a higher value
        // by push_frame in the non-OK path). The correct sp is bp + local_slots.
        fiber.sp = bp + local_slots;
        
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
            // resume_stack should be empty on Ok (no nested non-OK propagation).
            #[cfg(feature = "jit")]
            fiber.resume_stack.clear();
            OsrResult::ExitPc(ctx.ctx.loop_exit_pc as usize)
        }
        JitResult::Panic => {
            // Materialize any intermediate JIT frames so panic unwinding
            // can correctly traverse the frame chain.
            materialize_jit_frames(fiber, module, 0);
            let panic_msg = if ctx.is_user_panic() {
                ctx.panic_msg()
            } else {
                create_default_panic_msg(&mut vm.state.gc)
            };
            fiber.set_recoverable_panic(panic_msg);
            OsrResult::Panic
        }
        JitResult::Call => {
            let callee_func_id = ctx.call_func_id();
            let call_arg_start = ctx.call_arg_start() as usize;
            let callee_ret_slots = ctx.call_ret_slots();
            let call_kind = ctx.ctx.call_kind;
            let call_ret_reg = ctx.call_ret_reg();
            
            // Check for special call_kind values (Yield/Block)
            match call_kind {
                JitContext::CALL_KIND_YIELD | JitContext::CALL_KIND_BLOCK => {
                    let resume_pc = ctx.call_resume_pc();
                    materialize_jit_frames(fiber, module, resume_pc);
                    return OsrResult::FrameChanged;
                }
                _ => {}
            }
            
            if call_kind == JitContext::CALL_KIND_PREPARED {
                let callee_bp = ctx.call_resume_pc() as usize;
                let caller_resume_pc = ctx.call_arg_start() as u32;
                setup_prepared_call(
                    fiber, module, callee_func_id,
                    callee_ret_slots, call_ret_reg,
                    callee_bp, caller_resume_pc,
                );
                // Hint: trigger JIT compilation for callee so future loop OSR
                // iterations can use JIT-to-JIT fast path. Error ignored because
                // this is best-effort — the VM fallback path works regardless.
                if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                    let callee_func_def = &module.functions[callee_func_id as usize];
                    let _ = jit_mgr.resolve_call(callee_func_id, callee_func_def, module);
                }
                OsrResult::FrameChanged
            } else {
                let resume_pc = ctx.call_resume_pc();
                setup_regular_call(
                    fiber, module, callee_func_id,
                    callee_ret_slots, call_ret_reg,
                    call_arg_start, resume_pc,
                );
                OsrResult::FrameChanged
            }
        }
        #[cfg(feature = "std")]
        JitResult::WaitIo => {
            let token = ctx.wait_io_token();
            let resume_pc = ctx.call_resume_pc();
            
            // Materialize any intermediate JIT frames from nested calls.
            // Without this, resume_stack entries are lost and the fiber
            // resumes at the wrong PC (callee's PC in the loop function's frame).
            materialize_jit_frames(fiber, module, resume_pc);
            
            // Store token for scheduler
            fiber.resume_io_token = Some(token);
            
            OsrResult::WaitIo
        }
        #[cfg(not(feature = "std"))]
        JitResult::WaitIo => {
            panic!("Loop OSR returned WaitIo but std feature not enabled")
        }
        JitResult::WaitQueue => {
            let resume_pc = ctx.call_resume_pc();
            materialize_jit_frames(fiber, module, resume_pc);
            OsrResult::WaitQueue
        }
        JitResult::Replay => {
            let resume_pc = ctx.call_resume_pc();
            materialize_jit_frames(fiber, module, resume_pc);
            OsrResult::FrameChanged
        }
    }
}

// =============================================================================
// Loop OSR entry points (called from VM main loop)
// =============================================================================

/// Try loop OSR at backedge. Returns None if loop not compiled/not hot.
pub(crate) fn try_loop_osr(
    vm: &mut Vm,
    fiber_id: crate::scheduler::FiberId,
    func_id: u32,
    loop_pc: usize,
    bp: usize,
) -> Option<OsrResult> {
    let loop_func = get_or_compile_loop(vm, func_id, loop_pc)?;
    let module = vm.module.as_ref().unwrap();
    let local_slots = module.functions[func_id as usize].local_slots as usize;
    Some(dispatch_loop_osr(vm, fiber_id, loop_func, bp, local_slots))
}

/// Get compiled loop or compile if hot. Returns None if not ready.
fn get_or_compile_loop(vm: &mut Vm, func_id: u32, loop_pc: usize) -> Option<vo_jit::LoopFunc> {
    use vo_runtime::instruction::Opcode;
    
    let module = vm.module.as_ref()?;
    let func_def = &module.functions[func_id as usize];
    let jit_mgr = vm.jit_mgr.as_mut()?;
    
    // Already compiled?
    if let Some(lf) = unsafe { jit_mgr.get_loop_func(func_id, loop_pc) } {
        return Some(lf);
    }
    
    // Already failed?
    if jit_mgr.is_loop_failed(func_id, loop_pc) {
        return None;
    }
    
    // Not hot yet?
    if !jit_mgr.record_backedge(func_id, loop_pc) {
        return None;
    }
    
    // Hot - try to compile
    let loop_info = match jit_mgr.find_loop(func_id, func_def, loop_pc) {
        Some(info) => info,
        None => {
            // Back-edge detected but no LoopInfo found - codegen bug or analysis bug
            // Mark as failed to avoid retrying
            jit_mgr.mark_loop_failed(func_id, loop_pc);
            return None;
        }
    };
    
    // Pre-compile Call targets so JIT-to-JIT calls can succeed
    let loop_end = loop_info.end_pc + 1;
    for pc in loop_info.begin_pc..loop_end {
        let inst = &func_def.code[pc];
        if inst.opcode() == Opcode::Call {
            let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
            if !jit_mgr.is_compiled(target_func_id) && !jit_mgr.is_unsupported(target_func_id) {
                let target_func = &module.functions[target_func_id as usize];
                let _ = jit_mgr.compile_function(target_func_id, target_func, module);
            }
        }
    }
    
    match jit_mgr.compile_loop(func_id, func_def, module, &loop_info) {
        Ok(_) => unsafe { jit_mgr.get_loop_func(func_id, loop_pc) },
        Err(_) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc);
            None
        }
    }
}
