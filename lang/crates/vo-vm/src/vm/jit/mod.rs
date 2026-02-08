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
    let _ = callee_func_id; // suppress unused warning
    handle_jit_result(vm, fiber, module, result, ctx, caller_bp, caller_arg_start, callee_ret_slots, callee_bp, &ret)
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

            let heap_returns = ctx.ctx.ret_is_heap != 0;
            let ret_gcref_start = ctx.ctx.ret_gcref_start as usize;

            let frame = fiber.frames.last().unwrap();
            let func = &module.functions[frame.func_id as usize];

            // errdefer should run if:
            // - explicit fail return (set by JIT), OR
            // - function has error return and error is non-nil
            let include_errdefers = if ctx.ctx.is_error_return != 0 {
                true
            } else if func.error_ret_slot < 0 {
                false
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
            
            materialize_jit_frames(fiber, 0);
            
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
            let callee_func_def = &module.functions[callee_func_id as usize];

            if call_kind == JitContext::CALL_KIND_PREPARED {
                // Prepared call: prepare_closure/iface_call already did:
                // - jit_push_frame: args copied to fiber.stack[callee_bp..],
                //   ensure_capacity called, fiber.sp set
                //
                // callee_bp was saved in call_resume_pc by the trampoline.
                // caller_resume_pc was saved in call_arg_start by the trampoline
                // (PREPARED repurposes arg_start since args are already copied).
                let callee_bp = ctx.call_resume_pc() as usize;
                let caller_resume_pc = ctx.call_arg_start() as u32;
                let param_slots = callee_func_def.param_slots as usize;
                let local_slots = callee_func_def.local_slots as usize;
                let new_sp = callee_bp + local_slots;
                
                // Materialize any intermediate JIT frames from non-OK propagation.
                // caller_resume_pc is the pc where the innermost caller should
                // continue after the callee returns (instruction after CallIface/CallClosure).
                if !fiber.resume_stack.is_empty() {
                    materialize_jit_frames(fiber, caller_resume_pc);
                    fiber.resume_stack.clear();
                } else {
                    // Simple case: no intermediate frames. Set entry_frame.pc directly.
                    if let Some(frame) = fiber.frames.last_mut() {
                        frame.pc = caller_resume_pc as usize;
                    }
                }
                
                // Zero non-arg local slots for VM interpreter.
                // Args are already at fiber.stack[callee_bp..callee_bp+param_slots].
                // ensure_capacity was done by prepare's push_frame.
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
                let call_ret_reg = ctx.call_ret_reg();
                fiber.frames.push(CallFrame::new(
                    callee_func_id,
                    callee_bp,
                    call_ret_reg, // ret_reg from inst.a
                    callee_ret_slots as u16,
                ));
                
                return ExecResult::FrameChanged;
            }
            
            // Regular call: JIT requests VM to execute a non-JIT function.
            // JIT locals are already in fiber.stack[jit_bp..] - no copy needed!
            let resume_pc = ctx.call_resume_pc();

            materialize_jit_frames(fiber, resume_pc);
            
            // After materialize_jit_frames, the last frame in fiber.frames is the
            // immediate caller of the requested callee. Use its bp as the source
            // for arg copying (NOT ctx.jit_bp, which was overwritten by intermediate
            // non-OK blocks' push_frame calls when resume_stack > 0).
            let caller_frame = fiber.frames.last().unwrap();
            let actual_caller_bp = caller_frame.bp;
            let caller_func = &module.functions[caller_frame.func_id as usize];
            
            // Recompute fiber.sp from the materialized caller frame.
            // Intermediate push_frame calls may have left fiber.sp at the wrong position.
            fiber.sp = actual_caller_bp + caller_func.local_slots as usize;
            
            // Set up callee frame like exec_call does
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
            let call_ret_reg = ctx.call_ret_reg();
            fiber.frames.push(CallFrame::new(
                callee_func_id,
                callee_bp,
                call_ret_reg, // ret_reg: where caller expects returns (relative to caller_bp)
                callee_ret_slots as u16,
            ));

            // Check if callee can be JIT-executed instead of interpreter fallback
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                if let Some(jit_func) = jit_mgr.resolve_call(callee_func_id, callee_func_def, module) {
                    return execute_jit_callee(vm, fiber, module, jit_func, callee_func_id, callee_bp, callee_ret_slots, actual_caller_bp, call_arg_start);
                }
            }

            ExecResult::FrameChanged
        }
        JitResult::WaitIo => {
            let resume_pc = ctx.call_resume_pc();
            materialize_jit_frames(fiber, resume_pc);
            
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
            materialize_jit_frames(fiber, resume_pc);
            ExecResult::Block(crate::fiber::BlockReason::Queue)
        }
        JitResult::Replay => {
            // Extern returned CallClosure — exit JIT and let VM handle it.
            // resume_pc points at the CallExtern instruction itself, so VM will
            // re-execute it and go through the suspend/replay path.
            let resume_pc = ctx.call_resume_pc();
            materialize_jit_frames(fiber, resume_pc);
            ExecResult::FrameChanged
        }
    }
}

/// Convert resume_stack to fiber.frames when VM takes over from JIT.
///
/// Called when JIT returns Call/WaitIo/Panic. The resume_stack contains
/// shadow frames for the JIT call chain. We convert them to real CallFrames
/// so the VM can continue execution, GC can scan them, and panic can unwind.
///
/// # Resume Stack Structure
///
/// With lazy push, resume_stack is built in REVERSE order (callee first, then caller):
/// - JIT A calls B, B calls C, C returns non-OK
/// - C's frame info is pushed first (by B's non-OK handler)
/// - B's frame info is pushed next (by A's non-OK handler)
/// - resume_stack = [C_info, B_info] (reverse order!)
///
/// We iterate in reverse to get [B, C] order for fiber.frames.
///
/// # ResumePoint Semantics
///
/// Each ResumePoint contains:
/// - func_id, bp: identifies the callee frame
/// - resume_pc: the CALLER's resume pc (where caller should continue after callee returns)
///
/// # OSR Deduplication
///
/// When a function enters JIT via OSR (On-Stack Replacement), its frame already
/// exists in fiber.frames (pushed by VM at function entry). If the same function
/// later appears in resume_stack (from JIT-to-JIT call chain), we must NOT create
/// a duplicate frame. Instead, we only update the existing frame's pc.
///
/// Detection: func_id AND bp must both match (same function at same stack position).
fn materialize_jit_frames(fiber: &mut Fiber, resume_pc: u32) {
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
    
    // Take closure_replay_results from fiber (populated by VM suspend/replay on re-entry)
    let closure_replay_results = core::mem::take(&mut fiber.closure_replay_results);
    let closure_replay_panicked = fiber.closure_replay_panicked;
    fiber.closure_replay_panicked = false;
    
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
        None, // No closure_call_fn — CallClosure handled by Replay exit
        &module.well_known,
        program_args,
        sentinel_errors,
        io,
        resume_io_token,
        closure_replay_results,
        closure_replay_panicked,
    );
    
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
        ExternResult::Yield => JitResult::Call,
        ExternResult::Block => JitResult::Call,
        #[cfg(feature = "std")]
        ExternResult::WaitIo { token } => {
            ctx_ref.wait_io_token = token;
            JitResult::WaitIo
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

/// Special return values from loop OSR execution.
pub const OSR_RESULT_FRAME_CHANGED: usize = usize::MAX;
pub const OSR_RESULT_WAITIO: usize = usize::MAX - 1;
pub const OSR_RESULT_WAITQUEUE: usize = usize::MAX - 2;

/// Execute a compiled loop via OSR.
/// Returns:
/// - Some(exit_pc) for normal exit
/// - Some(OSR_RESULT_FRAME_CHANGED) if loop made a Call (VM should continue)
/// - Some(OSR_RESULT_WAITIO) if loop blocks on I/O
/// - Some(OSR_RESULT_WAITQUEUE) if loop blocks on channel/port
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
    
    // Clear resume_stack for non-Call results. For Call results, resume_stack
    // is handled per call_kind below.
    #[cfg(feature = "jit")]
    let needs_resume_clear = !matches!(result, JitResult::Call);
    #[cfg(feature = "jit")]
    if needs_resume_clear {
        fiber.resume_stack.clear();
    }
    
    match result {
        JitResult::Ok => {
            // Loop exited normally - return exit_pc from context
            Some(ctx.ctx.loop_exit_pc as usize)
        }
        JitResult::Panic => {
            // Use is_user_panic to distinguish user panic from runtime errors
            let panic_msg = if ctx.is_user_panic() {
                ctx.panic_msg()
            } else {
                create_default_panic_msg(&mut vm.state.gc)
            };
            fiber.set_recoverable_panic(panic_msg);
            None
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
                    fiber.resume_stack.clear();
                    if let Some(frame) = fiber.current_frame_mut() {
                        frame.pc = resume_pc as usize;
                    }
                    return Some(OSR_RESULT_FRAME_CHANGED);
                }
                _ => {}
            }
            
            let callee_func_def = &module.functions[callee_func_id as usize];
            
            if call_kind == JitContext::CALL_KIND_PREPARED {
                // Prepared call (closure/interface): prepare callback already did
                // push_frame (args copied, fiber.sp set). We just need to:
                // 1. Set caller's frame.pc for resume
                // 2. Zero non-arg local slots
                // 3. Push callee frame to fiber.frames
                let callee_bp = ctx.call_resume_pc() as usize;
                let caller_resume_pc = ctx.call_arg_start() as usize;
                let param_slots = callee_func_def.param_slots as usize;
                let local_slots = callee_func_def.local_slots as usize;
                let new_sp = callee_bp + local_slots;
                
                // Materialize any intermediate JIT frames from non-OK propagation
                if !fiber.resume_stack.is_empty() {
                    materialize_jit_frames(fiber, caller_resume_pc as u32);
                    fiber.resume_stack.clear();
                } else {
                    if let Some(frame) = fiber.current_frame_mut() {
                        frame.pc = caller_resume_pc;
                    }
                }
                
                // Zero non-arg local slots
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
                
                Some(OSR_RESULT_FRAME_CHANGED)
            } else {
                // Regular call: read args from caller's frame, push callee frame
                let resume_pc = ctx.call_resume_pc();
                
                // Materialize intermediate frames, then use last frame as caller
                materialize_jit_frames(fiber, resume_pc);
                fiber.resume_stack.clear();
                
                // After materialize, last frame is the immediate caller
                let caller_frame = fiber.frames.last().unwrap();
                let actual_caller_bp = caller_frame.bp;
                let caller_func = &module.functions[caller_frame.func_id as usize];
                fiber.sp = actual_caller_bp + caller_func.local_slots as usize;
                
                let callee_bp = fiber.sp;
                let callee_local_slots = callee_func_def.local_slots as usize;
                let new_sp = callee_bp + callee_local_slots;
                
                fiber.ensure_capacity(new_sp);
                
                let stack = fiber.stack_ptr();
                unsafe { core::ptr::write_bytes(stack.add(callee_bp), 0, callee_local_slots) };
                
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
                
                Some(OSR_RESULT_FRAME_CHANGED)
            }
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
        JitResult::WaitQueue => {
            let resume_pc = ctx.call_resume_pc();
            
            // Update frame for resume (re-execute the blocking instruction)
            if let Some(frame) = fiber.current_frame_mut() {
                frame.pc = resume_pc as usize;
            }
            
            Some(OSR_RESULT_WAITQUEUE)
        }
        JitResult::Replay => {
            let resume_pc = ctx.call_resume_pc();
            
            // Exit loop OSR: set frame.pc to CallExtern instruction, VM will replay it
            if let Some(frame) = fiber.current_frame_mut() {
                frame.pc = resume_pc as usize;
            }
            
            Some(OSR_RESULT_FRAME_CHANGED)
        }
    }
}

// =============================================================================
// Loop OSR entry points (called from VM main loop)
// =============================================================================

/// Try loop OSR at backedge. Returns result PC or None to continue VM.
pub(crate) fn try_loop_osr(
    vm: &mut Vm,
    fiber_id: crate::scheduler::FiberId,
    func_id: u32,
    loop_pc: usize,
    bp: usize,
) -> Option<usize> {
    let loop_func = get_or_compile_loop(vm, func_id, loop_pc)?;
    dispatch_loop_osr(vm, fiber_id, loop_func, bp)
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
