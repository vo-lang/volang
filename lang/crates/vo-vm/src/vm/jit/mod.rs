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
//! Top-level JIT entries use `fiber.stack[jit_bp..]` as their ABI buffer. Nested
//! direct JIT calls may pass arguments through native stack slots on the OK path,
//! then spill/materialize into `fiber.stack` before any side-exit.
//!
//! ## Shadow Frame Design
//!
//! JIT-to-JIT calls keep `fiber.frames` shallow on the OK path. A side-exit lazily
//! records shadow frames in `resume_stack` just before returning to the VM:
//! - `jit_push_frame`: reserve a callee window in `fiber.stack` without a real frame
//! - `jit_pop_frame`: restore caller bp/sp after an OK prepared call
//! - `jit_push_resume_point`: record a side-exiting callee frame
//! - On Call/WaitIo/WaitQueue: `materialize_jit_frames` converts shadow frames to real CallFrames
//!
//! This avoids redundant frame management during pure JIT execution.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::bytecode::Module;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContext, JitResult, JitRuntimeTrapKind};
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::{CallFrame, Fiber, FiberCapacityError};
use crate::vm::jit_mgr::JitFallbackReason;
use crate::vm::{helpers, ExecResult, RuntimeTrapKind, Vm};

pub mod callbacks;
mod context;
mod frame;

pub use context::{build_jit_context, JitContextWrapper};

struct JitPanicInfo {
    trap_kind: Option<RuntimeTrapKind>,
    msg: InterfaceSlot,
}

enum SetupJitPanicError {
    Capacity(FiberCapacityError),
    MissingPayload,
    MissingLocation(&'static str),
}

fn jit_error_message(action: &str, func_name: &str, err: &vo_jit::JitError) -> String {
    format!("JIT {action} failed for {func_name}: {err}")
}

fn jit_context_error_message(ctx: &JitContextWrapper, module: &Module) -> String {
    let extern_id = ctx.ctx.runtime_trap_arg0 as u32;
    let not_registered_id = ctx.ctx.runtime_trap_arg1 as u32;
    if let Some(extern_def) = module.externs.get(extern_id as usize) {
        format!(
            "JIT extern call failed: extern function '{}' (id={}) not registered",
            extern_def.name, not_registered_id
        )
    } else {
        format!(
            "JIT execution failed: extern id {} not registered",
            not_registered_id
        )
    }
}

fn runtime_trap_from_jit(kind: JitRuntimeTrapKind) -> Option<RuntimeTrapKind> {
    match kind {
        JitRuntimeTrapKind::None => None,
        JitRuntimeTrapKind::NilPointerDereference => Some(RuntimeTrapKind::NilPointerDereference),
        JitRuntimeTrapKind::NilMapWrite => Some(RuntimeTrapKind::NilMapWrite),
        JitRuntimeTrapKind::UnhashableType => Some(RuntimeTrapKind::UnhashableType),
        JitRuntimeTrapKind::UncomparableType => Some(RuntimeTrapKind::UncomparableType),
        JitRuntimeTrapKind::NegativeShift => Some(RuntimeTrapKind::NegativeShift),
        JitRuntimeTrapKind::NilFuncCall => Some(RuntimeTrapKind::NilFuncCall),
        JitRuntimeTrapKind::TypeAssertionFailed => Some(RuntimeTrapKind::TypeAssertionFailed),
        JitRuntimeTrapKind::DivisionByZero => Some(RuntimeTrapKind::DivisionByZero),
        JitRuntimeTrapKind::IndexOutOfBounds => Some(RuntimeTrapKind::IndexOutOfBounds),
        JitRuntimeTrapKind::SliceBoundsOutOfRange => Some(RuntimeTrapKind::SliceBoundsOutOfRange),
        JitRuntimeTrapKind::MakeSlice => Some(RuntimeTrapKind::MakeSlice),
        JitRuntimeTrapKind::MakeChan => Some(RuntimeTrapKind::MakeChan),
        JitRuntimeTrapKind::MakePort => Some(RuntimeTrapKind::MakePort),
        JitRuntimeTrapKind::SendOnClosedChannel => Some(RuntimeTrapKind::SendOnClosedChannel),
        JitRuntimeTrapKind::SendOnNilChannel => Some(RuntimeTrapKind::SendOnNilChannel),
        JitRuntimeTrapKind::RecvOnNilChannel => Some(RuntimeTrapKind::RecvOnNilChannel),
        JitRuntimeTrapKind::CloseNilChannel => Some(RuntimeTrapKind::CloseNilChannel),
        JitRuntimeTrapKind::CloseClosedChannel => Some(RuntimeTrapKind::CloseClosedChannel),
        JitRuntimeTrapKind::StackOverflow => Some(RuntimeTrapKind::StackOverflow),
    }
}

fn jit_runtime_trap_message(kind: RuntimeTrapKind, arg0: u64, arg1: u64) -> String {
    match kind {
        RuntimeTrapKind::IndexOutOfBounds => {
            format!(
                "runtime error: index out of range [{}] with length {}",
                arg0 as i64, arg1 as i64
            )
        }
        RuntimeTrapKind::SliceBoundsOutOfRange => {
            format!(
                "runtime error: slice bounds out of range [{}:{}]",
                arg0 as i64, arg1 as i64
            )
        }
        RuntimeTrapKind::MakeSlice => helpers::makeslice_error_message(arg0 as i32).to_string(),
        RuntimeTrapKind::MakeChan | RuntimeTrapKind::MakePort => {
            helpers::make_queue_error_message(kind).to_string()
        }
        _ => helpers::runtime_trap_message(kind).to_string(),
    }
}

fn interface_string(gc: &mut vo_runtime::gc::Gc, msg: String) -> InterfaceSlot {
    let msg_str = vo_runtime::objects::string::new_from_string(gc, msg);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    InterfaceSlot::new(slot0, msg_str as u64)
}

fn stack_overflow_exec_result(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    err: FiberCapacityError,
) -> ExecResult {
    let stack = fiber.stack_ptr();
    helpers::runtime_panic(
        &mut vm.state.gc,
        fiber,
        stack,
        module,
        RuntimeTrapKind::StackOverflow,
        err.message(),
    )
}

fn set_stack_overflow_panic(vm: &mut Vm, fiber: &mut Fiber, err: FiberCapacityError) {
    let msg = err.message();
    fiber.capture_panic_source_loc();
    callbacks::helpers::set_jit_trap(
        &mut vm.state.gc,
        fiber,
        RuntimeTrapKind::StackOverflow,
        &msg,
    );
}

/// Shared JIT panic setup: materialize frames, capture source location, resolve panic message.
///
/// For user panics the message comes from ctx (set by JIT extern callback); for runtime errors
/// or VM-fallback panics it may already be in fiber.panic_state — take() handles both, with a
/// default message as fallback.
///
/// Source location uses `runtime_trap_pc` for typed traps and `user_panic_pc`
/// for explicit or extern panics. `call_resume_pc` is reserved for
/// WaitIo/Replay/call materialization and is not a panic location fallback.
///
/// Returns the resolved panic message and optional runtime-trap kind; caller
/// must restore it on the fiber before invoking panic unwinding.
fn setup_jit_panic(
    ctx: &JitContextWrapper,
    fiber: &mut Fiber,
    gc: &mut vo_runtime::gc::Gc,
    module: &Module,
) -> Result<JitPanicInfo, SetupJitPanicError> {
    if ctx.is_user_panic() {
        fiber.set_recoverable_panic(ctx.panic_msg());
    } else if let Some(jit_kind) = JitRuntimeTrapKind::from_u8(ctx.ctx.runtime_trap_kind) {
        if let Some(kind) = runtime_trap_from_jit(jit_kind) {
            let msg = jit_runtime_trap_message(
                kind,
                ctx.ctx.runtime_trap_arg0,
                ctx.ctx.runtime_trap_arg1,
            );
            fiber.set_recoverable_trap(kind, interface_string(gc, msg));
        }
    }
    let (trap_kind, panic_msg) = fiber
        .take_recoverable_panic_with_kind()
        .ok_or(SetupJitPanicError::MissingPayload)?;

    let trap_pc = (ctx.ctx.runtime_trap_pc != u32::MAX).then_some(ctx.ctx.runtime_trap_pc);
    let user_panic_pc = (ctx.ctx.user_panic_pc != u32::MAX).then_some(ctx.ctx.user_panic_pc);

    materialize_jit_frames(fiber, module, 0).map_err(SetupJitPanicError::Capacity)?;

    if fiber.panic_source_loc.is_none() {
        let (pc, missing) = if trap_kind.is_some() {
            (trap_pc, "runtime_trap_pc")
        } else {
            (user_panic_pc, "user_panic_pc")
        };
        let pc = pc.ok_or(SetupJitPanicError::MissingLocation(missing))?;
        fiber.panic_source_loc = fiber.current_frame().map(|f| (f.func_id, pc));
    }

    Ok(JitPanicInfo {
        trap_kind,
        msg: panic_msg,
    })
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
    let arg_start = inst.b as usize;
    let caller_frame = fiber
        .frames
        .last()
        .copied()
        .expect("dispatch_jit_call: missing caller frame");
    let caller_func = &module.functions[caller_frame.func_id as usize];
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(arg_start as u16);

    let func_def = &module.functions[func_id as usize];
    let arg_slots = func_def.param_slots as usize;
    let local_slots = func_def.local_slots as usize;
    let gc_scan_slots = func_def.gc_scan_slots as usize;
    let ret_slots = func_def.ret_slots as usize;

    let jit_bp = match fiber.try_push_borrowed_call_frame(
        func_id,
        arg_start as u16,
        (arg_start + arg_slots) as u16,
        ret_slots as u16,
        caller_scan_slots,
        local_slots as u16,
        func_def.gc_scan_slots,
    ) {
        Ok(bp) => bp,
        Err(err) => return stack_overflow_exec_result(vm, fiber, module, err),
    };
    fiber.zero_slots_tail_at(jit_bp, gc_scan_slots, arg_slots);

    invoke_jit_and_handle(vm, fiber, module, jit_func, jit_bp, ret_slots)
}

/// Execute an already-materialized frame through its compiled JIT entry.
///
/// This is used after a JIT side exit materializes a callee frame and returns
/// to the VM scheduler. Re-entering here from the interpreter loop preserves
/// JIT execution without recursively growing the host stack.
pub fn dispatch_jit_frame(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    jit_func: vo_jit::JitFunc,
) -> ExecResult {
    let frame = fiber
        .frames
        .last()
        .copied()
        .expect("dispatch_jit_frame: missing current frame");
    let func_def = &module.functions[frame.func_id as usize];
    invoke_jit_and_handle(
        vm,
        fiber,
        module,
        jit_func,
        frame.bp,
        func_def.ret_slots as usize,
    )
}

/// Shared JIT invocation: build context, call function, handle result.
fn invoke_jit_and_handle(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    jit_func: vo_jit::JitFunc,
    jit_bp: usize,
    ret_slots: usize,
) -> ExecResult {
    if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
        jit_mgr.record_function_entry();
    }
    let mut ctx = build_jit_context(vm, fiber, module);
    ctx.ctx.stack_ptr = fiber.stack_ptr();
    ctx.ctx.stack_cap = fiber.stack.len() as u32;
    ctx.ctx.jit_bp = jit_bp as u32;

    // Stack buffer for return values — avoids heap allocation for the common case (≤16 slots).
    // Most functions return 0-4 slots; 16 covers all practical cases.
    const RET_STACK_MAX: usize = 16;
    let mut ret_stack_buf = [0u64; RET_STACK_MAX];
    let mut ret_heap_buf: Vec<u64>;
    let ret: &mut [u64] = if ret_slots <= RET_STACK_MAX {
        &mut ret_stack_buf[..ret_slots.max(1)]
    } else {
        ret_heap_buf = vec![0u64; ret_slots];
        &mut ret_heap_buf
    };
    let args_ptr = unsafe { fiber.stack_ptr().add(jit_bp) };
    let result = jit_func(ctx.as_ptr(), args_ptr, ret.as_mut_ptr());

    handle_jit_result(vm, fiber, module, result, ctx, ret_slots, ret)
}

fn handle_jit_result(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    result: JitResult,
    ctx: JitContextWrapper,
    ret_slots: usize,
    ret: &[u64],
) -> ExecResult {
    match result {
        JitResult::Ok => {
            fiber.resume_stack.clear();

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

            let ret_start = ctx.ret_start() as usize;
            crate::exec::handle_jit_ok_return(
                &mut vm.state.gc,
                fiber,
                func,
                module,
                &ret[..ret_slots],
                heap_returns,
                ret_gcref_start,
                ret_start,
                include_errdefers,
            )
        }
        JitResult::Panic => {
            let panic_info = match setup_jit_panic(&ctx, fiber, &mut vm.state.gc, module) {
                Ok(info) => info,
                Err(SetupJitPanicError::Capacity(err)) => {
                    return stack_overflow_exec_result(vm, fiber, module, err);
                }
                Err(SetupJitPanicError::MissingPayload) => {
                    return ExecResult::JitError(
                        "JIT returned Panic without user panic or typed runtime trap payload"
                            .to_string(),
                    );
                }
                Err(SetupJitPanicError::MissingLocation(field)) => {
                    return ExecResult::JitError(format!(
                        "JIT returned Panic without required {field} location"
                    ));
                }
            };
            if let Some(kind) = panic_info.trap_kind {
                fiber.set_recoverable_trap(kind, panic_info.msg);
            } else {
                fiber.set_recoverable_panic(panic_info.msg);
            }
            let stack_ptr = fiber.stack_ptr();
            helpers::panic_unwind(&mut vm.state.gc, fiber, stack_ptr, module)
        }
        JitResult::Call => {
            // Check for special call_kind values that don't need frame setup
            let call_kind = ctx.ctx.call_kind;
            match call_kind {
                JitContext::CALL_KIND_YIELD => {
                    if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                        jit_mgr.record_fallback(JitFallbackReason::Yield);
                    }
                    return ExecResult::TimesliceExpired;
                }
                JitContext::CALL_KIND_BLOCK => {
                    if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                        jit_mgr.record_fallback(JitFallbackReason::QueueBlock);
                    }
                    return ExecResult::Block(crate::fiber::BlockReason::Queue);
                }
                _ => {} // Regular or Prepared call - continue to frame setup
            }

            let callee_func_id = ctx.call_func_id();
            let call_arg_start = ctx.call_arg_start() as usize;
            let callee_ret_slots = module.functions[callee_func_id as usize].ret_slots as usize;

            if call_kind == JitContext::CALL_KIND_PREPARED {
                if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                    jit_mgr.record_fallback(JitFallbackReason::PreparedDynamicCall);
                    let callee_func_def = &module.functions[callee_func_id as usize];
                    if let Err(err) = jit_mgr.resolve_call(callee_func_id, callee_func_def, module)
                    {
                        return ExecResult::JitError(jit_error_message(
                            "prepared dynamic callee compilation",
                            &callee_func_def.name,
                            &err,
                        ));
                    }
                }
                let callee_bp = ctx.call_resume_pc() as usize;
                let caller_resume_pc = ctx.call_arg_start() as u32;
                let call_ret_reg = ctx.call_ret_reg();
                if let Err(err) = setup_prepared_call(
                    fiber,
                    module,
                    callee_func_id,
                    callee_ret_slots as u16,
                    call_ret_reg,
                    callee_bp,
                    caller_resume_pc,
                ) {
                    return stack_overflow_exec_result(vm, fiber, module, err);
                }
                return ExecResult::FrameChanged;
            }

            // Regular call: JIT requests VM to execute a non-JIT function.
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                jit_mgr.record_fallback(JitFallbackReason::RegularCall);
            }
            let resume_pc = ctx.call_resume_pc();
            let call_ret_reg = ctx.call_ret_reg();
            if let Err(err) = setup_regular_call(
                fiber,
                module,
                callee_func_id,
                callee_ret_slots as u16,
                call_ret_reg,
                call_arg_start,
                resume_pc,
            ) {
                return stack_overflow_exec_result(vm, fiber, module, err);
            }

            // Return to the VM scheduler after materializing the callee frame.
            // Re-entering a compiled callee recursively here can overflow the
            // host stack before the fiber frame limit turns recursion into a
            // recoverable Vo stack panic. Resolve now so strict JIT compile or
            // metadata failures remain fail-fast; the compiled callee runs from
            // the next VM frame-entry dispatch.
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                let callee_func_def = &module.functions[callee_func_id as usize];
                match jit_mgr.resolve_call(callee_func_id, callee_func_def, module) {
                    Ok(Some(_)) | Ok(None) => {}
                    Err(err) => {
                        return ExecResult::JitError(jit_error_message(
                            "callee compilation",
                            &callee_func_def.name,
                            &err,
                        ));
                    }
                }
            }
            ExecResult::FrameChanged
        }
        JitResult::WaitIo => {
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                jit_mgr.record_fallback(JitFallbackReason::WaitIo);
            }
            let resume_pc = ctx.call_resume_pc();
            if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                return stack_overflow_exec_result(vm, fiber, module, err);
            }

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
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                jit_mgr.record_fallback(JitFallbackReason::WaitQueue);
            }
            let resume_pc = ctx.call_resume_pc();
            if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                return stack_overflow_exec_result(vm, fiber, module, err);
            }
            ExecResult::Block(crate::fiber::BlockReason::Queue)
        }
        JitResult::Replay => {
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                jit_mgr.record_fallback(JitFallbackReason::Replay);
            }
            // Extern returned CallClosure — exit JIT and let VM handle it.
            // resume_pc points at the CallExtern instruction itself, so VM will
            // re-execute it and go through the suspend/replay path.
            let resume_pc = ctx.call_resume_pc();
            if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                return stack_overflow_exec_result(vm, fiber, module, err);
            }
            ExecResult::FrameChanged
        }
        JitResult::JitError => ExecResult::JitError(jit_context_error_message(&ctx, module)),
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
) -> Result<(), FiberCapacityError> {
    let callee_func_def = &module.functions[callee_func_id as usize];
    let param_slots = callee_func_def.param_slots as usize;
    let local_slots = callee_func_def.local_slots as usize;
    let gc_scan_slots = callee_func_def.gc_scan_slots as usize;

    // Materialize any intermediate JIT frames from non-OK propagation
    if !fiber.resume_stack.is_empty() {
        materialize_jit_frames(fiber, module, caller_resume_pc)?;
        // resume_stack already cleared by materialize_jit_frames
    } else {
        if let Some(frame) = fiber.frames.last_mut() {
            frame.pc = caller_resume_pc as usize;
        }
    }

    fiber.try_reserve_slots_at(callee_bp, local_slots)?;
    fiber.zero_slots_tail_at(callee_bp, gc_scan_slots, param_slots);

    fiber.try_push_call_frame(
        callee_func_id,
        callee_bp,
        call_ret_reg,
        callee_ret_slots,
        callee_func_def.gc_scan_slots,
    )?;
    Ok(())
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
) -> Result<usize, FiberCapacityError> {
    materialize_jit_frames(fiber, module, resume_pc)?;

    // After materialize_jit_frames, the last frame is the immediate caller.
    let caller_frame = fiber.frames.last().unwrap();
    let actual_caller_bp = caller_frame.bp;
    let caller_func = &module.functions[caller_frame.func_id as usize];

    // Recompute fiber.sp from the materialized caller frame
    fiber.sp = actual_caller_bp + caller_func.local_slots as usize;

    let callee_func_def = &module.functions[callee_func_id as usize];
    let callee_local_slots = callee_func_def.local_slots as usize;
    let callee_gc_scan_slots = callee_func_def.gc_scan_slots as usize;
    let arg_slots = callee_func_def.param_slots as usize;
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(call_arg_start as u16);

    let callee_bp = fiber.try_push_borrowed_call_frame(
        callee_func_id,
        call_arg_start as u16,
        call_ret_reg,
        callee_ret_slots,
        caller_scan_slots,
        callee_local_slots as u16,
        callee_func_def.gc_scan_slots,
    )?;
    fiber.zero_slots_tail_at(callee_bp, callee_gc_scan_slots, arg_slots);

    Ok(callee_bp)
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
fn materialize_jit_frames(
    fiber: &mut Fiber,
    module: &Module,
    resume_pc: u32,
) -> Result<(), FiberCapacityError> {
    let len = fiber.resume_stack.len();

    if len == 0 {
        // No shadow frames, just update entry frame's pc for resume
        if let Some(frame) = fiber.frames.last_mut() {
            frame.pc = resume_pc as usize;
        }
        return Ok(());
    }

    fiber.try_reserve_call_frames(len)?;
    let innermost = &fiber.resume_stack[0];
    let innermost_local_slots = module.functions[innermost.func_id as usize].local_slots as usize;
    let innermost_sp =
        innermost
            .bp
            .checked_add(innermost_local_slots)
            .ok_or(FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: crate::fiber::MAX_STACK_CAPACITY,
            })?;
    fiber.try_ensure_capacity(innermost_sp)?;

    // Step 1: Update entry frame's pc (the frame that was in fiber.frames before JIT ran)
    // The last element in resume_stack is the outermost caller's info, containing
    // the resume_pc for the entry frame.
    if let Some(entry_frame) = fiber.frames.last_mut() {
        entry_frame.pc = fiber.resume_stack[len - 1].resume_pc as usize;
    }

    // Step 2: Convert resume_stack entries to frames (reverse order: outermost first)
    for i in (0..len).rev() {
        let rp = fiber.resume_stack[i];
        let func_id = rp.func_id;
        let bp = rp.bp;
        let func_def = &module.functions[func_id as usize];

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
        let existing = fiber
            .frames
            .iter_mut()
            .find(|f| f.func_id == func_id && f.bp == bp);

        if let Some(frame) = existing {
            // OSR case: frame already exists, just update pc
            frame.pc = pc;
            frame.scan_slots = func_def.gc_scan_slots;
        } else {
            // Normal case: create new frame
            let mut frame = CallFrame::new(
                func_id,
                bp,
                bp,
                rp.ret_reg,
                rp.ret_slots,
                func_def.gc_scan_slots,
                None,
                0,
                0,
            );
            frame.pc = pc;
            fiber.frames.push(frame);
        }
    }

    // Step 3: Fix fiber.sp — must cover the innermost frame.
    // The non-OK propagation chain's push_frame calls overwrote fiber.sp with
    // progressively lower values. Restore it to the innermost callee's sp.
    fiber.sp = innermost_sp;

    // Clear resume_stack since VM now owns the frames
    fiber.resume_stack.clear();
    #[cfg(debug_assertions)]
    if let Err(err) = materialized_jit_frame_invariants(fiber, module) {
        panic!("JIT frame materialization invariant failed: {err}");
    }
    Ok(())
}

#[cfg(any(debug_assertions, test))]
fn materialized_jit_frame_invariants(fiber: &Fiber, module: &Module) -> Result<(), &'static str> {
    #[cfg(feature = "jit")]
    if !fiber.resume_stack.is_empty() {
        return Err("resume_stack must be empty after JIT frame materialization");
    }

    if fiber.sp > fiber.stack.len() {
        return Err("fiber.sp is outside allocated stack");
    }

    for (idx, frame) in fiber.frames.iter().enumerate() {
        let func = module
            .functions
            .get(frame.func_id as usize)
            .ok_or("materialized frame func_id is out of module range")?;
        if frame.bp > fiber.sp {
            return Err("materialized frame bp is outside fiber.sp");
        }
        if frame.scan_slots > func.gc_scan_slots {
            return Err("materialized frame scan slots exceed function metadata");
        }
        if frame.scan_slots > func.local_slots {
            return Err("materialized frame scan slots exceed function locals");
        }
        let scan_end = frame
            .bp
            .checked_add(frame.scan_slots as usize)
            .ok_or("materialized frame scan extent overflowed")?;
        if scan_end > fiber.sp {
            return Err("materialized frame scan extent is outside fiber.sp");
        }

        if let Some(restore) = frame.caller_scan_slots_restore {
            if idx == 0 {
                return Err("borrowed frame scan restore has no parent frame");
            }
            let parent = &fiber.frames[idx - 1];
            let parent_func = module
                .functions
                .get(parent.func_id as usize)
                .ok_or("borrowed frame parent func_id is out of module range")?;
            if restore > parent_func.gc_scan_slots {
                return Err("borrowed frame scan restore exceeds parent metadata");
            }
            if restore > parent_func.local_slots {
                return Err("borrowed frame scan restore exceeds parent locals");
            }
            if frame.caller_zero_start > frame.caller_zero_end {
                return Err("borrowed frame caller zero range is inverted");
            }
            if frame.caller_zero_end > restore {
                return Err("borrowed frame caller zero range exceeds restore scan slots");
            }
        }
    }

    if let Some(frame) = fiber.frames.last() {
        let func = module
            .functions
            .get(frame.func_id as usize)
            .ok_or("innermost materialized frame func_id is out of module range")?;
        let frame_end = frame
            .bp
            .checked_add(func.local_slots as usize)
            .ok_or("innermost materialized frame stack extent overflowed")?;
        if frame_end > fiber.sp {
            return Err("innermost materialized frame is outside fiber.sp");
        }
    }

    Ok(())
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
    use vo_runtime::ffi::{
        ExternFiberInputs, ExternInvoke, ExternRegistry, ExternResult, ExternWorld,
    };
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
    // SAFETY: output pointer was set from Arc<dyn OutputSink> in build_jit_context;
    // the Arc keeps it alive for the VM's lifetime.
    let output: &dyn vo_runtime::output::OutputSink = unsafe { &*ctx_ref.output };

    // Get resume_io_token from fiber (for replay-at-PC semantics)
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let resume_io_token = fiber.resume_io_token.take();

    // Take closure replay state from fiber (populated by VM suspend/replay on re-entry)
    let (closure_replay_results, closure_replay_panic_message) =
        fiber.closure_replay.take_for_extern();

    let invoke = ExternInvoke {
        extern_id,
        bp: 0, // start of buffer
        arg_start: 0,
        arg_slots: arg_count as u16,
        ret_start: 0, // returns overwrite args in same buffer
        ret_slots: ret_slots as u16,
    };
    let host_output = unsafe { &mut *ctx_ref.host_output };
    let world = ExternWorld {
        gc,
        module,
        itab_cache,
        vm_opaque: ctx_ref.vm,
        program_args,
        output,
        sentinel_errors,
        host_output,
        io,
    };
    let fiber_inputs = ExternFiberInputs {
        fiber_opaque: ctx_ref.fiber,
        resume_io_token,
        resume_host_event_token: fiber.resume_host_event_token.take(),
        resume_host_event_data: fiber.resume_host_event_data.take(),
        replay_results: closure_replay_results,
        replay_panic_message: closure_replay_panic_message,
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
            let slot0 =
                vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
            unsafe {
                *ctx_ref.panic_flag = true;
                *ctx_ref.is_user_panic = true;
                ctx_ref.runtime_trap_kind = JitRuntimeTrapKind::None as u8;
                ctx_ref.runtime_trap_pc = u32::MAX;
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
        ExternResult::NotRegistered(id) => {
            ctx_ref.runtime_trap_arg0 = extern_id as u64;
            ctx_ref.runtime_trap_arg1 = id as u64;
            JitResult::JitError
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
    /// Loop blocks on channel.
    WaitQueue,
    /// Panic occurred during loop execution.
    Panic,
    /// Fatal JIT infrastructure error. This is not recoverable by user code.
    JitError(String),
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

        if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
            jit_mgr.record_loop_entry();
        }

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
            let panic_info = match setup_jit_panic(&ctx, fiber, &mut vm.state.gc, module) {
                Ok(info) => info,
                Err(SetupJitPanicError::Capacity(err)) => {
                    set_stack_overflow_panic(vm, fiber, err);
                    return OsrResult::Panic;
                }
                Err(SetupJitPanicError::MissingPayload) => {
                    return OsrResult::JitError(
                        "JIT returned Panic without user panic or typed runtime trap payload"
                            .to_string(),
                    );
                }
                Err(SetupJitPanicError::MissingLocation(field)) => {
                    return OsrResult::JitError(format!(
                        "JIT returned Panic without required {field} location"
                    ));
                }
            };
            if let Some(kind) = panic_info.trap_kind {
                fiber.set_recoverable_trap(kind, panic_info.msg);
            } else {
                fiber.set_recoverable_panic(panic_info.msg);
            }
            OsrResult::Panic
        }
        JitResult::Call => {
            let call_kind = ctx.ctx.call_kind;
            let call_ret_reg = ctx.call_ret_reg();

            // Check for special call_kind values (Yield/Block)
            match call_kind {
                JitContext::CALL_KIND_YIELD | JitContext::CALL_KIND_BLOCK => {
                    if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                        jit_mgr.record_fallback(if call_kind == JitContext::CALL_KIND_YIELD {
                            JitFallbackReason::Yield
                        } else {
                            JitFallbackReason::QueueBlock
                        });
                    }
                    let resume_pc = ctx.call_resume_pc();
                    if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                        set_stack_overflow_panic(vm, fiber, err);
                        return OsrResult::Panic;
                    }
                    return OsrResult::FrameChanged;
                }
                _ => {}
            }

            let callee_func_id = ctx.call_func_id();
            let call_arg_start = ctx.call_arg_start() as usize;
            let callee_ret_slots = module.functions[callee_func_id as usize].ret_slots;

            if call_kind == JitContext::CALL_KIND_PREPARED {
                if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                    jit_mgr.record_fallback(JitFallbackReason::PreparedDynamicCall);
                    let callee_func_def = &module.functions[callee_func_id as usize];
                    if let Err(err) = jit_mgr.resolve_call(callee_func_id, callee_func_def, module)
                    {
                        return OsrResult::JitError(jit_error_message(
                            "prepared dynamic callee compilation",
                            &callee_func_def.name,
                            &err,
                        ));
                    }
                }
                let callee_bp = ctx.call_resume_pc() as usize;
                let caller_resume_pc = ctx.call_arg_start() as u32;
                if let Err(err) = setup_prepared_call(
                    fiber,
                    module,
                    callee_func_id,
                    callee_ret_slots,
                    call_ret_reg,
                    callee_bp,
                    caller_resume_pc,
                ) {
                    set_stack_overflow_panic(vm, fiber, err);
                    return OsrResult::Panic;
                }
                OsrResult::FrameChanged
            } else {
                if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                    jit_mgr.record_fallback(JitFallbackReason::RegularCall);
                }
                let resume_pc = ctx.call_resume_pc();
                if let Err(err) = setup_regular_call(
                    fiber,
                    module,
                    callee_func_id,
                    callee_ret_slots,
                    call_ret_reg,
                    call_arg_start,
                    resume_pc,
                ) {
                    set_stack_overflow_panic(vm, fiber, err);
                    return OsrResult::Panic;
                }
                OsrResult::FrameChanged
            }
        }
        #[cfg(feature = "std")]
        JitResult::WaitIo => {
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                jit_mgr.record_fallback(JitFallbackReason::WaitIo);
            }
            let token = ctx.wait_io_token();
            let resume_pc = ctx.call_resume_pc();

            // Materialize any intermediate JIT frames from nested calls.
            // Without this, resume_stack entries are lost and the fiber
            // resumes at the wrong PC (callee's PC in the loop function's frame).
            if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                set_stack_overflow_panic(vm, fiber, err);
                return OsrResult::Panic;
            }

            // Store token for scheduler
            fiber.resume_io_token = Some(token);

            OsrResult::WaitIo
        }
        #[cfg(not(feature = "std"))]
        JitResult::WaitIo => {
            panic!("Loop OSR returned WaitIo but std feature not enabled")
        }
        JitResult::WaitQueue => {
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                jit_mgr.record_fallback(JitFallbackReason::WaitQueue);
            }
            let resume_pc = ctx.call_resume_pc();
            if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                set_stack_overflow_panic(vm, fiber, err);
                return OsrResult::Panic;
            }
            OsrResult::WaitQueue
        }
        JitResult::Replay => {
            if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
                jit_mgr.record_fallback(JitFallbackReason::Replay);
            }
            let resume_pc = ctx.call_resume_pc();
            if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                set_stack_overflow_panic(vm, fiber, err);
                return OsrResult::Panic;
            }
            OsrResult::FrameChanged
        }
        JitResult::JitError => OsrResult::JitError(jit_context_error_message(&ctx, module)),
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
    let loop_func = match get_or_compile_loop(vm, func_id, loop_pc) {
        Ok(Some(loop_func)) => loop_func,
        Ok(None) => return None,
        Err(err) => {
            let func_name = vm
                .module
                .as_ref()
                .and_then(|module| module.functions.get(func_id as usize))
                .map(|func| func.name.as_str())
                .unwrap_or("<unknown>");
            return Some(OsrResult::JitError(format!(
                "JIT OSR compilation failed for {func_name} at loop pc {loop_pc}: {err}"
            )));
        }
    };
    let module = vm.module.as_ref().unwrap();
    let local_slots = module.functions[func_id as usize].local_slots as usize;
    Some(dispatch_loop_osr(vm, fiber_id, loop_func, bp, local_slots))
}

/// Get compiled loop or compile if hot. Returns None if not ready.
fn get_or_compile_loop(
    vm: &mut Vm,
    func_id: u32,
    loop_pc: usize,
) -> Result<Option<vo_jit::LoopFunc>, vo_jit::JitError> {
    use vo_runtime::instruction::Opcode;

    let module = vm
        .module
        .as_ref()
        .ok_or_else(|| vo_jit::JitError::Internal("OSR requested without loaded module".into()))?;
    let func_def = &module.functions[func_id as usize];
    let Some(jit_mgr) = vm.jit_mgr.as_mut() else {
        return Ok(None);
    };

    // Already compiled?
    if let Some(lf) = unsafe { jit_mgr.get_loop_func(func_id, loop_pc) } {
        return Ok(Some(lf));
    }

    // Already failed?
    if jit_mgr.is_loop_failed(func_id, loop_pc) {
        return Err(vo_jit::JitError::Internal(format!(
            "loop at pc {loop_pc} previously failed JIT compilation"
        )));
    }

    // Not hot yet?
    if !jit_mgr.record_backedge(func_id, loop_pc) {
        jit_mgr.record_fallback(JitFallbackReason::LoopNotHot);
        return Ok(None);
    }

    // Hot - try to compile
    let loop_info = match jit_mgr.find_loop(func_id, func_def, module, loop_pc) {
        Ok(Some(info)) => info,
        Ok(None) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc);
            return Err(vo_jit::JitError::Internal(format!(
                "hot back-edge at pc {loop_pc} has no LoopInfo"
            )));
        }
        Err(err) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc);
            return Err(err);
        }
    };

    // Pre-compile Call targets so JIT-to-JIT calls can succeed
    let loop_end = loop_info.end_pc + 1;
    for pc in loop_info.begin_pc..loop_end {
        let inst = &func_def.code[pc];
        if inst.opcode() == Opcode::Call {
            let target_func_id = inst.static_call_func_id();
            if !jit_mgr.is_compiled(target_func_id) && !jit_mgr.is_unsupported(target_func_id) {
                let target_func = &module.functions[target_func_id as usize];
                jit_mgr.resolve_call(target_func_id, target_func, module)?;
            }
        }
    }

    match jit_mgr.compile_loop(func_id, func_def, module, &loop_info) {
        Ok(_) => {
            let loop_func =
                unsafe { jit_mgr.get_loop_func(func_id, loop_pc) }.ok_or_else(|| {
                    vo_jit::JitError::Internal(format!(
                        "compiled loop at pc {loop_pc} but no function pointer was registered"
                    ))
                })?;
            Ok(Some(loop_func))
        }
        Err(err) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc);
            Err(err)
        }
    }
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::*;
    use crate::fiber::ResumePoint;
    use crate::scheduler::FiberId;
    use crate::vm::JitConfig;
    use vo_runtime::bytecode::{FunctionDef, Module};

    fn function(local_slots: u16, gc_scan_slots: u16) -> FunctionDef {
        FunctionDef {
            name: "f".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots,
            gc_scan_slots,
            ret_slots: 0,
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: vec![0],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    extern "C" fn user_panic_without_location(
        ctx: *mut JitContext,
        _locals: *mut u64,
    ) -> JitResult {
        unsafe {
            *(*ctx).panic_flag = true;
            *(*ctx).is_user_panic = true;
            *(*ctx).panic_msg = InterfaceSlot::default();
        }
        JitResult::Panic
    }

    extern "C" fn runtime_trap_without_location(
        ctx: *mut JitContext,
        _locals: *mut u64,
    ) -> JitResult {
        unsafe {
            (*ctx).runtime_trap_kind = JitRuntimeTrapKind::DivisionByZero as u8;
        }
        JitResult::Panic
    }

    fn vm_with_jit_frame() -> (Vm, FiberId) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-panic-location-test".to_string());
        module.functions.push(function(1, 0));
        vm.load(module);

        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.get_fiber_mut(fid).push_frame(0, 1, 0, 0, 0);
        (vm, fid)
    }

    #[test]
    fn osr_user_panic_without_user_panic_pc_is_jit_error() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_loop_osr(&mut vm, fid, user_panic_without_location, 0, 1) {
            OsrResult::JitError(msg) => assert!(msg.contains("user_panic_pc")),
            _ => panic!("missing user_panic_pc must be a JitError"),
        }
    }

    #[test]
    fn osr_runtime_trap_without_runtime_trap_pc_is_jit_error() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_loop_osr(&mut vm, fid, runtime_trap_without_location, 0, 1) {
            OsrResult::JitError(msg) => assert!(msg.contains("runtime_trap_pc")),
            _ => panic!("missing runtime_trap_pc must be a JitError"),
        }
    }

    #[test]
    fn materialize_jit_frames_preserves_nested_frame_invariants() {
        let mut module = Module::new("jit-frame-test".to_string());
        module.functions.push(function(2, 0));
        module.functions.push(function(3, 1));
        module.functions.push(function(4, 2));

        let mut fiber = Fiber::new(1);
        let entry_bp = fiber.push_frame(0, 2, 0, 0, 0);
        let outer_bp = fiber.reserve_slots_at(entry_bp + 2, 3) - 3;
        let inner_bp = fiber.reserve_slots_at(outer_bp + 3, 4) - 4;

        fiber.resume_stack.push(ResumePoint {
            func_id: 2,
            resume_pc: 22,
            bp: inner_bp,
            caller_bp: outer_bp,
            ret_reg: 5,
            ret_slots: 1,
        });
        fiber.resume_stack.push(ResumePoint {
            func_id: 1,
            resume_pc: 11,
            bp: outer_bp,
            caller_bp: entry_bp,
            ret_reg: 3,
            ret_slots: 1,
        });

        materialize_jit_frames(&mut fiber, &module, 33).expect("materialize");

        assert!(fiber.resume_stack.is_empty());
        assert_eq!(fiber.sp, inner_bp + 4);
        assert_eq!(fiber.frames.len(), 3);
        assert_eq!(fiber.frames[0].pc, 11);
        assert_eq!(fiber.frames[1].func_id, 1);
        assert_eq!(fiber.frames[1].pc, 22);
        assert_eq!(fiber.frames[2].func_id, 2);
        assert_eq!(fiber.frames[2].pc, 33);
        assert!(materialized_jit_frame_invariants(&fiber, &module).is_ok());
    }

    #[test]
    fn materialized_invariants_allow_borrowed_parent_above_current_sp() {
        let mut module = Module::new("jit-borrowed-parent-frame-test".to_string());
        module.functions.push(function(10, 4));
        module.functions.push(function(2, 1));
        module.functions.push(function(3, 1));

        let mut fiber = Fiber::new(1);
        let parent_bp = fiber.push_frame(0, 10, 4, 0, 0);
        let entry_bp = fiber.push_borrowed_call_frame(1, 2, 0, 0, 2, 2, 1);
        let inner_bp = fiber.reserve_slots_at(entry_bp + 2, 3) - 3;

        assert_eq!(parent_bp, 0);
        assert_eq!(entry_bp, 2);
        assert_eq!(inner_bp, 4);
        assert_eq!(fiber.frames[0].scan_slots, 2);

        fiber.resume_stack.push(ResumePoint {
            func_id: 2,
            resume_pc: 22,
            bp: inner_bp,
            caller_bp: entry_bp,
            ret_reg: 0,
            ret_slots: 0,
        });

        materialize_jit_frames(&mut fiber, &module, 33).expect("materialize");

        assert_eq!(fiber.sp, 7);
        assert_eq!(fiber.frames.len(), 3);
        assert_eq!(
            fiber.frames[0].bp + module.functions[0].local_slots as usize,
            10
        );
        assert!(fiber.frames[0].bp + module.functions[0].local_slots as usize > fiber.sp);
        assert!(materialized_jit_frame_invariants(&fiber, &module).is_ok());
    }
}
