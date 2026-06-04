#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::bytecode::Module;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitResult;

use crate::fiber::Fiber;
use crate::vm::{ExecResult, Vm};

use super::bridge_result::{
    exec_result_from_bridge_transition, stack_overflow_exec_result, JitBridgeMode,
};
use super::context::{build_jit_context, JitContextWrapper};
use super::transition::handle_jit_non_ok_transition;

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
    let Some(caller_frame) = fiber.frames.last().copied() else {
        return ExecResult::JitError("JIT call requested without an active caller frame".into());
    };
    let Some(caller_func) = module.functions.get(caller_frame.func_id as usize) else {
        return ExecResult::JitError(format!(
            "JIT call requested from missing caller function id {}",
            caller_frame.func_id
        ));
    };
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(arg_start as u16);

    let Some(func_def) = module.functions.get(func_id as usize) else {
        return ExecResult::JitError(format!(
            "JIT call requested missing callee function id {func_id}"
        ));
    };
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
    let Some(frame) = fiber.frames.last().copied() else {
        return ExecResult::JitError("JIT frame entry requested without an active frame".into());
    };
    let Some(func_def) = module.functions.get(frame.func_id as usize) else {
        return ExecResult::JitError(format!(
            "JIT frame entry requested missing function id {}",
            frame.func_id
        ));
    };
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
    if let Some(jit_mgr) = vm.jit.manager_mut() {
        jit_mgr.record_function_entry();
    }
    let mut ctx = match build_jit_context(vm, fiber, module) {
        Ok(ctx) => ctx,
        Err(err) => return ExecResult::JitError(err),
    };
    ctx.ctx.stack_ptr = fiber.stack_ptr();
    ctx.ctx.stack_cap = fiber.stack.len() as u32;
    ctx.ctx.jit_bp = jit_bp as u32;

    // Stack buffer for return values - avoids heap allocation for the common case (<=16 slots).
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

            let Some(frame) = fiber.frames.last() else {
                return ExecResult::JitError("JIT returned Ok without an active frame".into());
            };
            let Some(func) = module.functions.get(frame.func_id as usize) else {
                return ExecResult::JitError(format!(
                    "JIT returned Ok for missing function id {}",
                    frame.func_id
                ));
            };

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
                    let Some(gcref_slot) = bp
                        .checked_add(ret_gcref_start)
                        .and_then(|slot| slot.checked_add(error_gcref_idx))
                    else {
                        return ExecResult::JitError(
                            "JIT heap return error slot calculation overflowed".into(),
                        );
                    };
                    let Some(&gcref_raw) = fiber.stack.get(gcref_slot) else {
                        return ExecResult::JitError(format!(
                            "JIT heap return error slot {gcref_slot} is outside fiber stack"
                        ));
                    };
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
                if idx >= ret_slots {
                    return ExecResult::JitError(format!(
                        "JIT stack return error slot {idx} is outside returned slot count {ret_slots}"
                    ));
                }
                (ret[idx] & 0xFF) != 0
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
        non_ok => {
            let transition = handle_jit_non_ok_transition(
                JitBridgeMode::FullFunction,
                vm,
                fiber,
                module,
                non_ok,
                &ctx,
            );
            exec_result_from_bridge_transition(vm, fiber, module, transition)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::context::build_jit_context;
    use super::super::test_support::function;
    use super::*;
    use crate::vm::JitConfig;

    #[test]
    fn heap_return_error_slot_outside_stack_is_jit_error() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-heap-return-bounds-test".to_string());
        let mut func = function(1, 0);
        func.heap_ret_gcref_count = 1;
        func.heap_ret_slots = vec![2];
        func.error_ret_slot = 0;
        module.functions.push(func);

        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.ret_is_heap = 1;
        ctx.ctx.ret_gcref_start = u16::MAX;

        match handle_jit_result(&mut vm, &mut fiber, &module, JitResult::Ok, ctx, 0, &[]) {
            ExecResult::JitError(msg) => {
                assert!(msg.contains("heap return error slot"), "{msg}")
            }
            other => panic!("out-of-range heap return slot must be JitError, got {other:?}"),
        }
    }

    #[test]
    fn stack_return_error_slot_outside_ret_buffer_is_jit_error() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-stack-return-error-slot-test".to_string());
        let mut func = function(1, 0);
        func.ret_slots = 1;
        func.error_ret_slot = 1;
        module.functions.push(func);

        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0, 0);
        let ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        match handle_jit_result(&mut vm, &mut fiber, &module, JitResult::Ok, ctx, 1, &[0]) {
            ExecResult::JitError(msg) => {
                assert!(msg.contains("JIT stack return error slot"), "{msg}")
            }
            other => panic!("out-of-range stack return error slot must be JitError, got {other:?}"),
        }
    }
}
