#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::bytecode::Module;
#[cfg(test)]
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitResult;

use crate::fiber::Fiber;
#[cfg(test)]
use crate::frame_call::{validate_call_frame_shape, validate_call_return_window};
use crate::vm::{ExecResult, Vm};

#[cfg(test)]
use super::bridge_result::stack_overflow_exec_result;
use super::bridge_result::{exec_result_from_bridge_transition, JitBridgeMode};
use super::context::{build_jit_context, JitContextWrapper};
use super::transition::handle_jit_non_ok_transition;

#[cfg(test)]
fn heap_error_return_gcref_index(func: &vo_runtime::bytecode::FunctionDef) -> Option<usize> {
    if func.error_ret_slot < 0 {
        return None;
    }
    let gcref_count = func.heap_ret_gcref_count as usize;
    if gcref_count == 0 {
        None
    } else {
        Some(gcref_count - 1)
    }
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
#[cfg(test)]
fn dispatch_jit_call(
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
    let ret_reg = match arg_start.checked_add(arg_slots) {
        Some(ret_reg) => match u16::try_from(ret_reg) {
            Ok(ret_reg) => ret_reg,
            Err(_) => {
                return ExecResult::JitError(format!(
                    "JIT call return offset {ret_reg} exceeds u16 for func_id={} name={}",
                    func_id, func_def.name
                ));
            }
        },
        None => {
            return ExecResult::JitError(format!(
                "JIT call return offset overflow: arg_start={arg_start} arg_slots={arg_slots} func_id={} name={}",
                func_id, func_def.name
            ));
        }
    };
    if let Err(err) = validate_call_frame_shape(func_def) {
        return ExecResult::JitError(err.message("JIT call callee frame shape"));
    }
    if let Err(err) = validate_call_return_window(caller_func, ret_reg, ret_slots as u16) {
        return ExecResult::JitError(err.message("JIT call caller return window"));
    }

    let jit_bp = match fiber.try_push_borrowed_call_frame(
        func_id,
        arg_start as u16,
        ret_reg,
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
    ctx.ctx.current_func_id = fiber
        .frames
        .last()
        .map(|frame| frame.func_id)
        .unwrap_or(u32::MAX);

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
    fiber.execution_budget = ctx.ctx.execution_budget;

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
            if heap_returns && ctx.ctx.ret_gcref_start != func.heap_ret_gcref_start {
                return ExecResult::JitError(format!(
                    "JIT heap return metadata mismatch: ctx ret_gcref_start={} function ret_gcref_start={} func_id={} name={}",
                    ctx.ctx.ret_gcref_start,
                    func.heap_ret_gcref_start,
                    frame.func_id,
                    func.name
                ));
            }
            let ret_gcref_start = func.heap_ret_gcref_start as usize;

            // errdefer should run if:
            // - explicit fail return (set by JIT), OR
            // - function has error return and error is non-nil
            let include_errdefers = if func.error_ret_slot < 0 {
                false
            } else if ctx.ctx.is_error_return != 0 {
                true
            } else if heap_returns {
                let slot0 = match crate::exec::read_heap_error_return_slot0_for_errdefer(
                    &vm.state.gc,
                    fiber,
                    func,
                    frame.bp,
                    ret_gcref_start,
                    func.heap_ret_gcref_count as usize,
                    frame.func_id,
                    frame.pc,
                    "JIT heap return error check",
                ) {
                    Ok(slot0) => slot0,
                    Err(result) => return result,
                };
                (slot0 & 0xFF) != 0
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
    use vo_runtime::SlotType;

    #[test]
    fn vm_heap_return_metadata_mismatch_is_jit_error() {
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
                assert!(msg.contains("heap return metadata mismatch"), "{msg}")
            }
            other => panic!("heap return metadata mismatch must be JitError, got {other:?}"),
        }
    }

    #[test]
    fn vm_stack_return_error_slot_outside_ret_buffer_is_jit_error() {
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

    extern "C" fn unreachable_jit_call(
        _ctx: *mut vo_runtime::jit_api::JitContext,
        _args: *mut u64,
        _ret: *mut u64,
    ) -> JitResult {
        panic!("frame-shape validation must reject before invoking JIT code")
    }

    #[test]
    fn vm_jit_dispatch_rejects_scan_slots_beyond_locals_before_stack_overflow_trap_062() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-dispatch-frame-shape-test".to_string());
        module.functions.push(function(1, 0));
        module.functions.push(function(1, 2));

        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0, 0);
        let before_frames = fiber.frames.len();
        let before_sp = fiber.sp;
        let inst = Instruction::new(vo_runtime::instruction::Opcode::Call, 1, 0, 0);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            dispatch_jit_call(&mut vm, &mut fiber, &inst, &module, unreachable_jit_call, 1)
        }));

        match result {
            Ok(ExecResult::JitError(msg)) => {
                assert!(msg.contains("JIT call callee frame shape"), "{msg}");
            }
            Ok(other) => panic!("JIT frame-shape drift should be JitError, got {other:?}"),
            Err(_) => panic!("JIT frame-shape drift must not panic"),
        }
        assert_eq!(fiber.frames.len(), before_frames);
        assert_eq!(fiber.sp, before_sp);
    }

    #[test]
    fn vm_jit_ok_errdefer_heap_return_check_rejects_short_error_allocation_before_defer_059() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-errdefer-heap-return-short-error".to_string());
        let mut func = function(1, 0);
        func.slot_types = vec![SlotType::GcRef];
        func.ret_slots = 2;
        func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
        func.heap_ret_gcref_count = 1;
        func.heap_ret_gcref_start = 0;
        func.heap_ret_slots = vec![2];
        func.error_ret_slot = 0;
        func.has_defer = true;
        module.functions.push(func);
        module.functions.push(function(0, 0));

        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0, 0);
        let short_ref = vm.state.gc.alloc(
            vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Struct),
            1,
        );
        unsafe { vo_runtime::gc::Gc::write_slot(short_ref, 0, 1) };
        fiber.stack[0] = short_ref as u64;
        fiber.defer_stack.push(crate::fiber::DeferEntry {
            frame_depth: fiber.frames.len(),
            func_id: 1,
            closure: core::ptr::null_mut(),
            args: core::ptr::null_mut(),
            arg_layout: crate::fiber::DeferArgLayout {
                slot_types: Vec::new(),
            },
            is_closure: false,
            is_errdefer: true,
            registered_at_generation: 0,
        });
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.ret_is_heap = 1;
        ctx.ctx.ret_gcref_start = 0;
        ctx.ctx.is_error_return = 0;

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            handle_jit_result(&mut vm, &mut fiber, &module, JitResult::Ok, ctx, 0, &[])
        }));

        match result {
            Ok(ExecResult::JitError(msg)) => {
                assert!(msg.contains("JIT heap return error check"), "{msg}");
                assert!(msg.contains("heap return allocation too small"), "{msg}");
                assert!(msg.contains("required_slots=2"), "{msg}");
            }
            Ok(other) => panic!(
                "short JIT heap error allocation should fail before errdefer frame, got {other:?}"
            ),
            Err(_) => panic!("short JIT heap error allocation must not be dereferenced"),
        }
        assert_eq!(fiber.frames.len(), 1);
        assert!(fiber.unwinding.is_none());
    }

    #[test]
    fn vm_jit_heap_error_return_uses_final_heap_ref_058() {
        let mut func = function(1, 0);
        func.ret_slots = 4;
        func.ret_slot_types = vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Interface0,
            SlotType::Interface1,
        ];
        func.heap_ret_gcref_count = 2;
        func.heap_ret_slots = vec![2, 2];
        func.error_ret_slot = 2;

        assert_eq!(heap_error_return_gcref_index(&func), Some(1));
    }
}
