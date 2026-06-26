//! Defer instructions: DeferPush, ErrDeferPush, Recover

#[cfg(not(feature = "std"))]
use alloc::{
    string::{String, ToString},
    vec::Vec,
};
#[cfg(feature = "std")]
use std::string::{String, ToString};

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::slot::Slot;
use vo_runtime::{ValueKind, ValueMeta};

use crate::bytecode::{FunctionDef, Module};
use crate::fiber::{CallFrame, DeferArgLayout, DeferEntry};
use crate::frame_call::validate_closure_target;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

/// DeferPush instruction format:
/// - a: func_id (if flags bit 0 = 0) or closure_reg (if flags bit 0 = 1)
/// - b: arg_start
/// - c: arg_slots
/// - flags bit 0: is_closure
#[inline]
#[allow(clippy::too_many_arguments)]
pub fn exec_defer_push(
    stack: *const Slot,
    bp: usize,
    frames: &[CallFrame],
    caller_func: &FunctionDef,
    module: &Module,
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    panic_generation: u64,
) -> Result<(), String> {
    push_defer_entry(
        stack,
        bp,
        frames,
        caller_func,
        module,
        defer_stack,
        inst,
        gc,
        false,
        panic_generation,
    )
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn exec_err_defer_push(
    stack: *const Slot,
    bp: usize,
    frames: &[CallFrame],
    caller_func: &FunctionDef,
    module: &Module,
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    panic_generation: u64,
) -> Result<(), String> {
    push_defer_entry(
        stack,
        bp,
        frames,
        caller_func,
        module,
        defer_stack,
        inst,
        gc,
        true,
        panic_generation,
    )
}

#[allow(clippy::too_many_arguments)]
fn push_defer_entry(
    stack: *const Slot,
    bp: usize,
    frames: &[CallFrame],
    caller_func: &FunctionDef,
    module: &Module,
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    is_errdefer: bool,
    panic_generation: u64,
) -> Result<(), String> {
    let is_closure = inst.call_shape_is_closure();
    let arg_start = inst.b;
    let arg_slots = inst.c;
    let Some(caller_frame) = frames.last() else {
        return Err("DeferPush missing caller frame".to_string());
    };
    let arg_layout = DeferArgLayout::try_from_caller_slot_types(
        &caller_func.slot_types,
        caller_frame.func_id,
        caller_frame.pc.saturating_sub(1) as u32,
        arg_start,
        arg_slots,
    )?;
    let frame_depth = frames.len();

    let (func_id, closure) = if is_closure {
        let closure_ref = stack_get(stack, bp + inst.a as usize) as GcRef;
        let closure = if closure_ref.is_null() {
            core::ptr::null_mut()
        } else {
            validate_closure_target(gc, module, closure_ref as u64, "DeferPush closure")?
                .closure_gcref
        };
        (0, closure)
    } else {
        let func_id = inst.call_shape_static_func_id();
        (func_id, core::ptr::null_mut())
    };

    let args = if arg_slots > 0 {
        let args_ref = gc.alloc(ValueMeta::new(0, ValueKind::Void), arg_slots);
        for i in 0..arg_slots {
            let val = stack_get(stack, bp + arg_start as usize + i as usize);
            unsafe { Gc::write_slot(args_ref, i as usize, val) };
        }
        args_ref
    } else {
        core::ptr::null_mut()
    };

    defer_stack.push(DeferEntry {
        frame_depth,
        func_id,
        closure,
        args,
        arg_layout,
        is_closure,
        is_errdefer,
        registered_at_generation: panic_generation,
    });
    Ok(())
}

/// recover() - only catches recoverable panics.
/// Returns interface{} as AnySlot (2 slots).
/// If no panic, returns nil interface.
///
/// Per Go semantics, recover() only works when called DIRECTLY from a deferred function.
/// If called from a nested function (e.g., `defer func() { innerRecover() }()`),
/// recover() returns nil without consuming the panic state.
///
/// When recover succeeds, also switches unwinding mode from Panic to Return.
/// This is critical: without this change, nested calls within the defer function
/// would incorrectly trigger panic_unwind when they return.
#[inline]
pub fn exec_recover(
    stack: *mut Slot,
    bp: usize,
    fiber: &mut crate::fiber::Fiber,
    inst: &Instruction,
) {
    use vo_runtime::InterfaceSlot;

    if !fiber.is_direct_defer_context() {
        // Not in direct defer context - return nil without consuming panic
        stack_set(stack, bp + inst.a as usize, 0);
        stack_set(stack, bp + inst.a as usize + 1, 0);
        return;
    }

    let recovered = fiber.take_recoverable_panic();
    let val = recovered.unwrap_or(InterfaceSlot::nil());
    stack_set(stack, bp + inst.a as usize, val.slot0);
    stack_set(stack, bp + inst.a as usize + 1, val.slot1);

    // If recover succeeded, switch unwinding mode from Panic to Return
    // so nested calls don't trigger panic_unwind.
    if recovered.is_some() {
        fiber.switch_panic_to_return_mode();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::Fiber;
    use crate::instruction::Opcode;
    use vo_runtime::SlotType;

    fn caller_func_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
        FunctionDef {
            name: "caller".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: slot_types.len() as u16,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: true,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types,
            borrowed_scan_slots_prefix: vec![0, 0],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn vm_defer_closure_kind_062_rejects_non_closure_before_defer_publication() {
        let mut gc = Gc::new();
        let non_closure = gc.alloc(ValueMeta::new(0, ValueKind::String), 1);
        let stack = [non_closure as u64];
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let caller_func = caller_func_with_slot_types(vec![SlotType::GcRef]);
        let module = vo_runtime::bytecode::Module::new("defer-closure-kind".to_string());
        let inst = Instruction::with_flags(Opcode::DeferPush, 1, 0, 0, 0);
        let mut defer_stack = Vec::new();

        let err = exec_defer_push(
            stack.as_ptr(),
            0,
            &fiber.frames,
            &caller_func,
            &module,
            &mut defer_stack,
            &inst,
            &mut gc,
            0,
        )
        .unwrap_err();

        assert!(err.contains("non-closure object kind"), "{err}");
        assert!(defer_stack.is_empty());
    }

    #[test]
    fn vm_defer_closure_kind_062_preserves_nil_defer_registration_for_recover() {
        let mut gc = Gc::new();
        let stack = [0_u64];
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let caller_func = caller_func_with_slot_types(vec![SlotType::GcRef]);
        let module = vo_runtime::bytecode::Module::new("defer-nil-closure".to_string());
        let inst = Instruction::with_flags(Opcode::DeferPush, 1, 0, 0, 0);
        let mut defer_stack = Vec::new();

        exec_defer_push(
            stack.as_ptr(),
            0,
            &fiber.frames,
            &caller_func,
            &module,
            &mut defer_stack,
            &inst,
            &mut gc,
            0,
        )
        .expect("nil deferred closure remains registered for unwind-time panic/recover");

        assert_eq!(defer_stack.len(), 1);
        assert!(defer_stack[0].is_closure);
        assert!(defer_stack[0].closure.is_null());
    }

    #[test]
    fn vm_defer_closure_kind_062_source_validates_before_entry_publication() {
        let source = crate::source_contract::production_source_without_test_modules(include_str!(
            "defer.rs"
        ));
        let push_body = source
            .split("fn push_defer_entry")
            .nth(1)
            .expect("push_defer_entry source");
        assert!(
            vo_source_contract::compact_pattern_before(
                push_body,
                "validate_closure_target(",
                "defer_stack.push(DeferEntry"
            ),
            "closure-form defer registration must validate closure target before DeferEntry publication"
        );
    }

    #[test]
    fn vm_defer_closure_kind_062_source_order_ignores_comment_spoofed_validator() {
        let probe = r#"
            fn push_defer_entry() {
                // validate_closure_target(
                defer_stack.push(DeferEntry {
                    frame_depth,
                });
            }
        "#;

        assert!(
            !vo_source_contract::compact_pattern_before(
                probe,
                "validate_closure_target(",
                "defer_stack.push(DeferEntry"
            ),
            "comments must not satisfy defer validator-before-publication source contracts"
        );
    }
}
