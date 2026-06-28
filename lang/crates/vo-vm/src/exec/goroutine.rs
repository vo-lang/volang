#![allow(clippy::not_unsafe_ptr_arg_deref)]
//! Goroutine instructions: GoStart

#[cfg(not(feature = "std"))]
use alloc::{format, string::String};
#[cfg(feature = "std")]
use std::string::String;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::slot::Slot;
use vo_runtime::SlotType;

use crate::bytecode::Module;
use crate::fiber::Fiber;
use crate::frame_call::{
    validate_closure_arg_shape, validate_closure_callsite_arg_layout, validate_closure_target,
    validate_function_arg_shape, validate_function_callsite_arg_layout,
};
use crate::instruction::Instruction;
use crate::vm::helpers::{
    stack_get, try_build_validated_closure_fiber_from_args_ptr, ClosureFiberBuildError,
};
use crate::vm::RuntimeTrapKind;

pub struct GoResult {
    pub new_fiber: Fiber,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GoStartError {
    Trap(RuntimeTrapKind),
    Malformed(String),
}

/// GoStart: Start goroutine
/// - a: func_id_low (if flags bit 0 = 0) or closure_reg (if flags bit 0 = 1)
/// - b: args_start
/// - c: arg_slots
/// - flags bit 0: is_closure, bits 1-7: func_id_high (when not closure)
pub fn exec_go_start(
    gc: &Gc,
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    module: &Module,
    next_fiber_id: u32,
    callsite_arg_layout: &[SlotType],
    callsite_ret_layout: &[SlotType],
) -> Result<GoResult, GoStartError> {
    let functions = module.functions.as_slice();
    let is_closure_call = inst.call_shape_is_closure();
    let args_start = inst.b;
    let arg_slots = inst.c;

    let arg_count = arg_slots as usize;
    let src_start = bp + args_start as usize;
    if !callsite_ret_layout.is_empty() {
        return Err(GoStartError::Malformed(format!(
            "GoStart callsite return layout must be empty, got {callsite_ret_layout:?}"
        )));
    }
    let new_fiber = if is_closure_call {
        let raw_ref = stack_get(stack, bp + inst.a as usize) as GcRef;
        if raw_ref.is_null() {
            return Err(GoStartError::Trap(RuntimeTrapKind::NilFuncCall));
        }
        let target = validate_closure_target(gc, module, raw_ref as u64, "Go closure spawn")
            .map_err(GoStartError::Malformed)?;
        validate_closure_arg_shape("Go closure spawn", &target, arg_count)
            .map_err(GoStartError::Malformed)?;
        validate_closure_callsite_arg_layout("GoStart", &target, callsite_arg_layout)
            .map_err(GoStartError::Malformed)?;
        unsafe {
            try_build_validated_closure_fiber_from_args_ptr(
                next_fiber_id,
                &target,
                stack.add(src_start),
                arg_slots as u32,
            )
            .map_err(|err| match err {
                ClosureFiberBuildError::Trap(kind) => GoStartError::Trap(kind),
                ClosureFiberBuildError::Malformed(msg) => GoStartError::Malformed(msg),
            })?
        }
    } else {
        let func_id = inst.call_shape_static_func_id();
        let Some(func) = functions.get(func_id as usize) else {
            return Err(GoStartError::Malformed(format!(
                "GoStart missing function id {func_id}"
            )));
        };
        validate_function_arg_shape("GoStart", func_id, func, arg_count)
            .map_err(GoStartError::Malformed)?;
        validate_function_callsite_arg_layout(
            "GoStart",
            func_id,
            func,
            0,
            arg_count,
            callsite_arg_layout,
        )
        .map_err(GoStartError::Malformed)?;
        let mut new_fiber = Fiber::new(next_fiber_id);
        new_fiber
            .try_push_frame(func_id, func.local_slots, func.gc_scan_slots, 0, 0)
            .map_err(|_| GoStartError::Trap(RuntimeTrapKind::StackOverflow))?;
        let new_stack = new_fiber.stack_ptr();
        for i in 0..arg_count {
            unsafe { *new_stack.add(i) = stack_get(stack, src_start + i) };
        }
        new_fiber
    };

    Ok(GoResult { new_fiber })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Opcode;
    use vo_runtime::bytecode::FunctionDef;
    use vo_runtime::objects::closure;
    use vo_runtime::SlotType;
    use vo_runtime::{ValueKind, ValueMeta};

    fn function_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
        FunctionDef {
            name: "target".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: slot_types.len() as u16,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
            ret_slots: 0,
            ret_slot_types: Vec::new(),
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
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(
                &slot_types,
            ),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
            slot_types,
        }
    }

    #[test]
    fn vm_gostart_closure_signature_003_rejects_header_slot_drift_before_raw_read() {
        let mut gc = Gc::new();
        let malformed_closure = gc.alloc(ValueMeta::new(0, ValueKind::Closure), 0);
        let stack = [malformed_closure as Slot];
        let inst = Instruction::with_flags(Opcode::GoStart, 1, 0, 0, 0);
        let module = Module::new("gostart-malformed-closure".to_string());

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            exec_go_start(&gc, stack.as_ptr(), 0, &inst, &module, 1, &[], &[])
        }));

        match result {
            Ok(Err(GoStartError::Malformed(msg))) => {
                assert!(
                    msg.contains("closure layout has 0 allocation slots"),
                    "{msg}"
                );
            }
            Ok(Err(other)) => panic!("malformed closure GoStart should reject, got {other:?}"),
            Ok(Ok(_)) => panic!("malformed closure GoStart should not create a fiber"),
            Err(_) => panic!("malformed closure GoStart must not panic"),
        }
    }

    #[test]
    fn vm_gostart_closure_shape_006_rejects_arg_slot_drift_before_spawn() {
        let mut module = Module::new("gostart-closure-arg-shape".to_string());
        let mut target = function_with_slot_types(vec![SlotType::GcRef, SlotType::Value]);
        target.param_count = 1;
        target.param_slots = 2;
        target.local_slots = 2;
        target.gc_scan_slots = 1;
        target.is_closure = true;
        module.functions.push(target);

        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 0);
        let stack = [closure_ref as Slot];
        let inst = Instruction::with_flags(Opcode::GoStart, 1, 0, 0, 0);

        match exec_go_start(&gc, stack.as_ptr(), 0, &inst, &module, 1, &[], &[]) {
            Err(GoStartError::Malformed(msg)) => {
                assert!(
                    msg.contains("Go closure spawn arg slot count 0 does not match target 1"),
                    "{msg}"
                );
            }
            Err(other) => panic!("arg shape drift should be malformed, got {other:?}"),
            Ok(_) => panic!("arg shape drift should not create a fiber"),
        }
    }

    #[test]
    fn vm_gostart_static_shape_006_rejects_arg_slot_drift_before_spawn() {
        let mut module = Module::new("gostart-static-arg-shape".to_string());
        let mut target = function_with_slot_types(vec![SlotType::Value]);
        target.param_count = 1;
        target.param_slots = 1;
        target.local_slots = 1;
        module.functions.push(target);

        let gc = Gc::new();
        let stack = [11_u64, 22_u64];
        let inst = Instruction::with_flags(Opcode::GoStart, 0, 0, 0, 2);

        match exec_go_start(
            &gc,
            stack.as_ptr(),
            0,
            &inst,
            &module,
            1,
            &[SlotType::Value, SlotType::Value],
            &[],
        ) {
            Err(GoStartError::Malformed(msg)) => {
                assert!(
                    msg.contains("GoStart arg slot count 2 does not match target 1"),
                    "{msg}"
                );
            }
            Err(other) => panic!("arg shape drift should be malformed, got {other:?}"),
            Ok(_) => panic!("arg shape drift should not create a fiber"),
        }
    }

    #[test]
    fn vm_gostart_closure_signature_003_rejects_arg_slot_metadata_drift_before_spawn() {
        let mut module = Module::new("gostart-closure-arg-metadata".to_string());
        let mut target = function_with_slot_types(vec![SlotType::GcRef, SlotType::GcRef]);
        target.param_count = 1;
        target.param_slots = 2;
        target.local_slots = 2;
        target.gc_scan_slots = 2;
        target.is_closure = true;
        module.functions.push(target);

        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 0);
        let stack = [closure_ref as Slot, 0];
        let inst = Instruction::with_flags(Opcode::GoStart, 1, 0, 1, 1);

        match exec_go_start(
            &gc,
            stack.as_ptr(),
            0,
            &inst,
            &module,
            1,
            &[SlotType::Value],
            &[],
        ) {
            Err(GoStartError::Malformed(msg)) => {
                assert!(msg.contains("GoStart arg slot metadata mismatch"), "{msg}");
            }
            Err(other) => panic!("metadata drift should be malformed, got {other:?}"),
            Ok(_) => panic!("metadata drift should not create a fiber"),
        }
    }
}
