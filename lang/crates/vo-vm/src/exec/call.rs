//! Call instructions: Call, CallExtern, CallClosure, CallIface
//!
//! Note: Return and panic unwinding logic has been moved to unwind.rs

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::ToString;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::{closure, interface};

use crate::bytecode::Module;
use crate::fiber::{Fiber, FiberCapacityError};
use crate::instruction::Instruction;
use crate::vm::helpers::{runtime_panic, runtime_trap, stack_get, stack_set};
use crate::vm::{ExecResult, RuntimeTrapKind};
use vo_runtime::gc::Gc;
use vo_runtime::itab::ItabCache;

#[derive(Clone, Copy)]
struct CallIfaceTarget {
    func_id: u32,
    local_slots: u16,
    gc_scan_slots: u16,
}

#[inline]
fn probe_call_iface_ic(
    fiber: &mut Fiber,
    caller_func_id: u32,
    callsite_pc: u32,
    itab_id: u32,
    method_idx: u8,
) -> Option<CallIfaceTarget> {
    let index = Fiber::call_iface_ic_index(caller_func_id, callsite_pc);
    let table = fiber.ensure_call_iface_ic_table();
    let entry = &table[index];
    if entry.matches(caller_func_id, callsite_pc, itab_id, method_idx) {
        Some(CallIfaceTarget {
            func_id: entry.func_id,
            local_slots: entry.local_slots,
            gc_scan_slots: entry.gc_scan_slots,
        })
    } else {
        None
    }
}

#[inline]
fn fill_call_iface_ic(
    fiber: &mut Fiber,
    caller_func_id: u32,
    callsite_pc: u32,
    itab_id: u32,
    method_idx: u8,
    target: CallIfaceTarget,
) {
    let index = Fiber::call_iface_ic_index(caller_func_id, callsite_pc);
    let table = fiber.ensure_call_iface_ic_table();
    table[index] = crate::fiber::CallIfaceICEntry {
        caller_func_id,
        callsite_pc,
        itab_id,
        method_idx,
        valid: true,
        local_slots: target.local_slots,
        gc_scan_slots: target.gc_scan_slots,
        func_id: target.func_id,
    };
}

fn resolve_call_iface_target(
    fiber: &Fiber,
    module: &Module,
    itab_cache: &ItabCache,
    itab_id: u32,
    method_idx: usize,
) -> Result<CallIfaceTarget, ExecResult> {
    let Some(itab) = itab_cache.get_itab(itab_id) else {
        return Err(ExecResult::JitError(format!(
            "CallIface: missing itab_id={itab_id} method_idx={method_idx}"
        )));
    };
    let Some(&func_id) = itab.methods.get(method_idx) else {
        let caller = fiber.frames.last().copied();
        let (caller_func_id, caller_pc, caller_name) = caller
            .map(|frame| {
                let name = module
                    .functions
                    .get(frame.func_id as usize)
                    .map(|func| func.name.as_str())
                    .unwrap_or("<missing-func>");
                (frame.func_id, frame.pc, name)
            })
            .unwrap_or((u32::MAX, 0, "<no-frame>"));
        return Err(ExecResult::JitError(format!(
            "CallIface: method_idx={} out of bounds for itab_id={} len={} caller_func_id={} caller_name={} caller_pc={}",
            method_idx,
            itab_id,
            itab.methods.len(),
            caller_func_id,
            caller_name,
            caller_pc,
        )));
    };

    let Some(func) = module.functions.get(func_id as usize) else {
        return Err(ExecResult::JitError(format!(
            "CallIface: target function id {func_id} out of bounds"
        )));
    };
    let recv_slots = func.recv_slots as usize;
    if recv_slots != 1 {
        return Err(ExecResult::JitError(format!(
            "CallIface ABI only supports recv_slots == 1, got {recv_slots} for func_id={func_id} name={}",
            func.name
        )));
    }

    Ok(CallIfaceTarget {
        func_id,
        local_slots: func.local_slots,
        gc_scan_slots: func.gc_scan_slots,
    })
}

#[inline]
fn stack_overflow_panic(
    gc: &mut Gc,
    fiber: &mut Fiber,
    module: &Module,
    err: FiberCapacityError,
) -> ExecResult {
    let stack = fiber.stack_ptr();
    runtime_panic(
        gc,
        fiber,
        stack,
        module,
        RuntimeTrapKind::StackOverflow,
        err.message(),
    )
}

pub fn exec_call(
    gc: &mut Gc,
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    let func_id = inst.static_call_func_id();
    let arg_start = inst.b as usize;
    let caller_frame = fiber.frames.last().copied().ok_or_else(|| {
        ExecResult::JitError("Call requested without an active caller frame".to_string())
    });
    let caller_frame = match caller_frame {
        Ok(frame) => frame,
        Err(result) => return result,
    };
    let Some(caller_func) = module.functions.get(caller_frame.func_id as usize) else {
        return ExecResult::JitError(format!(
            "Call requested from missing caller function id {}",
            caller_frame.func_id
        ));
    };
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(arg_start as u16);

    let Some(func) = module.functions.get(func_id as usize) else {
        return ExecResult::JitError(format!("missing call target function id {func_id}"));
    };
    let arg_slots = func.param_slots as usize;
    let local_slots = func.local_slots as usize;
    let gc_scan_slots = func.gc_scan_slots as usize;
    let ret_slots = func.ret_slots;
    let new_bp = match fiber.try_push_borrowed_call_frame(
        func_id,
        arg_start as u16,
        inst.b + arg_slots as u16,
        ret_slots,
        caller_scan_slots,
        local_slots as u16,
        func.gc_scan_slots,
    ) {
        Ok(bp) => bp,
        Err(err) => return stack_overflow_panic(gc, fiber, module, err),
    };
    fiber.zero_slots_tail_at(new_bp, gc_scan_slots, arg_slots);

    ExecResult::FrameChanged
}

pub fn exec_call_closure(
    gc: &mut Gc,
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    let caller_bp = fiber.frames.last().map_or(0, |f| f.bp);
    let stack = fiber.stack_ptr();
    let closure_ref = stack_get(stack, caller_bp + inst.a as usize) as GcRef;
    if closure_ref.is_null() {
        return runtime_trap(gc, fiber, stack, module, RuntimeTrapKind::NilFuncCall);
    }
    let func_id = closure::func_id(closure_ref);
    let arg_start = inst.b as usize;

    let Some(func) = module.functions.get(func_id as usize) else {
        return ExecResult::JitError(format!("missing closure target function id {func_id}"));
    };
    let arg_slots = func.param_slots as usize;
    let ret_slots = func.ret_slots;

    // New frame's bp is current stack top
    let layout = vo_runtime::objects::closure::call_layout(
        closure_ref as u64,
        closure_ref,
        func.recv_slots as usize,
        func.is_closure,
    );

    if layout.arg_offset > 1 {
        return ExecResult::JitError(format!(
            "CallClosure ABI only supports arg_offset <= 1, got {} for func_id={} name={}",
            layout.arg_offset, func_id, func.name
        ));
    }
    if arg_start < layout.arg_offset {
        return ExecResult::JitError(format!(
            "CallClosure ABI underflow: arg_start={} arg_offset={} func_id={} name={}",
            arg_start, layout.arg_offset, func_id, func.name
        ));
    }

    let borrowed_start = (arg_start - layout.arg_offset) as u16;
    let caller_frame = fiber.frames.last().copied().ok_or_else(|| {
        ExecResult::JitError("CallClosure requested without an active caller frame".to_string())
    });
    let caller_frame = match caller_frame {
        Ok(frame) => frame,
        Err(result) => return result,
    };
    let Some(caller_func) = module.functions.get(caller_frame.func_id as usize) else {
        return ExecResult::JitError(format!(
            "CallClosure requested from missing caller function id {}",
            caller_frame.func_id
        ));
    };
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(borrowed_start);

    let new_bp = match fiber.try_push_borrowed_call_frame(
        func_id,
        borrowed_start,
        borrowed_start + arg_slots as u16,
        ret_slots,
        caller_scan_slots,
        func.local_slots,
        func.gc_scan_slots,
    ) {
        Ok(bp) => bp,
        Err(err) => return stack_overflow_panic(gc, fiber, module, err),
    };
    fiber.zero_slots_tail_at(new_bp, func.gc_scan_slots as usize, arg_slots);
    let stack = fiber.stack_ptr();

    if let Some(slot0_val) = layout.slot0 {
        stack_set(stack, new_bp, slot0_val);
    }

    ExecResult::FrameChanged
}

pub fn exec_call_iface(
    gc: &mut Gc,
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
    itab_cache: &ItabCache,
) -> ExecResult {
    let method_idx_u8 = inst.flags;
    let method_idx = method_idx_u8 as usize;

    let caller_frame = fiber.frames.last().copied().ok_or_else(|| {
        ExecResult::JitError("CallIface requested without an active caller frame".to_string())
    });
    let caller_frame = match caller_frame {
        Ok(frame) => frame,
        Err(result) => return result,
    };
    let caller_bp = caller_frame.bp;
    let caller_func_id = caller_frame.func_id;
    let callsite_pc = caller_frame.pc.saturating_sub(1) as u32;
    if inst.b == 0 {
        return ExecResult::JitError(
            "CallIface ABI requires a hidden receiver prefix slot before arg_start".to_string(),
        );
    }
    let borrowed_start = inst.b - 1;
    let Some(caller_func) = module.functions.get(caller_func_id as usize) else {
        return ExecResult::JitError(format!(
            "CallIface requested from missing caller function id {caller_func_id}"
        ));
    };
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(borrowed_start);

    let stack = fiber.stack_ptr();
    let slot0 = stack_get(stack, caller_bp + inst.a as usize);
    let slot1 = stack_get(stack, caller_bp + inst.a as usize + 1);
    if interface::is_nil(slot0) {
        return runtime_trap(
            gc,
            fiber,
            stack,
            module,
            RuntimeTrapKind::NilPointerDereference,
        );
    }

    let itab_id = (slot0 >> 32) as u32;
    let target =
        match probe_call_iface_ic(fiber, caller_func_id, callsite_pc, itab_id, method_idx_u8) {
            Some(target) => target,
            None => {
                let target =
                    match resolve_call_iface_target(fiber, module, itab_cache, itab_id, method_idx)
                    {
                        Ok(target) => target,
                        Err(result) => return result,
                    };
                fill_call_iface_ic(
                    fiber,
                    caller_func_id,
                    callsite_pc,
                    itab_id,
                    method_idx_u8,
                    target,
                );
                target
            }
        };
    let Some(target_func) = module.functions.get(target.func_id as usize) else {
        return ExecResult::JitError(format!(
            "CallIface cached target function id {} out of bounds",
            target.func_id
        ));
    };
    let arg_slots = target_func.param_slots as usize;
    let ret_slots = target_func.ret_slots;

    let new_bp = match fiber.try_push_borrowed_call_frame(
        target.func_id,
        borrowed_start,
        borrowed_start + arg_slots as u16,
        ret_slots,
        caller_scan_slots,
        target.local_slots,
        target.gc_scan_slots,
    ) {
        Ok(bp) => bp,
        Err(err) => return stack_overflow_panic(gc, fiber, module, err),
    };
    fiber.zero_slots_tail_at(new_bp, target.gc_scan_slots as usize, arg_slots);
    let stack = fiber.stack_ptr();

    stack_set(stack, new_bp, slot1);

    ExecResult::FrameChanged
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::FunctionDef;
    use vo_runtime::instruction::Opcode;
    use vo_runtime::itab::ItabCache;
    use vo_runtime::SlotType;

    fn function(local_slots: u16) -> FunctionDef {
        FunctionDef {
            name: "f".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: vec![SlotType::Value; local_slots as usize],
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&vec![
                SlotType::Value;
                local_slots as usize
            ]),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn call_iface_missing_itab_is_jit_error_instead_of_raw_panic() {
        let mut module = Module::new("missing-itab-test".to_string());
        module.functions.push(function(4));
        let mut gc = Gc::new();
        let mut fiber = Fiber::new(0);
        let bp = fiber.push_frame(0, 4, 0, 0, 0);
        fiber.stack[bp] = (99u64 << 32) | 1;
        fiber.stack[bp + 1] = 0;
        let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);
        let cache = ItabCache::default();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache)
        }));

        match result {
            Ok(ExecResult::JitError(msg)) => {
                assert!(msg.contains("missing itab_id=99"), "{msg}");
            }
            Ok(other) => panic!("missing itab should be a JitError, got {other:?}"),
            Err(_) => panic!("missing itab must not panic in CallIface"),
        }
    }

    #[test]
    fn call_closure_missing_function_is_jit_error_instead_of_index_panic() {
        let mut module = Module::new("missing-closure-target-test".to_string());
        module.functions.push(function(4));
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 7, 0);
        let mut fiber = Fiber::new(0);
        let bp = fiber.push_frame(0, 4, 0, 0, 0);
        fiber.stack[bp] = closure_ref as u64;
        let inst = Instruction::new(Opcode::CallClosure, 0, 0, 0);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            exec_call_closure(&mut gc, &mut fiber, &inst, &module)
        }));

        match result {
            Ok(ExecResult::JitError(msg)) => {
                assert!(
                    msg.contains("missing closure target function id 7"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("missing closure target should be a JitError, got {other:?}"),
            Err(_) => panic!("missing closure target must not panic in CallClosure"),
        }
    }
}
