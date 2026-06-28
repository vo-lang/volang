//! Call instructions: Call, CallExtern, CallClosure, CallIface
//!
//! Note: Return and panic unwinding logic has been moved to unwind.rs

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};

#[cfg(test)]
use vo_runtime::objects::closure;
use vo_runtime::objects::interface;

use crate::bytecode::Module;
use crate::fiber::{Fiber, FiberCapacityError};
use crate::frame_call::{
    call_iface_layout_for_callsite, validate_call_frame_shape, validate_call_return_window,
    validate_dynamic_call_shape, validate_function_callsite_layout, FrameCallBuilder,
};
use crate::instruction::Instruction;
use crate::vm::helpers::{runtime_panic, runtime_trap, stack_get, stack_set};
use crate::vm::{ExecResult, RuntimeTrapKind};
use vo_runtime::gc::Gc;
use vo_runtime::itab::ItabCache;

#[derive(Clone, Copy)]
pub(crate) struct CallIfaceTarget {
    pub(crate) func_id: u32,
    pub(crate) local_slots: u16,
    pub(crate) gc_scan_slots: u16,
}

#[inline]
fn named_type_id_for_rttid(module: &Module, rttid: u32) -> Option<u32> {
    match module.runtime_types.get(rttid as usize)? {
        vo_runtime::RuntimeType::Named { id, .. } => Some(*id),
        vo_runtime::RuntimeType::Pointer(inner) => named_type_id_for_rttid(module, inner.rttid()),
        _ => None,
    }
}

#[inline]
fn iface_method_info_for_target(
    module: &Module,
    rttid: u32,
    target_id: u32,
) -> Option<&vo_runtime::bytecode::MethodInfo> {
    let named_type_id = named_type_id_for_rttid(module, rttid)?;
    let named = module.named_type_metas.get(named_type_id as usize)?;
    named
        .methods
        .values()
        .find(|method| method.func_id == target_id)
}

pub(crate) fn validate_iface_receiver_layout(
    module: &Module,
    iface_slot0: u64,
    target_func: &crate::bytecode::FunctionDef,
    target_func_id: u32,
    context: &str,
) -> Result<(), String> {
    let value_kind = interface::unpack_value_kind(iface_slot0);
    if value_kind == vo_runtime::ValueKind::Interface {
        return Err(format!(
            "{context} receiver layout cannot be derived from raw interface-kind slot0"
        ));
    }
    if value_kind == vo_runtime::ValueKind::Void {
        return Err(format!(
            "{context} receiver layout cannot be derived from nil interface slot0"
        ));
    }
    let rttid = interface::unpack_rttid(iface_slot0);
    let value_rttid = vo_runtime::ValueRttid::new(rttid, value_kind);
    if module
        .canonical_value_meta_for_value_rttid(value_rttid)
        .is_none()
    {
        return Err(format!(
            "{context} receiver layout has non-canonical slot0 kind {value_kind:?} for RTTID {rttid}"
        ));
    }
    let method = iface_method_info_for_target(module, rttid, target_func_id).ok_or_else(|| {
        format!(
            "{context} receiver layout target func_id={target_func_id} is not a method of receiver RTTID {rttid}"
        )
    })?;
    let expected = method
        .iface_receiver_slot_type_for_source_kind(value_kind)
        .map_err(|reason| {
            format!(
                "{context} receiver layout target func_id={target_func_id} violates receiver ownership: {reason} for source kind {value_kind:?}"
            )
        })?;
    if target_func.recv_slots != 1 || target_func.slot_types.first() != Some(&expected) {
        return Err(format!(
            "{context} receiver layout {:?} does not match source kind {:?} storage {:?} for func_id={} name={}",
            target_func.slot_types.first(),
            value_kind,
            expected,
            target_func_id,
            target_func.name
        ));
    }
    Ok(())
}

#[inline]
fn probe_call_iface_ic(
    fiber: &Fiber,
    caller_func_id: u32,
    callsite_pc: u32,
    itab_id: u32,
    method_idx: u8,
) -> Option<CallIfaceTarget> {
    if fiber.call_iface_ic_table.is_empty() {
        return None;
    }
    let index = Fiber::call_iface_ic_index(caller_func_id, callsite_pc);
    let entry = &fiber.call_iface_ic_table[index];
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

pub(crate) fn resolve_iface_call_target(
    module: &Module,
    itab_cache: &ItabCache,
    itab_id: u32,
    method_idx: usize,
) -> Result<CallIfaceTarget, String> {
    let Some(itab) = itab_cache.get_itab(itab_id) else {
        return Err(format!(
            "CallIface: missing itab_id={itab_id} method_idx={method_idx}"
        ));
    };
    let Some(&func_id) = itab.methods.get(method_idx) else {
        return Err(format!(
            "CallIface: method_idx={} out of bounds for itab_id={} len={}",
            method_idx,
            itab_id,
            itab.methods.len()
        ));
    };

    let Some(func) = module.functions.get(func_id as usize) else {
        return Err(format!(
            "CallIface: target function id {func_id} out of bounds"
        ));
    };
    let recv_slots = func.recv_slots as usize;
    if recv_slots != 1 {
        return Err(format!(
            "CallIface ABI only supports recv_slots == 1, got {recv_slots} for func_id={func_id} name={}",
            func.name
        ));
    }

    Ok(CallIfaceTarget {
        func_id,
        local_slots: func.local_slots,
        gc_scan_slots: func.gc_scan_slots,
    })
}

fn resolve_call_iface_target(
    fiber: &Fiber,
    module: &Module,
    itab_cache: &ItabCache,
    itab_id: u32,
    method_idx: usize,
) -> Result<CallIfaceTarget, ExecResult> {
    resolve_iface_call_target(module, itab_cache, itab_id, method_idx).map_err(|msg| {
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
        ExecResult::JitError(format!(
            "{msg} caller_func_id={caller_func_id} caller_name={caller_name} caller_pc={caller_pc}"
        ))
    })
}

pub(crate) fn validate_call_iface_itab_for_callsite(
    itab_cache: &ItabCache,
    itab_id: u32,
    method_idx: usize,
    expected_iface_meta_id: u32,
    context: &str,
) -> Result<(), String> {
    let Some(actual_itab) = itab_cache.get_itab(itab_id) else {
        return Err(format!(
            "{context}: missing itab_id={itab_id} for callsite interface {expected_iface_meta_id} method_idx={method_idx}"
        ));
    };
    if actual_itab.iface_meta_id != expected_iface_meta_id {
        return Err(format!(
            "{context}: itab_id={itab_id} belongs to interface {} but callsite interface is {expected_iface_meta_id}",
            actual_itab.iface_meta_id
        ));
    }
    Ok(())
}

fn interface_meta_label(module: &Module, iface_meta_id: u32) -> String {
    module
        .interface_metas
        .get(iface_meta_id as usize)
        .map(|meta| {
            if meta.name.is_empty() {
                format!("#{iface_meta_id}")
            } else {
                format!("{iface_meta_id}:{}", meta.name)
            }
        })
        .unwrap_or_else(|| format!("{iface_meta_id}:<missing-interface-meta>"))
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

fn checked_borrowed_return_reg(
    opcode: &str,
    borrowed_start: u16,
    arg_slots: usize,
    func_id: u32,
    func_name: &str,
) -> Result<u16, ExecResult> {
    let arg_slots = u16::try_from(arg_slots).map_err(|_| {
        ExecResult::JitError(format!(
            "{opcode} arg slot count {arg_slots} exceeds u16 for func_id={func_id} name={func_name}"
        ))
    })?;
    borrowed_start.checked_add(arg_slots).ok_or_else(|| {
        ExecResult::JitError(format!(
            "{opcode} return offset overflow: borrowed_start={borrowed_start} arg_slots={arg_slots} func_id={func_id} name={func_name}"
        ))
    })
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
    let ret_reg = match checked_borrowed_return_reg("Call", inst.b, arg_slots, func_id, &func.name)
    {
        Ok(ret_reg) => ret_reg,
        Err(result) => return result,
    };
    if let Err(err) = validate_call_frame_shape(func) {
        return ExecResult::JitError(err.message("Call callee frame shape"));
    }
    if let Err(err) = validate_call_return_window(caller_func, ret_reg, ret_slots) {
        return ExecResult::JitError(err.message("Call caller return window"));
    }
    let new_bp = match fiber.try_push_borrowed_call_frame(
        func_id,
        arg_start as u16,
        ret_reg,
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
    let caller_frame = fiber.frames.last().copied().ok_or_else(|| {
        ExecResult::JitError("CallClosure requested without an active caller frame".to_string())
    });
    let caller_frame = match caller_frame {
        Ok(frame) => frame,
        Err(result) => return result,
    };
    let caller_bp = caller_frame.bp;
    let stack = fiber.stack_ptr();
    let closure_value = stack_get(stack, caller_bp + inst.a as usize);
    FrameCallBuilder::new(gc, fiber, module).call_closure_borrowed(
        closure_value,
        inst.b as usize,
        inst.packed_arg_slots(),
        inst.packed_ret_slots(),
    )
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
    let callsite_pc = match caller_frame.pc.checked_sub(1) {
        Some(pc) => pc as u32,
        None => {
            return ExecResult::JitError(
                "CallIface requested before caller pc advanced".to_string(),
            );
        }
    };
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
    let (expected_iface_meta_id, callsite_arg_layout, callsite_ret_layout) =
        match call_iface_layout_for_callsite(caller_func, callsite_pc as usize, "CallIface") {
            Ok(layout) => layout,
            Err(err) => return ExecResult::JitError(err),
        };

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
    let callsite_context = format!(
        "CallIface caller_func_id={} caller_name={} callsite_pc={} callsite_interface={}",
        caller_func_id,
        caller_func.name,
        callsite_pc,
        interface_meta_label(module, expected_iface_meta_id)
    );
    if let Err(err) = validate_call_iface_itab_for_callsite(
        itab_cache,
        itab_id,
        method_idx,
        expected_iface_meta_id,
        &callsite_context,
    ) {
        return ExecResult::JitError(err);
    }
    let (target, fill_ic_after_validation) =
        match probe_call_iface_ic(fiber, caller_func_id, callsite_pc, itab_id, method_idx_u8) {
            Some(target) => (target, false),
            None => {
                let target =
                    match resolve_call_iface_target(fiber, module, itab_cache, itab_id, method_idx)
                    {
                        Ok(target) => target,
                        Err(result) => return result,
                    };
                (target, true)
            }
        };
    let Some(target_func) = module.functions.get(target.func_id as usize) else {
        return ExecResult::JitError(format!(
            "CallIface cached target function id {} out of bounds",
            target.func_id
        ));
    };
    if let Err(err) =
        validate_iface_receiver_layout(module, slot0, target_func, target.func_id, "CallIface")
    {
        return ExecResult::JitError(err);
    }
    let arg_slots = target_func.param_slots as usize;
    let expected_user_arg_slots = match arg_slots.checked_sub(target_func.recv_slots as usize) {
        Some(slots) => slots,
        None => {
            return ExecResult::JitError(format!(
                "CallIface target recv_slots {} exceed param_slots {} for func_id={} name={}",
                target_func.recv_slots, target_func.param_slots, target.func_id, target_func.name
            ));
        }
    };
    if let Err(result) = validate_dynamic_call_shape(
        "CallIface",
        inst.packed_arg_slots(),
        inst.packed_ret_slots(),
        expected_user_arg_slots,
        target_func.ret_slots,
        target.func_id,
        &target_func.name,
    ) {
        return result;
    }
    if let Err(err) = validate_function_callsite_layout(
        "CallIface",
        target.func_id,
        target_func,
        target_func.recv_slots as usize,
        expected_user_arg_slots,
        callsite_arg_layout,
        callsite_ret_layout,
    ) {
        return ExecResult::JitError(err);
    }
    let ret_slots = target_func.ret_slots;
    let ret_reg = match checked_borrowed_return_reg(
        "CallIface",
        borrowed_start,
        arg_slots,
        target.func_id,
        &target_func.name,
    ) {
        Ok(ret_reg) => ret_reg,
        Err(result) => return result,
    };
    if let Err(err) = validate_call_frame_shape(target_func) {
        return ExecResult::JitError(err.message("CallIface callee frame shape"));
    }
    if let Err(err) = validate_call_return_window(caller_func, ret_reg, ret_slots) {
        return ExecResult::JitError(err.message("CallIface caller return window"));
    }

    let new_bp = match fiber.try_push_borrowed_call_frame(
        target.func_id,
        borrowed_start,
        ret_reg,
        ret_slots,
        caller_scan_slots,
        target.local_slots,
        target.gc_scan_slots,
    ) {
        Ok(bp) => bp,
        Err(err) => return stack_overflow_panic(gc, fiber, module, err),
    };
    if fill_ic_after_validation {
        fill_call_iface_ic(
            fiber,
            caller_func_id,
            callsite_pc,
            itab_id,
            method_idx_u8,
            target,
        );
    }
    fiber.zero_slots_tail_at(new_bp, target.gc_scan_slots as usize, arg_slots);
    let stack = fiber.stack_ptr();

    stack_set(stack, new_bp, slot1);

    ExecResult::FrameChanged
}

#[cfg(test)]
mod tests;
