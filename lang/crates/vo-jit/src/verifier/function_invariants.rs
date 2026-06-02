use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use super::JitMetadataError;

pub(super) fn verify_function_invariants(func: &FunctionDef) -> Result<(), JitMetadataError> {
    let invariant = |detail: String| JitMetadataError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };

    if func.local_slots as usize != func.slot_types.len() {
        return Err(invariant(format!(
            "local_slots={} but slot_types.len()={}",
            func.local_slots,
            func.slot_types.len()
        )));
    }
    if func.param_slots > func.local_slots {
        return Err(invariant(format!(
            "param_slots={} exceeds local_slots={}",
            func.param_slots, func.local_slots
        )));
    }
    if func.recv_slots > func.param_slots {
        return Err(invariant(format!(
            "recv_slots={} exceeds param_slots={}",
            func.recv_slots, func.param_slots
        )));
    }
    if func.ret_slot_types.len() != func.ret_slots as usize {
        return Err(invariant(format!(
            "ret_slot_types.len()={} but ret_slots={}",
            func.ret_slot_types.len(),
            func.ret_slots
        )));
    }
    if func.gc_scan_slots != FunctionDef::compute_gc_scan_slots(&func.slot_types) {
        return Err(invariant(format!(
            "gc_scan_slots={} but computed={}",
            func.gc_scan_slots,
            FunctionDef::compute_gc_scan_slots(&func.slot_types)
        )));
    }
    let expected_prefix = FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    if func.borrowed_scan_slots_prefix != expected_prefix {
        return Err(invariant(format!(
            "borrowed_scan_slots_prefix.len()={} but computed len={}",
            func.borrowed_scan_slots_prefix.len(),
            expected_prefix.len()
        )));
    }
    let has_defer = func
        .code
        .iter()
        .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
    if func.has_defer != has_defer {
        return Err(invariant(format!(
            "has_defer={} but bytecode has_defer={}",
            func.has_defer, has_defer
        )));
    }
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
    if func.has_calls != has_calls {
        return Err(invariant(format!(
            "has_calls={} but bytecode has_calls={}",
            func.has_calls, has_calls
        )));
    }
    if func.has_call_extern != has_call_extern {
        return Err(invariant(format!(
            "has_call_extern={} but bytecode has_call_extern={}",
            func.has_call_extern, has_call_extern
        )));
    }
    if func.heap_ret_slots.len() != func.heap_ret_gcref_count as usize {
        return Err(invariant(format!(
            "heap_ret_slots.len()={} but heap_ret_gcref_count={}",
            func.heap_ret_slots.len(),
            func.heap_ret_gcref_count
        )));
    }
    if func.heap_ret_gcref_count > 0 {
        let end = func
            .heap_ret_gcref_start
            .checked_add(func.heap_ret_gcref_count)
            .ok_or_else(|| {
                invariant(format!(
                    "heap return range {}..+{} overflows",
                    func.heap_ret_gcref_start, func.heap_ret_gcref_count
                ))
            })?;
        if end > func.local_slots {
            return Err(invariant(format!(
                "heap return range {}..{} exceeds local_slots={}",
                func.heap_ret_gcref_start, end, func.local_slots
            )));
        }
        for slot in func.heap_ret_gcref_start..end {
            if func.slot_types[slot as usize] != SlotType::GcRef {
                return Err(invariant(format!(
                    "heap return slot {slot} must be GcRef, got {:?}",
                    func.slot_types[slot as usize]
                )));
            }
        }
    }
    if func.error_ret_slot >= 0 && (func.error_ret_slot as u16).saturating_add(1) >= func.ret_slots
    {
        return Err(invariant(format!(
            "error_ret_slot={} is not a two-slot interface inside ret_slots={}",
            func.error_ret_slot, func.ret_slots
        )));
    }
    if func.is_closure
        && (func.param_slots == 0 || func.slot_types.first() != Some(&SlotType::GcRef))
    {
        return Err(invariant(
            "closure functions must reserve GcRef slot 0".to_string(),
        ));
    }
    for (idx, transfer) in func.capture_types.iter().enumerate() {
        validate_transfer_type_kinds(func, idx, "capture_types", transfer)?;
    }
    for (idx, transfer) in func.param_types.iter().enumerate() {
        validate_transfer_type_kinds(func, idx, "param_types", transfer)?;
    }
    validate_transfer_shape_invariants(func)?;

    Ok(())
}

fn validate_transfer_shape_invariants(func: &FunctionDef) -> Result<(), JitMetadataError> {
    let invariant = |detail: String| JitMetadataError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };

    if func.capture_types.len() != func.capture_slot_types.len() {
        return Err(invariant(format!(
            "capture_types.len()={} but capture_slot_types.len()={}",
            func.capture_types.len(),
            func.capture_slot_types.len()
        )));
    }

    if !func.param_types.is_empty() {
        let implicit_param_slots = func
            .recv_slots
            .checked_add(u16::from(func.is_closure))
            .ok_or_else(|| {
                invariant(format!(
                    "implicit param slots overflow recv_slots={} is_closure={}",
                    func.recv_slots, func.is_closure
                ))
            })?;
        if implicit_param_slots > func.param_slots {
            return Err(invariant(format!(
                "implicit param slots {} exceed param_slots={}",
                implicit_param_slots, func.param_slots
            )));
        }
        let expected_without_receiver = func.param_slots - implicit_param_slots;
        let expected_with_receiver = func
            .param_slots
            .checked_sub(u16::from(func.is_closure))
            .ok_or_else(|| {
                invariant(format!(
                    "closure self slot exceeds param_slots={}",
                    func.param_slots
                ))
            })?;
        let actual_transfer_slots = func
            .param_types
            .iter()
            .try_fold(0u16, |acc, transfer| acc.checked_add(transfer.slots));
        let Some(actual_transfer_slots) = actual_transfer_slots else {
            return Err(invariant(
                "param_types total slots overflow u16".to_string(),
            ));
        };
        if actual_transfer_slots != expected_without_receiver
            && (func.recv_slots == 0 || actual_transfer_slots != expected_with_receiver)
        {
            return Err(invariant(format!(
                "param_types total slots {} but expected {} without receiver or {} with explicit receiver from param_slots={} recv_slots={} is_closure={}",
                actual_transfer_slots,
                expected_without_receiver,
                expected_with_receiver,
                func.param_slots,
                func.recv_slots,
                func.is_closure
            )));
        }
    }

    Ok(())
}

fn validate_transfer_type_kinds(
    func: &FunctionDef,
    idx: usize,
    access: &'static str,
    transfer: &vo_common_core::TransferType,
) -> Result<(), JitMetadataError> {
    let invariant = |detail: String| JitMetadataError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };
    let meta_kind = transfer.meta_raw as u8;
    let meta_kind = ValueKind::try_from(meta_kind).map_err(|_| {
        invariant(format!(
            "{access}[{idx}] has invalid ValueMeta kind tag {meta_kind}"
        ))
    })?;
    let rttid_kind = transfer.rttid_raw as u8;
    let rttid_kind = ValueKind::try_from(rttid_kind).map_err(|_| {
        invariant(format!(
            "{access}[{idx}] has invalid ValueRttid kind tag {rttid_kind}"
        ))
    })?;
    if meta_kind != rttid_kind {
        return Err(invariant(format!(
            "{access}[{idx}] ValueMeta kind {:?} does not match ValueRttid kind {:?}",
            meta_kind, rttid_kind
        )));
    }
    Ok(())
}
