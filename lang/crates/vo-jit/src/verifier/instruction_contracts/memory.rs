use super::{
    decode_metadata_layout, flattened_global_slot_types, local_layout, verify_layout,
    verify_local_layout_matches, verify_raw_or_exact_layout_matches, verify_structural_layout,
    VerifierCtx,
};
use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let vo_module = ctx._vo_module;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::SlotGet => verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotGetN => {
            verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::SlotSet => verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotSetN => {
            verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::GlobalGet => {
            verify_global_get_contract(func, vo_module, pc, opcode, inst.b, inst.a, 1)
        }
        Opcode::GlobalGetN => verify_global_get_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags as u16,
        ),
        Opcode::GlobalSet => {
            verify_global_set_contract(func, vo_module, pc, opcode, inst.a, inst.b, 1)
        }
        Opcode::GlobalSetN => verify_global_set_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.a,
            inst.b,
            inst.flags as u16,
        ),
        Opcode::PtrNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "PtrNew metadata",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrNew destination",
            )
        }
        Opcode::PtrGet => verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, 1),
        Opcode::PtrGetN => {
            verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, inst.flags as u16)
        }
        Opcode::PtrAdd => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrAdd destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "PtrAdd pointer",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::Value],
                "PtrAdd offset",
            )
        }
        Opcode::PtrSet => verify_ptr_set_contract(func, pc, opcode, inst.a, inst.c, inst.flags),
        Opcode::PtrSetN => {
            let value_layout = ptr_value_layout(func, pc, opcode)?;
            if value_layout.len() != inst.flags as usize {
                return Err(ctx.call_shape_mismatch(format!(
                    "PtrSetN metadata layout slots {} do not match encoded count {}",
                    value_layout.len(),
                    inst.flags
                )));
            }
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrSetN pointer",
            )?;
            let source = local_layout(func, pc, inst.c, inst.flags as u16, "PtrSetN source")?;
            verify_local_layout_matches(func, pc, opcode, inst.c, &value_layout, "PtrSetN source")?;
            if source
                .iter()
                .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1))
            {
                return Err(JitMetadataError::SlotTypeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access: "PtrSetN source requires typed write barriers",
                    slot: inst.c,
                    expected: source
                        .iter()
                        .map(|st| match st {
                            SlotType::GcRef | SlotType::Interface1 => SlotType::Value,
                            other => *other,
                        })
                        .collect(),
                    actual: source.to_vec(),
                });
            }
            Ok(())
        }
        other => unreachable!("memory verifier received {other:?}"),
    }
}

fn ptr_value_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "PtrLayout",
        crate::metadata::ptr_value_layout_from_instruction,
    )
}

fn slot_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "SlotLayout",
        crate::metadata::slot_elem_layout_from_instruction,
    )
}

fn verify_ptr_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    ptr_slot: u16,
    src_slot: u16,
    flags: u8,
) -> Result<(), JitMetadataError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != 1 {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "PtrSet metadata layout slots {} do not match encoded count 1",
                value_layout.len()
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcRef],
        "PtrSet pointer",
    )?;
    let source = local_layout(func, pc, src_slot, 1, "PtrSet source")?;
    verify_raw_or_exact_layout_matches(func, pc, opcode, src_slot, &value_layout, "PtrSet source")?;
    let requires_barrier = matches!(source[0], SlotType::GcRef | SlotType::Interface1);
    let has_barrier = (flags & 1) != 0;
    if requires_barrier && !has_barrier {
        return Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access: "PtrSet missing write barrier",
            slot: src_slot,
            expected: vec![SlotType::GcRef],
            actual: source.to_vec(),
        });
    }
    Ok(())
}

fn verify_global_set_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    src_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let globals = flattened_global_slot_types(vo_module);
    let end =
        global_start
            .checked_add(count)
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: global_start,
                count,
                access: "global write",
            })? as usize;
    if end > globals.len() {
        return Err(JitMetadataError::GlobalSlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: global_start,
            global_slots: globals.len(),
            access: "write",
        });
    }
    let expected = &globals[global_start as usize..end];
    verify_structural_layout(func, pc, opcode, global_start, expected, "GlobalSet target")?;
    verify_local_layout_matches(func, pc, opcode, src_start, expected, "GlobalSet source")
}

fn verify_global_get_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    dst_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let globals = flattened_global_slot_types(vo_module);
    let end =
        global_start
            .checked_add(count)
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: global_start,
                count,
                access: "global read",
            })? as usize;
    if end > globals.len() {
        return Err(JitMetadataError::GlobalSlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: global_start,
            global_slots: globals.len(),
            access: "read",
        });
    }
    let expected = &globals[global_start as usize..end];
    verify_structural_layout(func, pc, opcode, global_start, expected, "GlobalGet source")?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        dst_start,
        expected,
        "GlobalGet destination",
    )
}

fn verify_ptr_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    ptr_slot: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "PtrGet metadata layout slots {} do not match encoded count {}",
                value_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcRef],
        "PtrGet pointer",
    )?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        dst_start,
        &value_layout,
        "PtrGet destination",
    )
}

fn verify_slot_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    base_start: u16,
    index_slot: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SlotGet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        index_slot,
        &[SlotType::Value],
        "SlotGet index",
    )?;
    if count == 1 {
        verify_raw_or_exact_layout_matches(
            func,
            pc,
            opcode,
            base_start,
            &elem_layout,
            "SlotGet element",
        )?;
        verify_raw_or_exact_layout_matches(
            func,
            pc,
            opcode,
            dst_start,
            &elem_layout,
            "SlotGet destination",
        )
    } else {
        verify_local_layout_matches(
            func,
            pc,
            opcode,
            base_start,
            &elem_layout,
            "SlotGet element",
        )?;
        verify_local_layout_matches(
            func,
            pc,
            opcode,
            dst_start,
            &elem_layout,
            "SlotGet destination",
        )
    }
}

fn verify_slot_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    base_start: u16,
    index_slot: u16,
    src_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SlotSet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        index_slot,
        &[SlotType::Value],
        "SlotSet index",
    )?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        base_start,
        &elem_layout,
        "SlotSet element",
    )?;
    verify_local_layout_matches(func, pc, opcode, src_start, &elem_layout, "SlotSet source")
}
