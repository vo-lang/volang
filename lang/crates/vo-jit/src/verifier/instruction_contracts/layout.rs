use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::{Constant, FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

pub(crate) const RAW_I64_SLOTS: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
];
pub(crate) const ANY_SINGLE_SLOT: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
    SlotType::Float,
];
pub(crate) const FLOAT_STORAGE_SLOTS: &[SlotType] = &[SlotType::Float, SlotType::Value];

pub(crate) fn constant_at<'a>(
    func: &FunctionDef,
    vo_module: &'a VoModule,
    pc: usize,
    const_id: u16,
) -> Result<&'a Constant, JitMetadataError> {
    vo_module
        .constants
        .get(const_id as usize)
        .ok_or_else(|| JitMetadataError::MissingConstant {
            func: func.name.clone(),
            pc,
            const_id,
        })
}

pub(crate) fn constant_kind(constant: &Constant) -> &'static str {
    match constant {
        Constant::Nil => "Nil",
        Constant::Bool(_) => "Bool",
        Constant::Int(_) => "Int",
        Constant::Float(_) => "Float",
        Constant::String(_) => "String",
    }
}

pub(crate) fn decode_metadata_layout<T>(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    layout: &'static str,
    decode: impl FnOnce(&JitInstructionMetadata) -> Option<T>,
) -> Result<T, JitMetadataError> {
    func.jit_metadata
        .get(pc)
        .and_then(decode)
        .ok_or_else(|| JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout,
        })
}

pub(crate) fn checked_slot_offset_for_verifier(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    offset: u16,
    access: &'static str,
) -> Result<u16, JitMetadataError> {
    start
        .checked_add(offset)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count: offset.saturating_add(1),
            access,
        })
}

pub(crate) fn forloop_target_i64(pc: usize, offset: i16) -> i64 {
    pc as i64 + 1 + i64::from(offset)
}

pub(crate) fn verify_interface_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1] {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidInterfaceLayout {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot,
            actual: actual.to_vec(),
        })
    }
}

pub(crate) fn verify_value_range(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        start,
        &vec![SlotType::Value; count as usize],
        access,
    )
}

pub(crate) fn verify_local_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    verify_structural_layout(func, pc, opcode, start, actual, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

pub(crate) fn verify_raw_or_exact_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        verify_structural_layout(func, pc, opcode, start, actual, access)?;
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

pub(crate) fn verify_structural_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    layout: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let mut i = 0usize;
    while i < layout.len() {
        match layout[i] {
            SlotType::Interface0 => {
                if layout.get(i + 1) != Some(&SlotType::Interface1) {
                    return Err(JitMetadataError::InvalidInterfaceLayout {
                        func: func.name.clone(),
                        pc,
                        opcode,
                        access,
                        slot: start + i as u16,
                        actual: layout[i..(i + 1).min(layout.len())].to_vec(),
                    });
                }
                i += 2;
            }
            SlotType::Interface1 => {
                return Err(JitMetadataError::InvalidInterfaceLayout {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access,
                    slot: start + i as u16,
                    actual: vec![SlotType::Interface1],
                });
            }
            _ => i += 1,
        }
    }
    Ok(())
}

pub(crate) fn verify_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

pub(crate) fn verify_one_of_single_slot_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected_any: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, 1, access)?;
    if expected_any.contains(&actual[0]) {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected_any.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

pub(crate) fn local_layout<'a>(
    func: &'a FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<&'a [SlotType], JitMetadataError> {
    verify_range(func, pc, start, count, access)?;
    let start = start as usize;
    let end = start + count as usize;
    Ok(&func.slot_types[start..end])
}

pub(crate) fn verify_range(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if count == 0 {
        return Ok(());
    }
    let end = start
        .checked_add(count - 1)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count,
            access,
        })?;
    if end >= func.local_slots || end as usize >= func.slot_types.len() {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: end,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}

pub(crate) fn flattened_global_slot_types(vo_module: &VoModule) -> Vec<SlotType> {
    vo_module
        .globals
        .iter()
        .flat_map(|global| global.slot_types.iter().copied())
        .collect()
}

pub(crate) fn verify_slot(
    func: &FunctionDef,
    pc: usize,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if slot >= func.local_slots {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}
