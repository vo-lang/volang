use super::super::{
    checked_slot_offset_for_verifier, decode_metadata_layout, verify_layout,
    verify_local_layout_matches, verify_value_range,
};
use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

pub(super) fn verify_array_new_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "ArrayNew destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "ArrayNew metadata",
    )?;
    verify_value_range(
        func,
        pc,
        opcode,
        inst.c,
        if inst.flags == 0 { 2 } else { 1 },
        "ArrayNew length/elem_bytes",
    )
}

pub(super) fn verify_array_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Array source")?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Array index")?;
    verify_indexed_layout_contract(func, pc, opcode, inst, inst.a, "ArrayGet destination")
}

pub(super) fn verify_array_addr_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "ArrayAddr destination",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Array source")?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Array index")
}

pub(super) fn verify_array_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "ArraySet target",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "ArraySet index",
    )?;
    verify_indexed_layout_contract(func, pc, opcode, inst, inst.c, "ArraySet source")
}

pub(super) fn verify_slice_new_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "SliceNew destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "SliceNew metadata",
    )?;
    verify_value_range(
        func,
        pc,
        opcode,
        inst.c,
        if inst.flags == 0 { 3 } else { 2 },
        "SliceNew len/cap/elem_bytes",
    )
}

pub(super) fn verify_slice_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Slice index")?;
    verify_indexed_layout_contract(func, pc, opcode, inst, inst.a, "SliceGet destination")
}

pub(super) fn verify_slice_addr_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "SliceAddr destination",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Slice index")
}

pub(super) fn verify_slice_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "SliceSet target",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "SliceSet index",
    )?;
    verify_indexed_layout_contract(func, pc, opcode, inst, inst.c, "SliceSet source")
}

pub(super) fn verify_slice_append_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "SliceAppend destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcRef],
        "SliceAppend source",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "SliceAppend metadata",
    )?;
    let value_start = checked_slot_offset_for_verifier(
        func,
        pc,
        inst.c,
        if inst.flags == 0 { 2 } else { 1 },
        "SliceAppend value",
    )?;
    verify_indexed_layout_contract(func, pc, opcode, inst, value_start, "SliceAppend value")
}

pub(super) fn verify_slice_len_or_cap_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::Value],
        "slice length/capacity result",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")
}

pub(super) fn verify_slice_slice_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "SliceSlice destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcRef],
        "SliceSlice source",
    )?;
    let bound_slots = if (inst.flags & 0b10) != 0 { 3 } else { 2 };
    verify_value_range(func, pc, opcode, inst.c, bound_slots, "SliceSlice bounds")
}

fn verify_indexed_layout_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    start: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let layout = indexed_elem_layout(func, pc, opcode, inst)?;
    if layout.is_empty() {
        return Ok(());
    }
    verify_local_layout_matches(func, pc, opcode, start, &layout, access)
}

fn indexed_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<Vec<SlotType>, JitMetadataError> {
    let layout = decode_metadata_layout(
        func,
        pc,
        opcode,
        "ElemLayout",
        crate::metadata::elem_layout_from_instruction,
    )?;
    debug_assert_eq!(layout.slot_layout.len(), layout.slots as usize);
    let from_flags = if inst.flags == 0 {
        None
    } else {
        Some(crate::metadata::elem_layout_from_flags(inst.flags))
    };
    if let Some(from_flags) = from_flags {
        if from_flags.bytes != layout.bytes
            || from_flags.needs_sign_extend != layout.needs_sign_extend
        {
            return Err(JitMetadataError::InconsistentElemLayout {
                func: func.name.clone(),
                pc,
                flags: inst.flags,
            });
        }
    }
    Ok(layout.slot_layout)
}
