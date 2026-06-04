use super::super::{
    checked_slot_offset_for_verifier, decode_metadata_layout, verify_layout,
    verify_local_layout_matches, verify_value_range,
};
use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

pub(super) fn verify_map_new_contract(
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
        "MapNew destination",
    )?;
    verify_value_range(func, pc, opcode, inst.b, 2, "MapNew metadata")
}

pub(super) fn verify_map_get_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapGet;
    let layout = map_get_layout(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "MapGet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapGet metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.c, 1, "MapGet key")?;
    verify_layout(
        func,
        pc,
        opcode,
        key_start,
        &layout.key_layout,
        "MapGet key",
    )?;
    let output_slots =
        layout
            .output_slots()
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: inst.a,
                count: layout.val_slots,
                access: "MapGet destination",
            })?;
    let mut output_layout = layout.val_layout.clone();
    if layout.has_ok {
        output_layout.push(SlotType::Value);
    }
    debug_assert_eq!(output_layout.len(), output_slots as usize);
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &output_layout,
        "MapGet destination",
    )
}

pub(super) fn verify_map_set_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapSet;
    let layout = map_set_layout(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], "MapSet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapSet metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.b, 1, "MapSet key")?;
    verify_layout(
        func,
        pc,
        opcode,
        key_start,
        &layout.key_layout,
        "MapSet key",
    )?;
    verify_layout(func, pc, opcode, inst.c, &layout.val_layout, "MapSet value")
}

pub(super) fn verify_map_delete_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapDelete;
    let key_layout = map_delete_key_layout(func, pc, opcode)?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "MapDelete map",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapDelete metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.b, 1, "MapDelete key")?;
    verify_layout(func, pc, opcode, key_start, &key_layout, "MapDelete key")
}

pub(super) fn verify_map_len_contract(
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
        "MapLen destination",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "map")
}

pub(super) fn verify_map_iter_init_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.a,
        &vo_runtime::objects::map::MAP_ITER_SLOT_TYPES,
        "MapIterInit iterator",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "map")
}

pub(super) fn verify_map_iter_next_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.b,
        &vo_runtime::objects::map::MAP_ITER_SLOT_TYPES,
        "MapIterNext iterator",
    )?;
    let key_slots = inst.map_iter_key_slots();
    let val_slots = inst.map_iter_val_slots();
    let (key_layout, val_layout) = map_iter_layout(func, pc, opcode)?;
    if key_layout.len() != key_slots as usize || val_layout.len() != val_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "MapIterNext metadata layout slots key={} value={} do not match encoded key={} value={}",
                key_layout.len(),
                val_layout.len(),
                key_slots,
                val_slots
            ),
        });
    }
    let kv_slots = key_slots
        .checked_add(val_slots)
        .ok_or(JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.a,
            count: key_slots,
            access: "MapIterNext key/value",
        })?;
    let mut kv_layout = key_layout;
    kv_layout.extend(val_layout);
    debug_assert_eq!(kv_layout.len(), kv_slots as usize);
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.a,
        &kv_layout,
        "MapIterNext key/value",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapIterNext ok",
    )
}

fn map_get_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<crate::metadata::MapGetLayout, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapGet",
        crate::metadata::map_get_layout_from_instruction,
    )
}

fn map_set_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<crate::metadata::MapSetLayout, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapSet",
        crate::metadata::map_set_layout_from_instruction,
    )
}

fn map_delete_key_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapDelete",
        crate::metadata::map_delete_key_layout_from_instruction,
    )
}

fn map_iter_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapIterNext",
        crate::metadata::map_iter_next_layout_from_instruction,
    )
}
