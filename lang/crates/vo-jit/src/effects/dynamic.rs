use vo_runtime::bytecode::{ExternDef, FunctionDef};
use vo_runtime::instruction::Instruction;

use crate::metadata;
use crate::semantics::{
    opcode_register_effects, DynamicRegisterReadEffect, DynamicRegisterWriteEffect,
};

use super::operand_eval::{checked_slot_offset, try_push_slot_range};
use super::{EffectError, EffectFacts, MapGetLayout, MapSetLayout, SlotRangeError};

pub(super) fn required_indexed_get_result_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    indexed_get_result_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "ElemLayout"))
}

fn required_indexed_set_value_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    indexed_set_value_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "ElemLayout"))
}

fn required_slice_append_value_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    slice_append_value_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "ElemLayout"))
}

fn required_map_get_layout(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<MapGetLayout, EffectError> {
    map_get_layout(inst, facts).ok_or_else(|| EffectError::missing_layout(inst.opcode(), "MapGet"))
}

fn required_map_set_layout(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<MapSetLayout, EffectError> {
    map_set_layout(inst, facts).ok_or_else(|| EffectError::missing_layout(inst.opcode(), "MapSet"))
}

fn required_map_delete_key_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    map_delete_key_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "MapDelete"))
}

pub fn indexed_get_result_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::indexed_get_result_slots(inst, facts)
}

pub fn indexed_set_value_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::indexed_set_value_slots(inst, facts)
}

pub fn slice_append_value_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::slice_append_value_slots(inst, facts)
}

pub fn map_get_layout(inst: &Instruction, facts: EffectFacts<'_>) -> Option<MapGetLayout> {
    metadata::map_get_layout(inst, facts)
}

pub fn map_set_layout(inst: &Instruction, facts: EffectFacts<'_>) -> Option<MapSetLayout> {
    metadata::map_set_layout(inst, facts)
}

pub fn map_delete_key_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::map_delete_key_slots(inst, facts)
}

pub(super) fn try_dynamic_read_regs(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    functions: &[FunctionDef],
) -> Result<Option<Vec<u16>>, EffectError> {
    let dynamic = opcode_register_effects(inst.opcode()).dynamic_reads;
    let mut regs = Vec::new();
    match dynamic {
        DynamicRegisterReadEffect::None => Ok(None),
        DynamicRegisterReadEffect::StaticCallSignature => {
            if let Some(callee) = functions.get(inst.static_call_func_id() as usize) {
                try_push_slot_range(&mut regs, inst.b, callee.param_slots, "read")?;
                Ok(Some(regs))
            } else {
                Ok(None)
            }
        }
        DynamicRegisterReadEffect::IndexedSetValueLayout => {
            if !facts.has_facts() {
                return Ok(None);
            }
            let value_slots = required_indexed_set_value_slots(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, value_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::SliceAppendValueLayout => {
            if !facts.has_facts() {
                return Ok(None);
            }
            let value_slots = required_slice_append_value_slots(inst, facts)?;
            regs.push(inst.b);
            regs.push(inst.c);
            let value_start =
                checked_slot_offset(inst.c, if inst.flags == 0 { 2 } else { 1 }, "read")?;
            try_push_slot_range(&mut regs, value_start, value_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapGetLayout => {
            let layout = required_map_get_layout(inst, facts)?;
            regs.push(inst.b);
            regs.push(inst.c);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.c, 1, "read")?,
                layout.key_slots,
                "read",
            )?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapSetLayout => {
            let layout = required_map_set_layout(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.b, 1, "read")?,
                layout.key_slots,
                "read",
            )?;
            try_push_slot_range(&mut regs, inst.c, layout.val_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapDeleteLayout => {
            let key_slots = required_map_delete_key_slots(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.b, 1, "read")?,
                key_slots,
                "read",
            )?;
            Ok(Some(regs))
        }
    }
}

pub(super) fn try_dynamic_multi_write_regs(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<Option<Vec<u16>>, EffectError> {
    let dynamic = opcode_register_effects(inst.opcode()).dynamic_writes;
    let mut regs = Vec::new();
    match dynamic {
        DynamicRegisterWriteEffect::None => Ok(None),
        DynamicRegisterWriteEffect::StaticCallSignature => {
            if let Some(callee) = functions.get(inst.static_call_func_id() as usize) {
                let ret_start = checked_slot_offset(inst.b, callee.param_slots, "write")?;
                try_push_slot_range(&mut regs, ret_start, callee.ret_slots, "write")?;
                Ok(Some(regs))
            } else {
                Ok(None)
            }
        }
        DynamicRegisterWriteEffect::ExternSignature => {
            if externs.is_empty() {
                return Ok(None);
            }
            let ret_slots = externs
                .get(inst.b as usize)
                .map(|extern_def| extern_def.ret_slots)
                .ok_or_else(|| EffectError::missing_extern(inst.b))?;
            try_push_slot_range(&mut regs, inst.a, ret_slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::IndexedGetResultLayout => {
            if !facts.has_facts() {
                return Ok(None);
            }
            let slots = required_indexed_get_result_slots(inst, facts)?;
            try_push_slot_range(&mut regs, inst.a, slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::MapGetLayout => {
            let layout = required_map_get_layout(inst, facts)?;
            let slots = layout
                .output_slots()
                .ok_or_else(|| SlotRangeError::new("write", inst.a, layout.val_slots))?;
            try_push_slot_range(&mut regs, inst.a, slots, "write")?;
            Ok(Some(regs))
        }
    }
}
