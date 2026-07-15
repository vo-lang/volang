use vo_runtime::bytecode::{ExternDef, FunctionDef};
use vo_runtime::instruction::{Instruction, Opcode};

use crate::metadata;
use crate::semantics::{
    opcode_register_effects, DynamicRegisterReadEffect, DynamicRegisterWriteEffect,
};

use super::operand_eval::{checked_slot_offset, try_push_slot_range};
use super::{
    EffectError, EffectFacts, MapGetLayout, MapIterNextLayout, MapSetLayout, SlotRangeError,
    MAP_ITER_SLOTS,
};

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

fn required_map_iter_next_layout(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<MapIterNextLayout, EffectError> {
    map_iter_next_layout(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "MapIterNext"))
}

fn required_queue_elem_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    metadata::queue_elem_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "QueueLayout"))
}

fn required_call_layout_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<(u16, u16), EffectError> {
    facts
        .call_layout_slots()
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "call layout"))
}

fn required_slot_elem_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    metadata::slot_elem_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "SlotLayout"))
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

pub fn map_iter_next_layout(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Option<MapIterNextLayout> {
    metadata::map_iter_next_layout(inst, facts)
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
            let func_id = inst.static_call_func_id();
            let callee = functions
                .get(func_id as usize)
                .ok_or_else(|| EffectError::missing_function(func_id))?;
            try_push_slot_range(&mut regs, inst.b, callee.param_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::CallLayout => {
            let (arg_slots, _) = required_call_layout_slots(inst, facts)?;
            match inst.opcode() {
                Opcode::CallClosure => {
                    regs.push(inst.a);
                    try_push_slot_range(&mut regs, inst.b, arg_slots, "read")?;
                }
                Opcode::CallIface => {
                    regs.push(inst.a);
                    regs.push(checked_slot_offset(inst.a, 1, "read")?);
                    try_push_slot_range(&mut regs, inst.b, arg_slots, "read")?;
                }
                Opcode::CallExtern => {
                    try_push_slot_range(&mut regs, inst.c, arg_slots, "read")?;
                }
                Opcode::GoIsland => {
                    regs.push(inst.a);
                    regs.push(inst.b);
                    try_push_slot_range(&mut regs, inst.c, arg_slots, "read")?;
                }
                _ => return Err(EffectError::missing_layout(inst.opcode(), "call layout")),
            }
            Ok(Some(regs))
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
        DynamicRegisterReadEffect::QueueSendLayout => {
            let elem_slots = required_queue_elem_slots(inst, facts)?;
            regs.push(inst.a);
            try_push_slot_range(&mut regs, inst.b, elem_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::SlotSetLayout => {
            let elem_slots = required_slot_elem_slots(inst, facts)?;
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, elem_slots, "read")?;
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
            let func_id = inst.static_call_func_id();
            let callee = functions
                .get(func_id as usize)
                .ok_or_else(|| EffectError::missing_function(func_id))?;
            let ret_start = checked_slot_offset(inst.b, callee.param_slots, "write")?;
            try_push_slot_range(&mut regs, ret_start, callee.ret_slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::CallLayout => {
            let (arg_slots, ret_slots) = required_call_layout_slots(inst, facts)?;
            let ret_start = checked_slot_offset(inst.b, arg_slots, "write")?;
            try_push_slot_range(&mut regs, ret_start, ret_slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::ExternSignature => {
            let ret_slots = externs
                .get(inst.b as usize)
                .map(|extern_def| extern_def.returns.slots)
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
        DynamicRegisterWriteEffect::MapIterNextLayout => {
            let layout = required_map_iter_next_layout(inst, facts)?;
            try_push_slot_range(&mut regs, inst.b, MAP_ITER_SLOTS, "write")?;
            let slots = layout
                .key_slots
                .checked_add(layout.val_slots)
                .ok_or_else(|| {
                    SlotRangeError::new("write", inst.a, layout.key_slots.saturating_add(1))
                })?;
            try_push_slot_range(&mut regs, inst.a, slots, "write")?;
            regs.push(inst.c);
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::QueueRecvLayout => {
            let elem_slots = required_queue_elem_slots(inst, facts)?;
            let result_slots = elem_slots
                .checked_add(u16::from(inst.recv_has_ok()))
                .ok_or_else(|| SlotRangeError::new("write", inst.a, elem_slots))?;
            try_push_slot_range(&mut regs, inst.a, result_slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::SlotGetLayout => {
            let elem_slots = required_slot_elem_slots(inst, facts)?;
            try_push_slot_range(&mut regs, inst.a, elem_slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::IfaceAssertLayout => {
            let layout = metadata::iface_assert_layout(inst, facts)
                .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "IfaceAssertLayout"))?;
            let result_slots = layout
                .result_slots
                .checked_add(u16::from(((inst.flags >> 2) & 1) != 0))
                .ok_or_else(|| SlotRangeError::new("write", inst.a, layout.result_slots))?;
            try_push_slot_range(&mut regs, inst.a, result_slots, "write")?;
            Ok(Some(regs))
        }
    }
}
