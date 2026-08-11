use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};

use crate::metadata;
use crate::semantics::{opcode_register_effects, DynamicRegisterReadEffect};

use super::operand_eval::{checked_slot_offset, try_push_slot_range};
use super::{EffectError, EffectFacts, MapGetLayout, MapSetLayout};

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

fn required_ptr_value_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    metadata::ptr_value_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "PtrLayout"))
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
            let value_slots = required_indexed_set_value_slots(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, value_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::SliceAppendValueLayout => {
            let value_slots = required_slice_append_value_slots(inst, facts)?;
            regs.push(inst.b);
            regs.push(inst.c);
            let value_start = checked_slot_offset(inst.c, 1, "read")?;
            try_push_slot_range(&mut regs, value_start, value_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapGetLayout => {
            let layout = required_map_get_layout(inst, facts)?;
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, layout.key_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapSetLayout => {
            let layout = required_map_set_layout(inst, facts)?;
            regs.push(inst.a);
            try_push_slot_range(&mut regs, inst.b, layout.key_slots, "read")?;
            try_push_slot_range(&mut regs, inst.c, layout.val_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapDeleteLayout => {
            let key_slots = required_map_delete_key_slots(inst, facts)?;
            regs.push(inst.a);
            try_push_slot_range(&mut regs, inst.b, key_slots, "read")?;
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
        DynamicRegisterReadEffect::PtrSetLayout => {
            let value_slots = required_ptr_value_slots(inst, facts)?;
            regs.push(inst.a);
            try_push_slot_range(&mut regs, inst.c, value_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::SharedCall => {
            if inst.call_shape_is_closure() {
                let (arg_slots, _) = required_call_layout_slots(inst, facts)?;
                regs.push(inst.a);
                try_push_slot_range(&mut regs, inst.b, arg_slots, "read")?;
            } else {
                let func_id = inst.call_shape_static_func_id();
                let callee = functions
                    .get(func_id as usize)
                    .ok_or_else(|| EffectError::missing_function(func_id))?;
                try_push_slot_range(&mut regs, inst.b, callee.param_slots, "read")?;
            }
            Ok(Some(regs))
        }
    }
}
