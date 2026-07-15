use vo_runtime::instruction::Instruction;

use crate::metadata;
use crate::semantics::{
    RegisterCondition, RegisterCount, RegisterEffectOperand, RegisterOperand, RegisterRangeStart,
};

use super::{EffectError, SlotRangeError, MAP_ITER_SLOTS};

pub(super) fn checked_slot_offset(
    start: u16,
    offset: u16,
    access: &'static str,
) -> Result<u16, SlotRangeError> {
    start
        .checked_add(offset)
        .ok_or_else(|| SlotRangeError::new(access, start, offset.saturating_add(1)))
}

pub(super) fn operand_slot(inst: &Instruction, operand: RegisterOperand) -> u16 {
    match operand {
        RegisterOperand::A => inst.a,
        RegisterOperand::B => inst.b,
        RegisterOperand::C => inst.c,
        RegisterOperand::Zero => 0,
    }
}

fn condition_matches(inst: &Instruction, condition: RegisterCondition) -> bool {
    match condition {
        RegisterCondition::FlagSet(mask) => (inst.flags & mask) != 0,
        RegisterCondition::FlagsEq(expected) => inst.flags == expected,
    }
}

fn register_count(
    inst: &Instruction,
    count: RegisterCount,
    access: &'static str,
) -> Result<u16, EffectError> {
    match count {
        RegisterCount::Fixed(slots) => Ok(slots),
        RegisterCount::OperandB => Ok(inst.b),
        RegisterCount::OperandC => Ok(inst.c),
        RegisterCount::Flags => Ok(inst.flags as u16),
        RegisterCount::CopyNCount => Ok(inst.copy_n_count()),
        RegisterCount::PackedArgSlots => Ok(inst.packed_arg_slots()),
        RegisterCount::PackedRetSlots => Ok(inst.packed_ret_slots()),
        RegisterCount::ElemSlotsFromFlags => {
            if inst.flags == 0 {
                Err(EffectError::missing_layout(inst.opcode(), "ElemLayout"))
            } else {
                Ok(slice_elem_slots_from_flags(inst.flags))
            }
        }
        RegisterCount::MapIterSlots => Ok(MAP_ITER_SLOTS),
        RegisterCount::MapIterKeyValueSlots => {
            let key_slots = inst.map_iter_key_slots();
            key_slots
                .checked_add(inst.map_iter_val_slots())
                .ok_or_else(|| SlotRangeError::new(access, inst.a, key_slots).into())
        }
    }
}

fn register_range_start(
    inst: &Instruction,
    start: RegisterRangeStart,
    access: &'static str,
) -> Result<u16, EffectError> {
    match start {
        RegisterRangeStart::Operand(operand) => Ok(operand_slot(inst, operand)),
        RegisterRangeStart::OperandOffset(operand, offset) => {
            checked_slot_offset(operand_slot(inst, operand), offset, access).map_err(Into::into)
        }
        RegisterRangeStart::BPlusPackedArgSlots => {
            checked_slot_offset(inst.b, inst.packed_arg_slots(), access).map_err(Into::into)
        }
        RegisterRangeStart::SliceAppendValueStart => {
            let offset = if inst.flags == 0 { 2 } else { 1 };
            checked_slot_offset(inst.c, offset, access).map_err(Into::into)
        }
    }
}

fn push_register_effect_operand(
    regs: &mut Vec<u16>,
    inst: &Instruction,
    operand: RegisterEffectOperand,
    access: &'static str,
) -> Result<(), EffectError> {
    match operand {
        RegisterEffectOperand::Slot(operand) => regs.push(operand_slot(inst, operand)),
        RegisterEffectOperand::SlotOffset(operand, offset) => {
            regs.push(checked_slot_offset(
                operand_slot(inst, operand),
                offset,
                access,
            )?);
        }
        RegisterEffectOperand::ConditionalSlot { condition, operand } => {
            if condition_matches(inst, condition) {
                regs.push(operand_slot(inst, operand));
            }
        }
        RegisterEffectOperand::ConditionalSlotOffset {
            condition,
            operand,
            offset,
        } => {
            if condition_matches(inst, condition) {
                regs.push(checked_slot_offset(
                    operand_slot(inst, operand),
                    offset,
                    access,
                )?);
            }
        }
        RegisterEffectOperand::Range { start, count } => {
            let start = register_range_start(inst, start, access)?;
            let count = register_count(inst, count, access)?;
            try_push_slot_range(regs, start, count, access)?;
        }
    }
    Ok(())
}

pub(super) fn push_register_effect_operands(
    regs: &mut Vec<u16>,
    inst: &Instruction,
    operands: &'static [RegisterEffectOperand],
    access: &'static str,
) -> Result<(), EffectError> {
    for &operand in operands {
        push_register_effect_operand(regs, inst, operand, access)?;
    }
    Ok(())
}

pub fn try_push_slot_range(
    regs: &mut Vec<u16>,
    start: u16,
    slots: u16,
    access: &'static str,
) -> Result<(), SlotRangeError> {
    if slots == 0 {
        return Ok(());
    }
    let last_offset = slots - 1;
    start
        .checked_add(last_offset)
        .ok_or_else(|| SlotRangeError::new(access, start, slots))?;
    for i in 0..slots {
        regs.push(start + i);
    }
    Ok(())
}

pub fn slice_elem_slots_from_flags(flags: u8) -> u16 {
    assert_ne!(
        flags, 0,
        "dynamic element layouts must use per-instruction metadata"
    );
    metadata::elem_layout_from_flags(flags).slots
}
