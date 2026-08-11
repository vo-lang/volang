use vo_runtime::instruction::Instruction;

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
    _access: &'static str,
) -> Result<u16, EffectError> {
    match count {
        RegisterCount::OperandB => Ok(inst.b),
        RegisterCount::Flags => Ok(inst.flags as u16),
        RegisterCount::CopyNCount => Ok(inst.copy_n_count()),
        RegisterCount::MapIterSlots => Ok(MAP_ITER_SLOTS),
    }
}

fn register_range_start(
    inst: &Instruction,
    start: RegisterRangeStart,
    _access: &'static str,
) -> Result<u16, EffectError> {
    match start {
        RegisterRangeStart::Operand(operand) => Ok(operand_slot(inst, operand)),
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
