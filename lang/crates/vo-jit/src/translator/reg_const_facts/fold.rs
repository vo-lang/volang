use std::collections::HashMap;

use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode};

fn const_to_i64(constant: &Constant) -> Option<i64> {
    match constant {
        Constant::Nil => Some(0),
        Constant::Bool(v) => Some(*v as i64),
        Constant::Int(v) => Some(*v),
        Constant::Float(v) => Some(v.to_bits() as i64),
        Constant::String(_) => None,
    }
}

fn binary_const(
    facts: &HashMap<u16, i64>,
    inst: &Instruction,
    op: impl FnOnce(i64, i64) -> Option<i64>,
) -> Option<i64> {
    let lhs = facts.get(&inst.b).copied()?;
    let rhs = facts.get(&inst.c).copied()?;
    op(lhs, rhs)
}

fn unary_const(
    facts: &HashMap<u16, i64>,
    inst: &Instruction,
    op: impl FnOnce(i64) -> Option<i64>,
) -> Option<i64> {
    let value = facts.get(&inst.b).copied()?;
    op(value)
}

pub(super) fn single_slot_const_result(
    inst: &Instruction,
    constants: &[Constant],
    facts: &HashMap<u16, i64>,
) -> Option<Option<i64>> {
    match inst.opcode() {
        Opcode::LoadInt => Some(Some(inst.imm32() as i64)),
        Opcode::LoadConst => Some(constants.get(inst.b as usize).and_then(const_to_i64)),
        Opcode::Copy => Some(facts.get(&inst.b).copied()),
        Opcode::AddI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(lhs.wrapping_add(rhs))
        })),
        Opcode::SubI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(lhs.wrapping_sub(rhs))
        })),
        Opcode::MulI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(lhs.wrapping_mul(rhs))
        })),
        Opcode::DivI => Some(binary_const(facts, inst, |lhs, rhs| {
            (rhs != 0).then(|| lhs.wrapping_div(rhs))
        })),
        Opcode::DivU => Some(binary_const(facts, inst, |lhs, rhs| {
            let rhs = rhs as u64;
            (rhs != 0).then(|| (lhs as u64).wrapping_div(rhs) as i64)
        })),
        Opcode::ModI => Some(binary_const(facts, inst, |lhs, rhs| {
            (rhs != 0).then(|| lhs.wrapping_rem(rhs))
        })),
        Opcode::ModU => Some(binary_const(facts, inst, |lhs, rhs| {
            let rhs = rhs as u64;
            (rhs != 0).then(|| (lhs as u64).wrapping_rem(rhs) as i64)
        })),
        Opcode::NegI => Some(unary_const(facts, inst, |value| Some(value.wrapping_neg()))),
        Opcode::EqI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs == rhs) as i64)
        })),
        Opcode::NeI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs != rhs) as i64)
        })),
        Opcode::LtI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs < rhs) as i64)
        })),
        Opcode::LeI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs <= rhs) as i64)
        })),
        Opcode::GtI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs > rhs) as i64)
        })),
        Opcode::GeI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs >= rhs) as i64)
        })),
        Opcode::LtU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) < (rhs as u64)) as i64)
        })),
        Opcode::LeU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) <= (rhs as u64)) as i64)
        })),
        Opcode::GtU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) > (rhs as u64)) as i64)
        })),
        Opcode::GeU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) >= (rhs as u64)) as i64)
        })),
        Opcode::And => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs & rhs))),
        Opcode::Or => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs | rhs))),
        Opcode::Xor => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs ^ rhs))),
        Opcode::AndNot => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs & !rhs))),
        Opcode::Not => Some(unary_const(facts, inst, |value| Some(!value))),
        Opcode::BoolNot => Some(unary_const(facts, inst, |value| Some((value == 0) as i64))),
        Opcode::Shl => Some(binary_const(facts, inst, |lhs, rhs| {
            let rhs_unsigned = inst.flags & vo_runtime::instruction::SHIFT_FLAG_RHS_UNSIGNED != 0;
            if !rhs_unsigned && rhs < 0 {
                None
            } else if (rhs as u64) >= 64 {
                Some(0)
            } else {
                Some((lhs as u64).wrapping_shl(rhs as u32) as i64)
            }
        })),
        Opcode::ShrS => Some(binary_const(facts, inst, |lhs, rhs| {
            let rhs_unsigned = inst.flags & vo_runtime::instruction::SHIFT_FLAG_RHS_UNSIGNED != 0;
            if !rhs_unsigned && rhs < 0 {
                None
            } else if (rhs as u64) >= 64 {
                Some(if lhs < 0 { -1 } else { 0 })
            } else {
                Some(lhs >> rhs as u32)
            }
        })),
        Opcode::ShrU => Some(binary_const(facts, inst, |lhs, rhs| {
            let rhs_unsigned = inst.flags & vo_runtime::instruction::SHIFT_FLAG_RHS_UNSIGNED != 0;
            if !rhs_unsigned && rhs < 0 {
                None
            } else if (rhs as u64) >= 64 {
                Some(0)
            } else {
                Some((lhs as u64).wrapping_shr(rhs as u32) as i64)
            }
        })),
        _ => None,
    }
}
