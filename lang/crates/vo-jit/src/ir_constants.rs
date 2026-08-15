use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode};

fn const_to_i64(constant: &Constant) -> Option<i64> {
    match constant {
        Constant::Nil => Some(0),
        Constant::Bool(value) => Some(*value as i64),
        Constant::Int(value) => Some(*value),
        Constant::Float(value) => Some(value.to_bits() as i64),
        Constant::String(_) => None,
    }
}

fn binary_const(
    facts: &mut impl FnMut(u16) -> Option<i64>,
    instruction: &Instruction,
    operation: impl FnOnce(i64, i64) -> Option<i64>,
) -> Option<i64> {
    let lhs = facts(instruction.b)?;
    let rhs = facts(instruction.c)?;
    operation(lhs, rhs)
}

fn unary_const(
    facts: &mut impl FnMut(u16) -> Option<i64>,
    instruction: &Instruction,
    operation: impl FnOnce(i64) -> Option<i64>,
) -> Option<i64> {
    operation(facts(instruction.b)?)
}

pub(crate) fn single_slot_result(
    instruction: &Instruction,
    constants: &[Constant],
    mut fact: impl FnMut(u16) -> Option<i64>,
) -> Option<Option<i64>> {
    match instruction.opcode() {
        Opcode::LoadInt => Some(Some(instruction.imm32() as i64)),
        Opcode::LoadConst => Some(constants.get(instruction.b as usize).and_then(const_to_i64)),
        Opcode::Copy => Some(fact(instruction.b)),
        Opcode::AddI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(lhs.wrapping_add(rhs))
        })),
        Opcode::SubI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(lhs.wrapping_sub(rhs))
        })),
        Opcode::MulI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(lhs.wrapping_mul(rhs))
        })),
        Opcode::DivI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            (rhs != 0).then(|| lhs.wrapping_div(rhs))
        })),
        Opcode::DivU => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            let rhs = rhs as u64;
            (rhs != 0).then(|| (lhs as u64).wrapping_div(rhs) as i64)
        })),
        Opcode::ModI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            (rhs != 0).then(|| lhs.wrapping_rem(rhs))
        })),
        Opcode::ModU => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            let rhs = rhs as u64;
            (rhs != 0).then(|| (lhs as u64).wrapping_rem(rhs) as i64)
        })),
        Opcode::NegI => Some(unary_const(&mut fact, instruction, |value| {
            Some(value.wrapping_neg())
        })),
        Opcode::EqI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some((lhs == rhs) as i64)
        })),
        Opcode::NeI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some((lhs != rhs) as i64)
        })),
        Opcode::LtI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some((lhs < rhs) as i64)
        })),
        Opcode::LeI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some((lhs <= rhs) as i64)
        })),
        Opcode::GtI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some((lhs > rhs) as i64)
        })),
        Opcode::GeI => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some((lhs >= rhs) as i64)
        })),
        Opcode::LtU => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(((lhs as u64) < (rhs as u64)) as i64)
        })),
        Opcode::LeU => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(((lhs as u64) <= (rhs as u64)) as i64)
        })),
        Opcode::GtU => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(((lhs as u64) > (rhs as u64)) as i64)
        })),
        Opcode::GeU => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(((lhs as u64) >= (rhs as u64)) as i64)
        })),
        Opcode::And => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(lhs & rhs)
        })),
        Opcode::Or => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(lhs | rhs)
        })),
        Opcode::Xor => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(lhs ^ rhs)
        })),
        Opcode::AndNot => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            Some(lhs & !rhs)
        })),
        Opcode::Not => Some(unary_const(&mut fact, instruction, |value| Some(!value))),
        Opcode::BoolNot => Some(unary_const(&mut fact, instruction, |value| {
            Some((value == 0) as i64)
        })),
        Opcode::Shl => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            fold_shift(instruction, lhs, rhs, |lhs, rhs| {
                (lhs as u64).wrapping_shl(rhs) as i64
            })
        })),
        Opcode::ShrS => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            if shift_is_invalid(instruction, rhs) {
                None
            } else if (rhs as u64) >= 64 {
                Some(if lhs < 0 { -1 } else { 0 })
            } else {
                Some(lhs >> rhs as u32)
            }
        })),
        Opcode::ShrU => Some(binary_const(&mut fact, instruction, |lhs, rhs| {
            fold_shift(instruction, lhs, rhs, |lhs, rhs| {
                (lhs as u64).wrapping_shr(rhs) as i64
            })
        })),
        _ => None,
    }
}

fn shift_is_invalid(instruction: &Instruction, rhs: i64) -> bool {
    let rhs_unsigned = instruction.flags & vo_runtime::instruction::SHIFT_FLAG_RHS_UNSIGNED != 0;
    !rhs_unsigned && rhs < 0
}

fn fold_shift(
    instruction: &Instruction,
    lhs: i64,
    rhs: i64,
    operation: impl FnOnce(i64, u32) -> i64,
) -> Option<i64> {
    if shift_is_invalid(instruction, rhs) {
        None
    } else if (rhs as u64) >= 64 {
        Some(0)
    } else {
        Some(operation(lhs, rhs as u32))
    }
}
