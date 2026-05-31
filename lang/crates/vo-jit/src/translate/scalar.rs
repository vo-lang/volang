#![allow(unused_imports)]

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value,
};
use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode, QUEUE_KIND_PORT_FLAG};
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, HelperCallEffect, IrEmitter,
};
use crate::JitError;

use super::emit_runtime_trap_if;

pub(super) fn load_int<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let val = e.builder().ins().iconst(types::I64, inst.imm32() as i64);
    e.write_var(inst.a, val);
    e.set_reg_const(inst.a, inst.imm32() as i64);
}

pub(super) fn load_const<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let const_idx = inst.b as usize;
    let constant = e
        .vo_module()
        .constants
        .get(const_idx)
        .cloned()
        .ok_or_else(|| {
            JitError::Internal(format!(
                "LoadConst constant index {const_idx} missing at pc {}",
                e.current_pc()
            ))
        })?;
    match constant {
        Constant::Float(f) => {
            // Float constant: load as F64 directly, no bitcast needed
            let v = e.builder().ins().f64const(f);
            e.write_var_f64(inst.a, v);
            e.set_reg_const(inst.a, f.to_bits() as i64);
        }
        Constant::String(_) => {
            return Err(JitError::Internal(format!(
                "LoadConst at pc {} cannot load String constants; use StrNew",
                e.current_pc()
            )));
        }
        Constant::Nil => {
            let v = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, v);
            e.set_reg_const(inst.a, 0);
        }
        Constant::Bool(b) => {
            let val = b as i64;
            let v = e.builder().ins().iconst(types::I64, val);
            e.write_var(inst.a, v);
            e.set_reg_const(inst.a, val);
        }
        Constant::Int(i) => {
            let val = i;
            let v = e.builder().ins().iconst(types::I64, val);
            e.write_var(inst.a, v);
            e.set_reg_const(inst.a, val);
        }
    }
    Ok(())
}

pub(super) fn copy<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let const_val = e.get_reg_const(inst.b);
    let v = e.read_var(inst.b);
    e.write_var(inst.a, v);
    if let Some(c) = const_val {
        e.set_reg_const(inst.a, c);
    }
}

pub(super) fn copy_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let count = inst.copy_n_count();
    let mut values = Vec::with_capacity(count as usize);
    for i in 0..count {
        values.push((e.read_var(inst.b + i), e.get_reg_const(inst.b + i)));
    }
    for (i, (v, const_val)) in values.into_iter().enumerate() {
        e.write_var(inst.a + i as u16, v);
        if let Some(c) = const_val {
            e.set_reg_const(inst.a + i as u16, c);
        }
    }
}

pub(super) fn add_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let r = e.builder().ins().iadd(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn sub_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let r = e.builder().ins().isub(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn mul_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let r = e.builder().ins().imul(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn div_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);

    // Constant divisor optimization: skip zero-check and overflow-check when divisor is known.
    if let Some(const_b) = e.get_reg_const(inst.c) {
        if const_b == 0 {
            // Division by constant zero — always panic.
            let one_val = e.builder().ins().iconst(types::I8, 1);
            let zero_val = e.builder().ins().iconst(types::I8, 0);
            let is_true = e.builder().ins().icmp(IntCC::NotEqual, one_val, zero_val);
            emit_runtime_trap_if(e, is_true, JitRuntimeTrapKind::DivisionByZero, None, None);
            // Unreachable, but satisfy SSA
            let undef = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, undef);
            return;
        }
        let b = e.read_var(inst.c);
        if const_b == -1 {
            // Only need overflow check (MIN_INT64 / -1)
            let min_i64 = e.builder().ins().iconst(types::I64, i64::MIN);
            let one = e.builder().ins().iconst(types::I64, 1);
            let is_min = e.builder().ins().icmp(IntCC::Equal, a, min_i64);
            let safe_b = e.builder().ins().select(is_min, one, b);
            let r = e.builder().ins().sdiv(a, safe_b);
            e.write_var(inst.a, r);
        } else {
            // Non-zero, non-(-1) constant: no checks needed at all
            let r = e.builder().ins().sdiv(a, b);
            e.write_var(inst.a, r);
        }
        return;
    }

    let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_runtime_trap_if(e, is_zero, JitRuntimeTrapKind::DivisionByZero, None, None);
    // Handle MIN_INT64 / -1 overflow: result would be MAX_INT64+1, which overflows.
    // x86 idiv traps on this. Go semantics: result wraps to MIN_INT64.
    // Replace b with 1 when overflow would occur to avoid the trap.
    let min_i64 = e.builder().ins().iconst(types::I64, i64::MIN);
    let neg_one = e.builder().ins().iconst(types::I64, -1i64);
    let one = e.builder().ins().iconst(types::I64, 1);
    let is_min = e.builder().ins().icmp(IntCC::Equal, a, min_i64);
    let is_neg_one = e.builder().ins().icmp(IntCC::Equal, b, neg_one);
    let is_overflow = e.builder().ins().band(is_min, is_neg_one);
    // If overflow, use 1 as divisor (MIN / 1 = MIN), otherwise use original b
    let safe_b = e.builder().ins().select(is_overflow, one, b);
    let r = e.builder().ins().sdiv(a, safe_b);
    e.write_var(inst.a, r);
}

pub(super) fn mod_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);

    // Constant divisor optimization
    if let Some(const_b) = e.get_reg_const(inst.c) {
        if const_b == 0 {
            let one_val = e.builder().ins().iconst(types::I8, 1);
            let zero_val = e.builder().ins().iconst(types::I8, 0);
            let is_true = e.builder().ins().icmp(IntCC::NotEqual, one_val, zero_val);
            emit_runtime_trap_if(e, is_true, JitRuntimeTrapKind::DivisionByZero, None, None);
            let undef = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, undef);
            return;
        }
        let b = e.read_var(inst.c);
        if const_b == -1 {
            // MIN_INT64 % -1 should be 0, but x86 idiv traps. Guard only for that.
            let min_i64 = e.builder().ins().iconst(types::I64, i64::MIN);
            let one = e.builder().ins().iconst(types::I64, 1);
            let is_min = e.builder().ins().icmp(IntCC::Equal, a, min_i64);
            let safe_b = e.builder().ins().select(is_min, one, b);
            let r = e.builder().ins().srem(a, safe_b);
            e.write_var(inst.a, r);
        } else {
            let r = e.builder().ins().srem(a, b);
            e.write_var(inst.a, r);
        }
        return;
    }

    let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_runtime_trap_if(e, is_zero, JitRuntimeTrapKind::DivisionByZero, None, None);
    // Handle MIN_INT64 % -1: x86 idiv traps on this. Result should be 0.
    // Replace b with 1 when overflow would occur (MIN % 1 = 0).
    let min_i64 = e.builder().ins().iconst(types::I64, i64::MIN);
    let neg_one = e.builder().ins().iconst(types::I64, -1i64);
    let one = e.builder().ins().iconst(types::I64, 1);
    let is_min = e.builder().ins().icmp(IntCC::Equal, a, min_i64);
    let is_neg_one = e.builder().ins().icmp(IntCC::Equal, b, neg_one);
    let is_overflow = e.builder().ins().band(is_min, is_neg_one);
    let safe_b = e.builder().ins().select(is_overflow, one, b);
    let r = e.builder().ins().srem(a, safe_b);
    e.write_var(inst.a, r);
}

pub(super) fn div_u<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);

    // Constant divisor optimization: skip zero-check
    if let Some(const_b) = e.get_reg_const(inst.c) {
        if const_b == 0 {
            let one_val = e.builder().ins().iconst(types::I8, 1);
            let zero_val = e.builder().ins().iconst(types::I8, 0);
            let is_true = e.builder().ins().icmp(IntCC::NotEqual, one_val, zero_val);
            emit_runtime_trap_if(e, is_true, JitRuntimeTrapKind::DivisionByZero, None, None);
            let undef = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, undef);
            return;
        }
        let b = e.read_var(inst.c);
        let r = e.builder().ins().udiv(a, b);
        e.write_var(inst.a, r);
        return;
    }

    let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_runtime_trap_if(e, is_zero, JitRuntimeTrapKind::DivisionByZero, None, None);
    let r = e.builder().ins().udiv(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn mod_u<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);

    // Constant divisor optimization: skip zero-check
    if let Some(const_b) = e.get_reg_const(inst.c) {
        if const_b == 0 {
            let one_val = e.builder().ins().iconst(types::I8, 1);
            let zero_val = e.builder().ins().iconst(types::I8, 0);
            let is_true = e.builder().ins().icmp(IntCC::NotEqual, one_val, zero_val);
            emit_runtime_trap_if(e, is_true, JitRuntimeTrapKind::DivisionByZero, None, None);
            let undef = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, undef);
            return;
        }
        let b = e.read_var(inst.c);
        let r = e.builder().ins().urem(a, b);
        e.write_var(inst.a, r);
        return;
    }

    let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_runtime_trap_if(e, is_zero, JitRuntimeTrapKind::DivisionByZero, None, None);
    let r = e.builder().ins().urem(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn neg_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let r = e.builder().ins().ineg(a);
    e.write_var(inst.a, r);
}

pub(super) fn add_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fadd(fa, fb);
    e.write_var_f64(inst.a, fr);
}

pub(super) fn sub_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fsub(fa, fb);
    e.write_var_f64(inst.a, fr);
}

pub(super) fn mul_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fmul(fa, fb);
    e.write_var_f64(inst.a, fr);
}

pub(super) fn div_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fdiv(fa, fb);
    e.write_var_f64(inst.a, fr);
}

pub(super) fn neg_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fr = e.builder().ins().fneg(fa);
    e.write_var_f64(inst.a, fr);
}

pub(super) fn cmp_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, cc: IntCC) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let cmp = e.builder().ins().icmp(cc, a, b);
    let r = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, r);
}

pub(super) fn cmp_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, cc: FloatCC) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let cmp = e.builder().ins().fcmp(cc, fa, fb);
    let r = e.builder().ins().uextend(types::I64, cmp);
    // Result is bool (I64), not F64
    e.write_var(inst.a, r);
}

pub(super) fn bitwise_not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let r = e.builder().ins().bnot(a);
    e.write_var(inst.a, r);
}

pub(super) fn bool_not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let zero = e.builder().ins().iconst(types::I64, 0);
    let cmp = e.builder().ins().icmp(IntCC::Equal, a, zero);
    let r = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, r);
}

pub(super) fn and<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let r = e.builder().ins().band(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn or<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let r = e.builder().ins().bor(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn xor<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let r = e.builder().ins().bxor(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn and_not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let r = e.builder().ins().band_not(a, b);
    e.write_var(inst.a, r);
}

pub(super) fn shift_precheck<'a>(e: &mut impl IrEmitter<'a>, shift_amt: Value) -> (Value, Value) {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_negative = e
        .builder()
        .ins()
        .icmp(IntCC::SignedLessThan, shift_amt, zero);
    emit_runtime_trap_if(
        e,
        is_negative,
        JitRuntimeTrapKind::NegativeShift,
        None,
        None,
    );
    let sixty_four = e.builder().ins().iconst(types::I64, 64);
    let is_large = e
        .builder()
        .ins()
        .icmp(IntCC::SignedGreaterThanOrEqual, shift_amt, sixty_four);
    (zero, is_large)
}

pub(super) fn shl<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    // Constant shift optimization: skip precheck when shift amount is known valid
    if let Some(const_b) = e.get_reg_const(inst.c) {
        if const_b < 0 {
            // Negative constant shift — always panic
            let one_val = e.builder().ins().iconst(types::I8, 1);
            let zero_val = e.builder().ins().iconst(types::I8, 0);
            let is_true = e.builder().ins().icmp(IntCC::NotEqual, one_val, zero_val);
            emit_runtime_trap_if(e, is_true, JitRuntimeTrapKind::NegativeShift, None, None);
            let undef = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, undef);
        } else if const_b >= 64 {
            let zero = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, zero);
        } else {
            let r = e.builder().ins().ishl(a, b);
            e.write_var(inst.a, r);
        }
        return;
    }
    let (zero, is_large) = shift_precheck(e, b);
    let safe_shift = e.builder().ins().select(is_large, zero, b);
    let shifted = e.builder().ins().ishl(a, safe_shift);
    let r = e.builder().ins().select(is_large, zero, shifted);
    e.write_var(inst.a, r);
}

pub(super) fn shr_s<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    // Constant shift optimization
    if let Some(const_b) = e.get_reg_const(inst.c) {
        if const_b < 0 {
            let one_val = e.builder().ins().iconst(types::I8, 1);
            let zero_val = e.builder().ins().iconst(types::I8, 0);
            let is_true = e.builder().ins().icmp(IntCC::NotEqual, one_val, zero_val);
            emit_runtime_trap_if(e, is_true, JitRuntimeTrapKind::NegativeShift, None, None);
            let undef = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, undef);
        } else if const_b >= 64 {
            // Arithmetic right shift >= 64: result is 0 or -1 depending on sign
            let zero = e.builder().ins().iconst(types::I64, 0);
            let minus_one = e.builder().ins().iconst(types::I64, -1i64);
            let is_neg = e.builder().ins().icmp(IntCC::SignedLessThan, a, zero);
            let r = e.builder().ins().select(is_neg, minus_one, zero);
            e.write_var(inst.a, r);
        } else {
            let r = e.builder().ins().sshr(a, b);
            e.write_var(inst.a, r);
        }
        return;
    }
    let (zero, is_large) = shift_precheck(e, b);
    let safe_shift = e.builder().ins().select(is_large, zero, b);
    let shifted = e.builder().ins().sshr(a, safe_shift);
    let is_a_negative = e.builder().ins().icmp(IntCC::SignedLessThan, a, zero);
    let minus_one = e.builder().ins().iconst(types::I64, -1i64);
    let large_result = e.builder().ins().select(is_a_negative, minus_one, zero);
    let r = e.builder().ins().select(is_large, large_result, shifted);
    e.write_var(inst.a, r);
}

pub(super) fn shr_u<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    // Constant shift optimization
    if let Some(const_b) = e.get_reg_const(inst.c) {
        if const_b < 0 {
            let one_val = e.builder().ins().iconst(types::I8, 1);
            let zero_val = e.builder().ins().iconst(types::I8, 0);
            let is_true = e.builder().ins().icmp(IntCC::NotEqual, one_val, zero_val);
            emit_runtime_trap_if(e, is_true, JitRuntimeTrapKind::NegativeShift, None, None);
            let undef = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, undef);
        } else if const_b >= 64 {
            let zero = e.builder().ins().iconst(types::I64, 0);
            e.write_var(inst.a, zero);
        } else {
            let r = e.builder().ins().ushr(a, b);
            e.write_var(inst.a, r);
        }
        return;
    }
    let (zero, is_large) = shift_precheck(e, b);
    let safe_shift = e.builder().ins().select(is_large, zero, b);
    let shifted = e.builder().ins().ushr(a, safe_shift);
    let r = e.builder().ins().select(is_large, zero, shifted);
    e.write_var(inst.a, r);
}
