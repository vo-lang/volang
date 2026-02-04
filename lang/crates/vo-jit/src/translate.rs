//! Shared instruction translation logic.

use cranelift_codegen::ir::{types, InstBuilder, MemFlags, StackSlot, Value};
use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};

use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode};

use crate::translator::{IrEmitter, TranslateResult};
use crate::JitError;

/// Translate a single instruction.
pub fn translate_inst<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) -> Result<TranslateResult, JitError> {
    use Opcode::*;
    use TranslateResult::*;
    
    match inst.opcode() {
        Hint => Ok(Completed),
        LoadInt => { load_int(e, inst); Ok(Completed) }
        LoadConst => { load_const(e, inst); Ok(Completed) }
        Copy => { copy(e, inst); Ok(Completed) }
        CopyN => { copy_n(e, inst); Ok(Completed) }
        AddI => { add_i(e, inst); Ok(Completed) }
        SubI => { sub_i(e, inst); Ok(Completed) }
        MulI => { mul_i(e, inst); Ok(Completed) }
        DivI => { div_i(e, inst); Ok(Completed) }
        DivU => { div_u(e, inst); Ok(Completed) }
        ModI => { mod_i(e, inst); Ok(Completed) }
        ModU => { mod_u(e, inst); Ok(Completed) }
        NegI => { neg_i(e, inst); Ok(Completed) }
        AddF => { add_f(e, inst); Ok(Completed) }
        SubF => { sub_f(e, inst); Ok(Completed) }
        MulF => { mul_f(e, inst); Ok(Completed) }
        DivF => { div_f(e, inst); Ok(Completed) }
        NegF => { neg_f(e, inst); Ok(Completed) }
        EqI => { cmp_i(e, inst, IntCC::Equal); Ok(Completed) }
        NeI => { cmp_i(e, inst, IntCC::NotEqual); Ok(Completed) }
        LtI => { cmp_i(e, inst, IntCC::SignedLessThan); Ok(Completed) }
        LeI => { cmp_i(e, inst, IntCC::SignedLessThanOrEqual); Ok(Completed) }
        GtI => { cmp_i(e, inst, IntCC::SignedGreaterThan); Ok(Completed) }
        GeI => { cmp_i(e, inst, IntCC::SignedGreaterThanOrEqual); Ok(Completed) }
        LtU => { cmp_i(e, inst, IntCC::UnsignedLessThan); Ok(Completed) }
        LeU => { cmp_i(e, inst, IntCC::UnsignedLessThanOrEqual); Ok(Completed) }
        GtU => { cmp_i(e, inst, IntCC::UnsignedGreaterThan); Ok(Completed) }
        GeU => { cmp_i(e, inst, IntCC::UnsignedGreaterThanOrEqual); Ok(Completed) }
        EqF => { cmp_f(e, inst, FloatCC::Equal); Ok(Completed) }
        NeF => { cmp_f(e, inst, FloatCC::NotEqual); Ok(Completed) }
        LtF => { cmp_f(e, inst, FloatCC::LessThan); Ok(Completed) }
        LeF => { cmp_f(e, inst, FloatCC::LessThanOrEqual); Ok(Completed) }
        GtF => { cmp_f(e, inst, FloatCC::GreaterThan); Ok(Completed) }
        GeF => { cmp_f(e, inst, FloatCC::GreaterThanOrEqual); Ok(Completed) }
        Not => { bitwise_not(e, inst); Ok(Completed) }
        BoolNot => { bool_not(e, inst); Ok(Completed) }
        And => { and(e, inst); Ok(Completed) }
        Or => { or(e, inst); Ok(Completed) }
        Xor => { xor(e, inst); Ok(Completed) }
        AndNot => { and_not(e, inst); Ok(Completed) }
        Shl => { shl(e, inst); Ok(Completed) }
        ShrS => { shr_s(e, inst); Ok(Completed) }
        ShrU => { shr_u(e, inst); Ok(Completed) }
        GlobalGet => { global_get(e, inst); Ok(Completed) }
        GlobalSet => { global_set(e, inst); Ok(Completed) }
        GlobalGetN => { global_get_n(e, inst); Ok(Completed) }
        GlobalSetN => { global_set_n(e, inst); Ok(Completed) }
        PtrGet => { ptr_get(e, inst); Ok(Completed) }
        PtrSet => { ptr_set(e, inst); Ok(Completed) }
        PtrGetN => { ptr_get_n(e, inst); Ok(Completed) }
        PtrSetN => { ptr_set_n(e, inst); Ok(Completed) }
        PtrAdd => { ptr_add(e, inst); Ok(Completed) }
        SlotGet => { slot_get(e, inst); Ok(Completed) }
        SlotSet => { slot_set(e, inst); Ok(Completed) }
        SlotGetN => { slot_get_n(e, inst); Ok(Completed) }
        SlotSetN => { slot_set_n(e, inst); Ok(Completed) }
        ConvI2F => { conv_i2f(e, inst); Ok(Completed) }
        ConvF2I => { conv_f2i(e, inst); Ok(Completed) }
        ConvF64F32 => { conv_f64_f32(e, inst); Ok(Completed) }
        ConvF32F64 => { conv_f32_f64(e, inst); Ok(Completed) }
        Trunc => { trunc(e, inst); Ok(Completed) }
        IndexCheck => { index_check(e, inst); Ok(Completed) }
        // Slice operations
        SliceNew => { slice_new(e, inst); Ok(Completed) }
        SliceGet => { slice_get(e, inst); Ok(Completed) }
        SliceSet => { slice_set(e, inst); Ok(Completed) }
        SliceLen => { slice_len(e, inst); Ok(Completed) }
        SliceCap => { slice_cap(e, inst); Ok(Completed) }
        SliceSlice => { slice_slice(e, inst); Ok(Completed) }
        SliceAppend => { slice_append(e, inst); Ok(Completed) }
        SliceAddr => { slice_addr(e, inst); Ok(Completed) }
        // Array operations
        ArrayNew => { array_new(e, inst); Ok(Completed) }
        ArrayGet => { array_get(e, inst); Ok(Completed) }
        ArraySet => { array_set(e, inst); Ok(Completed) }
        ArrayAddr => { array_addr(e, inst); Ok(Completed) }
        // String operations
        StrLen => { str_len(e, inst); Ok(Completed) }
        StrIndex => { str_index(e, inst); Ok(Completed) }
        StrConcat => { str_concat(e, inst); Ok(Completed) }
        StrSlice => { str_slice(e, inst); Ok(Completed) }
        StrEq => { str_eq(e, inst); Ok(Completed) }
        StrNe => { str_ne(e, inst); Ok(Completed) }
        StrLt => { str_cmp(e, inst, IntCC::SignedLessThan); Ok(Completed) }
        StrLe => { str_cmp(e, inst, IntCC::SignedLessThanOrEqual); Ok(Completed) }
        StrGt => { str_cmp(e, inst, IntCC::SignedGreaterThan); Ok(Completed) }
        StrGe => { str_cmp(e, inst, IntCC::SignedGreaterThanOrEqual); Ok(Completed) }
        StrDecodeRune => { str_decode_rune(e, inst); Ok(Completed) }
        // Map operations
        MapNew => { map_new(e, inst); Ok(Completed) }
        MapLen => { map_len(e, inst); Ok(Completed) }
        MapGet => { map_get(e, inst); Ok(Completed) }
        MapSet => { map_set(e, inst); Ok(Completed) }
        MapDelete => { map_delete(e, inst); Ok(Completed) }
        MapIterInit => { map_iter_init(e, inst); Ok(Completed) }
        MapIterNext => { map_iter_next(e, inst); Ok(Completed) }
        // Closure operations
        ClosureNew => { closure_new(e, inst); Ok(Completed) }
        ClosureGet => { closure_get(e, inst); Ok(Completed) }
        // Allocation
        PtrNew => { ptr_new(e, inst); Ok(Completed) }
        ChanNew => { chan_new(e, inst); Ok(Completed) }
        ChanLen => { chan_len(e, inst); Ok(Completed) }
        ChanCap => { chan_cap(e, inst); Ok(Completed) }
        PortNew => { port_new(e, inst); Ok(Completed) }
        PortLen => { port_len(e, inst); Ok(Completed) }
        PortCap => { port_cap(e, inst); Ok(Completed) }
        // Interface
        IfaceAssert => { iface_assert(e, inst); Ok(Completed) }
        StrNew => { str_new(e, inst); Ok(Completed) }
        IfaceAssign => { iface_assign(e, inst); Ok(Completed) }
        IfaceEq => { iface_eq(e, inst); Ok(Completed) }
        // Control flow - compiler specific
        Jump | JumpIf | JumpIfNot | Return | Panic => Ok(Unhandled),
        // Function calls - compiler specific
        Call | CallExtern | CallClosure | CallIface => Ok(Unhandled),
        // Unsupported
        _ => Ok(Unhandled),
    }
}

fn load_int<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let val = e.builder().ins().iconst(types::I64, inst.imm32() as i64);
    e.write_var(inst.a, val);
}

fn load_const<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let const_idx = inst.b as usize;
    // Clone the constant to avoid borrow conflict
    let constant = e.vo_module().constants[const_idx].clone();
    match &constant {
        Constant::Float(f) => {
            // Float constant: load as F64 directly, no bitcast needed
            e.set_reg_const(inst.a, f.to_bits() as i64);
            let v = e.builder().ins().f64const(*f);
            e.write_var_f64(inst.a, v);
        }
        constant => {
            let (val, reg_const) = match constant {
                Constant::Nil => (0i64, Some(0i64)),
                Constant::Bool(b) => (*b as i64, Some(*b as i64)),
                Constant::Int(i) => (*i, Some(*i)),
                Constant::String(_) => (0, None),
                Constant::Float(_) => unreachable!(),
            };
            if let Some(c) = reg_const { e.set_reg_const(inst.a, c); }
            let v = e.builder().ins().iconst(types::I64, val);
            e.write_var(inst.a, v);
        }
    }
}

fn copy<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let v = e.read_var(inst.b);
    e.write_var(inst.a, v);
}

fn copy_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let count = inst.c as usize;
    for i in 0..count {
        let v = e.read_var(inst.b + i as u16);
        e.write_var(inst.a + i as u16, v);
    }
}

fn add_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().iadd(a, b);
    e.write_var(inst.a, r);
}

fn sub_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().isub(a, b);
    e.write_var(inst.a, r);
}

fn mul_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().imul(a, b);
    e.write_var(inst.a, r);
}

fn div_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_panic_if(e, is_zero, true);
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

fn mod_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_panic_if(e, is_zero, true);
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

fn div_u<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_panic_if(e, is_zero, true);
    let r = e.builder().ins().udiv(a, b);
    e.write_var(inst.a, r);
}

fn mod_u<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    // Check for division by zero
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_zero = e.builder().ins().icmp(IntCC::Equal, b, zero);
    emit_panic_if(e, is_zero, true);
    let r = e.builder().ins().urem(a, b);
    e.write_var(inst.a, r);
}

fn neg_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let r = e.builder().ins().ineg(a);
    e.write_var(inst.a, r);
}

fn add_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fadd(fa, fb);
    e.write_var_f64(inst.a, fr);
}

fn sub_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fsub(fa, fb);
    e.write_var_f64(inst.a, fr);
}

fn mul_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fmul(fa, fb);
    e.write_var_f64(inst.a, fr);
}

fn div_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let fr = e.builder().ins().fdiv(fa, fb);
    e.write_var_f64(inst.a, fr);
}

fn neg_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let fa = e.read_var_f64(inst.b);
    let fr = e.builder().ins().fneg(fa);
    e.write_var_f64(inst.a, fr);
}

fn cmp_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, cc: IntCC) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let cmp = e.builder().ins().icmp(cc, a, b);
    let r = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, r);
}

fn cmp_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, cc: FloatCC) {
    let fa = e.read_var_f64(inst.b);
    let fb = e.read_var_f64(inst.c);
    let cmp = e.builder().ins().fcmp(cc, fa, fb);
    let r = e.builder().ins().uextend(types::I64, cmp);
    // Result is bool (I64), not F64
    e.write_var(inst.a, r);
}

fn bitwise_not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let r = e.builder().ins().bnot(a);
    e.write_var(inst.a, r);
}

fn bool_not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let zero = e.builder().ins().iconst(types::I64, 0);
    let cmp = e.builder().ins().icmp(IntCC::Equal, a, zero);
    let r = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, r);
}

fn and<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().band(a, b);
    e.write_var(inst.a, r);
}

fn or<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().bor(a, b);
    e.write_var(inst.a, r);
}

fn xor<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().bxor(a, b);
    e.write_var(inst.a, r);
}

fn and_not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().band_not(a, b);
    e.write_var(inst.a, r);
}

fn shift_precheck<'a>(e: &mut impl IrEmitter<'a>, shift_amt: Value) -> (Value, Value) {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_negative = e.builder().ins().icmp(IntCC::SignedLessThan, shift_amt, zero);
    emit_panic_if(e, is_negative, true);
    let sixty_four = e.builder().ins().iconst(types::I64, 64);
    let is_large = e.builder().ins().icmp(IntCC::SignedGreaterThanOrEqual, shift_amt, sixty_four);
    (zero, is_large)
}

fn shl<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let (zero, is_large) = shift_precheck(e, b);
    let shifted = e.builder().ins().ishl(a, b);
    let r = e.builder().ins().select(is_large, zero, shifted);
    e.write_var(inst.a, r);
}

fn shr_s<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let (zero, is_large) = shift_precheck(e, b);
    let shifted = e.builder().ins().sshr(a, b);
    let is_a_negative = e.builder().ins().icmp(IntCC::SignedLessThan, a, zero);
    let minus_one = e.builder().ins().iconst(types::I64, -1i64);
    let large_result = e.builder().ins().select(is_a_negative, minus_one, zero);
    let r = e.builder().ins().select(is_large, large_result, shifted);
    e.write_var(inst.a, r);
}

fn shr_u<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let (zero, is_large) = shift_precheck(e, b);
    let shifted = e.builder().ins().ushr(a, b);
    let r = e.builder().ins().select(is_large, zero, shifted);
    e.write_var(inst.a, r);
}

fn global_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let offset = (inst.b as i32) * 8;
    let v = e.builder().ins().load(types::I64, MemFlags::trusted(), globals, offset);
    e.write_var(inst.a, v);
}

fn global_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let v = e.read_var(inst.b);
    let offset = (inst.a as i32) * 8;
    e.builder().ins().store(MemFlags::trusted(), v, globals, offset);
}

fn global_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    for i in 0..inst.flags as usize {
        let offset = ((inst.b as usize + i) * 8) as i32;
        let v = e.builder().ins().load(types::I64, MemFlags::trusted(), globals, offset);
        e.write_var(inst.a + i as u16, v);
    }
}

fn global_set_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.b + i as u16);
        let offset = ((inst.a as usize + i) * 8) as i32;
        e.builder().ins().store(MemFlags::trusted(), v, globals, offset);
    }
}

/// Emit conditional panic: if `condition` is true, return panic; otherwise continue.
/// Optionally calls vo_panic to set panic_flag for defer/recover support.
/// For runtime panics (nil pointer, bounds check), msg slots are 0 - VM will use default message.
fn emit_panic_if<'a>(e: &mut impl IrEmitter<'a>, condition: Value, call_vo_panic: bool) {
    let panic_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder().ins().brif(condition, panic_block, &[], ok_block, &[]);
    
    e.builder().switch_to_block(panic_block);
    e.builder().seal_block(panic_block);
    if call_vo_panic {
        if let Some(panic_func) = e.helpers().panic {
            let ctx = e.ctx_param();
            // Runtime panics pass 0 for both slots - VM will use default "nil pointer dereference" message
            let msg_slot0 = e.builder().ins().iconst(types::I64, 0);
            let msg_slot1 = e.builder().ins().iconst(types::I64, 0);
            e.builder().ins().call(panic_func, &[ctx, msg_slot0, msg_slot1]);
        }
    }
    let panic_ret_val = e.panic_return_value();
    let panic_ret = e.builder().ins().iconst(types::I32, panic_ret_val as i64);
    e.builder().ins().return_(&[panic_ret]);
    
    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
}

/// Emit nil check for pointer. Panics if ptr is nil.
fn emit_nil_ptr_check<'a>(e: &mut impl IrEmitter<'a>, ptr: Value) {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, ptr, zero);
    emit_panic_if(e, is_nil, true); // call vo_panic for defer/recover
}

/// Emit nil check for pointer with slot tracking.
/// Skips the check if the slot has already been verified non-nil.
fn emit_nil_ptr_check_for_slot<'a>(e: &mut impl IrEmitter<'a>, ptr_slot: u16, ptr: Value) {
    if e.is_checked_non_nil(ptr_slot) {
        return; // Already verified non-nil in this basic block
    }
    emit_nil_ptr_check(e, ptr);
    e.mark_checked_non_nil(ptr_slot);
}

fn ptr_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.b);
    emit_nil_ptr_check_for_slot(e, inst.b, ptr);
    let offset = (inst.c as i32) * 8;
    let v = e.builder().ins().load(types::I64, MemFlags::trusted(), ptr, offset);
    e.write_var(inst.a, v);
}

fn ptr_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.a);
    emit_nil_ptr_check_for_slot(e, inst.a, ptr);
    let v = e.read_var(inst.c);
    let offset = (inst.b as i32) * 8;
    e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    
    // Write barrier if val may be GcRef (flags & 1)
    if (inst.flags & 1) != 0 {
        if let Some(wb_ref) = e.helpers().write_barrier {
            let gc = e.gc_ptr();
            let offset_val = e.builder().ins().iconst(types::I32, inst.b as i64);
            e.builder().ins().call(wb_ref, &[gc, ptr, offset_val, v]);
        }
    }
}

fn ptr_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.b);
    emit_nil_ptr_check_for_slot(e, inst.b, ptr);
    for i in 0..inst.flags as usize {
        let offset = ((inst.c as usize + i) * 8) as i32;
        let v = e.builder().ins().load(types::I64, MemFlags::trusted(), ptr, offset);
        e.write_var(inst.a + i as u16, v);
    }
}

fn ptr_set_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.a);
    emit_nil_ptr_check_for_slot(e, inst.a, ptr);
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.c + i as u16);
        let offset = ((inst.b as usize + i) * 8) as i32;
        e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    }
}

fn ptr_add<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // a=dst, b=ptr, c=offset_slots: dst = ptr + offset * 8
    let ptr = e.read_var(inst.b);
    let offset_slots = e.read_var(inst.c);
    let offset_bytes = e.builder().ins().imul_imm(offset_slots, 8);
    let result = e.builder().ins().iadd(ptr, offset_bytes);
    e.write_var(inst.a, result);
}

fn slot_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.b);
    let idx = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm(idx, 8);
    let addr = e.builder().ins().iadd(base, offset);
    let v = e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0);
    e.write_var(inst.a, v);
}

fn slot_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.a);
    let idx = e.read_var(inst.b);
    let v = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm(idx, 8);
    let addr = e.builder().ins().iadd(base, offset);
    e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
}

fn slot_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.b);
    let idx = e.read_var(inst.c);
    let elem_slots = inst.flags as usize;
    let byte_off = e.builder().ins().imul_imm(idx, (elem_slots * 8) as i64);
    let start = e.builder().ins().iadd(base, byte_off);
    for i in 0..elem_slots {
        let addr = e.builder().ins().iadd_imm(start, (i * 8) as i64);
        let v = e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0);
        e.write_var(inst.a + i as u16, v);
    }
}

fn slot_set_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.a);
    let idx = e.read_var(inst.b);
    let elem_slots = inst.flags as usize;
    let byte_off = e.builder().ins().imul_imm(idx, (elem_slots * 8) as i64);
    let start = e.builder().ins().iadd(base, byte_off);
    for i in 0..elem_slots {
        let v = e.read_var(inst.c + i as u16);
        let addr = e.builder().ins().iadd_imm(start, (i * 8) as i64);
        e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
    }
}

fn conv_i2f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is I64 (int), output is F64 (float)
    let a = e.read_var(inst.b);
    let f = e.builder().ins().fcvt_from_sint(types::F64, a);
    e.write_var_f64(inst.a, f);
}

fn conv_f2i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is F64 (float), output is I64 (int)
    let f = e.read_var_f64(inst.b);
    let r = e.builder().ins().fcvt_to_sint(types::I64, f);
    e.write_var(inst.a, r);
}

fn conv_f64_f32<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is F64, output is F32 stored as I64 (low 32 bits)
    let f64v = e.read_var_f64(inst.b);
    let f32v = e.builder().ins().fdemote(types::F32, f64v);
    // f32 is 32-bit, bitcast to i32 first, then extend to i64
    let i32v = e.builder().ins().bitcast(types::I32, MemFlags::new(), f32v);
    let r = e.builder().ins().uextend(types::I64, i32v);
    e.write_var(inst.a, r);
}

fn conv_f32_f64<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is F32 stored as I64 (low 32 bits), output is F64
    let a = e.read_var(inst.b);
    let i32v = e.builder().ins().ireduce(types::I32, a);
    let f32v = e.builder().ins().bitcast(types::F32, MemFlags::new(), i32v);
    let f64v = e.builder().ins().fpromote(types::F64, f32v);
    e.write_var_f64(inst.a, f64v);
}

fn trunc<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let flags = inst.flags;
    let signed = (flags & 0x80) != 0;
    let bytes = flags & 0x7F;
    
    let r = match bytes {
        1 => e.builder().ins().ireduce(types::I8, a),
        2 => e.builder().ins().ireduce(types::I16, a),
        4 => e.builder().ins().ireduce(types::I32, a),
        _ => {
            e.write_var(inst.a, a);
            return;
        }
    };
    
    let result = if signed {
        e.builder().ins().sextend(types::I64, r)
    } else {
        e.builder().ins().uextend(types::I64, r)
    };
    e.write_var(inst.a, result);
}

fn index_check<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let idx = e.read_var(inst.a);
    let len = e.read_var(inst.b);
    let out_of_bounds = e.builder().ins().icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_panic_if(e, out_of_bounds, true);
}

// =============================================================================
// Slice/Array element size helpers
// =============================================================================

/// Resolve elem_bytes from instruction flags.
/// When flags==0, elem_bytes is stored in the specified register (set by preceding LoadConst).
/// Returns (elem_bytes, needs_sign_extend).
fn resolve_elem_bytes<'a>(e: &impl IrEmitter<'a>, flags: u8, eb_reg: u16) -> (usize, bool) {
    match flags {
        0 => {
            // Dynamic: elem_bytes > 63, stored in register by LoadConst
            let eb = e.get_reg_const(eb_reg).unwrap() as usize;
            (eb, false)
        }
        0x81 => (1, true),   // int8
        0x82 => (2, true),   // int16
        0x84 => (4, true),   // int32
        0x44 => (4, false),  // float32
        f => (f as usize, false),
    }
}

/// Load a single element (1/2/4/8 bytes) from memory address, with optional sign extension.
fn load_element<'a>(e: &mut impl IrEmitter<'a>, addr: Value, elem_bytes: usize, needs_sext: bool) -> Value {
    match elem_bytes {
        1 => {
            let v = e.builder().ins().load(types::I8, MemFlags::trusted(), addr, 0);
            if needs_sext { e.builder().ins().sextend(types::I64, v) }
            else { e.builder().ins().uextend(types::I64, v) }
        }
        2 => {
            let v = e.builder().ins().load(types::I16, MemFlags::trusted(), addr, 0);
            if needs_sext { e.builder().ins().sextend(types::I64, v) }
            else { e.builder().ins().uextend(types::I64, v) }
        }
        4 => {
            let v = e.builder().ins().load(types::I32, MemFlags::trusted(), addr, 0);
            if needs_sext { e.builder().ins().sextend(types::I64, v) }
            else { e.builder().ins().uextend(types::I64, v) }
        }
        _ => e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0),
    }
}

/// Store a single element (1/2/4/8 bytes) to memory address.
fn store_element<'a>(e: &mut impl IrEmitter<'a>, addr: Value, val: Value, elem_bytes: usize) {
    match elem_bytes {
        1 => {
            let v = e.builder().ins().ireduce(types::I8, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        2 => {
            let v = e.builder().ins().ireduce(types::I16, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        4 => {
            let v = e.builder().ins().ireduce(types::I32, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        _ => { e.builder().ins().store(MemFlags::trusted(), val, addr, 0); }
    }
}

fn emit_elem_slots_i32<'a>(e: &mut impl IrEmitter<'a>, flags: u8, eb_reg: u16) -> Value {
    if flags == 0 {
        let eb_raw = e.read_var(eb_reg);
        e.builder().ins().ireduce(types::I32, eb_raw)
    } else {
        let slots = match flags {
            0x81 | 0x82 => 1,
            0x84 | 0x44 => 1,
            f => f as i64,
        };
        e.builder().ins().iconst(types::I32, slots)
    }
}

fn emit_elem_bytes_i32<'a>(e: &mut impl IrEmitter<'a>, flags: u8, eb_reg: u16) -> Value {
    if flags == 0 {
        let eb_raw = e.read_var(eb_reg);
        e.builder().ins().ireduce(types::I32, eb_raw)
    } else {
        let bytes = match flags {
            0x81 => 1,
            0x82 => 2,
            0x84 | 0x44 => 4,
            f => f as i64,
        };
        e.builder().ins().iconst(types::I32, bytes)
    }
}

// =============================================================================
// Slice operations
// =============================================================================

use vo_runtime::objects::slice::{FIELD_DATA_PTR as SLICE_FIELD_DATA_PTR_SLOT, FIELD_LEN as SLICE_FIELD_LEN_SLOT};
const SLICE_FIELD_DATA_PTR: i32 = (SLICE_FIELD_DATA_PTR_SLOT * 8) as i32;
const SLICE_FIELD_LEN: i32 = (SLICE_FIELD_LEN_SLOT * 8) as i32;

/// Emit bounds check for slice access. Panics if idx >= len or slice is nil.
/// Returns data_ptr for the slice (only valid if bounds check passed).
fn emit_slice_bounds_check<'a>(e: &mut impl IrEmitter<'a>, s: Value, idx: Value) -> Value {
    // len = 0 if nil, otherwise load from slice
    let len = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
    
    // Check idx >= len (nil slice has len=0, so any idx will be out of bounds)
    let out_of_bounds = e.builder().ins().icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_panic_if(e, out_of_bounds, false);
    
    e.builder().ins().load(types::I64, MemFlags::trusted(), s, SLICE_FIELD_DATA_PTR)
}

fn slice_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_new_func = e.helpers().slice_new.expect("slice_new helper not registered");
    let elem_bytes_val = emit_elem_bytes_i32(e, inst.flags, inst.c + 2);
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let len = e.read_var(inst.c);
    let cap = e.read_var(inst.c + 1);
    let call = e.builder().ins().call(slice_new_func, &[gc_ptr, meta_i32, elem_bytes_val, len, cap]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn slice_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let (elem_bytes, needs_sext) = resolve_elem_bytes(e, inst.flags, inst.c + 1);
    
    let data_ptr = emit_slice_bounds_check(e, s, idx);
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    
    if elem_bytes <= 8 {
        let addr = e.builder().ins().iadd(data_ptr, off);
        let val = load_element(e, addr, elem_bytes, needs_sext);
        e.write_var(inst.a, val);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        for i in 0..elem_slots {
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(data_ptr, slot_off);
            let val = e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0);
            e.write_var(inst.a + i as u16, val);
        }
    }
}

fn slice_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.a);
    let idx = e.read_var(inst.b);
    let (elem_bytes, _) = resolve_elem_bytes(e, inst.flags, inst.b + 1);
    
    let data_ptr = emit_slice_bounds_check(e, s, idx);
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    
    if elem_bytes <= 8 {
        let val = e.read_var(inst.c);
        let addr = e.builder().ins().iadd(data_ptr, off);
        store_element(e, addr, val, elem_bytes);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        for i in 0..elem_slots {
            let v = e.read_var(inst.c + i as u16);
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(data_ptr, slot_off);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
    }
}

/// Load a field from a pointer, returning 0 if pointer is nil.
/// Pattern: if ptr == 0 { 0 } else { ptr.field }
fn emit_nil_guarded_load<'a>(e: &mut impl IrEmitter<'a>, ptr: Value, offset: i32) -> Value {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, ptr, zero);
    
    let nil_block = e.builder().create_block();
    let not_nil_block = e.builder().create_block();
    let merge_block = e.builder().create_block();
    e.builder().append_block_param(merge_block, types::I64);
    e.builder().ins().brif(is_nil, nil_block, &[], not_nil_block, &[]);
    
    e.builder().switch_to_block(nil_block);
    e.builder().seal_block(nil_block);
    e.builder().ins().jump(merge_block, &[zero]);
    
    e.builder().switch_to_block(not_nil_block);
    e.builder().seal_block(not_nil_block);
    let val = e.builder().ins().load(types::I64, MemFlags::trusted(), ptr, offset);
    e.builder().ins().jump(merge_block, &[val]);
    
    e.builder().switch_to_block(merge_block);
    e.builder().seal_block(merge_block);
    e.builder().block_params(merge_block)[0]
}

fn slice_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
    e.write_var(inst.a, result);
}

const SLICE_FIELD_CAP: i32 = (vo_runtime::objects::slice::FIELD_CAP * 8) as i32;

fn slice_cap<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_CAP);
    e.write_var(inst.a, result);
}

fn slice_slice<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let gc_ptr = e.gc_ptr();
    let src = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);
    
    let is_array = (inst.flags & 0b01) != 0;
    let has_max = (inst.flags & 0b10) != 0;
    
    // Helper functions do bounds checking and return u64::MAX on error
    let result = if has_max {
        let max = e.read_var(inst.c + 2);
        if is_array {
            let func = e.helpers().slice_from_array3.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi, max]);
            e.builder().inst_results(call)[0]
        } else {
            let func = e.helpers().slice_slice3.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi, max]);
            e.builder().inst_results(call)[0]
        }
    } else {
        if is_array {
            let func = e.helpers().slice_from_array.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi]);
            e.builder().inst_results(call)[0]
        } else {
            let func = e.helpers().slice_slice.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi]);
            e.builder().inst_results(call)[0]
        }
    };
    
    // Check for bounds error (helper returns u64::MAX on error)
    let error_val = e.builder().ins().iconst(types::I64, -1i64);
    let is_error = e.builder().ins().icmp(IntCC::Equal, result, error_val);
    emit_panic_if(e, is_error, true);
    
    e.write_var(inst.a, result);
}

fn slice_append<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_append_func = e.helpers().slice_append.expect("slice_append helper not registered");
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    
    // Instruction format:
    // - c = elem_meta slot
    // - flags==0: c+1 = elem_bytes, c+2.. = elem value
    // - flags!=0: c+1.. = elem value (elem_bytes derived from flags)
    
    // elem_meta from slot c (as i32)
    let elem_meta_raw = e.read_var(inst.c);
    let elem_meta = e.builder().ins().ireduce(types::I32, elem_meta_raw);
    
    // elem_bytes (as i32)
    let elem_bytes = emit_elem_bytes_i32(e, inst.flags, inst.c + 1);
    
    // val_ptr: pointer to element value in stack
    let elem_slot = inst.c + if inst.flags == 0 { 2 } else { 1 };
    let val_ptr = e.var_addr(elem_slot);
    
    // vo_slice_append(gc, elem_meta: u32, elem_bytes: u32, s: u64, val_ptr: *const u64) -> u64
    let call = e.builder().ins().call(slice_append_func, &[gc_ptr, elem_meta, elem_bytes, s, val_ptr]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn slice_addr<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let elem_bytes = inst.flags as i64;
    let data_ptr = e.builder().ins().load(types::I64, MemFlags::trusted(), s, SLICE_FIELD_DATA_PTR);
    let eb = e.builder().ins().iconst(types::I64, elem_bytes);
    let off = e.builder().ins().imul(idx, eb);
    let addr = e.builder().ins().iadd(data_ptr, off);
    e.write_var(inst.a, addr);
}

// =============================================================================
// Array operations
// =============================================================================

const ARRAY_HEADER_BYTES: i64 = 16; // 2 slots

fn array_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let array_new_func = e.helpers().array_new.expect("array_new helper not registered");
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let elem_slots_i32 = emit_elem_slots_i32(e, inst.flags, inst.c + 1);
    let len = e.read_var(inst.c);
    let call = e.builder().ins().call(array_new_func, &[gc_ptr, meta_i32, elem_slots_i32, len]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn array_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let arr = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    // Bounds check: load len from ArrayHeader (offset 0)
    let len = e.builder().ins().load(types::I64, MemFlags::trusted(), arr, 0);
    let out_of_bounds = e.builder().ins().icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_panic_if(e, out_of_bounds, true);
    
    let (elem_bytes, needs_sext) = resolve_elem_bytes(e, inst.flags, inst.c + 1);
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
    
    if elem_bytes <= 8 {
        let addr = e.builder().ins().iadd(arr, off);
        let val = load_element(e, addr, elem_bytes, needs_sext);
        e.write_var(inst.a, val);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        for i in 0..elem_slots {
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(arr, slot_off);
            let val = e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0);
            e.write_var(inst.a + i as u16, val);
        }
    }
}

fn array_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let arr = e.read_var(inst.a);
    let idx = e.read_var(inst.b);
    // Bounds check: load len from ArrayHeader (offset 0)
    let len = e.builder().ins().load(types::I64, MemFlags::trusted(), arr, 0);
    let out_of_bounds = e.builder().ins().icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_panic_if(e, out_of_bounds, true);
    
    let (elem_bytes, _) = resolve_elem_bytes(e, inst.flags, inst.b + 1);
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
    
    if elem_bytes <= 8 {
        let val = e.read_var(inst.c);
        let addr = e.builder().ins().iadd(arr, off);
        store_element(e, addr, val, elem_bytes);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        for i in 0..elem_slots {
            let v = e.read_var(inst.c + i as u16);
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(arr, slot_off);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
    }
}

fn array_addr<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let arr = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let elem_bytes = inst.flags as i64;
    let eb = e.builder().ins().iconst(types::I64, elem_bytes);
    let off = e.builder().ins().imul(idx, eb);
    let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
    let addr = e.builder().ins().iadd(arr, off);
    e.write_var(inst.a, addr);
}

// =============================================================================
// String operations
// =============================================================================

fn str_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().str_len.expect("str_len helper not registered");
    let s = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[s]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_index<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let str_index_func = e.helpers().str_index.expect("str_index helper not registered");
    let str_len_func = e.helpers().str_len.expect("str_len helper not registered");
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    // Bounds check: get string length and compare
    let len_call = e.builder().ins().call(str_len_func, &[s]);
    let len = e.builder().inst_results(len_call)[0];
    let out_of_bounds = e.builder().ins().icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_panic_if(e, out_of_bounds, true);
    let call = e.builder().ins().call(str_index_func, &[s, idx]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_concat<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().str_concat.expect("str_concat helper not registered");
    let gc_ptr = e.gc_ptr();
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[gc_ptr, a, b]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_slice<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().str_slice.expect("str_slice helper not registered");
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);
    let call = e.builder().ins().call(func, &[gc_ptr, s, lo, hi]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_eq<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().str_eq.expect("str_eq helper not registered");
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[a, b]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_ne<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().str_eq.expect("str_eq helper not registered");
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[a, b]);
    let eq_result = e.builder().inst_results(call)[0];
    let zero = e.builder().ins().iconst(types::I64, 0);
    let cmp = e.builder().ins().icmp(IntCC::Equal, eq_result, zero);
    let result = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, result);
}

fn str_cmp<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, cc: IntCC) {
    let func = e.helpers().str_cmp.expect("str_cmp helper not registered");
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[a, b]);
    let cmp_result = e.builder().inst_results(call)[0]; // i32
    let zero = e.builder().ins().iconst(types::I32, 0);
    let cmp = e.builder().ins().icmp(cc, cmp_result, zero);
    let result = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, result);
}

fn str_decode_rune<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().str_decode_rune.expect("str_decode_rune helper not registered");
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[s, idx]);
    let packed = e.builder().inst_results(call)[0];
    // Unpack: packed = (rune << 32) | width
    let rune = e.builder().ins().ushr_imm(packed, 32);
    let width = e.builder().ins().band_imm(packed, 0xFFFFFFFF);
    e.write_var(inst.a, rune);
    e.write_var(inst.a + 1, width);
}

// =============================================================================
// Map operations
// =============================================================================

fn map_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().map_new.expect("map_new helper not registered");
    let gc_ptr = e.gc_ptr();
    // b = packed_meta, b+1 = key_rttid
    let packed_meta = e.read_var(inst.b);
    let key_rttid = e.read_var(inst.b + 1);
    let key_meta = e.builder().ins().ushr_imm(packed_meta, 32);
    let key_meta_i32 = e.builder().ins().ireduce(types::I32, key_meta);
    let val_meta_i32 = e.builder().ins().ireduce(types::I32, packed_meta);
    let key_slots = e.builder().ins().iconst(types::I32, (inst.c >> 8) as i64);
    let val_slots = e.builder().ins().iconst(types::I32, (inst.c & 0xFF) as i64);
    let key_rttid_i32 = e.builder().ins().ireduce(types::I32, key_rttid);
    let call = e.builder().ins().call(func, &[gc_ptr, key_meta_i32, val_meta_i32, key_slots, val_slots, key_rttid_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn map_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().map_len.expect("map_len helper not registered");
    let m = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[m]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

/// Helper: create stack slot and store values from consecutive registers
fn store_to_stack<'a>(e: &mut impl IrEmitter<'a>, start_reg: u16, slots: usize) -> (StackSlot, Value, Value) {
    use cranelift_codegen::ir::{StackSlotData, StackSlotKind};
    let stack_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot, (slots.max(1) * 8) as u32, 8));
    for i in 0..slots {
        let val = e.read_var(start_reg + i as u16);
        e.builder().ins().stack_store(val, stack_slot, (i * 8) as i32);
    }
    let ptr = e.builder().ins().stack_addr(types::I64, stack_slot, 0);
    let slots_i32 = e.builder().ins().iconst(types::I32, slots as i64);
    (stack_slot, ptr, slots_i32)
}

fn map_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().map_get.expect("map_get helper not registered");
    // MapGet: a=dst, b=map, c=meta_slot, key at c+1
    // meta: key_slots<<16 | val_slots<<1 | has_ok
    let meta = e.get_reg_const(inst.c).expect("MapGet: meta not found in reg_consts") as u64;
    let key_slots = ((meta >> 16) & 0xFFFF) as usize;
    let val_slots = ((meta >> 1) & 0x7FFF) as usize;
    let has_ok = (meta & 1) != 0;
    
    let m = e.read_var(inst.b);
    let (_, key_ptr, key_slots_i32) = store_to_stack(e, inst.c + 1, key_slots);
    
    // Create output buffer for val (+ ok flag if needed)
    let out_slots = val_slots + if has_ok { 1 } else { 0 };
    let val_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, (out_slots.max(1) * 8) as u32, 8));
    let val_ptr = e.builder().ins().stack_addr(types::I64, val_slot, 0);
    let val_slots_i32 = e.builder().ins().iconst(types::I32, val_slots as i64);
    
    let ctx = e.ctx_param();
    let call = e.builder().ins().call(func, &[ctx, m, key_ptr, key_slots_i32, val_ptr, val_slots_i32]);
    let found = e.builder().inst_results(call)[0];
    
    // Load results to dst registers
    for i in 0..val_slots {
        let val = e.builder().ins().stack_load(types::I64, val_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    if has_ok {
        e.write_var(inst.a + val_slots as u16, found);
    }
}

fn map_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().map_set.expect("map_set helper not registered");
    // MapSet: a=map, b=meta_slot, c=val_start, key at b+1
    // meta: key_slots<<8 | val_slots
    let meta = e.get_reg_const(inst.b).expect("MapSet: meta not found in reg_consts") as u64;
    let key_slots = ((meta >> 8) & 0xFF) as usize;
    let val_slots = (meta & 0xFF) as usize;
    
    let m = e.read_var(inst.a);
    
    // nil map write panics (Go semantics)
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, m, zero);
    emit_panic_if(e, is_nil, false);
    
    let (_, key_ptr, key_slots_i32) = store_to_stack(e, inst.b + 1, key_slots);
    let (_, val_ptr, val_slots_i32) = store_to_stack(e, inst.c, val_slots);
    
    let ctx = e.ctx_param();
    let call = e.builder().ins().call(func, &[ctx, m, key_ptr, key_slots_i32, val_ptr, val_slots_i32]);
    let result = e.builder().inst_results(call)[0];
    
    // Check if vo_map_set returned panic (unhashable interface key)
    let is_panic = e.builder().ins().icmp(IntCC::NotEqual, result, zero);
    emit_panic_if(e, is_panic, false);
}

fn map_delete<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().map_delete.expect("map_delete helper not registered");
    // MapDelete: a=map, b=meta_slot (=key_slots), key at b+1
    let key_slots = e.get_reg_const(inst.b).expect("MapDelete: meta not found in reg_consts") as usize;
    
    let m = e.read_var(inst.a);
    let (_, key_ptr, key_slots_i32) = store_to_stack(e, inst.b + 1, key_slots);
    
    let ctx = e.ctx_param();
    e.builder().ins().call(func, &[ctx, m, key_ptr, key_slots_i32]);
}

const MAP_ITER_SLOTS: usize = vo_runtime::objects::map::MAP_ITER_SLOTS;
const MAP_ITER_BYTES: u32 = (MAP_ITER_SLOTS * 8) as u32;

fn map_iter_init<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().map_iter_init.expect("map_iter_init helper not registered");
    let m = e.read_var(inst.b);
    let iter_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, MAP_ITER_BYTES, 8));
    let iter_ptr = e.builder().ins().stack_addr(types::I64, iter_slot, 0);
    e.builder().ins().call(func, &[m, iter_ptr]);
    for i in 0..MAP_ITER_SLOTS {
        let val = e.builder().ins().stack_load(types::I64, iter_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
}

fn map_iter_next<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().map_iter_next.expect("map_iter_next helper not registered");
    let key_slots = (inst.flags & 0x0F) as usize;
    let val_slots = ((inst.flags >> 4) & 0x0F) as usize;
    
    let iter_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, MAP_ITER_BYTES, 8));
    let key_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, (key_slots.max(1) * 8) as u32, 8));
    let val_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, (val_slots.max(1) * 8) as u32, 8));
    
    for i in 0..MAP_ITER_SLOTS {
        let val = e.read_var(inst.b + i as u16);
        e.builder().ins().stack_store(val, iter_slot, (i * 8) as i32);
    }
    
    let iter_ptr = e.builder().ins().stack_addr(types::I64, iter_slot, 0);
    let key_ptr = e.builder().ins().stack_addr(types::I64, key_slot, 0);
    let val_ptr = e.builder().ins().stack_addr(types::I64, val_slot, 0);
    let key_slots_i32 = e.builder().ins().iconst(types::I32, key_slots as i64);
    let val_slots_i32 = e.builder().ins().iconst(types::I32, val_slots as i64);
    
    let call = e.builder().ins().call(func, &[iter_ptr, key_ptr, key_slots_i32, val_ptr, val_slots_i32]);
    let ok = e.builder().inst_results(call)[0];
    
    for i in 0..MAP_ITER_SLOTS {
        let val = e.builder().ins().stack_load(types::I64, iter_slot, (i * 8) as i32);
        e.write_var(inst.b + i as u16, val);
    }
    
    // Copy key to VM stack
    for i in 0..key_slots {
        let val = e.builder().ins().stack_load(types::I64, key_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    // Copy val to VM stack (at key_slot + key_slots)
    for i in 0..val_slots {
        let val = e.builder().ins().stack_load(types::I64, val_slot, (i * 8) as i32);
        e.write_var(inst.a + key_slots as u16 + i as u16, val);
    }
    // Write ok flag to inst.c
    e.write_var(inst.c, ok);
}

// =============================================================================
// Closure operations
// =============================================================================

fn closure_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().closure_new.expect("closure_new helper not registered");
    let gc_ptr = e.gc_ptr();
    let func_id = ((inst.flags as u32) << 16) | (inst.b as u32);
    let capture_count = inst.c as u32;
    let func_id_i32 = e.builder().ins().iconst(types::I32, func_id as i64);
    let capture_count_i32 = e.builder().ins().iconst(types::I32, capture_count as i64);
    let call = e.builder().ins().call(func, &[gc_ptr, func_id_i32, capture_count_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn closure_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    use vo_runtime::objects::closure::HEADER_SLOTS;
    let closure = e.read_var(0);
    let capture_idx = inst.b as usize;
    let offset = ((HEADER_SLOTS + capture_idx) * 8) as i32;
    let val = e.builder().ins().load(types::I64, MemFlags::trusted(), closure, offset);
    e.write_var(inst.a, val);
}

// =============================================================================
// Allocation operations
// =============================================================================

fn ptr_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().gc_alloc.expect("gc_alloc helper not registered");
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let slots_i32 = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let call = e.builder().ins().call(func, &[gc_ptr, meta_i32, slots_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn chan_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().chan_new.expect("chan_new helper not registered");
    let gc_ptr = e.gc_ptr();
    let elem_meta = e.read_var(inst.b);
    let elem_meta_i32 = e.builder().ins().ireduce(types::I32, elem_meta);
    let elem_slots_i32 = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let cap = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[gc_ptr, elem_meta_i32, elem_slots_i32, cap]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn chan_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().chan_len.expect("chan_len helper not registered");
    let ch = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[ch]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn chan_cap<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().chan_cap.expect("chan_cap helper not registered");
    let ch = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[ch]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn port_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().port_new.expect("port_new helper not registered");
    let gc_ptr = e.gc_ptr();
    let elem_meta = e.read_var(inst.b);
    let elem_meta_i32 = e.builder().ins().ireduce(types::I32, elem_meta);
    let elem_slots_i32 = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let cap = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[gc_ptr, elem_meta_i32, elem_slots_i32, cap]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn port_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().port_len.expect("port_len helper not registered");
    let p = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[p]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn port_cap<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().port_cap.expect("port_cap helper not registered");
    let p = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[p]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

// =============================================================================
// Interface operations
// =============================================================================

fn str_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    use vo_runtime::bytecode::Constant;
    let func = e.helpers().str_new.expect("str_new helper not registered");
    let const_idx = inst.b as usize;
    let bytes: Vec<u8> = if let Constant::String(s) = &e.vo_module().constants[const_idx] {
        s.as_bytes().to_vec()
    } else {
        let zero = e.builder().ins().iconst(types::I64, 0);
        e.write_var(inst.a, zero);
        return;
    };
    let len = bytes.len();
    if len == 0 {
        let zero = e.builder().ins().iconst(types::I64, 0);
        e.write_var(inst.a, zero);
    } else {
        let gc_ptr = e.gc_ptr();
        let stack_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot, len as u32, 0));
        for (i, &b) in bytes.iter().enumerate() {
            let byte_val = e.builder().ins().iconst(types::I8, b as i64);
            e.builder().ins().stack_store(byte_val, stack_slot, i as i32);
        }
        let data_ptr = e.builder().ins().stack_addr(types::I64, stack_slot, 0);
        let len_val = e.builder().ins().iconst(types::I64, len as i64);
        let call = e.builder().ins().call(func, &[gc_ptr, data_ptr, len_val]);
        let result = e.builder().inst_results(call)[0];
        e.write_var(inst.a, result);
    }
}

fn iface_assign<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    use vo_runtime::bytecode::Constant;
    let vk = inst.flags;
    let src = e.read_var(inst.b);
    
    // ValueKind: Array=14, Struct=15, Interface=16
    let (slot0, slot1) = if vk == 16 {
        // Interface source: preserve rttid/vk from source, update itab_id
        // For interface->any (iface_meta_id=0), itab_id must be 0
        let const_idx = inst.c as usize;
        let iface_meta_id = if let Constant::Int(packed) = &e.vo_module().constants[const_idx] {
            (*packed & 0xFFFFFFFF) as u32
        } else { 0 };
        
        let src_slot0 = src;
        let src_slot1 = e.read_var(inst.b + 1);
        
        if iface_meta_id == 0 {
            // Target is any: itab_id=0, preserve rttid and vk from source
            // slot0 format: [itab_id:32 | rttid:24 | vk:8]
            // Clear top 32 bits (itab_id), keep bottom 32 bits (rttid | vk)
            let mask = e.builder().ins().iconst(types::I64, 0x00000000_FFFFFFFF_u64 as i64);
            let new_slot0 = e.builder().ins().band(src_slot0, mask);
            (new_slot0, src_slot1)
        } else {
            // Target is non-empty interface: runtime itab lookup
            if let Some(iface_to_iface_func) = e.helpers().iface_to_iface {
                let ctx = e.ctx_param();
                let iface_meta_id_val = e.builder().ins().iconst(types::I32, iface_meta_id as i64);
                let call = e.builder().ins().call(iface_to_iface_func, &[ctx, src_slot0, iface_meta_id_val]);
                let new_slot0 = e.builder().inst_results(call)[0];
                (new_slot0, src_slot1)
            } else {
                (src_slot0, src_slot1)
            }
        }
    } else {
        // Concrete type source: use compile-time constants
        let const_idx = inst.c as usize;
        let (rttid, itab_id) = if let Constant::Int(packed) = &e.vo_module().constants[const_idx] {
            let rttid = (*packed >> 32) as u32;
            let itab_id = (*packed & 0xFFFFFFFF) as u32;
            (rttid, itab_id)
        } else {
            (0, 0)
        };
        let itab_shifted = (itab_id as u64) << 32;
        let rttid_shifted = (rttid as u64) << 8;
        let slot0_val = itab_shifted | rttid_shifted | (vk as u64);
        let slot0 = e.builder().ins().iconst(types::I64, slot0_val as i64);
        
        let slot1 = if vk == 14 || vk == 15 {
            // Struct or Array: ptr_clone the GcRef
            if let Some(ptr_clone_func) = e.helpers().ptr_clone {
                let gc_ptr = e.gc_ptr();
                let call = e.builder().ins().call(ptr_clone_func, &[gc_ptr, src]);
                e.builder().inst_results(call)[0]
            } else { src }
        } else { src };
        (slot0, slot1)
    };
    
    e.write_var(inst.a, slot0);
    e.write_var(inst.a + 1, slot1);
}

fn iface_assert<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().iface_assert.expect("iface_assert helper not registered");
    let ctx = e.ctx_param();
    let slot0 = e.read_var(inst.b);
    let slot1 = e.read_var(inst.b + 1);
    let target_id_i32 = e.builder().ins().iconst(types::I32, inst.c as i64);
    let flags_i16 = e.builder().ins().iconst(types::I16, inst.flags as i64);
    let has_ok = ((inst.flags >> 2) & 0x1) != 0;
    let assert_kind = inst.flags & 0x3;
    let target_slots = (inst.flags >> 3) as usize;
    let result_slots = if assert_kind == 1 { 3 } else { target_slots.max(1) + 1 };
    let result_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, (result_slots * 8) as u32, 8));
    let dst_ptr = e.builder().ins().stack_addr(types::I64, result_slot, 0);
    let call = e.builder().ins().call(func, &[ctx, slot0, slot1, target_id_i32, flags_i16, dst_ptr]);
    let result = e.builder().inst_results(call)[0];
    if !has_ok {
        let zero = e.builder().ins().iconst(types::I64, 0);
        let is_panic = e.builder().ins().icmp(IntCC::Equal, result, zero);
        emit_panic_if(e, is_panic, false);
    }
    let dst_slots = if assert_kind == 1 { 2 } else { target_slots.max(1) };
    for i in 0..dst_slots {
        let val = e.builder().ins().stack_load(types::I64, result_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    if has_ok {
        let ok_offset = if assert_kind == 1 { 2 } else { target_slots.max(1) };
        let ok_val = e.builder().ins().stack_load(types::I64, result_slot, (ok_offset * 8) as i32);
        e.write_var(inst.a + ok_offset as u16, ok_val);
    }
}

fn iface_eq<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let b0 = e.read_var(inst.b);
    let b1 = e.read_var(inst.b + 1);
    let c0 = e.read_var(inst.c);
    let c1 = e.read_var(inst.c + 1);
    
    // Call runtime helper for correct comparison (handles string content, struct/array deep eq, etc.)
    // Returns: 0=false, 1=true, 2=panic (uncomparable type)
    let iface_eq_func = e.helpers().iface_eq.expect("iface_eq helper must be available");
    let ctx = e.ctx_param();
    let call = e.builder().ins().call(iface_eq_func, &[ctx, b0, b1, c0, c1]);
    let result = e.builder().inst_results(call)[0];
    
    // Check if result == 2 (panic for uncomparable type)
    let two = e.builder().ins().iconst(types::I64, 2);
    let is_panic = e.builder().ins().icmp(IntCC::Equal, result, two);
    emit_panic_if(e, is_panic, false);
    
    // Mask result to 0 or 1 (already know it's not 2)
    let one = e.builder().ins().iconst(types::I64, 1);
    let masked = e.builder().ins().band(result, one);
    e.write_var(inst.a, masked);
}
