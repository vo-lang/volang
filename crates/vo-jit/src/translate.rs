//! Shared instruction translation logic.

use cranelift_codegen::ir::{types, InstBuilder, MemFlags};
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
        ModI => { mod_i(e, inst); Ok(Completed) }
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
        EqF => { cmp_f(e, inst, FloatCC::Equal); Ok(Completed) }
        NeF => { cmp_f(e, inst, FloatCC::NotEqual); Ok(Completed) }
        LtF => { cmp_f(e, inst, FloatCC::LessThan); Ok(Completed) }
        LeF => { cmp_f(e, inst, FloatCC::LessThanOrEqual); Ok(Completed) }
        GtF => { cmp_f(e, inst, FloatCC::GreaterThan); Ok(Completed) }
        GeF => { cmp_f(e, inst, FloatCC::GreaterThanOrEqual); Ok(Completed) }
        Not => { not(e, inst); Ok(Completed) }
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
        SlotGet => { slot_get(e, inst); Ok(Completed) }
        SlotSet => { slot_set(e, inst); Ok(Completed) }
        SlotGetN => { slot_get_n(e, inst); Ok(Completed) }
        SlotSetN => { slot_set_n(e, inst); Ok(Completed) }
        ConvI2F => { conv_i2f(e, inst); Ok(Completed) }
        ConvF2I => { conv_f2i(e, inst); Ok(Completed) }
        ConvI32I64 => { conv_i32_i64(e, inst); Ok(Completed) }
        ConvI64I32 => { conv_i64_i32(e, inst); Ok(Completed) }
        ConvF64F32 => { conv_f64_f32(e, inst); Ok(Completed) }
        ConvF32F64 => { conv_f32_f64(e, inst); Ok(Completed) }
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
        // Control flow - compiler specific
        Jump | JumpIf | JumpIfNot | Return | Panic => Ok(Unhandled),
        // Function calls - compiler specific
        Call | CallExtern | CallClosure | CallIface => Ok(Unhandled),
        // Compiler specific (needs constant table access)
        StrNew | IfaceAssign => Ok(Unhandled),
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
    let module = e.vo_module();
    let (val, reg_const) = match &module.constants[const_idx] {
        Constant::Nil => (0i64, Some(0i64)),
        Constant::Bool(b) => (*b as i64, Some(*b as i64)),
        Constant::Int(i) => (*i, Some(*i)),
        Constant::Float(f) => (f.to_bits() as i64, Some(f.to_bits() as i64)),
        Constant::String(_) => (0, None),
    };
    if let Some(c) = reg_const { e.set_reg_const(inst.a, c); }
    let v = e.builder().ins().iconst(types::I64, val);
    e.write_var(inst.a, v);
}

fn copy<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let v = e.read_var(inst.b);
    e.write_var(inst.a, v);
}

fn copy_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    for i in 0..inst.flags as usize {
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
    let r = e.builder().ins().sdiv(a, b);
    e.write_var(inst.a, r);
}

fn mod_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().srem(a, b);
    e.write_var(inst.a, r);
}

fn neg_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let r = e.builder().ins().ineg(a);
    e.write_var(inst.a, r);
}

fn add_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let fa = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let fb = e.builder().ins().bitcast(types::F64, MemFlags::new(), b);
    let fr = e.builder().ins().fadd(fa, fb);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), fr);
    e.write_var(inst.a, r);
}

fn sub_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let fa = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let fb = e.builder().ins().bitcast(types::F64, MemFlags::new(), b);
    let fr = e.builder().ins().fsub(fa, fb);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), fr);
    e.write_var(inst.a, r);
}

fn mul_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let fa = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let fb = e.builder().ins().bitcast(types::F64, MemFlags::new(), b);
    let fr = e.builder().ins().fmul(fa, fb);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), fr);
    e.write_var(inst.a, r);
}

fn div_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let fa = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let fb = e.builder().ins().bitcast(types::F64, MemFlags::new(), b);
    let fr = e.builder().ins().fdiv(fa, fb);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), fr);
    e.write_var(inst.a, r);
}

fn neg_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let fa = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let fr = e.builder().ins().fneg(fa);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), fr);
    e.write_var(inst.a, r);
}

fn cmp_i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, cc: IntCC) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let cmp = e.builder().ins().icmp(cc, a, b);
    let r = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, r);
}

fn cmp_f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, cc: FloatCC) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let fa = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let fb = e.builder().ins().bitcast(types::F64, MemFlags::new(), b);
    let cmp = e.builder().ins().fcmp(cc, fa, fb);
    let r = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, r);
}

fn not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
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

fn shl<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().ishl(a, b);
    e.write_var(inst.a, r);
}

fn shr_s<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().sshr(a, b);
    e.write_var(inst.a, r);
}

fn shr_u<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b); let b = e.read_var(inst.c);
    let r = e.builder().ins().ushr(a, b);
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

fn ptr_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.b);
    let offset = (inst.c as i32) * 8;
    let v = e.builder().ins().load(types::I64, MemFlags::trusted(), ptr, offset);
    e.write_var(inst.a, v);
}

fn ptr_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.a);
    let v = e.read_var(inst.c);
    let offset = (inst.b as i32) * 8;
    e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
}

fn ptr_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.b);
    for i in 0..inst.flags as usize {
        let offset = ((inst.c as usize + i) * 8) as i32;
        let v = e.builder().ins().load(types::I64, MemFlags::trusted(), ptr, offset);
        e.write_var(inst.a + i as u16, v);
    }
}

fn ptr_set_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.a);
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.c + i as u16);
        let offset = ((inst.b as usize + i) * 8) as i32;
        e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    }
}

fn slot_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm(idx, 8);
    let addr = e.builder().ins().iadd(base, offset);
    let v = e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0);
    e.write_var(inst.a, v);
}

fn slot_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.read_var(inst.a);
    let idx = e.read_var(inst.b);
    let v = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm(idx, 8);
    let addr = e.builder().ins().iadd(base, offset);
    e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
}

fn slot_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.read_var(inst.b);
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
    let base = e.read_var(inst.a);
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
    let a = e.read_var(inst.b);
    let f = e.builder().ins().fcvt_from_sint(types::F64, a);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), f);
    e.write_var(inst.a, r);
}

fn conv_f2i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let f = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let r = e.builder().ins().fcvt_to_sint(types::I64, f);
    e.write_var(inst.a, r);
}

fn conv_i32_i64<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let t = e.builder().ins().ireduce(types::I32, a);
    let r = e.builder().ins().sextend(types::I64, t);
    e.write_var(inst.a, r);
}

fn conv_i64_i32<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let t = e.builder().ins().ireduce(types::I32, a);
    let r = e.builder().ins().uextend(types::I64, t);
    e.write_var(inst.a, r);
}

fn conv_f64_f32<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let f64v = e.builder().ins().bitcast(types::F64, MemFlags::new(), a);
    let f32v = e.builder().ins().fdemote(types::F32, f64v);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), f32v);
    e.write_var(inst.a, r);
}

fn conv_f32_f64<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let i32v = e.builder().ins().ireduce(types::I32, a);
    let f32v = e.builder().ins().bitcast(types::F32, MemFlags::new(), i32v);
    let f64v = e.builder().ins().fpromote(types::F64, f32v);
    let r = e.builder().ins().bitcast(types::I64, MemFlags::new(), f64v);
    e.write_var(inst.a, r);
}

// =============================================================================
// Slice operations
// =============================================================================

const SLICE_FIELD_DATA_PTR: i32 = 0;

fn slice_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_new_func = match e.helpers().slice_new {
        Some(f) => f,
        None => return,
    };
    let elem_bytes: usize = match inst.flags {
        0 => 0,
        0x81 => 1,
        0x82 => 2,
        0x84 | 0x44 => 4,
        f => f as usize,
    };
    let elem_bytes_val = if inst.flags == 0 {
        let eb = e.read_var(inst.c + 2);
        e.builder().ins().ireduce(types::I32, eb)
    } else {
        e.builder().ins().iconst(types::I32, elem_bytes as i64)
    };
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
    let (elem_bytes, needs_sext) = match inst.flags {
        0 => (e.get_reg_const(inst.c + 1).unwrap_or(8) as usize, false),
        0x81 => (1, true),
        0x82 => (2, true),
        0x84 => (4, true),
        0x44 => (4, false),
        f => (f as usize, false),
    };
    let data_ptr = e.builder().ins().load(types::I64, MemFlags::trusted(), s, SLICE_FIELD_DATA_PTR);
    if elem_bytes <= 8 {
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
        let addr = e.builder().ins().iadd(data_ptr, off);
        let val = match elem_bytes {
            1 => {
                let v8 = e.builder().ins().load(types::I8, MemFlags::trusted(), addr, 0);
                if needs_sext { e.builder().ins().sextend(types::I64, v8) }
                else { e.builder().ins().uextend(types::I64, v8) }
            }
            2 => {
                let v16 = e.builder().ins().load(types::I16, MemFlags::trusted(), addr, 0);
                if needs_sext { e.builder().ins().sextend(types::I64, v16) }
                else { e.builder().ins().uextend(types::I64, v16) }
            }
            4 => {
                let v32 = e.builder().ins().load(types::I32, MemFlags::trusted(), addr, 0);
                if needs_sext { e.builder().ins().sextend(types::I64, v32) }
                else { e.builder().ins().uextend(types::I64, v32) }
            }
            _ => e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0),
        };
        e.write_var(inst.a, val);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
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
    let val = e.read_var(inst.c);
    let elem_bytes = match inst.flags {
        0 => e.get_reg_const(inst.b + 1).unwrap_or(8) as usize,
        0x81 => 1,
        0x82 => 2,
        0x84 | 0x44 => 4,
        f => f as usize,
    };
    let data_ptr = e.builder().ins().load(types::I64, MemFlags::trusted(), s, SLICE_FIELD_DATA_PTR);
    if elem_bytes <= 8 {
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
        let addr = e.builder().ins().iadd(data_ptr, off);
        match elem_bytes {
            1 => {
                let v8 = e.builder().ins().ireduce(types::I8, val);
                e.builder().ins().store(MemFlags::trusted(), v8, addr, 0);
            }
            2 => {
                let v16 = e.builder().ins().ireduce(types::I16, val);
                e.builder().ins().store(MemFlags::trusted(), v16, addr, 0);
            }
            4 => {
                let v32 = e.builder().ins().ireduce(types::I32, val);
                e.builder().ins().store(MemFlags::trusted(), v32, addr, 0);
            }
            _ => { e.builder().ins().store(MemFlags::trusted(), val, addr, 0); }
        }
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
        for i in 0..elem_slots {
            let v = e.read_var(inst.c + i as u16);
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(data_ptr, slot_off);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
    }
}

fn slice_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_len_func = match e.helpers().slice_len {
        Some(f) => f,
        None => return,
    };
    let s = e.read_var(inst.b);
    let call = e.builder().ins().call(slice_len_func, &[s]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn slice_cap<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_cap_func = match e.helpers().slice_cap {
        Some(f) => f,
        None => return,
    };
    let s = e.read_var(inst.b);
    let call = e.builder().ins().call(slice_cap_func, &[s]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn slice_slice<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_slice_func = match e.helpers().slice_slice {
        Some(f) => f,
        None => return,
    };
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);
    let call = e.builder().ins().call(slice_slice_func, &[gc_ptr, s, lo, hi]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn slice_append<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_append_func = match e.helpers().slice_append {
        Some(f) => f,
        None => return,
    };
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    let elem_ptr = e.read_var(inst.c);
    let elem_slots = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let call = e.builder().ins().call(slice_append_func, &[gc_ptr, s, elem_ptr, elem_slots]);
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
    let array_new_func = match e.helpers().array_new {
        Some(f) => f,
        None => return,
    };
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let elem_slots_i32 = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let len = e.read_var(inst.c);
    let call = e.builder().ins().call(array_new_func, &[gc_ptr, meta_i32, elem_slots_i32, len]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn array_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let arr = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let (elem_bytes, needs_sext) = match inst.flags {
        0 => (e.get_reg_const(inst.c + 1).unwrap_or(8) as usize, false),
        0x81 => (1, true),
        0x82 => (2, true),
        0x84 => (4, true),
        0x44 => (4, false),
        f => (f as usize, false),
    };
    if elem_bytes <= 8 {
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
        let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
        let addr = e.builder().ins().iadd(arr, off);
        let val = match elem_bytes {
            1 => {
                let v8 = e.builder().ins().load(types::I8, MemFlags::trusted(), addr, 0);
                if needs_sext { e.builder().ins().sextend(types::I64, v8) }
                else { e.builder().ins().uextend(types::I64, v8) }
            }
            2 => {
                let v16 = e.builder().ins().load(types::I16, MemFlags::trusted(), addr, 0);
                if needs_sext { e.builder().ins().sextend(types::I64, v16) }
                else { e.builder().ins().uextend(types::I64, v16) }
            }
            4 => {
                let v32 = e.builder().ins().load(types::I32, MemFlags::trusted(), addr, 0);
                if needs_sext { e.builder().ins().sextend(types::I64, v32) }
                else { e.builder().ins().uextend(types::I64, v32) }
            }
            _ => e.builder().ins().load(types::I64, MemFlags::trusted(), addr, 0),
        };
        e.write_var(inst.a, val);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
        let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
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
    let val = e.read_var(inst.c);
    let elem_bytes = match inst.flags {
        0 => e.get_reg_const(inst.b + 1).unwrap_or(8) as usize,
        0x81 => 1,
        0x82 => 2,
        0x84 | 0x44 => 4,
        f => f as usize,
    };
    if elem_bytes <= 8 {
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
        let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
        let addr = e.builder().ins().iadd(arr, off);
        match elem_bytes {
            1 => {
                let v8 = e.builder().ins().ireduce(types::I8, val);
                e.builder().ins().store(MemFlags::trusted(), v8, addr, 0);
            }
            2 => {
                let v16 = e.builder().ins().ireduce(types::I16, val);
                e.builder().ins().store(MemFlags::trusted(), v16, addr, 0);
            }
            4 => {
                let v32 = e.builder().ins().ireduce(types::I32, val);
                e.builder().ins().store(MemFlags::trusted(), v32, addr, 0);
            }
            _ => { e.builder().ins().store(MemFlags::trusted(), val, addr, 0); }
        }
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
        let off = e.builder().ins().imul(idx, eb);
        let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
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
