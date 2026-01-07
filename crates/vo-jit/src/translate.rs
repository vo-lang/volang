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
        SlotGet => { slot_get(e, inst); Ok(Completed) }
        SlotSet => { slot_set(e, inst); Ok(Completed) }
        SlotGetN => { slot_get_n(e, inst); Ok(Completed) }
        SlotSetN => { slot_set_n(e, inst); Ok(Completed) }
        ConvI2F => { conv_i2f(e, inst); Ok(Completed) }
        ConvF2I => { conv_f2i(e, inst); Ok(Completed) }
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
        MapIterGet => { map_iter_get(e, inst); Ok(Completed) }
        // Closure operations
        ClosureNew => { closure_new(e, inst); Ok(Completed) }
        ClosureGet => { closure_get(e, inst); Ok(Completed) }
        // Allocation
        PtrNew => { ptr_new(e, inst); Ok(Completed) }
        ChanNew => { chan_new(e, inst); Ok(Completed) }
        // Interface
        IfaceAssert => { iface_assert(e, inst); Ok(Completed) }
        StrNew => { str_new(e, inst); Ok(Completed) }
        IfaceAssign => { iface_assign(e, inst); Ok(Completed) }
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

fn bool_not<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // BoolNot: if input == 0, output 1; else output 0
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

/// Resolve elem_bytes for SliceAppend/ArrayNew etc.
/// Returns elem_slots (not elem_bytes).
fn resolve_elem_slots<'a>(e: &impl IrEmitter<'a>, flags: u8, eb_reg: u16) -> usize {
    let elem_bytes = match flags {
        0 => e.get_reg_const(eb_reg).unwrap() as usize,
        0x81 | 0x82 => 1,
        0x84 | 0x44 => 1,
        f => f as usize,
    };
    if flags == 0 { (elem_bytes + 7) / 8 } else { elem_bytes }
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
use cranelift_codegen::ir::Value;
const SLICE_FIELD_DATA_PTR: i32 = (SLICE_FIELD_DATA_PTR_SLOT * 8) as i32;
const SLICE_FIELD_LEN: i32 = (SLICE_FIELD_LEN_SLOT * 8) as i32;

/// Emit bounds check for slice access. Panics if idx >= len or slice is nil.
/// Returns data_ptr for the slice (only valid if bounds check passed).
fn emit_slice_bounds_check<'a>(e: &mut impl IrEmitter<'a>, s: Value, idx: Value) -> Value {
    // If s is nil, len=0; otherwise load len from slice
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, s, zero);
    let nil_block = e.builder().create_block();
    let not_nil_block = e.builder().create_block();
    let merge_block = e.builder().create_block();
    e.builder().append_block_param(merge_block, types::I64); // len
    e.builder().ins().brif(is_nil, nil_block, &[], not_nil_block, &[]);
    
    e.builder().switch_to_block(nil_block);
    e.builder().seal_block(nil_block);
    e.builder().ins().jump(merge_block, &[zero]);
    
    e.builder().switch_to_block(not_nil_block);
    e.builder().seal_block(not_nil_block);
    let len_from_slice = e.builder().ins().load(types::I64, MemFlags::trusted(), s, SLICE_FIELD_LEN);
    e.builder().ins().jump(merge_block, &[len_from_slice]);
    
    e.builder().switch_to_block(merge_block);
    e.builder().seal_block(merge_block);
    let len = e.builder().block_params(merge_block)[0];
    
    // Check idx >= len
    let out_of_bounds = e.builder().ins().icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    let panic_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder().ins().brif(out_of_bounds, panic_block, &[], ok_block, &[]);
    
    e.builder().switch_to_block(panic_block);
    e.builder().seal_block(panic_block);
    let panic_ret_val = e.panic_return_value();
    let panic_ret = e.builder().ins().iconst(types::I32, panic_ret_val as i64);
    e.builder().ins().return_(&[panic_ret]);
    
    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
    
    e.builder().ins().load(types::I64, MemFlags::trusted(), s, SLICE_FIELD_DATA_PTR)
}

fn slice_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_new_func = match e.helpers().slice_new {
        Some(f) => f,
        None => return,
    };
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
    let (elem_bytes, _) = resolve_elem_bytes(e, inst.flags, inst.b + 1);
    
    let data_ptr = emit_slice_bounds_check(e, s, idx);
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
    let gc_ptr = e.gc_ptr();
    let src = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);
    
    let is_array = (inst.flags & 0b01) != 0;
    let has_max = (inst.flags & 0b10) != 0;
    
    let result = if is_array {
        if has_max {
            let max = e.read_var(inst.c + 2);
            let func = e.helpers().slice_from_array3.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi, max]);
            e.builder().inst_results(call)[0]
        } else {
            let func = e.helpers().slice_from_array.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi]);
            e.builder().inst_results(call)[0]
        }
    } else {
        if has_max {
            let max = e.read_var(inst.c + 2);
            let func = e.helpers().slice_slice3.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi, max]);
            e.builder().inst_results(call)[0]
        } else {
            let func = e.helpers().slice_slice.unwrap();
            let call = e.builder().ins().call(func, &[gc_ptr, src, lo, hi]);
            e.builder().inst_results(call)[0]
        }
    };
    e.write_var(inst.a, result);
}

fn slice_append<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let slice_append_func = match e.helpers().slice_append {
        Some(f) => f,
        None => return,
    };
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    
    // flags==0: elem at c+2 (c+1 is elem_bytes); flags!=0: elem at c+1
    let elem_slots = resolve_elem_slots(e, inst.flags, inst.c + 1);
    let elem_ptr = e.read_var(inst.c + if inst.flags == 0 { 2 } else { 1 });
    let elem_slots_val = e.builder().ins().iconst(types::I32, elem_slots as i64);
    
    let call = e.builder().ins().call(slice_append_func, &[gc_ptr, s, elem_ptr, elem_slots_val]);
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
    let elem_slots_i32 = emit_elem_slots_i32(e, inst.flags, inst.c + 1);
    let len = e.read_var(inst.c);
    let call = e.builder().ins().call(array_new_func, &[gc_ptr, meta_i32, elem_slots_i32, len]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn array_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let arr = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let (elem_bytes, needs_sext) = resolve_elem_bytes(e, inst.flags, inst.c + 1);
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
    let (elem_bytes, _) = resolve_elem_bytes(e, inst.flags, inst.b + 1);
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

// =============================================================================
// String operations
// =============================================================================

fn str_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().str_len { Some(f) => f, None => return };
    let s = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[s]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_index<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().str_index { Some(f) => f, None => return };
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[s, idx]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_concat<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().str_concat { Some(f) => f, None => return };
    let gc_ptr = e.gc_ptr();
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[gc_ptr, a, b]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_slice<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().str_slice { Some(f) => f, None => return };
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);
    let call = e.builder().ins().call(func, &[gc_ptr, s, lo, hi]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_eq<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().str_eq { Some(f) => f, None => return };
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[a, b]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn str_ne<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().str_eq { Some(f) => f, None => return };
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
    let func = match e.helpers().str_cmp { Some(f) => f, None => return };
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[a, b]);
    let cmp_result = e.builder().inst_results(call)[0];
    let zero = e.builder().ins().iconst(types::I64, 0);
    let cmp = e.builder().ins().icmp(cc, cmp_result, zero);
    let result = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, result);
}

fn str_decode_rune<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().str_decode_rune { Some(f) => f, None => return };
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[s, idx]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

// =============================================================================
// Map operations
// =============================================================================

fn map_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().map_new { Some(f) => f, None => return };
    let gc_ptr = e.gc_ptr();
    let packed_meta = e.read_var(inst.b);
    let key_meta = e.builder().ins().ushr_imm(packed_meta, 32);
    let key_meta_i32 = e.builder().ins().ireduce(types::I32, key_meta);
    let val_meta_i32 = e.builder().ins().ireduce(types::I32, packed_meta);
    let key_slots = e.builder().ins().iconst(types::I32, (inst.c >> 8) as i64);
    let val_slots = e.builder().ins().iconst(types::I32, (inst.c & 0xFF) as i64);
    let call = e.builder().ins().call(func, &[gc_ptr, key_meta_i32, val_meta_i32, key_slots, val_slots]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn map_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().map_len { Some(f) => f, None => return };
    let m = e.read_var(inst.b);
    let call = e.builder().ins().call(func, &[m]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn map_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().map_get { Some(f) => f, None => return };
    let m = e.read_var(inst.a);
    let key = e.read_var(inst.b);
    let key_bytes = e.builder().ins().iconst(types::I64, inst.c as i64);
    let val_bytes = e.builder().ins().iconst(types::I64, inst.flags as i64);
    let call = e.builder().ins().call(func, &[m, key, key_bytes, val_bytes]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn map_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().map_set { Some(f) => f, None => return };
    let gc_ptr = e.gc_ptr();
    let m = e.read_var(inst.a);
    
    // nil map write panics (Go semantics)
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, m, zero);
    let panic_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder().ins().brif(is_nil, panic_block, &[], ok_block, &[]);
    
    e.builder().switch_to_block(panic_block);
    e.builder().seal_block(panic_block);
    let panic_ret_val = e.panic_return_value();
    let panic_ret = e.builder().ins().iconst(types::I32, panic_ret_val as i64);
    e.builder().ins().return_(&[panic_ret]);
    
    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
    
    let key = e.read_var(inst.b);
    let val = e.read_var(inst.c);
    let key_bytes = e.builder().ins().iconst(types::I64, (inst.flags & 0x0F) as i64);
    let val_bytes = e.builder().ins().iconst(types::I64, (inst.flags >> 4) as i64);
    e.builder().ins().call(func, &[gc_ptr, m, key, val, key_bytes, val_bytes]);
}

fn map_delete<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().map_delete { Some(f) => f, None => return };
    let m = e.read_var(inst.a);
    let key = e.read_var(inst.b);
    let key_bytes = e.builder().ins().iconst(types::I64, inst.c as i64);
    e.builder().ins().call(func, &[m, key, key_bytes]);
}

fn map_iter_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().map_iter_get { Some(f) => f, None => return };
    let m = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let key_slots = (inst.flags >> 4) as usize;
    let val_slots = (inst.flags & 0xF) as usize;
    let key_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, (key_slots.max(1) * 8) as u32, 8));
    let val_slot = e.builder().create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot, (val_slots.max(1) * 8) as u32, 8));
    let key_ptr = e.builder().ins().stack_addr(types::I64, key_slot, 0);
    let val_ptr = e.builder().ins().stack_addr(types::I64, val_slot, 0);
    let key_slots_i32 = e.builder().ins().iconst(types::I32, key_slots as i64);
    let val_slots_i32 = e.builder().ins().iconst(types::I32, val_slots as i64);
    let call = e.builder().ins().call(func, &[m, idx, key_ptr, key_slots_i32, val_ptr, val_slots_i32]);
    let done = e.builder().inst_results(call)[0];
    for i in 0..key_slots {
        let val = e.builder().ins().stack_load(types::I64, key_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    for i in 0..val_slots {
        let val = e.builder().ins().stack_load(types::I64, val_slot, (i * 8) as i32);
        e.write_var(inst.a + key_slots as u16 + i as u16, val);
    }
    e.write_var(inst.a + (key_slots + val_slots) as u16, done);
}

// =============================================================================
// Closure operations
// =============================================================================

fn closure_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().closure_new { Some(f) => f, None => return };
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
    let func = match e.helpers().gc_alloc { Some(f) => f, None => return };
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let slots_i32 = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let call = e.builder().ins().call(func, &[gc_ptr, meta_i32, slots_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

fn chan_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = match e.helpers().chan_new { Some(f) => f, None => return };
    let gc_ptr = e.gc_ptr();
    let elem_meta = e.read_var(inst.b);
    let elem_meta_i32 = e.builder().ins().ireduce(types::I32, elem_meta);
    let elem_slots_i32 = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let cap = e.read_var(inst.c);
    let call = e.builder().ins().call(func, &[gc_ptr, elem_meta_i32, elem_slots_i32, cap]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

// =============================================================================
// Interface operations
// =============================================================================

fn str_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    use vo_runtime::bytecode::Constant;
    let func = match e.helpers().str_new { Some(f) => f, None => return };
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
        // For interface->other interface, would need runtime itab lookup (not implemented)
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
    let func = match e.helpers().iface_assert { Some(f) => f, None => return };
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
        let panic_ret = e.panic_return_value();
        let zero = e.builder().ins().iconst(types::I64, 0);
        let is_panic = e.builder().ins().icmp(IntCC::Equal, result, zero);
        let panic_block = e.builder().create_block();
        let continue_block = e.builder().create_block();
        e.builder().ins().brif(is_panic, panic_block, &[], continue_block, &[]);
        e.builder().switch_to_block(panic_block);
        e.builder().seal_block(panic_block);
        let panic_val = e.builder().ins().iconst(types::I32, panic_ret as i64);
        e.builder().ins().return_(&[panic_val]);
        e.builder().switch_to_block(continue_block);
        e.builder().seal_block(continue_block);
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
