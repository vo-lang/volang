//! Instruction translation: individual bytecode -> Cranelift IR.
//!
//! This module contains shared instruction translation logic.
//! The `translate_inst` function is the unified entry point used by both
//! FunctionCompiler and LoopCompiler.

use cranelift_codegen::ir::{types, InstBuilder, Value};
use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};

use vo_runtime::instruction::{Instruction, Opcode};

use crate::translator::BytecodeTranslator;
use crate::JitError;

// =============================================================================
// TranslateResult and unified entry point
// =============================================================================

/// Result of translating a single instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslateResult {
    /// Instruction was fully handled.
    Completed,
    /// Instruction requires compiler-specific handling.
    Unhandled,
    /// Instruction terminated the block (e.g., return, jump).
    Terminated,
}

/// Unified instruction translation entry point.
/// 
/// Returns `Completed` for most instructions, `Unhandled` for control flow
/// and function calls that need compiler-specific handling.
pub fn translate_inst(t: &mut impl BytecodeTranslator, inst: &Instruction) -> Result<TranslateResult, JitError> {
    use TranslateResult::*;
    
    match inst.opcode() {
        Opcode::Hint => Ok(Completed),
        
        // Load/Copy
        Opcode::LoadInt => { translate_load_int(t, inst); Ok(Completed) }
        Opcode::LoadConst => { translate_load_const(t, inst); Ok(Completed) }
        Opcode::Copy => { translate_copy(t, inst); Ok(Completed) }
        Opcode::CopyN => { translate_copy_n(t, inst); Ok(Completed) }
        
        // Integer arithmetic
        Opcode::AddI => { translate_add_i(t, inst); Ok(Completed) }
        Opcode::SubI => { translate_sub_i(t, inst); Ok(Completed) }
        Opcode::MulI => { translate_mul_i(t, inst); Ok(Completed) }
        Opcode::DivI => { translate_div_i(t, inst); Ok(Completed) }
        Opcode::ModI => { translate_mod_i(t, inst); Ok(Completed) }
        Opcode::NegI => { translate_neg_i(t, inst); Ok(Completed) }
        
        // Float arithmetic
        Opcode::AddF => { translate_add_f(t, inst); Ok(Completed) }
        Opcode::SubF => { translate_sub_f(t, inst); Ok(Completed) }
        Opcode::MulF => { translate_mul_f(t, inst); Ok(Completed) }
        Opcode::DivF => { translate_div_f(t, inst); Ok(Completed) }
        Opcode::NegF => { translate_neg_f(t, inst); Ok(Completed) }
        
        // Integer comparison
        Opcode::EqI => { translate_cmp_i(t, inst, IntCC::Equal); Ok(Completed) }
        Opcode::NeI => { translate_cmp_i(t, inst, IntCC::NotEqual); Ok(Completed) }
        Opcode::LtI => { translate_cmp_i(t, inst, IntCC::SignedLessThan); Ok(Completed) }
        Opcode::LeI => { translate_cmp_i(t, inst, IntCC::SignedLessThanOrEqual); Ok(Completed) }
        Opcode::GtI => { translate_cmp_i(t, inst, IntCC::SignedGreaterThan); Ok(Completed) }
        Opcode::GeI => { translate_cmp_i(t, inst, IntCC::SignedGreaterThanOrEqual); Ok(Completed) }
        
        // Float comparison
        Opcode::EqF => { translate_cmp_f(t, inst, FloatCC::Equal); Ok(Completed) }
        Opcode::NeF => { translate_cmp_f(t, inst, FloatCC::NotEqual); Ok(Completed) }
        Opcode::LtF => { translate_cmp_f(t, inst, FloatCC::LessThan); Ok(Completed) }
        Opcode::LeF => { translate_cmp_f(t, inst, FloatCC::LessThanOrEqual); Ok(Completed) }
        Opcode::GtF => { translate_cmp_f(t, inst, FloatCC::GreaterThan); Ok(Completed) }
        Opcode::GeF => { translate_cmp_f(t, inst, FloatCC::GreaterThanOrEqual); Ok(Completed) }
        
        // Logical
        Opcode::Not => { translate_not(t, inst); Ok(Completed) }
        
        // Bitwise
        Opcode::And => { translate_bit_and(t, inst); Ok(Completed) }
        Opcode::Or => { translate_bit_or(t, inst); Ok(Completed) }
        Opcode::Xor => { translate_bit_xor(t, inst); Ok(Completed) }
        Opcode::AndNot => { translate_and_not(t, inst); Ok(Completed) }
        Opcode::Shl => { translate_shl(t, inst); Ok(Completed) }
        Opcode::ShrS => { translate_shr_s(t, inst); Ok(Completed) }
        Opcode::ShrU => { translate_shr_u(t, inst); Ok(Completed) }
        
        // Global operations
        Opcode::GlobalGet => { translate_global_get(t, inst); Ok(Completed) }
        Opcode::GlobalGetN => { translate_global_get_n(t, inst); Ok(Completed) }
        Opcode::GlobalSet => { translate_global_set(t, inst); Ok(Completed) }
        Opcode::GlobalSetN => { translate_global_set_n(t, inst); Ok(Completed) }
        
        // Pointer operations
        Opcode::PtrGet => { translate_ptr_get(t, inst); Ok(Completed) }
        Opcode::PtrSet => { translate_ptr_set(t, inst); Ok(Completed) }
        Opcode::PtrGetN => { translate_ptr_get_n(t, inst); Ok(Completed) }
        Opcode::PtrSetN => { translate_ptr_set_n(t, inst); Ok(Completed) }
        
        // Slot operations
        Opcode::SlotGet => { translate_slot_get(t, inst); Ok(Completed) }
        Opcode::SlotSet => { translate_slot_set(t, inst); Ok(Completed) }
        Opcode::SlotGetN => { translate_slot_get_n(t, inst); Ok(Completed) }
        Opcode::SlotSetN => { translate_slot_set_n(t, inst); Ok(Completed) }
        
        // Slice operations
        Opcode::SliceNew => { translate_slice_new(t, inst); Ok(Completed) }
        Opcode::SliceGet => { translate_slice_get(t, inst); Ok(Completed) }
        Opcode::SliceSet => { translate_slice_set(t, inst); Ok(Completed) }
        Opcode::SliceLen => { translate_slice_len(t, inst); Ok(Completed) }
        Opcode::SliceCap => { translate_slice_cap(t, inst); Ok(Completed) }
        
        // Array operations
        Opcode::ArrayNew => { translate_array_new(t, inst); Ok(Completed) }
        Opcode::ArrayGet => { translate_array_get(t, inst); Ok(Completed) }
        Opcode::ArraySet => { translate_array_set(t, inst); Ok(Completed) }
        
        // Type conversions
        Opcode::ConvI2F => { translate_conv_i2f(t, inst); Ok(Completed) }
        Opcode::ConvF2I => { translate_conv_f2i(t, inst); Ok(Completed) }
        Opcode::ConvI32I64 => { translate_conv_i32_i64(t, inst); Ok(Completed) }
        Opcode::ConvI64I32 => { translate_conv_i64_i32(t, inst); Ok(Completed) }
        Opcode::ConvF64F32 => { translate_conv_f64_f32(t, inst); Ok(Completed) }
        Opcode::ConvF32F64 => { translate_conv_f32_f64(t, inst); Ok(Completed) }
        
        // String operations
        Opcode::StrLen => { translate_str_len(t, inst); Ok(Completed) }
        Opcode::StrIndex => { translate_str_index(t, inst); Ok(Completed) }
        Opcode::StrConcat => { translate_str_concat(t, inst); Ok(Completed) }
        Opcode::StrSlice => { translate_str_slice(t, inst); Ok(Completed) }
        Opcode::StrEq => { translate_str_eq(t, inst); Ok(Completed) }
        Opcode::StrNe => { translate_str_ne(t, inst); Ok(Completed) }
        Opcode::StrLt => { translate_str_cmp(t, inst, IntCC::SignedLessThan); Ok(Completed) }
        Opcode::StrLe => { translate_str_cmp(t, inst, IntCC::SignedLessThanOrEqual); Ok(Completed) }
        Opcode::StrGt => { translate_str_cmp(t, inst, IntCC::SignedGreaterThan); Ok(Completed) }
        Opcode::StrGe => { translate_str_cmp(t, inst, IntCC::SignedGreaterThanOrEqual); Ok(Completed) }
        Opcode::StrDecodeRune => { translate_str_decode_rune(t, inst); Ok(Completed) }
        
        // Map operations
        Opcode::MapLen => { translate_map_len(t, inst); Ok(Completed) }
        Opcode::MapGet => { translate_map_get(t, inst); Ok(Completed) }
        Opcode::MapSet => { translate_map_set(t, inst); Ok(Completed) }
        Opcode::MapDelete => { translate_map_delete(t, inst); Ok(Completed) }
        
        // Allocation
        Opcode::PtrNew => { translate_ptr_new(t, inst); Ok(Completed) }
        Opcode::MapNew => { translate_map_new(t, inst); Ok(Completed) }
        Opcode::ChanNew => { translate_chan_new(t, inst); Ok(Completed) }
        
        // Slice/Array complex operations
        Opcode::SliceSlice => { translate_slice_slice(t, inst); Ok(Completed) }
        Opcode::SliceAppend => { translate_slice_append(t, inst); Ok(Completed) }
        Opcode::SliceAddr => { translate_slice_addr(t, inst); Ok(Completed) }
        Opcode::ArrayAddr => { translate_array_addr(t, inst); Ok(Completed) }
        
        // Map iteration
        Opcode::MapIterGet => { translate_map_iter_get(t, inst); Ok(Completed) }
        
        // Closure operations
        Opcode::ClosureNew => { translate_closure_new(t, inst); Ok(Completed) }
        Opcode::ClosureGet => { translate_closure_get(t, inst); Ok(Completed) }
        
        // Interface operations
        Opcode::IfaceAssert => { translate_iface_assert(t, inst); Ok(Completed) }
        
        // Panic
        Opcode::Panic => { translate_panic(t, inst); Ok(Terminated) }
        
        // Control flow - compiler specific
        Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::Return => Ok(Unhandled),
        
        // Function calls - compiler specific (FunctionCompiler has JIT-to-JIT optimization)
        Opcode::Call | Opcode::CallExtern | Opcode::CallClosure | Opcode::CallIface => Ok(Unhandled),
        
        // Compiler specific (needs constant table content access)
        Opcode::StrNew | Opcode::IfaceAssign => Ok(Unhandled),
        
        // Unsupported
        _ => Ok(Unhandled),
    }
}

// =============================================================================
// Helper functions using BytecodeTranslator trait
// =============================================================================

fn translate_load_int(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let val = t.ins_iconst(inst.imm32() as i64);
    t.write_var(inst.a, val);
}

fn translate_load_const(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    use vo_runtime::bytecode::Constant;
    
    let const_idx = inst.b as usize;
    // Extract constant value first to avoid borrow conflict
    let (const_val, reg_const) = match &t.vo_module().constants[const_idx] {
        Constant::Nil => (0i64, Some(0i64)),
        Constant::Bool(b) => (*b as i64, Some(*b as i64)),
        Constant::Int(i) => (*i, Some(*i)),
        Constant::Float(f) => {
            let bits = f.to_bits() as i64;
            (bits, Some(bits))
        }
        Constant::String(_) => (0, None),
    };
    
    if let Some(rc) = reg_const {
        t.set_reg_const(inst.a, rc);
    }
    let val = t.ins_iconst(const_val);
    t.write_var(inst.a, val);
}

fn translate_copy(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let val = t.read_var(inst.b);
    t.write_var(inst.a, val);
}

fn translate_copy_n(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let count = inst.flags as usize;
    for i in 0..count {
        let val = t.read_var(inst.b + i as u16);
        t.write_var(inst.a + i as u16, val);
    }
}

// Arithmetic
fn translate_add_i(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_iadd(a, b);
    t.write_var(inst.a, result);
}

fn translate_sub_i(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_isub(a, b);
    t.write_var(inst.a, result);
}

fn translate_mul_i(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_imul(a, b);
    t.write_var(inst.a, result);
}

fn translate_div_i(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_sdiv(a, b);
    t.write_var(inst.a, result);
}

fn translate_mod_i(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_srem(a, b);
    t.write_var(inst.a, result);
}

fn translate_neg_i(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let result = t.ins_ineg(a);
    t.write_var(inst.a, result);
}

fn translate_add_f(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let fa = t.ins_bitcast_i64_to_f64(a);
    let fb = t.ins_bitcast_i64_to_f64(b);
    let fr = t.ins_fadd(fa, fb);
    let result = t.ins_bitcast_f64_to_i64(fr);
    t.write_var(inst.a, result);
}

fn translate_sub_f(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let fa = t.ins_bitcast_i64_to_f64(a);
    let fb = t.ins_bitcast_i64_to_f64(b);
    let fr = t.ins_fsub(fa, fb);
    let result = t.ins_bitcast_f64_to_i64(fr);
    t.write_var(inst.a, result);
}

fn translate_mul_f(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let fa = t.ins_bitcast_i64_to_f64(a);
    let fb = t.ins_bitcast_i64_to_f64(b);
    let fr = t.ins_fmul(fa, fb);
    let result = t.ins_bitcast_f64_to_i64(fr);
    t.write_var(inst.a, result);
}

fn translate_div_f(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let fa = t.ins_bitcast_i64_to_f64(a);
    let fb = t.ins_bitcast_i64_to_f64(b);
    let fr = t.ins_fdiv(fa, fb);
    let result = t.ins_bitcast_f64_to_i64(fr);
    t.write_var(inst.a, result);
}

fn translate_neg_f(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let fa = t.ins_bitcast_i64_to_f64(a);
    let fr = t.ins_fneg(fa);
    let result = t.ins_bitcast_f64_to_i64(fr);
    t.write_var(inst.a, result);
}

// Comparison
fn translate_cmp_i(t: &mut impl BytecodeTranslator, inst: &Instruction, cc: IntCC) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let cmp = t.ins_icmp(cc, a, b);
    let result = t.ins_uextend_i8_to_i64(cmp);
    t.write_var(inst.a, result);
}

fn translate_cmp_f(t: &mut impl BytecodeTranslator, inst: &Instruction, cc: FloatCC) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let fa = t.ins_bitcast_i64_to_f64(a);
    let fb = t.ins_bitcast_i64_to_f64(b);
    let cmp = t.ins_fcmp(cc, fa, fb);
    let result = t.ins_uextend_i8_to_i64(cmp);
    t.write_var(inst.a, result);
}

// Logical
fn translate_not(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let zero = t.ins_iconst(0);
    let cmp = t.ins_icmp(IntCC::Equal, a, zero);
    let result = t.ins_uextend_i8_to_i64(cmp);
    t.write_var(inst.a, result);
}

// Bitwise
fn translate_bit_and(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_band(a, b);
    t.write_var(inst.a, result);
}

fn translate_bit_or(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_bor(a, b);
    t.write_var(inst.a, result);
}

fn translate_bit_xor(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_bxor(a, b);
    t.write_var(inst.a, result);
}

fn translate_bit_not(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let result = t.ins_bnot(a);
    t.write_var(inst.a, result);
}

fn translate_shl(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_ishl(a, b);
    t.write_var(inst.a, result);
}

fn translate_shr_s(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_sshr(a, b);
    t.write_var(inst.a, result);
}

fn translate_shr_u(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let result = t.ins_ushr(a, b);
    t.write_var(inst.a, result);
}

fn translate_and_not(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    let not_b = t.ins_bnot(b);
    let result = t.ins_band(a, not_b);
    t.write_var(inst.a, result);
}

// Global operations
fn translate_global_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let globals_ptr = t.load_globals_ptr();
    let offset = (inst.b as i32) * 8;
    let val = t.ins_load(types::I64, globals_ptr, offset);
    t.write_var(inst.a, val);
}

fn translate_global_get_n(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let globals_ptr = t.load_globals_ptr();
    let count = inst.flags as usize;
    for i in 0..count {
        let offset = ((inst.b as usize + i) * 8) as i32;
        let val = t.ins_load(types::I64, globals_ptr, offset);
        t.write_var(inst.a + i as u16, val);
    }
}

fn translate_global_set(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let globals_ptr = t.load_globals_ptr();
    let val = t.read_var(inst.b);
    let offset = (inst.a as i32) * 8;
    t.ins_store(val, globals_ptr, offset);
}

fn translate_global_set_n(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let globals_ptr = t.load_globals_ptr();
    let count = inst.flags as usize;
    for i in 0..count {
        let val = t.read_var(inst.b + i as u16);
        let offset = ((inst.a as usize + i) * 8) as i32;
        t.ins_store(val, globals_ptr, offset);
    }
}

// Pointer operations
fn translate_ptr_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let ptr = t.read_var(inst.b);
    let offset = (inst.c as i32) * 8;
    let val = t.ins_load(types::I64, ptr, offset);
    t.write_var(inst.a, val);
}

fn translate_ptr_set(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let ptr = t.read_var(inst.a);
    let offset = (inst.b as i32) * 8;
    let val = t.read_var(inst.c);
    t.ins_store(val, ptr, offset);
}

fn translate_ptr_get_n(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let ptr = t.read_var(inst.b);
    let count = inst.flags as usize;
    for i in 0..count {
        let offset = ((inst.c as usize + i) * 8) as i32;
        let val = t.ins_load(types::I64, ptr, offset);
        t.write_var(inst.a + i as u16, val);
    }
}

fn translate_ptr_set_n(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let ptr = t.read_var(inst.a);
    let count = inst.flags as usize;
    for i in 0..count {
        let val = t.read_var(inst.c + i as u16);
        let offset = ((inst.b as usize + i) * 8) as i32;
        t.ins_store(val, ptr, offset);
    }
}

// Slot operations (dynamic stack access)
fn translate_slot_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let idx = t.read_var(inst.c);
    let base_ptr = t.read_var(inst.b);
    let byte_offset = t.ins_imul_imm(idx, 8);
    let addr = t.ins_iadd(base_ptr, byte_offset);
    let val = t.ins_load(types::I64, addr, 0);
    t.write_var(inst.a, val);
}

fn translate_slot_set(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let idx = t.read_var(inst.b);
    let val = t.read_var(inst.c);
    let base_ptr = t.read_var(inst.a);
    let byte_offset = t.ins_imul_imm(idx, 8);
    let addr = t.ins_iadd(base_ptr, byte_offset);
    t.ins_store(val, addr, 0);
}

fn translate_slot_get_n(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let idx = t.read_var(inst.c);
    let elem_slots = inst.flags as usize;
    let base_ptr = t.read_var(inst.b);
    let elem_byte_offset = t.ins_imul_imm(idx, (elem_slots * 8) as i64);
    let start_addr = t.ins_iadd(base_ptr, elem_byte_offset);
    
    for i in 0..elem_slots {
        let addr = t.ins_iadd_imm(start_addr, (i * 8) as i64);
        let val = t.ins_load(types::I64, addr, 0);
        t.write_var(inst.a + i as u16, val);
    }
}

fn translate_slot_set_n(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let idx = t.read_var(inst.b);
    let elem_slots = inst.flags as usize;
    let base_ptr = t.read_var(inst.a);
    let elem_byte_offset = t.ins_imul_imm(idx, (elem_slots * 8) as i64);
    let start_addr = t.ins_iadd(base_ptr, elem_byte_offset);
    
    for i in 0..elem_slots {
        let val = t.read_var(inst.c + i as u16);
        let addr = t.ins_iadd_imm(start_addr, (i * 8) as i64);
        t.ins_store(val, addr, 0);
    }
}

// Slice operations
const SLICE_FIELD_DATA_PTR: i32 = 0;
const SLICE_FIELD_LEN: i32 = 8;
const SLICE_FIELD_CAP: i32 = 16;

fn translate_slice_new(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let slice_new_func = match t.slice_funcs().slice_new {
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
        let eb = t.read_var(inst.c + 2);
        t.ins_ireduce_i32(eb)
    } else {
        let v = t.ins_iconst(elem_bytes as i64);
        t.ins_ireduce_i32(v)
    };
    
    let gc_ptr = t.load_gc_ptr();
    let meta_raw = t.read_var(inst.b);
    let meta_i32 = t.ins_ireduce_i32(meta_raw);
    let len = t.read_var(inst.c);
    let cap = t.read_var(inst.c + 1);
    
    if let Some(result) = t.ins_call(slice_new_func, &[gc_ptr, meta_i32, elem_bytes_val, len, cap]) {
        t.write_var(inst.a, result);
    }
}

fn translate_slice_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let s = t.read_var(inst.b);
    let idx = t.read_var(inst.c);
    
    let (elem_bytes, needs_sext) = match inst.flags {
        0 => (t.get_const_from_reg(inst.c + 1) as usize, false),
        0x81 => (1, true),
        0x82 => (2, true),
        0x84 => (4, true),
        0x44 => (4, false),
        f => (f as usize, false),
    };
    
    let data_ptr = t.ins_load(types::I64, s, SLICE_FIELD_DATA_PTR);
    
    if elem_bytes <= 8 {
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        let addr = t.ins_iadd(data_ptr, off);
        
        let val = match elem_bytes {
            1 => {
                let v8 = t.ins_load_i8(addr, 0);
                if needs_sext { t.ins_sextend_i8_to_i64(v8) }
                else { t.ins_uextend_i8_to_i64(v8) }
            }
            2 => {
                let v16 = t.ins_load_i16(addr, 0);
                if needs_sext { t.ins_sextend_i16_to_i64(v16) }
                else { t.ins_uextend_i16_to_i64(v16) }
            }
            4 => {
                let v32 = t.ins_load_i32(addr, 0);
                if needs_sext { t.ins_sextend_i32_to_i64(v32) }
                else { t.ins_uextend_i32_to_i64(v32) }
            }
            _ => t.ins_load(types::I64, addr, 0),
        };
        t.write_var(inst.a, val);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        for i in 0..elem_slots {
            let slot_off = t.ins_iadd_imm(off, (i * 8) as i64);
            let addr = t.ins_iadd(data_ptr, slot_off);
            let val = t.ins_load(types::I64, addr, 0);
            t.write_var(inst.a + i as u16, val);
        }
    }
}

fn translate_slice_set(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let s = t.read_var(inst.a);
    let idx = t.read_var(inst.b);
    let val = t.read_var(inst.c);
    
    let elem_bytes = match inst.flags {
        0 => t.get_const_from_reg(inst.b + 1) as usize,
        0x81 => 1,
        0x82 => 2,
        0x84 | 0x44 => 4,
        f => f as usize,
    };
    
    let data_ptr = t.ins_load(types::I64, s, SLICE_FIELD_DATA_PTR);
    
    if elem_bytes <= 8 {
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        let addr = t.ins_iadd(data_ptr, off);
        
        match elem_bytes {
            1 => {
                let v8 = t.ins_ireduce_i8(val);
                t.ins_store_i8(v8, addr, 0);
            }
            2 => {
                let v16 = t.ins_ireduce_i16(val);
                t.ins_store_i16(v16, addr, 0);
            }
            4 => {
                let v32 = t.ins_ireduce_i32(val);
                t.ins_store_i32(v32, addr, 0);
            }
            _ => {
                t.ins_store(val, addr, 0);
            }
        }
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        for i in 0..elem_slots {
            let v = t.read_var(inst.c + i as u16);
            let slot_off = t.ins_iadd_imm(off, (i * 8) as i64);
            let addr = t.ins_iadd(data_ptr, slot_off);
            t.ins_store(v, addr, 0);
        }
    }
}

fn translate_slice_len(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let slice_len_func = match t.slice_funcs().slice_len {
        Some(f) => f,
        None => return,
    };
    let s = t.read_var(inst.b);
    if let Some(result) = t.ins_call(slice_len_func, &[s]) {
        t.write_var(inst.a, result);
    }
}

fn translate_slice_cap(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let slice_cap_func = match t.slice_funcs().slice_cap {
        Some(f) => f,
        None => return,
    };
    let s = t.read_var(inst.b);
    if let Some(result) = t.ins_call(slice_cap_func, &[s]) {
        t.write_var(inst.a, result);
    }
}

// Array operations
const ARRAY_HEADER_BYTES: i64 = 2 * 8;

fn translate_array_new(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let array_new_func = match t.array_funcs().array_new {
        Some(f) => f,
        None => return,
    };
    
    let gc_ptr = t.load_gc_ptr();
    let meta_raw = t.read_var(inst.b);
    let meta_i32 = t.ins_ireduce_i32(meta_raw);
    let elem_slots = t.ins_iconst(inst.flags as i64);
    let elem_slots_i32 = t.ins_ireduce_i32(elem_slots);
    let len = t.read_var(inst.c);
    
    if let Some(result) = t.ins_call(array_new_func, &[gc_ptr, meta_i32, elem_slots_i32, len]) {
        t.write_var(inst.a, result);
    }
}

fn translate_array_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let arr = t.read_var(inst.b);
    let idx = t.read_var(inst.c);
    
    let (elem_bytes, needs_sext) = match inst.flags {
        0 => (t.get_const_from_reg(inst.c + 1) as usize, false),
        0x81 => (1, true),
        0x82 => (2, true),
        0x84 => (4, true),
        0x44 => (4, false),
        f => (f as usize, false),
    };
    
    if elem_bytes <= 8 {
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        let off = t.ins_iadd_imm(off, ARRAY_HEADER_BYTES);
        let addr = t.ins_iadd(arr, off);
        
        let val = match elem_bytes {
            1 => {
                let v8 = t.ins_load_i8(addr, 0);
                if needs_sext { t.ins_sextend_i8_to_i64(v8) }
                else { t.ins_uextend_i8_to_i64(v8) }
            }
            2 => {
                let v16 = t.ins_load_i16(addr, 0);
                if needs_sext { t.ins_sextend_i16_to_i64(v16) }
                else { t.ins_uextend_i16_to_i64(v16) }
            }
            4 => {
                let v32 = t.ins_load_i32(addr, 0);
                if needs_sext { t.ins_sextend_i32_to_i64(v32) }
                else { t.ins_uextend_i32_to_i64(v32) }
            }
            _ => t.ins_load(types::I64, addr, 0),
        };
        t.write_var(inst.a, val);
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        let off = t.ins_iadd_imm(off, ARRAY_HEADER_BYTES);
        for i in 0..elem_slots {
            let slot_off = t.ins_iadd_imm(off, (i * 8) as i64);
            let addr = t.ins_iadd(arr, slot_off);
            let val = t.ins_load(types::I64, addr, 0);
            t.write_var(inst.a + i as u16, val);
        }
    }
}

fn translate_array_set(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let arr = t.read_var(inst.a);
    let idx = t.read_var(inst.b);
    let val = t.read_var(inst.c);
    
    let elem_bytes = match inst.flags {
        0 => t.get_const_from_reg(inst.b + 1) as usize,
        0x81 => 1,
        0x82 => 2,
        0x84 | 0x44 => 4,
        f => f as usize,
    };
    
    if elem_bytes <= 8 {
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        let off = t.ins_iadd_imm(off, ARRAY_HEADER_BYTES);
        let addr = t.ins_iadd(arr, off);
        
        match elem_bytes {
            1 => {
                let v8 = t.ins_ireduce_i8(val);
                t.ins_store_i8(v8, addr, 0);
            }
            2 => {
                let v16 = t.ins_ireduce_i16(val);
                t.ins_store_i16(v16, addr, 0);
            }
            4 => {
                let v32 = t.ins_ireduce_i32(val);
                t.ins_store_i32(v32, addr, 0);
            }
            _ => {
                t.ins_store(val, addr, 0);
            }
        }
    } else {
        let elem_slots = (elem_bytes + 7) / 8;
        let eb = t.ins_iconst(elem_bytes as i64);
        let off = t.ins_imul(idx, eb);
        let off = t.ins_iadd_imm(off, ARRAY_HEADER_BYTES);
        for i in 0..elem_slots {
            let v = t.read_var(inst.c + i as u16);
            let slot_off = t.ins_iadd_imm(off, (i * 8) as i64);
            let addr = t.ins_iadd(arr, slot_off);
            t.ins_store(v, addr, 0);
        }
    }
}

// Type conversions
fn translate_conv_i2f(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let f = t.ins_fcvt_from_sint(a);
    let result = t.ins_bitcast_f64_to_i64(f);
    t.write_var(inst.a, result);
}

fn translate_conv_f2i(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let f = t.ins_bitcast_i64_to_f64(a);
    let result = t.ins_fcvt_to_sint(f);
    t.write_var(inst.a, result);
}

fn translate_conv_i32_i64(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let truncated = t.ins_ireduce_i32(a);
    let result = t.ins_sextend_i32_to_i64(truncated);
    t.write_var(inst.a, result);
}

fn translate_conv_i64_i32(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let truncated = t.ins_ireduce_i32(a);
    let result = t.ins_uextend_i32_to_i64(truncated);
    t.write_var(inst.a, result);
}

fn translate_conv_f64_f32(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let f64_val = t.ins_bitcast_i64_to_f64(a);
    let f32_val = t.ins_fdemote(f64_val);
    let result = t.ins_bitcast_f64_to_i64(f32_val);
    t.write_var(inst.a, result);
}

fn translate_conv_f32_f64(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let a = t.read_var(inst.b);
    let truncated = t.ins_ireduce_i32(a);
    let f32_val = t.ins_bitcast_i64_to_f64(truncated);
    let f64_val = t.ins_fpromote(f32_val);
    let result = t.ins_bitcast_f64_to_i64(f64_val);
    t.write_var(inst.a, result);
}

// String operations
fn translate_str_len(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let str_len_func = match t.str_funcs().str_len {
        Some(f) => f,
        None => return,
    };
    let s = t.read_var(inst.b);
    if let Some(result) = t.ins_call(str_len_func, &[s]) {
        t.write_var(inst.a, result);
    }
}

fn translate_str_index(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let str_index_func = match t.str_funcs().str_index {
        Some(f) => f,
        None => return,
    };
    let s = t.read_var(inst.b);
    let idx = t.read_var(inst.c);
    if let Some(result) = t.ins_call(str_index_func, &[s, idx]) {
        t.write_var(inst.a, result);
    }
}

fn translate_str_concat(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let str_concat_func = match t.str_funcs().str_concat {
        Some(f) => f,
        None => return,
    };
    let gc_ptr = t.load_gc_ptr();
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    if let Some(result) = t.ins_call(str_concat_func, &[gc_ptr, a, b]) {
        t.write_var(inst.a, result);
    }
}

fn translate_str_slice(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let str_slice_func = match t.str_funcs().str_slice {
        Some(f) => f,
        None => return,
    };
    let gc_ptr = t.load_gc_ptr();
    let s = t.read_var(inst.b);
    let lo = t.read_var(inst.c);
    let hi = t.read_var(inst.c + 1);
    if let Some(result) = t.ins_call(str_slice_func, &[gc_ptr, s, lo, hi]) {
        t.write_var(inst.a, result);
    }
}

fn translate_str_eq(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let str_eq_func = match t.str_funcs().str_eq {
        Some(f) => f,
        None => return,
    };
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    if let Some(result) = t.ins_call(str_eq_func, &[a, b]) {
        t.write_var(inst.a, result);
    }
}

fn translate_str_ne(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let str_eq_func = match t.str_funcs().str_eq {
        Some(f) => f,
        None => return,
    };
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    if let Some(eq_result) = t.ins_call(str_eq_func, &[a, b]) {
        let zero = t.ins_iconst(0);
        let cmp = t.ins_icmp(IntCC::Equal, eq_result, zero);
        let result = t.ins_uextend_i8_to_i64(cmp);
        t.write_var(inst.a, result);
    }
}

fn translate_str_cmp(t: &mut impl BytecodeTranslator, inst: &Instruction, cc: IntCC) {
    let str_cmp_func = match t.str_funcs().str_cmp {
        Some(f) => f,
        None => return,
    };
    let a = t.read_var(inst.b);
    let b = t.read_var(inst.c);
    if let Some(cmp_result) = t.ins_call(str_cmp_func, &[a, b]) {
        let zero = t.ins_iconst(0);
        let cmp = t.ins_icmp(cc, cmp_result, zero);
        let result = t.ins_uextend_i8_to_i64(cmp);
        t.write_var(inst.a, result);
    }
}

fn translate_str_decode_rune(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let str_decode_rune_func = match t.str_funcs().str_decode_rune {
        Some(f) => f,
        None => return,
    };
    let s = t.read_var(inst.b);
    let idx = t.read_var(inst.c);
    if let Some(result) = t.ins_call(str_decode_rune_func, &[s, idx]) {
        t.write_var(inst.a, result);
    }
}

// Map operations
fn translate_map_len(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let map_len_func = match t.map_funcs().map_len {
        Some(f) => f,
        None => return,
    };
    let m = t.read_var(inst.b);
    if let Some(result) = t.ins_call(map_len_func, &[m]) {
        t.write_var(inst.a, result);
    }
}

fn translate_map_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let map_get_func = match t.map_funcs().map_get {
        Some(f) => f,
        None => return,
    };
    let m = t.read_var(inst.a);
    let key = t.read_var(inst.b);
    let key_bytes = t.ins_iconst(inst.c as i64);
    let val_bytes = t.ins_iconst(inst.flags as i64);
    if let Some(result) = t.ins_call(map_get_func, &[m, key, key_bytes, val_bytes]) {
        t.write_var(inst.a, result);
    }
}

fn translate_map_set(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let map_set_func = match t.map_funcs().map_set {
        Some(f) => f,
        None => return,
    };
    let gc_ptr = t.load_gc_ptr();
    let m = t.read_var(inst.a);
    let key = t.read_var(inst.b);
    let val = t.read_var(inst.c);
    let key_bytes = t.ins_iconst((inst.flags & 0x0F) as i64);
    let val_bytes = t.ins_iconst((inst.flags >> 4) as i64);
    t.ins_call(map_set_func, &[gc_ptr, m, key, val, key_bytes, val_bytes]);
}

fn translate_map_delete(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let map_delete_func = match t.map_funcs().map_delete {
        Some(f) => f,
        None => return,
    };
    let m = t.read_var(inst.a);
    let key = t.read_var(inst.b);
    let key_bytes = t.ins_iconst(inst.c as i64);
    t.ins_call(map_delete_func, &[m, key, key_bytes]);
}

// =============================================================================
// Shared Call translation (for loops)
// =============================================================================

pub fn translate_call_vm(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let call_vm_func = match t.call_vm_func() {
        Some(f) => f,
        None => return,
    };
    
    let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    
    let arg_slot = t.create_stack_slot((arg_slots.max(1) * 8) as u32);
    let ret_slot = t.create_stack_slot((ret_slots.max(1) * 8) as u32);
    
    for i in 0..arg_slots {
        let val = t.read_var((arg_start + i) as u16);
        t.ins_stack_store(val, arg_slot, (i * 8) as i32);
    }
    
    let arg_ptr = t.ins_stack_addr(arg_slot);
    let ret_ptr = t.ins_stack_addr(ret_slot);
    let ctx = t.get_ctx_param();
    
    let func_id_val = t.ins_iconst(func_id as i64);
    let func_id_i32 = t.ins_ireduce_i32(func_id_val);
    let arg_count_val = t.ins_iconst(arg_slots as i64);
    let arg_count_i32 = t.ins_ireduce_i32(arg_count_val);
    let ret_count_val = t.ins_iconst(ret_slots as i64);
    let ret_count_i32 = t.ins_ireduce_i32(ret_count_val);
    
    if let Some(result) = t.ins_call(call_vm_func, &[ctx, func_id_i32, arg_ptr, arg_count_i32, ret_ptr, ret_count_i32]) {
        let panic_block = t.create_block();
        let continue_block = t.create_block();
        
        t.ins_brif(result, panic_block, continue_block);
        
        t.switch_to_block(panic_block);
        t.seal_block(panic_block);
        let panic_val = t.ins_iconst(t.panic_return_value() as i64);
        let panic_i32 = t.ins_ireduce_i32(panic_val);
        t.ins_return(panic_i32);
        
        t.switch_to_block(continue_block);
        t.seal_block(continue_block);
        
        for i in 0..ret_slots {
            let val = t.ins_stack_load(ret_slot, (i * 8) as i32);
            t.write_var((arg_start + i) as u16, val);
        }
    }
}

// =============================================================================
// Shared CallExtern/CallClosure/CallIface translation (for loops)
// =============================================================================

pub fn translate_call_extern_vm(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let call_extern_func = match t.call_extern_func() {
        Some(f) => f,
        None => return,
    };
    
    t.emit_safepoint();
    
    let dst = inst.a;
    let extern_id = inst.b as u32;
    let arg_start = inst.c as usize;
    let arg_count = inst.flags as usize;
    
    let arg_slot = t.create_stack_slot((arg_count.max(1) * 8) as u32);
    
    for i in 0..arg_count {
        let val = t.read_var((arg_start + i) as u16);
        t.ins_stack_store(val, arg_slot, (i * 8) as i32);
    }
    
    let arg_ptr = t.ins_stack_addr(arg_slot);
    let ctx = t.get_ctx_param();
    let extern_id_val = t.ins_iconst(extern_id as i64);
    let extern_id_i32 = t.ins_ireduce_i32(extern_id_val);
    let arg_count_val = t.ins_iconst(arg_count as i64);
    let arg_count_i32 = t.ins_ireduce_i32(arg_count_val);
    
    if let Some(result) = t.ins_call(call_extern_func, &[ctx, extern_id_i32, arg_ptr, arg_count_i32, arg_ptr]) {
        let panic_block = t.create_block();
        let continue_block = t.create_block();
        
        t.ins_brif(result, panic_block, continue_block);
        
        t.switch_to_block(panic_block);
        t.seal_block(panic_block);
        let panic_val = t.ins_iconst(t.panic_return_value() as i64);
        let panic_i32 = t.ins_ireduce_i32(panic_val);
        t.ins_return(panic_i32);
        
        t.switch_to_block(continue_block);
        t.seal_block(continue_block);
        
        for i in 0..arg_count {
            let val = t.ins_stack_load(arg_slot, (i * 8) as i32);
            t.write_var(dst + i as u16, val);
        }
    }
}

pub fn translate_call_closure_vm(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let call_closure_func = match t.call_closure_func() {
        Some(f) => f,
        None => return,
    };
    
    t.emit_safepoint();
    
    let closure_ref = t.read_var(inst.a);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    
    let arg_slot = t.create_stack_slot((arg_slots.max(1) * 8) as u32);
    let ret_slot = t.create_stack_slot((ret_slots.max(1) * 8) as u32);
    
    for i in 0..arg_slots {
        let val = t.read_var((arg_start + i) as u16);
        t.ins_stack_store(val, arg_slot, (i * 8) as i32);
    }
    
    let arg_ptr = t.ins_stack_addr(arg_slot);
    let ret_ptr = t.ins_stack_addr(ret_slot);
    let ctx = t.get_ctx_param();
    let arg_count_val = t.ins_iconst(arg_slots as i64);
    let arg_count_i32 = t.ins_ireduce_i32(arg_count_val);
    let ret_count_val = t.ins_iconst(ret_slots as i64);
    let ret_count_i32 = t.ins_ireduce_i32(ret_count_val);
    
    if let Some(result) = t.ins_call(call_closure_func, &[ctx, closure_ref, arg_ptr, arg_count_i32, ret_ptr, ret_count_i32]) {
        let panic_block = t.create_block();
        let continue_block = t.create_block();
        
        t.ins_brif(result, panic_block, continue_block);
        
        t.switch_to_block(panic_block);
        t.seal_block(panic_block);
        let panic_val = t.ins_iconst(t.panic_return_value() as i64);
        let panic_i32 = t.ins_ireduce_i32(panic_val);
        t.ins_return(panic_i32);
        
        t.switch_to_block(continue_block);
        t.seal_block(continue_block);
        
        for i in 0..ret_slots {
            let val = t.ins_stack_load(ret_slot, (i * 8) as i32);
            t.write_var((arg_start + i) as u16, val);
        }
    }
}

pub fn translate_call_iface_vm(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let call_iface_func = match t.call_iface_func() {
        Some(f) => f,
        None => return,
    };
    
    t.emit_safepoint();
    
    let iface_slot0 = t.read_var(inst.a);
    let iface_slot1 = t.read_var(inst.a + 1);
    let method_idx = inst.flags as u32;
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    
    let arg_slot = t.create_stack_slot((arg_slots.max(1) * 8) as u32);
    let ret_slot = t.create_stack_slot((ret_slots.max(1) * 8) as u32);
    
    for i in 0..arg_slots {
        let val = t.read_var((arg_start + i) as u16);
        t.ins_stack_store(val, arg_slot, (i * 8) as i32);
    }
    
    let arg_ptr = t.ins_stack_addr(arg_slot);
    let ret_ptr = t.ins_stack_addr(ret_slot);
    let ctx = t.get_ctx_param();
    let method_idx_val = t.ins_iconst(method_idx as i64);
    let method_idx_i32 = t.ins_ireduce_i32(method_idx_val);
    let arg_count_val = t.ins_iconst(arg_slots as i64);
    let arg_count_i32 = t.ins_ireduce_i32(arg_count_val);
    let ret_count_val = t.ins_iconst(ret_slots as i64);
    let ret_count_i32 = t.ins_ireduce_i32(ret_count_val);
    let func_id_val = t.ins_iconst(0i64);
    let func_id_i32 = t.ins_ireduce_i32(func_id_val);
    
    if let Some(result) = t.ins_call(call_iface_func, &[ctx, iface_slot0, iface_slot1, method_idx_i32, arg_ptr, arg_count_i32, ret_ptr, ret_count_i32, func_id_i32]) {
        let panic_block = t.create_block();
        let continue_block = t.create_block();
        
        t.ins_brif(result, panic_block, continue_block);
        
        t.switch_to_block(panic_block);
        t.seal_block(panic_block);
        let panic_val = t.ins_iconst(t.panic_return_value() as i64);
        let panic_i32 = t.ins_ireduce_i32(panic_val);
        t.ins_return(panic_i32);
        
        t.switch_to_block(continue_block);
        t.seal_block(continue_block);
        
        for i in 0..ret_slots {
            let val = t.ins_stack_load(ret_slot, (i * 8) as i32);
            t.write_var((arg_start + i) as u16, val);
        }
    }
}

// =============================================================================
// Allocation operations
// =============================================================================

fn translate_ptr_new(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let gc_alloc_func = match t.gc_alloc_func() {
        Some(f) => f,
        None => return,
    };
    
    let gc_ptr = t.load_gc_ptr();
    let meta_raw = t.read_var(inst.b);
    let meta_i32 = t.ins_ireduce_i32(meta_raw);
    let slots = t.ins_iconst(inst.flags as i64);
    let slots_i32 = t.ins_ireduce_i32(slots);
    
    if let Some(gc_ref) = t.ins_call(gc_alloc_func, &[gc_ptr, meta_i32, slots_i32]) {
        t.write_var(inst.a, gc_ref);
    }
}

fn translate_map_new(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let map_new_func = match t.map_funcs().map_new {
        Some(f) => f,
        None => return,
    };
    
    let gc_ptr = t.load_gc_ptr();
    let packed_meta = t.read_var(inst.b);
    
    // Unpack key_meta (high 32) and val_meta (low 32)
    let key_meta = t.ins_ushr_imm(packed_meta, 32);
    let key_meta_i32 = t.ins_ireduce_i32(key_meta);
    let val_meta_i32 = t.ins_ireduce_i32(packed_meta);
    
    let key_slots = (inst.c >> 8) as i64;
    let val_slots = (inst.c & 0xFF) as i64;
    let key_slots_val = t.ins_iconst(key_slots);
    let key_slots_i32 = t.ins_ireduce_i32(key_slots_val);
    let val_slots_val = t.ins_iconst(val_slots);
    let val_slots_i32 = t.ins_ireduce_i32(val_slots_val);
    
    if let Some(map_ref) = t.ins_call(map_new_func, &[gc_ptr, key_meta_i32, val_meta_i32, key_slots_i32, val_slots_i32]) {
        t.write_var(inst.a, map_ref);
    }
}

fn translate_chan_new(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let chan_new_func = match t.misc_funcs().chan_new {
        Some(f) => f,
        None => return,
    };
    
    let gc_ptr = t.load_gc_ptr();
    let elem_meta = t.read_var(inst.b);
    let elem_meta_i32 = t.ins_ireduce_i32(elem_meta);
    let elem_slots = t.ins_iconst(inst.flags as i64);
    let elem_slots_i32 = t.ins_ireduce_i32(elem_slots);
    let cap = t.read_var(inst.c);
    
    if let Some(chan_ref) = t.ins_call(chan_new_func, &[gc_ptr, elem_meta_i32, elem_slots_i32, cap]) {
        t.write_var(inst.a, chan_ref);
    }
}

// =============================================================================
// Slice/Array complex operations
// =============================================================================

fn translate_slice_slice(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let is_array = (inst.flags & 1) != 0;
    let has_max = (inst.flags & 2) != 0;
    
    let gc_ptr = t.load_gc_ptr();
    let src = t.read_var(inst.b);
    let lo = t.read_var(inst.c);
    let hi = t.read_var(inst.c + 1);
    
    if is_array {
        if has_max {
            let func = match t.slice_funcs().slice_from_array3 {
                Some(f) => f,
                None => return,
            };
            let max = t.read_var(inst.c + 2);
            if let Some(result) = t.ins_call(func, &[gc_ptr, src, lo, hi, max]) {
                t.write_var(inst.a, result);
            }
        } else {
            let func = match t.slice_funcs().slice_from_array {
                Some(f) => f,
                None => return,
            };
            if let Some(result) = t.ins_call(func, &[gc_ptr, src, lo, hi]) {
                t.write_var(inst.a, result);
            }
        }
    } else {
        if has_max {
            let func = match t.slice_funcs().slice_slice3 {
                Some(f) => f,
                None => return,
            };
            let max = t.read_var(inst.c + 2);
            if let Some(result) = t.ins_call(func, &[gc_ptr, src, lo, hi, max]) {
                t.write_var(inst.a, result);
            }
        } else {
            let func = match t.slice_funcs().slice_slice {
                Some(f) => f,
                None => return,
            };
            if let Some(result) = t.ins_call(func, &[gc_ptr, src, lo, hi]) {
                t.write_var(inst.a, result);
            }
        }
    }
}

fn translate_slice_append(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let slice_append_func = match t.slice_funcs().slice_append {
        Some(f) => f,
        None => return,
    };
    
    let (elem_bytes, elem_offset): (usize, u16) = match inst.flags {
        0 => (0, 2),
        0x81 => (1, 1),
        0x82 => (2, 1),
        0x84 | 0x44 => (4, 1),
        f => (f as usize, 1),
    };
    let is_dynamic = inst.flags == 0;
    
    let gc_ptr = t.load_gc_ptr();
    let s = t.read_var(inst.b);
    
    let elem_meta_i64 = t.read_var(inst.c);
    let elem_meta = t.ins_ireduce_i32(elem_meta_i64);
    let elem_bytes_val = if is_dynamic {
        let eb = t.read_var(inst.c + 1);
        t.ins_ireduce_i32(eb)
    } else {
        let eb = t.ins_iconst(elem_bytes as i64);
        t.ins_ireduce_i32(eb)
    };
    
    let val_slot = t.create_stack_slot(256);
    
    let max_slots = if is_dynamic { 32 } else { (elem_bytes + 7) / 8 };
    for i in 0..max_slots {
        let val = t.read_var(inst.c + elem_offset + i as u16);
        t.ins_stack_store(val, val_slot, (i * 8) as i32);
    }
    
    let val_ptr = t.ins_stack_addr(val_slot);
    
    if let Some(result) = t.ins_call(slice_append_func, &[gc_ptr, elem_meta, elem_bytes_val, s, val_ptr]) {
        t.write_var(inst.a, result);
    }
}

fn translate_slice_addr(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let s = t.read_var(inst.b);
    let idx = t.read_var(inst.c);
    let elem_bytes = inst.flags as i64;
    
    let data_ptr = t.ins_load(types::I64, s, 0);
    let eb = t.ins_iconst(elem_bytes);
    let off = t.ins_imul(idx, eb);
    let addr = t.ins_iadd(data_ptr, off);
    
    t.write_var(inst.a, addr);
}

fn translate_array_addr(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let arr = t.read_var(inst.b);
    let idx = t.read_var(inst.c);
    let elem_bytes = inst.flags as i64;
    
    let eb = t.ins_iconst(elem_bytes);
    let off = t.ins_imul(idx, eb);
    let off = t.ins_iadd_imm(off, ARRAY_HEADER_BYTES);
    let addr = t.ins_iadd(arr, off);
    
    t.write_var(inst.a, addr);
}

// =============================================================================
// Map iteration
// =============================================================================

fn translate_map_iter_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let map_iter_get_func = match t.map_funcs().map_iter_get {
        Some(f) => f,
        None => return,
    };
    
    let m = t.read_var(inst.b);
    let idx = t.read_var(inst.c);
    let key_slots = (inst.flags >> 4) as usize;
    let val_slots = (inst.flags & 0xF) as usize;
    
    let key_slot = t.create_stack_slot((key_slots.max(1) * 8) as u32);
    let val_slot = t.create_stack_slot((val_slots.max(1) * 8) as u32);
    
    let key_ptr = t.ins_stack_addr(key_slot);
    let val_ptr = t.ins_stack_addr(val_slot);
    let key_slots_val = t.ins_iconst(key_slots as i64);
    let key_slots_i32 = t.ins_ireduce_i32(key_slots_val);
    let val_slots_val = t.ins_iconst(val_slots as i64);
    let val_slots_i32 = t.ins_ireduce_i32(val_slots_val);
    
    if let Some(done) = t.ins_call(map_iter_get_func, &[m, idx, key_ptr, key_slots_i32, val_ptr, val_slots_i32]) {
        for i in 0..key_slots {
            let val = t.ins_stack_load(key_slot, (i * 8) as i32);
            t.write_var(inst.a + i as u16, val);
        }
        for i in 0..val_slots {
            let val = t.ins_stack_load(val_slot, (i * 8) as i32);
            t.write_var(inst.a + key_slots as u16 + i as u16, val);
        }
        t.write_var(inst.a + (key_slots + val_slots) as u16, done);
    }
}

// =============================================================================
// Closure operations
// =============================================================================

fn translate_closure_new(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let closure_new_func = match t.misc_funcs().closure_new {
        Some(f) => f,
        None => return,
    };
    
    let gc_ptr = t.load_gc_ptr();
    let func_id = ((inst.flags as u32) << 16) | (inst.b as u32);
    let capture_count = inst.c as u32;
    
    let func_id_val = t.ins_iconst(func_id as i64);
    let func_id_i32 = t.ins_ireduce_i32(func_id_val);
    let capture_count_val = t.ins_iconst(capture_count as i64);
    let capture_count_i32 = t.ins_ireduce_i32(capture_count_val);
    
    if let Some(closure_ref) = t.ins_call(closure_new_func, &[gc_ptr, func_id_i32, capture_count_i32]) {
        t.write_var(inst.a, closure_ref);
    }
}

fn translate_closure_get(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    use vo_runtime::objects::closure::HEADER_SLOTS;
    
    let closure = t.read_var(0);
    let capture_idx = inst.b as usize;
    let offset = ((HEADER_SLOTS + capture_idx) * 8) as i32;
    let val = t.ins_load(types::I64, closure, offset);
    t.write_var(inst.a, val);
}

// =============================================================================
// Interface operations
// =============================================================================

fn translate_iface_assert(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let iface_assert_func = match t.misc_funcs().iface_assert {
        Some(f) => f,
        None => return,
    };
    
    let ctx = t.get_ctx_param();
    let slot0 = t.read_var(inst.b);
    let slot1 = t.read_var(inst.b + 1);
    let target_id = t.ins_iconst(inst.c as i64);
    let target_id_i32 = t.ins_ireduce_i32(target_id);
    let flags = t.ins_iconst(inst.flags as i64);
    let flags_i16 = t.ins_ireduce_i16(flags);
    
    let has_ok = ((inst.flags >> 2) & 0x1) != 0;
    let assert_kind = inst.flags & 0x3;
    let target_slots = (inst.flags >> 3) as usize;
    
    let result_slots = if assert_kind == 1 { 3 } else { target_slots.max(1) + 1 };
    let result_slot = t.create_stack_slot((result_slots * 8) as u32);
    let dst_ptr = t.ins_stack_addr(result_slot);
    
    if let Some(result) = t.ins_call(iface_assert_func, &[ctx, slot0, slot1, target_id_i32, flags_i16, dst_ptr]) {
        if !has_ok {
            let zero = t.ins_iconst(0);
            let is_panic = t.ins_icmp(IntCC::Equal, result, zero);
            
            let panic_block = t.create_block();
            let continue_block = t.create_block();
            
            t.ins_brif(is_panic, panic_block, continue_block);
            
            t.switch_to_block(panic_block);
            t.seal_block(panic_block);
            let panic_val = t.ins_iconst(t.panic_return_value() as i64);
            let panic_i32 = t.ins_ireduce_i32(panic_val);
            t.ins_return(panic_i32);
            
            t.switch_to_block(continue_block);
            t.seal_block(continue_block);
        }
        
        let dst_slots = if assert_kind == 1 { 2 } else { target_slots.max(1) };
        for i in 0..dst_slots {
            let val = t.ins_stack_load(result_slot, (i * 8) as i32);
            t.write_var(inst.a + i as u16, val);
        }
        
        if has_ok {
            let ok_offset = if assert_kind == 1 { 2 } else { target_slots.max(1) };
            let ok_val = t.ins_stack_load(result_slot, (ok_offset * 8) as i32);
            t.write_var(inst.a + ok_offset as u16, ok_val);
        }
    }
}

// =============================================================================
// Panic
// =============================================================================

fn translate_panic(t: &mut impl BytecodeTranslator, inst: &Instruction) {
    let panic_func = match t.panic_func() {
        Some(f) => f,
        None => {
            let panic_val = t.ins_iconst(t.panic_return_value() as i64);
            let panic_i32 = t.ins_ireduce_i32(panic_val);
            t.ins_return(panic_i32);
            return;
        }
    };
    
    let ctx = t.get_ctx_param();
    let msg = t.read_var(inst.b);
    
    t.ins_call(panic_func, &[ctx, msg]);
    
    let panic_val = t.ins_iconst(t.panic_return_value() as i64);
    let panic_i32 = t.ins_ireduce_i32(panic_val);
    t.ins_return(panic_i32);
}

// =============================================================================
// FunctionCompiler-specific implementations
// These methods need access to self.blocks or constant table content
// =============================================================================

use crate::func_compiler::FunctionCompiler;

impl FunctionCompiler<'_> {
    // =========================================================================
    // Control flow (needs self.blocks)
    // =========================================================================

    pub(crate) fn translate_jump(&mut self, inst: &Instruction) {
        // Jump offset is relative to current PC
        let offset = inst.imm32();
        let target_pc = (self.current_pc as i32 + offset) as usize;
        
        // Check if this is a back-edge (loop)
        if target_pc <= self.current_pc {
            self.emit_safepoint();
        }
        
        let target_block = self.blocks[&target_pc];
        self.builder.ins().jump(target_block, &[]);
    }

    pub(crate) fn translate_jump_if(&mut self, inst: &Instruction) {
        let cond = self.read_var(inst.a);
        // Jump offset is relative to current PC
        let offset = inst.imm32();
        let target_pc = (self.current_pc as i32 + offset) as usize;
        
        let target_block = self.blocks[&target_pc];
        let fallthrough_block = self.builder.create_block();
        
        // If cond != 0, jump to target
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond, zero);
        self.builder.ins().brif(cmp, target_block, &[], fallthrough_block, &[]);
        
        self.builder.switch_to_block(fallthrough_block);
        self.builder.seal_block(fallthrough_block);
    }

    pub(crate) fn translate_jump_if_not(&mut self, inst: &Instruction) {
        let cond = self.read_var(inst.a);
        // Jump offset is relative to current PC
        let offset = inst.imm32();
        let target_pc = (self.current_pc as i32 + offset) as usize;
        
        let target_block = self.blocks[&target_pc];
        let fallthrough_block = self.builder.create_block();
        
        // If cond == 0, jump to target
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::Equal, cond, zero);
        self.builder.ins().brif(cmp, target_block, &[], fallthrough_block, &[]);
        
        self.builder.switch_to_block(fallthrough_block);
        self.builder.seal_block(fallthrough_block);
    }

    // =========================================================================
    // Function calls
    // =========================================================================

    pub(crate) fn translate_call(&mut self, inst: &Instruction) {
        // Direct JIT-to-JIT call optimization:
        // 1. Load function pointer from jit_func_table[func_id]
        // 2. If non-null, call JIT function directly
        // 3. Otherwise, fall back to vo_call_vm
        
        let call_vm_func = match self.call_vm_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        // Decode instruction fields
        let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        // Allocate stack space for args and ret buffers
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            3,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointers to buffers
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let ctx = self.get_ctx_param();
        
        // JitContext layout (offset in bytes):
        // jit_func_table: offset 112 (after iface_assert_fn)
        const JIT_FUNC_TABLE_OFFSET: i32 = 112;
        const JIT_FUNC_COUNT_OFFSET: i32 = 120;
        
        // Load jit_func_table pointer from ctx
        let func_table = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            ctx,
            JIT_FUNC_TABLE_OFFSET,
        );

        // Fail-fast: bounds check func_id against jit_func_count before indexing
        let func_count = self.builder.ins().load(
            types::I32,
            cranelift_codegen::ir::MemFlags::trusted(),
            ctx,
            JIT_FUNC_COUNT_OFFSET,
        );
        let func_id_i32 = self.builder.ins().iconst(types::I32, func_id as i64);
        let in_bounds = self.builder.ins().icmp(IntCC::UnsignedLessThan, func_id_i32, func_count);
        let in_bounds_block = self.builder.create_block();
        let oob_block = self.builder.create_block();
        self.builder.ins().brif(in_bounds, in_bounds_block, &[], oob_block, &[]);

        self.builder.switch_to_block(oob_block);
        self.builder.seal_block(oob_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);

        self.builder.switch_to_block(in_bounds_block);
        self.builder.seal_block(in_bounds_block);
        
        // Calculate &jit_func_table[func_id] = func_table + func_id * 8
        let func_id_i64 = self.builder.ins().iconst(types::I64, func_id as i64);
        let offset = self.builder.ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = self.builder.ins().iadd(func_table, offset);
        
        // Load function pointer: jit_func_table[func_id]
        let func_ptr = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            func_ptr_addr,
            0,
        );
        
        // Check if func_ptr is non-null
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_jit = self.builder.ins().icmp(IntCC::NotEqual, func_ptr, zero);
        
        let jit_call_block = self.builder.create_block();
        let vm_call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I32); // result param
        
        self.builder.ins().brif(is_jit, jit_call_block, &[], vm_call_block, &[]);
        
        // === JIT call block ===
        self.builder.switch_to_block(jit_call_block);
        self.builder.seal_block(jit_call_block);
        
        // Build signature for JIT function: (ctx, args, ret) -> i32
        let ptr_type = types::I64;
        let call_conv = self.builder.func.signature.call_conv;
        let mut sig = cranelift_codegen::ir::Signature::new(call_conv);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type)); // args
        sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type)); // ret
        sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // JitResult
        
        let sig_ref = self.builder.import_signature(sig);
        let jit_call = self.builder.ins().call_indirect(sig_ref, func_ptr, &[ctx, arg_ptr, ret_ptr]);
        let jit_result = self.builder.inst_results(jit_call)[0];
        self.builder.ins().jump(merge_block, &[jit_result]);
        
        // === VM call block (fallback) ===
        self.builder.switch_to_block(vm_call_block);
        self.builder.seal_block(vm_call_block);
        
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count_val = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let vm_call = self.builder.ins().call(
            call_vm_func,
            &[ctx, func_id_val, arg_ptr, arg_count_val, ret_ptr, ret_count_val],
        );
        let vm_result = self.builder.inst_results(vm_call)[0];
        self.builder.ins().jump(merge_block, &[vm_result]);
        
        // === Merge block ===
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let result = self.builder.block_params(merge_block)[0];
        
        // Check for panic (result != 0)
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        // Panic block: return JitResult::Panic
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        // Continue block: copy return values
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
    }

    pub(crate) fn translate_call_extern(&mut self, inst: &Instruction) {
        // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_count
        // Return values are written starting at dst (inst.a)
        // In VM, ret_start = arg_start (reuses argument slots), but we use inst.a as dst
        let call_extern_func = match self.call_extern_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        let dst = inst.a;
        let extern_id = inst.b as u32;
        let arg_start = inst.c as usize;
        let arg_count = inst.flags as usize;
        
        // Allocate stack space for args buffer
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_count.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer
        for i in 0..arg_count {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointer to arg buffer (also used for return values)
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        
        // Call vo_call_extern(ctx, extern_id, args, arg_count, ret)
        let ctx = self.get_ctx_param();
        let extern_id_val = self.builder.ins().iconst(types::I32, extern_id as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_count as i64);
        
        let call = self.builder.ins().call(
            call_extern_func,
            &[ctx, extern_id_val, arg_ptr, arg_count_val, arg_ptr], // ret = args (same buffer)
        );
        let result = self.builder.inst_results(call)[0];
        
        // Check for panic (result != 0)
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        // Panic block: return JitResult::Panic
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        // Continue block: copy return values back to dst slots
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        // Extern functions return values in the same buffer (reusing arg slots)
        // Copy them to dst (inst.a). Number of return values = arg_count (same buffer reuse)
        for i in 0..arg_count {
            let val = self.builder.ins().stack_load(types::I64, arg_slot, (i * 8) as i32);
            self.write_var(dst + i as u16, val);
        }
    }

    pub(crate) fn translate_call_closure(&mut self, inst: &Instruction) {
        // CallClosure: a = closure slot, b = arg start, c = (arg_slots << 8) | ret_slots
        let call_closure_func = match self.call_closure_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        let closure_ref = self.read_var(inst.a);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        // Allocate stack space for args and ret buffers (min 8 bytes to avoid zero-size slots)
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            3,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer (excluding closure)
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointers to buffers
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Call vo_call_closure(ctx, closure_ref, args, arg_count, ret, ret_count)
        let ctx = self.get_ctx_param();
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count_val = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let call = self.builder.ins().call(
            call_closure_func,
            &[ctx, closure_ref, arg_ptr, arg_count_val, ret_ptr, ret_count_val],
        );
        let result = self.builder.inst_results(call)[0];
        
        // Check for panic
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        // Copy return values
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
    }

    pub(crate) fn translate_call_iface(&mut self, inst: &Instruction) {
        // CallIface: a = iface slot (2 slots), b = arg start, c = (arg_slots << 8) | ret_slots, flags = method_idx
        let call_iface_func = match self.call_iface_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        let iface_slot0 = self.read_var(inst.a);
        let iface_slot1 = self.read_var(inst.a + 1);
        let method_idx = inst.flags as u32;
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        // Allocate stack space for args and ret buffers (min 8 bytes to avoid zero-size slots)
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            3,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointers to buffers
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Extract itab_id from slot0 and lookup func_id
        // For now, we pass 0 as func_id and let runtime resolve it
        // TODO: Access itab table from JitContext for O(1) method lookup
        let itab_id = self.builder.ins().ushr_imm(iface_slot0, 32);
        let itab_id_i32 = self.builder.ins().ireduce(types::I32, itab_id);
        
        // Call vo_call_iface(ctx, slot0, slot1, method_idx, args, arg_count, ret, ret_count, func_id)
        let ctx = self.get_ctx_param();
        let method_idx_val = self.builder.ins().iconst(types::I32, method_idx as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count_val = self.builder.ins().iconst(types::I32, ret_slots as i64);
        // func_id = 0 means runtime should resolve it
        let func_id_val = self.builder.ins().iconst(types::I32, 0);
        
        let call = self.builder.ins().call(
            call_iface_func,
            &[ctx, iface_slot0, iface_slot1, method_idx_val, arg_ptr, arg_count_val, ret_ptr, ret_count_val, func_id_val],
        );
        let result = self.builder.inst_results(call)[0];
        
        // Check for panic
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        // Copy return values
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
        
        let _ = itab_id_i32; // Will be used for O(1) lookup later
    }

    pub(crate) fn translate_return(&mut self, inst: &Instruction) {
        // Copy return values to ret pointer
        let ret_ptr = self.get_ret_param();
        let ret_slots = self.func_def.ret_slots as usize;
        let ret_reg = inst.a as usize;
        
        for i in 0..ret_slots {
            let val = self.read_var((ret_reg + i) as u16);
            let offset = (i * 8) as i32;
            self.builder.ins().store(
                cranelift_codegen::ir::MemFlags::trusted(),
                val,
                ret_ptr,
                offset,
            );
        }
        
        // Return JitResult::Ok (0)
        let ok = self.builder.ins().iconst(types::I32, 0);
        self.builder.ins().return_(&[ok]);
    }

    // =========================================================================
    // String operations (needs constant table content)
    // =========================================================================

    pub(crate) fn translate_str_new(&mut self, inst: &Instruction) {
        // StrNew: a = dst, b = const_idx
        // Get string constant and create GcRef via vo_str_new
        use vo_runtime::bytecode::Constant;
        
        let str_new_func = match self.str_funcs.str_new {
            Some(f) => f,
            None => return,
        };
        
        let const_idx = inst.b as usize;
        if let Constant::String(s) = &self.vo_module.constants[const_idx] {
            // Create a global data section for the string bytes
            // For now, we'll embed the string bytes as a series of iconst + stores
            // This is inefficient but works for small strings
            // TODO: Use Cranelift's data sections for constant strings
            
            let gc_ptr = self.load_gc_ptr();
            let len = s.len();
            
            if len == 0 {
                // Empty string is null
                let zero = self.builder.ins().iconst(types::I64, 0);
                self.write_var(inst.a, zero);
            } else {
                // Allocate stack space for string bytes
                let stack_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                    cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    len as u32,
                    0,
                ));
                
                // Copy bytes to stack
                let bytes = s.as_bytes();
                for (i, &b) in bytes.iter().enumerate() {
                    let byte_val = self.builder.ins().iconst(types::I8, b as i64);
                    self.builder.ins().stack_store(byte_val, stack_slot, i as i32);
                }
                
                // Get pointer to stack data
                let data_ptr = self.builder.ins().stack_addr(types::I64, stack_slot, 0);
                let len_val = self.builder.ins().iconst(types::I64, len as i64);
                
                // Call vo_str_new(gc, data, len)
                let call = self.builder.ins().call(str_new_func, &[gc_ptr, data_ptr, len_val]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            }
        } else {
            let zero = self.builder.ins().iconst(types::I64, 0);
            self.write_var(inst.a, zero);
        }
    }

    // =========================================================================
    // Interface operations (needs constant table content)
    // =========================================================================

    pub(crate) fn translate_iface_assign(&mut self, inst: &Instruction) {
        // IfaceAssign: a=dst (2 slots), b=src, c=const_idx, flags=value_kind
        use vo_runtime::bytecode::Constant;
        
        let vk = inst.flags;
        let src = self.read_var(inst.b);
        
        let const_idx = inst.c as usize;
        let (rttid, itab_id) = if let Constant::Int(packed) = &self.vo_module.constants[const_idx] {
            let rttid = (*packed >> 32) as u32;
            let itab_id = (*packed & 0xFFFFFFFF) as u32;
            (rttid, itab_id)
        } else {
            (0, 0)
        };
        
        let itab_shifted = (itab_id as u64) << 32;
        let rttid_shifted = (rttid as u64) << 8;
        let slot0_val = itab_shifted | rttid_shifted | (vk as u64);
        let slot0 = self.builder.ins().iconst(types::I64, slot0_val as i64);
        
        let slot1 = if vk == 7 || vk == 8 {
            if let Some(ptr_clone_func) = self.misc_funcs.ptr_clone {
                let gc_ptr = self.load_gc_ptr();
                let call = self.builder.ins().call(ptr_clone_func, &[gc_ptr, src]);
                self.builder.inst_results(call)[0]
            } else {
                src
            }
        } else if vk == 11 {
            self.read_var(inst.b + 1)
        } else {
            src
        };
        
        self.write_var(inst.a, slot0);
        self.write_var(inst.a + 1, slot1);
    }
}

        let str_cmp_func = match self.str_funcs.str_cmp {
            Some(f) => f,
            None => return,
        };
        
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_cmp_func, &[a, b]);
        let cmp_result = self.builder.inst_results(call)[0];
        let zero = self.builder.ins().iconst(types::I32, 0);
        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, cmp_result, zero);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_decode_rune(&mut self, inst: &Instruction) {
        // StrDecodeRune: a = rune_slot, b = str, c = pos
        // Writes: rune at a, width at a+1
        let str_decode_rune_func = match self.str_funcs.str_decode_rune {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let pos = self.read_var(inst.c);
        let call = self.builder.ins().call(str_decode_rune_func, &[s, pos]);
        let packed = self.builder.inst_results(call)[0];
        
        // Unpack: rune = packed >> 32, width = packed & 0xFFFFFFFF
        let rune = self.builder.ins().ushr_imm(packed, 32);
        let width = self.builder.ins().band_imm(packed, 0xFFFFFFFF);
        
        self.write_var(inst.a, rune);
        self.write_var(inst.a + 1, width);
    }

    // =========================================================================
    // Array operations
    // =========================================================================

    pub(crate) fn translate_array_new(&mut self, inst: &Instruction) {
        // ArrayNew: a = dst, b = elem_meta_slot, c = len_slot, flags = elem_slots
        let array_new_func = match self.array_funcs.array_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let meta_raw = self.read_var(inst.b);
        let meta_i32 = self.builder.ins().ireduce(types::I32, meta_raw);
        let elem_slots = self.builder.ins().iconst(types::I32, inst.flags as i64);
        let len = self.read_var(inst.c);
        
        let call = self.builder.ins().call(array_new_func, &[gc_ptr, meta_i32, elem_slots, len]);
        let arr_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, arr_ref);
    }

    pub(crate) fn translate_array_get(&mut self, inst: &Instruction) {
        use vo_runtime::objects::array::HEADER_SLOTS;
        use cranelift_codegen::ir::MemFlags;
        
        let arr = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let header_bytes = (HEADER_SLOTS * 8) as i64;
        
        // flags: 0=dynamic (elem_bytes in c+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed (just u32)
        let (elem_bytes, needs_sext) = match inst.flags {
            0 => (self.get_const_from_reg(inst.c + 1) as usize, false),  // dynamic: elem_bytes in c+1
            0x81 => (1, true),   // int8
            0x82 => (2, true),   // int16
            0x84 => (4, true),   // int32
            0x44 => (4, false),  // float32 (f32 bits as u32)
            f => (f as usize, false),
        };
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            let addr = self.builder.ins().iadd(arr, off);
            
            let val = match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().load(types::I8, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v8) }
                    else { self.builder.ins().uextend(types::I64, v8) }
                }
                2 => {
                    let v16 = self.builder.ins().load(types::I16, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v16) }
                    else { self.builder.ins().uextend(types::I64, v16) }
                }
                4 => {
                    let v32 = self.builder.ins().load(types::I32, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v32) }
                    else { self.builder.ins().uextend(types::I64, v32) }
                }
                _ => self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0),
            };
            self.write_var(inst.a, val);
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            for i in 0..elem_slots {
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(arr, slot_off);
                let val = self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0);
                self.write_var(inst.a + i as u16, val);
            }
        }
    }

    pub(crate) fn translate_array_set(&mut self, inst: &Instruction) {
        use vo_runtime::objects::array::HEADER_SLOTS;
        use cranelift_codegen::ir::MemFlags;
        
        let arr = self.read_var(inst.a);
        let idx = self.read_var(inst.b);
        let val = self.read_var(inst.c);
        let header_bytes = (HEADER_SLOTS * 8) as i64;
        
        // flags: 0=dynamic (elem_bytes in b+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed
        let elem_bytes = match inst.flags {
            0 => self.get_const_from_reg(inst.b + 1) as usize,  // dynamic: elem_bytes in b+1
            0x81 => 1,  // int8
            0x82 => 2,  // int16
            0x84 | 0x44 => 4,  // int32 or float32
            f => f as usize,
        };
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            let addr = self.builder.ins().iadd(arr, off);
            
            match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().ireduce(types::I8, val);
                    self.builder.ins().store(MemFlags::trusted(), v8, addr, 0);
                }
                2 => {
                    let v16 = self.builder.ins().ireduce(types::I16, val);
                    self.builder.ins().store(MemFlags::trusted(), v16, addr, 0);
                }
                4 => {
                    let v32 = self.builder.ins().ireduce(types::I32, val);
                    self.builder.ins().store(MemFlags::trusted(), v32, addr, 0);
                }
                _ => {
                    self.builder.ins().store(MemFlags::trusted(), val, addr, 0);
                }
            }
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            for i in 0..elem_slots {
                let v = self.read_var(inst.c + i as u16);
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(arr, slot_off);
                self.builder.ins().store(MemFlags::trusted(), v, addr, 0);
            }
        }
    }

    // =========================================================================
    // Slice operations
    // =========================================================================

    pub(crate) fn translate_slice_new(&mut self, inst: &Instruction) {
        // SliceNew: a = dst, b = elem_meta_slot, c = len_slot (cap at c+1), flags = elem_flags
        let slice_new_func = match self.slice_funcs.slice_new {
            Some(f) => f,
            None => return,
        };
        
        // flags: 0=dynamic (read from c+2 via LoadConst), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        let elem_bytes: usize = match inst.flags {
            0 => {
                // dynamic: elem_bytes loaded via LoadConst, we need to find the const index
                // Look at the instruction that loads c+2 - it should be a LoadConst
                // For now, read from register (the value is already loaded)
                // TODO: Could optimize by reading from const table directly
                0 // Will use register value
            }
            0x81 => 1,   // int8
            0x82 => 2,   // int16
            0x84 | 0x44 => 4,   // int32 or float32
            f => f as usize,
        };
        let elem_bytes_val = if inst.flags == 0 {
            let eb = self.read_var(inst.c + 2);
            self.builder.ins().ireduce(types::I32, eb)
        } else {
            self.builder.ins().iconst(types::I32, elem_bytes as i64)
        };
        
        let gc_ptr = self.load_gc_ptr();
        let meta_raw = self.read_var(inst.b);
        let meta_i32 = self.builder.ins().ireduce(types::I32, meta_raw);
        let len = self.read_var(inst.c);
        let cap = self.read_var(inst.c + 1);
        
        let call = self.builder.ins().call(slice_new_func, &[gc_ptr, meta_i32, elem_bytes_val, len, cap]);
        let slice_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, slice_ref);
    }

    pub(crate) fn translate_slice_get(&mut self, inst: &Instruction) {
        use vo_runtime::objects::slice::FIELD_DATA_PTR;
        use cranelift_codegen::ir::MemFlags;
        
        let s = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        
        // flags: 0=dynamic (elem_bytes in c+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed
        let (elem_bytes, needs_sext) = match inst.flags {
            0 => {
                let eb = self.get_const_from_reg(inst.c + 1);
                (eb as usize, false)
            }
            0x81 => (1, true),   // int8
            0x82 => (2, true),   // int16
            0x84 => (4, true),   // int32
            0x44 => (4, false),  // float32 (f32 bits as u32)
            f => (f as usize, false),
        };
        
        // Load data_ptr directly - no need to load array and compute offset
        let data_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_DATA_PTR * 8) as i32);
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let addr = self.builder.ins().iadd(data_ptr, off);
            
            let val = match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().load(types::I8, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v8) }
                    else { self.builder.ins().uextend(types::I64, v8) }
                }
                2 => {
                    let v16 = self.builder.ins().load(types::I16, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v16) }
                    else { self.builder.ins().uextend(types::I64, v16) }
                }
                4 => {
                    let v32 = self.builder.ins().load(types::I32, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v32) }
                    else { self.builder.ins().uextend(types::I64, v32) }
                }
                _ => self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0),
            };
            self.write_var(inst.a, val);
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            for i in 0..elem_slots {
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(data_ptr, slot_off);
                let val = self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0);
                self.write_var(inst.a + i as u16, val);
            }
        }
    }

    pub(crate) fn translate_slice_set(&mut self, inst: &Instruction) {
        use vo_runtime::objects::slice::FIELD_DATA_PTR;
        use cranelift_codegen::ir::MemFlags;
        
        let s = self.read_var(inst.a);
        let idx = self.read_var(inst.b);
        let val = self.read_var(inst.c);
        
        // flags: 0=dynamic (elem_bytes in b+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed
        let elem_bytes = match inst.flags {
            0 => self.get_const_from_reg(inst.b + 1) as usize,  // dynamic: elem_bytes in b+1
            0x81 => 1,  // int8
            0x82 => 2,  // int16
            0x84 | 0x44 => 4,  // int32 or float32
            f => f as usize,
        };
        
        // Load data_ptr directly - no need to load array and compute offset
        let data_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_DATA_PTR * 8) as i32);
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let addr = self.builder.ins().iadd(data_ptr, off);
            
            match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().ireduce(types::I8, val);
                    self.builder.ins().store(MemFlags::trusted(), v8, addr, 0);
                }
                2 => {
                    let v16 = self.builder.ins().ireduce(types::I16, val);
                    self.builder.ins().store(MemFlags::trusted(), v16, addr, 0);
                }
                4 => {
                    let v32 = self.builder.ins().ireduce(types::I32, val);
                    self.builder.ins().store(MemFlags::trusted(), v32, addr, 0);
                }
                _ => {
                    self.builder.ins().store(MemFlags::trusted(), val, addr, 0);
                }
            }
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            for i in 0..elem_slots {
                let v = self.read_var(inst.c + i as u16);
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(data_ptr, slot_off);
                self.builder.ins().store(MemFlags::trusted(), v, addr, 0);
            }
        }
    }

    pub(crate) fn translate_slice_len(&mut self, inst: &Instruction) {
        // SliceLen: a = dst, b = slice
        let slice_len_func = match self.slice_funcs.slice_len {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let call = self.builder.ins().call(slice_len_func, &[s]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_slice_cap(&mut self, inst: &Instruction) {
        // SliceCap: a = dst, b = slice
        let slice_cap_func = match self.slice_funcs.slice_cap {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let call = self.builder.ins().call(slice_cap_func, &[s]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_slice_slice(&mut self, inst: &Instruction) {
        // SliceSlice: a = dst, b = slice/array, c = lo (hi at c+1, max at c+2 if three-index)
        // flags: bit0 = is_array, bit1 = has_max
        let is_array = (inst.flags & 1) != 0;
        let has_max = (inst.flags & 2) != 0;
        
        let gc_ptr = self.load_gc_ptr();
        let src = self.read_var(inst.b);
        let lo = self.read_var(inst.c);
        let hi = self.read_var(inst.c + 1);
        
        if is_array {
            // Array slicing: arr[lo:hi] or arr[lo:hi:max]
            if has_max {
                let slice_from_array3_func = match self.slice_funcs.slice_from_array3 {
                    Some(f) => f,
                    None => return,
                };
                let max = self.read_var(inst.c + 2);
                let call = self.builder.ins().call(slice_from_array3_func, &[gc_ptr, src, lo, hi, max]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            } else {
                let slice_from_array_func = match self.slice_funcs.slice_from_array {
                    Some(f) => f,
                    None => return,
                };
                let call = self.builder.ins().call(slice_from_array_func, &[gc_ptr, src, lo, hi]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            }
        } else {
            // Slice slicing: s[lo:hi] or s[lo:hi:max]
            if has_max {
                let slice_slice3_func = match self.slice_funcs.slice_slice3 {
                    Some(f) => f,
                    None => return,
                };
                let max = self.read_var(inst.c + 2);
                let call = self.builder.ins().call(slice_slice3_func, &[gc_ptr, src, lo, hi, max]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            } else {
                let slice_slice_func = match self.slice_funcs.slice_slice {
                    Some(f) => f,
                    None => return,
                };
                let call = self.builder.ins().call(slice_slice_func, &[gc_ptr, src, lo, hi]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            }
        }
    }

    pub(crate) fn translate_slice_append(&mut self, inst: &Instruction) {
        // SliceAppend: a=dst, b=slice, c=meta_and_elem, flags=elem_flags
        // When flags!=0: c=[elem_meta], c+1..=[elem]
        // When flags==0: c=[elem_meta], c+1=[elem_bytes], c+2..=[elem]
        let slice_append_func = match self.slice_funcs.slice_append {
            Some(f) => f,
            None => return,
        };
        
        // flags: 0=dynamic (read from c+1 via LoadConst), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        let (elem_bytes, elem_offset): (usize, u16) = match inst.flags {
            0 => (0, 2),  // dynamic: elem_bytes in c+1, elem at c+2 (0 means read from register)
            0x81 => (1, 1),   // int8
            0x82 => (2, 1),   // int16
            0x84 | 0x44 => (4, 1),   // int32 or float32
            f => (f as usize, 1),
        };
        let is_dynamic = inst.flags == 0;
        
        let gc_ptr = self.load_gc_ptr();
        let s = self.read_var(inst.b);
        
        // Read elem_meta from c (first slot of meta_and_elem)
        let elem_meta_i64 = self.read_var(inst.c);
        let elem_meta = self.builder.ins().ireduce(types::I32, elem_meta_i64);
        let elem_bytes_val = if is_dynamic {
            let eb = self.read_var(inst.c + 1);
            self.builder.ins().ireduce(types::I32, eb)
        } else {
            self.builder.ins().iconst(types::I32, elem_bytes as i64)
        };
        
        // Allocate fixed size (256 bytes = 32 slots) for simplicity
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            256,
            3,
        ));
        
        // Copy slots (max 32 for 256 bytes)
        let max_slots = if is_dynamic { 32 } else { (elem_bytes + 7) / 8 };
        for i in 0..max_slots {
            let val = self.read_var(inst.c + elem_offset + i as u16);
            self.builder.ins().stack_store(val, val_slot, (i * 8) as i32);
        }
        
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        
        let call = self.builder.ins().call(slice_append_func, &[gc_ptr, elem_meta, elem_bytes_val, s, val_ptr]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_slice_addr(&mut self, inst: &Instruction) {
        // SliceAddr: a=dst, b=slice, c=index, flags=elem_bytes
        // Returns data_ptr + idx * elem_bytes
        use vo_runtime::objects::slice::FIELD_DATA_PTR;
        use cranelift_codegen::ir::MemFlags;
        
        let s = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let elem_bytes = inst.flags as i64;
        
        // Load data_ptr from slice
        let data_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_DATA_PTR * 8) as i32);
        
        // addr = data_ptr + idx * elem_bytes
        let eb = self.builder.ins().iconst(types::I64, elem_bytes);
        let off = self.builder.ins().imul(idx, eb);
        let addr = self.builder.ins().iadd(data_ptr, off);
        
        self.write_var(inst.a, addr);
    }

    pub(crate) fn translate_array_addr(&mut self, inst: &Instruction) {
        // ArrayAddr: a=dst, b=array_gcref, c=index, flags=elem_bytes
        // Returns data_ptr + idx * elem_bytes (data starts after ArrayHeader)
        use vo_runtime::objects::array::HEADER_SLOTS;
        
        let arr = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let elem_bytes = inst.flags as i64;
        
        // data_ptr = arr + HEADER_SLOTS * 8 (ArrayHeader is 2 slots = 16 bytes)
        let data_ptr = self.builder.ins().iadd_imm(arr, (HEADER_SLOTS * 8) as i64);
        
        // addr = data_ptr + idx * elem_bytes
        let eb = self.builder.ins().iconst(types::I64, elem_bytes);
        let off = self.builder.ins().imul(idx, eb);
        let addr = self.builder.ins().iadd(data_ptr, off);
        
        self.write_var(inst.a, addr);
    }

    // =========================================================================
    // Map operations
    // =========================================================================

    pub(crate) fn translate_map_new(&mut self, inst: &Instruction) {
        // MapNew: a = dst, b = packed_meta_slot, c = (key_slots << 8) | val_slots
        // packed_meta = (key_meta << 32) | val_meta
        let map_new_func = match self.map_funcs.map_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let packed_meta = self.read_var(inst.b);
        
        // Unpack key_meta (high 32) and val_meta (low 32)
        let key_meta = self.builder.ins().ushr_imm(packed_meta, 32);
        let key_meta_i32 = self.builder.ins().ireduce(types::I32, key_meta);
        let val_meta_i32 = self.builder.ins().ireduce(types::I32, packed_meta);
        
        let key_slots = (inst.c >> 8) as i64;
        let val_slots = (inst.c & 0xFF) as i64;
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots);
        
        let call = self.builder.ins().call(map_new_func, &[gc_ptr, key_meta_i32, val_meta_i32, key_slots_val, val_slots_val]);
        let map_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, map_ref);
    }

    pub(crate) fn translate_map_get(&mut self, inst: &Instruction) {
        // MapGet: a = dst, b = map, c = key_start, flags = (key_slots << 4) | val_slots
        let map_get_func = match self.map_funcs.map_get {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.b);
        let key_slots = (inst.flags >> 4) as usize;
        let val_slots = (inst.flags & 0xF) as usize;
        
        // Allocate stack space for key and value
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (val_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy key to stack
        for i in 0..key_slots {
            let val = self.read_var(inst.c + i as u16);
            self.builder.ins().stack_store(val, key_slot, (i * 8) as i32);
        }
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots as i64);
        
        let call = self.builder.ins().call(map_get_func, &[m, key_ptr, key_slots_val, val_ptr, val_slots_val]);
        let found = self.builder.inst_results(call)[0];
        
        // Copy value back from stack (dst = inst.a for value, found flag at dst + val_slots)
        for i in 0..val_slots {
            let val = self.builder.ins().stack_load(types::I64, val_slot, (i * 8) as i32);
            self.write_var(inst.a + i as u16, val);
        }
        self.write_var(inst.a + val_slots as u16, found);
    }

    pub(crate) fn translate_map_set(&mut self, inst: &Instruction) {
        // MapSet: a = map, b = key_start, c = val_start, flags = (key_slots << 4) | val_slots
        let map_set_func = match self.map_funcs.map_set {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.a);
        let key_slots = (inst.flags >> 4) as usize;
        let val_slots = (inst.flags & 0xF) as usize;
        
        // Allocate stack space for key and value
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (val_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy key and value to stack
        for i in 0..key_slots {
            let val = self.read_var(inst.b + i as u16);
            self.builder.ins().stack_store(val, key_slot, (i * 8) as i32);
        }
        for i in 0..val_slots {
            let val = self.read_var(inst.c + i as u16);
            self.builder.ins().stack_store(val, val_slot, (i * 8) as i32);
        }
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots as i64);
        
        self.builder.ins().call(map_set_func, &[m, key_ptr, key_slots_val, val_ptr, val_slots_val]);
    }

    pub(crate) fn translate_map_delete(&mut self, inst: &Instruction) {
        // MapDelete: a = map, b = key_start, flags = key_slots
        let map_delete_func = match self.map_funcs.map_delete {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.a);
        let key_slots = inst.flags as usize;
        
        // Allocate stack space for key
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy key to stack
        for i in 0..key_slots {
            let val = self.read_var(inst.b + i as u16);
            self.builder.ins().stack_store(val, key_slot, (i * 8) as i32);
        }
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        
        self.builder.ins().call(map_delete_func, &[m, key_ptr, key_slots_val]);
    }

    pub(crate) fn translate_map_len(&mut self, inst: &Instruction) {
        // MapLen: a = dst, b = map
        let map_len_func = match self.map_funcs.map_len {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.b);
        let call = self.builder.ins().call(map_len_func, &[m]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_map_iter_get(&mut self, inst: &Instruction) {
        // MapIterGet: a = dst (key+val+done), b = map, c = idx, flags = (key_slots << 4) | val_slots
        let map_iter_get_func = match self.map_funcs.map_iter_get {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let key_slots = (inst.flags >> 4) as usize;
        let val_slots = (inst.flags & 0xF) as usize;
        
        // Allocate stack space for key and value
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (val_slots.max(1) * 8) as u32,
            3,
        ));
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots as i64);
        
        let call = self.builder.ins().call(map_iter_get_func, &[m, idx, key_ptr, key_slots_val, val_ptr, val_slots_val]);
        let done = self.builder.inst_results(call)[0];
        
        // Copy key and value back from stack
        for i in 0..key_slots {
            let val = self.builder.ins().stack_load(types::I64, key_slot, (i * 8) as i32);
            self.write_var(inst.a + i as u16, val);
        }
        for i in 0..val_slots {
            let val = self.builder.ins().stack_load(types::I64, val_slot, (i * 8) as i32);
            self.write_var(inst.a + key_slots as u16 + i as u16, val);
        }
        self.write_var(inst.a + (key_slots + val_slots) as u16, done);
    }

    // =========================================================================
    // Channel operations (only ChanNew is JIT-able)
    // =========================================================================

    pub(crate) fn translate_chan_new(&mut self, inst: &Instruction) {
        // ChanNew: a = dst, b = elem_meta_slot, c = cap_slot, flags = elem_slots
        let chan_new_func = match self.misc_funcs.chan_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let elem_meta = self.read_var(inst.b);
        let elem_meta_i32 = self.builder.ins().ireduce(types::I32, elem_meta);
        let elem_slots = self.builder.ins().iconst(types::I32, inst.flags as i64);
        let cap = self.read_var(inst.c);
        
        let call = self.builder.ins().call(chan_new_func, &[gc_ptr, elem_meta_i32, elem_slots, cap]);
        let chan_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, chan_ref);
    }

    // =========================================================================
    // Closure operations
    // =========================================================================

    pub(crate) fn translate_closure_new(&mut self, inst: &Instruction) {
        // ClosureNew: a = dst, b = func_id_low, c = capture_count, flags = func_id_high
        let closure_new_func = match self.misc_funcs.closure_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let func_id = ((inst.flags as u32) << 16) | (inst.b as u32);
        let capture_count = inst.c as u32;
        
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let capture_count_val = self.builder.ins().iconst(types::I32, capture_count as i64);
        
        let call = self.builder.ins().call(closure_new_func, &[gc_ptr, func_id_val, capture_count_val]);
        let closure_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, closure_ref);
    }

    pub(crate) fn translate_closure_get(&mut self, inst: &Instruction) {
        // ClosureGet: a = dst, b = capture_idx
        // Closure is always in slot 0 of current function
        use vo_runtime::objects::closure::HEADER_SLOTS;
        
        let closure = self.read_var(0);
        let capture_idx = inst.b as usize;
        
        // Offset = (HEADER_SLOTS + capture_idx) * 8 bytes
        let offset = ((HEADER_SLOTS + capture_idx) * 8) as i32;
        let val = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            closure,
            offset,
        );
        self.write_var(inst.a, val);
    }

    // =========================================================================
    // Interface operations
    // =========================================================================

    pub(crate) fn translate_iface_assign(&mut self, inst: &Instruction) {
        // IfaceAssign: a=dst (2 slots), b=src, c=const_idx, flags=value_kind
        // For concrete type -> interface (simple case):
        // slot0 = pack(itab_id, rttid, vk), slot1 = src or clone(src)
        //
        // Complex cases (interface->interface) need runtime itab lookup
        // TODO: Add vo_iface_assign runtime helper for complex cases
        
        use vo_runtime::bytecode::Constant;
        
        let vk = inst.flags;
        let src = self.read_var(inst.b);
        
        // Read packed constant: (rttid << 32) | itab_id
        let const_idx = inst.c as usize;
        let (rttid, itab_id) = if let Constant::Int(packed) = &self.vo_module.constants[const_idx] {
            let rttid = (*packed >> 32) as u32;
            let itab_id = (*packed & 0xFFFFFFFF) as u32;
            (rttid, itab_id)
        } else {
            (0, 0)
        };
        
        // Pack slot0: (itab_id << 32) | (rttid << 8) | vk
        let itab_shifted = (itab_id as u64) << 32;
        let rttid_shifted = (rttid as u64) << 8;
        let slot0_val = itab_shifted | rttid_shifted | (vk as u64);
        let slot0 = self.builder.ins().iconst(types::I64, slot0_val as i64);
        
        // slot1 depends on value kind
        // ValueKind::Struct = 7, Array = 8 need ptr_clone (deep copy for value semantics)
        // ValueKind::Interface = 11 is complex (skip for now)
        let slot1 = if vk == 7 || vk == 8 {
            // Struct/Array: need deep copy via vo_ptr_clone
            if let Some(ptr_clone_func) = self.misc_funcs.ptr_clone {
                let gc_ptr = self.load_gc_ptr();
                let call = self.builder.ins().call(ptr_clone_func, &[gc_ptr, src]);
                self.builder.inst_results(call)[0]
            } else {
                // Fallback: just copy pointer (incorrect but allows compilation)
                src
            }
        } else if vk == 11 {
            // Interface -> Interface: need runtime support
            // For now, just copy slot1 from source
            self.read_var(inst.b + 1)
        } else {
            // Primitive types: direct copy
            src
        };
        
        self.write_var(inst.a, slot0);
        self.write_var(inst.a + 1, slot1);
    }
}
