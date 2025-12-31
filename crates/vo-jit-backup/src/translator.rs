//! BytecodeTranslator trait for shared instruction translation.
//!
//! This trait abstracts the common interface needed by translate.rs,
//! allowing both FunctionCompiler and LoopCompiler to reuse translation logic.

use std::collections::HashMap;

use cranelift_codegen::ir::{types, FuncRef, Value};

use vo_runtime::bytecode::Module as VoModule;

use crate::func_compiler::{StringFuncs, MapFuncs, ArrayFuncs, SliceFuncs, MiscFuncs};

/// Trait for bytecode translators (FunctionCompiler, LoopCompiler).
///
/// Provides the common interface needed by instruction translation functions.
/// Instead of exposing FunctionBuilder directly, we provide specific operations.
pub trait BytecodeTranslator {
    /// Read a variable (slot) value.
    fn read_var(&mut self, slot: u16) -> Value;
    
    /// Write a value to a variable (slot).
    fn write_var(&mut self, slot: u16, val: Value);
    
    /// Get the ctx parameter.
    fn get_ctx_param(&mut self) -> Value;
    
    /// Get the Vo module.
    fn vo_module(&self) -> &VoModule;
    
    /// Get current PC being translated.
    fn current_pc(&self) -> usize;
    
    /// Insert a constant into reg_consts.
    fn set_reg_const(&mut self, reg: u16, val: i64);
    
    /// Get const from reg.
    fn get_const_from_reg(&self, reg: u16) -> i64;
    
    /// Load globals pointer from JitContext.
    fn load_globals_ptr(&mut self) -> Value;
    
    /// Load GC pointer from JitContext.
    fn load_gc_ptr(&mut self) -> Value;
    
    /// Emit a safepoint check.
    fn emit_safepoint(&mut self);
    
    // =========================================================================
    // Cranelift IR building operations
    // =========================================================================
    
    /// Create an integer constant.
    fn ins_iconst(&mut self, val: i64) -> Value;
    
    /// Create a float constant (as bits).
    fn ins_f64const(&mut self, bits: u64) -> Value;
    
    /// Integer add.
    fn ins_iadd(&mut self, a: Value, b: Value) -> Value;
    
    /// Integer subtract.
    fn ins_isub(&mut self, a: Value, b: Value) -> Value;
    
    /// Integer multiply.
    fn ins_imul(&mut self, a: Value, b: Value) -> Value;
    
    /// Signed integer divide.
    fn ins_sdiv(&mut self, a: Value, b: Value) -> Value;
    
    /// Signed integer remainder.
    fn ins_srem(&mut self, a: Value, b: Value) -> Value;
    
    /// Integer negate.
    fn ins_ineg(&mut self, a: Value) -> Value;
    
    /// Float add.
    fn ins_fadd(&mut self, a: Value, b: Value) -> Value;
    
    /// Float subtract.
    fn ins_fsub(&mut self, a: Value, b: Value) -> Value;
    
    /// Float multiply.
    fn ins_fmul(&mut self, a: Value, b: Value) -> Value;
    
    /// Float divide.
    fn ins_fdiv(&mut self, a: Value, b: Value) -> Value;
    
    /// Float negate.
    fn ins_fneg(&mut self, a: Value) -> Value;
    
    /// Bitcast i64 to f64.
    fn ins_bitcast_i64_to_f64(&mut self, val: Value) -> Value;
    
    /// Bitcast f64 to i64.
    fn ins_bitcast_f64_to_i64(&mut self, val: Value) -> Value;
    
    /// Integer compare.
    fn ins_icmp(&mut self, cc: cranelift_codegen::ir::condcodes::IntCC, a: Value, b: Value) -> Value;
    
    /// Float compare.
    fn ins_fcmp(&mut self, cc: cranelift_codegen::ir::condcodes::FloatCC, a: Value, b: Value) -> Value;
    
    /// Zero extend i8 to i64.
    fn ins_uextend_i8_to_i64(&mut self, val: Value) -> Value;
    
    /// Bitwise AND.
    fn ins_band(&mut self, a: Value, b: Value) -> Value;
    
    /// Bitwise OR.
    fn ins_bor(&mut self, a: Value, b: Value) -> Value;
    
    /// Bitwise XOR.
    fn ins_bxor(&mut self, a: Value, b: Value) -> Value;
    
    /// Bitwise NOT.
    fn ins_bnot(&mut self, a: Value) -> Value;
    
    /// Shift left.
    fn ins_ishl(&mut self, a: Value, b: Value) -> Value;
    
    /// Arithmetic shift right.
    fn ins_sshr(&mut self, a: Value, b: Value) -> Value;
    
    /// Logical shift right.
    fn ins_ushr(&mut self, a: Value, b: Value) -> Value;
    
    /// Logical shift right by immediate.
    fn ins_ushr_imm(&mut self, a: Value, imm: i64) -> Value;
    
    /// Load from memory.
    fn ins_load(&mut self, ty: types::Type, ptr: Value, offset: i32) -> Value;
    
    /// Store to memory.
    fn ins_store(&mut self, val: Value, ptr: Value, offset: i32);
    
    /// Call a function.
    fn ins_call(&mut self, func: FuncRef, args: &[Value]) -> Option<Value>;
    
    /// Select between two values based on condition.
    fn ins_select(&mut self, cond: Value, a: Value, b: Value) -> Value;
    
    /// Integer multiply by immediate.
    fn ins_imul_imm(&mut self, a: Value, imm: i64) -> Value;
    
    /// Integer add immediate.
    fn ins_iadd_imm(&mut self, a: Value, imm: i64) -> Value;
    
    /// Reduce i64 to i32.
    fn ins_ireduce_i32(&mut self, val: Value) -> Value;
    
    /// Reduce i64 to i16.
    fn ins_ireduce_i16(&mut self, val: Value) -> Value;
    
    /// Reduce i64 to i8.
    fn ins_ireduce_i8(&mut self, val: Value) -> Value;
    
    /// Sign extend i8 to i64.
    fn ins_sextend_i8_to_i64(&mut self, val: Value) -> Value;
    
    /// Sign extend i16 to i64.
    fn ins_sextend_i16_to_i64(&mut self, val: Value) -> Value;
    
    /// Sign extend i32 to i64.
    fn ins_sextend_i32_to_i64(&mut self, val: Value) -> Value;
    
    /// Zero extend i32 to i64.
    fn ins_uextend_i32_to_i64(&mut self, val: Value) -> Value;
    
    /// Zero extend i16 to i64.
    fn ins_uextend_i16_to_i64(&mut self, val: Value) -> Value;
    
    /// Load i8 from memory.
    fn ins_load_i8(&mut self, ptr: Value, offset: i32) -> Value;
    
    /// Load i16 from memory.
    fn ins_load_i16(&mut self, ptr: Value, offset: i32) -> Value;
    
    /// Load i32 from memory.
    fn ins_load_i32(&mut self, ptr: Value, offset: i32) -> Value;
    
    /// Store i8 to memory.
    fn ins_store_i8(&mut self, val: Value, ptr: Value, offset: i32);
    
    /// Store i16 to memory.
    fn ins_store_i16(&mut self, val: Value, ptr: Value, offset: i32);
    
    /// Store i32 to memory.
    fn ins_store_i32(&mut self, val: Value, ptr: Value, offset: i32);
    
    /// Convert signed int to f64.
    fn ins_fcvt_from_sint(&mut self, val: Value) -> Value;
    
    /// Convert f64 to signed int.
    fn ins_fcvt_to_sint(&mut self, val: Value) -> Value;
    
    /// Demote f64 to f32.
    fn ins_fdemote(&mut self, val: Value) -> Value;
    
    /// Promote f32 to f64.
    fn ins_fpromote(&mut self, val: Value) -> Value;
    
    /// Create a sized stack slot.
    fn create_stack_slot(&mut self, size: u32) -> cranelift_codegen::ir::StackSlot;
    
    /// Store to stack slot.
    fn ins_stack_store(&mut self, val: Value, slot: cranelift_codegen::ir::StackSlot, offset: i32);
    
    /// Load from stack slot.
    fn ins_stack_load(&mut self, slot: cranelift_codegen::ir::StackSlot, offset: i32) -> Value;
    
    /// Get stack slot address.
    fn ins_stack_addr(&mut self, slot: cranelift_codegen::ir::StackSlot) -> Value;
    
    /// Create a new block.
    fn create_block(&mut self) -> cranelift_codegen::ir::Block;
    
    /// Switch to block.
    fn switch_to_block(&mut self, block: cranelift_codegen::ir::Block);
    
    /// Seal block.
    fn seal_block(&mut self, block: cranelift_codegen::ir::Block);
    
    /// Branch if condition is true.
    fn ins_brif(&mut self, cond: Value, then_block: cranelift_codegen::ir::Block, else_block: cranelift_codegen::ir::Block);
    
    /// Return with value.
    fn ins_return(&mut self, val: Value);
    
    /// Get the panic return value for this compiler context.
    fn panic_return_value(&self) -> i32;
    
    // =========================================================================
    // Helper FuncRefs
    // =========================================================================
    
    fn safepoint_func(&self) -> Option<FuncRef>;
    fn call_vm_func(&self) -> Option<FuncRef>;
    fn gc_alloc_func(&self) -> Option<FuncRef>;
    fn call_closure_func(&self) -> Option<FuncRef>;
    fn call_iface_func(&self) -> Option<FuncRef>;
    fn panic_func(&self) -> Option<FuncRef>;
    fn call_extern_func(&self) -> Option<FuncRef>;
    fn str_funcs(&self) -> &StringFuncs;
    fn map_funcs(&self) -> &MapFuncs;
    fn array_funcs(&self) -> &ArrayFuncs;
    fn slice_funcs(&self) -> &SliceFuncs;
    fn misc_funcs(&self) -> &MiscFuncs;
}
