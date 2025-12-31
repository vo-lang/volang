//! IrEmitter trait - shared IR generation interface.

use cranelift_codegen::ir::{FuncRef, Value};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::bytecode::Module as VoModule;

/// Translation result
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslateResult {
    /// Instruction handled
    Completed,
    /// Block terminated (e.g. panic)
    Terminated,
    /// Needs compiler-specific handling
    Unhandled,
}

/// Runtime helper function references
#[derive(Default, Clone, Copy)]
pub struct HelperFuncs {
    pub safepoint: Option<FuncRef>,
    pub call_vm: Option<FuncRef>,
    pub gc_alloc: Option<FuncRef>,
    pub call_closure: Option<FuncRef>,
    pub call_iface: Option<FuncRef>,
    pub panic: Option<FuncRef>,
    pub call_extern: Option<FuncRef>,
    pub str_new: Option<FuncRef>,
    pub str_len: Option<FuncRef>,
    pub str_index: Option<FuncRef>,
    pub str_concat: Option<FuncRef>,
    pub str_slice: Option<FuncRef>,
    pub str_eq: Option<FuncRef>,
    pub str_cmp: Option<FuncRef>,
    pub str_decode_rune: Option<FuncRef>,
    pub ptr_clone: Option<FuncRef>,
    pub closure_new: Option<FuncRef>,
    pub chan_new: Option<FuncRef>,
    pub array_new: Option<FuncRef>,
    pub array_len: Option<FuncRef>,
    pub slice_new: Option<FuncRef>,
    pub slice_len: Option<FuncRef>,
    pub slice_cap: Option<FuncRef>,
    pub slice_append: Option<FuncRef>,
    pub slice_slice: Option<FuncRef>,
    pub slice_slice3: Option<FuncRef>,
    pub slice_from_array: Option<FuncRef>,
    pub slice_from_array3: Option<FuncRef>,
    pub map_new: Option<FuncRef>,
    pub map_len: Option<FuncRef>,
    pub map_get: Option<FuncRef>,
    pub map_set: Option<FuncRef>,
    pub map_delete: Option<FuncRef>,
    pub map_iter_get: Option<FuncRef>,
    pub iface_assert: Option<FuncRef>,
}

/// IR emitter trait - implemented by FunctionCompiler and LoopCompiler
pub trait IrEmitter<'a> {
    /// Get FunctionBuilder
    fn builder(&mut self) -> &mut FunctionBuilder<'a>;
    
    /// Read variable
    fn read_var(&mut self, slot: u16) -> Value;
    
    /// Write variable
    fn write_var(&mut self, slot: u16, val: Value);
    
    /// Get ctx parameter
    fn ctx_param(&mut self) -> Value;
    
    /// Load GC pointer
    fn gc_ptr(&mut self) -> Value;
    
    /// Load globals pointer
    fn globals_ptr(&mut self) -> Value;
    
    /// Get Vo module
    fn vo_module(&self) -> &VoModule;
    
    /// Get current PC
    fn current_pc(&self) -> usize;
    
    /// Emit safepoint check
    fn emit_safepoint(&mut self);
    
    /// Get helper function references
    fn helpers(&self) -> &HelperFuncs;
    
    /// Set register constant (for optimization)
    fn set_reg_const(&mut self, reg: u16, val: i64);
    
    /// Get register constant
    fn get_reg_const(&self, reg: u16) -> Option<i64>;
    
    /// Panic return value (FunctionCompiler=1, LoopCompiler=LOOP_RESULT_PANIC)
    fn panic_return_value(&self) -> i32;
}
