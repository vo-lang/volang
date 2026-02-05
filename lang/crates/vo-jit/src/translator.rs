//! IrEmitter trait - shared IR generation interface.

use cranelift_codegen::ir::{FuncRef, Value};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::bytecode::Module as VoModule;
use vo_runtime::SlotType;

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
    pub call_vm: Option<FuncRef>,
    pub gc_alloc: Option<FuncRef>,
    pub write_barrier: Option<FuncRef>,
    pub closure_get_func_id: Option<FuncRef>,
    pub iface_get_func_id: Option<FuncRef>,
    pub set_closure_call_request: Option<FuncRef>,
    pub set_iface_call_request: Option<FuncRef>,
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
    pub chan_new_checked: Option<FuncRef>,
    pub chan_len: Option<FuncRef>,
    pub chan_cap: Option<FuncRef>,
    pub port_new_checked: Option<FuncRef>,
    pub port_len: Option<FuncRef>,
    pub port_cap: Option<FuncRef>,
    pub array_new: Option<FuncRef>,
    pub array_len: Option<FuncRef>,
    pub slice_new_checked: Option<FuncRef>,
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
    pub map_iter_init: Option<FuncRef>,
    pub map_iter_next: Option<FuncRef>,
    pub iface_assert: Option<FuncRef>,
    pub iface_to_iface: Option<FuncRef>,
    pub iface_eq: Option<FuncRef>,
    pub set_call_request: Option<FuncRef>,
    // Batch 1: Island/Channel/Port operations
    pub island_new: Option<FuncRef>,
    pub chan_close: Option<FuncRef>,
    pub port_close: Option<FuncRef>,
    // Batch 2: Channel Send/Recv
    pub chan_send: Option<FuncRef>,
    pub chan_recv: Option<FuncRef>,
    // Batch 3: Port Send/Recv
    pub port_send: Option<FuncRef>,
    pub port_recv: Option<FuncRef>,
    // Batch 4: Goroutine Start
    pub go_start: Option<FuncRef>,
    pub go_island: Option<FuncRef>,
    // Defer/Recover
    pub defer_push: Option<FuncRef>,
    pub recover: Option<FuncRef>,
    // Select Statement
    pub select_begin: Option<FuncRef>,
    pub select_send: Option<FuncRef>,
    pub select_recv: Option<FuncRef>,
    pub select_exec: Option<FuncRef>,
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
    
    /// Get helper function references
    fn helpers(&self) -> &HelperFuncs;
    
    /// Set register constant (for optimization)
    fn set_reg_const(&mut self, reg: u16, val: i64);
    
    /// Get register constant
    fn get_reg_const(&self, reg: u16) -> Option<i64>;
    
    /// Panic return value (FunctionCompiler=1, LoopCompiler=LOOP_RESULT_PANIC)
    fn panic_return_value(&self) -> i32;
    
    /// Get memory address of a variable slot.
    /// Used by SlotGet/SlotSet for stack array access.
    fn var_addr(&mut self, slot: u16) -> Value;
    
    /// Spill all SSA variables to memory.
    /// Called before returning non-Ok JitResult so VM can see/restore state.
    fn spill_all_vars(&mut self);
    
    /// Get the number of local variable slots.
    fn local_slot_count(&self) -> usize;
    
    /// Get the function ID being compiled.
    fn func_id(&self) -> u32;
    
    /// Get slot type for a variable.
    fn slot_type(&self, slot: u16) -> SlotType;
    
    /// Read variable as F64. Load F64 directly from memory.
    fn read_var_f64(&mut self, slot: u16) -> Value;
    
    /// Write variable as F64. Store F64 directly to memory.
    fn write_var_f64(&mut self, slot: u16, val: Value);
    
    /// Check if a slot has been verified non-nil in the current basic block.
    fn is_checked_non_nil(&self, slot: u16) -> bool;
    
    /// Mark a slot as verified non-nil (after nil check passed).
    fn mark_checked_non_nil(&mut self, slot: u16);
}
