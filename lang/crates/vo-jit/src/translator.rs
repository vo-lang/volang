//! IrEmitter trait - shared IR generation interface.

use cranelift_codegen::ir::{types, FuncRef, Value};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::Module as VoModule;
use vo_runtime::instruction::{Instruction, Opcode};
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
    pub island_new: Option<FuncRef>,
    pub chan_close: Option<FuncRef>,
    pub port_close: Option<FuncRef>,
    pub chan_send: Option<FuncRef>,
    pub chan_recv: Option<FuncRef>,
    pub port_send: Option<FuncRef>,
    pub port_recv: Option<FuncRef>,
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
    
    /// Reload all SSA variables from memory.
    /// Called after external callbacks that may write to locals memory without updating SSA
    /// (e.g., select_exec writes recv results to fiber.stack via callback).
    fn reload_all_vars_from_memory(&mut self);

    /// Check if a slot has been verified non-nil in the current basic block.
    fn is_checked_non_nil(&self, slot: u16) -> bool;
    
    /// Mark a slot as verified non-nil (after nil check passed).
    fn mark_checked_non_nil(&mut self, slot: u16);
}

/// Scan instructions to find the minimum base register used in SlotSet/SlotSetN.
/// All slots below this value are safe for SSA reads (no aliasing with pointer-based memory writes).
pub fn compute_memory_only_start(code: &[Instruction]) -> u16 {
    let mut min_base = u16::MAX;
    for inst in code {
        match inst.opcode() {
            Opcode::SlotSet | Opcode::SlotSetN => {
                min_base = min_base.min(inst.a);
            }
            _ => {}
        }
    }
    min_base
}

#[inline]
pub fn is_float_slot(slot_types: &[SlotType], slot: u16) -> bool {
    slot_types.get(slot as usize).copied() == Some(SlotType::Float)
}

/// Cranelift IR type for a variable slot: F64 for Float slots, I64 for everything else.
#[inline]
pub fn slot_ir_type(slot_types: &[SlotType], slot: u16) -> cranelift_codegen::ir::Type {
    if is_float_slot(slot_types, slot) { types::F64 } else { types::I64 }
}

/// Declare SSA variables for all local slots, using F64 for Float slots.
pub fn declare_variables(
    builder: &mut FunctionBuilder,
    num_slots: usize,
    slot_types: &[SlotType],
) -> Vec<Variable> {
    let mut vars = Vec::with_capacity(num_slots);
    for i in 0..num_slots {
        let var = Variable::from_u32(i as u32);
        let ty = slot_ir_type(slot_types, i as u16);
        builder.declare_var(var, ty);
        vars.push(var);
    }
    vars
}
