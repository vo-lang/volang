//! Shared IR generation interface.

use cranelift_codegen::ir::{FuncRef, Value};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::bytecode::{JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::Instruction;

use crate::JitError;

mod helper_calls;
mod reg_const_facts;

pub use helper_calls::{
    emit_funcref_call, emit_funcref_call_raw, emit_funcref_call_with_effect, HelperCallEffect,
};
pub use reg_const_facts::compute_reg_const_facts_with_context;

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
    pub gc_alloc: Option<FuncRef>,
    pub write_barrier: Option<FuncRef>,
    pub typed_write_barrier_by_meta: Option<FuncRef>,
    pub panic: Option<FuncRef>,
    pub runtime_trap: Option<FuncRef>,
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
    pub queue_new_checked: Option<FuncRef>,
    pub queue_len: Option<FuncRef>,
    pub queue_cap: Option<FuncRef>,
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
    pub queue_close: Option<FuncRef>,
    pub queue_send: Option<FuncRef>,
    pub queue_recv: Option<FuncRef>,
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

#[derive(Debug, Clone, Copy)]
pub enum SelectSyncCase {
    Send,
    Recv {
        dst_reg: u16,
        elem_slots: u8,
        has_ok: bool,
    },
}

/// Mutable access to the Cranelift function builder.
pub trait IrBuilder<'a> {
    /// Get FunctionBuilder
    fn builder(&mut self) -> &mut FunctionBuilder<'a>;
}

/// Slot storage operations for the current compiled frame.
pub trait SlotAccess<'a>: IrBuilder<'a> {
    /// Read variable
    fn read_var(&mut self, slot: u16) -> Value;

    /// Write variable
    fn write_var(&mut self, slot: u16, val: Value);

    /// Get memory address of a variable slot.
    /// Used by SlotGet/SlotSet for stack array access.
    fn var_addr(&mut self, slot: u16) -> Value;

    /// Synchronize the requested SSA-visible slots to frame memory.
    fn sync_slots_to_memory(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError>;

    /// Get the number of local variable slots.
    fn local_slot_count(&self) -> usize;

    /// Read variable as F64. Load F64 directly from memory.
    fn read_var_f64(&mut self, slot: u16) -> Value;

    /// Write variable as F64. Store F64 directly to memory.
    fn write_var_f64(&mut self, slot: u16, val: Value);

    /// Reload all SSA variables from memory.
    /// Called after external callbacks that may write to locals memory without updating SSA
    /// (e.g., select_exec writes recv results to fiber.stack via callback).
    fn reload_all_vars_from_memory(&mut self);

    fn sync_written_slots(&mut self, _start_slot: u16, _slot_count: u16) -> Result<(), JitError> {
        self.reload_all_vars_from_memory();
        Ok(())
    }
}

/// Access to the JIT runtime context and global runtime pointers.
pub trait RuntimeContext<'a>: IrBuilder<'a> {
    /// Get ctx parameter
    fn ctx_param(&mut self) -> Value;

    /// Load GC pointer
    fn gc_ptr(&mut self) -> Value;

    /// Load globals pointer
    fn globals_ptr(&mut self) -> Value;
}

/// Module, function, and instruction metadata needed during lowering.
pub trait MetadataAccess {
    /// Get Vo module
    fn vo_module(&self) -> &VoModule;

    /// Get current PC
    fn current_pc(&self) -> usize;

    /// Get the function ID being compiled.
    fn func_id(&self) -> u32;

    /// Get JIT metadata attached to the instruction at current_pc, if present.
    fn current_jit_metadata(&self) -> Option<&JitInstructionMetadata> {
        None
    }

    /// Resolve typed array/slice element metadata for JIT lowering.
    fn elem_layout(
        &self,
        flags: u8,
        _dynamic_bytes_slot: u16,
    ) -> Option<crate::metadata::ElemLayout> {
        if flags == 0 {
            self.current_jit_metadata()
                .and_then(crate::metadata::elem_layout_from_instruction)
        } else {
            Some(crate::metadata::elem_layout_from_flags(flags))
        }
    }

    /// Resolve typed map-get metadata for JIT lowering.
    fn map_get_layout(&self, inst: &Instruction) -> Option<crate::metadata::MapGetLayout> {
        let _ = inst;
        self.current_jit_metadata()
            .and_then(crate::metadata::map_get_layout_from_instruction)
    }

    /// Resolve typed map-set metadata for JIT lowering.
    fn map_set_layout(&self, inst: &Instruction) -> Option<crate::metadata::MapSetLayout> {
        let _ = inst;
        self.current_jit_metadata()
            .and_then(crate::metadata::map_set_layout_from_instruction)
    }

    /// Resolve typed map-delete metadata for JIT lowering.
    fn map_delete_key_slots(&self, inst: &Instruction) -> Option<u16> {
        let _ = inst;
        self.current_jit_metadata()
            .and_then(crate::metadata::map_delete_key_slots_from_instruction)
    }
}

/// Runtime helper function references.
pub trait HelperAccess {
    /// Get helper function references
    fn helpers(&self) -> &HelperFuncs;
}

/// Compile-time constant facts tracked while emitting a block.
pub trait RegConstAccess {
    /// Set register constant (for optimization)
    fn set_reg_const(&mut self, reg: u16, val: i64);

    /// Get register constant
    fn get_reg_const(&self, reg: u16) -> Option<i64>;

    /// Clear compile-time constant state for a slot after non-constant writes.
    fn clear_reg_const(&mut self, reg: u16);

    /// Clear all compile-time constant state at control-flow and helper-call
    /// boundaries where a single linear fact map is no longer sound.
    fn clear_reg_consts(&mut self);
}

/// Slow-path frame publication and JitResult return semantics.
pub trait FrameBoundary {
    /// Panic return value (FunctionCompiler=1, LoopCompiler=LOOP_RESULT_PANIC)
    fn panic_return_value(&self) -> i32;

    /// Spill all SSA variables to memory.
    /// Called before returning non-Ok JitResult so VM can see/restore state.
    fn spill_all_vars(&mut self);
}

/// Select lowering state that bridges callback-written memory back into SSA.
pub trait SelectSync<'a>: SlotAccess<'a> {
    /// Begin tracking compile-time SelectSend/SelectRecv metadata for a SelectExec.
    fn begin_select_tracking(&mut self) {}

    /// Record a SelectSend case in source order.
    fn record_select_send_case(&mut self) {}

    /// Record a SelectRecv case in source order.
    fn record_select_recv_case(&mut self, _dst_reg: u16, _elem_slots: u8, _has_ok: bool) {}

    /// Synchronize only the slots that SelectExec may have written.
    fn sync_select_exec_state(&mut self, _result_reg: u16) -> Result<(), JitError> {
        self.reload_all_vars_from_memory();
        Ok(())
    }
}

/// Basic-block-local control-flow facts.
pub trait FlowFacts {
    /// Check if a slot has been verified non-nil in the current basic block.
    fn is_checked_non_nil(&self, slot: u16) -> bool;

    /// Mark a slot as verified non-nil (after nil check passed).
    fn mark_checked_non_nil(&mut self, slot: u16);
}

/// Call boundary values used by direct JIT and prepared-call lowering.
pub trait CallBoundary<'a>: IrBuilder<'a> {
    /// Caller bp value to record for a call boundary.
    fn call_caller_bp(&mut self) -> Value;

    /// Fiber sp value to restore if a call returns through the native fast path.
    fn call_old_fiber_sp(&mut self) -> Value;
}

/// Stack base refresh after callbacks or calls that may reallocate fiber.stack.
pub trait StackRefresh {
    /// Refresh the cached fiber.stack base pointer after a call that may have triggered
    /// fiber.stack reallocation (via jit_push_frame inside prepare_closure_call, etc.).
    /// Implementations use def_var on their args_ptr/locals_ptr Variable so Cranelift
    /// inserts phi nodes correctly at join points.
    fn refresh_stack_base_after_reallocation(&mut self) {}
}

/// Full emitter capability set. Prefer narrower trait bounds in translation
/// helpers; this composite remains for top-level dispatch and call paths that
/// genuinely cross most lowering boundaries.
pub trait IrEmitter<'a>:
    IrBuilder<'a>
    + SlotAccess<'a>
    + RuntimeContext<'a>
    + MetadataAccess
    + HelperAccess
    + RegConstAccess
    + FrameBoundary
    + SelectSync<'a>
    + FlowFacts
    + CallBoundary<'a>
    + StackRefresh
{
}

impl<'a, T> IrEmitter<'a> for T where
    T: IrBuilder<'a>
        + SlotAccess<'a>
        + RuntimeContext<'a>
        + MetadataAccess
        + HelperAccess
        + RegConstAccess
        + FrameBoundary
        + SelectSync<'a>
        + FlowFacts
        + CallBoundary<'a>
        + StackRefresh
{
}

/// Capability set for helper calls that may publish the frame or invalidate
/// compile-time facts.
pub trait HelperCallEmitter<'a>: IrBuilder<'a> + RegConstAccess + FrameBoundary {}

impl<'a, T> HelperCallEmitter<'a> for T where T: IrBuilder<'a> + RegConstAccess + FrameBoundary {}

/// Capability set for lowering runtime traps and user panic returns.
pub trait TrapEmitter<'a>:
    HelperCallEmitter<'a> + RuntimeContext<'a> + MetadataAccess + HelperAccess
{
}

impl<'a, T> TrapEmitter<'a> for T where
    T: HelperCallEmitter<'a> + RuntimeContext<'a> + MetadataAccess + HelperAccess
{
}

/// Scalar and conversion lowering.
pub trait ScalarEmitter<'a>: TrapEmitter<'a> + SlotAccess<'a> + RegConstAccess {}

impl<'a, T> ScalarEmitter<'a> for T where T: TrapEmitter<'a> + SlotAccess<'a> + RegConstAccess {}

/// Global, pointer, and stack-slot memory lowering.
pub trait MemoryEmitter<'a>:
    TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + FlowFacts
{
}

impl<'a, T> MemoryEmitter<'a> for T where
    T: TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + FlowFacts
{
}

/// Collection lowering needs metadata layouts plus runtime helpers.
pub trait CollectionEmitter<'a>:
    TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + MetadataAccess
{
}

impl<'a, T> CollectionEmitter<'a> for T where
    T: TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + MetadataAccess
{
}

/// Runtime operations that can schedule, call VM callbacks, or update locals
/// through callback-written memory.
pub trait RuntimeOpsEmitter<'a>:
    TrapEmitter<'a> + SlotAccess<'a> + SelectSync<'a> + StackRefresh
{
}

impl<'a, T> RuntimeOpsEmitter<'a> for T where
    T: TrapEmitter<'a> + SlotAccess<'a> + SelectSync<'a> + StackRefresh
{
}
