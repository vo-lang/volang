//! IrEmitter trait - shared IR generation interface.

use cranelift_codegen::ir::{types, FuncRef, Inst, InstBuilder, Value};
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

pub fn emit_funcref_call<'a>(
    emitter: &mut impl IrEmitter<'a>,
    func_ref: FuncRef,
    args: &[Value],
) -> Inst {
    // TODO(jit): This unconditional spill is a conservative correctness barrier for helper calls.
    // Today some imported/runtime helpers may trigger GC, suspend into the VM, return non-OK,
    // reallocate fiber.stack, or otherwise observe the caller frame through fiber.stack materialization.
    // In those cases the caller's SSA-only locals must already be synchronized before the call.
    // The downside is that pure helpers also pay a full-frame spill cost, which shows up in
    // helper-heavy benchmarks such as dynamic calls, task queues, select, and runtime-intensive code.
    // The intended refactor is to make helper-call lowering effect-aware: classify each helper as
    // may_gc / may_suspend / may_return_non_ok / may_realloc_stack / may_observe_frame, spill only
    // when one of those effects is present, and eventually narrow the spill set to live GC-visible
    // slots instead of materializing the entire frame on every helper call.
    emitter.spill_all_vars();
    emit_funcref_call_raw(emitter, func_ref, args)
}

pub fn emit_funcref_call_raw<'a>(
    emitter: &mut impl IrEmitter<'a>,
    func_ref: FuncRef,
    args: &[Value],
) -> Inst {
    if cfg!(target_arch = "aarch64") {
        let sig = emitter.builder().func.dfg.ext_funcs[func_ref].signature;
        let func_addr = emitter.builder().ins().func_addr(types::I64, func_ref);
        emitter.builder().ins().call_indirect(sig, func_addr, args)
    } else {
        emitter.builder().ins().call(func_ref, args)
    }
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

    fn sync_slots_to_memory(&mut self, start_slot: u16, slot_count: u16) {
        let _ = start_slot;
        let _ = slot_count;
        self.spill_all_vars();
    }

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

    /// Begin tracking compile-time SelectSend/SelectRecv metadata for a SelectExec.
    fn begin_select_tracking(&mut self) {}

    /// Record a SelectSend case in source order.
    fn record_select_send_case(&mut self) {}

    /// Record a SelectRecv case in source order.
    fn record_select_recv_case(&mut self, _dst_reg: u16, _elem_slots: u8, _has_ok: bool) {}

    /// Synchronize only the slots that SelectExec may have written.
    fn sync_select_exec_state(&mut self, _result_reg: u16) {
        self.reload_all_vars_from_memory();
    }

    fn sync_written_slots(&mut self, _start_slot: u16, _slot_count: u16) {
        self.reload_all_vars_from_memory();
    }

    /// Check if a slot has been verified non-nil in the current basic block.
    fn is_checked_non_nil(&self, slot: u16) -> bool;

    /// Mark a slot as verified non-nil (after nil check passed).
    fn mark_checked_non_nil(&mut self, slot: u16);

    /// Prologue-saved ctx.jit_bp (i32). Reused by call sites to avoid redundant loads.
    fn prologue_caller_bp(&self) -> Option<Value> {
        None
    }

    /// Prologue-saved ctx.fiber_sp (i32). Reused by call sites to avoid redundant loads.
    fn prologue_fiber_sp(&self) -> Option<Value> {
        None
    }

    /// Refresh the cached fiber.stack base pointer after a call that may have triggered
    /// fiber.stack reallocation (via jit_push_frame inside prepare_closure_call, etc.).
    /// Implementations use def_var on their args_ptr/locals_ptr Variable so Cranelift
    /// inserts phi nodes correctly at join points.
    fn refresh_stack_base_after_reallocation(&mut self) {}
}

/// Scan instructions to find the minimum base register accessed via memory pointers.
/// Slots below this value are pure SSA — never accessed through memory by callbacks
/// or dynamic indexing, so store_local can skip memory writes for them.
pub fn compute_memory_only_start(code: &[Instruction]) -> u16 {
    let mut min_base = u16::MAX;
    for inst in code {
        match inst.opcode() {
            // Dynamic indexed memory access (SlotGet reads, SlotSet writes)
            Opcode::SlotSet | Opcode::SlotSetN => {
                min_base = min_base.min(inst.a);
            }
            Opcode::SlotGet | Opcode::SlotGetN => {
                min_base = min_base.min(inst.b);
            }
            // Callbacks that read from var_addr pointers
            Opcode::QueueSend => {
                min_base = min_base.min(inst.b);
            }
            Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
                min_base = min_base.min(inst.b);
            }
            Opcode::GoIsland => {
                min_base = min_base.min(inst.c);
            }
            Opcode::SliceAppend => {
                let elem_slot = inst.c + if inst.flags == 0 { 2 } else { 1 };
                min_base = min_base.min(elem_slot);
            }
            // Select callbacks read/write fiber.stack directly via register numbers —
            // all slots must be memory-synced
            Opcode::SelectSend | Opcode::SelectRecv | Opcode::SelectExec => {
                return 0;
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
    if is_float_slot(slot_types, slot) {
        types::F64
    } else {
        types::I64
    }
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
