//! IrEmitter trait - shared IR generation interface.

use std::collections::{HashMap, VecDeque};

use cranelift_codegen::ir::{types, FuncRef, Inst, InstBuilder, Value};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::{
    Constant, ExternDef, FunctionDef, JitInstructionMetadata, Module as VoModule,
};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use crate::{
    effects::{self, MemorySyncEffect},
    JitError,
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HelperCallEffect {
    /// Helper neither observes nor mutates the caller frame and cannot reallocate fiber.stack.
    FrameIndependent,
    /// Helper may trigger GC/scheduler/VM paths or observe locals through fiber.stack.
    MayObserveFrame,
}

impl HelperCallEffect {
    fn needs_frame_sync(self) -> bool {
        matches!(self, HelperCallEffect::MayObserveFrame)
    }

    fn invalidates_reg_consts(self) -> bool {
        matches!(self, HelperCallEffect::MayObserveFrame)
    }
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
    emit_funcref_call_with_effect(emitter, func_ref, args, HelperCallEffect::MayObserveFrame)
}

pub fn emit_funcref_call_with_effect<'a>(
    emitter: &mut impl IrEmitter<'a>,
    func_ref: FuncRef,
    args: &[Value],
    effect: HelperCallEffect,
) -> Inst {
    if effect.needs_frame_sync() {
        emitter.spill_all_vars();
    }
    let call = emit_funcref_call_raw(emitter, func_ref, args);
    if effect.invalidates_reg_consts() {
        emitter.clear_reg_consts();
    }
    call
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

    /// Clear compile-time constant state for a slot after non-constant writes.
    fn clear_reg_const(&mut self, reg: u16);

    /// Clear all compile-time constant state at control-flow and helper-call
    /// boundaries where a single linear fact map is no longer sound.
    fn clear_reg_consts(&mut self);

    /// Panic return value (FunctionCompiler=1, LoopCompiler=LOOP_RESULT_PANIC)
    fn panic_return_value(&self) -> i32;

    /// Get memory address of a variable slot.
    /// Used by SlotGet/SlotSet for stack array access.
    fn var_addr(&mut self, slot: u16) -> Value;

    /// Spill all SSA variables to memory.
    /// Called before returning non-Ok JitResult so VM can see/restore state.
    fn spill_all_vars(&mut self);

    fn sync_slots_to_memory(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError> {
        let _ = start_slot;
        let _ = slot_count;
        self.spill_all_vars();
        Ok(())
    }

    /// Get the number of local variable slots.
    fn local_slot_count(&self) -> usize;

    /// Get the function ID being compiled.
    fn func_id(&self) -> u32;

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
    fn sync_select_exec_state(&mut self, _result_reg: u16) -> Result<(), JitError> {
        self.reload_all_vars_from_memory();
        Ok(())
    }

    fn sync_written_slots(&mut self, _start_slot: u16, _slot_count: u16) -> Result<(), JitError> {
        self.reload_all_vars_from_memory();
        Ok(())
    }

    /// Check if a slot has been verified non-nil in the current basic block.
    fn is_checked_non_nil(&self, slot: u16) -> bool;

    /// Mark a slot as verified non-nil (after nil check passed).
    fn mark_checked_non_nil(&mut self, slot: u16);

    /// Caller bp value to record for a call boundary.
    fn call_caller_bp(&mut self) -> Value;

    /// Fiber sp value to restore if a call returns through the native fast path.
    fn call_old_fiber_sp(&mut self) -> Value;

    /// Refresh the cached fiber.stack base pointer after a call that may have triggered
    /// fiber.stack reallocation (via jit_push_frame inside prepare_closure_call, etc.).
    /// Implementations use def_var on their args_ptr/locals_ptr Variable so Cranelift
    /// inserts phi nodes correctly at join points.
    fn refresh_stack_base_after_reallocation(&mut self) {}
}

/// Scan instructions to find the minimum base register accessed via memory pointers.
/// Slots below this value are pure SSA — never accessed through memory by callbacks
/// or dynamic indexing, so store_local can skip memory writes for them.
#[allow(dead_code)]
pub fn try_compute_memory_only_start(code: &[Instruction]) -> Result<u16, effects::SlotRangeError> {
    let mut min_base = u16::MAX;
    for inst in code {
        match effects::try_memory_sync_effect(inst)? {
            MemorySyncEffect::None => {}
            MemorySyncEffect::From(base) => {
                min_base = min_base.min(base);
            }
            MemorySyncEffect::All => return Ok(0),
        }
    }
    Ok(min_base)
}

pub type RegConstFacts = Vec<HashMap<u16, i64>>;

#[cfg(test)]
fn compute_reg_const_facts_with_metadata(
    code: &[Instruction],
    jit_metadata: &[JitInstructionMetadata],
    constants: &[Constant],
    externs: &[ExternDef],
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> RegConstFacts {
    compute_reg_const_facts_with_context(
        code,
        jit_metadata,
        constants,
        &[],
        externs,
        begin_pc,
        end_pc_exclusive,
    )
}

pub fn compute_reg_const_facts_with_context(
    code: &[Instruction],
    jit_metadata: &[JitInstructionMetadata],
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> RegConstFacts {
    let mut in_facts = vec![HashMap::new(); code.len()];
    if code.is_empty() || begin_pc >= end_pc_exclusive || begin_pc >= code.len() {
        return in_facts;
    }

    let end_pc_exclusive = end_pc_exclusive.min(code.len());
    let mut out_facts = vec![HashMap::new(); code.len()];
    let mut reachable = vec![false; code.len()];
    let mut processed = vec![false; code.len()];
    let mut worklist = VecDeque::new();

    reachable[begin_pc] = true;
    worklist.push_back(begin_pc);

    while let Some(pc) = worklist.pop_front() {
        if pc < begin_pc || pc >= end_pc_exclusive {
            continue;
        }

        let mut out = in_facts[pc].clone();
        transfer_reg_const_facts(
            &code[pc],
            jit_metadata.get(pc),
            constants,
            functions,
            externs,
            &mut out,
        );
        if processed[pc] && out == out_facts[pc] {
            continue;
        }
        processed[pc] = true;
        out_facts[pc] = out.clone();

        for succ in reg_const_successors(pc, &code[pc], begin_pc, end_pc_exclusive) {
            if !reachable[succ] {
                reachable[succ] = true;
                in_facts[succ] = out.clone();
                worklist.push_back(succ);
                continue;
            }

            let merged = intersect_reg_const_facts(&in_facts[succ], &out);
            if merged != in_facts[succ] {
                in_facts[succ] = merged;
                worklist.push_back(succ);
            }
        }
    }

    in_facts
}

fn reg_const_successors(
    pc: usize,
    inst: &Instruction,
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> Vec<usize> {
    let mut succs = Vec::with_capacity(2);
    let mut push = |target: Option<usize>| {
        if let Some(target) = target {
            if target >= begin_pc && target < end_pc_exclusive {
                succs.push(target);
            }
        }
    };
    let next = pc.checked_add(1);

    match inst.opcode() {
        Opcode::Jump => {
            push(offset_target(pc, inst.imm32()));
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            push(offset_target(pc, inst.imm32()));
            push(next);
        }
        Opcode::ForLoop => {
            push(Some(inst.forloop_target(pc)));
            push(next);
        }
        Opcode::Return | Opcode::Panic => {}
        _ => push(next),
    }

    succs
}

fn offset_target(pc: usize, offset: i32) -> Option<usize> {
    let target = pc as i64 + offset as i64;
    (target >= 0).then_some(target as usize)
}

fn intersect_reg_const_facts(a: &HashMap<u16, i64>, b: &HashMap<u16, i64>) -> HashMap<u16, i64> {
    let mut merged = HashMap::new();
    for (&reg, &value) in a {
        if b.get(&reg) == Some(&value) {
            merged.insert(reg, value);
        }
    }
    merged
}

fn const_to_i64(constant: &Constant) -> Option<i64> {
    match constant {
        Constant::Nil => Some(0),
        Constant::Bool(v) => Some(*v as i64),
        Constant::Int(v) => Some(*v),
        Constant::Float(v) => Some(v.to_bits() as i64),
        Constant::String(_) => None,
    }
}

fn kill_slot(facts: &mut HashMap<u16, i64>, slot: u16) {
    facts.remove(&slot);
}

fn kill_slots(facts: &mut HashMap<u16, i64>, start: u16, count: u16) {
    for i in 0..count {
        let Some(slot) = start.checked_add(i) else {
            break;
        };
        facts.remove(&slot);
    }
}

fn kill_slots_at_or_after(facts: &mut HashMap<u16, i64>, start: u16) {
    facts.retain(|slot, _| *slot < start);
}

#[derive(Debug, PartialEq, Eq)]
enum RegConstEffect {
    Preserve,
    Clear,
    KillSlots {
        start: u16,
        count: u16,
    },
    KillSlotsAtOrAfter {
        start: u16,
    },
    SetSlot {
        slot: u16,
        value: Option<i64>,
    },
    SetSlots {
        start: u16,
        values: Vec<Option<i64>>,
    },
}

impl RegConstEffect {
    fn apply(self, facts: &mut HashMap<u16, i64>) {
        match self {
            RegConstEffect::Preserve => {}
            RegConstEffect::Clear => facts.clear(),
            RegConstEffect::KillSlots { start, count } => kill_slots(facts, start, count),
            RegConstEffect::KillSlotsAtOrAfter { start } => {
                kill_slots_at_or_after(facts, start);
            }
            RegConstEffect::SetSlot { slot, value } => set_slot_const(facts, slot, value),
            RegConstEffect::SetSlots { start, values } => {
                kill_slots(facts, start, values.len() as u16);
                for (i, value) in values.into_iter().enumerate() {
                    if let Some(value) = value {
                        let Some(slot) = start.checked_add(i as u16) else {
                            break;
                        };
                        facts.insert(slot, value);
                    }
                }
            }
        }
    }
}

fn set_slot_const(facts: &mut HashMap<u16, i64>, slot: u16, value: Option<i64>) {
    kill_slot(facts, slot);
    if let Some(value) = value {
        facts.insert(slot, value);
    }
}

fn binary_const(
    facts: &HashMap<u16, i64>,
    inst: &Instruction,
    op: impl FnOnce(i64, i64) -> Option<i64>,
) -> Option<i64> {
    let lhs = facts.get(&inst.b).copied()?;
    let rhs = facts.get(&inst.c).copied()?;
    op(lhs, rhs)
}

fn unary_const(
    facts: &HashMap<u16, i64>,
    inst: &Instruction,
    op: impl FnOnce(i64) -> Option<i64>,
) -> Option<i64> {
    let value = facts.get(&inst.b).copied()?;
    op(value)
}

fn transfer_reg_const_facts(
    inst: &Instruction,
    jit_metadata: Option<&JitInstructionMetadata>,
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    facts: &mut HashMap<u16, i64>,
) {
    reg_const_effect(inst, jit_metadata, constants, functions, externs, facts).apply(facts);
}

fn reg_const_effect(
    inst: &Instruction,
    jit_metadata: Option<&JitInstructionMetadata>,
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    facts: &HashMap<u16, i64>,
) -> RegConstEffect {
    if let Some(value) = single_slot_const_result(inst, constants, facts) {
        return RegConstEffect::SetSlot {
            slot: inst.a,
            value,
        };
    }

    let metadata_facts = crate::metadata::MetadataFacts::from_instruction(jit_metadata);

    match inst.opcode() {
        Opcode::CopyN => {
            let count = inst.copy_n_count();
            let values = (0..count)
                .map(|i| {
                    inst.b
                        .checked_add(i)
                        .and_then(|slot| facts.get(&slot).copied())
                })
                .collect();
            RegConstEffect::SetSlots {
                start: inst.a,
                values,
            }
        }
        Opcode::Call => {
            let Some(callee) = functions.get(inst.static_call_func_id() as usize) else {
                return RegConstEffect::Clear;
            };
            let (arg_slots, ret_slots) = (callee.param_slots, callee.ret_slots);
            let Some(ret_start) = inst.b.checked_add(arg_slots) else {
                return RegConstEffect::Clear;
            };
            RegConstEffect::KillSlots {
                start: ret_start,
                count: ret_slots,
            }
        }
        Opcode::CallClosure | Opcode::CallIface => {
            let Some(ret_start) = inst.b.checked_add(inst.packed_arg_slots()) else {
                return RegConstEffect::Clear;
            };
            let ret_slots = inst.packed_ret_slots();
            RegConstEffect::KillSlots {
                start: ret_start,
                count: ret_slots,
            }
        }
        Opcode::CallExtern => {
            if let Some(extern_def) = externs.get(inst.b as usize) {
                RegConstEffect::KillSlots {
                    start: inst.a,
                    count: extern_def.ret_slots,
                }
            } else {
                RegConstEffect::Clear
            }
        }
        Opcode::PtrGetN | Opcode::SlotGetN | Opcode::GlobalGetN => RegConstEffect::KillSlots {
            start: inst.a,
            count: inst.flags as u16,
        },
        Opcode::SliceGet | Opcode::ArrayGet => {
            if let Some(slots) = crate::metadata::indexed_get_result_slots(inst, metadata_facts) {
                RegConstEffect::KillSlots {
                    start: inst.a,
                    count: slots,
                }
            } else {
                RegConstEffect::Clear
            }
        }
        Opcode::QueueRecv => RegConstEffect::KillSlots {
            start: inst.a,
            count: effects::recv_result_slots(inst.flags, false),
        },
        Opcode::SelectRecv => RegConstEffect::KillSlots {
            start: inst.a,
            count: effects::recv_result_slots(inst.flags, true),
        },
        Opcode::IfaceAssign => RegConstEffect::KillSlots {
            start: inst.a,
            count: 2,
        },
        Opcode::IfaceAssert => {
            let target_slots = (inst.flags >> 3) as u16;
            let has_ok = ((inst.flags >> 2) & 1) != 0;
            let assert_kind = inst.flags & 0x3;
            let dst_slots = if assert_kind == 1 {
                2
            } else {
                target_slots.max(1)
            };
            let Some(count) = dst_slots.checked_add(u16::from(has_ok)) else {
                return RegConstEffect::Clear;
            };
            RegConstEffect::KillSlots {
                start: inst.a,
                count,
            }
        }
        Opcode::MapGet => {
            if let Some(out_slots) = crate::metadata::map_get_layout(inst, metadata_facts)
                .and_then(|layout| layout.output_slots())
            {
                RegConstEffect::KillSlots {
                    start: inst.a,
                    count: out_slots,
                }
            } else {
                RegConstEffect::Clear
            }
        }
        Opcode::MapIterInit => RegConstEffect::KillSlots {
            start: inst.a,
            count: effects::MAP_ITER_SLOTS,
        },
        Opcode::StrDecodeRune | Opcode::Recover => RegConstEffect::KillSlots {
            start: inst.a,
            count: 2,
        },
        Opcode::SlotSet | Opcode::SlotSetN => RegConstEffect::KillSlotsAtOrAfter { start: inst.a },
        op if effects::single_slot_unknown_result_opcode(op) => RegConstEffect::KillSlots {
            start: inst.a,
            count: 1,
        },
        op if effects::preserves_reg_const_facts(op) => RegConstEffect::Preserve,
        Opcode::MapIterNext | Opcode::SelectExec => RegConstEffect::Clear,
        _ => RegConstEffect::Clear,
    }
}

fn single_slot_const_result(
    inst: &Instruction,
    constants: &[Constant],
    facts: &HashMap<u16, i64>,
) -> Option<Option<i64>> {
    match inst.opcode() {
        Opcode::LoadInt => Some(Some(inst.imm32() as i64)),
        Opcode::LoadConst => Some(constants.get(inst.b as usize).and_then(const_to_i64)),
        Opcode::Copy => Some(facts.get(&inst.b).copied()),
        Opcode::AddI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(lhs.wrapping_add(rhs))
        })),
        Opcode::SubI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(lhs.wrapping_sub(rhs))
        })),
        Opcode::MulI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(lhs.wrapping_mul(rhs))
        })),
        Opcode::DivI => Some(binary_const(facts, inst, |lhs, rhs| {
            (rhs != 0).then(|| lhs.wrapping_div(rhs))
        })),
        Opcode::DivU => Some(binary_const(facts, inst, |lhs, rhs| {
            let rhs = rhs as u64;
            (rhs != 0).then(|| (lhs as u64).wrapping_div(rhs) as i64)
        })),
        Opcode::ModI => Some(binary_const(facts, inst, |lhs, rhs| {
            (rhs != 0).then(|| lhs.wrapping_rem(rhs))
        })),
        Opcode::ModU => Some(binary_const(facts, inst, |lhs, rhs| {
            let rhs = rhs as u64;
            (rhs != 0).then(|| (lhs as u64).wrapping_rem(rhs) as i64)
        })),
        Opcode::NegI => Some(unary_const(facts, inst, |value| Some(value.wrapping_neg()))),
        Opcode::EqI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs == rhs) as i64)
        })),
        Opcode::NeI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs != rhs) as i64)
        })),
        Opcode::LtI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs < rhs) as i64)
        })),
        Opcode::LeI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs <= rhs) as i64)
        })),
        Opcode::GtI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs > rhs) as i64)
        })),
        Opcode::GeI => Some(binary_const(facts, inst, |lhs, rhs| {
            Some((lhs >= rhs) as i64)
        })),
        Opcode::LtU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) < (rhs as u64)) as i64)
        })),
        Opcode::LeU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) <= (rhs as u64)) as i64)
        })),
        Opcode::GtU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) > (rhs as u64)) as i64)
        })),
        Opcode::GeU => Some(binary_const(facts, inst, |lhs, rhs| {
            Some(((lhs as u64) >= (rhs as u64)) as i64)
        })),
        Opcode::And => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs & rhs))),
        Opcode::Or => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs | rhs))),
        Opcode::Xor => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs ^ rhs))),
        Opcode::AndNot => Some(binary_const(facts, inst, |lhs, rhs| Some(lhs & !rhs))),
        Opcode::Not => Some(unary_const(facts, inst, |value| Some(!value))),
        Opcode::BoolNot => Some(unary_const(facts, inst, |value| Some((value == 0) as i64))),
        Opcode::Shl => Some(binary_const(facts, inst, |lhs, rhs| {
            if rhs < 0 {
                None
            } else if rhs >= 64 {
                Some(0)
            } else {
                Some((lhs as u64).wrapping_shl(rhs as u32) as i64)
            }
        })),
        Opcode::ShrS => Some(binary_const(facts, inst, |lhs, rhs| {
            if rhs < 0 {
                None
            } else if rhs >= 64 {
                Some(if lhs < 0 { -1 } else { 0 })
            } else {
                Some(lhs >> rhs as u32)
            }
        })),
        Opcode::ShrU => Some(binary_const(facts, inst, |lhs, rhs| {
            if rhs < 0 {
                None
            } else if rhs >= 64 {
                Some(0)
            } else {
                Some((lhs as u64).wrapping_shr(rhs as u32) as i64)
            }
        })),
        _ => None,
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn jump_if_not(cond: u16, offset: i32) -> Instruction {
        Instruction {
            op: Opcode::JumpIfNot as u8,
            flags: 0,
            a: cond,
            b: (offset as u32 & 0xFFFF) as u16,
            c: ((offset as u32 >> 16) & 0xFFFF) as u16,
        }
    }

    #[test]
    fn reg_const_facts_drop_disagreeing_branch_values() {
        let code = vec![
            Instruction::new(Opcode::LoadInt, 2, 1, 0),
            jump_if_not(0, 2),
            Instruction::new(Opcode::LoadInt, 2, 2, 0),
            Instruction::new(Opcode::Shl, 3, 4, 2),
        ];
        let facts = compute_reg_const_facts_with_metadata(&code, &[], &[], &[], 0, code.len());

        assert!(
            !facts[3].contains_key(&2),
            "branch merge must not keep a constant when predecessors disagree"
        );
    }

    #[test]
    fn reg_const_facts_preserve_agreeing_branch_values() {
        let constants = vec![Constant::Int(42)];
        let code = vec![
            Instruction::new(Opcode::LoadConst, 5, 0, 0),
            jump_if_not(0, 2),
            Instruction::new(Opcode::Copy, 6, 5, 0),
            Instruction::new(Opcode::MapGet, 7, 8, 5),
        ];
        let facts =
            compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

        assert_eq!(
            facts[3].get(&5),
            Some(&42),
            "constants that agree on all predecessors should survive merge"
        );
    }

    #[test]
    fn reg_const_facts_preserve_metadata_across_helper_and_map_write() {
        let constants = vec![Constant::String("key".to_string()), Constant::Int(258)];
        let code = vec![
            Instruction::new(Opcode::LoadConst, 1, 1, 0),
            Instruction::new(Opcode::StrNew, 2, 0, 0),
            Instruction::new(Opcode::MapSet, 0, 1, 3),
            Instruction::new(Opcode::MapDelete, 0, 1, 2),
        ];
        let facts =
            compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

        assert_eq!(
            facts[2].get(&1),
            Some(&258),
            "helper-backed string creation must only kill its destination"
        );
        assert_eq!(
            facts[3].get(&1),
            Some(&258),
            "map writes must not discard unrelated constants they only read"
        );
    }

    #[test]
    fn reg_const_facts_use_instruction_metadata_when_meta_register_is_not_constant() {
        let constants = vec![Constant::Int(42), Constant::Int(99)];
        let code = vec![
            Instruction::new(Opcode::LoadConst, 5, 0, 0),
            Instruction::new(Opcode::LoadConst, 7, 1, 0),
            Instruction::new(Opcode::MapGet, 7, 1, 20),
            Instruction::new(Opcode::MapLen, 30, 1, 0),
        ];
        let mut metadata = vec![JitInstructionMetadata::None; code.len()];
        metadata[2] = JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value, SlotType::GcRef],
            val_layout: vec![SlotType::Interface0, SlotType::Interface1],
            has_ok: true,
        };
        let facts =
            compute_reg_const_facts_with_metadata(&code, &metadata, &constants, &[], 0, code.len());

        assert_eq!(
            facts[3].get(&5),
            Some(&42),
            "unrelated constants should survive a MapGet described by instruction metadata"
        );
        assert!(
            !facts[3].contains_key(&7) && !facts[3].contains_key(&8) && !facts[3].contains_key(&9),
            "MapGet output slots should be killed from instruction metadata"
        );
    }

    #[test]
    fn reg_const_facts_call_extern_only_kills_return_slots() {
        let constants = vec![Constant::Int(258), Constant::Int(1234)];
        let externs = vec![ExternDef {
            name: "native".to_string(),
            param_slots: 1,
            ret_slots: 1,
            is_blocking: false,
            param_kinds: Vec::new(),
        }];
        let code = vec![
            Instruction::new(Opcode::LoadConst, 5, 0, 0),
            Instruction::new(Opcode::LoadConst, 13, 1, 0),
            Instruction::with_flags(Opcode::CallExtern, 1, 13, 0, 20),
            Instruction::new(Opcode::MapSet, 0, 5, 8),
        ];
        let facts =
            compute_reg_const_facts_with_metadata(&code, &[], &constants, &externs, 0, code.len());

        assert_eq!(
            facts[3].get(&5),
            Some(&258),
            "extern calls should preserve metadata outside their declared return slots"
        );
        assert!(
            !facts[3].contains_key(&13),
            "extern return slots must lose stale constants"
        );
    }

    #[test]
    fn reg_const_facts_fold_integer_arithmetic() {
        let constants = vec![Constant::Int(17), Constant::Int(4111)];
        let code = vec![
            Instruction::new(Opcode::LoadConst, 5, 0, 0),
            Instruction::new(Opcode::LoadInt, 6, 32, 0),
            Instruction::new(Opcode::Shl, 5, 5, 6),
            Instruction::new(Opcode::LoadConst, 7, 1, 0),
            Instruction::new(Opcode::Or, 5, 5, 7),
            Instruction::new(Opcode::MapNew, 8, 5, 0),
        ];
        let facts =
            compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

        assert_eq!(
            facts[5].get(&5),
            Some(&((17i64 << 32) | 4111)),
            "integer facts built from pure ops should stay available"
        );
    }

    #[test]
    fn reg_const_facts_map_iter_init_kills_whole_iterator() {
        let constants = vec![Constant::Int(99)];
        let iter_start = 10;
        let iter_last = iter_start + effects::MAP_ITER_SLOTS - 1;
        let code = vec![
            Instruction::new(Opcode::LoadConst, iter_last, 0, 0),
            Instruction::new(Opcode::MapIterInit, iter_start, 1, 0),
            Instruction::new(Opcode::MapLen, 20, 1, 0),
        ];
        let facts =
            compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

        assert!(
            !facts[2].contains_key(&iter_last),
            "MapIterInit writes all iterator slots, so constants in the tail must be killed"
        );
    }
}
