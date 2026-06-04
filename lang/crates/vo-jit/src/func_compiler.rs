#![allow(clippy::result_large_err, clippy::too_many_arguments)]
//! Function compiler: bytecode -> Cranelift IR.

use std::collections::{HashMap, HashSet};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, FuncRef, Function, InstBuilder, MemFlags, Value};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use crate::translate::translate_inst;
use crate::translator::{HelperFuncs, SelectSyncCase, SlotAccess, TranslateResult};
use crate::JitError;
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::JitContext;

pub struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    func_id: u32,
    func_def: &'a FunctionDef,
    vo_module: &'a VoModule,
    vars: Vec<Variable>,
    blocks: HashMap<usize, Block>,
    entry_block: Block,
    current_pc: usize,
    helpers: HelperFuncs,
    reg_consts: HashMap<u16, i64>,
    reg_const_facts: Vec<HashMap<u16, i64>>,
    /// FuncRef table for other already-compiled callees.
    /// Index by func_id. None means generated code keeps the VM call materialization path.
    callee_func_refs: &'a [Option<FuncRef>],
    /// Saved jit_bp from function entry, used to recompute fiber.stack address after reallocation
    saved_jit_bp: Variable,
    /// Variable wrapping the args_ptr for this function (points to fiber.stack[jit_bp]).
    /// Declared as a Cranelift Variable so def_var/use_var handle phi insertion at join points,
    /// allowing refresh_stack_base_after_reallocation to redefine it after any call that may
    /// have triggered fiber.stack reallocation via jit_push_frame.
    args_ptr_var: Variable,
    args_ptr_is_stack_var: Variable,
    /// Slots that have been verified non-nil in the current basic block.
    /// Cleared on block transitions (jump targets).
    checked_non_nil: HashSet<u16>,
    /// Slots >= this value must use memory reads (may be aliased by SlotSet/SlotSetN).
    /// Slots below this value use SSA reads via use_var for better register allocation.
    memory_only_start: u16,
    /// ctx.jit_bp at function entry (i32). Reused by all call sites as caller_bp.
    saved_caller_bp: Value,
    /// ctx.fiber_sp at function entry (i32). Reused by all call sites as old_fiber_sp.
    saved_fiber_sp: Value,
    pending_select_cases: Vec<SelectSyncCase>,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        helpers: HelperFuncs,
        callee_func_refs: &'a [Option<FuncRef>],
    ) -> Self {
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        let local_slots = func_def.local_slots as u32;

        Self {
            builder,
            func_id,
            func_def,
            vo_module,
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            current_pc: 0,
            helpers,
            reg_consts: HashMap::new(),
            reg_const_facts: Vec::new(),
            callee_func_refs,
            saved_jit_bp: Variable::from_u32(local_slots + 1000),
            args_ptr_var: Variable::from_u32(local_slots + 1001),
            args_ptr_is_stack_var: Variable::from_u32(local_slots + 1002),
            checked_non_nil: HashSet::new(),
            memory_only_start: u16::MAX,
            saved_caller_bp: Value::from_u32(0),
            saved_fiber_sp: Value::from_u32(0),
            pending_select_cases: Vec::new(),
        }
    }

    pub fn compile(mut self) -> Result<(), JitError> {
        let analysis =
            crate::analysis::FunctionAnalysis::for_function(self.func_def, self.vo_module)?;
        self.memory_only_start = analysis.memory_only_start;
        self.reg_const_facts = analysis.reg_const_facts;
        self.declare_variables();
        self.scan_jump_targets()?;

        self.builder.switch_to_block(self.entry_block);
        self.emit_prologue();
        crate::compile_common::drive_compile(&mut self)?;

        self.builder.seal_all_blocks();
        self.builder.finalize();

        Ok(())
    }

    fn declare_variables(&mut self) {
        self.vars = crate::compile_common::declare_variables(&mut self.builder, self.func_def);
    }

    #[inline]
    fn is_float_slot(&self, slot: u16) -> bool {
        crate::compile_common::is_float_slot(self.func_def, slot)
    }

    fn scan_jump_targets(&mut self) -> Result<(), JitError> {
        let policy = crate::compile_common::ControlPolicy::full_function(self.func_def.code.len());
        for target in crate::compile_common::jump_targets_for_policy(&self.func_def.code, policy)? {
            self.ensure_block(target);
        }
        Ok(())
    }

    fn ensure_block(&mut self, pc: usize) {
        if !self.blocks.contains_key(&pc) {
            let block = self.builder.create_block();
            self.blocks.insert(pc, block);
        }
    }

    fn block_for_pc(&self, pc: usize, context: &'static str) -> Result<Block, JitError> {
        self.blocks.get(&pc).copied().ok_or_else(|| {
            JitError::Internal(format!(
                "missing JIT basic block for {context} target pc {pc}"
            ))
        })
    }

    fn checked_branch_target(
        &self,
        pc: usize,
        offset: i32,
        opcode: Opcode,
    ) -> Result<usize, JitError> {
        crate::compile_common::checked_branch_target(self.func_def.code.len(), pc, offset, opcode)
    }

    fn checked_forloop_target(&self, pc: usize, inst: &Instruction) -> Result<usize, JitError> {
        crate::compile_common::checked_forloop_target(self.func_def.code.len(), pc, inst)
    }

    fn clear_flow_facts(&mut self) {
        crate::compile_common::clear_flow_facts(&mut self.checked_non_nil, &mut self.reg_consts);
    }

    fn apply_reg_const_facts(&mut self, pc: usize) -> Result<(), JitError> {
        crate::compile_common::apply_reg_const_facts(
            &mut self.reg_consts,
            &self.reg_const_facts,
            pc,
        )
    }

    /// Spill all SSA variables to fiber.stack (recomputed base, handles reallocation).
    /// Called on slow path (Call/WaitIo) so VM can see the current state.
    /// Note: args_ptr may be stale if fiber.stack was reallocated during nested calls,
    /// so we recompute the destination from ctx.stack_ptr + saved_jit_bp.
    fn emit_variable_spill(&mut self) {
        let dst_ptr = self.fiber_stack_args_ptr();
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .spill_for_materialized_frame(&mut self.builder, args_ptr, dst_ptr);
    }

    fn current_memory_base_ptr(&mut self) -> Value {
        let entry_args_ptr = self.builder.use_var(self.args_ptr_var);
        let uses_stack = self.builder.use_var(self.args_ptr_is_stack_var);
        let use_stack = self.builder.ins().icmp_imm(IntCC::NotEqual, uses_stack, 0);
        let stack_args_ptr = self.fiber_stack_args_ptr();
        self.builder
            .ins()
            .select(use_stack, stack_args_ptr, entry_args_ptr)
    }

    /// Compute fiber.stack base dynamically from ctx.stack_ptr + saved_jit_bp.
    /// Needed because fiber.stack may reallocate during nested calls.
    fn fiber_stack_args_ptr(&mut self) -> Value {
        let ctx = self.builder.block_params(self.entry_block)[0];
        let stack_ptr = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            ctx,
            JitContext::OFFSET_STACK_PTR,
        );
        let jit_bp = self.builder.use_var(self.saved_jit_bp);
        // fiber_args_ptr = stack_ptr + jit_bp * 8
        let bp_offset = self.builder.ins().imul_imm(jit_bp, 8);
        self.builder.ins().iadd(stack_ptr, bp_offset)
    }

    fn sync_select_exec_state_precise(&mut self, result_reg: u16) {
        let stack_args_ptr = self.fiber_stack_args_ptr();
        let result_offset = (result_reg as i32) * 8;
        let result_val = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            stack_args_ptr,
            result_offset,
        );
        self.store_local(result_reg, result_val);

        let recv_cases: Vec<(usize, u16, usize, bool)> = self
            .pending_select_cases
            .iter()
            .enumerate()
            .filter_map(|(idx, case)| match *case {
                SelectSyncCase::Send => None,
                SelectSyncCase::Recv {
                    dst_reg,
                    elem_slots,
                    has_ok,
                } => Some((idx, dst_reg, elem_slots as usize, has_ok)),
            })
            .collect();

        if recv_cases.is_empty() {
            self.pending_select_cases.clear();
            return;
        }

        let done_block = self.builder.create_block();
        for (case_idx, dst_reg, elem_slots, has_ok) in recv_cases {
            let match_block = self.builder.create_block();
            let miss_block = self.builder.create_block();
            let case_idx_val = self.builder.ins().iconst(types::I64, case_idx as i64);
            let is_match = self
                .builder
                .ins()
                .icmp(IntCC::Equal, result_val, case_idx_val);
            self.builder
                .ins()
                .brif(is_match, match_block, &[], miss_block, &[]);

            self.builder.switch_to_block(match_block);
            self.builder.seal_block(match_block);
            let slot_count = elem_slots + if has_ok { 1 } else { 0 };
            for slot_offset in 0..slot_count {
                let slot = dst_reg + slot_offset as u16;
                if self.is_float_slot(slot) {
                    let val = crate::compile_common::load_memory_slot_f64(
                        &mut self.builder,
                        stack_args_ptr,
                        slot,
                    );
                    self.write_var_f64(slot, val);
                } else {
                    let val = crate::compile_common::load_memory_slot_i64(
                        &mut self.builder,
                        stack_args_ptr,
                        slot,
                    );
                    self.store_local(slot, val);
                }
            }
            self.builder.ins().jump(done_block, &[]);

            self.builder.switch_to_block(miss_block);
            self.builder.seal_block(miss_block);
        }

        self.builder.ins().jump(done_block, &[]);
        self.builder.switch_to_block(done_block);
        self.builder.seal_block(done_block);
        self.pending_select_cases.clear();
    }

    fn sync_written_slots_precise(
        &mut self,
        start_slot: u16,
        slot_count: u16,
    ) -> Result<(), JitError> {
        if slot_count == 0 {
            return Ok(());
        }
        let args_ptr = self.current_memory_base_ptr();
        let slots = crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .load_memory_slot_range(
            &mut self.builder,
            args_ptr,
            start_slot,
            slot_count,
            "select sync",
        )?;
        for slot in slots {
            if slot.is_float {
                self.write_var_f64(slot.slot, slot.value);
            } else {
                self.store_local(slot.slot, slot.value);
            }
        }
        Ok(())
    }

    fn emit_prologue(&mut self) {
        // entry_block has no predecessors (it's the function entry point)
        self.builder.seal_block(self.entry_block);

        let params = self.builder.block_params(self.entry_block);
        let ctx = params[0];
        let args_ptr = params[1]; // Points to fiber.stack[jit_bp]
        let _ret = params[2];

        // Wrap args_ptr in a Variable so refresh_stack_base_after_reallocation can redefine
        // it after any call that may have triggered fiber.stack reallocation.
        self.builder.declare_var(self.args_ptr_var, types::I64);
        self.builder.def_var(self.args_ptr_var, args_ptr);

        // Save jit_bp from ctx at function entry.
        // This is needed to compute fiber.stack address for spilling.
        // Also saved as caller_bp (i32) for reuse by all call sites.
        self.builder.declare_var(self.saved_jit_bp, types::I64);
        let jit_bp_i32 = self.builder.ins().load(
            types::I32,
            MemFlags::trusted(),
            ctx,
            JitContext::OFFSET_JIT_BP,
        );
        let jit_bp_i64 = self.builder.ins().uextend(types::I64, jit_bp_i32);
        self.builder.def_var(self.saved_jit_bp, jit_bp_i64);
        self.builder
            .declare_var(self.args_ptr_is_stack_var, types::I8);
        let stack_args_ptr = self.fiber_stack_args_ptr();
        let uses_stack = self
            .builder
            .ins()
            .icmp(IntCC::Equal, args_ptr, stack_args_ptr);
        let one_i8 = self.builder.ins().iconst(types::I8, 1);
        let zero_i8 = self.builder.ins().iconst(types::I8, 0);
        let uses_stack_i8 = self.builder.ins().select(uses_stack, one_i8, zero_i8);
        self.builder
            .def_var(self.args_ptr_is_stack_var, uses_stack_i8);
        self.saved_caller_bp = jit_bp_i32;

        // Save fiber_sp from ctx at function entry. Reused by all call sites.
        let fiber_sp_i32 = self.builder.ins().load(
            types::I32,
            MemFlags::trusted(),
            ctx,
            JitContext::OFFSET_FIBER_SP,
        );
        self.saved_fiber_sp = fiber_sp_i32;

        let param_slots = self.func_def.param_slots as usize;
        let num_slots = self.vars.len();

        // Load params from args_ptr into SSA vars (params already in args_ptr from caller)
        for i in 0..param_slots {
            let slot = i as u16;
            let val = if self.is_float_slot(slot) {
                crate::compile_common::load_memory_slot_f64(&mut self.builder, args_ptr, slot)
            } else {
                crate::compile_common::load_memory_slot_i64(&mut self.builder, args_ptr, slot)
            };
            self.builder.def_var(self.vars[i], val);
        }

        // Initialize non-param SSA vars to 0.
        // Memory store only needed for memory-aliased slots (>= memory_only_start).
        let zero_i64 = self.builder.ins().iconst(types::I64, 0);
        let zero_f64 = self.builder.ins().f64const(0.0);
        for i in param_slots..num_slots {
            if self.is_float_slot(i as u16) {
                self.builder.def_var(self.vars[i], zero_f64);
            } else {
                self.builder.def_var(self.vars[i], zero_i64);
            }
            if i as u16 >= self.memory_only_start {
                crate::compile_common::store_memory_slot(
                    &mut self.builder,
                    args_ptr,
                    i as u16,
                    zero_i64,
                );
            }
        }
    }

    fn translate_instruction(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        match translate_inst(self, inst)? {
            TranslateResult::Completed => return Ok(false),
            TranslateResult::Terminated => return Ok(true),
            TranslateResult::Unhandled => {}
        }

        match inst.opcode() {
            Opcode::Jump => {
                self.jump(inst)?;
                Ok(true)
            }
            Opcode::JumpIf => {
                self.jump_if(inst)?;
                Ok(false)
            }
            Opcode::JumpIfNot => {
                self.jump_if_not(inst)?;
                Ok(false)
            }
            Opcode::Return => {
                self.ret(inst)?;
                Ok(true)
            }
            Opcode::Panic => {
                self.panic(inst);
                Ok(true)
            }
            Opcode::Call => self.call(inst),
            Opcode::CallExtern => {
                crate::call_helpers::emit_call_extern(
                    self,
                    inst,
                    crate::call_helpers::CallExternConfig {
                        current_pc: self.current_pc,
                    },
                )?;
                Ok(false)
            }
            Opcode::CallClosure => {
                crate::call_helpers::emit_call_closure(self, inst)?;
                Ok(false)
            }
            Opcode::CallIface => {
                crate::call_helpers::emit_call_iface(self, inst)?;
                Ok(false)
            }
            Opcode::ForLoop => {
                self.forloop(inst)?;
                Ok(false)
            }
            other => Err(JitError::UnsupportedOpcode(other)),
        }
    }

    fn jump(&mut self, inst: &Instruction) -> Result<(), JitError> {
        let target = self.checked_branch_target(self.current_pc, inst.imm32(), inst.opcode())?;
        let block = self.block_for_pc(target, "jump")?;

        self.builder.ins().jump(block, &[]);
        Ok(())
    }

    fn jump_if(&mut self, inst: &Instruction) -> Result<(), JitError> {
        self.conditional_jump(inst, IntCC::NotEqual)
    }

    fn jump_if_not(&mut self, inst: &Instruction) -> Result<(), JitError> {
        self.conditional_jump(inst, IntCC::Equal)
    }

    /// Read variable as I64: SSA when safe, memory when slot may be aliased by SlotSet/SlotSetN.
    fn load_local(&mut self, slot: u16) -> Value {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .load_i64(&mut self.builder, args_ptr, slot)
    }

    /// Write I64 value to variable slot.
    /// SSA-only slots (< memory_only_start): only update SSA variable (memory synced on slow path by spill).
    /// Memory-aliased slots (>= memory_only_start): write both SSA and memory.
    fn store_local(&mut self, slot: u16, val: Value) {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .store_i64(&mut self.builder, args_ptr, slot, val);
        // Writes invalidate local compile-time facts for the slot.
        self.checked_non_nil.remove(&slot);
        self.reg_consts.remove(&slot);
    }

    fn conditional_jump(&mut self, inst: &Instruction, cmp_cond: IntCC) -> Result<(), JitError> {
        let cond = self.load_local(inst.a);
        let target = self.checked_branch_target(self.current_pc, inst.imm32(), inst.opcode())?;
        let target_block = self.block_for_pc(target, "conditional jump")?;
        let fall_through = self.builder.create_block();

        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(cmp_cond, cond, zero);
        self.builder
            .ins()
            .brif(cmp, target_block, &[], fall_through, &[]);

        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
        self.clear_flow_facts();
        Ok(())
    }

    fn forloop(&mut self, inst: &Instruction) -> Result<(), JitError> {
        let idx = self.load_local(inst.a);
        let limit = self.load_local(inst.b);
        let (is_decrement, is_unsigned, is_inclusive) = inst.forloop_flags();

        let (next_idx, continue_loop) = crate::translate::emit_forloop_step(
            &mut self.builder,
            idx,
            limit,
            is_decrement,
            is_unsigned,
            is_inclusive,
        );
        self.store_local(inst.a, next_idx);

        let target = self.checked_forloop_target(self.current_pc, inst)?;
        let target_block = self.block_for_pc(target, "forloop")?;
        let fall_through = self.builder.create_block();

        self.builder
            .ins()
            .brif(continue_loop, target_block, &[], fall_through, &[]);
        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
        self.clear_flow_facts();
        Ok(())
    }

    fn ret(&mut self, inst: &Instruction) -> Result<(), JitError> {
        use vo_common_core::bytecode::ReturnFlags;
        let ret_ptr = self.builder.block_params(self.entry_block)[2];
        let ctx = self.builder.block_params(self.entry_block)[0];
        let flags = ReturnFlags::from_bits(inst.flags).ok_or_else(|| {
            JitError::InvalidMetadata(crate::JitMetadataError::InvalidInstructionFlags {
                func: self.func_def.name.clone(),
                pc: self.current_pc,
                opcode: Opcode::Return,
                flags: inst.flags,
                allowed: ReturnFlags::ALLOWED_BITS,
            })
        })?;
        let heap_returns = flags.has_heap_returns();
        let is_error_return = flags.is_error_return();

        // Pure function: no defer, no error return, no heap returns.
        // VM guards metadata reads with func attributes, so we can skip all metadata stores.
        let is_pure = !self.func_def.has_defer
            && self.func_def.error_ret_slot < 0
            && self.func_def.heap_ret_gcref_count == 0;

        if !is_pure {
            // Set is_error_return for VM errdefer decision.
            let err_flag = self
                .builder
                .ins()
                .iconst(types::I8, if is_error_return { 1 } else { 0 });
            self.builder.ins().store(
                MemFlags::trusted(),
                err_flag,
                ctx,
                JitContext::OFFSET_IS_ERROR_RETURN,
            );
        }

        if heap_returns {
            if self.func_def.has_defer {
                // With defers, VM must read heap returns AFTER defers execute.
                // Record metadata for VM to read GcRefs from fiber.stack[jit_bp + ret_gcref_start..].
                let gcref_start = self.builder.ins().iconst(types::I16, inst.a as i64);
                self.builder.ins().store(
                    MemFlags::trusted(),
                    gcref_start,
                    ctx,
                    JitContext::OFFSET_RET_GCREF_START,
                );
                let one = self.builder.ins().iconst(types::I8, 1);
                self.builder.ins().store(
                    MemFlags::trusted(),
                    one,
                    ctx,
                    JitContext::OFFSET_RET_IS_HEAP,
                );

                // Spill GcRef slots to args_ptr so VM can read them from fiber.stack.
                // SSA-only slots (< memory_only_start) aren't written to memory by store_local.
                let gcref_count = inst.b as usize;
                let args_ptr = self.fiber_stack_args_ptr();
                for i in 0..gcref_count {
                    let slot = (inst.a as usize + i) as u16;
                    if slot < self.memory_only_start {
                        let val_i64 = crate::compile_common::read_ssa_slot_i64(
                            &mut self.builder,
                            &self.vars,
                            &self.func_def.slot_types,
                            slot,
                        );
                        crate::compile_common::store_memory_slot(
                            &mut self.builder,
                            args_ptr,
                            slot,
                            val_i64,
                        );
                    }
                }
            } else {
                let zero = self.builder.ins().iconst(types::I8, 0);
                self.builder.ins().store(
                    MemFlags::trusted(),
                    zero,
                    ctx,
                    JitContext::OFFSET_RET_IS_HEAP,
                );

                // Escaped named returns (no defers): dereference GcRefs and copy to ret buffer.
                let gcref_start = inst.a as usize;
                let gcref_count = inst.b as usize;

                let mut ret_offset = 0i32;
                for i in 0..gcref_count {
                    let gcref = self.load_local((gcref_start + i) as u16);
                    let slots_for_this_ref = self
                        .func_def
                        .heap_ret_slots
                        .get(i)
                        .copied()
                        .ok_or_else(|| {
                            JitError::Internal(format!(
                                "heap return slot metadata missing for JIT return gcref {i}"
                            ))
                        })? as usize;
                    for j in 0..slots_for_this_ref {
                        let val = self.builder.ins().load(
                            types::I64,
                            MemFlags::trusted(),
                            gcref,
                            (j * 8) as i32,
                        );
                        self.builder
                            .ins()
                            .store(MemFlags::trusted(), val, ret_ptr, ret_offset);
                        ret_offset += 8;
                    }
                }
            }
        } else {
            if !is_pure {
                let zero = self.builder.ins().iconst(types::I8, 0);
                self.builder.ins().store(
                    MemFlags::trusted(),
                    zero,
                    ctx,
                    JitContext::OFFSET_RET_IS_HEAP,
                );

                // Store ret_start for VM to extract slot_types for GC scanning
                let ret_start_val = self.builder.ins().iconst(types::I16, inst.a as i64);
                self.builder.ins().store(
                    MemFlags::trusted(),
                    ret_start_val,
                    ctx,
                    JitContext::OFFSET_RET_START,
                );
            }

            let ret_slots = self.func_def.ret_slots as usize;
            let ret_reg = inst.a as usize;

            for i in 0..ret_slots {
                let val = self.load_local((ret_reg + i) as u16);
                let offset = (i * 8) as i32;
                self.builder
                    .ins()
                    .store(MemFlags::trusted(), val, ret_ptr, offset);
            }
        }

        let ok = self.builder.ins().iconst(types::I32, 0);
        self.builder.ins().return_(&[ok]);
        Ok(())
    }

    fn panic(&mut self, inst: &Instruction) {
        crate::contract::emit_user_panic_return(self, inst.a);
    }

    /// Returns true if the block was terminated
    fn call(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        let target_func_id = inst.static_call_func_id();
        let arg_start = inst.b as usize;

        let target_func = self
            .vo_module
            .functions
            .get(target_func_id as usize)
            .ok_or(JitError::FunctionNotFound(target_func_id))?;
        let callee_func_ref = self
            .callee_func_refs
            .get(target_func_id as usize)
            .copied()
            .flatten();
        let call_plan = crate::call_helpers::CallPlan::new(
            target_func_id,
            arg_start,
            target_func,
            callee_func_ref,
        );

        match call_plan.route_for_full_function(self.func_id) {
            crate::call_helpers::CallRoute::KnownDirectJit
            | crate::call_helpers::CallRoute::DynamicJitTable => {
                crate::call_helpers::emit_jit_call_with_vm_materialization(
                    self,
                    call_plan.jit_materialization_config(),
                )?;
                Ok(false)
            }
            crate::call_helpers::CallRoute::VmCallMaterialization => {
                crate::call_helpers::emit_call_via_vm(
                    self,
                    call_plan.vm_config(self.current_pc + 1),
                )?;
                Ok(true)
            }
            crate::call_helpers::CallRoute::DynamicInlineCache => Err(JitError::Internal(
                "static full-function call selected dynamic inline-cache route".into(),
            )),
        }
    }
}

impl<'a> crate::compile_common::CompileDriver for FunctionCompiler<'a> {
    fn control_policy(&self) -> crate::compile_common::ControlPolicy {
        crate::compile_common::ControlPolicy::full_function(self.func_def.code.len())
    }

    fn set_current_pc(&mut self, pc: usize) {
        self.current_pc = pc;
    }

    fn enter_pc_block(&mut self, pc: usize, block_terminated: &mut bool) -> Result<(), JitError> {
        if crate::compile_common::enter_compile_pc(
            &mut self.builder,
            &self.blocks,
            pc,
            block_terminated,
        ) {
            self.clear_flow_facts();
        }
        Ok(())
    }

    fn apply_pc_facts(&mut self, pc: usize) -> Result<(), JitError> {
        self.apply_reg_const_facts(pc)
    }

    fn instruction_for_pc(&self, pc: usize) -> Result<Instruction, JitError> {
        self.func_def
            .code
            .get(pc)
            .copied()
            .ok_or_else(|| JitError::Internal(format!("function compile pc {pc} is outside code")))
    }

    fn translate_pc_instruction(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        self.translate_instruction(inst)
    }

    fn finish_fallthrough(&mut self, _block_terminated: bool) -> Result<(), JitError> {
        Ok(())
    }
}

impl<'a> crate::translator::IrBuilder<'a> for FunctionCompiler<'a> {
    fn builder(&mut self) -> &mut FunctionBuilder<'a> {
        &mut self.builder
    }
}

impl<'a> crate::translator::SlotAccess<'a> for FunctionCompiler<'a> {
    fn read_var(&mut self, slot: u16) -> Value {
        self.load_local(slot)
    }
    fn write_var(&mut self, slot: u16, val: Value) {
        self.store_local(slot, val);
    }
    fn var_addr(&mut self, slot: u16) -> Value {
        let args_ptr = self.current_memory_base_ptr();
        self.builder.ins().iadd_imm(args_ptr, (slot as i64) * 8)
    }
    fn sync_slots_to_memory(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError> {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .sync_ssa_slots_to_memory(
            &mut self.builder,
            args_ptr,
            start_slot,
            slot_count,
            "memory sync",
        )
    }
    fn local_slot_count(&self) -> usize {
        self.vars.len()
    }
    fn read_var_f64(&mut self, slot: u16) -> Value {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .load_f64(&mut self.builder, args_ptr, slot)
    }
    fn write_var_f64(&mut self, slot: u16, val: Value) {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .store_f64(&mut self.builder, args_ptr, slot, val);
        self.checked_non_nil.remove(&slot);
        self.reg_consts.remove(&slot);
    }
    fn reload_all_vars_from_memory(&mut self) {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .reload_all_from_memory(&mut self.builder, args_ptr);
        self.clear_flow_facts();
    }
    fn sync_written_slots(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError> {
        self.sync_written_slots_precise(start_slot, slot_count)
    }
}

impl<'a> crate::translator::RuntimeContext<'a> for FunctionCompiler<'a> {
    fn ctx_param(&mut self) -> Value {
        self.builder.block_params(self.entry_block)[0]
    }
    fn gc_ptr(&mut self) -> Value {
        let ctx = self.ctx_param();
        self.builder
            .ins()
            .load(types::I64, MemFlags::trusted(), ctx, 0)
    }
    fn globals_ptr(&mut self) -> Value {
        let ctx = self.ctx_param();
        self.builder
            .ins()
            .load(types::I64, MemFlags::trusted(), ctx, 8)
    }
}

impl crate::translator::MetadataAccess for FunctionCompiler<'_> {
    fn vo_module(&self) -> &VoModule {
        self.vo_module
    }
    fn current_pc(&self) -> usize {
        self.current_pc
    }
    fn func_id(&self) -> u32 {
        self.func_id
    }
    fn current_jit_metadata(&self) -> Option<&vo_runtime::bytecode::JitInstructionMetadata> {
        self.func_def.jit_metadata.get(self.current_pc)
    }
}

impl crate::translator::HelperAccess for FunctionCompiler<'_> {
    fn helpers(&self) -> &HelperFuncs {
        &self.helpers
    }
}

impl crate::translator::RegConstAccess for FunctionCompiler<'_> {
    fn set_reg_const(&mut self, reg: u16, val: i64) {
        self.reg_consts.insert(reg, val);
    }
    fn get_reg_const(&self, reg: u16) -> Option<i64> {
        self.reg_consts.get(&reg).copied()
    }
    fn clear_reg_const(&mut self, reg: u16) {
        self.reg_consts.remove(&reg);
    }
    fn clear_reg_consts(&mut self) {
        self.reg_consts.clear();
    }
}

impl crate::translator::FrameBoundary for FunctionCompiler<'_> {
    fn panic_return_value(&self) -> i32 {
        1
    }
    fn spill_all_vars(&mut self) {
        self.emit_variable_spill();
    }
}

impl<'a> crate::translator::SelectSync<'a> for FunctionCompiler<'a> {
    fn begin_select_tracking(&mut self) {
        self.pending_select_cases.clear();
    }
    fn record_select_send_case(&mut self) {
        self.pending_select_cases.push(SelectSyncCase::Send);
    }
    fn record_select_recv_case(&mut self, dst_reg: u16, elem_slots: u8, has_ok: bool) {
        self.pending_select_cases.push(SelectSyncCase::Recv {
            dst_reg,
            elem_slots,
            has_ok,
        });
    }
    fn sync_select_exec_state(&mut self, result_reg: u16) -> Result<(), JitError> {
        self.sync_select_exec_state_precise(result_reg);
        Ok(())
    }
}

impl crate::translator::FlowFacts for FunctionCompiler<'_> {
    fn is_checked_non_nil(&self, slot: u16) -> bool {
        self.checked_non_nil.contains(&slot)
    }
    fn mark_checked_non_nil(&mut self, slot: u16) {
        self.checked_non_nil.insert(slot);
    }
}

impl<'a> crate::translator::CallBoundary<'a> for FunctionCompiler<'a> {
    fn call_caller_bp(&mut self) -> Value {
        self.saved_caller_bp
    }
    fn call_old_fiber_sp(&mut self) -> Value {
        self.saved_fiber_sp
    }
}

impl crate::translator::StackRefresh for FunctionCompiler<'_> {
    fn refresh_stack_base_after_reallocation(&mut self) {}
}
