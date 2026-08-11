#![allow(clippy::result_large_err, clippy::too_many_arguments)]
//! Function compiler: bytecode -> Cranelift IR.

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    types, FuncRef, Function, InstBuilder, MemFlagsData as MemFlags, Value,
};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use crate::translate::translate_inst;
use crate::translator::{
    HelperKind, HelperRefs, RuntimeContext as _, SelectSyncCase, SlotAccess, TranslateResult,
};
use crate::{analysis::FunctionAnalysis, JitCompileEnv, JitError};
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::JitContextField;

pub struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    core: crate::compile_common::CompilerCore<'a>,
    copy_frame_slots: FuncRef,
    /// Saved jit_bp from function entry, used to recompute fiber.stack address after reallocation
    saved_jit_bp: Variable,
    /// Variable wrapping the args_ptr for this function (points to fiber.stack[jit_bp]).
    /// Declared as a Cranelift Variable so def_var/use_var handle phi insertion at join points,
    /// allowing refresh_stack_base_after_reallocation to redefine it after any call that may
    /// have triggered fiber.stack reallocation via jit_push_frame.
    args_ptr_var: Variable,
    args_ptr_is_stack_var: Variable,
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
        env: JitCompileEnv<'a>,
        mut helpers: HelperRefs<'a>,
        analysis: &'a FunctionAnalysis,
    ) -> Self {
        let copy_frame_slots = helpers
            .resolve(HelperKind::copy_frame_slots, func)
            .func_ref();
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        let saved_jit_bp = builder.declare_var(types::I64);
        let args_ptr_var = builder.declare_var(types::I64);
        let args_ptr_is_stack_var = builder.declare_var(types::I8);
        let jit_memory_flags = crate::translator::JitMemoryFlags::new(&mut builder);

        Self {
            builder,
            core: crate::compile_common::CompilerCore::new(
                func_id,
                func_def,
                vo_module,
                env,
                entry_block,
                helpers,
                analysis,
                analysis.memory_only_start,
                jit_memory_flags,
            ),
            copy_frame_slots,
            saved_jit_bp,
            args_ptr_var,
            args_ptr_is_stack_var,
            saved_caller_bp: Value::from_u32(0),
            saved_fiber_sp: Value::from_u32(0),
            pending_select_cases: Vec::new(),
        }
    }

    pub fn compile(mut self, frontend_config: TargetFrontendConfig) -> Result<(), JitError> {
        self.core.declare_variables(&mut self.builder);
        let policy =
            crate::compile_common::ControlPolicy::full_function(self.core.func_def.code.len());
        self.core.execution_budget_regions = crate::compile_common::prepare_control_flow(
            &mut self.builder,
            &mut self.core.blocks,
            &self.core.func_def.code,
            policy,
        )?;

        self.builder.switch_to_block(self.core.entry_block);
        self.emit_prologue();
        crate::compile_common::drive_compile(&mut self)?;

        self.builder.seal_all_blocks();
        self.builder.finalize(frontend_config);

        Ok(())
    }

    /// Spill all SSA variables to fiber.stack (recomputed base, handles reallocation).
    /// Called on slow path (Call/WaitIo) so VM can see the current state.
    /// Note: args_ptr may be stale if fiber.stack was reallocated during nested calls,
    /// so we recompute the destination from ctx.stack_ptr + saved_jit_bp.
    fn emit_variable_spill(&mut self) {
        let dst_ptr = self.fiber_stack_args_ptr();
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .spill_for_materialized_frame(
            &mut self.builder,
            args_ptr,
            dst_ptr,
            self.copy_frame_slots,
        );
    }

    fn emit_cooperative_yield(&mut self, resume_pc: usize) {
        self.emit_variable_spill();
        let ctx = self.builder.block_params(self.core.entry_block)[0];
        crate::compile_common::emit_cooperative_yield_return(&mut self.builder, ctx, resume_pc);
    }

    fn emit_execution_budget_checkpoint(&mut self, resume_pc: usize, cost: u32) {
        let ctx = self.builder.block_params(self.core.entry_block)[0];
        let poll = crate::compile_common::branch_on_execution_budget(&mut self.builder, ctx, cost);

        self.builder.switch_to_block(poll.exhausted);
        self.builder.seal_block(poll.exhausted);
        self.emit_cooperative_yield(resume_pc);

        crate::compile_common::continue_after_execution_budget_poll(&mut self.builder, ctx, &poll);
    }

    fn current_memory_base_ptr(&mut self) -> Value {
        let entry_args_ptr = self.builder.use_var(self.args_ptr_var);
        let uses_stack = self.builder.use_var(self.args_ptr_is_stack_var);
        let use_stack = self
            .builder
            .ins()
            .icmp_imm_u(IntCC::NotEqual, uses_stack, 0);
        let stack_args_ptr = self.fiber_stack_args_ptr();
        self.builder
            .ins()
            .select(use_stack, stack_args_ptr, entry_args_ptr)
    }

    /// Compute fiber.stack base dynamically from ctx.stack_ptr + saved_jit_bp.
    /// Needed because fiber.stack may reallocate during nested calls.
    fn fiber_stack_args_ptr(&mut self) -> Value {
        let stack_ptr = self.load_context_field(types::I64, JitContextField::StackPtr);
        let jit_bp = self.builder.use_var(self.saved_jit_bp);
        // fiber_args_ptr = stack_ptr + jit_bp * 8
        let bp_offset = self.builder.ins().imul_imm_u(jit_bp, 8);
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

        let recv_cases: Vec<(u16, u16, usize, bool)> = self
            .pending_select_cases
            .iter()
            .filter_map(|case| match *case {
                SelectSyncCase::Send => None,
                SelectSyncCase::Recv {
                    case_idx,
                    dst_reg,
                    elem_slots,
                    has_ok,
                } => Some((case_idx, dst_reg, elem_slots as usize, has_ok)),
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
                if slot >= self.core.memory_only_start {
                    continue;
                }
                if self.core.is_float_slot(slot) {
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
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
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
        self.builder.seal_block(self.core.entry_block);

        let params = self.builder.block_params(self.core.entry_block);
        let args_ptr = params[1]; // Points to fiber.stack[jit_bp]
        let _ret = params[2];
        let current_func_id = self
            .builder
            .ins()
            .iconst(types::I32, i64::from(self.core.func_id));
        self.store_context_field(current_func_id, JitContextField::CurrentFuncId);

        // Wrap args_ptr in a Variable so refresh_stack_base_after_reallocation can redefine
        // it after any call that may have triggered fiber.stack reallocation.
        self.builder.def_var(self.args_ptr_var, args_ptr);

        // Save jit_bp from ctx at function entry.
        // This is needed to compute fiber.stack address for spilling.
        // Also saved as caller_bp (i32) for reuse by all call sites.
        let jit_bp_i32 = self.load_context_field(types::I32, JitContextField::JitBp);
        let jit_bp_i64 = self.builder.ins().uextend(types::I64, jit_bp_i32);
        self.builder.def_var(self.saved_jit_bp, jit_bp_i64);
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
        let fiber_sp_i32 = self.load_context_field(types::I32, JitContextField::FiberSp);
        self.saved_fiber_sp = fiber_sp_i32;

        let param_slots = self.core.func_def.param_slots as usize;
        let ssa_slots = self.core.vars.len();
        let num_slots = self.core.func_def.local_slots as usize;

        // Load params from args_ptr into SSA vars (params already in args_ptr from caller)
        for i in 0..param_slots.min(ssa_slots) {
            let slot = i as u16;
            let val = if self.core.is_float_slot(slot) {
                crate::compile_common::load_memory_slot_f64(&mut self.builder, args_ptr, slot)
            } else {
                crate::compile_common::load_memory_slot_i64(&mut self.builder, args_ptr, slot)
            };
            self.builder.def_var(self.core.vars[i], val);
        }

        // Initialize the non-parameter SSA prefix and memory-backed suffix.
        let zero_i64 = self.builder.ins().iconst(types::I64, 0);
        let zero_f64 = self.builder.ins().f64const(0.0);
        for i in param_slots..ssa_slots {
            if self.core.is_float_slot(i as u16) {
                self.builder.def_var(self.core.vars[i], zero_f64);
            } else {
                self.builder.def_var(self.core.vars[i], zero_i64);
            }
        }
        for i in param_slots.max(ssa_slots)..num_slots {
            crate::compile_common::store_memory_slot(
                &mut self.builder,
                args_ptr,
                i as u16,
                zero_i64,
            );
        }
    }

    fn translate_instruction(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        match translate_inst(self, inst)? {
            TranslateResult::Completed => return Ok(false),
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
                let terminated = crate::call_helpers::emit_call_extern(
                    self,
                    inst,
                    crate::call_helpers::CallExternConfig {
                        current_pc: self.core.current_pc,
                    },
                )?;
                Ok(terminated)
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
        let target =
            self.core
                .checked_branch_target(self.core.current_pc, inst.imm32(), inst.opcode())?;
        let block = self.core.block_for_pc(target, "jump")?;

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
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .load_i64(&mut self.builder, args_ptr, slot)
    }

    /// Write I64 value to variable slot.
    /// SSA-prefix slots update their variable and reach memory at frame-sync
    /// boundaries. Memory-suffix slots write their authoritative frame cell.
    fn store_local(&mut self, slot: u16, val: Value) {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .store_i64(&mut self.builder, args_ptr, slot, val);
        // Writes invalidate local compile-time facts for the slot.
        self.core.checked_non_nil.remove(&slot);
        self.core.reg_consts.remove(&slot);
    }

    fn conditional_jump(&mut self, inst: &Instruction, cmp_cond: IntCC) -> Result<(), JitError> {
        let cond = self.load_local(inst.a);
        let target =
            self.core
                .checked_branch_target(self.core.current_pc, inst.imm32(), inst.opcode())?;
        let target_block = self.core.block_for_pc(target, "conditional jump")?;
        let fall_through = self.builder.create_block();

        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(cmp_cond, cond, zero);
        self.builder
            .ins()
            .brif(cmp, target_block, &[], fall_through, &[]);

        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
        self.core.clear_flow_facts();
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

        let target = self
            .core
            .checked_forloop_target(self.core.current_pc, inst)?;
        let target_block = self.core.block_for_pc(target, "forloop")?;
        let fall_through = self.builder.create_block();

        self.builder
            .ins()
            .brif(continue_loop, target_block, &[], fall_through, &[]);
        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
        self.core.clear_flow_facts();
        Ok(())
    }

    fn ret(&mut self, inst: &Instruction) -> Result<(), JitError> {
        use vo_common_core::bytecode::ReturnFlags;
        let ret_ptr = self.builder.block_params(self.core.entry_block)[2];
        let flags = ReturnFlags::from_bits(inst.flags).ok_or_else(|| {
            JitError::InvalidMetadata(crate::JitMetadataError::InvalidInstructionFlags {
                func: self.core.func_def.name.clone(),
                pc: self.core.current_pc,
                opcode: Opcode::Return,
                flags: inst.flags,
                allowed: ReturnFlags::ALLOWED_BITS,
            })
        })?;
        let heap_returns = flags.has_heap_returns();
        let is_error_return = flags.is_error_return();

        // Pure function: no defer, no error return, no heap returns.
        // VM guards metadata reads with func attributes, so we can skip all metadata stores.
        let is_pure = !self.core.func_def.has_defer
            && self.core.func_def.error_ret_slot < 0
            && self.core.func_def.heap_ret_gcref_count == 0;

        if !is_pure {
            // Set is_error_return for VM errdefer decision.
            let err_flag = self
                .builder
                .ins()
                .iconst(types::I8, if is_error_return { 1 } else { 0 });
            self.store_context_field(err_flag, JitContextField::IsErrorReturn);
        }

        if heap_returns {
            // Heap returns are materialized by the VM for every JIT function.
            // This keeps defer/recover timing correct and lets canonical array
            // returns flatten through ArrayHeader-aware runtime code.
            let gcref_start = self.builder.ins().iconst(types::I16, inst.a as i64);
            self.store_context_field(gcref_start, JitContextField::RetGcRefStart);
            let one = self.builder.ins().iconst(types::I8, 1);
            self.store_context_field(one, JitContextField::RetIsHeap);

            // SSA-only slots are not guaranteed to have reached frame memory.
            let gcref_count = inst.b as usize;
            let args_ptr = self.fiber_stack_args_ptr();
            for i in 0..gcref_count {
                let slot = (inst.a as usize + i) as u16;
                if slot < self.core.memory_only_start {
                    let val_i64 = crate::compile_common::read_ssa_slot_i64(
                        &mut self.builder,
                        &self.core.vars,
                        &self.core.func_def.slot_types,
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
            if !is_pure {
                let zero = self.builder.ins().iconst(types::I8, 0);
                self.store_context_field(zero, JitContextField::RetIsHeap);

                // Store ret_start for VM to extract slot_types for GC scanning
                let ret_start_val = self.builder.ins().iconst(types::I16, inst.a as i64);
                self.store_context_field(ret_start_val, JitContextField::RetStart);
            }

            let ret_slots = self.core.func_def.ret_slots as usize;
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
            .core
            .vo_module
            .functions
            .get(target_func_id as usize)
            .ok_or(JitError::FunctionNotFound(target_func_id))?;
        let call_plan = crate::call_helpers::CallPlan::new(target_func_id, arg_start, target_func);

        match call_plan.route_for_full_function(self.core.func_id) {
            crate::call_helpers::CallRoute::DynamicJitTable => {
                crate::call_helpers::emit_jit_call_with_vm_materialization(self, call_plan)?;
                Ok(false)
            }
            crate::call_helpers::CallRoute::VmCallMaterialization => {
                crate::call_helpers::emit_call_via_vm(
                    self,
                    call_plan.vm_config(self.core.current_pc + 1),
                )?;
                Ok(true)
            }
        }
    }
}

impl<'a> crate::compile_common::CompileDriver for FunctionCompiler<'a> {
    fn control_policy(&self) -> crate::compile_common::ControlPolicy {
        crate::compile_common::ControlPolicy::full_function(self.core.func_def.code.len())
    }

    fn set_current_pc(&mut self, pc: usize) {
        self.core.current_pc = pc;
    }

    fn enter_pc_block(&mut self, pc: usize, block_terminated: &mut bool) -> Result<(), JitError> {
        if crate::compile_common::enter_compile_pc(
            &mut self.builder,
            &self.core.blocks,
            pc,
            block_terminated,
        ) {
            self.core.clear_flow_facts();
            if let Some(cost) = self.core.execution_budget_regions.get(&pc).copied() {
                self.emit_execution_budget_checkpoint(pc, cost);
            }
        }
        Ok(())
    }

    fn apply_pc_facts(&mut self, pc: usize) -> Result<(), JitError> {
        self.core.apply_reg_const_facts(pc)
    }

    fn instruction_for_pc(&self, pc: usize) -> Result<Instruction, JitError> {
        self.core
            .func_def
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

    fn jit_memory_flags(&self) -> crate::translator::JitMemoryFlags {
        self.core.jit_memory_flags
    }
}

impl<'a> crate::translator::ScratchAccess<'a> for FunctionCompiler<'a> {
    fn native_scratch_slots(&mut self) -> &mut crate::translator::NativeScratchSlots {
        &mut self.core.native_scratch_slots
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
        self.builder.ins().iadd_imm_u(args_ptr, i64::from(slot) * 8)
    }
    fn local_slot_count(&self) -> usize {
        self.core.func_def.local_slots as usize
    }
    fn read_var_f64(&mut self, slot: u16) -> Value {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .load_f64(&mut self.builder, args_ptr, slot)
    }
    fn write_var_f64(&mut self, slot: u16, val: Value) {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .store_f64(&mut self.builder, args_ptr, slot, val);
        self.core.checked_non_nil.remove(&slot);
        self.core.reg_consts.remove(&slot);
    }
    fn reload_all_vars_from_memory(&mut self) {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .reload_all_from_memory(&mut self.builder, args_ptr);
        self.core.clear_flow_facts();
    }
    fn sync_written_slots(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError> {
        self.sync_written_slots_precise(start_slot, slot_count)
    }
}

impl<'a> crate::translator::RuntimeContext<'a> for FunctionCompiler<'a> {
    fn ctx_param(&mut self) -> Value {
        self.builder.block_params(self.core.entry_block)[0]
    }
}

crate::translator::impl_shared_compiler_traits!(FunctionCompiler<'_>);

impl crate::translator::FrameBoundary for FunctionCompiler<'_> {
    fn spill_all_vars(&mut self) {
        self.emit_variable_spill();
    }
}

impl<'a> crate::translator::SelectSync<'a> for FunctionCompiler<'a> {
    fn begin_select_tracking(&mut self) {
        self.pending_select_cases.clear();
    }
    fn record_select_send_case(&mut self, _case_idx: u16) {
        self.pending_select_cases.push(SelectSyncCase::Send);
    }
    fn record_select_recv_case(
        &mut self,
        case_idx: u16,
        dst_reg: u16,
        elem_slots: u16,
        has_ok: bool,
    ) {
        self.pending_select_cases.push(SelectSyncCase::Recv {
            case_idx,
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
