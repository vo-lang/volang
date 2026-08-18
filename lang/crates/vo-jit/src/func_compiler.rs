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
use vo_runtime::bytecode::{FunctionDef, InstructionMetadata, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::JitContextField;

struct VirtualObject {
    active: Variable,
    pointer: Variable,
    fields: Vec<Variable>,
}

pub struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    core: crate::compile_common::CompilerCore<'a>,
    /// Saved jit_bp from function entry, used to recompute fiber.stack address after reallocation
    saved_jit_bp: Variable,
    /// Frame base derived from the canonical frame pointer at function entry.
    saved_caller_bp: Value,
    /// End of this function's verified frame window.
    saved_fiber_sp: Value,
    pending_select_cases: Vec<SelectSyncCase>,
    tier: vo_runtime::jit_api::JitTier,
    inline_plan: &'a crate::optimizer::ModuleInlinePlan,
    optimization_plan: Option<&'a crate::optimizer::ModuleOptimizationPlan>,
    instruction_optimization: Option<&'a crate::optimizer::OptimizedFunction>,
    self_native_ref: Option<FuncRef>,
    virtual_objects: Vec<VirtualObject>,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        env: JitCompileEnv<'a>,
        entry_eligibility: &'a [crate::JitFrameEntryEligibility],
        helpers: HelperRefs<'a>,
        analysis: &'a FunctionAnalysis,
        tier: vo_runtime::jit_api::JitTier,
        inline_plan: &'a crate::optimizer::ModuleInlinePlan,
        optimization_plan: Option<&'a crate::optimizer::ModuleOptimizationPlan>,
        instruction_optimization: Option<&'a crate::optimizer::OptimizedFunction>,
        self_native_ref: Option<FuncRef>,
    ) -> Self {
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        let saved_jit_bp = builder.declare_var(types::I64);
        let jit_memory_flags = crate::translator::JitMemoryFlags::new(&mut builder);
        let virtual_objects = instruction_optimization
            .map(crate::optimizer::OptimizedFunction::scalar_object_slots)
            .unwrap_or_default()
            .into_iter()
            .map(|slots| VirtualObject {
                active: builder.declare_var(types::I8),
                pointer: builder.declare_var(types::I64),
                fields: (0..slots)
                    .map(|_| builder.declare_var(types::I64))
                    .collect(),
            })
            .collect();

        Self {
            builder,
            core: crate::compile_common::CompilerCore::new(
                func_id,
                func_def,
                vo_module,
                env,
                entry_eligibility,
                entry_block,
                helpers,
                analysis,
                std::borrow::Cow::Borrowed(analysis.memory_slots()),
                jit_memory_flags,
            ),
            saved_jit_bp,
            saved_caller_bp: Value::from_u32(0),
            saved_fiber_sp: Value::from_u32(0),
            pending_select_cases: Vec::new(),
            tier,
            inline_plan,
            optimization_plan,
            instruction_optimization,
            self_native_ref,
            virtual_objects,
        }
    }

    pub fn compile(mut self, frontend_config: TargetFrontendConfig) -> Result<(), JitError> {
        self.core.declare_variables(&mut self.builder);
        let policy =
            crate::compile_common::ControlPolicy::full_function(self.core.func_def.code.len());
        self.core.execution_budget_regions = crate::compile_common::prepare_control_flow(
            &mut self.builder,
            &mut self.core.blocks,
            self.core.analysis.ir(),
            policy,
            &self.core.vars,
            self.instruction_optimization.is_some(),
            self.instruction_optimization,
        )?;

        self.builder.switch_to_block(self.core.entry_block);
        self.emit_prologue();
        self.initialize_virtual_objects();
        crate::compile_common::drive_compile(&mut self)?;

        self.builder.seal_all_blocks();
        self.builder.finalize(frontend_config);

        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn compile_inline_probe(
        mut self,
        frontend_config: TargetFrontendConfig,
        inline: &crate::call_helpers::SmallFunctionInline,
    ) -> Result<(), JitError> {
        self.core.declare_variables(&mut self.builder);
        let policy =
            crate::compile_common::ControlPolicy::full_function(self.core.func_def.code.len());
        let optimized = crate::optimizer::OptimizedFunction::inline_cost_probe(
            self.core.analysis.ir(),
            inline.cost().try_into().unwrap_or(u32::MAX),
        );
        self.core.execution_budget_regions = crate::compile_common::prepare_control_flow(
            &mut self.builder,
            &mut self.core.blocks,
            self.core.analysis.ir(),
            policy,
            &self.core.vars,
            false,
            Some(&optimized),
        )?;
        self.builder.switch_to_block(self.core.entry_block);
        self.emit_prologue();
        if let Some(cost) = self.core.execution_budget_regions.get(&0).copied() {
            self.emit_execution_budget_checkpoint(0, cost);
        }
        inline.emit_into_for_test(&mut self, 0)?;
        let value = self.load_local(0);
        let ret_ptr = self.builder.block_params(self.core.entry_block)[2];
        self.builder
            .ins()
            .store(MemFlags::trusted(), value, ret_ptr, 0);
        let ok = self.builder.ins().iconst(types::I32, 0);
        self.builder.ins().return_(&[ok]);
        self.builder.seal_all_blocks();
        self.builder.finalize(frontend_config);
        Ok(())
    }

    /// Publish the exact state needed to execute the bytecode at `resume_pc`.
    /// The canonical entry pointer identifies this frame's BP, so a current
    /// destination can always be rebuilt after fiber.stack reallocation.
    fn publish_recovery_state(&mut self, resume_pc: usize) {
        self.materialize_virtual_objects();
        self.publish_execution_context();
        let dst_ptr = self.fiber_stack_args_ptr();
        let recovery_values = self
            .core
            .analysis
            .ir()
            .resume_values(resume_pc)
            .unwrap_or_else(|| {
                panic!(
                    "native side exit lacks recovery state: func={} pc={} opcode={:?}",
                    self.core.func_def.name,
                    resume_pc,
                    self.core.func_def.code[resume_pc].opcode()
                )
            });
        crate::compile_common::CompilerStorage::for_function(self.core.func_def, &self.core.vars)
            .spill_recovery_state_to_memory(&mut self.builder, dst_ptr, recovery_values);
    }

    fn emit_variable_spill(&mut self) {
        self.publish_recovery_state(self.core.current_pc);
    }

    fn publish_execution_context(&mut self) {
        let func_id = self
            .builder
            .ins()
            .iconst(types::I32, i64::from(self.core.func_id));
        self.store_context_field(self.saved_caller_bp, JitContextField::JitBp);
        self.store_context_field(self.saved_fiber_sp, JitContextField::FiberSp);
        self.store_context_field(func_id, JitContextField::CurrentFuncId);
    }

    fn initialize_virtual_objects(&mut self) {
        if self.virtual_objects.is_empty() {
            return;
        }
        let inactive = self.builder.ins().iconst(types::I8, 0);
        let zero = self.builder.ins().iconst(types::I64, 0);
        for object in &self.virtual_objects {
            self.builder.def_var(object.active, inactive);
            self.builder.def_var(object.pointer, zero);
            for field in &object.fields {
                self.builder.def_var(*field, zero);
            }
        }
    }

    fn materialize_virtual_objects(&mut self) {
        for object_id in 0..self.virtual_objects.len() {
            let active = self.builder.use_var(self.virtual_objects[object_id].active);
            let materialize = crate::compile_common::cold_block(&mut self.builder);
            let done = self.builder.create_block();
            let is_active = self.builder.ins().icmp_imm_u(IntCC::NotEqual, active, 0);
            self.builder
                .ins()
                .brif(is_active, materialize, &[], done, &[]);

            self.builder.switch_to_block(materialize);
            self.builder.seal_block(materialize);
            let pointer = self
                .builder
                .use_var(self.virtual_objects[object_id].pointer);
            let fields = self.virtual_objects[object_id].fields.clone();
            for (offset, field) in fields.into_iter().enumerate() {
                let value = self.builder.use_var(field);
                self.builder
                    .ins()
                    .store(MemFlags::trusted(), value, pointer, (offset * 8) as i32);
            }
            self.builder.ins().jump(done, &[]);

            self.builder.switch_to_block(done);
            self.builder.seal_block(done);
        }
    }

    fn emit_cooperative_yield(&mut self, resume_pc: usize) {
        self.publish_recovery_state(resume_pc);
        let ctx = self.builder.block_params(self.core.entry_block)[0];
        crate::compile_common::emit_cooperative_yield_return(&mut self.builder, ctx, resume_pc);
    }

    fn emit_execution_budget_checkpoint(&mut self, resume_pc: usize, cost: u32) {
        let ctx = self.builder.block_params(self.core.entry_block)[0];
        let refill = self
            .core
            .helpers
            .resolve(HelperKind::refill_execution_budget, self.builder.func)
            .func_ref();
        let poll = crate::compile_common::branch_on_execution_budget(&mut self.builder, ctx, cost);

        self.builder.switch_to_block(poll.exhausted);
        self.builder.seal_block(poll.exhausted);
        let yield_block = crate::compile_common::cold_block(&mut self.builder);
        let can_poll_gc = self
            .core
            .entry_eligibility
            .get(self.core.func_id as usize)
            .is_some_and(|eligibility| eligibility.may_gc);
        if can_poll_gc {
            crate::translator::emit_gc_safepoint_poll(self);
        } else {
            let clear = crate::compile_common::continue_if_no_gc_requested(
                &mut self.builder,
                ctx,
                yield_block,
            );
            self.builder.switch_to_block(clear);
            self.builder.seal_block(clear);
        }
        crate::compile_common::refill_execution_budget(
            &mut self.builder,
            ctx,
            refill,
            &poll,
            yield_block,
        );
        self.builder.switch_to_block(yield_block);
        self.builder.seal_block(yield_block);
        self.emit_cooperative_yield(resume_pc);

        crate::compile_common::continue_after_execution_budget_poll(&mut self.builder, ctx, &poll);
    }

    fn current_memory_base_ptr(&mut self) -> Value {
        self.fiber_stack_args_ptr()
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
                if !self.core.is_ssa_slot(slot) {
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

        let params = self.builder.block_params(self.core.entry_block).to_vec();
        let frame_bp = params[1];
        let _ret = params[2];
        if self.tier == vo_runtime::jit_api::JitTier::Baseline {
            let profile_table = self.builder.ins().load(
                types::I64,
                MemFlags::trusted(),
                params[0],
                JitContextField::JitProfileTable.offset(),
            );
            let profile_offset =
                i64::from(self.core.func_id) * vo_runtime::jit_api::JitProfileCounters::SIZE as i64;
            let profile = self.builder.ins().iadd_imm_u(profile_table, profile_offset);
            let tier_up_state = self.builder.ins().load(
                types::I64,
                MemFlags::trusted(),
                profile,
                vo_runtime::jit_api::JitProfileCounters::OFFSET_TIER_UP_STATE,
            );
            let eligible = self
                .builder
                .ins()
                .icmp_imm_u(IntCC::Equal, tier_up_state, 0);
            let profile_block = self.builder.create_block();
            let continue_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(eligible, profile_block, &[], continue_block, &[]);

            self.builder.switch_to_block(profile_block);
            self.builder.seal_block(profile_block);
            let entries = self.builder.ins().load(
                types::I64,
                MemFlags::trusted(),
                profile,
                vo_runtime::jit_api::JitProfileCounters::OFFSET_ENTRIES,
            );
            let entries = self.builder.ins().iadd_imm_u(entries, 1);
            self.builder.ins().store(
                MemFlags::trusted(),
                entries,
                profile,
                vo_runtime::jit_api::JitProfileCounters::OFFSET_ENTRIES,
            );
            let threshold = self.builder.ins().load(
                types::I64,
                MemFlags::trusted(),
                params[0],
                JitContextField::OptimizingThreshold.offset(),
            );
            let hot =
                self.builder
                    .ins()
                    .icmp(IntCC::UnsignedGreaterThanOrEqual, entries, threshold);
            let request_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(hot, request_block, &[], continue_block, &[]);
            self.builder.switch_to_block(request_block);
            self.builder.seal_block(request_block);
            let func_id = self
                .builder
                .ins()
                .iconst(types::I32, i64::from(self.core.func_id));
            let requested = self.builder.ins().iconst(types::I64, 1);
            self.builder.ins().store(
                MemFlags::trusted(),
                requested,
                profile,
                vo_runtime::jit_api::JitProfileCounters::OFFSET_TIER_UP_STATE,
            );
            let tier_up = self
                .core
                .helpers
                .resolve(HelperKind::tier_up, self.builder.func);
            // Tier-up runs before the function's local SSA state exists. A
            // strict callback failure can therefore return directly without
            // spilling locals; the VM frame remains the source of truth.
            let call =
                crate::translator::emit_runtime_helper_call(self, tier_up, &[params[0], func_id]);
            let result = self.builder.inst_results(call)[0];
            crate::call_helpers::check_call_result(self, result, false);
            self.builder.ins().jump(continue_block, &[]);
            self.builder.switch_to_block(continue_block);
            self.builder.seal_block(continue_block);
        }
        if !self.core.entry_eligibility[self.core.func_id as usize].frame_elided {
            let current_func_id = self
                .builder
                .ins()
                .iconst(types::I32, i64::from(self.core.func_id));
            self.store_context_field(current_func_id, JitContextField::CurrentFuncId);
        }

        // The native ABI carries the canonical slot index directly. It stays
        // valid across callbacks that relocate the fiber stack; raw pointers
        // are reconstructed lazily only for memory-backed slots.
        let jit_bp_i32 = self.builder.ins().ireduce(types::I32, frame_bp);
        self.builder.def_var(self.saved_jit_bp, frame_bp);
        self.saved_caller_bp = jit_bp_i32;
        self.saved_fiber_sp = self
            .builder
            .ins()
            .iadd_imm_u(jit_bp_i32, i64::from(self.core.func_def.local_slots));

        let param_slots = self.core.func_def.param_slots as usize;
        let needs_frame_ptr = param_slots > crate::NATIVE_ARG_LANES
            || self
                .core
                .memory_slots
                .slots()
                .any(|slot| usize::from(slot) >= param_slots);
        let frame_ptr = needs_frame_ptr.then(|| self.fiber_stack_args_ptr());

        // The internal native ABI carries the first raw argument words in
        // machine lanes. Wide signatures continue in frame memory. Float
        // locals receive an explicit raw-word bitcast at this boundary.
        for i in 0..param_slots {
            let slot = i as u16;
            let Some(variable) = self.core.vars.get(slot) else {
                continue;
            };
            let raw = if i < crate::NATIVE_ARG_LANES {
                params[3 + i]
            } else {
                crate::compile_common::load_memory_slot_i64(
                    &mut self.builder,
                    frame_ptr.expect("wide parameter requires frame memory"),
                    slot,
                )
            };
            let val = if self.core.is_float_slot(slot) {
                self.builder.ins().bitcast(types::F64, MemFlags::new(), raw)
            } else {
                raw
            };
            self.builder.def_var(variable, val);
        }

        // Initialize only active non-parameter SSA slots. Slots forced into
        // memory by aliasing retain the canonical frame initialization.
        let zero_i64 = self.builder.ins().iconst(types::I64, 0);
        let zero_f64 = self.builder.ins().f64const(0.0);
        for (slot, variable) in self
            .core
            .vars
            .iter()
            .filter(|(slot, _)| usize::from(*slot) >= param_slots)
        {
            if self.core.is_float_slot(slot) {
                self.builder.def_var(variable, zero_f64);
            } else {
                self.builder.def_var(variable, zero_i64);
            }
        }
        for slot in self
            .core
            .memory_slots
            .slots()
            .filter(|slot| usize::from(*slot) >= param_slots)
        {
            crate::compile_common::store_memory_slot(
                &mut self.builder,
                frame_ptr.expect("memory-backed local requires frame memory"),
                slot,
                zero_i64,
            );
        }
    }

    fn translate_instruction(
        &mut self,
        instruction: crate::ir::TypedInstruction,
        optimized: Option<crate::optimizer::OptimizedInstruction>,
    ) -> Result<bool, JitError> {
        let source = instruction.source();
        let inst = &source;
        if optimized.is_some_and(|node| node.action == crate::optimizer::LoweringAction::AlwaysJump)
        {
            debug_assert!(matches!(inst.opcode(), Opcode::JumpIf | Opcode::JumpIfNot));
            self.jump(inst)?;
            return Ok(true);
        }
        if let Some(crate::optimizer::LoweringAction::Replace(replacement)) =
            optimized.map(|node| node.action)
        {
            let value = self.core.lowered_value(replacement).ok_or_else(|| {
                JitError::Internal(format!(
                    "GVN replacement value {} is unavailable at pc {}",
                    replacement.index(),
                    self.core.current_pc
                ))
            })?;
            let output = *self
                .core
                .analysis
                .ir()
                .outputs(instruction)
                .first()
                .ok_or_else(|| {
                    JitError::Internal(format!(
                        "GVN replacement at pc {} has no output",
                        self.core.current_pc
                    ))
                })?;
            let output = self.core.analysis.ir().value(output);
            match output.ty {
                crate::ir::ValueType::Float64 => self.write_var_f64(output.slot, value),
                _ => self.store_local(output.slot, value),
            }
            return Ok(false);
        }
        if self.tier == vo_runtime::jit_api::JitTier::Optimizing
            && inst.opcode() == Opcode::PtrNew
            && self
                .emit_virtual_allocation(inst, optimized.and_then(|node| node.scalar_replacement))?
        {
            return Ok(false);
        }
        if self.tier == vo_runtime::jit_api::JitTier::Optimizing {
            match inst.opcode() {
                Opcode::PtrGet | Opcode::PtrGetN
                    if self.emit_virtual_ptr_get(
                        inst,
                        optimized.and_then(|node| node.virtual_object),
                    ) =>
                {
                    return Ok(false);
                }
                Opcode::PtrSet | Opcode::PtrSetN
                    if self.emit_virtual_ptr_set(
                        inst,
                        optimized.and_then(|node| node.virtual_object),
                    ) =>
                {
                    return Ok(false);
                }
                _ => {}
            }
            if optimized.and_then(|node| node.fresh_shape).is_some() {
                match inst.opcode() {
                    Opcode::PtrGet | Opcode::PtrGetN => {
                        crate::translate::fresh_ptr_get(self, inst)?;
                        return Ok(false);
                    }
                    Opcode::PtrSet | Opcode::PtrSetN => {
                        crate::translate::fresh_ptr_set(self, inst)?;
                        return Ok(false);
                    }
                    _ => {}
                }
            }
        }
        match translate_inst(self, instruction)? {
            TranslateResult::Completed => {
                return Ok(false);
            }
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
            Opcode::Call => self.call(inst, optimized.and_then(|node| node.inline_target())),
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
                if self.try_inline_dynamic_call(
                    inst,
                    optimized.and_then(|node| node.inline_target()),
                )? {
                    return Ok(false);
                }
                crate::call_helpers::emit_call_closure(self, inst)?;
                Ok(false)
            }
            Opcode::CallIface => {
                if self.try_inline_dynamic_call(
                    inst,
                    optimized.and_then(|node| node.inline_target()),
                )? {
                    return Ok(false);
                }
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

    fn try_inline_dynamic_call(
        &mut self,
        inst: &Instruction,
        inline_target: Option<u32>,
    ) -> Result<bool, JitError> {
        let Some(target) = inline_target else {
            return Ok(false);
        };
        let Some(inline) = self
            .optimization_plan
            .and_then(|plan| plan.pure_leaf_inline(self.core.func_id, target))
        else {
            return Ok(false);
        };
        let Some(metadata) = self
            .core
            .func_def
            .instruction_metadata
            .get(self.core.current_pc)
        else {
            return Ok(false);
        };
        let (arg_slots, ret_slots) = match (inst.opcode(), metadata) {
            (
                Opcode::CallClosure,
                InstructionMetadata::CallLayout {
                    arg_layout,
                    ret_layout,
                },
            )
            | (
                Opcode::CallIface,
                InstructionMetadata::CallIfaceLayout {
                    arg_layout,
                    ret_layout,
                    ..
                },
            ) => (arg_layout.len(), ret_layout.len()),
            _ => return Ok(false),
        };
        if !inline.supports_dynamic_layout(arg_slots, ret_slots) {
            return Ok(false);
        }

        let slot0 = match inst.opcode() {
            Opcode::CallClosure => self.load_local(inst.a),
            Opcode::CallIface => self.load_local(inst.a + 1),
            _ => unreachable!("dynamic inline was filtered by opcode"),
        };
        let arg_start = usize::from(inst.b);
        inline.emit_dynamic(self, slot0, arg_start, arg_start + arg_slots)?;
        Ok(true)
    }

    fn emit_virtual_allocation(
        &mut self,
        inst: &Instruction,
        replacement: Option<crate::escape::ScalarReplacement>,
    ) -> Result<bool, JitError> {
        let Some(replacement) = replacement else {
            return Ok(false);
        };
        crate::translate::materialize_scalar_replaced_ptr_new(self, inst)?;
        let pointer = self.load_local(inst.a);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let active = self.builder.ins().iconst(types::I8, 1);
        let Some(object) = self.virtual_objects.get(replacement.object as usize) else {
            return Err(JitError::Internal(format!(
                "scalar replacement object {} is absent at pc {}",
                replacement.object, self.core.current_pc
            )));
        };
        debug_assert_eq!(object.fields.len(), usize::from(replacement.slots));
        self.builder.def_var(object.pointer, pointer);
        self.builder.def_var(object.active, active);
        for field in &object.fields {
            self.builder.def_var(*field, zero);
        }
        self.core.checked_non_nil.insert(inst.a);
        Ok(true)
    }

    fn emit_virtual_ptr_get(&mut self, inst: &Instruction, object_id: Option<u32>) -> bool {
        let Some(object_id) = object_id else {
            return false;
        };
        let count = match inst.opcode() {
            Opcode::PtrGet => 1,
            Opcode::PtrGetN => self
                .core
                .func_def
                .instruction_metadata
                .get(self.core.current_pc)
                .and_then(vo_runtime::bytecode::InstructionMetadata::ptr_value_layout)
                .map_or(0, <[vo_runtime::SlotType]>::len),
            _ => unreachable!(),
        };
        let start = usize::from(inst.c);
        let Some(object) = self.virtual_objects.get(object_id as usize) else {
            return false;
        };
        let Some(fields) = object.fields.get(start..start.saturating_add(count)) else {
            return false;
        };
        let fields = fields.to_vec();
        for (offset, field) in fields.into_iter().enumerate() {
            let value = self.builder.use_var(field);
            let destination = inst.a + offset as u16;
            self.store_local(destination, value);
        }
        true
    }

    fn emit_virtual_ptr_set(&mut self, inst: &Instruction, object_id: Option<u32>) -> bool {
        let Some(object_id) = object_id else {
            return false;
        };
        let count = match inst.opcode() {
            Opcode::PtrSet => 1,
            Opcode::PtrSetN => self
                .core
                .func_def
                .instruction_metadata
                .get(self.core.current_pc)
                .and_then(vo_runtime::bytecode::InstructionMetadata::ptr_value_layout)
                .map_or(0, <[vo_runtime::SlotType]>::len),
            _ => unreachable!(),
        };
        let values = (0..count)
            .map(|offset| self.load_local(inst.c + offset as u16))
            .collect::<Vec<_>>();
        let start = usize::from(inst.b);
        let Some(object) = self.virtual_objects.get(object_id as usize) else {
            return false;
        };
        let Some(fields) = object.fields.get(start..start.saturating_add(count)) else {
            return false;
        };
        for (&field, value) in fields.iter().zip(values) {
            self.builder.def_var(field, value);
        }
        true
    }

    fn jump(&mut self, inst: &Instruction) -> Result<(), JitError> {
        let target =
            self.core
                .checked_branch_target(self.core.current_pc, inst.imm32(), inst.opcode())?;
        let block = self.core.block_for_pc(target, "jump")?;
        let arguments = crate::compile_common::block_arguments(
            &mut self.builder,
            self.core.analysis.ir(),
            &self.core.vars,
            target,
        );
        self.builder.ins().jump(block, &arguments);
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
        if self.core.is_ssa_slot(slot) {
            if let Some(value) = self.core.lowered_value_for_slot(slot) {
                return if self.core.is_float_slot(slot) {
                    self.builder
                        .ins()
                        .bitcast(types::I64, MemFlags::new(), value)
                } else {
                    value
                };
            }
        }
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(self.core.func_def, &self.core.vars)
            .load_i64(&mut self.builder, args_ptr, slot)
    }

    /// Write I64 value to variable slot.
    /// SSA slots update their variable and reach memory at frame-sync
    /// boundaries. Aliased slots write their authoritative frame cell.
    fn store_local(&mut self, slot: u16, val: Value) {
        let ir_value = if self.core.is_ssa_slot(slot) {
            let storage = crate::compile_common::CompilerStorage::for_function(
                self.core.func_def,
                &self.core.vars,
            );
            storage.store_ssa_i64(&mut self.builder, slot, val)
        } else {
            let args_ptr = self.current_memory_base_ptr();
            let storage = crate::compile_common::CompilerStorage::for_function(
                self.core.func_def,
                &self.core.vars,
            );
            storage.store_memory_i64(&mut self.builder, args_ptr, slot, val)
        };
        self.core.record_output_value(slot, ir_value);
        // Writes invalidate local compile-time facts for the slot.
        self.core.checked_non_nil.remove(&slot);
    }

    fn conditional_jump(&mut self, inst: &Instruction, cmp_cond: IntCC) -> Result<(), JitError> {
        let cond = self.load_local(inst.a);
        let target =
            self.core
                .checked_branch_target(self.core.current_pc, inst.imm32(), inst.opcode())?;
        let target_block = self.core.block_for_pc(target, "conditional jump")?;
        let fall_through = self.builder.create_block();
        let target_arguments = crate::compile_common::block_arguments(
            &mut self.builder,
            self.core.analysis.ir(),
            &self.core.vars,
            target,
        );

        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(cmp_cond, cond, zero);
        self.builder
            .ins()
            .brif(cmp, target_block, &target_arguments, fall_through, &[]);

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
        let target_arguments = crate::compile_common::block_arguments(
            &mut self.builder,
            self.core.analysis.ir(),
            &self.core.vars,
            target,
        );

        self.builder.ins().brif(
            continue_loop,
            target_block,
            &target_arguments,
            fall_through,
            &[],
        );
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
                if self.core.is_ssa_slot(slot) {
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
    fn call(&mut self, inst: &Instruction, inline_target: Option<u32>) -> Result<bool, JitError> {
        let target_func_id = inst.static_call_func_id();
        let arg_start = inst.b as usize;

        let target_func = self
            .core
            .vo_module
            .functions
            .get(target_func_id as usize)
            .ok_or(JitError::FunctionNotFound(target_func_id))?;
        let eligibility = self
            .core
            .entry_eligibility
            .get(target_func_id as usize)
            .copied()
            .ok_or(JitError::FunctionNotFound(target_func_id))?;
        let call_plan = crate::call_helpers::CallPlan::with_eligibility(
            target_func_id,
            arg_start,
            target_func,
            eligibility,
        );
        let selected_inline = inline_target
            .filter(|target| *target == target_func_id)
            .and_then(|_| {
                self.inline_plan
                    .static_inline(self.core.func_id, target_func_id)
            });
        if let Some(inline) = selected_inline {
            inline.emit(self, call_plan.arg_start)?;
            return Ok(false);
        }

        let direct_self = self
            .optimization_plan
            .is_some_and(|plan| plan.direct_self_call(self.core.func_id, target_func_id));
        let direct_native = direct_self.then_some(self.self_native_ref).flatten();
        let recursive_edge = self
            .inline_plan
            .is_recursive_edge(self.core.func_id, target_func_id);
        match call_plan.route_for_full_function(self.core.func_id) {
            crate::call_helpers::CallRoute::DynamicJitTable => {
                crate::call_helpers::emit_jit_call_with_vm_materialization(
                    self,
                    call_plan,
                    direct_native,
                    recursive_edge,
                )?;
                Ok(false)
            }
            crate::call_helpers::CallRoute::PreparedJitTable => {
                crate::call_helpers::emit_jit_call_with_vm_materialization(
                    self,
                    call_plan,
                    direct_native,
                    recursive_edge,
                )?;
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

    fn is_pc_executable(&self, pc: usize) -> bool {
        self.instruction_optimization
            .is_none_or(|graph| graph.is_executable(pc))
    }

    fn set_current_pc(&mut self, pc: usize) {
        self.core.current_pc = pc;
        self.core.current_bounds_check_elided = self
            .instruction_optimization
            .and_then(|graph| graph.instruction(pc))
            .is_some_and(|node| node.bounds_check_elided);
        self.core.current_nil_check_elided = self
            .instruction_optimization
            .and_then(|graph| graph.instruction(pc))
            .is_some_and(|node| node.nil_check_elided);
    }

    fn enter_pc_block(&mut self, pc: usize, block_terminated: &mut bool) -> Result<(), JitError> {
        if crate::compile_common::enter_compile_pc(
            &mut self.builder,
            crate::compile_common::CompileBlockView {
                blocks: &self.core.blocks,
                ir: self.core.analysis.ir(),
                vars: &self.core.vars,
                executable_only: self.instruction_optimization.is_some(),
                optimized: self.instruction_optimization,
            },
            pc,
            block_terminated,
        ) {
            if let Some(&block) = self.core.blocks.get(&pc) {
                self.core
                    .bind_ir_block_parameters(&mut self.builder, pc, block);
            }
            self.core.clear_flow_facts();
            if let Some(cost) = self.core.execution_budget_regions.get(&pc).copied() {
                self.emit_execution_budget_checkpoint(pc, cost);
            }
        }
        Ok(())
    }

    fn apply_pc_facts(&mut self, pc: usize) -> Result<(), JitError> {
        self.core.apply_ir_facts(pc)
    }

    fn should_skip_instruction(&self, inst: crate::compile_common::LoweringInstruction) -> bool {
        inst.optimized()
            .is_some_and(|node| node.action == crate::optimizer::LoweringAction::Eliminate)
    }

    fn instruction_for_pc(
        &self,
        pc: usize,
    ) -> Result<crate::compile_common::LoweringInstruction, JitError> {
        if let Some(graph) = self.instruction_optimization {
            return graph
                .instruction(pc)
                .map(crate::compile_common::LoweringInstruction::Optimized)
                .ok_or_else(|| {
                    JitError::Internal(format!("optimized function pc {pc} is outside graph"))
                });
        }
        self.core
            .analysis
            .ir()
            .instruction(pc)
            .copied()
            .map(crate::compile_common::LoweringInstruction::Baseline)
            .ok_or_else(|| JitError::Internal(format!("function compile pc {pc} is outside code")))
    }

    fn translate_pc_instruction(
        &mut self,
        inst: crate::compile_common::LoweringInstruction,
    ) -> Result<bool, JitError> {
        self.translate_instruction(inst.typed(), inst.optimized())
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
        if self.core.is_ssa_slot(slot) {
            if let Some(value) = self.core.lowered_value_for_slot(slot) {
                return if self.core.is_float_slot(slot) {
                    value
                } else {
                    self.builder
                        .ins()
                        .bitcast(types::F64, MemFlags::new(), value)
                };
            }
        }
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(self.core.func_def, &self.core.vars)
            .load_f64(&mut self.builder, args_ptr, slot)
    }
    fn write_var_f64(&mut self, slot: u16, val: Value) {
        let ir_value = if self.core.is_ssa_slot(slot) {
            let storage = crate::compile_common::CompilerStorage::for_function(
                self.core.func_def,
                &self.core.vars,
            );
            storage.store_ssa_f64(&mut self.builder, slot, val)
        } else {
            let args_ptr = self.current_memory_base_ptr();
            let storage = crate::compile_common::CompilerStorage::for_function(
                self.core.func_def,
                &self.core.vars,
            );
            storage.store_memory_f64(&mut self.builder, args_ptr, slot, val)
        };
        self.core.record_output_value(slot, ir_value);
        self.core.checked_non_nil.remove(&slot);
    }
    fn reload_all_vars_from_memory(&mut self) {
        let args_ptr = self.current_memory_base_ptr();
        crate::compile_common::CompilerStorage::for_function(self.core.func_def, &self.core.vars)
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
    fn publish_current_frame_state(&mut self) {
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
    fn call_caller_func_id(&mut self) -> Value {
        self.builder
            .ins()
            .iconst(types::I32, i64::from(self.core.func_id))
    }
    fn emit_residual_inline_call(
        &mut self,
        inst: &Instruction,
        arguments: &[(Value, bool)],
    ) -> Result<(), JitError> {
        let target_func_id = inst.static_call_func_id();
        let target_func = self
            .core
            .vo_module
            .functions
            .get(target_func_id as usize)
            .ok_or(JitError::FunctionNotFound(target_func_id))?;
        let eligibility = self
            .core
            .entry_eligibility
            .get(target_func_id as usize)
            .copied()
            .ok_or(JitError::FunctionNotFound(target_func_id))?;
        let call_plan = crate::call_helpers::CallPlan::with_eligibility(
            target_func_id,
            usize::from(inst.b),
            target_func,
            eligibility,
        );
        let direct_self = self
            .optimization_plan
            .is_some_and(|plan| plan.direct_self_call(self.core.func_id, target_func_id));
        let direct_native = direct_self.then_some(self.self_native_ref).flatten();
        let recursive_edge = self
            .inline_plan
            .is_recursive_edge(self.core.func_id, target_func_id);
        if direct_native.is_none()
            || !matches!(
                call_plan.route_for_full_function(self.core.func_id),
                crate::call_helpers::CallRoute::DynamicJitTable
                    | crate::call_helpers::CallRoute::PreparedJitTable
            )
        {
            return Err(JitError::Internal(
                "bounded recursive inline requires a stable native self entry".into(),
            ));
        }
        let abi_arguments = arguments
            .iter()
            .map(|&(value, is_float)| {
                if is_float {
                    self.builder
                        .ins()
                        .bitcast(types::I64, MemFlags::new(), value)
                } else {
                    value
                }
            })
            .collect::<Vec<_>>();
        crate::call_helpers::emit_jit_call_with_explicit_arguments(
            self,
            call_plan,
            direct_native,
            recursive_edge,
            &abi_arguments,
        )
    }
}

impl crate::translator::StackRefresh for FunctionCompiler<'_> {
    fn refresh_stack_base_after_reallocation(&mut self) {}
}
