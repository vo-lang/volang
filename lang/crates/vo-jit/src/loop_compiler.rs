#![allow(clippy::result_large_err, clippy::too_many_arguments)]
//! Loop compiler for OSR (On-Stack Replacement).

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, Function, InstBuilder, MemFlagsData as MemFlags, Value};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use crate::loop_analysis::LoopInfo;
use crate::translate::translate_inst;
use crate::translator::{HelperKind, HelperRefs, RuntimeContext as _, SlotAccess, TranslateResult};
use crate::{analysis::FunctionAnalysis, JitCompileEnv, JitError};
use vo_runtime::bytecode::{FunctionDef, InstructionMetadata, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::{JitContext, JitContextField, JitResult};

/// Loop function signature. Returns JitResult like function JIT.
/// On Ok, loop_exit_pc in JitContext contains the PC to resume at.
pub type LoopFunc = extern "C" fn(*mut JitContext, *mut u64) -> JitResult;

pub struct CompiledLoop {
    pub(crate) code_ptr: *const u8,
    pub(crate) loop_info: LoopInfo,
    pub(crate) metadata: std::sync::Arc<crate::JitArtifactMetadata>,
}

unsafe impl Send for CompiledLoop {}
unsafe impl Sync for CompiledLoop {}

pub struct LoopCompiler<'a> {
    builder: FunctionBuilder<'a>,
    core: crate::compile_common::CompilerCore<'a>,
    loop_info: &'a LoopInfo,
    exit_block: Block,
    locals_ptr_var: Variable,
    ctx_ptr: Value,
    instruction_optimization: &'a crate::optimizer::OptimizedFunction,
    optimization_plan: &'a crate::optimizer::ModuleOptimizationPlan,
}

impl<'a> LoopCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        env: JitCompileEnv<'a>,
        entry_eligibility: &'a [crate::JitFrameEntryEligibility],
        loop_info: &'a LoopInfo,
        helpers: HelperRefs<'a>,
        analysis: &'a FunctionAnalysis,
        instruction_optimization: &'a crate::optimizer::OptimizedFunction,
        optimization_plan: &'a crate::optimizer::ModuleOptimizationPlan,
    ) -> Result<Self, JitError> {
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        let exit_block = crate::compile_common::cold_block(&mut builder);
        builder.append_block_params_for_function_params(entry_block);

        let locals_ptr_var = builder.declare_var(types::I64);
        let jit_memory_flags = crate::translator::JitMemoryFlags::new(&mut builder);

        let memory_only_start = analysis.memory_only_start_for_loop(func_def, loop_info)?;
        Ok(Self {
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
                memory_only_start,
                jit_memory_flags,
            ),
            loop_info,
            exit_block,
            locals_ptr_var,
            ctx_ptr: Value::from_u32(0),
            instruction_optimization,
            optimization_plan,
        })
    }

    pub fn compile(mut self, frontend_config: TargetFrontendConfig) -> Result<(), JitError> {
        self.core.declare_variables(&mut self.builder);
        let policy = crate::compile_common::ControlPolicy::loop_osr(
            self.loop_info.begin_pc,
            self.loop_info.end_pc,
            self.loop_info.exit_pc,
            self.core.func_def.code.len(),
        );
        self.core.execution_budget_regions = crate::compile_common::prepare_control_flow(
            &mut self.builder,
            &mut self.core.blocks,
            self.core.analysis.ir(),
            policy,
            self.core.memory_only_start,
            true,
            Some(self.instruction_optimization),
        )?;

        // Exactly like func_compiler: entry_block -> prologue -> sequential compile
        self.builder.switch_to_block(self.core.entry_block);
        self.emit_prologue();
        crate::compile_common::drive_compile(&mut self)?;

        self.builder.seal_all_blocks();
        self.builder.finalize(frontend_config);

        Ok(())
    }

    fn emit_prologue(&mut self) {
        // entry_block has no predecessors
        self.builder.seal_block(self.core.entry_block);

        let params = self.builder.block_params(self.core.entry_block);
        self.ctx_ptr = params[0];
        let locals_ptr_init = params[1];
        let current_func_id = self
            .builder
            .ins()
            .iconst(types::I32, i64::from(self.core.func_id));
        self.store_context_field(current_func_id, JitContextField::CurrentFuncId);

        // Wrap locals_ptr in a Variable so refresh_stack_base_after_reallocation can redefine
        // it after any call that may have triggered fiber.stack reallocation.
        self.builder.def_var(self.locals_ptr_var, locals_ptr_init);

        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .reload_all_from_memory(&mut self.builder, locals_ptr_init);
    }

    fn store_vars_to_memory(&mut self) {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .spill_ssa_prefix_to_memory(&mut self.builder, locals_ptr);
    }

    fn emit_cooperative_yield(&mut self, resume_pc: usize) {
        self.store_vars_to_memory();
        crate::compile_common::emit_cooperative_yield_return(
            &mut self.builder,
            self.ctx_ptr,
            resume_pc,
        );
    }

    fn emit_execution_budget_checkpoint(&mut self, resume_pc: usize, cost: u32) {
        let refill = self
            .core
            .helpers
            .resolve(HelperKind::refill_execution_budget, self.builder.func)
            .func_ref();
        let poll = crate::compile_common::branch_on_execution_budget(
            &mut self.builder,
            self.ctx_ptr,
            cost,
            refill,
        );

        self.builder.switch_to_block(poll.exhausted);
        self.builder.seal_block(poll.exhausted);
        self.emit_cooperative_yield(resume_pc);

        crate::compile_common::continue_after_execution_budget_poll(
            &mut self.builder,
            self.ctx_ptr,
            &poll,
        );
    }

    fn translate_instruction(
        &mut self,
        instruction: crate::ir::TypedInstruction,
        optimized: crate::optimizer::OptimizedInstruction,
    ) -> Result<bool, JitError> {
        let source = instruction.source();
        let inst = &source;
        if optimized.action == crate::optimizer::LoweringAction::AlwaysJump {
            debug_assert!(matches!(inst.opcode(), Opcode::JumpIf | Opcode::JumpIfNot));
            self.jump(inst)?;
            return Ok(true);
        }
        if let crate::optimizer::LoweringAction::Replace(replacement) = optimized.action {
            let value = self.core.lowered_value(replacement).ok_or_else(|| {
                JitError::Internal(format!(
                    "OSR GVN replacement value {} is unavailable at pc {}",
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
                        "OSR GVN replacement at pc {} has no output",
                        self.core.current_pc
                    ))
                })?;
            let output = self.core.analysis.ir().value(output);
            match output.ty {
                crate::ir::ValueType::Float64 => self.write_var_f64(output.slot, value),
                _ => self.write_var(output.slot, value),
            }
            return Ok(false);
        }
        match translate_inst(self, instruction)? {
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
            Opcode::ForLoop => self.forloop(inst),
            Opcode::Return => {
                self.ret(inst);
                Ok(true)
            }
            Opcode::Panic => {
                self.panic(inst);
                Ok(true)
            }
            Opcode::Call => self.call(inst, optimized.inline_target()),
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
                if self.try_inline_dynamic_call(inst, optimized.inline_target())? {
                    return Ok(false);
                }
                crate::call_helpers::emit_call_closure(self, inst)?;
                Ok(false)
            }
            Opcode::CallIface => {
                if self.try_inline_dynamic_call(inst, optimized.inline_target())? {
                    return Ok(false);
                }
                crate::call_helpers::emit_call_iface(self, inst)?;
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
            .pure_leaf_inline(self.core.func_id, target)
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
            Opcode::CallClosure => self.read_var(inst.a),
            Opcode::CallIface => self.read_var(inst.a + 1),
            _ => unreachable!("dynamic inline was filtered by opcode"),
        };
        let arg_start = usize::from(inst.b);
        inline.emit_dynamic(self, slot0, arg_start, arg_start + arg_slots);
        Ok(true)
    }

    fn jump(&mut self, inst: &Instruction) -> Result<(), JitError> {
        let raw_target =
            self.core
                .checked_branch_target(self.core.current_pc, inst.imm32(), inst.opcode())?;
        let loop_end = self.loop_info.end_pc + 1;

        // Back-edge: jump to loop header (begin_pc = loop_start)
        if raw_target == self.loop_info.begin_pc {
            let loop_header = self
                .core
                .block_for_pc(self.loop_info.begin_pc, "loop header")?;
            let arguments = crate::compile_common::block_arguments(
                &mut self.builder,
                self.core.analysis.ir(),
                &self.core.vars,
                self.core.memory_only_start,
                self.loop_info.begin_pc,
            );
            self.builder.ins().jump(loop_header, &arguments);
        } else if raw_target < self.loop_info.begin_pc || raw_target >= loop_end {
            // Jump outside loop - exit to VM
            self.store_vars_to_memory();
            self.emit_loop_exit(raw_target as u32);
        } else {
            // Jump within loop body
            let block = self.core.block_for_pc(raw_target, "jump")?;
            let arguments = crate::compile_common::block_arguments(
                &mut self.builder,
                self.core.analysis.ir(),
                &self.core.vars,
                self.core.memory_only_start,
                raw_target,
            );
            self.builder.ins().jump(block, &arguments);
        }
        Ok(())
    }

    fn jump_if(&mut self, inst: &Instruction) -> Result<(), JitError> {
        self.conditional_jump(inst, IntCC::NotEqual)
    }

    fn jump_if_not(&mut self, inst: &Instruction) -> Result<(), JitError> {
        self.conditional_jump(inst, IntCC::Equal)
    }

    fn conditional_jump(&mut self, inst: &Instruction, cmp_cond: IntCC) -> Result<(), JitError> {
        let cond = self.read_var(inst.a);
        let target =
            self.core
                .checked_branch_target(self.core.current_pc, inst.imm32(), inst.opcode())?;

        let fall_through = self.builder.create_block();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(cmp_cond, cond, zero);

        if target < self.loop_info.begin_pc || target > self.loop_info.end_pc {
            // Target outside loop - exit to VM
            let exit_block = crate::compile_common::cold_block(&mut self.builder);
            self.builder
                .ins()
                .brif(cmp, exit_block, &[], fall_through, &[]);
            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
            self.store_vars_to_memory();
            self.emit_loop_exit(target as u32);
        } else {
            // Target within loop - stay in JIT
            let target_block = self.core.block_for_pc(target, "conditional jump")?;
            let target_arguments = crate::compile_common::block_arguments(
                &mut self.builder,
                self.core.analysis.ir(),
                &self.core.vars,
                self.core.memory_only_start,
                target,
            );
            self.builder
                .ins()
                .brif(cmp, target_block, &target_arguments, fall_through, &[]);
        }

        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
        self.core.clear_flow_facts();
        Ok(())
    }

    /// Returns true if block is terminated (exit to VM), false if fall-through continues in JIT
    fn forloop(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        let idx = self.read_var(inst.a);
        let limit = self.read_var(inst.b);
        let (is_decrement, is_unsigned, is_inclusive) = inst.forloop_flags();

        let (next_idx, continue_loop) = crate::translate::emit_forloop_step(
            &mut self.builder,
            idx,
            limit,
            is_decrement,
            is_unsigned,
            is_inclusive,
        );
        self.write_var(inst.a, next_idx);

        let target = self
            .core
            .checked_forloop_target(self.core.current_pc, inst)?;
        let target_block = self.core.block_for_pc(target, "forloop")?;
        let target_arguments = crate::compile_common::block_arguments(
            &mut self.builder,
            self.core.analysis.ir(),
            &self.core.vars,
            self.core.memory_only_start,
            target,
        );
        let exit_pc = self.core.current_pc + 1;

        // Check if exit_pc is within JIT compilation range
        if exit_pc >= self.loop_info.begin_pc && exit_pc <= self.loop_info.end_pc {
            // Exit within loop - continue in JIT
            let fall_through = self.builder.create_block();
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
            Ok(false)
        } else {
            // Exit outside loop - return to VM
            let exit_block = crate::compile_common::cold_block(&mut self.builder);
            self.builder.ins().brif(
                continue_loop,
                target_block,
                &target_arguments,
                exit_block,
                &[],
            );
            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
            self.store_vars_to_memory();
            self.emit_loop_exit(exit_pc as u32);
            Ok(true)
        }
    }

    fn ret(&mut self, _inst: &Instruction) {
        // Return inside loop - store vars and return to VM
        self.store_vars_to_memory();
        self.emit_loop_exit(self.core.current_pc as u32);
    }

    /// Emit code to exit loop normally with given exit_pc.
    /// Stores exit_pc to ctx.loop_exit_pc and returns JitResult::Ok.
    fn emit_loop_exit(&mut self, exit_pc: u32) {
        let exit_pc_val = self.builder.ins().iconst(types::I32, exit_pc as i64);
        self.store_context_field(exit_pc_val, JitContextField::LoopExitPc);
        let ok_val = self.builder.ins().iconst(types::I32, JitResult::Ok as i64);
        self.builder.ins().return_(&[ok_val]);
    }

    fn panic(&mut self, inst: &Instruction) {
        crate::contract::emit_user_panic_return(self, inst.a);
    }

    /// Returns true if block is terminated.
    /// JIT-to-JIT direct calls with VM call materialization when needed.
    fn call(&mut self, inst: &Instruction, inline_target: Option<u32>) -> Result<bool, JitError> {
        let func_id = inst.static_call_func_id();
        let arg_start = inst.b as usize;

        let target_func = self
            .core
            .vo_module
            .functions
            .get(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        let eligibility = self
            .core
            .entry_eligibility
            .get(func_id as usize)
            .copied()
            .ok_or(JitError::FunctionNotFound(func_id))?;
        let call_plan = crate::call_helpers::CallPlan::with_eligibility(
            func_id,
            arg_start,
            target_func,
            eligibility,
        );
        let planned_inline = inline_target
            .filter(|target| *target == func_id)
            .and_then(|_| {
                self.optimization_plan
                    .pure_leaf_inline(self.core.func_id, func_id)
            });
        if let Some(inline) = planned_inline {
            inline.emit(self, call_plan.arg_start);
            return Ok(false);
        }

        match call_plan.route_for_loop() {
            crate::call_helpers::CallRoute::DynamicJitTable => {
                crate::call_helpers::emit_jit_call_with_vm_materialization(self, call_plan, None)?;
                Ok(false)
            }
            crate::call_helpers::CallRoute::PreparedJitTable => {
                crate::call_helpers::emit_jit_call_with_vm_materialization(self, call_plan, None)?;
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

    /// Emit code to spill all SSA variables to fiber.stack.
    /// Called before returning Call so VM can see/restore state.
    fn emit_variable_spill(&mut self) {
        self.store_vars_to_memory();
    }
}

impl<'a> crate::compile_common::CompileDriver for LoopCompiler<'a> {
    fn control_policy(&self) -> crate::compile_common::ControlPolicy {
        crate::compile_common::ControlPolicy::loop_osr(
            self.loop_info.begin_pc,
            self.loop_info.end_pc,
            self.loop_info.exit_pc,
            self.core.func_def.code.len(),
        )
    }

    fn is_pc_executable(&self, pc: usize) -> bool {
        self.instruction_optimization.is_executable(pc)
    }

    fn set_current_pc(&mut self, pc: usize) {
        self.core.current_pc = pc;
        self.core.current_bounds_check_elided = self
            .instruction_optimization
            .instruction(pc)
            .is_some_and(|node| node.bounds_check_elided);
        self.core.current_nil_check_elided = self
            .instruction_optimization
            .instruction(pc)
            .is_some_and(|node| node.nil_check_elided);
    }

    fn enter_pc_block(&mut self, pc: usize, block_terminated: &mut bool) -> Result<(), JitError> {
        if crate::compile_common::enter_compile_pc(
            &mut self.builder,
            crate::compile_common::CompileBlockView {
                blocks: &self.core.blocks,
                ir: self.core.analysis.ir(),
                vars: &self.core.vars,
                memory_only_start: self.core.memory_only_start,
                executable_only: true,
                optimized: Some(self.instruction_optimization),
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

    fn instruction_for_pc(
        &self,
        pc: usize,
    ) -> Result<crate::compile_common::LoweringInstruction, JitError> {
        self.instruction_optimization
            .instruction(pc)
            .map(crate::compile_common::LoweringInstruction::Optimized)
            .ok_or(JitError::InvalidOsrTarget(pc))
    }

    fn should_skip_instruction(&self, inst: crate::compile_common::LoweringInstruction) -> bool {
        inst.typed().source().opcode() == Opcode::Hint
            || inst
                .optimized()
                .is_some_and(|node| node.action == crate::optimizer::LoweringAction::Eliminate)
    }

    fn translate_pc_instruction(
        &mut self,
        inst: crate::compile_common::LoweringInstruction,
    ) -> Result<bool, JitError> {
        let optimized = inst.optimized().ok_or_else(|| {
            JitError::Internal("OSR lowering received a baseline instruction".to_string())
        })?;
        self.translate_instruction(inst.typed(), optimized)
    }

    fn finish_fallthrough(&mut self, block_terminated: bool) -> Result<(), JitError> {
        if !block_terminated {
            self.builder.ins().jump(self.exit_block, &[]);
        }

        self.builder.switch_to_block(self.exit_block);
        self.store_vars_to_memory();
        let crate::compile_common::ControlPolicy::LoopOsr { exit_pc, .. } = self.control_policy()
        else {
            return Err(JitError::Internal(
                "LoopCompiler received a non-OSR control policy".to_string(),
            ));
        };
        self.emit_loop_exit(exit_pc as u32);
        Ok(())
    }
}

impl<'a> crate::translator::IrBuilder<'a> for LoopCompiler<'a> {
    fn builder(&mut self) -> &mut FunctionBuilder<'a> {
        &mut self.builder
    }

    fn jit_memory_flags(&self) -> crate::translator::JitMemoryFlags {
        self.core.jit_memory_flags
    }
}

impl<'a> crate::translator::ScratchAccess<'a> for LoopCompiler<'a> {
    fn native_scratch_slots(&mut self) -> &mut crate::translator::NativeScratchSlots {
        &mut self.core.native_scratch_slots
    }
}

impl<'a> crate::translator::SlotAccess<'a> for LoopCompiler<'a> {
    fn read_var(&mut self, slot: u16) -> Value {
        if slot < self.core.memory_only_start {
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
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .load_i64(&mut self.builder, locals_ptr, slot)
    }
    fn write_var(&mut self, slot: u16, val: Value) {
        let storage = crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        );
        let ir_value = if slot < self.core.memory_only_start {
            storage.store_ssa_i64(&mut self.builder, slot, val)
        } else {
            let locals_ptr = self.builder.use_var(self.locals_ptr_var);
            storage.store_memory_i64(&mut self.builder, locals_ptr, slot, val)
        };
        self.core.record_output_value(slot, ir_value);
        self.core.checked_non_nil.remove(&slot);
    }
    fn var_addr(&mut self, slot: u16) -> Value {
        let offset = (slot as i64) * 8;
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        self.builder.ins().iadd_imm_s(locals_ptr, offset)
    }
    fn local_slot_count(&self) -> usize {
        self.core.func_def.local_slots as usize
    }
    fn read_var_f64(&mut self, slot: u16) -> Value {
        if slot < self.core.memory_only_start {
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
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .load_f64(&mut self.builder, locals_ptr, slot)
    }
    fn write_var_f64(&mut self, slot: u16, val: Value) {
        let storage = crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        );
        let ir_value = if slot < self.core.memory_only_start {
            storage.store_ssa_f64(&mut self.builder, slot, val)
        } else {
            let locals_ptr = self.builder.use_var(self.locals_ptr_var);
            storage.store_memory_f64(&mut self.builder, locals_ptr, slot, val)
        };
        self.core.record_output_value(slot, ir_value);
        self.core.checked_non_nil.remove(&slot);
    }
    fn reload_all_vars_from_memory(&mut self) {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .reload_all_from_memory(&mut self.builder, locals_ptr);
        self.core.clear_flow_facts();
    }
    fn sync_written_slots(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError> {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        let slots = crate::compile_common::CompilerStorage::for_function(
            self.core.func_def,
            &self.core.vars,
            self.core.memory_only_start,
        )
        .load_memory_slot_range(
            &mut self.builder,
            locals_ptr,
            start_slot,
            slot_count,
            "select sync",
        )?;
        for slot in slots {
            if slot.is_float {
                self.write_var_f64(slot.slot, slot.value);
            } else {
                self.write_var(slot.slot, slot.value);
            }
        }
        Ok(())
    }
}

impl<'a> crate::translator::RuntimeContext<'a> for LoopCompiler<'a> {
    fn ctx_param(&mut self) -> Value {
        self.ctx_ptr
    }
}

crate::translator::impl_shared_compiler_traits!(LoopCompiler<'_>);

impl crate::translator::FrameBoundary for LoopCompiler<'_> {
    fn spill_all_vars(&mut self) {
        self.emit_variable_spill();
    }
}

impl<'a> crate::translator::SelectSync<'a> for LoopCompiler<'a> {}

impl<'a> crate::translator::CallBoundary<'a> for LoopCompiler<'a> {
    fn call_caller_bp(&mut self) -> Value {
        self.load_context_field(types::I32, JitContextField::JitBp)
    }
    fn call_old_fiber_sp(&mut self) -> Value {
        self.load_context_field(types::I32, JitContextField::FiberSp)
    }
}

impl crate::translator::StackRefresh for LoopCompiler<'_> {
    fn refresh_stack_base_after_reallocation(&mut self) {
        let stack_ptr = self.load_context_field(types::I64, JitContextField::StackPtr);
        let jit_bp_i32 = self.load_context_field(types::I32, JitContextField::JitBp);
        let jit_bp_i64 = self.builder.ins().uextend(types::I64, jit_bp_i32);
        let bp_offset = self.builder.ins().imul_imm_u(jit_bp_i64, 8);
        let refreshed = self.builder.ins().iadd(stack_ptr, bp_offset);
        self.builder.def_var(self.locals_ptr_var, refreshed);
    }
}
