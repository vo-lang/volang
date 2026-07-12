#![allow(clippy::result_large_err, clippy::too_many_arguments)]
//! Loop compiler for OSR (On-Stack Replacement).

use std::collections::{BTreeMap, HashMap, HashSet};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, FuncRef, Function, InstBuilder, MemFlags, Value};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use crate::loop_analysis::LoopInfo;
use crate::translate::translate_inst;
use crate::translator::{HelperFuncs, SlotAccess, TranslateResult};
use crate::{JitCompileEnv, JitError};
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::{JitContext, JitResult};

/// Loop function signature. Returns JitResult like function JIT.
/// On Ok, loop_exit_pc in JitContext contains the PC to resume at.
pub type LoopFunc = extern "C" fn(*mut JitContext, *mut u64) -> JitResult;

pub struct CompiledLoop {
    pub(crate) code_ptr: *const u8,
    pub(crate) code_size: usize,
    pub(crate) loop_info: LoopInfo,
}

impl CompiledLoop {
    pub fn code_size(&self) -> usize {
        self.code_size
    }

    pub fn loop_info(&self) -> &LoopInfo {
        &self.loop_info
    }
}

unsafe impl Send for CompiledLoop {}
unsafe impl Sync for CompiledLoop {}

pub struct LoopCompiler<'a> {
    builder: FunctionBuilder<'a>,
    func_id: u32,
    func_def: &'a FunctionDef,
    vo_module: &'a VoModule,
    env: JitCompileEnv<'a>,
    loop_info: &'a LoopInfo,
    vars: Vec<Variable>,
    blocks: HashMap<usize, Block>,
    entry_block: Block,
    exit_block: Block,
    current_pc: usize,
    locals_ptr_var: Variable,
    execution_budget_regions: BTreeMap<usize, u32>,
    ctx_ptr: Value,
    helpers: HelperFuncs,
    reg_consts: HashMap<u16, i64>,
    reg_const_facts: Vec<HashMap<u16, i64>>,
    /// FuncRef table for already-compiled callees. Indexed by func_id.
    callee_func_refs: &'a [Option<FuncRef>],
    checked_non_nil: HashSet<u16>,
    /// Slots >= this value must use memory reads (may be aliased by SlotSet/SlotSetN).
    memory_only_start: u16,
}

impl<'a> LoopCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        env: JitCompileEnv<'a>,
        loop_info: &'a LoopInfo,
        helpers: HelperFuncs,
        callee_func_refs: &'a [Option<FuncRef>],
    ) -> Self {
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        let exit_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        let local_slots = func_def.local_slots as u32;

        Self {
            builder,
            func_id,
            func_def,
            vo_module,
            env,
            loop_info,
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            exit_block,
            current_pc: 0,
            locals_ptr_var: Variable::from_u32(local_slots + 1000),
            execution_budget_regions: BTreeMap::new(),
            ctx_ptr: Value::from_u32(0),
            helpers,
            reg_consts: HashMap::new(),
            reg_const_facts: Vec::new(),
            callee_func_refs,
            checked_non_nil: HashSet::new(),
            memory_only_start: u16::MAX,
        }
    }

    pub fn compile(mut self) -> Result<(), JitError> {
        let analysis = crate::analysis::FunctionAnalysis::for_range(
            self.func_def,
            self.vo_module,
            self.loop_info.begin_pc,
            self.loop_info.end_pc + 1,
        )?;
        self.memory_only_start = analysis.memory_only_start;
        self.reg_const_facts = analysis.reg_const_facts;
        self.declare_variables();
        self.scan_jump_targets()?;

        // Exactly like func_compiler: entry_block -> prologue -> sequential compile
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

    fn scan_jump_targets(&mut self) -> Result<(), JitError> {
        let policy = crate::compile_common::ControlPolicy::loop_osr(
            self.loop_info.begin_pc,
            self.loop_info.end_pc,
            self.loop_info.exit_pc,
            self.func_def.code.len(),
        );
        let regions = crate::compile_common::execution_budget_regions(&self.func_def.code, policy)?;
        for start in regions.keys().copied() {
            self.ensure_block(start);
        }
        self.execution_budget_regions = regions;
        Ok(())
    }

    fn ensure_block(&mut self, pc: usize) -> Block {
        if let Some(&block) = self.blocks.get(&pc) {
            block
        } else {
            let block = self.builder.create_block();
            self.blocks.insert(pc, block);
            block
        }
    }

    fn block_for_pc(&self, pc: usize, context: &'static str) -> Result<Block, JitError> {
        self.blocks.get(&pc).copied().ok_or_else(|| {
            JitError::Internal(format!(
                "missing OSR JIT basic block for {context} target pc {pc}"
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

    fn emit_prologue(&mut self) {
        // entry_block has no predecessors
        self.builder.seal_block(self.entry_block);

        let params = self.builder.block_params(self.entry_block);
        self.ctx_ptr = params[0];
        let locals_ptr_init = params[1];
        let current_func_id = self
            .builder
            .ins()
            .iconst(types::I32, i64::from(self.func_id));
        self.builder.ins().store(
            MemFlags::trusted(),
            current_func_id,
            self.ctx_ptr,
            JitContext::OFFSET_CURRENT_FUNC_ID,
        );

        // Wrap locals_ptr in a Variable so refresh_stack_base_after_reallocation can redefine
        // it after any call that may have triggered fiber.stack reallocation.
        self.builder.declare_var(self.locals_ptr_var, types::I64);
        self.builder.def_var(self.locals_ptr_var, locals_ptr_init);

        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .reload_all_from_memory(&mut self.builder, locals_ptr_init);
    }

    fn store_vars_to_memory(&mut self) {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
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
        let poll = crate::compile_common::branch_on_execution_budget(
            &mut self.builder,
            self.ctx_ptr,
            cost,
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
            Opcode::ForLoop => self.forloop(inst),
            Opcode::Return => {
                self.ret(inst);
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
                        current_pc: self.current_pc,
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
            other => Err(JitError::UnsupportedOpcode(other)),
        }
    }

    fn jump(&mut self, inst: &Instruction) -> Result<(), JitError> {
        let raw_target =
            self.checked_branch_target(self.current_pc, inst.imm32(), inst.opcode())?;
        let loop_end = self.loop_info.end_pc + 1;

        // Back-edge: jump to loop header (begin_pc = loop_start)
        if raw_target == self.loop_info.begin_pc {
            let loop_header = self.block_for_pc(self.loop_info.begin_pc, "loop header")?;
            self.builder.ins().jump(loop_header, &[]);
        } else if raw_target < self.loop_info.begin_pc || raw_target >= loop_end {
            // Jump outside loop - exit to VM
            self.store_vars_to_memory();
            self.emit_loop_exit(raw_target as u32);
        } else {
            // Jump within loop body
            let block = self.block_for_pc(raw_target, "jump")?;
            self.builder.ins().jump(block, &[]);
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
        let target = self.checked_branch_target(self.current_pc, inst.imm32(), inst.opcode())?;

        let fall_through = self.builder.create_block();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(cmp_cond, cond, zero);

        if target < self.loop_info.begin_pc || target > self.loop_info.end_pc {
            // Target outside loop - exit to VM
            let exit_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(cmp, exit_block, &[], fall_through, &[]);
            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
            self.store_vars_to_memory();
            self.emit_loop_exit(target as u32);
        } else {
            // Target within loop - stay in JIT
            let target_block = self.block_for_pc(target, "conditional jump")?;
            self.builder
                .ins()
                .brif(cmp, target_block, &[], fall_through, &[]);
        }

        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
        self.clear_flow_facts();
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

        let target = self.checked_forloop_target(self.current_pc, inst)?;
        let target_block = self.block_for_pc(target, "forloop")?;
        let exit_pc = self.current_pc + 1;

        // Check if exit_pc is within JIT compilation range
        if exit_pc >= self.loop_info.begin_pc && exit_pc <= self.loop_info.end_pc {
            // Exit within loop - continue in JIT
            let fall_through = self.builder.create_block();
            self.builder
                .ins()
                .brif(continue_loop, target_block, &[], fall_through, &[]);
            self.builder.switch_to_block(fall_through);
            self.builder.seal_block(fall_through);
            self.clear_flow_facts();
            Ok(false)
        } else {
            // Exit outside loop - return to VM
            let exit_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(continue_loop, target_block, &[], exit_block, &[]);
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
        self.emit_loop_exit(self.current_pc as u32);
    }

    /// Emit code to exit loop normally with given exit_pc.
    /// Stores exit_pc to ctx.loop_exit_pc and returns JitResult::Ok.
    fn emit_loop_exit(&mut self, exit_pc: u32) {
        let ctx = self.ctx_ptr;
        let exit_pc_val = self.builder.ins().iconst(types::I32, exit_pc as i64);
        self.builder.ins().store(
            MemFlags::trusted(),
            exit_pc_val,
            ctx,
            JitContext::OFFSET_LOOP_EXIT_PC,
        );
        let ok_val = self.builder.ins().iconst(types::I32, JitResult::Ok as i64);
        self.builder.ins().return_(&[ok_val]);
    }

    fn panic(&mut self, inst: &Instruction) {
        crate::contract::emit_user_panic_return(self, inst.a);
    }

    /// Returns true if block is terminated.
    /// JIT-to-JIT direct calls with VM call materialization when needed.
    fn call(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        let func_id = inst.static_call_func_id();
        let arg_start = inst.b as usize;

        let target_func = self
            .vo_module
            .functions
            .get(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        let callee_func_ref = self
            .callee_func_refs
            .get(func_id as usize)
            .copied()
            .flatten();
        let call_plan =
            crate::call_helpers::CallPlan::new(func_id, arg_start, target_func, callee_func_ref);

        match call_plan.route_for_loop() {
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
                "static loop call selected dynamic inline-cache route".into(),
            )),
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
            self.func_def.code.len(),
        )
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
            if let Some(cost) = self.execution_budget_regions.get(&pc).copied() {
                self.emit_execution_budget_checkpoint(pc, cost);
            }
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
            .ok_or(JitError::InvalidOsrTarget(pc))
    }

    fn should_skip_instruction(&self, inst: &Instruction) -> bool {
        inst.opcode() == Opcode::Hint
    }

    fn translate_pc_instruction(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        self.translate_instruction(inst)
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
}

impl<'a> crate::translator::SlotAccess<'a> for LoopCompiler<'a> {
    fn read_var(&mut self, slot: u16) -> Value {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .load_i64(&mut self.builder, locals_ptr, slot)
    }
    fn write_var(&mut self, slot: u16, val: Value) {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .store_i64(&mut self.builder, locals_ptr, slot, val);
        self.checked_non_nil.remove(&slot);
        self.reg_consts.remove(&slot);
    }
    fn var_addr(&mut self, slot: u16) -> Value {
        let offset = (slot as i64) * 8;
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        self.builder.ins().iadd_imm(locals_ptr, offset)
    }
    fn sync_slots_to_memory(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError> {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .sync_ssa_slots_to_memory(
            &mut self.builder,
            locals_ptr,
            start_slot,
            slot_count,
            "memory sync",
        )
    }
    fn local_slot_count(&self) -> usize {
        self.vars.len()
    }
    fn read_var_f64(&mut self, slot: u16) -> Value {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .load_f64(&mut self.builder, locals_ptr, slot)
    }
    fn write_var_f64(&mut self, slot: u16, val: Value) {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .store_f64(&mut self.builder, locals_ptr, slot, val);
        self.checked_non_nil.remove(&slot);
        self.reg_consts.remove(&slot);
    }
    fn reload_all_vars_from_memory(&mut self) {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
        )
        .reload_all_from_memory(&mut self.builder, locals_ptr);
        self.clear_flow_facts();
    }
    fn sync_written_slots(&mut self, start_slot: u16, slot_count: u16) -> Result<(), JitError> {
        let locals_ptr = self.builder.use_var(self.locals_ptr_var);
        let slots = crate::compile_common::CompilerStorage::for_function(
            self.func_def,
            &self.vars,
            self.memory_only_start,
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
    fn gc_ptr(&mut self) -> Value {
        self.builder
            .ins()
            .load(types::I64, MemFlags::trusted(), self.ctx_ptr, 0)
    }
    fn globals_ptr(&mut self) -> Value {
        self.builder
            .ins()
            .load(types::I64, MemFlags::trusted(), self.ctx_ptr, 8)
    }
}

impl crate::translator::MetadataAccess for LoopCompiler<'_> {
    fn vo_module(&self) -> &VoModule {
        self.vo_module
    }

    fn resolved_extern(
        &self,
        extern_id: u32,
    ) -> Result<&vo_runtime::bytecode::ResolvedExtern, JitError> {
        let resolved = self.env.externs.get(extern_id).ok_or_else(|| {
            JitError::Internal(format!("CallExtern missing resolved extern {extern_id}"))
        })?;
        if matches!(
            resolved.jit_route,
            vo_runtime::bytecode::ExternJitRoute::DirectHelper
        ) && !self.env.backend_caps.extern_suspend
            && !resolved.effective_effects.is_empty()
        {
            return Err(JitError::Internal(format!(
                "CallExtern extern {extern_id} requires extern suspend support"
            )));
        }
        Ok(resolved)
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

impl crate::translator::HelperAccess for LoopCompiler<'_> {
    fn helpers(&self) -> &HelperFuncs {
        &self.helpers
    }
}

impl crate::translator::RegConstAccess for LoopCompiler<'_> {
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

impl crate::translator::FrameBoundary for LoopCompiler<'_> {
    fn panic_return_value(&self) -> i32 {
        JitResult::Panic as i32
    }
    fn spill_all_vars(&mut self) {
        self.emit_variable_spill();
    }
}

impl<'a> crate::translator::SelectSync<'a> for LoopCompiler<'a> {}

impl crate::translator::FlowFacts for LoopCompiler<'_> {
    fn is_checked_non_nil(&self, slot: u16) -> bool {
        self.checked_non_nil.contains(&slot)
    }
    fn mark_checked_non_nil(&mut self, slot: u16) {
        self.checked_non_nil.insert(slot);
    }
}

impl<'a> crate::translator::CallBoundary<'a> for LoopCompiler<'a> {
    fn call_caller_bp(&mut self) -> Value {
        self.builder.ins().load(
            types::I32,
            MemFlags::trusted(),
            self.ctx_ptr,
            JitContext::OFFSET_JIT_BP,
        )
    }
    fn call_old_fiber_sp(&mut self) -> Value {
        self.builder.ins().load(
            types::I32,
            MemFlags::trusted(),
            self.ctx_ptr,
            JitContext::OFFSET_FIBER_SP,
        )
    }
}

impl crate::translator::StackRefresh for LoopCompiler<'_> {
    fn refresh_stack_base_after_reallocation(&mut self) {
        let stack_ptr = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            self.ctx_ptr,
            JitContext::OFFSET_STACK_PTR,
        );
        let jit_bp_i32 = self.builder.ins().load(
            types::I32,
            MemFlags::trusted(),
            self.ctx_ptr,
            JitContext::OFFSET_JIT_BP,
        );
        let jit_bp_i64 = self.builder.ins().uextend(types::I64, jit_bp_i32);
        let bp_offset = self.builder.ins().imul_imm(jit_bp_i64, 8);
        let refreshed = self.builder.ins().iadd(stack_ptr, bp_offset);
        self.builder.def_var(self.locals_ptr_var, refreshed);
    }
}
