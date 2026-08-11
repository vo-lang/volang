use std::collections::{BTreeMap, HashMap, HashSet};

use cranelift_codegen::ir::Block;
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::{FunctionDef, Module};
use vo_runtime::instruction::{Instruction, Opcode};

use crate::analysis::FunctionAnalysis;
use crate::translator::{HelperRefs, JitMemoryFlags, NativeScratchSlots};
use crate::{JitCompileEnv, JitError};

/// State shared by full-function and loop-OSR compilation.
pub(crate) struct CompilerCore<'a> {
    pub(crate) func_id: u32,
    pub(crate) func_def: &'a FunctionDef,
    pub(crate) vo_module: &'a Module,
    pub(crate) env: JitCompileEnv<'a>,
    pub(crate) vars: Vec<Variable>,
    pub(crate) blocks: HashMap<usize, Block>,
    pub(crate) entry_block: Block,
    pub(crate) current_pc: usize,
    pub(crate) helpers: HelperRefs<'a>,
    pub(crate) reg_consts: HashMap<u16, i64>,
    reg_const_facts: &'a [Box<[(u16, i64)]>],
    pub(crate) execution_budget_regions: BTreeMap<usize, u32>,
    pub(crate) checked_non_nil: HashSet<u16>,
    pub(crate) memory_only_start: u16,
    pub(crate) native_scratch_slots: NativeScratchSlots,
    pub(crate) jit_memory_flags: JitMemoryFlags,
    pub(crate) analysis: &'a FunctionAnalysis,
}

impl<'a> CompilerCore<'a> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a Module,
        env: JitCompileEnv<'a>,
        entry_block: Block,
        helpers: HelperRefs<'a>,
        analysis: &'a FunctionAnalysis,
        memory_only_start: u16,
        jit_memory_flags: JitMemoryFlags,
    ) -> Self {
        Self {
            func_id,
            func_def,
            vo_module,
            env,
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            current_pc: 0,
            helpers,
            reg_consts: HashMap::new(),
            reg_const_facts: &analysis.reg_const_facts,
            execution_budget_regions: BTreeMap::new(),
            checked_non_nil: HashSet::new(),
            memory_only_start: super::bounded_memory_only_start(memory_only_start),
            native_scratch_slots: NativeScratchSlots::default(),
            jit_memory_flags,
            analysis,
        }
    }

    pub(crate) fn declare_variables(&mut self, builder: &mut FunctionBuilder<'_>) {
        self.vars = super::declare_variables(builder, self.func_def, self.memory_only_start);
    }

    #[inline]
    pub(crate) fn is_float_slot(&self, slot: u16) -> bool {
        super::is_float_slot(self.func_def, slot)
    }

    pub(crate) fn block_for_pc(&self, pc: usize, context: &'static str) -> Result<Block, JitError> {
        self.blocks.get(&pc).copied().ok_or_else(|| {
            JitError::Internal(format!(
                "missing JIT basic block for {context} target pc {pc}"
            ))
        })
    }

    pub(crate) fn checked_branch_target(
        &self,
        pc: usize,
        offset: i32,
        opcode: Opcode,
    ) -> Result<usize, JitError> {
        super::checked_branch_target(self.func_def.code.len(), pc, offset, opcode)
    }

    pub(crate) fn checked_forloop_target(
        &self,
        pc: usize,
        inst: &Instruction,
    ) -> Result<usize, JitError> {
        super::checked_forloop_target(self.func_def.code.len(), pc, inst)
    }

    pub(crate) fn clear_flow_facts(&mut self) {
        super::clear_flow_facts(&mut self.checked_non_nil, &mut self.reg_consts);
    }

    pub(crate) fn apply_reg_const_facts(&mut self, pc: usize) -> Result<(), JitError> {
        super::apply_reg_const_facts(&mut self.reg_consts, self.reg_const_facts, pc)
    }
}
