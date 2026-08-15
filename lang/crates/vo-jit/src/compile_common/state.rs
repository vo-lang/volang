use std::collections::{BTreeMap, HashMap, HashSet};

use cranelift_codegen::ir::Block;
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::{FunctionDef, Module};
use vo_runtime::instruction::{Instruction, Opcode};

use crate::analysis::FunctionAnalysis;
use crate::translator::{HelperRefs, JitMemoryFlags, NativeScratchSlots};
use crate::{JitCompileEnv, JitError, JitFrameEntryEligibility};

/// State shared by full-function and loop-OSR compilation.
pub(crate) struct CompilerCore<'a> {
    pub(crate) func_id: u32,
    pub(crate) func_def: &'a FunctionDef,
    pub(crate) vo_module: &'a Module,
    pub(crate) env: JitCompileEnv<'a>,
    pub(crate) entry_eligibility: &'a [JitFrameEntryEligibility],
    pub(crate) vars: Vec<Variable>,
    pub(crate) blocks: HashMap<usize, Block>,
    pub(crate) entry_block: Block,
    pub(crate) current_pc: usize,
    pub(crate) current_bounds_check_elided: bool,
    pub(crate) current_nil_check_elided: bool,
    pub(crate) helpers: HelperRefs<'a>,
    pub(crate) execution_budget_regions: BTreeMap<usize, u32>,
    pub(crate) checked_non_nil: HashSet<u16>,
    pub(crate) memory_only_start: u16,
    pub(crate) native_scratch_slots: NativeScratchSlots,
    pub(crate) jit_memory_flags: JitMemoryFlags,
    pub(crate) analysis: &'a FunctionAnalysis,
    lowered_values: Vec<Option<cranelift_codegen::ir::Value>>,
}

impl<'a> CompilerCore<'a> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a Module,
        env: JitCompileEnv<'a>,
        entry_eligibility: &'a [JitFrameEntryEligibility],
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
            entry_eligibility,
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            current_pc: 0,
            current_bounds_check_elided: false,
            current_nil_check_elided: false,
            helpers,
            execution_budget_regions: BTreeMap::new(),
            checked_non_nil: HashSet::new(),
            memory_only_start: super::bounded_memory_only_start(memory_only_start),
            native_scratch_slots: NativeScratchSlots::default(),
            jit_memory_flags,
            analysis,
            lowered_values: vec![None; analysis.ir().value_count()],
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
        super::clear_flow_facts(&mut self.checked_non_nil);
    }

    pub(crate) fn apply_ir_facts(&mut self, pc: usize) -> Result<(), JitError> {
        if self.analysis.ir().instruction(pc).is_none() {
            return Err(JitError::Internal(format!(
                "missing SSA instruction facts at pc {pc}"
            )));
        }
        Ok(())
    }

    pub(crate) fn lowered_value_for_slot(&self, slot: u16) -> Option<cranelift_codegen::ir::Value> {
        let ir = self.analysis.ir();
        ir.input_value(self.current_pc, slot)
            .or_else(|| ir.frame_value(self.current_pc, slot))
            .and_then(|value| self.lowered_values[value.index()])
    }

    pub(crate) fn lowered_value(
        &self,
        value: crate::ir::ValueId,
    ) -> Option<cranelift_codegen::ir::Value> {
        self.lowered_values[value.index()]
    }

    pub(crate) fn record_output_value(&mut self, slot: u16, value: cranelift_codegen::ir::Value) {
        if let Some(output) = self.analysis.ir().output_value(self.current_pc, slot) {
            self.lowered_values[output.index()] = Some(value);
        }
    }

    pub(crate) fn bind_ir_block_parameters(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        pc: usize,
        block: Block,
    ) {
        let Some(instruction) = self.analysis.ir().instruction(pc).copied() else {
            return;
        };
        let ir_block = &self.analysis.ir().blocks()[instruction.block().index()];
        if ir_block.start_pc as usize != pc {
            return;
        }
        let parameters = self
            .analysis
            .ir()
            .block_parameters(ir_block.id)
            .iter()
            .filter(|parameter| parameter.slot < self.memory_only_start)
            .copied()
            .collect::<Vec<_>>();
        let values = builder.block_params(block).to_vec();
        for (parameter, value) in parameters.into_iter().zip(values) {
            builder.def_var(self.vars[parameter.slot as usize], value);
            self.lowered_values[parameter.value.index()] = Some(value);
        }
    }
}
