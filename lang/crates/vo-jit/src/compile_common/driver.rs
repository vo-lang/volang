use std::collections::HashMap;

use cranelift_codegen::ir::{Block, InstBuilder};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::Instruction;

use crate::JitError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ControlPolicy {
    FullFunction {
        code_len: usize,
    },
    LoopOsr {
        begin_pc: usize,
        end_pc: usize,
        exit_pc: usize,
        code_len: usize,
    },
}

impl ControlPolicy {
    pub(crate) fn full_function(code_len: usize) -> Self {
        Self::FullFunction { code_len }
    }

    pub(crate) fn loop_osr(
        begin_pc: usize,
        end_pc: usize,
        exit_pc: usize,
        code_len: usize,
    ) -> Self {
        Self::LoopOsr {
            begin_pc,
            end_pc,
            exit_pc,
            code_len,
        }
    }

    pub(crate) fn pc_range(self) -> std::ops::Range<usize> {
        match self {
            Self::FullFunction { code_len } => 0..code_len,
            Self::LoopOsr {
                begin_pc, end_pc, ..
            } => begin_pc..(end_pc + 1),
        }
    }

    pub(crate) fn compiled_target(self, target: usize) -> bool {
        match self {
            Self::FullFunction { .. } => true,
            Self::LoopOsr {
                begin_pc, end_pc, ..
            } => target >= begin_pc && target <= end_pc,
        }
    }

    pub(super) fn code_len(self) -> usize {
        match self {
            Self::FullFunction { code_len } | Self::LoopOsr { code_len, .. } => code_len,
        }
    }
}

pub(crate) trait CompileDriver {
    fn control_policy(&self) -> ControlPolicy;
    fn set_current_pc(&mut self, pc: usize);
    fn enter_pc_block(&mut self, pc: usize, block_terminated: &mut bool) -> Result<(), JitError>;
    fn apply_pc_facts(&mut self, pc: usize) -> Result<(), JitError>;
    fn instruction_for_pc(&self, pc: usize) -> Result<Instruction, JitError>;
    fn should_skip_instruction(&self, _inst: &Instruction) -> bool {
        false
    }
    fn translate_pc_instruction(&mut self, inst: &Instruction) -> Result<bool, JitError>;
    fn finish_fallthrough(&mut self, block_terminated: bool) -> Result<(), JitError>;
}

pub(crate) fn drive_compile(driver: &mut impl CompileDriver) -> Result<(), JitError> {
    let policy = driver.control_policy();
    let mut block_terminated = false;

    for pc in policy.pc_range() {
        driver.set_current_pc(pc);
        driver.enter_pc_block(pc, &mut block_terminated)?;
        driver.apply_pc_facts(pc)?;
        let inst = driver.instruction_for_pc(pc)?;
        if driver.should_skip_instruction(&inst) {
            continue;
        }
        block_terminated = driver.translate_pc_instruction(&inst)?;
    }

    driver.finish_fallthrough(block_terminated)
}

pub(crate) fn enter_compile_pc(
    builder: &mut FunctionBuilder<'_>,
    blocks: &HashMap<usize, Block>,
    pc: usize,
    block_terminated: &mut bool,
) -> bool {
    if let Some(&block) = blocks.get(&pc) {
        if !*block_terminated {
            builder.ins().jump(block, &[]);
        }
        builder.switch_to_block(block);
        *block_terminated = false;
        true
    } else if *block_terminated {
        let dummy = builder.create_block();
        builder.switch_to_block(dummy);
        *block_terminated = false;
        true
    } else {
        false
    }
}

pub(crate) fn declare_variables(
    builder: &mut FunctionBuilder<'_>,
    func_def: &FunctionDef,
) -> Vec<Variable> {
    let num_slots = func_def.local_slots as usize;
    let mut vars = Vec::with_capacity(num_slots);
    for i in 0..num_slots {
        let var = Variable::from_u32(i as u32);
        let ty = crate::compile_common::slot_ir_type(&func_def.slot_types, i as u16);
        builder.declare_var(var, ty);
        vars.push(var);
    }
    vars
}
