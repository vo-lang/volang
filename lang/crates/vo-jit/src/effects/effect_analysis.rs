use vo_runtime::bytecode::{ExternDef, FunctionDef};
use vo_runtime::instruction::Instruction;

use crate::semantics::opcode_register_effects;

use super::{
    try_memory_sync_effect, try_read_regs_with_module_context, try_write_regs_with_module_context,
    EffectError, EffectFacts, InstructionEffects,
};

#[allow(dead_code)]
pub fn try_instruction_effects_with_facts(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<InstructionEffects, EffectError> {
    try_instruction_effects_with_context(inst, facts, &[])
}

pub fn try_instruction_effects_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> Result<InstructionEffects, EffectError> {
    try_instruction_effects_with_module_context(inst, facts, externs, &[])
}

pub fn try_instruction_effects_with_module_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<InstructionEffects, EffectError> {
    Ok(InstructionEffects {
        reads: try_read_regs_with_module_context(inst, facts, functions)?,
        writes: try_write_regs_with_module_context(inst, facts, externs, functions)?,
        memory_sync: try_memory_sync_effect(inst)?,
        may_call: may_call(inst),
    })
}

pub fn may_call(inst: &Instruction) -> bool {
    opcode_register_effects(inst.opcode()).may_call
}
