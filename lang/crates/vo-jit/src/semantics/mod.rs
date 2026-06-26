//! Global opcode semantic matrix for JIT correctness checks.
//!
//! This is intentionally close to the opcode list instead of being inferred from
//! scattered lowering code. Adding or changing an opcode should force one
//! explicit update here plus a metadata-contract update when the opcode consumes
//! per-PC JIT metadata.

use vo_runtime::instruction::Instruction;

pub use crate::metadata_contract::JitMetadataRequirement;

mod register_effects;
mod requirements;
mod rows;
mod runtime_policy;
#[cfg(test)]
mod tests;
mod types;

#[cfg(test)]
pub use rows::opcode_semantic_rows;
pub(crate) use rows::{
    opcode_capability_contract, opcode_effect_contract, opcode_metadata_requirement_from_semantics,
};
pub use rows::{opcode_register_effects, opcode_semantic_matrix, opcode_semantics};
#[cfg(test)]
pub use runtime_policy::{runtime_helper_lowering_descriptors, RuntimeHelperLoweringPolicy};
pub use types::*;

pub fn static_call_target_from_semantics(inst: &Instruction) -> Option<u32> {
    let row = opcode_semantics(inst.opcode());
    row.packed_operands
        .contains(&PackedOperand::StaticCallFuncId)
        .then(|| inst.static_call_func_id())
}
