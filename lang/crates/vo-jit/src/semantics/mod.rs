//! Global opcode semantic matrix for JIT correctness checks.
//!
//! This is intentionally close to the opcode list instead of being inferred from
//! scattered lowering code. Adding or changing an opcode should force one
//! explicit update here.

mod rows;
#[cfg(test)]
mod tests;
mod types;

#[cfg(test)]
pub(crate) use rows::opcode_capability_contract;
#[cfg(test)]
pub use rows::opcode_semantic_rows;
#[cfg(test)]
pub use rows::{opcode_semantic_matrix, opcode_semantics};
