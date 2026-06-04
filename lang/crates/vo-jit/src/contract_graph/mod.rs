//! Machine-readable JIT correctness contract graph.
//!
//! The graph deliberately records producer/consumer edges instead of relying on
//! prose notes. Tests below check that every opcode, packed operand, metadata
//! payload, runtime helper, JitContext callback, GC typed entry, and JIT entry
//! policy is covered by an executable contract row.

mod edge_builders;
mod entry_policy;
mod fields;
mod gc;
mod metadata;
mod opcode;
mod runtime;
mod types;

pub use entry_policy::entry_policy_edges;
pub use gc::gc_contract_edges;
pub use metadata::jit_metadata_contract_edges;
#[cfg(test)]
pub use metadata::legacy_jit_metadata_compat_edges;
pub use opcode::{
    codegen_decoder_pair_edges, opcode_contract_edges, packed_operand_contract_edges,
};
pub use runtime::{
    callback_callsite_contract_edges, callback_contract_edges, runtime_helper_contract_edges,
};
pub use types::*;

pub fn jit_contract_graph() -> Vec<ContractEdge> {
    let mut graph = Vec::new();
    graph.extend(opcode_contract_edges());
    graph.extend(codegen_decoder_pair_edges());
    graph.extend_from_slice(packed_operand_contract_edges());
    graph.extend_from_slice(jit_metadata_contract_edges());
    graph.extend(runtime_helper_contract_edges());
    graph.extend(callback_contract_edges());
    graph.extend(callback_callsite_contract_edges());
    graph.extend_from_slice(gc_contract_edges());
    graph.extend_from_slice(entry_policy_edges());
    graph
}

#[cfg(test)]
mod tests;
