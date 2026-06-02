//! Verifier for bytecode facts consumed by the JIT.

#![allow(clippy::too_many_arguments)]

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::Opcode;
mod errors;
mod function_invariants;
mod instruction_contracts;
mod metadata_checks;

pub use errors::JitMetadataError;
use function_invariants::verify_function_invariants;
use instruction_contracts::{verify_slot, verify_slot_contract};
use metadata_checks::verify_metadata_kind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ModuleDigest {
    hash: u64,
    serialized_len: usize,
    function_count: usize,
}

impl ModuleDigest {
    fn for_module(vo_module: &VoModule) -> Self {
        let bytes = vo_module.serialize();
        let mut hasher = DefaultHasher::new();
        bytes.hash(&mut hasher);
        Self {
            hash: hasher.finish(),
            serialized_len: bytes.len(),
            function_count: vo_module.functions.len(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VerifiedModule {
    digest: ModuleDigest,
}

impl VerifiedModule {
    pub fn matches(self, vo_module: &VoModule) -> bool {
        self.digest == ModuleDigest::for_module(vo_module)
    }
}

fn forloop_target_i64(pc: usize, offset: i16) -> i64 {
    instruction_contracts::forloop_target_i64(pc, offset)
}

pub fn verify_jit_metadata(
    func: &FunctionDef,
    vo_module: &VoModule,
) -> Result<(), JitMetadataError> {
    verify_function_invariants(func)?;

    if func.code.len() != func.jit_metadata.len() {
        return Err(JitMetadataError::LengthMismatch {
            func: func.name.clone(),
            code_len: func.code.len(),
            metadata_len: func.jit_metadata.len(),
        });
    }

    for (pc, inst) in func.code.iter().enumerate() {
        let opcode = inst.opcode();
        if opcode == Opcode::Invalid {
            return Err(JitMetadataError::InvalidOpcode {
                func: func.name.clone(),
                pc,
                raw: inst.op,
            });
        }
        verify_metadata_kind(
            func,
            pc,
            opcode,
            func.code[pc].flags,
            &func.jit_metadata[pc],
        )?;
        verify_slot_contract(func, vo_module, pc)?;
    }

    let analysis = crate::analysis::FunctionAnalysis::for_function(func, vo_module)?;
    for (pc, effects) in analysis.effects.iter().enumerate() {
        for &slot in &effects.reads {
            verify_slot(func, pc, slot, "read")?;
        }
        for &slot in &effects.writes {
            verify_slot(func, pc, slot, "write")?;
        }
    }

    Ok(())
}

pub fn verify_module(vo_module: &VoModule) -> Result<VerifiedModule, JitMetadataError> {
    for func in &vo_module.functions {
        verify_jit_metadata(func, vo_module)?;
    }
    Ok(VerifiedModule {
        digest: ModuleDigest::for_module(vo_module),
    })
}

#[cfg(test)]
mod tests;
