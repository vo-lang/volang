//! Verifier for bytecode facts consumed by the JIT.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use vo_common_core::verifier as module_verifier;
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
#[cfg(test)]
use vo_runtime::instruction::Opcode;
mod errors;
mod metadata_checks;

pub use errors::JitMetadataError;
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
    pc as i64 + 1 + i64::from(offset)
}

pub fn verify_jit_metadata(
    func: &FunctionDef,
    vo_module: &VoModule,
) -> Result<(), JitMetadataError> {
    verify_strict_jit_metadata_only(func).and_then(|()| {
        module_verifier::verify_function(func, vo_module).map_err(JitMetadataError::from)
    })
}

fn verify_strict_jit_metadata_only(func: &FunctionDef) -> Result<(), JitMetadataError> {
    if func.code.len() != func.jit_metadata.len() {
        return Err(JitMetadataError::LengthMismatch {
            func: func.name.clone(),
            code_len: func.code.len(),
            metadata_len: func.jit_metadata.len(),
        });
    }

    for (pc, inst) in func.code.iter().enumerate() {
        let opcode = inst.opcode();
        if opcode == vo_runtime::instruction::Opcode::Invalid {
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
    }

    Ok(())
}

pub fn verify_module(vo_module: &VoModule) -> Result<VerifiedModule, JitMetadataError> {
    let verified = module_verifier::verify_module(vo_module).map_err(JitMetadataError::from)?;
    verify_module_after_common(verified)
}

pub fn verify_module_after_common(
    verified: module_verifier::VerifiedModule<'_>,
) -> Result<VerifiedModule, JitMetadataError> {
    let vo_module = verified.module();
    for func in &vo_module.functions {
        verify_strict_jit_metadata_only(func)?;
    }
    Ok(VerifiedModule {
        digest: ModuleDigest::for_module(vo_module),
    })
}

#[cfg(test)]
mod tests;
