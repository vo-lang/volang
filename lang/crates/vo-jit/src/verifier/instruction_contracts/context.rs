use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};

#[derive(Clone, Copy)]
pub(crate) struct VerifierCtx<'a> {
    pub(crate) func: &'a FunctionDef,
    pub(crate) _vo_module: &'a VoModule,
    pub(crate) pc: usize,
    pub(crate) inst: Instruction,
    pub(crate) opcode: Opcode,
}

impl<'a> VerifierCtx<'a> {
    pub(crate) fn new(func: &'a FunctionDef, vo_module: &'a VoModule, pc: usize) -> Self {
        let inst = func.code[pc];
        Self {
            func,
            _vo_module: vo_module,
            pc,
            inst,
            opcode: inst.opcode(),
        }
    }

    pub(crate) fn call_shape_mismatch(self, detail: String) -> JitMetadataError {
        JitMetadataError::CallShapeMismatch {
            func: self.func.name.clone(),
            pc: self.pc,
            opcode: self.opcode,
            detail,
        }
    }
}
