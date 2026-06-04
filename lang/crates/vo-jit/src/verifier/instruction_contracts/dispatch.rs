use super::{
    calls, collections, control, interface, memory, scalar, verify_requirement_preflight,
    VerifierCtx,
};
use crate::semantics::OpcodeSemantics;
use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::{FunctionDef, Module as VoModule};

pub(crate) fn verify_slot_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
) -> Result<(), JitMetadataError> {
    let ctx = VerifierCtx::new(func, vo_module, pc);
    let row = crate::semantics::opcode_semantics(ctx.opcode);
    verify_slot_contract_with_row(ctx, &row)
}

pub(crate) fn verify_slot_contract_with_row(
    ctx: VerifierCtx<'_>,
    row: &OpcodeSemantics,
) -> Result<(), JitMetadataError> {
    verify_requirement_preflight(ctx, row)?;

    match row.verifier_domain {
        crate::semantics::VerifierDomain::None => Ok(()),
        crate::semantics::VerifierDomain::Scalar => scalar::verify(ctx),
        crate::semantics::VerifierDomain::Control => control::verify(ctx),
        crate::semantics::VerifierDomain::Memory => memory::verify(ctx),
        crate::semantics::VerifierDomain::Collections => collections::verify(ctx),
        crate::semantics::VerifierDomain::Calls => calls::verify(ctx),
        crate::semantics::VerifierDomain::Interface => interface::verify(ctx),
        crate::semantics::VerifierDomain::Invalid => Err(JitMetadataError::InvalidOpcode {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            raw: ctx.inst.op,
        }),
    }
}
