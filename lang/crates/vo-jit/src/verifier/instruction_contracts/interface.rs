use super::*;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let vo_module = ctx._vo_module;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::IfaceAssign => {
            verify_interface_or_raw_pair(func, pc, opcode, inst.a, "IfaceAssign destination")?;
            let packed =
                verify_iface_assign_metadata_constant(func, vo_module, pc, opcode, inst.c)?;
            let value_kind = verify_value_kind_tag(func, pc, opcode, inst.flags)?;
            verify_iface_assign_metadata_schema(func, vo_module, pc, opcode, value_kind, packed)?;
            verify_iface_assign_source(func, pc, opcode, inst.b, value_kind)
        }
        Opcode::IfaceAssert => verify_iface_assert_contract(func, pc, inst),
        Opcode::IfaceEq => verify_iface_eq_contract(func, pc, inst),
        Opcode::Recover => verify_interface_pair(func, pc, opcode, inst.a, "Recover destination"),
        other => unreachable!("interface verifier received {other:?}"),
    }
}
