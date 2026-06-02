use super::*;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let vo_module = ctx._vo_module;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::ClosureNew => verify_closure_new_contract(func, vo_module, pc, inst),
        Opcode::ClosureGet => verify_closure_get_contract(func, pc, inst),
        Opcode::Call => verify_static_call_contract(func, vo_module, pc, inst),
        Opcode::CallClosure => verify_dynamic_call_contract(ctx, DynamicCallContractKind::Closure),
        Opcode::CallIface => verify_dynamic_call_contract(ctx, DynamicCallContractKind::Interface),
        Opcode::CallExtern => verify_call_extern_contract(func, vo_module, pc, inst),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            verify_shared_call_shape_contract(func, vo_module, pc, opcode, inst)
        }
        Opcode::GoIsland => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "GoIsland island",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "GoIsland closure",
            )?;
            let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
            if !ret_layout.is_empty() || arg_layout.len() != inst.flags as usize {
                return Err(ctx.call_shape_mismatch(format!(
                    "GoIsland metadata layout slots args={} returns={} do not match encoded args={}",
                    arg_layout.len(),
                    ret_layout.len(),
                    inst.flags
                )));
            }
            verify_local_layout_matches(func, pc, opcode, inst.c, &arg_layout, "GoIsland args")
        }
        other => unreachable!("call verifier received {other:?}"),
    }
}
