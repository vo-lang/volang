use super::*;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let vo_module = ctx._vo_module;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::SlotGet => verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotGetN => {
            verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::SlotSet => verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotSetN => {
            verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::GlobalGet => {
            verify_global_get_contract(func, vo_module, pc, opcode, inst.b, inst.a, 1)
        }
        Opcode::GlobalGetN => verify_global_get_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags as u16,
        ),
        Opcode::GlobalSet => {
            verify_global_set_contract(func, vo_module, pc, opcode, inst.a, inst.b, 1)
        }
        Opcode::GlobalSetN => verify_global_set_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.a,
            inst.b,
            inst.flags as u16,
        ),
        Opcode::PtrNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "PtrNew metadata",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrNew destination",
            )
        }
        Opcode::PtrGet => verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, 1),
        Opcode::PtrGetN => {
            verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, inst.flags as u16)
        }
        Opcode::PtrAdd => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrAdd destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "PtrAdd pointer",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::Value],
                "PtrAdd offset",
            )
        }
        Opcode::PtrSet => verify_ptr_set_contract(func, pc, opcode, inst.a, inst.c, inst.flags),
        Opcode::PtrSetN => {
            let value_layout = ptr_value_layout(func, pc, opcode)?;
            if value_layout.len() != inst.flags as usize {
                return Err(ctx.call_shape_mismatch(format!(
                    "PtrSetN metadata layout slots {} do not match encoded count {}",
                    value_layout.len(),
                    inst.flags
                )));
            }
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrSetN pointer",
            )?;
            let source = local_layout(func, pc, inst.c, inst.flags as u16, "PtrSetN source")?;
            verify_local_layout_matches(func, pc, opcode, inst.c, &value_layout, "PtrSetN source")?;
            if source
                .iter()
                .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1))
            {
                return Err(JitMetadataError::SlotTypeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access: "PtrSetN source requires typed write barriers",
                    slot: inst.c,
                    expected: source
                        .iter()
                        .map(|st| match st {
                            SlotType::GcRef | SlotType::Interface1 => SlotType::Value,
                            other => *other,
                        })
                        .collect(),
                    actual: source.to_vec(),
                });
            }
            Ok(())
        }
        other => unreachable!("memory verifier received {other:?}"),
    }
}
