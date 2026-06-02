use super::*;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::Jump => {
            verify_jump_target_contract(func, pc, opcode, jump_target_i64(pc, inst.imm32()))
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                if opcode == Opcode::JumpIf {
                    "JumpIf condition"
                } else {
                    "JumpIfNot condition"
                },
            )?;
            verify_jump_target_contract(func, pc, opcode, jump_target_i64(pc, inst.imm32()))
        }
        Opcode::ForLoop => {
            if inst.flags & !0x07 != 0 {
                return Err(ctx.call_shape_mismatch(format!(
                    "unsupported ForLoop flags 0x{:02x}",
                    inst.flags
                )));
            }
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "ForLoop index",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "ForLoop limit",
            )?;
            verify_jump_target_contract(func, pc, opcode, forloop_target_i64(pc, inst.c as i16))
        }
        Opcode::Return => verify_return_contract(func, pc, inst),
        Opcode::Panic => verify_interface_pair(func, pc, opcode, inst.a, "Panic payload"),
        other => unreachable!("control verifier received {other:?}"),
    }
}
