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

fn verify_return_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::Return;
    let flags = ReturnFlags::from_bits(inst.flags).ok_or_else(|| {
        JitMetadataError::InvalidInstructionFlags {
            func: func.name.clone(),
            pc,
            opcode,
            flags: inst.flags,
            allowed: ReturnFlags::ALLOWED_BITS,
        }
    })?;
    if flags.has_heap_returns() {
        if func.heap_ret_gcref_count == 0 {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: "heap return flag set but function has no heap return GcRefs".to_string(),
            });
        }
        if inst.b != func.heap_ret_gcref_count {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "heap return count {} does not match function heap_ret_gcref_count {}",
                    inst.b, func.heap_ret_gcref_count
                ),
            });
        }
        if inst.a != func.heap_ret_gcref_start {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "heap return start {} does not match function heap_ret_gcref_start {}",
                    inst.a, func.heap_ret_gcref_start
                ),
            });
        }
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &vec![SlotType::GcRef; inst.b as usize],
            "Return heap named returns",
        )?;
        return Ok(());
    }

    if inst.b != func.ret_slots {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "return slot count {} does not match function ret_slots {}",
                inst.b, func.ret_slots
            ),
        });
    }
    if func.ret_slot_types.len() != func.ret_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "function has {} ret_slot_types but ret_slots={}",
                func.ret_slot_types.len(),
                func.ret_slots
            ),
        });
    }
    let expected = &func.ret_slot_types[..inst.b as usize];
    verify_local_layout_matches(func, pc, opcode, inst.a, expected, "Return values")?;

    if func.error_ret_slot >= 0 {
        let error_offset = func.error_ret_slot as u16;
        if error_offset + 1 < inst.b {
            verify_interface_pair(func, pc, opcode, inst.a + error_offset, "Return error slot")?;
        }
    }
    Ok(())
}

fn verify_jump_target_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    target: i64,
) -> Result<(), JitMetadataError> {
    if target >= 0 && (target as usize) < func.code.len() {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidBranchTarget {
            func: func.name.clone(),
            pc,
            opcode,
            target,
            code_len: func.code.len(),
        })
    }
}

fn jump_target_i64(pc: usize, offset: i32) -> i64 {
    pc as i64 + offset as i64
}
