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

fn verify_closure_new_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        Opcode::ClosureNew,
        inst.a,
        &[SlotType::GcRef],
        "ClosureNew destination",
    )?;
    let callee_id = inst.closure_new_func_id();
    vo_module.functions.get(callee_id as usize).ok_or_else(|| {
        JitMetadataError::MissingFunction {
            func: func.name.clone(),
            pc,
            callee_id,
        }
    })?;
    Ok(())
}

fn verify_closure_get_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        Opcode::ClosureGet,
        0,
        &[SlotType::GcRef],
        "ClosureGet closure",
    )?;
    let capture_slot = inst.b as usize;
    let Some(expected) = func.capture_slot_types.get(capture_slot).copied() else {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode: Opcode::ClosureGet,
            detail: format!(
                "capture slot {} out of range for {} capture slots",
                inst.b,
                func.capture_slot_types.len()
            ),
        });
    };
    verify_layout(
        func,
        pc,
        Opcode::ClosureGet,
        inst.a,
        &[expected],
        "ClosureGet destination",
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DynamicCallContractKind {
    Closure,
    Interface,
}

impl DynamicCallContractKind {
    fn args_access(self) -> &'static str {
        match self {
            Self::Closure => "CallClosure args",
            Self::Interface => "CallIface args",
        }
    }

    fn returns_access(self) -> &'static str {
        match self {
            Self::Closure => "CallClosure returns",
            Self::Interface => "CallIface returns",
        }
    }
}

fn verify_dynamic_call_contract(
    ctx: VerifierCtx<'_>,
    kind: DynamicCallContractKind,
) -> Result<(), JitMetadataError> {
    match kind {
        DynamicCallContractKind::Closure => verify_layout(
            ctx.func,
            ctx.pc,
            ctx.opcode,
            ctx.inst.a,
            &[SlotType::GcRef],
            "CallClosure callee",
        )?,
        DynamicCallContractKind::Interface => verify_interface_pair(
            ctx.func,
            ctx.pc,
            ctx.opcode,
            ctx.inst.a,
            "CallIface receiver",
        )?,
    }

    let (arg_layout, ret_layout) = call_layout(ctx.func, ctx.pc, ctx.opcode)?;
    if arg_layout.len() != ctx.inst.packed_arg_slots() as usize
        || ret_layout.len() != ctx.inst.packed_ret_slots() as usize
    {
        return Err(ctx.call_shape_mismatch(format!(
            "{:?} metadata layout slots args={} returns={} do not match encoded args={} returns={}",
            ctx.opcode,
            arg_layout.len(),
            ret_layout.len(),
            ctx.inst.packed_arg_slots(),
            ctx.inst.packed_ret_slots()
        )));
    }
    verify_local_layout_matches(
        ctx.func,
        ctx.pc,
        ctx.opcode,
        ctx.inst.b,
        &arg_layout,
        kind.args_access(),
    )?;
    verify_local_layout_matches(
        ctx.func,
        ctx.pc,
        ctx.opcode,
        ctx.inst.packed_call_ret_start(),
        &ret_layout,
        kind.returns_access(),
    )
}

fn verify_static_call_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::Call;
    let callee_id = inst.static_call_func_id();
    let callee = vo_module.functions.get(callee_id as usize).ok_or_else(|| {
        JitMetadataError::MissingFunction {
            func: func.name.clone(),
            pc,
            callee_id,
        }
    })?;

    if callee.param_slots <= u8::MAX as u16 && callee.ret_slots <= u8::MAX as u16 {
        if inst.packed_arg_slots() != callee.param_slots {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "encoded arg slots {} do not match callee {} param_slots {}",
                    inst.packed_arg_slots(),
                    callee.name,
                    callee.param_slots
                ),
            });
        }
        if inst.packed_ret_slots() != callee.ret_slots {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "encoded ret slots {} do not match callee {} ret_slots {}",
                    inst.packed_ret_slots(),
                    callee.name,
                    callee.ret_slots
                ),
            });
        }
    } else if inst.c != 0 {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "large static call to {} must use zero legacy shape mirror, got 0x{:04x}",
                callee.name, inst.c
            ),
        });
    }

    let expected_args = callee
        .slot_types
        .get(..callee.param_slots as usize)
        .ok_or_else(|| JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "callee {} has {} slot_types but param_slots={}",
                callee.name,
                callee.slot_types.len(),
                callee.param_slots
            ),
        })?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        expected_args,
        "Call argument buffer",
    )?;
    let ret_start = inst.b.checked_add(callee.param_slots).ok_or_else(|| {
        JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.b,
            count: callee.param_slots,
            access: "Call return buffer",
        }
    })?;
    if callee.ret_slot_types.len() != callee.ret_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "callee {} has {} ret_slot_types but ret_slots={}",
                callee.name,
                callee.ret_slot_types.len(),
                callee.ret_slots
            ),
        });
    }
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        ret_start,
        &callee.ret_slot_types,
        "Call return buffer",
    )
}

fn verify_shared_call_shape_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    if inst.call_shape_is_closure() {
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "closure callee",
        )?;
        let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
        if !ret_layout.is_empty() || arg_layout.len() != inst.c as usize {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "{opcode:?} closure metadata layout slots args={} returns={} do not match encoded args={}",
                    arg_layout.len(),
                    ret_layout.len(),
                    inst.c
                ),
            });
        }
        verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "closure call args")
    } else {
        let callee_id = inst.call_shape_static_func_id();
        let callee = vo_module.functions.get(callee_id as usize).ok_or_else(|| {
            JitMetadataError::MissingFunction {
                func: func.name.clone(),
                pc,
                callee_id,
            }
        })?;
        if inst.c != callee.param_slots {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "encoded arg slots {} do not match callee {} param_slots {}",
                    inst.c, callee.name, callee.param_slots
                ),
            });
        }
        let expected_args = callee
            .slot_types
            .get(..callee.param_slots as usize)
            .ok_or_else(|| JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "callee {} has {} slot_types but param_slots={}",
                    callee.name,
                    callee.slot_types.len(),
                    callee.param_slots
                ),
            })?;
        verify_layout(func, pc, opcode, inst.b, expected_args, "static call args")?;
        Ok(())
    }
}

fn verify_call_extern_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::CallExtern;
    let extern_def =
        vo_module
            .externs
            .get(inst.b as usize)
            .ok_or_else(|| JitMetadataError::MissingExtern {
                func: func.name.clone(),
                pc,
                extern_id: inst.b,
            })?;
    if !extern_def.param_kinds.is_empty() && extern_def.param_kinds.len() != inst.flags as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "extern {} has {} param_kinds but instruction encodes {} arg slots",
                extern_def.name,
                extern_def.param_kinds.len(),
                inst.flags
            ),
        });
    }
    let (arg_layout, ret_layout) = call_extern_layout(func, pc, opcode)?;
    if arg_layout.len() != inst.flags as usize || ret_layout.len() != extern_def.ret_slots as usize
    {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "CallExtern metadata layout slots args={} returns={} do not match encoded args={} extern returns={}",
                arg_layout.len(),
                ret_layout.len(),
                inst.flags,
                extern_def.ret_slots
            ),
        });
    }
    verify_local_layout_matches(func, pc, opcode, inst.c, &arg_layout, "CallExtern args")?;
    verify_local_layout_matches(func, pc, opcode, inst.a, &ret_layout, "CallExtern returns")
}

fn call_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "CallLayout",
        crate::metadata::call_layout_from_instruction,
    )
}

fn call_extern_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "CallExternLayout",
        crate::metadata::call_extern_layout_from_instruction,
    )
}
