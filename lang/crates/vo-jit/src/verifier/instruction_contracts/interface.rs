use super::{
    checked_slot_offset_for_verifier, constant_at, constant_kind, decode_metadata_layout,
    local_layout, verify_interface_pair, verify_layout, verify_local_layout_matches, VerifierCtx,
};
use crate::verifier::JitMetadataError;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{Constant, FunctionDef, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

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

fn verify_iface_assert_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::IfaceAssert;
    verify_interface_pair(func, pc, opcode, inst.b, "IfaceAssert source")?;

    let assert_kind = inst.flags & 0x03;
    if assert_kind > 1 {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!("unsupported IfaceAssert kind {assert_kind}"),
        });
    }
    let has_ok = ((inst.flags >> 2) & 0x01) != 0;
    let target_slots = (inst.flags >> 3) as u16;
    let result_layout = iface_assert_result_layout(func, pc, opcode)?;
    let dst_slots = if assert_kind == 1 {
        2
    } else {
        target_slots.max(1)
    };
    if result_layout.len() != dst_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "IfaceAssert metadata layout slots {} do not match encoded destination slots {}",
                result_layout.len(),
                dst_slots
            ),
        });
    }
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.a,
        &result_layout,
        "IfaceAssert destination",
    )?;
    if has_ok {
        let ok_slot =
            checked_slot_offset_for_verifier(func, pc, inst.a, dst_slots, "IfaceAssert ok")?;
        verify_layout(
            func,
            pc,
            opcode,
            ok_slot,
            &[SlotType::Value],
            "IfaceAssert ok",
        )?;
    }
    Ok(())
}

fn verify_iface_eq_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::IfaceEq;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::Value],
        "IfaceEq result",
    )?;
    verify_interface_pair(func, pc, opcode, inst.b, "IfaceEq lhs")?;
    verify_interface_pair(func, pc, opcode, inst.c, "IfaceEq rhs")
}

fn verify_iface_assign_metadata_constant(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    const_id: u16,
) -> Result<i64, JitMetadataError> {
    let constant = constant_at(func, vo_module, pc, const_id)?;
    if let Constant::Int(packed) = constant {
        Ok(*packed)
    } else {
        Err(JitMetadataError::ConstantKindMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            const_id,
            expected: "Int",
            actual: constant_kind(constant),
        })
    }
}

fn verify_value_kind_tag(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    raw: u8,
) -> Result<ValueKind, JitMetadataError> {
    ValueKind::try_from(raw).map_err(|_| JitMetadataError::InvalidValueKind {
        func: func.name.clone(),
        pc,
        opcode,
        raw,
    })
}

fn verify_iface_assign_metadata_schema(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    value_kind: ValueKind,
    packed: i64,
) -> Result<(), JitMetadataError> {
    let packed = packed as u64;
    let high = (packed >> 32) as u32;
    let low = (packed & 0xFFFF_FFFF) as u32;
    if value_kind == ValueKind::Interface {
        if high != 0 {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "interface source metadata must store target iface id in low word only, got high word {high}"
                ),
            });
        }
        if low != 0 && low as usize >= vo_module.interface_metas.len() {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "target interface meta id {low} exceeds interface metadata count {}",
                    vo_module.interface_metas.len()
                ),
            });
        }
    } else if low != 0 && low as usize >= vo_module.itabs.len() {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!("itab id {low} exceeds itab count {}", vo_module.itabs.len()),
        });
    }
    Ok(())
}

fn verify_iface_assign_source(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    src_slot: u16,
    vk: ValueKind,
) -> Result<(), JitMetadataError> {
    match vk {
        ValueKind::Interface => {
            verify_interface_pair(func, pc, opcode, src_slot, "IfaceAssign source")
        }
        ValueKind::Array
        | ValueKind::Struct
        | ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Closure
        | ValueKind::Pointer
        | ValueKind::Port
        | ValueKind::Island => verify_layout(
            func,
            pc,
            opcode,
            src_slot,
            &[SlotType::GcRef],
            "IfaceAssign source",
        ),
        ValueKind::Float32 | ValueKind::Float64 => verify_layout(
            func,
            pc,
            opcode,
            src_slot,
            &[SlotType::Float],
            "IfaceAssign source",
        ),
        _ => verify_layout(
            func,
            pc,
            opcode,
            src_slot,
            &[SlotType::Value],
            "IfaceAssign source",
        ),
    }
}

fn verify_interface_or_raw_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1]
        || actual == [SlotType::Value, SlotType::Value]
    {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidInterfaceLayout {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot,
            actual: actual.to_vec(),
        })
    }
}

fn iface_assert_result_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "IfaceAssertLayout",
        crate::metadata::iface_assert_result_layout_from_instruction,
    )
}
