use super::super::{constant_at, constant_kind, verify_layout, verify_value_range};
use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::{Constant, FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

pub(super) fn verify_str_new_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        Opcode::StrNew,
        inst.a,
        &[SlotType::GcRef],
        "StrNew destination",
    )?;
    let constant = constant_at(func, vo_module, pc, inst.b)?;
    if !matches!(constant, Constant::String(_)) {
        return Err(JitMetadataError::ConstantKindMismatch {
            func: func.name.clone(),
            pc,
            opcode: Opcode::StrNew,
            const_id: inst.b,
            expected: "String",
            actual: constant_kind(constant),
        });
    }
    Ok(())
}

pub(super) fn verify_str_len_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::Value],
        "StrLen destination",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string")
}

pub(super) fn verify_str_index_or_decode_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    if opcode == Opcode::StrDecodeRune {
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::Value, SlotType::Value],
            "StrDecodeRune destination",
        )?;
    } else {
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::Value],
            "StrIndex destination",
        )?;
    }
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string")?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "string index")
}

pub(super) fn verify_str_concat_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "StrConcat destination",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string lhs")?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::GcRef], "string rhs")
}

pub(super) fn verify_str_compare_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::Value],
        "string comparison destination",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string lhs")?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::GcRef], "string rhs")
}

pub(super) fn verify_str_slice_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "StrSlice destination",
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string")?;
    verify_value_range(func, pc, opcode, inst.c, 2, "string slice bounds")
}
