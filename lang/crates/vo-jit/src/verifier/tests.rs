use super::*;
use vo_common_core::bytecode::ReturnFlags;
use vo_common_core::instruction::HINT_LOOP;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{
    Constant, ExternDef, GlobalDef, JitInstructionMetadata, Module as VoModule,
};
use vo_runtime::instruction::Instruction;
use vo_runtime::SlotType;

fn make_func(
    code: Vec<Instruction>,
    jit_metadata: Vec<JitInstructionMetadata>,
    local_slots: u16,
) -> FunctionDef {
    crate::test_fixtures::JitFunctionBuilder::new(code)
        .name("verify")
        .local_slots(local_slots)
        .jit_metadata(jit_metadata)
        .build()
}

fn make_func_with_slot_types(
    code: Vec<Instruction>,
    jit_metadata: Vec<JitInstructionMetadata>,
    slot_types: Vec<SlotType>,
    ret_slots: u16,
) -> FunctionDef {
    crate::test_fixtures::JitFunctionBuilder::new(code)
        .name("verify")
        .slot_types(slot_types)
        .signature(0, 0, ret_slots)
        .jit_metadata(jit_metadata)
        .build()
}

fn make_func_with_shape(
    code: Vec<Instruction>,
    jit_metadata: Vec<JitInstructionMetadata>,
    slot_types: Vec<SlotType>,
    param_slots: u16,
    ret_slots: u16,
    error_ret_slot: i32,
) -> FunctionDef {
    crate::test_fixtures::function_with_shape(
        code,
        jit_metadata,
        slot_types,
        param_slots,
        ret_slots,
        error_ret_slot,
    )
}

fn hint_loop(end_offset: u8) -> Instruction {
    Instruction::with_flags(Opcode::Hint, HINT_LOOP, (end_offset as u16) << 8, 0, 0)
}

fn jump(offset: i32) -> Instruction {
    let encoded = offset as u32;
    Instruction::new(Opcode::Jump, 0, encoded as u16, (encoded >> 16) as u16)
}

fn load_int32(dst: u16, value: u32) -> Instruction {
    Instruction::new(Opcode::LoadInt, dst, value as u16, (value >> 16) as u16)
}

fn refresh_function_metadata(func: &mut FunctionDef) {
    crate::test_fixtures::refresh_derived_function_fields(func);
}

fn transfer_int(slots: u16) -> vo_runtime::bytecode::TransferType {
    vo_runtime::bytecode::TransferType {
        meta_raw: vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int).to_raw(),
        rttid_raw: vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int).to_raw(),
        slots,
    }
}

#[path = "tests/calls.rs"]
mod calls;
#[path = "tests/collections.rs"]
mod collections;
#[path = "tests/control.rs"]
mod control;
#[path = "tests/interface.rs"]
mod interface;
#[path = "tests/invariants.rs"]
mod invariants;
#[path = "tests/memory.rs"]
mod memory;
#[path = "tests/metadata.rs"]
mod metadata;
#[path = "tests/scalar.rs"]
mod scalar;
