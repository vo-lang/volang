use super::super::{
    checked_slot_offset_for_verifier, decode_metadata_layout, verify_layout,
    verify_local_layout_matches,
};
use crate::verifier::JitMetadataError;
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

pub(super) fn verify_queue_new_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::QueueNew;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "QueueNew destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "QueueNew element metadata",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "QueueNew capacity",
    )
}

pub(super) fn verify_queue_send_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::QueueSend;
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != inst.flags as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "QueueSend metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                inst.flags
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "QueueSend queue",
    )?;
    verify_local_layout_matches(func, pc, opcode, inst.b, &elem_layout, "QueueSend value")
}

pub(super) fn verify_queue_recv_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_recv_contract(
        func,
        pc,
        opcode,
        inst.b,
        inst.a,
        inst.flags,
        false,
        "QueueRecv",
    )
}

pub(super) fn verify_queue_len_cap_close_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_queue_ref_contract(func, pc, opcode, inst)?;
    if opcode == Opcode::QueueLen || opcode == Opcode::QueueCap {
        verify_layout(func, pc, opcode, inst.a, &[SlotType::Value], "queue result")?;
    }
    Ok(())
}

pub(super) fn verify_select_send_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::SelectSend;
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    let elem_slots = if inst.flags == 0 {
        1
    } else {
        inst.flags as u16
    };
    if elem_layout.len() != elem_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SelectSend metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                elem_slots
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "SelectSend queue",
    )?;
    verify_local_layout_matches(func, pc, opcode, inst.b, &elem_layout, "SelectSend value")
}

pub(super) fn verify_select_recv_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    verify_recv_contract(
        func,
        pc,
        opcode,
        inst.b,
        inst.a,
        inst.flags,
        true,
        "SelectRecv",
    )
}

pub(super) fn verify_select_exec_contract(
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
        "SelectExec result",
    )
}

fn verify_queue_ref_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), JitMetadataError> {
    let slot = if opcode == Opcode::QueueClose {
        inst.a
    } else {
        inst.b
    };
    verify_layout(func, pc, opcode, slot, &[SlotType::GcRef], "queue")
}

fn verify_recv_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    queue_slot: u16,
    dst_start: u16,
    flags: u8,
    normalize_zero_elem_slots: bool,
    access_prefix: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        queue_slot,
        &[SlotType::GcRef],
        "recv queue",
    )?;

    let inst = Instruction::with_flags(opcode, flags, 0, 0, 0);
    let mut elem_slots = inst.recv_elem_slots();
    if normalize_zero_elem_slots && elem_slots == 0 {
        elem_slots = 1;
    }
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != elem_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "{access_prefix} metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                elem_slots
            ),
        });
    }
    if elem_slots > 0 {
        verify_local_layout_matches(func, pc, opcode, dst_start, &elem_layout, access_prefix)?;
    }
    if inst.recv_has_ok() {
        let ok_slot =
            checked_slot_offset_for_verifier(func, pc, dst_start, elem_slots, access_prefix)?;
        verify_layout(
            func,
            pc,
            opcode,
            ok_slot,
            &[SlotType::Value],
            if opcode == Opcode::QueueRecv {
                "QueueRecv ok"
            } else {
                "SelectRecv ok"
            },
        )?;
    }
    Ok(())
}

fn queue_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "QueueLayout",
        crate::metadata::queue_elem_layout_from_instruction,
    )
}
