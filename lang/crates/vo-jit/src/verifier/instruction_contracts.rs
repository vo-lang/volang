use vo_common_core::bytecode::ReturnFlags;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{Constant, FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use super::JitMetadataError;

mod calls;
mod collections;
mod control;
mod interface;
mod memory;
mod scalar;

const RAW_I64_SLOTS: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
];
const ANY_SINGLE_SLOT: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
    SlotType::Float,
];
const FLOAT_STORAGE_SLOTS: &[SlotType] = &[SlotType::Float, SlotType::Value];

#[derive(Clone, Copy)]
struct VerifierCtx<'a> {
    func: &'a FunctionDef,
    _vo_module: &'a VoModule,
    pc: usize,
    inst: Instruction,
    opcode: Opcode,
}

impl<'a> VerifierCtx<'a> {
    fn new(func: &'a FunctionDef, vo_module: &'a VoModule, pc: usize) -> Self {
        let inst = func.code[pc];
        Self {
            func,
            _vo_module: vo_module,
            pc,
            inst,
            opcode: inst.opcode(),
        }
    }

    fn call_shape_mismatch(self, detail: String) -> JitMetadataError {
        JitMetadataError::CallShapeMismatch {
            func: self.func.name.clone(),
            pc: self.pc,
            opcode: self.opcode,
            detail,
        }
    }
}

pub(super) fn verify_slot_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
) -> Result<(), JitMetadataError> {
    let ctx = VerifierCtx::new(func, vo_module, pc);
    match ctx.opcode {
        Opcode::LoadInt
        | Opcode::LoadConst
        | Opcode::Copy
        | Opcode::CopyN
        | Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::NegI
        | Opcode::AddF
        | Opcode::SubF
        | Opcode::MulF
        | Opcode::DivF
        | Opcode::NegF
        | Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::GeI
        | Opcode::GeU
        | Opcode::EqF
        | Opcode::NeF
        | Opcode::LtF
        | Opcode::LeF
        | Opcode::GtF
        | Opcode::GeF
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Not
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU
        | Opcode::BoolNot
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc
        | Opcode::IndexCheck => scalar::verify(ctx),

        Opcode::Jump
        | Opcode::JumpIf
        | Opcode::JumpIfNot
        | Opcode::ForLoop
        | Opcode::Return
        | Opcode::Panic => control::verify(ctx),

        Opcode::Call
        | Opcode::CallExtern
        | Opcode::CallClosure
        | Opcode::CallIface
        | Opcode::ClosureNew
        | Opcode::ClosureGet
        | Opcode::GoStart
        | Opcode::DeferPush
        | Opcode::ErrDeferPush
        | Opcode::GoIsland => calls::verify(ctx),

        Opcode::SlotGet
        | Opcode::SlotSet
        | Opcode::SlotGetN
        | Opcode::SlotSetN
        | Opcode::GlobalGet
        | Opcode::GlobalGetN
        | Opcode::GlobalSet
        | Opcode::GlobalSetN
        | Opcode::PtrNew
        | Opcode::PtrGet
        | Opcode::PtrSet
        | Opcode::PtrGetN
        | Opcode::PtrSetN
        | Opcode::PtrAdd => memory::verify(ctx),

        Opcode::StrNew
        | Opcode::StrLen
        | Opcode::StrIndex
        | Opcode::StrConcat
        | Opcode::StrSlice
        | Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe
        | Opcode::StrDecodeRune
        | Opcode::ArrayNew
        | Opcode::ArrayGet
        | Opcode::ArraySet
        | Opcode::ArrayAddr
        | Opcode::SliceNew
        | Opcode::SliceGet
        | Opcode::SliceSet
        | Opcode::SliceLen
        | Opcode::SliceCap
        | Opcode::SliceSlice
        | Opcode::SliceAppend
        | Opcode::SliceAddr
        | Opcode::MapNew
        | Opcode::MapGet
        | Opcode::MapSet
        | Opcode::MapDelete
        | Opcode::MapLen
        | Opcode::MapIterInit
        | Opcode::MapIterNext
        | Opcode::QueueNew
        | Opcode::QueueSend
        | Opcode::QueueRecv
        | Opcode::QueueClose
        | Opcode::QueueLen
        | Opcode::QueueCap
        | Opcode::SelectBegin
        | Opcode::SelectSend
        | Opcode::SelectRecv
        | Opcode::SelectExec
        | Opcode::IslandNew => collections::verify(ctx),

        Opcode::IfaceAssign | Opcode::IfaceAssert | Opcode::IfaceEq | Opcode::Recover => {
            interface::verify(ctx)
        }

        Opcode::Hint => Ok(()),
        Opcode::Invalid => Err(JitMetadataError::InvalidOpcode {
            func: ctx.func.name.clone(),
            pc,
            raw: ctx.inst.op,
        }),
    }
}

fn verify_map_iter_next_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.b,
        &vo_runtime::objects::map::MAP_ITER_SLOT_TYPES,
        "MapIterNext iterator",
    )?;
    let key_slots = inst.map_iter_key_slots();
    let val_slots = inst.map_iter_val_slots();
    let (key_layout, val_layout) = map_iter_layout(func, pc, opcode)?;
    if key_layout.len() != key_slots as usize || val_layout.len() != val_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "MapIterNext metadata layout slots key={} value={} do not match encoded key={} value={}",
                key_layout.len(),
                val_layout.len(),
                key_slots,
                val_slots
            ),
        });
    }
    let kv_slots = key_slots
        .checked_add(val_slots)
        .ok_or(JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.a,
            count: key_slots,
            access: "MapIterNext key/value",
        })?;
    let mut kv_layout = key_layout;
    kv_layout.extend(val_layout);
    debug_assert_eq!(kv_layout.len(), kv_slots as usize);
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.a,
        &kv_layout,
        "MapIterNext key/value",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapIterNext ok",
    )
}

fn verify_queue_new_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
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

fn verify_queue_ref_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let slot = if opcode == Opcode::QueueClose {
        inst.a
    } else {
        inst.b
    };
    verify_layout(func, pc, opcode, slot, &[SlotType::GcRef], "queue")
}

fn verify_queue_send_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
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

fn verify_select_send_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
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

    let inst = vo_runtime::instruction::Instruction::with_flags(opcode, flags, 0, 0, 0);
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

fn verify_binary_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst: SlotType,
    lhs: SlotType,
    rhs: SlotType,
    dst_access: &'static str,
    lhs_access: &'static str,
    rhs_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(func, pc, opcode, inst.a, &[dst], dst_access)?;
    verify_layout(func, pc, opcode, inst.b, &[lhs], lhs_access)?;
    verify_layout(func, pc, opcode, inst.c, &[rhs], rhs_access)
}

fn verify_unary_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst: SlotType,
    src: SlotType,
    dst_access: &'static str,
    src_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(func, pc, opcode, inst.a, &[dst], dst_access)?;
    verify_layout(func, pc, opcode, inst.b, &[src], src_access)
}

fn verify_binary_one_of_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst_any: &[SlotType],
    lhs_any: &[SlotType],
    rhs_any: &[SlotType],
    dst_access: &'static str,
    lhs_access: &'static str,
    rhs_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_one_of_single_slot_layout(func, pc, opcode, inst.a, dst_any, dst_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.b, lhs_any, lhs_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.c, rhs_any, rhs_access)
}

fn verify_unary_one_of_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst_any: &[SlotType],
    src_any: &[SlotType],
    dst_access: &'static str,
    src_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_one_of_single_slot_layout(func, pc, opcode, inst.a, dst_any, dst_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.b, src_any, src_access)
}

fn scalar_destination_access(opcode: Opcode) -> &'static str {
    match opcode {
        Opcode::AddI => "AddI destination",
        Opcode::SubI => "SubI destination",
        Opcode::MulI => "MulI destination",
        Opcode::DivI => "DivI destination",
        Opcode::DivU => "DivU destination",
        Opcode::ModI => "ModI destination",
        Opcode::ModU => "ModU destination",
        Opcode::NegI => "NegI destination",
        Opcode::AddF => "AddF destination",
        Opcode::SubF => "SubF destination",
        Opcode::MulF => "MulF destination",
        Opcode::DivF => "DivF destination",
        Opcode::NegF => "NegF destination",
        Opcode::EqI => "EqI destination",
        Opcode::NeI => "NeI destination",
        Opcode::LtI => "LtI destination",
        Opcode::LtU => "LtU destination",
        Opcode::LeI => "LeI destination",
        Opcode::LeU => "LeU destination",
        Opcode::GtI => "GtI destination",
        Opcode::GtU => "GtU destination",
        Opcode::GeI => "GeI destination",
        Opcode::GeU => "GeU destination",
        Opcode::EqF => "EqF destination",
        Opcode::NeF => "NeF destination",
        Opcode::LtF => "LtF destination",
        Opcode::LeF => "LeF destination",
        Opcode::GtF => "GtF destination",
        Opcode::GeF => "GeF destination",
        Opcode::And => "And destination",
        Opcode::Or => "Or destination",
        Opcode::Xor => "Xor destination",
        Opcode::AndNot => "AndNot destination",
        Opcode::Not => "Not destination",
        Opcode::Shl => "Shl destination",
        Opcode::ShrS => "ShrS destination",
        Opcode::ShrU => "ShrU destination",
        Opcode::BoolNot => "BoolNot destination",
        _ => "scalar destination",
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

pub(super) fn forloop_target_i64(pc: usize, offset: i16) -> i64 {
    pc as i64 + 1 + i64::from(offset)
}

fn verify_load_int_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    if inst.imm32() == 0 {
        verify_one_of_single_slot_layout(
            func,
            pc,
            Opcode::LoadInt,
            inst.a,
            ANY_SINGLE_SLOT,
            "LoadInt destination",
        )
    } else {
        verify_layout(
            func,
            pc,
            Opcode::LoadInt,
            inst.a,
            &[SlotType::Value],
            "LoadInt destination",
        )
    }
}

fn verify_load_const_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let constant = constant_at(func, vo_module, pc, inst.b)?;
    let expected_slot = match constant {
        Constant::String(_) => {
            return Err(JitMetadataError::ConstantKindMismatch {
                func: func.name.clone(),
                pc,
                opcode: Opcode::LoadConst,
                const_id: inst.b,
                expected: "non-string constant; use StrNew for string allocation",
                actual: constant_kind(constant),
            });
        }
        Constant::Float(_) => {
            return verify_one_of_single_slot_layout(
                func,
                pc,
                Opcode::LoadConst,
                inst.a,
                FLOAT_STORAGE_SLOTS,
                "LoadConst destination",
            );
        }
        Constant::Nil => {
            return verify_one_of_single_slot_layout(
                func,
                pc,
                Opcode::LoadConst,
                inst.a,
                ANY_SINGLE_SLOT,
                "LoadConst destination",
            );
        }
        Constant::Bool(_) | Constant::Int(_) => SlotType::Value,
    };
    verify_layout(
        func,
        pc,
        Opcode::LoadConst,
        inst.a,
        &[expected_slot],
        "LoadConst destination",
    )
}

fn verify_str_new_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
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

fn constant_at<'a>(
    func: &FunctionDef,
    vo_module: &'a VoModule,
    pc: usize,
    const_id: u16,
) -> Result<&'a Constant, JitMetadataError> {
    vo_module
        .constants
        .get(const_id as usize)
        .ok_or_else(|| JitMetadataError::MissingConstant {
            func: func.name.clone(),
            pc,
            const_id,
        })
}

fn constant_kind(constant: &Constant) -> &'static str {
    match constant {
        Constant::Nil => "Nil",
        Constant::Bool(_) => "Bool",
        Constant::Int(_) => "Int",
        Constant::Float(_) => "Float",
        Constant::String(_) => "String",
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

fn verify_indexed_layout_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    start: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let layout = indexed_elem_layout(func, pc, opcode, inst)?;
    if layout.is_empty() {
        return Ok(());
    }
    verify_local_layout_matches(func, pc, opcode, start, &layout, access)
}

fn indexed_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<Vec<SlotType>, JitMetadataError> {
    let layout = decode_metadata_layout(
        func,
        pc,
        opcode,
        "ElemLayout",
        crate::metadata::elem_layout_from_instruction,
    )?;
    debug_assert_eq!(layout.slot_layout.len(), layout.slots as usize);
    let from_flags = if inst.flags == 0 {
        None
    } else {
        Some(crate::metadata::elem_layout_from_flags(inst.flags))
    };
    if let Some(from_flags) = from_flags {
        if from_flags.bytes != layout.bytes
            || from_flags.needs_sign_extend != layout.needs_sign_extend
        {
            return Err(JitMetadataError::InconsistentElemLayout {
                func: func.name.clone(),
                pc,
                flags: inst.flags,
            });
        }
    }
    Ok(layout.slot_layout)
}

fn verify_map_get_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapGet;
    let layout = map_get_layout(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "MapGet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapGet metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.c, 1, "MapGet key")?;
    verify_layout(
        func,
        pc,
        opcode,
        key_start,
        &layout.key_layout,
        "MapGet key",
    )?;
    let output_slots =
        layout
            .output_slots()
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: inst.a,
                count: layout.val_slots,
                access: "MapGet destination",
            })?;
    let mut output_layout = layout.val_layout.clone();
    if layout.has_ok {
        output_layout.push(SlotType::Value);
    }
    debug_assert_eq!(output_layout.len(), output_slots as usize);
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &output_layout,
        "MapGet destination",
    )
}

fn verify_map_set_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapSet;
    let layout = map_set_layout(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], "MapSet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapSet metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.b, 1, "MapSet key")?;
    verify_layout(
        func,
        pc,
        opcode,
        key_start,
        &layout.key_layout,
        "MapSet key",
    )?;
    verify_layout(func, pc, opcode, inst.c, &layout.val_layout, "MapSet value")
}

fn verify_map_delete_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapDelete;
    let key_layout = map_delete_key_layout(func, pc, opcode)?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "MapDelete map",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapDelete metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.b, 1, "MapDelete key")?;
    verify_layout(func, pc, opcode, key_start, &key_layout, "MapDelete key")
}

fn map_get_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<crate::metadata::MapGetLayout, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapGet",
        crate::metadata::map_get_layout_from_instruction,
    )
}

fn map_set_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<crate::metadata::MapSetLayout, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapSet",
        crate::metadata::map_set_layout_from_instruction,
    )
}

fn map_delete_key_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapDelete",
        crate::metadata::map_delete_key_layout_from_instruction,
    )
}

fn ptr_value_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "PtrLayout",
        crate::metadata::ptr_value_layout_from_instruction,
    )
}

fn slot_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "SlotLayout",
        crate::metadata::slot_elem_layout_from_instruction,
    )
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

fn map_iter_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), JitMetadataError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "MapIterNext",
        crate::metadata::map_iter_next_layout_from_instruction,
    )
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

fn decode_metadata_layout<T>(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    layout: &'static str,
    decode: impl FnOnce(&JitInstructionMetadata) -> Option<T>,
) -> Result<T, JitMetadataError> {
    func.jit_metadata
        .get(pc)
        .and_then(decode)
        .ok_or_else(|| JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout,
        })
}

fn checked_slot_offset_for_verifier(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    offset: u16,
    access: &'static str,
) -> Result<u16, JitMetadataError> {
    start
        .checked_add(offset)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count: offset.saturating_add(1),
            access,
        })
}

fn verify_ptr_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    ptr_slot: u16,
    src_slot: u16,
    flags: u8,
) -> Result<(), JitMetadataError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != 1 {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "PtrSet metadata layout slots {} do not match encoded count 1",
                value_layout.len()
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcRef],
        "PtrSet pointer",
    )?;
    let source = local_layout(func, pc, src_slot, 1, "PtrSet source")?;
    verify_raw_or_exact_layout_matches(func, pc, opcode, src_slot, &value_layout, "PtrSet source")?;
    let requires_barrier = matches!(source[0], SlotType::GcRef | SlotType::Interface1);
    let has_barrier = (flags & 1) != 0;
    if requires_barrier && !has_barrier {
        return Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access: "PtrSet missing write barrier",
            slot: src_slot,
            expected: vec![SlotType::GcRef],
            actual: source.to_vec(),
        });
    }
    Ok(())
}

fn verify_global_set_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    src_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let globals = flattened_global_slot_types(vo_module);
    let end =
        global_start
            .checked_add(count)
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: global_start,
                count,
                access: "global write",
            })? as usize;
    if end > globals.len() {
        return Err(JitMetadataError::GlobalSlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: global_start,
            global_slots: globals.len(),
            access: "write",
        });
    }
    let expected = &globals[global_start as usize..end];
    verify_structural_layout(func, pc, opcode, global_start, expected, "GlobalSet target")?;
    verify_local_layout_matches(func, pc, opcode, src_start, expected, "GlobalSet source")
}

fn verify_global_get_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    dst_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let globals = flattened_global_slot_types(vo_module);
    let end =
        global_start
            .checked_add(count)
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: global_start,
                count,
                access: "global read",
            })? as usize;
    if end > globals.len() {
        return Err(JitMetadataError::GlobalSlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: global_start,
            global_slots: globals.len(),
            access: "read",
        });
    }
    let expected = &globals[global_start as usize..end];
    verify_structural_layout(func, pc, opcode, global_start, expected, "GlobalGet source")?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        dst_start,
        expected,
        "GlobalGet destination",
    )
}

fn verify_ptr_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    ptr_slot: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "PtrGet metadata layout slots {} do not match encoded count {}",
                value_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcRef],
        "PtrGet pointer",
    )?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        dst_start,
        &value_layout,
        "PtrGet destination",
    )
}

fn verify_slot_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    base_start: u16,
    index_slot: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SlotGet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        index_slot,
        &[SlotType::Value],
        "SlotGet index",
    )?;
    if count == 1 {
        verify_raw_or_exact_layout_matches(
            func,
            pc,
            opcode,
            base_start,
            &elem_layout,
            "SlotGet element",
        )?;
        verify_raw_or_exact_layout_matches(
            func,
            pc,
            opcode,
            dst_start,
            &elem_layout,
            "SlotGet destination",
        )
    } else {
        verify_local_layout_matches(
            func,
            pc,
            opcode,
            base_start,
            &elem_layout,
            "SlotGet element",
        )?;
        verify_local_layout_matches(
            func,
            pc,
            opcode,
            dst_start,
            &elem_layout,
            "SlotGet destination",
        )
    }
}

fn verify_slot_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    base_start: u16,
    index_slot: u16,
    src_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SlotSet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        index_slot,
        &[SlotType::Value],
        "SlotSet index",
    )?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        base_start,
        &elem_layout,
        "SlotSet element",
    )?;
    verify_local_layout_matches(func, pc, opcode, src_start, &elem_layout, "SlotSet source")
}

fn verify_copy_n_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let count = inst.copy_n_count();
    let source = local_layout(func, pc, inst.b, count, "CopyN source")?;
    verify_structural_layout(func, pc, opcode, inst.b, source, "CopyN source")?;
    verify_local_layout_matches(func, pc, opcode, inst.a, source, "CopyN destination")
}

fn verify_copy_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let source = local_layout(func, pc, inst.b, 1, "Copy source")?;
    let actual = local_layout(func, pc, inst.a, 1, "Copy destination")?;
    if actual == source {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access: "Copy destination",
            slot: inst.a,
            expected: source.to_vec(),
            actual: actual.to_vec(),
        })
    }
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

fn verify_interface_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1] {
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

fn verify_value_range(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        start,
        &vec![SlotType::Value; count as usize],
        access,
    )
}

fn verify_local_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    verify_structural_layout(func, pc, opcode, start, actual, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn verify_raw_or_exact_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        verify_structural_layout(func, pc, opcode, start, actual, access)?;
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn verify_structural_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    layout: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let mut i = 0usize;
    while i < layout.len() {
        match layout[i] {
            SlotType::Interface0 => {
                if layout.get(i + 1) != Some(&SlotType::Interface1) {
                    return Err(JitMetadataError::InvalidInterfaceLayout {
                        func: func.name.clone(),
                        pc,
                        opcode,
                        access,
                        slot: start + i as u16,
                        actual: layout[i..(i + 1).min(layout.len())].to_vec(),
                    });
                }
                i += 2;
            }
            SlotType::Interface1 => {
                return Err(JitMetadataError::InvalidInterfaceLayout {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access,
                    slot: start + i as u16,
                    actual: vec![SlotType::Interface1],
                });
            }
            _ => i += 1,
        }
    }
    Ok(())
}

fn verify_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn verify_one_of_single_slot_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected_any: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, 1, access)?;
    if expected_any.contains(&actual[0]) {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected_any.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn local_layout<'a>(
    func: &'a FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<&'a [SlotType], JitMetadataError> {
    verify_range(func, pc, start, count, access)?;
    let start = start as usize;
    let end = start + count as usize;
    Ok(&func.slot_types[start..end])
}

fn verify_range(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if count == 0 {
        return Ok(());
    }
    let end = start
        .checked_add(count - 1)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count,
            access,
        })?;
    if end >= func.local_slots || end as usize >= func.slot_types.len() {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: end,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}

fn flattened_global_slot_types(vo_module: &VoModule) -> Vec<SlotType> {
    vo_module
        .globals
        .iter()
        .flat_map(|global| global.slot_types.iter().copied())
        .collect()
}

pub(super) fn verify_slot(
    func: &FunctionDef,
    pc: usize,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if slot >= func.local_slots {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}
