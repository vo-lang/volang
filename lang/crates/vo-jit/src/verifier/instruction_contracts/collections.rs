use super::*;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let vo_module = ctx._vo_module;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::StrNew => verify_str_new_contract(func, vo_module, pc, inst),
        Opcode::ArrayNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "ArrayNew destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "ArrayNew metadata",
            )?;
            verify_value_range(
                func,
                pc,
                opcode,
                inst.c,
                if inst.flags == 0 { 2 } else { 1 },
                "ArrayNew length/elem_bytes",
            )
        }
        Opcode::ArrayGet => {
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Array source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Array index")?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.a, "ArrayGet destination")
        }
        Opcode::ArrayAddr => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "ArrayAddr destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Array source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Array index")
        }
        Opcode::ArraySet => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "ArraySet target",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "ArraySet index",
            )?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.c, "ArraySet source")
        }
        Opcode::SliceNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceNew destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "SliceNew metadata",
            )?;
            verify_value_range(
                func,
                pc,
                opcode,
                inst.c,
                if inst.flags == 0 { 3 } else { 2 },
                "SliceNew len/cap/elem_bytes",
            )
        }
        Opcode::SliceGet => {
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Slice index")?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.a, "SliceGet destination")
        }
        Opcode::SliceAddr => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceAddr destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Slice index")
        }
        Opcode::SliceSet => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceSet target",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "SliceSet index",
            )?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.c, "SliceSet source")
        }
        Opcode::SliceAppend => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceAppend destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "SliceAppend source",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::Value],
                "SliceAppend metadata",
            )?;
            let value_start = checked_slot_offset_for_verifier(
                func,
                pc,
                inst.c,
                if inst.flags == 0 { 2 } else { 1 },
                "SliceAppend value",
            )?;
            verify_indexed_layout_contract(func, pc, opcode, inst, value_start, "SliceAppend value")
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "slice length/capacity result",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")
        }
        Opcode::StrLen => {
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
        Opcode::StrIndex | Opcode::StrDecodeRune => {
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
        Opcode::StrConcat => {
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
        Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe => {
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
        Opcode::StrSlice => {
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
        Opcode::SliceSlice => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceSlice destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "SliceSlice source",
            )?;
            let bound_slots = if (inst.flags & 0b10) != 0 { 3 } else { 2 };
            verify_value_range(func, pc, opcode, inst.c, bound_slots, "SliceSlice bounds")
        }
        Opcode::MapNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "MapNew destination",
            )?;
            verify_value_range(func, pc, opcode, inst.b, 2, "MapNew metadata")
        }
        Opcode::MapGet => verify_map_get_contract(func, pc, inst),
        Opcode::MapSet => verify_map_set_contract(func, pc, inst),
        Opcode::MapDelete => verify_map_delete_contract(func, pc, inst),
        Opcode::MapLen => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "MapLen destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "map")
        }
        Opcode::MapIterInit => {
            verify_local_layout_matches(
                func,
                pc,
                opcode,
                inst.a,
                &vo_runtime::objects::map::MAP_ITER_SLOT_TYPES,
                "MapIterInit iterator",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "map")
        }
        Opcode::MapIterNext => verify_map_iter_next_contract(func, pc, opcode, inst),
        Opcode::QueueNew => verify_queue_new_contract(func, pc, inst),
        Opcode::QueueSend => verify_queue_send_contract(func, pc, inst),
        Opcode::QueueRecv => verify_recv_contract(
            func,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags,
            false,
            "QueueRecv",
        ),
        Opcode::QueueLen | Opcode::QueueCap | Opcode::QueueClose => {
            verify_queue_ref_contract(func, pc, opcode, inst)?;
            if opcode == Opcode::QueueLen || opcode == Opcode::QueueCap {
                verify_layout(func, pc, opcode, inst.a, &[SlotType::Value], "queue result")?;
            }
            Ok(())
        }
        Opcode::SelectBegin => Ok(()),
        Opcode::SelectSend => verify_select_send_contract(func, pc, inst),
        Opcode::SelectRecv => verify_recv_contract(
            func,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags,
            true,
            "SelectRecv",
        ),
        Opcode::SelectExec => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::Value],
            "SelectExec result",
        ),
        Opcode::IslandNew => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "IslandNew destination",
        ),
        other => unreachable!("collections verifier received {other:?}"),
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
