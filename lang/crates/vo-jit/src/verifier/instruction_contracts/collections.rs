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
