use vo_common_core::bytecode::ReturnFlags;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{Constant, FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use super::JitMetadataError;

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

pub(super) fn verify_slot_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
) -> Result<(), JitMetadataError> {
    let inst = func.code[pc];
    let opcode = inst.opcode();
    match opcode {
        Opcode::LoadInt => verify_load_int_contract(func, pc, inst),
        Opcode::LoadConst => verify_load_const_contract(func, vo_module, pc, inst),
        Opcode::StrNew => verify_str_new_contract(func, vo_module, pc, inst),
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::GeI
        | Opcode::GeU
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU => verify_binary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            scalar_destination_access(opcode),
            "scalar lhs",
            "scalar rhs",
        ),
        Opcode::EqI | Opcode::NeI | Opcode::And | Opcode::Or | Opcode::Xor | Opcode::AndNot => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                &[SlotType::Value],
                RAW_I64_SLOTS,
                RAW_I64_SLOTS,
                scalar_destination_access(opcode),
                "raw lhs",
                "raw rhs",
            )
        }
        Opcode::NegI | Opcode::BoolNot => verify_unary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            scalar_destination_access(opcode),
            "scalar source",
        ),
        Opcode::Not => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            &[SlotType::Value],
            RAW_I64_SLOTS,
            scalar_destination_access(opcode),
            "raw source",
        ),
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                scalar_destination_access(opcode),
                "float lhs",
                "float rhs",
            )
        }
        Opcode::NegF => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            FLOAT_STORAGE_SLOTS,
            scalar_destination_access(opcode),
            "float source",
        ),
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                &[SlotType::Value],
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                scalar_destination_access(opcode),
                "float lhs",
                "float rhs",
            )
        }
        Opcode::ConvI2F => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            &[SlotType::Value],
            "ConvI2F destination",
            "ConvI2F source",
        ),
        Opcode::ConvF2I => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            &[SlotType::Value],
            FLOAT_STORAGE_SLOTS,
            "ConvF2I destination",
            "ConvF2I source",
        ),
        Opcode::ConvF64F32 => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            FLOAT_STORAGE_SLOTS,
            "ConvF64F32 destination",
            "ConvF64F32 source",
        ),
        Opcode::ConvF32F64 => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            &[SlotType::Value, SlotType::Float],
            "ConvF32F64 destination",
            "ConvF32F64 source",
        ),
        Opcode::Trunc => verify_unary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            "Trunc destination",
            "Trunc source",
        ),
        Opcode::IndexCheck => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "IndexCheck index",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "IndexCheck length",
            )
        }
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
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!("unsupported ForLoop flags 0x{:02x}", inst.flags),
                });
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
        Opcode::Copy => verify_copy_contract(func, pc, opcode, inst),
        Opcode::CopyN => verify_copy_n_contract(func, pc, opcode, inst),
        Opcode::SlotGet => verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotGetN => {
            verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::IfaceAssign => {
            verify_interface_or_raw_pair(func, pc, opcode, inst.a, "IfaceAssign destination")?;
            let packed =
                verify_iface_assign_metadata_constant(func, vo_module, pc, opcode, inst.c)?;
            let value_kind = verify_value_kind_tag(func, pc, opcode, inst.flags)?;
            verify_iface_assign_metadata_schema(func, vo_module, pc, opcode, value_kind, packed)?;
            verify_iface_assign_source(func, pc, opcode, inst.b, value_kind)
        }
        Opcode::ClosureNew => verify_closure_new_contract(func, vo_module, pc, inst),
        Opcode::ClosureGet => verify_closure_get_contract(func, pc, inst),
        Opcode::Return => verify_return_contract(func, pc, inst),
        Opcode::Call => verify_static_call_contract(func, vo_module, pc, inst),
        Opcode::CallClosure => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "CallClosure callee",
            )?;
            let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
            if arg_layout.len() != inst.packed_arg_slots() as usize
                || ret_layout.len() != inst.packed_ret_slots() as usize
            {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "CallClosure metadata layout slots args={} returns={} do not match encoded args={} returns={}",
                        arg_layout.len(),
                        ret_layout.len(),
                        inst.packed_arg_slots(),
                        inst.packed_ret_slots()
                    ),
                });
            }
            verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "CallClosure args")?;
            verify_local_layout_matches(
                func,
                pc,
                opcode,
                inst.packed_call_ret_start(),
                &ret_layout,
                "CallClosure returns",
            )
        }
        Opcode::CallIface => {
            verify_interface_pair(func, pc, opcode, inst.a, "CallIface receiver")?;
            let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
            if arg_layout.len() != inst.packed_arg_slots() as usize
                || ret_layout.len() != inst.packed_ret_slots() as usize
            {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "CallIface metadata layout slots args={} returns={} do not match encoded args={} returns={}",
                        arg_layout.len(),
                        ret_layout.len(),
                        inst.packed_arg_slots(),
                        inst.packed_ret_slots()
                    ),
                });
            }
            verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "CallIface args")?;
            verify_local_layout_matches(
                func,
                pc,
                opcode,
                inst.packed_call_ret_start(),
                &ret_layout,
                "CallIface returns",
            )
        }
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
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "GoIsland metadata layout slots args={} returns={} do not match encoded args={}",
                        arg_layout.len(),
                        ret_layout.len(),
                        inst.flags
                    ),
                });
            }
            verify_local_layout_matches(func, pc, opcode, inst.c, &arg_layout, "GoIsland args")
        }
        Opcode::GlobalGet => {
            verify_global_get_contract(func, vo_module, pc, opcode, inst.b, inst.a, 1)
        }
        Opcode::GlobalGetN => verify_global_get_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags as u16,
        ),
        Opcode::PtrNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "PtrNew metadata",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrNew destination",
            )
        }
        Opcode::PtrGet => verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, 1),
        Opcode::PtrGetN => {
            verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, inst.flags as u16)
        }
        Opcode::PtrAdd => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrAdd destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "PtrAdd pointer",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::Value],
                "PtrAdd offset",
            )
        }
        Opcode::PtrSet => verify_ptr_set_contract(func, pc, opcode, inst.a, inst.c, inst.flags),
        Opcode::PtrSetN => {
            let value_layout = ptr_value_layout(func, pc, opcode)?;
            if value_layout.len() != inst.flags as usize {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "PtrSetN metadata layout slots {} do not match encoded count {}",
                        value_layout.len(),
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
                "PtrSetN pointer",
            )?;
            let source = local_layout(func, pc, inst.c, inst.flags as u16, "PtrSetN source")?;
            verify_local_layout_matches(func, pc, opcode, inst.c, &value_layout, "PtrSetN source")?;
            if source
                .iter()
                .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1))
            {
                return Err(JitMetadataError::SlotTypeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access: "PtrSetN source requires typed write barriers",
                    slot: inst.c,
                    expected: source
                        .iter()
                        .map(|st| match st {
                            SlotType::GcRef | SlotType::Interface1 => SlotType::Value,
                            other => *other,
                        })
                        .collect(),
                    actual: source.to_vec(),
                });
            }
            Ok(())
        }
        Opcode::GlobalSet => {
            verify_global_set_contract(func, vo_module, pc, opcode, inst.a, inst.b, 1)
        }
        Opcode::GlobalSetN => verify_global_set_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.a,
            inst.b,
            inst.flags as u16,
        ),
        Opcode::SlotSet => verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotSetN => {
            verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
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
        Opcode::Panic => verify_interface_pair(func, pc, opcode, inst.a, "Panic payload"),
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
        Opcode::Recover => verify_interface_pair(func, pc, opcode, inst.a, "Recover destination"),
        Opcode::IfaceAssert => verify_iface_assert_contract(func, pc, inst),
        Opcode::IfaceEq => verify_iface_eq_contract(func, pc, inst),
        Opcode::IslandNew => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "IslandNew destination",
        ),
        Opcode::Hint => Ok(()),
        Opcode::Invalid => Err(JitMetadataError::InvalidOpcode {
            func: func.name.clone(),
            pc,
            raw: inst.op,
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
