//! Materialized-frame interface instructions.
use super::super::*;
use super::FrameContext;

pub(super) fn compile(
    body: &mut Function,
    context: FrameContext<'_>,
    instruction: vo_common_core::Instruction,
) -> Result<bool, WasmAotError> {
    let FrameContext {
        module,
        function,
        pc,
        current_block,
        runtime_globals,
        static_data,
        allocation_descriptors,
        ..
    } = context;
    let opcode = instruction.opcode();
    match opcode {
        Opcode::IfaceAssign => {
            let Constant::Int(packed) =
                module
                    .constants
                    .get(instruction.c as usize)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} references missing interface metadata",
                            function.name
                        ))
                    })?
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} interface metadata is not an integer",
                    function.name
                )));
            };
            let rttid = (*packed as u64) >> 32;
            let low = *packed as u32;
            if instruction.flags == vo_common_core::ValueKind::Interface as u8 {
                body.instruction(&W::Block(BlockType::Empty));
                load_slot(body, instruction.b);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty));
                store_const(body, instruction.a, 0);
                store_const(body, instruction.a + 1, 0);
                body.instruction(&W::Br(1)).instruction(&W::End);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::LocalSet(PACKED_LOCAL));
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0xff))
                    .instruction(&W::I32And)
                    .instruction(&W::LocalTee(STATUS_LOCAL))
                    .instruction(&W::I32Const(ValueKind::Struct as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::LocalGet(STATUS_LOCAL))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::I32Or)
                    .instruction(&W::If(BlockType::Empty));
                shallow_clone_payload(
                    body,
                    instruction.b + 1,
                    runtime_globals,
                    allocation_descriptors,
                );
                body.instruction(&W::End);
                if low == 0 {
                    store_prefix(body, instruction.a);
                    load_slot(body, instruction.b);
                    body.instruction(&W::I64Const(u32::MAX as i64))
                        .instruction(&W::I64And)
                        .instruction(&W::I64Store(memarg(0)));
                    store_prefix(body, instruction.a + 1);
                    body.instruction(&W::LocalGet(PACKED_LOCAL))
                        .instruction(&W::I64Store(memarg(0)))
                        .instruction(&W::Br(0));
                } else {
                    for (value_rttid, _) in interface_implementations(module, low)? {
                        load_slot(body, instruction.b);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(value_rttid as i32))
                            .instruction(&W::I32Eq)
                            .instruction(&W::If(BlockType::Empty));
                        store_const(
                            body,
                            instruction.a,
                            ((u64::from(low) << 32) | u64::from(value_rttid)) as i64,
                        );
                        store_prefix(body, instruction.a + 1);
                        body.instruction(&W::LocalGet(PACKED_LOCAL))
                            .instruction(&W::I64Store(memarg(0)))
                            .instruction(&W::Br(1))
                            .instruction(&W::End);
                    }
                    return_status(body, STATUS_INVALID_CONTROL_FLOW);
                }
                body.instruction(&W::End);
                return Ok(false);
            }
            let itab = if low == vo_common_core::bytecode::IFACE_ASSIGN_NO_ITAB {
                0
            } else {
                low
            };
            let slot0 = (u64::from(itab) << 32) | (rttid << 8) | u64::from(instruction.flags);
            store_const(body, instruction.a, slot0 as i64);
            if matches!(
                ValueKind::try_from(instruction.flags),
                Ok(ValueKind::Struct | ValueKind::Array)
            ) {
                shallow_clone_payload(body, instruction.b, runtime_globals, allocation_descriptors);
                store_prefix(body, instruction.a + 1);
                body.instruction(&W::LocalGet(PACKED_LOCAL));
            } else {
                store_prefix(body, instruction.a + 1);
                load_slot(body, instruction.b);
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::IfaceAssert => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::iface_assert_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing IfaceAssertLayout metadata",
                        function.name
                    ))
                })?;
            let has_ok = instruction.flags & 0x01 != 0;
            let array_layout = if layout.assert_kind == 0 {
                interface_array_assertion_layout(module, layout.target_id, layout.result_slots)?
            } else {
                None
            };
            for slot in 0..layout.result_slots + u16::from(has_ok) {
                store_const(body, instruction.a + slot, 0);
            }
            body.instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(ALLOC_LOCAL));
            if layout.assert_kind == 0 {
                load_slot(body, instruction.b);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I32)));
                load_slot(body, instruction.b);
                body.instruction(&W::I64Const(8))
                    .instruction(&W::I64ShrU)
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0x00ff_ffff))
                    .instruction(&W::I32And)
                    .instruction(&W::I32Const(layout.target_id as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::Else)
                    .instruction(&W::I32Const(0))
                    .instruction(&W::End)
                    .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            } else if layout.assert_kind == 1 {
                load_slot(body, instruction.b);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::Else);
                if layout.target_id == 0 {
                    body.instruction(&W::I32Const(1))
                        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
                } else {
                    for (value_rttid, _) in interface_implementations(module, layout.target_id)? {
                        load_slot(body, instruction.b);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(value_rttid as i32))
                            .instruction(&W::I32Eq)
                            .instruction(&W::If(BlockType::Empty))
                            .instruction(&W::I32Const(1))
                            .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                            .instruction(&W::I32Const(layout.target_id as i32))
                            .instruction(&W::LocalSet(ALLOC_LOCAL))
                            .instruction(&W::End);
                    }
                }
                body.instruction(&W::End);
            } else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} has invalid interface assertion kind {}",
                    function.name, layout.assert_kind
                )));
            }
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::If(BlockType::Empty));
            if layout.assert_kind == 1 {
                store_prefix(body, instruction.a);
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::I64Const(32))
                    .instruction(&W::I64Shl);
                load_slot(body, instruction.b);
                body.instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Or)
                    .instruction(&W::I64Store(memarg(0)));
                store_prefix(body, instruction.a + 1);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I64Store(memarg(0)));
            } else if let Some(array_layout) = array_layout.filter(|_| layout.result_slots > 0) {
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ALLOC_LOCAL));
                if array_layout.elem_bytes < 8 {
                    for index in 0..array_layout.len {
                        // The assertion helper checked the complete result
                        // width against the u16 frame ABI before this loop.
                        store_prefix(body, instruction.a + index as u16);
                        body.instruction(&W::LocalGet(ALLOC_LOCAL));
                        let offset = u64::from(index) * u64::from(array_layout.elem_bytes);
                        match (array_layout.elem_bytes, array_layout.needs_sign_extend) {
                            (1, false) => body.instruction(&W::I64Load8U(MemArg {
                                offset,
                                align: 0,
                                memory_index: 0,
                            })),
                            (1, true) => body.instruction(&W::I64Load8S(MemArg {
                                offset,
                                align: 0,
                                memory_index: 0,
                            })),
                            (2, false) => body.instruction(&W::I64Load16U(MemArg {
                                offset,
                                align: 1,
                                memory_index: 0,
                            })),
                            (2, true) => body.instruction(&W::I64Load16S(MemArg {
                                offset,
                                align: 1,
                                memory_index: 0,
                            })),
                            (4, false) => body.instruction(&W::I64Load32U(MemArg {
                                offset,
                                align: 2,
                                memory_index: 0,
                            })),
                            (4, true) => body.instruction(&W::I64Load32S(MemArg {
                                offset,
                                align: 2,
                                memory_index: 0,
                            })),
                            _ => unreachable!("packed interface array layout was validated"),
                        };
                        body.instruction(&W::I64Store(memarg(0)));
                    }
                } else {
                    store_prefix(body, instruction.a);
                    body.instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I32Const(i32::from(layout.result_slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            } else if layout.result_slots > 0 {
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0xff))
                    .instruction(&W::I32And)
                    .instruction(&W::LocalSet(LENGTH_LOCAL))
                    .instruction(&W::LocalGet(LENGTH_LOCAL))
                    .instruction(&W::I32Const(14))
                    .instruction(&W::I32Eq)
                    .instruction(&W::LocalGet(LENGTH_LOCAL))
                    .instruction(&W::I32Const(15))
                    .instruction(&W::I32Eq)
                    .instruction(&W::I32Or)
                    .instruction(&W::If(BlockType::Empty));
                store_prefix(body, instruction.a);
                body.instruction(&W::LocalGet(LENGTH_LOCAL))
                    .instruction(&W::I32Const(14))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Result(ValType::I32)));
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Else);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::End)
                    .instruction(&W::I32Const(i32::from(layout.result_slots) * 8))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    })
                    .instruction(&W::Else);
                store_prefix(body, instruction.a);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I64Store(memarg(0)))
                    .instruction(&W::End);
            }
            if has_ok {
                store_const(body, instruction.a + layout.result_slots, 1);
            }
            body.instruction(&W::Else);
            if !has_ok {
                return_runtime_panic(
                    body,
                    static_data.runtime_panic_refs[STATUS_TYPE_ASSERTION_FAILED as usize],
                    current_block,
                );
            }
            body.instruction(&W::End);
        }
        Opcode::IfaceEq => {
            store_const(body, instruction.a, 0);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64And);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64And)
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            // Composite equality is ordered and short-circuiting. A nested
            // interface with an uncomparable dynamic value only panics if the
            // comparison actually reaches that field or array element.
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed));
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(0xff))
                .instruction(&W::I32And)
                .instruction(&W::LocalSet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Eq)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(18))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Or)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(19))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Or)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(21))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Or)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_DYNAMIC_EQUALITY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(17))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
                .instruction(&W::I32Eqz)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(12))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32)
                .instruction(&W::F32Eq)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(13))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::F64ReinterpretI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F64Eq)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else);
            body.instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(ValueKind::Array as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(ValueKind::Struct as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else);
            load_slot(body, instruction.b + 1);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Eq)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End);
            body.instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_DYNAMIC_EQUALITY as usize],
                current_block,
            );
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        _ => unreachable!("frame instruction family selected by dispatcher"),
    }
    Ok(false)
}
