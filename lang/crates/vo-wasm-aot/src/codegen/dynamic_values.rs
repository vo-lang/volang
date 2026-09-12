//! Dynamic field, index, and assignment operations.
use super::*;

pub(super) fn emit_interface_identity_matches(body: &mut Function, slot: u16, value_rttid: u32) {
    load_slot(body, slot);
    body.instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64And)
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(value_rttid as i32))
        .instruction(&W::I32Eq);
}

pub(super) fn emit_dynamic_integer_kind(body: &mut Function, slot: u16) {
    load_slot(body, slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Int as i32))
        .instruction(&W::I32Eq);
    for kind in [
        ValueKind::Int8,
        ValueKind::Int16,
        ValueKind::Int32,
        ValueKind::Int64,
        ValueKind::Uint,
        ValueKind::Uint8,
        ValueKind::Uint16,
        ValueKind::Uint32,
        ValueKind::Uint64,
    ] {
        body.instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32Const(kind as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::I32Or);
    }
}

pub(super) fn emit_load_packed_element(body: &mut Function, kind: ValueKind, bytes: u32) {
    body.instruction(&match (bytes, kind) {
        (1, ValueKind::Int8) => W::I64Load8S(packed_memarg()),
        (1, _) => W::I64Load8U(packed_memarg()),
        (2, ValueKind::Int16) => W::I64Load16S(packed_memarg()),
        (2, _) => W::I64Load16U(packed_memarg()),
        (4, ValueKind::Int32) => W::I64Load32S(packed_memarg()),
        (4, _) => W::I64Load32U(packed_memarg()),
        (8, _) => W::I64Load(memarg(0)),
        _ => unreachable!("validated dynamic scalar width"),
    });
}

pub(super) fn emit_store_packed_element(body: &mut Function, bytes: u32) {
    body.instruction(&match bytes {
        1 => W::I64Store8(packed_memarg()),
        2 => W::I64Store16(packed_memarg()),
        4 => W::I64Store32(packed_memarg()),
        8 => W::I64Store(memarg(0)),
        _ => unreachable!("validated dynamic scalar width"),
    });
}

pub(super) fn dynamic_element_bytes(
    module: &ModuleAnalysis<'_>,
    elem: ValueRttid,
) -> Result<(u32, usize), WasmAotError> {
    let layout = module.slot_layout_for_value_rttid(elem).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "dynamic element runtime type {} has no slot layout",
            elem.rttid()
        ))
    })?;
    let bytes = match elem.value_kind() {
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        _ => u32::try_from(layout.len().checked_mul(8).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic element layout overflows wasm32".into())
        })?)
        .map_err(|_| WasmAotError::InvalidModule("dynamic element layout exceeds wasm32".into()))?,
    };
    Ok((bytes, layout.len()))
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_box_from_address(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    actual: ValueRttid,
    source_address_local: u32,
    source_bytes: u32,
    destination: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    match actual.value_kind() {
        ValueKind::Interface => {
            store_prefix(body, destination);
            body.instruction(&W::LocalGet(source_address_local))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(source_address_local))
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)));
        }
        ValueKind::Struct => {
            let struct_meta_id = dynamic_struct_meta_id(module, actual).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic struct runtime type {} has no struct metadata",
                    actual.rttid()
                ))
            })?;
            let slots = module
                .slot_layout_for_value_rttid(actual)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic struct layout is missing".into())
                })?
                .len();
            let bytes = u32::try_from(slots.checked_mul(8).ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic struct layout overflows wasm32".into())
            })?)
            .map_err(|_| {
                WasmAotError::InvalidModule("dynamic struct layout exceeds wasm32".into())
            })?;
            body.instruction(&W::I32Const(bytes as i32));
            select_allocation_descriptor(
                body,
                *descriptors
                    .fixed_by_struct_meta
                    .get(&struct_meta_id)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic struct allocation descriptor is missing".into(),
                        )
                    })?,
                globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End);
            if bytes > 0 {
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::LocalGet(source_address_local))
                    .instruction(&W::I32Const(bytes as i32))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
            store_const(body, destination, i64::from(actual.to_raw()));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        ValueKind::Array => {
            let Some((_, RuntimeType::Array { len, elem })) =
                module.runtime_type_resolver().resolve_value_rttid(actual)
            else {
                return Err(WasmAotError::InvalidModule(
                    "dynamic array metadata is missing".into(),
                ));
            };
            let (elem_bytes, elem_slots) = dynamic_element_bytes(module, *elem)?;
            let len = u32::try_from(*len).map_err(|_| {
                WasmAotError::InvalidModule("dynamic array length exceeds wasm32".into())
            })?;
            let bytes = len
                .checked_mul(elem_bytes)
                .and_then(|bytes| bytes.checked_add(32))
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic array allocation overflows wasm32".into())
                })?;
            body.instruction(&W::I32Const(bytes as i32));
            let value_meta =
                ValueMeta::try_new(actual.rttid(), ValueKind::Array).ok_or_else(|| {
                    WasmAotError::InvalidModule(
                        "dynamic array metadata exceeds packed domain".into(),
                    )
                })?;
            select_allocation_descriptor(
                body,
                *descriptors
                    .sequence_by_meta
                    .get(&value_meta.to_raw())
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic array allocation descriptor is missing".into(),
                        )
                    })?,
                globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(32))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(len)))
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(len)))
                .instruction(&W::I64Store(MemArg {
                    offset: 16,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(elem_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }));
            for index in 0..len {
                if elem_bytes == 0 {
                    break;
                }
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::I32Const(
                        32 + i32::try_from(index * elem_bytes).unwrap_or(i32::MAX),
                    ))
                    .instruction(&W::I32Add);
                if elem_slots == 1 && matches!(elem_bytes, 1 | 2 | 4 | 8) {
                    body.instruction(&W::LocalGet(source_address_local))
                        .instruction(&W::I64Load(MemArg {
                            offset: u64::from(index) * 8,
                            align: 3,
                            memory_index: 0,
                        }));
                    emit_store_packed_element(body, elem_bytes);
                } else {
                    body.instruction(&W::LocalGet(source_address_local))
                        .instruction(&W::I32Const(
                            i32::try_from(index as usize * elem_slots * 8).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add)
                        .instruction(&W::I32Const(elem_bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
            store_const(body, destination, i64::from(actual.to_raw()));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        _ => {
            store_const(body, destination, i64::from(actual.to_raw()));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(source_address_local));
            if matches!(source_bytes, 1 | 2 | 4 | 8) {
                emit_load_packed_element(body, actual.value_kind(), source_bytes);
            } else {
                return Err(WasmAotError::InvalidModule(format!(
                    "dynamic scalar source width {source_bytes} is invalid"
                )));
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
    }
    Ok(())
}

pub(super) fn emit_dynamic_expected_type_matches(
    body: &mut Function,
    expected_rttid_slot: u16,
    expected_kind_slot: u16,
    target: ValueRttid,
) {
    load_slot(body, expected_rttid_slot);
    body.instruction(&W::I64Const(i64::from(target.rttid())))
        .instruction(&W::I64Eq);
    load_slot(body, expected_kind_slot);
    body.instruction(&W::I64Const(i64::from(target.value_kind() as u8)))
        .instruction(&W::I64Eq)
        .instruction(&W::I32And);
}

pub(super) fn dynamic_kind_accepts_nil(kind: ValueKind) -> bool {
    matches!(
        kind,
        ValueKind::Interface
            | ValueKind::Pointer
            | ValueKind::Slice
            | ValueKind::Map
            | ValueKind::Closure
            | ValueKind::Channel
            | ValueKind::Port
            | ValueKind::Island
    )
}

pub(super) fn emit_dynamic_value_assignable(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    value_slot0: u16,
    target: ValueRttid,
) {
    let mut emitted = false;
    if dynamic_kind_accepts_nil(target.value_kind()) {
        load_slot(body, value_slot0);
        body.instruction(&W::I64Eqz);
        emitted = true;
    }
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(source) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if !runtime_value_is_assignable(source, target, module) {
            continue;
        }
        emit_interface_identity_matches(body, value_slot0, source.to_raw());
        if emitted {
            body.instruction(&W::I32Or);
        }
        emitted = true;
    }
    if !emitted {
        body.instruction(&W::I32Const(0));
    }
}

pub(super) fn emit_dynamic_integer_value(body: &mut Function, target: ValueKind, value_slot: u16) {
    load_slot(body, value_slot);
    match target {
        ValueKind::Int8 => {
            body.instruction(&W::I64Const(56))
                .instruction(&W::I64Shl)
                .instruction(&W::I64Const(56))
                .instruction(&W::I64ShrS);
        }
        ValueKind::Int16 => {
            body.instruction(&W::I64Const(48))
                .instruction(&W::I64Shl)
                .instruction(&W::I64Const(48))
                .instruction(&W::I64ShrS);
        }
        ValueKind::Int32 => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32S);
        }
        ValueKind::Uint8 => {
            body.instruction(&W::I64Const(0xff)).instruction(&W::I64And);
        }
        ValueKind::Uint16 => {
            body.instruction(&W::I64Const(0xffff))
                .instruction(&W::I64And);
        }
        ValueKind::Uint32 => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32U);
        }
        _ => {}
    }
}

pub(super) fn emit_dynamic_store_value(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    target: ValueRttid,
    value_slot0: u16,
    value_slot1: u16,
    destination_address_local: u32,
    destination_bytes: u32,
) -> Result<(), WasmAotError> {
    match target.value_kind() {
        ValueKind::Interface => {
            let target_meta_id = module
                .runtime_type_resolver()
                .resolve_value_rttid(target)
                .and_then(|(_, runtime_type)| match runtime_type {
                    RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
                    _ => None,
                })
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(
                        "dynamic interface target metadata is missing".into(),
                    )
                })?;
            body.instruction(&W::LocalGet(destination_address_local));
            load_slot(body, value_slot0);
            if target_meta_id != 0 {
                body.instruction(&W::LocalTee(PACKED_LOCAL))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(PACKED_LOCAL))
                    .instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Const(i64::from(target_meta_id) << 32))
                    .instruction(&W::I64Or)
                    .instruction(&W::End);
            }
            body.instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(destination_address_local));
            load_slot(body, value_slot1);
            body.instruction(&W::I64Store(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }));
        }
        ValueKind::Struct => {
            let bytes = u32::try_from(
                module
                    .slot_layout_for_value_rttid(target)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic struct target layout is missing".into(),
                        )
                    })?
                    .len()
                    .checked_mul(8)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic struct target layout overflows wasm32".into(),
                        )
                    })?,
            )
            .map_err(|_| {
                WasmAotError::InvalidModule("dynamic struct target exceeds wasm32".into())
            })?;
            if bytes > 0 {
                body.instruction(&W::LocalGet(destination_address_local));
                load_slot(body, value_slot1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(bytes as i32))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
        }
        ValueKind::Array => {
            let Some((_, RuntimeType::Array { len, elem })) =
                module.runtime_type_resolver().resolve_value_rttid(target)
            else {
                return Err(WasmAotError::InvalidModule(
                    "dynamic array target metadata is missing".into(),
                ));
            };
            let (elem_bytes, elem_slots) = dynamic_element_bytes(module, *elem)?;
            load_slot(body, value_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(ALLOC_LOCAL));
            for index in 0..u32::try_from(*len).unwrap_or(u32::MAX) {
                if elem_bytes == 0 {
                    break;
                }
                if elem_slots == 1 && matches!(elem_bytes, 1 | 2 | 4 | 8) {
                    body.instruction(&W::LocalGet(destination_address_local))
                        .instruction(&W::I32Const(i32::try_from(index * 8).unwrap_or(i32::MAX)))
                        .instruction(&W::I32Add)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I32Const(
                            i32::try_from(index * elem_bytes).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add);
                    emit_load_packed_element(body, elem.value_kind(), elem_bytes);
                    body.instruction(&W::I64Store(memarg(0)));
                } else {
                    body.instruction(&W::LocalGet(destination_address_local))
                        .instruction(&W::I32Const(
                            i32::try_from(index as usize * elem_slots * 8).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I32Const(
                            i32::try_from(index * elem_bytes).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add)
                        .instruction(&W::I32Const(elem_bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
        }
        kind if matches!(
            kind,
            ValueKind::Int
                | ValueKind::Int8
                | ValueKind::Int16
                | ValueKind::Int32
                | ValueKind::Int64
                | ValueKind::Uint
                | ValueKind::Uint8
                | ValueKind::Uint16
                | ValueKind::Uint32
                | ValueKind::Uint64
        ) =>
        {
            body.instruction(&W::LocalGet(destination_address_local));
            emit_dynamic_integer_value(body, kind, value_slot1);
            emit_store_packed_element(body, destination_bytes);
        }
        _ => {
            body.instruction(&W::LocalGet(destination_address_local));
            load_slot(body, value_slot1);
            if destination_bytes == 8 {
                body.instruction(&W::I64Store(memarg(0)));
            } else {
                emit_store_packed_element(body, destination_bytes);
            }
        }
    }
    Ok(())
}

pub(super) fn dynamic_integer_kind(kind: ValueKind) -> bool {
    matches!(
        kind,
        ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64
    )
}

pub(super) fn emit_dynamic_value_compatible(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    value_slot0: u16,
    target: ValueRttid,
) {
    if dynamic_integer_kind(target.value_kind()) {
        emit_dynamic_integer_kind(body, value_slot0);
    } else {
        emit_dynamic_value_assignable(body, module, value_slot0, target);
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_prepare_scratch(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    target: ValueRttid,
    value_slot0: u16,
    value_slot1: u16,
    frame_scratch: u16,
    frame_scratch_slots: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<u32, WasmAotError> {
    let slots = module
        .slot_layout_for_value_rttid(target)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic scratch target layout is missing".into())
        })?
        .len();
    let bytes = u32::try_from(slots.checked_mul(8).ok_or_else(|| {
        WasmAotError::InvalidModule("dynamic scratch target layout overflows wasm32".into())
    })?)
    .map_err(|_| WasmAotError::InvalidModule("dynamic scratch target exceeds wasm32".into()))?;
    if slots <= usize::from(frame_scratch_slots) {
        store_prefix(body, frame_scratch);
        body.instruction(&W::LocalSet(SEQUENCE_LOCAL));
    } else {
        body.instruction(&W::I32Const(bytes as i32));
        select_allocation_descriptor(
            body,
            *descriptors
                .fixed_by_value
                .get(&target.to_raw())
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(
                        "dynamic scratch allocation descriptor is missing".into(),
                    )
                })?,
            globals,
        );
        body.instruction(&W::Call(1))
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End);
    }
    emit_dynamic_store_value(
        body,
        module,
        target,
        value_slot0,
        value_slot1,
        SEQUENCE_LOCAL,
        bytes,
    )?;
    Ok(bytes)
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_get_success(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    actual: ValueRttid,
    instruction: vo_common_core::instruction::Instruction,
    expected: Option<(u16, u16)>,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    let Some((expected_rttid_slot, expected_kind_slot)) = expected else {
        store_const(body, instruction.a + 2, 0);
        store_const(body, instruction.a + 3, 0);
        return Ok(());
    };

    body.instruction(&W::Block(BlockType::Empty));
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(target) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if !runtime_value_is_assignable(actual, target, module) {
            continue;
        }
        emit_dynamic_expected_type_matches(body, expected_rttid_slot, expected_kind_slot, target);
        body.instruction(&W::If(BlockType::Empty));
        match target.value_kind() {
            ValueKind::Interface => {
                let target_meta_id = module
                    .runtime_type_resolver()
                    .resolve_value_rttid(target)
                    .and_then(|(_, runtime_type)| match runtime_type {
                        RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
                        _ => None,
                    })
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic interface target metadata is missing".into(),
                        )
                    })?;
                if target_meta_id != 0 {
                    load_slot(body, instruction.a);
                    body.instruction(&W::I64Eqz)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::Else);
                    store_prefix(body, instruction.a);
                    load_slot(body, instruction.a);
                    body.instruction(&W::I64Const(i64::from(u32::MAX)))
                        .instruction(&W::I64And)
                        .instruction(&W::I64Const(i64::from(target_meta_id) << 32))
                        .instruction(&W::I64Or)
                        .instruction(&W::I64Store(memarg(0)))
                        .instruction(&W::End);
                }
            }
            ValueKind::Array => {
                store_const(body, instruction.a, 0);
            }
            ValueKind::Struct => {
                let width = module
                    .slot_layout_for_value_rttid(target)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic expected struct layout is missing".into(),
                        )
                    })?
                    .len();
                if width <= 2 {
                    load_slot(body, instruction.a + 1);
                    body.instruction(&W::I32WrapI64)
                        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
                    if width > 0 {
                        store_prefix(body, instruction.a);
                        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                            .instruction(&W::I64Load(memarg(0)))
                            .instruction(&W::I64Store(memarg(0)));
                    } else {
                        store_const(body, instruction.a, 0);
                    }
                    if width > 1 {
                        store_prefix(body, instruction.a + 1);
                        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                            .instruction(&W::I64Load(MemArg {
                                offset: 8,
                                align: 3,
                                memory_index: 0,
                            }))
                            .instruction(&W::I64Store(memarg(0)));
                    } else {
                        store_const(body, instruction.a + 1, 0);
                    }
                } else {
                    store_const(body, instruction.a, 0);
                }
            }
            _ => {
                store_prefix(body, instruction.a);
                load_slot(body, instruction.a + 1);
                body.instruction(&W::I64Store(memarg(0)));
                store_const(body, instruction.a + 1, 0);
            }
        }
        store_const(body, instruction.a + 2, 0);
        store_const(body, instruction.a + 3, 0);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(
            DynamicErrorKind::TypeMismatch,
            "dynamic target type mismatch",
        ),
    )?;
    body.instruction(&W::End);
    Ok(())
}

pub(super) fn dynamic_basic_value_rttid(
    module: &ModuleAnalysis<'_>,
    kind: ValueKind,
) -> Result<ValueRttid, WasmAotError> {
    module
        .value_rttid_for_rttid(kind as u32)
        .filter(|value| value.value_kind() == kind)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!("dynamic runtime requires {kind:?} type metadata"))
        })
}

pub(super) fn dynamic_map_key_source_assignable(
    module: &ModuleAnalysis<'_>,
    target: ValueRttid,
    source: DynamicMapKeySource,
) -> Result<bool, WasmAotError> {
    match source {
        DynamicMapKeySource::Boxed { .. } => Ok(true),
        DynamicMapKeySource::FieldName { .. } => Ok(runtime_value_is_assignable(
            dynamic_basic_value_rttid(module, ValueKind::String)?,
            target,
            module,
        )),
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_prepare_map_key(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    target: ValueRttid,
    source: DynamicMapKeySource,
    frame_scratch: u16,
    frame_scratch_slots: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<u32, WasmAotError> {
    match source {
        DynamicMapKeySource::Boxed { slot0, slot1 } => emit_dynamic_prepare_scratch(
            body,
            module,
            target,
            slot0,
            slot1,
            frame_scratch,
            frame_scratch_slots,
            descriptors,
            globals,
        ),
        DynamicMapKeySource::FieldName { slot } => {
            let slots = module
                .slot_layout_for_value_rttid(target)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map key layout is missing".into())
                })?
                .len();
            let bytes = u32::try_from(slots.checked_mul(8).ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic map key layout overflows wasm32".into())
            })?)
            .map_err(|_| WasmAotError::InvalidModule("dynamic map key exceeds wasm32".into()))?;
            if slots > usize::from(frame_scratch_slots) {
                return Err(WasmAotError::InvalidModule(
                    "dynamic map field key exceeds its scratch window".into(),
                ));
            }
            store_prefix(body, frame_scratch);
            body.instruction(&W::LocalSet(SEQUENCE_LOCAL));
            match target.value_kind() {
                ValueKind::String => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, slot);
                    body.instruction(&W::I64Store(memarg(0)));
                }
                ValueKind::Interface => {
                    let target_meta_id = module
                        .runtime_type_resolver()
                        .resolve_value_rttid(target)
                        .and_then(|(_, runtime_type)| match runtime_type {
                            RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
                            _ => None,
                        })
                        .ok_or_else(|| {
                            WasmAotError::InvalidModule(
                                "dynamic map interface key metadata is missing".into(),
                            )
                        })?;
                    let string_value = dynamic_basic_value_rttid(module, ValueKind::String)?;
                    let slot0 =
                        u64::from(string_value.to_raw()) | (u64::from(target_meta_id) << 32);
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                        .instruction(&W::I64Const(slot0 as i64))
                        .instruction(&W::I64Store(memarg(0)))
                        .instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, slot);
                    body.instruction(&W::I64Store(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                _ => {
                    return Err(WasmAotError::InvalidModule(
                        "dynamic map field key is not string-compatible".into(),
                    ));
                }
            }
            Ok(bytes)
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_protocol_get(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    iface_meta_id: Option<u32>,
    is_field: bool,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = iface_meta_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let Some(wasm_target) = function_indices.get(&target).copied() else {
            continue;
        };
        let callee = &module.functions[target as usize];
        let expected_params = if is_field { 2 } else { 3 };
        if callee.param_slots != expected_params || callee.ret_slots != 4 {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic protocol target {target} has an invalid Core-Wasm ABI"
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        let caller_base = instruction
            .a
            .checked_sub(callee.param_slots)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} has no scratch window for dynamic protocol arguments",
                    caller.name
                ))
            })?;
        // Protocol arguments are already contiguous after the boxed receiver.
        // The compiler-provided scratch window may overlap the tail of that
        // source range, so one Wasm memory.copy (memmove semantics) is required
        // here; slot-by-slot stores can overwrite a later source slot.
        if !materialized.contains(&target) {
            store_prefix(body, caller_base);
            store_prefix(body, instruction.c + 1);
            body.instruction(&W::I32Const(i32::from(callee.param_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        compile_call_target(
            body,
            module,
            caller,
            pc,
            target,
            wasm_target,
            caller_base,
            MaterializedCallArguments::Contiguous {
                source: instruction.c + 1,
            },
            current_block,
            materialized,
            globals,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_map_get(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    instruction: vo_common_core::instruction::Instruction,
    key_source: DynamicMapKeySource,
    expected: Option<(u16, u16)>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Map { key, val })) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let key = *key;
        let val = *val;
        if !dynamic_map_key_source_assignable(module, key, key_source)? {
            continue;
        }
        let key_slots = module
            .slot_layout_for_value_rttid(key)
            .ok_or_else(|| WasmAotError::InvalidModule("dynamic map key layout is missing".into()))?
            .len();
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(
                DynamicErrorKind::NilBase,
                match key_source {
                    DynamicMapKeySource::Boxed { .. } => "cannot index nil map",
                    DynamicMapKeySource::FieldName { .. } => "cannot access field on nil map",
                },
            ),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        if let DynamicMapKeySource::Boxed { slot0, .. } = key_source {
            emit_dynamic_value_compatible(body, module, slot0, key);
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            emit_dynamic_get_error(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                DynamicErrorSpec::new(DynamicErrorKind::BadIndex, "map key type mismatch"),
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_prepare_map_key(
            body,
            module,
            key,
            key_source,
            instruction.a,
            4,
            descriptors,
            globals,
        )?;
        body.instruction(&W::I32Const(0))
            .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(0))
            .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(
                match key_source {
                    DynamicMapKeySource::Boxed { .. } => DynamicErrorKind::BadIndex,
                    DynamicMapKeySource::FieldName { .. } => DynamicErrorKind::BadField,
                },
                "map key is not hashable",
            ),
        )?;
        body.instruction(&W::Else);
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadField, "map key not found"),
        )?;
        body.instruction(&W::End)
            .instruction(&W::Br(2))
            .instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(
                8 + i32::try_from(key_slots * 8).unwrap_or(i32::MAX),
            ))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_box_from_address(
            body,
            module,
            val,
            SEQUENCE_LOCAL,
            8,
            instruction.a,
            descriptors,
            globals,
        )?;
        emit_dynamic_get_success(
            body,
            module,
            val,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_index_get(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let arg_slots = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .map(|layout| layout.0)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing dynamic index CallLayout metadata",
                caller.name
            ))
        })?;
    let expected = match arg_slots {
        4 => None,
        6 => Some((instruction.c + 4, instruction.c + 5)),
        _ => {
            return Err(WasmAotError::InvalidModule(format!(
                "{} pc {pc} dynamic index ABI has {arg_slots} argument slots",
                caller.name
            )));
        }
    };
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(DynamicErrorKind::NilBase, "cannot index nil"),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_get(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.index_object_iface_id,
        false,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_map_get(
        body,
        module,
        instruction,
        DynamicMapKeySource::Boxed {
            slot0: instruction.c + 2,
            slot1: instruction.c + 3,
        },
        expected,
        globals,
        static_data,
        descriptors,
    )?;
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, runtime_type)) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let (elem, constant_len, bounds_message, nil_message) = match runtime_type {
            RuntimeType::Array { len, elem } => {
                (*elem, Some(*len), "array index out of bounds", None)
            }
            RuntimeType::Slice(elem) => (
                *elem,
                None,
                "slice index out of bounds",
                Some("cannot index nil slice"),
            ),
            _ => continue,
        };
        let (elem_bytes, _) = dynamic_element_bytes(module, elem)?;
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        if let Some(nil_message) = nil_message {
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            emit_dynamic_get_error(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                DynamicErrorSpec::new(DynamicErrorKind::NilBase, nil_message),
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_integer_kind(body, instruction.c + 2);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadIndex, "index must be integer"),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        if let Some(len) = constant_len {
            body.instruction(&W::I32Const(i32::try_from(len).unwrap_or(-1)))
                .instruction(&W::LocalSet(LENGTH_LOCAL));
        } else {
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LENGTH_LOCAL));
        }
        load_slot(body, instruction.c + 3);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64LtS)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64GeU)
            .instruction(&W::I32Or)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::OutOfBounds, bounds_message),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(elem_bytes as i32))
            .instruction(&W::I32Mul)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_box_from_address(
            body,
            module,
            elem,
            SEQUENCE_LOCAL,
            elem_bytes,
            instruction.a,
            descriptors,
            globals,
        )?;
        emit_dynamic_get_success(
            body,
            module,
            elem,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    let uint8_value = module
        .value_rttid_for_rttid(ValueKind::Uint8 as u32)
        .filter(|value| value.value_kind() == ValueKind::Uint8)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic string indexing requires uint8 metadata".into())
        })?;
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if value_rttid.value_kind() != ValueKind::String {
            continue;
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        emit_dynamic_integer_kind(body, instruction.c + 2);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadIndex, "index must be integer"),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 3);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64LtS);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64LeU)
            .instruction(&W::I32Or)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::OutOfBounds, "string index out of bounds"),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_box_from_address(
            body,
            module,
            uint8_value,
            SEQUENCE_LOCAL,
            1,
            instruction.a,
            descriptors,
            globals,
        )?;
        emit_dynamic_get_success(
            body,
            module,
            uint8_value,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(
            DynamicErrorKind::TypeMismatch,
            "type does not support this access",
        ),
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_method_get_for_value(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    value_rttid: ValueRttid,
    instruction: vo_common_core::instruction::Instruction,
    expected: Option<(u16, u16)>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
    function_indices: &BTreeMap<u32, u32>,
) -> Result<(), WasmAotError> {
    let Some(named_id) = module.named_type_id_for_rttid(value_rttid.rttid()) else {
        return Ok(());
    };
    let named = module
        .named_type_metas
        .get(named_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic named type metadata is missing".into())
        })?;
    for (name, method) in &named.methods {
        if !is_exported_name(name)
            || method.is_pointer_receiver && value_rttid.value_kind() != ValueKind::Pointer
        {
            continue;
        }
        method
            .iface_receiver_slot_type_for_source_kind(value_rttid.value_kind())
            .map_err(|message| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {} receiver metadata is invalid: {message}",
                    method.func_id
                ))
            })?;
        let target = module
            .functions
            .get(method.func_id as usize)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {name} references missing function {}",
                    method.func_id
                ))
            })?;
        if !function_indices.contains_key(&method.func_id) {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic method {name} target {} is outside the AOT image",
                method.func_id
            )));
        }
        let signature = ValueRttid::try_new(method.signature_rttid, ValueKind::Closure)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {name} signature exceeds the packed RTTID domain"
                ))
            })?;
        emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const((u32::from(target.recv_slots) + 1) as i32 * 8));
        select_allocation_descriptor(
            body,
            *descriptors
                .closure_by_function
                .get(&method.func_id)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "dynamic method {name} closure descriptor is missing"
                    ))
                })?,
            globals,
        );
        body.instruction(&W::Call(1))
            .instruction(&W::LocalTee(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(
                ((u64::from(target.recv_slots) << 32) | u64::from(method.func_id)) as i64,
            ))
            .instruction(&W::I64Store(memarg(0)));
        if target.recv_slots == 1 {
            body.instruction(&W::LocalGet(ALLOC_LOCAL));
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Store(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }));
        } else if target.recv_slots > 1 {
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Add);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(target.recv_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        store_const(body, instruction.a, i64::from(signature.to_raw()));
        store_prefix(body, instruction.a + 1);
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64Store(memarg(0)));
        emit_dynamic_get_success(
            body,
            module,
            signature,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_field_get(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let arg_slots = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .map(|layout| layout.0)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing dynamic field CallLayout metadata",
                caller.name
            ))
        })?;
    let expected = match arg_slots {
        3 => None,
        5 => Some((instruction.c + 3, instruction.c + 4)),
        _ => {
            return Err(WasmAotError::InvalidModule(format!(
                "{} pc {pc} dynamic field ABI has {arg_slots} argument slots",
                caller.name
            )));
        }
    };
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(DynamicErrorKind::NilBase, "cannot access field on nil"),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_get(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.attr_object_iface_id,
        true,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_map_get(
        body,
        module,
        instruction,
        DynamicMapKeySource::FieldName {
            slot: instruction.c + 2,
        },
        expected,
        globals,
        static_data,
        descriptors,
    )?;
    let field_names: BTreeSet<&str> = module
        .struct_metas
        .iter()
        .flat_map(|metadata| metadata.fields.iter())
        .filter_map(dynamic_field_name)
        .collect();
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(struct_meta_id) = dynamic_struct_meta_id(module, value_rttid) else {
            continue;
        };
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        for name in &field_names {
            let DynamicFieldLookup::Found(field) =
                lookup_dynamic_field(module, struct_meta_id as usize, name)
            else {
                continue;
            };
            emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
            body.instruction(&W::If(BlockType::Empty));
            if value_rttid.value_kind() == ValueKind::Pointer {
                load_slot(body, instruction.c + 1);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty));
                emit_dynamic_get_error(
                    body,
                    module,
                    descriptors,
                    globals,
                    static_data,
                    instruction.a,
                    DynamicErrorSpec::new(DynamicErrorKind::NilBase, "cannot access field on nil"),
                )?;
                body.instruction(&W::Br(3)).instruction(&W::End);
            }
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            for deref in &field.ptr_derefs {
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(deref.offset) * 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                emit_dynamic_get_error(
                    body,
                    module,
                    descriptors,
                    globals,
                    static_data,
                    instruction.a,
                    DynamicErrorSpec::new(
                        DynamicErrorKind::NilBase,
                        "nil pointer in embedding path",
                    ),
                )?;
                body.instruction(&W::Br(3)).instruction(&W::End);
            }
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(i32::from(field.offset) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            emit_dynamic_box_from_address(
                body,
                module,
                field.value_rttid,
                SEQUENCE_LOCAL,
                8,
                instruction.a,
                descriptors,
                globals,
            )?;
            emit_dynamic_get_success(
                body,
                module,
                field.value_rttid,
                instruction,
                expected,
                descriptors,
                globals,
                static_data,
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        compile_dynamic_method_get_for_value(
            body,
            module,
            value_rttid,
            instruction,
            expected,
            globals,
            static_data,
            descriptors,
            function_indices,
        )?;
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadField, "field not found"),
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if dynamic_struct_meta_id(module, value_rttid).is_some()
            || module.named_type_id_for_rttid(rttid).is_none()
        {
            continue;
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        compile_dynamic_method_get_for_value(
            body,
            module,
            value_rttid,
            instruction,
            expected,
            globals,
            static_data,
            descriptors,
            function_indices,
        )?;
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadField, "field not found"),
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(
            DynamicErrorKind::TypeMismatch,
            "type does not support this access",
        ),
    )?;
    body.instruction(&W::End);
    Ok(())
}

pub(super) fn dynamic_struct_meta_id(
    module: &ModuleAnalysis<'_>,
    value_rttid: ValueRttid,
) -> Option<u32> {
    let resolver = module.runtime_type_resolver();
    let (_, runtime_type) = resolver.resolve_value_rttid(value_rttid)?;
    let struct_value = match runtime_type {
        RuntimeType::Struct { meta_id, .. } => return Some(*meta_id),
        RuntimeType::Pointer(inner) => *inner,
        _ => return None,
    };
    let (_, RuntimeType::Struct { meta_id, .. }) = resolver.resolve_value_rttid(struct_value)?
    else {
        return None;
    };
    Some(*meta_id)
}

pub(super) fn emit_dynamic_name_matches(
    body: &mut Function,
    name_slot: u16,
    static_data: &StaticData,
    name: &str,
) -> Result<(), WasmAotError> {
    load_slot(body, name_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(dynamic_string_ref(static_data, name)? as i32))
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_protocol_set(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    iface_meta_id: Option<u32>,
    is_field: bool,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = iface_meta_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let Some(wasm_target) = function_indices.get(&target).copied() else {
            continue;
        };
        let callee = &module.functions[target as usize];
        let expected_params = if is_field { 4 } else { 5 };
        if callee.param_slots != expected_params || callee.ret_slots != 2 {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic setter protocol target {target} has an invalid Core-Wasm ABI"
            )));
        }
        let caller_base = instruction.c + 1;
        let return_start = caller_base.checked_add(callee.param_slots).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic setter scratch window overflows u16".into())
        })?;
        if return_start + callee.ret_slots > caller.local_slots {
            return Err(WasmAotError::InvalidModule(format!(
                "{} pc {pc} has no scratch window for dynamic setter results",
                caller.name
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        compile_call_target(
            body,
            module,
            caller,
            pc,
            target,
            wasm_target,
            caller_base,
            MaterializedCallArguments::Contiguous {
                source: caller_base,
            },
            current_block,
            materialized,
            globals,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
        )?;
        store_prefix(body, instruction.a);
        store_prefix(body, return_start);
        body.instruction(&W::I32Const(16))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            })
            .instruction(&W::Br(1))
            .instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_field_set(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "cannot set field on nil",
        Some(DynamicErrorKind::NilBase),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_set(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.set_attr_object_iface_id,
        true,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_map_set(
        body,
        module,
        instruction,
        DynamicMapKeySource::FieldName {
            slot: instruction.c + 2,
        },
        instruction.c + 3,
        instruction.c + 4,
        globals,
        static_data,
        descriptors,
    )?;
    let field_names: BTreeSet<&str> = module
        .struct_metas
        .iter()
        .flat_map(|metadata| metadata.fields.iter())
        .filter_map(dynamic_field_name)
        .collect();
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(struct_meta_id) = dynamic_struct_meta_id(module, value_rttid) else {
            continue;
        };
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        for name in &field_names {
            let DynamicFieldLookup::Found(field) =
                lookup_dynamic_field(module, struct_meta_id as usize, name)
            else {
                continue;
            };
            emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
            body.instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            for deref in &field.ptr_derefs {
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(deref.offset) * 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                emit_dynamic_error_object(
                    body,
                    module,
                    descriptors,
                    globals,
                    static_data,
                    instruction.a,
                    "nil pointer in embedding path",
                    Some(DynamicErrorKind::NilBase),
                )?;
                body.instruction(&W::Br(3)).instruction(&W::End);
            }
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(i32::from(field.offset) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            emit_dynamic_value_compatible(body, module, instruction.c + 3, field.value_rttid);
            body.instruction(&W::If(BlockType::Empty));
            emit_dynamic_store_value(
                body,
                module,
                field.value_rttid,
                instruction.c + 3,
                instruction.c + 4,
                SEQUENCE_LOCAL,
                u32::from(field.slot_count) * 8,
            )?;
            emit_dynamic_success(body, instruction.a, 2);
            body.instruction(&W::Else);
            emit_dynamic_error_object(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                "dynamic target type mismatch",
                Some(DynamicErrorKind::TypeMismatch),
            )?;
            body.instruction(&W::End);
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "field not found",
            Some(DynamicErrorKind::BadField),
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "type does not support this assignment",
        Some(DynamicErrorKind::TypeMismatch),
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_slice_set(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    instruction: vo_common_core::instruction::Instruction,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Slice(elem))) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let elem = *elem;
        let (elem_bytes, _) = dynamic_element_bytes(module, elem)?;
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "cannot set index on nil slice",
            Some(DynamicErrorKind::NilBase),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        emit_dynamic_integer_kind(body, instruction.c + 2);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "index must be integer",
            Some(DynamicErrorKind::BadIndex),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 3);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64LtS);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64LeU)
            .instruction(&W::I32Or)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "slice index out of bounds",
            Some(DynamicErrorKind::OutOfBounds),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        let elem_kind = elem.value_kind();
        if matches!(
            elem_kind,
            ValueKind::Int
                | ValueKind::Int8
                | ValueKind::Int16
                | ValueKind::Int32
                | ValueKind::Int64
                | ValueKind::Uint
                | ValueKind::Uint8
                | ValueKind::Uint16
                | ValueKind::Uint32
                | ValueKind::Uint64
        ) {
            emit_dynamic_integer_kind(body, instruction.c + 4);
        } else {
            emit_dynamic_value_assignable(body, module, instruction.c + 4, elem);
        }
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "dynamic target type mismatch",
            Some(DynamicErrorKind::TypeMismatch),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(elem_bytes as i32))
            .instruction(&W::I32Mul)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_store_value(
            body,
            module,
            elem,
            instruction.c + 4,
            instruction.c + 5,
            SEQUENCE_LOCAL,
            elem_bytes,
        )?;
        emit_dynamic_success(body, instruction.a, 2);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_map_set(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    instruction: vo_common_core::instruction::Instruction,
    key_source: DynamicMapKeySource,
    value_slot0: u16,
    value_slot1: u16,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Map { key, val })) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let key = *key;
        let val = *val;
        if !dynamic_map_key_source_assignable(module, key, key_source)? {
            continue;
        }
        let key_bytes = u32::try_from(
            module
                .slot_layout_for_value_rttid(key)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map key layout is missing".into())
                })?
                .len()
                .checked_mul(8)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map key layout overflows wasm32".into())
                })?,
        )
        .map_err(|_| WasmAotError::InvalidModule("dynamic map key exceeds wasm32".into()))?;
        let val_bytes = u32::try_from(
            module
                .slot_layout_for_value_rttid(val)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map value layout is missing".into())
                })?
                .len()
                .checked_mul(8)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map value layout overflows wasm32".into())
                })?,
        )
        .map_err(|_| WasmAotError::InvalidModule("dynamic map value exceeds wasm32".into()))?;

        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            match key_source {
                DynamicMapKeySource::Boxed { .. } => "cannot set index on nil map",
                DynamicMapKeySource::FieldName { .. } => "cannot set field on nil map",
            },
            Some(DynamicErrorKind::NilBase),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        if let DynamicMapKeySource::Boxed { slot0, .. } = key_source {
            emit_dynamic_value_compatible(body, module, slot0, key);
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            emit_dynamic_error_object(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                "map key type mismatch",
                Some(DynamicErrorKind::BadIndex),
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }

        emit_dynamic_value_compatible(body, module, value_slot0, val);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "dynamic target type mismatch",
            Some(DynamicErrorKind::TypeMismatch),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        // Probe before growing. Besides avoiding unnecessary allocation for an
        // existing key, this validates interface-contained composite keys
        // before the map header can change.
        emit_dynamic_prepare_map_key(
            body,
            module,
            key,
            key_source,
            instruction.a,
            2,
            descriptors,
            globals,
        )?;
        body.instruction(&W::I32Const(0))
            .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(0))
            .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "map key is not hashable",
            Some(match key_source {
                DynamicMapKeySource::Boxed { .. } => DynamicErrorKind::BadIndex,
                DynamicMapKeySource::FieldName { .. } => DynamicErrorKind::BadField,
            }),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Add)
            .instruction(&W::I64Const(4))
            .instruction(&W::I64Mul)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Const(3))
            .instruction(&W::I64Mul)
            .instruction(&W::I64GeU)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::Call(MAP_GROW_FUNCTION_INDEX));
        propagate_status(body);
        body.instruction(&W::End);

        // Growth can run the collector, so rebuild the scratch key afterwards.
        emit_dynamic_prepare_map_key(
            body,
            module,
            key,
            key_source,
            instruction.a,
            2,
            descriptors,
            globals,
        )?;
        body.instruction(&W::I32Const(0))
            .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(1))
            .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "map key is not hashable",
            Some(match key_source {
                DynamicMapKeySource::Boxed { .. } => DynamicErrorKind::BadIndex,
                DynamicMapKeySource::FieldName { .. } => DynamicErrorKind::BadField,
            }),
        )?;
        body.instruction(&W::Br(3))
            .instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Ne)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Store(memarg(0)))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(8))
            .instruction(&W::I32Add)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(key_bytes as i32))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Add)
            .instruction(&W::I64Store(memarg(0)))
            .instruction(&W::End)
            .instruction(&W::End);

        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(8 + key_bytes as i32))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_store_value(
            body,
            module,
            val,
            value_slot0,
            value_slot1,
            SEQUENCE_LOCAL,
            val_bytes,
        )?;
        emit_dynamic_success(body, instruction.a, 2);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_index_set(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "cannot set index on nil",
        Some(DynamicErrorKind::NilBase),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_set(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.set_index_object_iface_id,
        false,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_slice_set(body, module, instruction, globals, static_data, descriptors)?;
    compile_dynamic_map_set(
        body,
        module,
        instruction,
        DynamicMapKeySource::Boxed {
            slot0: instruction.c + 2,
            slot1: instruction.c + 3,
        },
        instruction.c + 4,
        instruction.c + 5,
        globals,
        static_data,
        descriptors,
    )?;
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "type does not support this assignment",
        Some(DynamicErrorKind::TypeMismatch),
    )?;
    body.instruction(&W::End);
    Ok(())
}
