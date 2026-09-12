//! Dynamic support helpers.
use super::*;

pub(super) fn dynamic_string_ref(
    static_data: &StaticData,
    value: &str,
) -> Result<u32, WasmAotError> {
    static_data
        .dynamic_string_refs
        .get(value)
        .copied()
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "Core-Wasm dynamic runtime string was not interned: {value}"
            ))
        })
}

pub(super) fn dynamic_error_layout(
    module: &ModuleAnalysis<'_>,
    descriptors: &AllocationDescriptors,
) -> Result<(u32, u16, [u16; 2], u64), WasmAotError> {
    let struct_meta_id = module.well_known.error_struct_meta_id.ok_or_else(|| {
        WasmAotError::InvalidModule(
            "Core-Wasm dynamic runtime requires errors.Error metadata".into(),
        )
    })?;
    let metadata = module
        .struct_metas
        .get(struct_meta_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("errors.Error struct metadata is missing".into())
        })?;
    let slots = u16::try_from(metadata.slot_types.len())
        .map_err(|_| WasmAotError::InvalidModule("errors.Error layout exceeds u16".into()))?;
    let descriptor = *descriptors
        .fixed_by_struct_meta
        .get(&struct_meta_id)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("errors.Error allocation descriptor is missing".into())
        })?;
    let offsets = module.well_known.error_field_offsets.ok_or_else(|| {
        WasmAotError::InvalidModule("errors.Error field offsets are missing".into())
    })?;
    let pointer_rttid = module.well_known.error_ptr_rttid.ok_or_else(|| {
        WasmAotError::InvalidModule("*errors.Error runtime type is missing".into())
    })?;
    let slot0 = (u64::from(pointer_rttid) << 8) | u64::from(ValueKind::Pointer as u8);
    Ok((descriptor, slots, offsets, slot0))
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_error_object(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    destination: u16,
    message: &str,
    cause: Option<DynamicErrorKind>,
) -> Result<(), WasmAotError> {
    let (descriptor, slots, offsets, interface_slot0) = dynamic_error_layout(module, descriptors)?;
    body.instruction(&W::I32Const(i32::from(slots) * 8));
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(i64::from(dynamic_string_ref(
            static_data,
            message,
        )?)))
        .instruction(&W::I64Store(MemArg {
            offset: u64::from(offsets[0]) * 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    let cause_slot = cause.and_then(|cause| global_slot(module, cause.sentinel_name()));
    if let Some(cause_slot) = cause_slot {
        global_slot_address(
            body,
            u16::try_from(cause_slot).map_err(|_| {
                WasmAotError::InvalidModule("dynamic sentinel global exceeds u16".into())
            })?,
            globals,
        );
        body.instruction(&W::I64Load(memarg(0)));
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: u64::from(offsets[1]) * 8,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL));
    if let Some(cause_slot) = cause_slot {
        global_slot_address(
            body,
            u16::try_from(cause_slot + 1).map_err(|_| {
                WasmAotError::InvalidModule("dynamic sentinel global exceeds u16".into())
            })?,
            globals,
        );
        body.instruction(&W::I64Load(memarg(0)));
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: u64::from(offsets[1] + 1) * 8,
        align: 3,
        memory_index: 0,
    }));
    store_const(body, destination, interface_slot0 as i64);
    store_prefix(body, destination + 1);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
    Ok(())
}

pub(super) fn emit_dynamic_success(body: &mut Function, destination: u16, return_slots: u16) {
    for slot in 0..return_slots {
        store_const(body, destination + slot, 0);
    }
}

pub(super) fn emit_dynamic_get_error(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    destination: u16,
    error: DynamicErrorSpec<'_>,
) -> Result<(), WasmAotError> {
    store_const(body, destination, 0);
    store_const(body, destination + 1, 0);
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        destination + 2,
        error.message,
        Some(error.kind),
    )
}

pub(super) fn emit_dynamic_error_sentinels(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    destination: u16,
) -> Result<(), WasmAotError> {
    for (index, kind) in [
        DynamicErrorKind::Unknown,
        DynamicErrorKind::NilBase,
        DynamicErrorKind::BadField,
        DynamicErrorKind::BadIndex,
        DynamicErrorKind::OutOfBounds,
        DynamicErrorKind::BadCall,
        DynamicErrorKind::SigMismatch,
        DynamicErrorKind::TypeMismatch,
    ]
    .into_iter()
    .enumerate()
    {
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            destination + u16::try_from(index * 2).expect("eight error pairs fit u16"),
            kind.sentinel_message(),
            None,
        )?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_pack_error(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    instruction: vo_common_core::instruction::Instruction,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    kind: DynamicErrorKind,
    message: &str,
) -> Result<(), WasmAotError> {
    store_const(body, instruction.a, 0);
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a + 1,
        message,
        Some(kind),
    )
}

pub(super) fn emit_allocate_dynamic_any_slice(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(
        body,
        *descriptors
            .sequence_by_kind
            .get(&(ValueKind::Interface as u8))
            .ok_or_else(|| {
                WasmAotError::InvalidModule(
                    "dynamic any-slice allocation descriptor is missing".into(),
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
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(16))
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
    Ok(())
}

pub(super) fn emit_finish_dynamic_child(body: &mut Function, globals: RuntimeGlobals) {
    emit_pending_child_address(body);
    body.instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_materialized_stack_frame_free(body, globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::End);
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_prepare_dynamic_child_frame(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    target: u32,
    materialized: &BTreeSet<u32>,
    current_block: u32,
    stack_overflow_panic_ref: u32,
    globals: RuntimeGlobals,
    fill: impl FnOnce(&mut Function) -> Result<(), WasmAotError>,
) -> Result<(), WasmAotError> {
    let frame_bytes = required_shared_frame_slots(module, target, materialized)?
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| WasmAotError::InvalidModule("dynamic call frame exceeds wasm32".into()))?;
    emit_pending_child_address(body);
    body.instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(STACK_RESERVE_BYTES as i32))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, stack_overflow_panic_ref, current_block);
    body.instruction(&W::End);
    emit_materialized_stack_frame_alloc(body, frame_bytes, target, globals)?;
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    fill(body)?;
    body.instruction(&W::End);
    Ok(())
}
