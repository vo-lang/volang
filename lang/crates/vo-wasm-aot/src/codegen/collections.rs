//! Collection, channel, and select instruction lowering.
use super::*;

pub(super) fn select_allocation_descriptor(
    body: &mut Function,
    descriptor: u32,
    globals: RuntimeGlobals,
) {
    body.instruction(&W::I32Const(descriptor as i32))
        .instruction(&W::GlobalSet(globals.allocation_descriptor));
}

pub(super) fn reject_nil_reference(
    body: &mut Function,
    slot: u16,
    message_ref: u32,
    resume_block: u32,
) {
    load_slot(body, slot);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, message_ref, resume_block);
    body.instruction(&W::End);
}

/// Clone one heap-backed value payload with the VM's value-assignment
/// semantics. The containing allocation is copied byte-for-byte while child
/// references keep their identity. PACKED_LOCAL receives the cloned GcRef.
pub(super) fn shallow_clone_payload(
    body: &mut Function,
    source: u16,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) {
    load_slot(body, source);
    body.instruction(&W::LocalSet(PACKED_LOCAL))
        .instruction(&W::LocalGet(PACKED_LOCAL))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(PACKED_LOCAL))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    // Immutable static-image references are safe to share. Mutable boxed
    // values are always owned by a tracked heap allocation.
    body.instruction(&W::Else)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(PACKED_LOCAL))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(STATUS_LOCAL))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        if !matches!(descriptor, AllocationDescriptor::Sequence { .. }) {
            continue;
        }
        body.instruction(&W::LocalGet(STATUS_LOCAL))
            .instruction(&W::I32Const(descriptor_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalSet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::I32GeU)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32Add)
            .instruction(&W::I32LtU)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Add)
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64Store(memarg(0)))
            .instruction(&W::End)
            .instruction(&W::End);
    }
    body.instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalSet(PACKED_LOCAL))
        .instruction(&W::End)
        .instruction(&W::End);
}

pub(super) fn reject_unhashable_interface_key(
    body: &mut Function,
    function: &FunctionDef,
    key_start: u16,
    message_ref: u32,
    resume_block: u32,
    globals: RuntimeGlobals,
) {
    if function.slot_types.get(key_start as usize) != Some(&vo_common_core::SlotType::Interface0) {
        return;
    }
    load_slot(body, key_start);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
    load_slot(body, key_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, key_start + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, key_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(SEQUENCE_DEEP_HASH_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)));
    load_slot(body, key_start + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(key_start + 1) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::End);
    load_slot(body, key_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, message_ref, resume_block);
    body.instruction(&W::End).instruction(&W::End);
}

/// Compute interface equality for APIs such as errors.equal that deliberately
/// turn an uncomparable concrete value into false instead of raising the
/// language-level comparison panic. The result is left in SEQUENCE_LOCAL.
pub(super) fn emit_nonpanicking_interface_equal(
    body: &mut Function,
    left: u16,
    right: u16,
    globals: RuntimeGlobals,
) {
    body.instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.dynamic_compare_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    load_slot(body, left);
    body.instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64And);
    load_slot(body, right);
    body.instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64And)
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Interface as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Slice as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Map as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Closure as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::String as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Float32 as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::F32ReinterpretI32);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::F32Eq)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Float64 as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::F64ReinterpretI64);
    load_slot(body, right + 1);
    body.instruction(&W::F64ReinterpretI64)
        .instruction(&W::F64Eq)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, left);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, left);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else);
    load_slot(body, left + 1);
    load_slot(body, right + 1);
    body.instruction(&W::I64Eq)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        // errors.equal defines uncomparable dynamic values as unequal. Keep
        // that policy local so a nested failed comparison cannot leak into a
        // later language-level comparison or map operation.
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
}

pub(super) fn emit_errors_assign_to(
    body: &mut Function,
    module: &VoModule,
    destination: u16,
    arguments: u16,
) -> Result<(), WasmAotError> {
    store_const(body, destination, 0);
    let resolver = module.runtime_type_resolver();
    body.instruction(&W::Block(BlockType::Empty));
    for target_rttid in 0..module.runtime_types.len() as u32 {
        let Some(target) = resolver.value_rttid_for_rttid(target_rttid) else {
            continue;
        };
        if target.value_kind() != ValueKind::Pointer {
            continue;
        }
        let Some((_, RuntimeType::Pointer(target_value))) = resolver.resolve_value_rttid(target)
        else {
            continue;
        };
        if target_value.value_kind() != ValueKind::Struct {
            continue;
        }
        let target_slots = resolver
            .slot_count_for_value_rttid(*target_value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "errors.assignTo target runtime type {target_rttid} has no finite layout"
                ))
            })?;
        let target_bytes = target_slots.checked_mul(8).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "errors.assignTo target runtime type {target_rttid} layout overflows"
            ))
        })?;
        let target_bytes: i32 = target_bytes.try_into().map_err(|_| {
            WasmAotError::InvalidModule(format!(
                "errors.assignTo target runtime type {target_rttid} exceeds wasm32"
            ))
        })?;

        load_slot(body, arguments + 2);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(u32::MAX as i32))
            .instruction(&W::I32And)
            .instruction(&W::I32Const(target.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));

        let mut source_types = vec![target_value.to_raw()];
        for source_rttid in 0..module.runtime_types.len() as u32 {
            let Some(source) = resolver.value_rttid_for_rttid(source_rttid) else {
                continue;
            };
            if source.value_kind() != ValueKind::Pointer {
                continue;
            }
            let Some((_, RuntimeType::Pointer(source_value))) =
                resolver.resolve_value_rttid(source)
            else {
                continue;
            };
            if source_value.rttid() == target_value.rttid()
                && source_value.value_kind() == ValueKind::Struct
            {
                source_types.push(source.to_raw());
            }
        }
        source_types.sort_unstable();
        source_types.dedup();
        for (index, source) in source_types.iter().enumerate() {
            load_slot(body, arguments);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(u32::MAX as i32))
                .instruction(&W::I32And)
                .instruction(&W::I32Const(*source as i32))
                .instruction(&W::I32Eq);
            if index > 0 {
                body.instruction(&W::I32Or);
            }
        }
        if source_types.is_empty() {
            body.instruction(&W::I32Const(0));
        }
        load_slot(body, arguments + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::I32And);
        load_slot(body, arguments + 3);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, arguments + 3);
        body.instruction(&W::I32WrapI64);
        load_slot(body, arguments + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(target_bytes))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        store_const(body, destination, 1);
        body.instruction(&W::End)
            .instruction(&W::Br(1))
            .instruction(&W::End);
    }
    body.instruction(&W::End);
    Ok(())
}

pub(super) fn allocate_sequence(body: &mut Function, allocation: SequenceAllocation) {
    let SequenceAllocation {
        destination,
        len_slot,
        cap_slot,
        elem_bytes,
        descriptor,
        globals,
        negative_len_panic_ref,
        cap_panic_ref,
        len_gt_cap_panic_ref,
        resume_block,
    } = allocation;
    load_slot(body, len_slot);
    body.instruction(&W::I64Const(0))
        .instruction(&W::I64LtS)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, negative_len_panic_ref, resume_block);
    body.instruction(&W::End);
    load_slot(body, cap_slot);
    body.instruction(&W::I64Const(0))
        .instruction(&W::I64LtS)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, cap_panic_ref, resume_block);
    body.instruction(&W::End);
    load_slot(body, len_slot);
    load_slot(body, cap_slot);
    body.instruction(&W::I64GtU)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, len_gt_cap_panic_ref, resume_block);
    body.instruction(&W::End);

    let max_capacity = if elem_bytes == 0 {
        u32::MAX
    } else {
        (u32::MAX - 32) / elem_bytes
    };
    load_slot(body, cap_slot);
    body.instruction(&W::I64Const(i64::from(max_capacity)))
        .instruction(&W::I64GtU)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, cap_panic_ref, resume_block);
    body.instruction(&W::End);

    load_slot(body, cap_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(body, descriptor, globals);
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
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, len_slot);
    body.instruction(&W::I64Store(MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, cap_slot);
    body.instruction(&W::I64Store(MemArg {
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
    store_prefix(body, destination);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
}

pub(super) fn sequence_element_address(
    body: &mut Function,
    sequence_slot: u16,
    index_slot: u16,
    _elem_bytes: u32,
    _bounds_panic_ref: u32,
    nil_panic_ref: u32,
    resume_block: u32,
) {
    reject_nil_reference(body, sequence_slot, nil_panic_ref, resume_block);
    load_slot(body, sequence_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    load_slot(body, index_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64GeU)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, index_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
    return_index_panic(body, resume_block);
    body.instruction(&W::End);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64);
    load_slot(body, index_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
}

/// Store a scalar through a sequence view. Compact backing stores use their
/// physical width; inline array views use one canonical 64-bit VM slot per
/// element and therefore require a full-slot write.
pub(super) fn store_sequence_scalar(body: &mut Function, source: u16, bytes: u32) {
    if bytes == 8 {
        load_slot(body, source);
        body.instruction(&W::I64Store(memarg(0)));
        return;
    }
    body.instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(8))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, source);
    body.instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, source);
    body.instruction(&match bytes {
        1 => W::I64Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }),
        2 => W::I64Store16(MemArg {
            offset: 0,
            align: 1,
            memory_index: 0,
        }),
        4 => W::I64Store32(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }),
        _ => unreachable!("scalar sequence width was validated"),
    })
    .instruction(&W::End);
}

pub(super) fn append_slice_element(
    body: &mut Function,
    destination: u16,
    source: u16,
    value_start: u16,
    elem_bytes: u32,
    descriptor: u32,
    globals: RuntimeGlobals,
) {
    let max_capacity = if elem_bytes == 0 {
        u32::MAX
    } else {
        (u32::MAX - 32) / elem_bytes
    };
    load_slot(body, source);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const((max_capacity / 2) as i32))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(max_capacity as i32))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Mul)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32LeU)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        // New sequence header.
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
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        // Preserve the old contents when growing a non-nil slice.
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        // A slice expression owns its header even when append can reuse the
        // backing store. Keeping the source header immutable preserves len/cap
        // value semantics while the Sequence descriptor retains the shared
        // backing allocation through its interior data pointer.
        .instruction(&W::I32Const(32));
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
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
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::End)
        // Copy the appended logical element.
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
    store_prefix(body, value_start);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
    store_prefix(body, destination);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
}

pub(super) fn slice_sequence(body: &mut Function, slice: SequenceSlice) {
    let SequenceSlice {
        destination,
        source,
        bounds_start,
        has_max,
        inline_view,
        descriptor,
        globals,
        bounds_panic_ref,
        resume_block,
    } = slice;
    // Bounds are language-level integers. Validate their full i64 values
    // before narrowing to wasm32 addresses so values outside [0, u32::MAX]
    // cannot wrap into an apparently valid slice range.
    let bound_count = if has_max { 3 } else { 2 };
    for offset in 0..bound_count {
        load_slot(body, bounds_start + offset);
        body.instruction(&W::I64Const(i64::from(u32::MAX)))
            .instruction(&W::I64GtU)
            .instruction(&W::If(BlockType::Empty));
        return_runtime_panic(body, bounds_panic_ref, resume_block);
        body.instruction(&W::End);
    }
    load_slot(body, source);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    load_slot(body, bounds_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LOW_LOCAL));
    load_slot(body, bounds_start + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(HIGH_LOCAL))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::I32Or);
    if has_max {
        load_slot(body, bounds_start + 2);
        body.instruction(&W::I32WrapI64).instruction(&W::I32Or);
    }
    body.instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, bounds_panic_ref, resume_block);
    body.instruction(&W::End);
    store_const(body, destination, 0);
    // A nil slice with zero bounds remains nil.
    body.instruction(&W::Br(1)).instruction(&W::End);

    if inline_view {
        load_slot(body, source + 5);
        body.instruction(&W::I32WrapI64);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 16,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::I32GtU);
    if has_max {
        load_slot(body, bounds_start + 2);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalTee(LENGTH_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32GtU)
            .instruction(&W::I32Or)
            .instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32GtU)
            .instruction(&W::I32Or);
    } else {
        body.instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32GtU)
            .instruction(&W::I32Or)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
    }
    body.instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, bounds_panic_ref, resume_block);
    body.instruction(&W::End).instruction(&W::I32Const(32));
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        // data = source.data + low * source.storage_stride
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if inline_view {
        load_slot(body, source + 1);
        body.instruction(&W::I32WrapI64);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::LocalGet(LOW_LOCAL));
    if inline_view {
        load_slot(body, source + 4);
        body.instruction(&W::I32WrapI64);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 24,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        // len = high - low
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        // cap = selected max/cap - low
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if inline_view {
        load_slot(body, source + 4);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 24,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: 24,
        align: 3,
        memory_index: 0,
    }));
    store_prefix(body, destination);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::End);
}

pub(super) fn clone_remote_port_payload(
    body: &mut Function,
    destination_local: u32,
    elem_slot_types: &[u8],
    globals: RuntimeGlobals,
) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_KIND_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_HOME_ISLAND_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Ne)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Call(CLONE_BEGIN_FUNCTION_INDEX))
        .instruction(&W::LocalSet(LENGTH_LOCAL));
    emit_clone_memory_layout(
        body,
        destination_local,
        FRAME_LIMIT_LOCAL,
        LENGTH_LOCAL,
        elem_slot_types,
    );
    body.instruction(&W::GlobalGet(globals.clone_failed))
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End).instruction(&W::End);
}

pub(super) fn clear_pending_queue_receiver(body: &mut Function) {
    for offset in [
        QUEUE_PENDING_RECV_FIBER_OFFSET,
        QUEUE_PENDING_RECV_DESTINATION_OFFSET,
        QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
        QUEUE_PENDING_RECV_TOKEN_OFFSET,
    ] {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
    }
}

/// Leaves an i32 readiness flag on the Wasm operand stack.
pub(super) fn pending_queue_receiver_is_ready(body: &mut Function) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
}

pub(super) fn deliver_to_pending_queue_receiver(
    body: &mut Function,
    source: u16,
    elem_bytes: u32,
    elem_slot_types: Option<&[u8]>,
    globals: RuntimeGlobals,
) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    store_prefix(body, source);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    if let Some(elem_slot_types) = elem_slot_types {
        clone_remote_port_payload(body, SEQUENCE_LOCAL, elem_slot_types, globals);
    }
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    clear_pending_queue_receiver(body);
    mark_scheduler_progress(body, globals);
}

pub(super) fn compile_queue_send(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    elem_bytes: u32,
    elem_slot_types: Option<&[u8]>,
    current_block: u32,
    globals: RuntimeGlobals,
    closed_queue_panic_ref: u32,
) {
    load_slot(body, instruction.a);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End);
    load_slot(body, instruction.a);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, closed_queue_panic_ref, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Unbuffered send resumes successfully after a receiver acknowledges
        // the pending payload in the sender's fiber record. Consume that
        // acknowledgement before inspecting a newly published receiver: the
        // latter belongs to a later rendezvous and reusing this send would
        // commit its payload twice.
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    // A receiver that arrived first publishes its concrete destination.
    // Commit directly so non-blocking select sends observe rendezvous
    // readiness with the same semantics as ordinary sends.
    pending_queue_receiver_is_ready(body);
    body.instruction(&W::If(BlockType::Empty));
    deliver_to_pending_queue_receiver(body, instruction.b, elem_bytes, elem_slot_types, globals);
    body.instruction(&W::Else)
        // A non-zero registration with an acknowledged receiver is stale.
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    clear_pending_queue_receiver(body);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // The queue owns one pending payload while the sender is suspended.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL));
    store_prefix(body, instruction.b);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    if let Some(elem_slot_types) = elem_slot_types {
        clone_remote_port_payload(body, FRAME_LIMIT_LOCAL, elem_slot_types, globals);
    }
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    body.instruction(&W::End);
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::Else)
        // Buffered send.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64GeU)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_TAIL_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL));
    store_prefix(body, instruction.b);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    if let Some(elem_slot_types) = elem_slot_types {
        clone_remote_port_payload(body, FRAME_LIMIT_LOCAL, elem_slot_types, globals);
    }
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_TAIL_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64RemU)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_TAIL_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End);
    mark_scheduler_progress(body, globals);
}

pub(super) fn compile_queue_recv(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    elem_slots: u16,
    current_block: u32,
    globals: RuntimeGlobals,
) {
    let elem_bytes = u32::from(elem_slots) * 8;
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));

    for slot in 0..elem_slots + u16::from(instruction.recv_has_ok()) {
        store_const(body, instruction.a + slot, 0);
    }
    load_slot(body, instruction.b);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End);
    load_slot(body, instruction.b);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Unbuffered receive consumes a pending sender payload.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    if instruction.recv_has_ok() {
        store_const(body, instruction.a + elem_slots, 1);
    }
    mark_scheduler_progress(body, globals);
    body.instruction(&W::Else)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // A receiver that arrives first publishes its frame destinations.
        // A later ordinary or select send can then commit the rendezvous.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    store_prefix(body, instruction.a);
    body.instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if instruction.recv_has_ok() {
        store_prefix(body, instruction.a + elem_slots);
        body.instruction(&W::I64ExtendI32U);
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::I64Const(1))
    .instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_TOKEN_OFFSET,
        align: 3,
        memory_index: 0,
    }));
    body.instruction(&W::End);
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::Else)
        // Buffered receive: closed and empty succeeds with the zero value.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_HEAD_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_HEAD_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64RemU)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_HEAD_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Sub)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    if instruction.recv_has_ok() {
        store_const(body, instruction.a + elem_slots, 1);
    }
    mark_scheduler_progress(body, globals);
    body.instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::Else)
        // The sender already wrote the payload and optional ok result into
        // this frame before publishing the acknowledgement.
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    mark_scheduler_progress(body, globals);
    body.instruction(&W::End);
}

pub(super) fn clear_select_send_registration(
    body: &mut Function,
    cases: &[SelectCaseLayout],
    globals: RuntimeGlobals,
) {
    for case in cases {
        let SelectCaseLayout::Send { queue, .. } = *case else {
            continue;
        };
        load_slot(body, queue);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, queue);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: QUEUE_CAPACITY_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Eqz)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Eq)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::End)
            .instruction(&W::End);
    }
}

pub(super) fn clear_select_recv_registration(
    body: &mut Function,
    cases: &[SelectCaseLayout],
    globals: RuntimeGlobals,
) {
    for case in cases {
        let SelectCaseLayout::Recv { queue, .. } = *case else {
            continue;
        };
        load_slot(body, queue);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, queue);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        clear_pending_queue_receiver(body);
        body.instruction(&W::End).instruction(&W::End);
    }
}

pub(super) fn register_select_send_candidate(
    body: &mut Function,
    case: &SelectCaseLayout,
    case_index: usize,
    case_count: usize,
    after_rotation: bool,
    globals: RuntimeGlobals,
) {
    let SelectCaseLayout::Send {
        queue,
        value,
        elem_slots,
    } = *case
    else {
        return;
    };
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::I32Const(case_index as i32))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&if after_rotation { W::I32GeU } else { W::I32LtU })
        .instruction(&W::I32And);
    load_slot(body, queue);
    body.instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, queue);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64);
    store_prefix(body, value);
    body.instruction(&W::I32Const(i32::from(elem_slots) * 8))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const((case_index + 1) as i64))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(((case_index + 1) % case_count) as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LENGTH_LOCAL));
    body.instruction(&W::End).instruction(&W::End);
}

pub(super) fn register_select_send(
    body: &mut Function,
    cases: &[SelectCaseLayout],
    globals: RuntimeGlobals,
) {
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for after_rotation in [true, false] {
        for (case_index, case) in cases.iter().enumerate() {
            register_select_send_candidate(
                body,
                case,
                case_index,
                cases.len(),
                after_rotation,
                globals,
            );
        }
    }
}

pub(super) fn register_select_recv_candidate(
    body: &mut Function,
    case: &SelectCaseLayout,
    case_index: usize,
    after_rotation: bool,
    globals: RuntimeGlobals,
) {
    let SelectCaseLayout::Recv {
        destination,
        queue,
        elem_slots,
        has_ok,
    } = *case
    else {
        return;
    };
    body.instruction(&W::I32Const(case_index as i32))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&if after_rotation { W::I32GeU } else { W::I32LtU });
    load_slot(body, queue);
    body.instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, queue);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    store_prefix(body, destination);
    body.instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if has_ok {
        store_prefix(body, destination + elem_slots);
        body.instruction(&W::I64ExtendI32U);
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::I64Const((case_index + 1) as i64))
    .instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_TOKEN_OFFSET,
        align: 3,
        memory_index: 0,
    }));
    body.instruction(&W::End).instruction(&W::End);
}

pub(super) fn register_select_recv(
    body: &mut Function,
    cases: &[SelectCaseLayout],
    globals: RuntimeGlobals,
) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for after_rotation in [true, false] {
        for (case_index, case) in cases.iter().enumerate() {
            register_select_recv_candidate(body, case, case_index, after_rotation, globals);
        }
    }
}

pub(super) fn compile_select_exec(
    body: &mut Function,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    globals: RuntimeGlobals,
    closed_queue_panic_ref: u32,
) -> Result<(), WasmAotError> {
    let cases = function
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::select_cases)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing SelectExecLayout metadata",
                function.name
            ))
        })?;
    let begin_pc = pc.checked_sub(cases.len() + 1).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} select transaction underflows its instruction stream",
            function.name
        ))
    })?;
    let begin = function.code[begin_pc];
    if begin.opcode() != Opcode::SelectBegin || usize::from(begin.a) != cases.len() {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} select transaction does not match its SelectBegin",
            function.name
        )));
    }
    if cases.is_empty() {
        if begin.flags & 0x01 != 0 {
            store_const(body, instruction.a, -1);
        } else {
            // An empty select has no operation that can become ready.
            return_suspended(body, current_block);
        }
        return Ok(());
    }
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::I32Const(-1))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::End);
    clear_select_send_registration(body, cases, globals);
    clear_select_recv_registration(body, cases, globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for after_rotation in [true, false] {
        for (index, case) in cases.iter().enumerate() {
            let queue = match *case {
                SelectCaseLayout::Send { queue, .. } | SelectCaseLayout::Recv { queue, .. } => {
                    queue
                }
            };
            body.instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I32Const(-1))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Const(index as i32))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&if after_rotation { W::I32GeU } else { W::I32LtU })
                .instruction(&W::I32And)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, queue);
            body.instruction(&W::I64Eqz)
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, queue);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(ALLOC_LOCAL));
            match *case {
                SelectCaseLayout::Recv { .. } => {
                    body.instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::If(BlockType::Result(ValType::I32)))
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CLOSED_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::I32Or)
                        .instruction(&W::Else)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_LENGTH_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CLOSED_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::I32Or)
                        .instruction(&W::End);
                }
                SelectCaseLayout::Send { .. } => {
                    // A send on a closed queue is immediately selectable and
                    // commits the normal closed-queue panic. Buffered space is
                    // otherwise the immediate readiness condition. Rendezvous
                    // readiness is supplied by the waiter path below.
                    body.instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CLOSED_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_LENGTH_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64LtU)
                        .instruction(&W::I32And)
                        .instruction(&W::I32Or)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz);
                    pending_queue_receiver_is_ready(body);
                    body.instruction(&W::I32And).instruction(&W::I32Or);
                }
            }
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(index as i32))
                .instruction(&W::LocalSet(STATUS_LOCAL))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End);
        }
    }

    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(-1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    if begin.flags & 0x01 != 0 {
        store_const(body, instruction.a, -1);
    } else {
        register_select_send(body, cases, globals);
        register_select_recv(body, cases, globals);
        return_suspended(body, current_block);
    }
    body.instruction(&W::End);

    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32GeS)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::End);

    for (index, case) in cases.iter().enumerate() {
        body.instruction(&W::LocalGet(STATUS_LOCAL))
            .instruction(&W::I32Const(index as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match *case {
            SelectCaseLayout::Recv {
                destination,
                queue,
                elem_slots,
                has_ok,
            } => {
                body.instruction(&W::LocalGet(LOW_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                let recv = vo_common_core::instruction::Instruction::with_flags(
                    Opcode::QueueRecv,
                    u8::from(has_ok),
                    destination,
                    queue,
                    0,
                );
                compile_queue_recv(body, recv, elem_slots, current_block, globals);
                body.instruction(&W::End);
            }
            SelectCaseLayout::Send {
                queue,
                value,
                elem_slots,
            } => {
                body.instruction(&W::LocalGet(LOW_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                let send = vo_common_core::instruction::Instruction::new(
                    Opcode::QueueSend,
                    queue,
                    value,
                    0,
                );
                compile_queue_send(
                    body,
                    send,
                    u32::from(elem_slots) * 8,
                    None,
                    current_block,
                    globals,
                    closed_queue_panic_ref,
                );
                body.instruction(&W::End);
            }
        }
        store_const(
            body,
            instruction.a,
            i64::from(function.code[begin_pc + 1 + index].c),
        );
        body.instruction(&W::End);
    }
    Ok(())
}

pub(super) fn compile_spawn_fiber(
    body: &mut Function,
    spawn: FiberSpawn<'_>,
) -> Result<(), WasmAotError> {
    let FiberSpawn {
        target,
        callee,
        frame_slots,
        args_start,
        closure,
        island_state_slot,
        clone_transfer,
        globals,
    } = spawn;
    let frame_bytes = frame_slots
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| {
            WasmAotError::InvalidModule("goroutine frame size overflows wasm32".into())
        })?;
    body.instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(target as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(ALLOC_LOCAL));
    if let Some((closure_slot, prefix)) = closure {
        let arg_offset = match prefix {
            ClosureArgumentPrefix::None => 0,
            ClosureArgumentPrefix::ClosureRef => {
                body.instruction(&W::LocalGet(ALLOC_LOCAL));
                load_slot(body, closure_slot);
                body.instruction(&W::I64Store(memarg(0)));
                1
            }
            ClosureArgumentPrefix::ReceiverCaptures(slots) => {
                body.instruction(&W::LocalGet(ALLOC_LOCAL));
                load_slot(body, closure_slot);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::I32Const(i32::from(slots) * 8))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
                slots
            }
        };
        let explicit_slots = callee.param_slots.checked_sub(arg_offset).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "closure goroutine parameter prefix {arg_offset} exceeds {} slots",
                callee.param_slots
            ))
        })?;
        if explicit_slots > 0 {
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(i32::from(arg_offset) * 8))
                .instruction(&W::I32Add);
            store_prefix(body, args_start);
            body.instruction(&W::I32Const(i32::from(explicit_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
    } else if callee.param_slots > 0 {
        body.instruction(&W::LocalGet(ALLOC_LOCAL));
        store_prefix(body, args_start);
        body.instruction(&W::I32Const(i32::from(callee.param_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    if clone_transfer && callee.param_slots > 0 {
        let parameter_layout = callee
            .slot_types
            .get(..usize::from(callee.param_slots))
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} parameter layout is truncated",
                    callee.name
                ))
            })?;
        body.instruction(&W::Call(CLONE_BEGIN_FUNCTION_INDEX))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
        emit_clone_memory_layout(
            body,
            ALLOC_LOCAL,
            FRAME_LIMIT_LOCAL,
            LENGTH_LOCAL,
            &encoded_slot_types(parameter_layout),
        );
        body.instruction(&W::GlobalGet(globals.clone_failed))
            .instruction(&W::If(BlockType::Empty));
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
            .instruction(&W::Drop);
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(
        (FRAME_STATE_BYTES + FIBER_RECORD_BYTES) as i32,
    ))
    .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
    .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
    .instruction(&W::LocalTee(LENGTH_LOCAL))
    .instruction(&W::I32Eqz)
    .instruction(&W::If(BlockType::Empty));
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop);
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(i64::from(target)))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL));
    if let Some(island_state_slot) = island_state_slot {
        load_slot(body, island_state_slot);
    } else {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Load(MemArg {
                offset: FIBER_ISLAND_STATE_OFFSET,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: FIBER_ISLAND_STATE_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(LENGTH_LOCAL))
    .instruction(&W::I64Const(i64::from(STACK_RESERVE_BYTES)))
    .instruction(&W::I64Store(MemArg {
        offset: FIBER_DIRECT_BUDGET_OFFSET,
        align: 3,
        memory_index: 0,
    }));
    publish_spawned_fiber(body, globals);
    Ok(())
}

/// A saved nil invocation has no guest frame. The scheduler reports its panic
/// only when this new Fiber receives a turn, preserving launcher semantics.
pub(super) fn compile_spawn_trapped_fiber(
    body: &mut Function,
    globals: RuntimeGlobals,
    message_ref: u32,
) {
    body.instruction(&W::I32Const(
        (FRAME_STATE_BYTES + FIBER_RECORD_BYTES) as i32,
    ))
    .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
    .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
    .instruction(&W::LocalTee(LENGTH_LOCAL))
    .instruction(&W::I32Eqz)
    .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LENGTH_LOCAL));
    for (offset, value) in [
        (FIBER_PANIC_SLOT0_OFFSET, (17u64 << 8 | 17) as i64),
        (FIBER_PANIC_SLOT1_OFFSET, i64::from(message_ref)),
        (FIBER_PANIC_GENERATION_OFFSET, 1),
        (FIBER_ACTIVE_PANIC_GENERATION_OFFSET, 1),
    ] {
        body.instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I64Const(value))
            .instruction(&W::I64Store(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
    }
    publish_spawned_fiber(body, globals);
}

fn publish_spawned_fiber(body: &mut Function, globals: RuntimeGlobals) {
    body.instruction(&W::GlobalGet(globals.fiber_tail))
        .instruction(&W::LocalTee(CAPACITY_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::GlobalSet(globals.fiber_head))
        .instruction(&W::End)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::GlobalSet(globals.fiber_tail));
    mark_scheduler_progress(body, globals);
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_defer_push_instruction(
    body: &mut Function,
    module: &VoModule,
    function: &FunctionDef,
    function_id: u32,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    allocation_descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let arg_slots = if instruction.call_shape_is_closure() {
        let arg_slots = function
            .instruction_metadata
            .get(pc)
            .and_then(InstructionMetadata::call_layout_slots)
            .map(|layout| layout.0)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing closure defer CallLayout metadata",
                    function.name
                ))
            })?;
        let candidates = closure_callsite_candidates(
            module,
            function,
            pc,
            function_indices,
            ClosureResultUse::Discarded,
        )?;
        body.instruction(&W::Block(BlockType::Empty));
        // A nil deferred call is still registered. Its saved invocation fails
        // during unwind, after subsequent statements and registrations run.
        load_slot(body, instruction.a);
        body.instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const(0))
            .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::Br(1))
            .instruction(&W::End);
        for candidate in candidates {
            let target = candidate.target;
            let frame_bytes =
                required_shared_frame_slots(module, target.function_id, materialized)?
                    .checked_mul(8)
                    .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule("defer frame size overflows wasm32".into())
                    })?;
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(target.encoded_identity()))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(frame_bytes as i32))
                .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
                .instruction(&W::I64Const(i64::from(closure_prefix_code(
                    target.abi.prefix,
                ))))
                .instruction(&W::LocalSet(PACKED_LOCAL))
                .instruction(&W::Br(1))
                .instruction(&W::End);
        }
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End);
        arg_slots
    } else {
        let target = instruction.call_shape_static_func_id();
        let callee = module.functions.get(target as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} defers missing function {target}",
                function.name
            ))
        })?;
        let frame_bytes = required_shared_frame_slots(module, target, materialized)?
            .checked_mul(8)
            .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
            .ok_or_else(|| {
                WasmAotError::InvalidModule("defer frame size overflows wasm32".into())
            })?;
        body.instruction(&W::I32Const(frame_bytes as i32))
            .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::LocalSet(PACKED_LOCAL));
        callee.param_slots
    };
    let entry_bytes = u32::from(arg_slots)
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(56))
        .ok_or_else(|| WasmAotError::InvalidModule("defer entry size overflows wasm32".into()))?;
    body.instruction(&W::I32Const(entry_bytes as i32));
    select_allocation_descriptor(body, allocation_descriptors.site(function_id, pc)?, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    let packed = ((instruction.call_shape_static_func_id() << 2)
        | (u32::from(instruction.call_shape_is_closure()) * 2))
        | u32::from(instruction.opcode() == Opcode::ErrDeferPush);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(i64::from(packed)))
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if instruction.call_shape_is_closure() {
        load_slot(body, instruction.a);
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::I64Const(i64::from(arg_slots)))
    .instruction(&W::I64Store(MemArg {
        offset: 24,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
    .instruction(&W::I64ExtendI32U)
    .instruction(&W::I64Store(MemArg {
        offset: 32,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::GlobalGet(globals.current_fiber))
    .instruction(&W::I64Load(MemArg {
        offset: FIBER_DIRECT_DEFER_FRAME_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::I32WrapI64)
    .instruction(&W::LocalGet(FRAME_LOCAL))
    .instruction(&W::I32Eq)
    .instruction(&W::If(BlockType::Result(ValType::I64)))
    .instruction(&W::GlobalGet(globals.current_fiber))
    .instruction(&W::I64Load(MemArg {
        offset: FIBER_DIRECT_DEFER_PARENT_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::I32WrapI64)
    .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
    .instruction(&W::I32Sub)
    .instruction(&W::I32Load(MemArg {
        offset: FRAME_ACTIVE_DEFER_OFFSET,
        align: 2,
        memory_index: 0,
    }))
    .instruction(&W::I64Load(MemArg {
        offset: 40,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::Else)
    .instruction(&W::GlobalGet(globals.current_fiber))
    .instruction(&W::I64Load(MemArg {
        offset: FIBER_PANIC_GENERATION_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::End)
    .instruction(&W::I64Store(MemArg {
        offset: 40,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::LocalGet(PACKED_LOCAL))
    .instruction(&W::I64Store(MemArg {
        offset: 48,
        align: 3,
        memory_index: 0,
    }));
    if arg_slots > 0 {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(56))
            .instruction(&W::I32Add);
        store_prefix(body, instruction.b);
        body.instruction(&W::I32Const(i32::from(arg_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    Ok(())
}

pub(super) fn compile_resolved_intrinsic(
    body: &mut Function,
    intrinsic: ExternIntrinsic,
    instruction: &vo_common_core::instruction::Instruction,
    arg_slots: u16,
) -> bool {
    let operator = match intrinsic {
        ExternIntrinsic::Sqrt if arg_slots == 1 => W::F64Sqrt,
        ExternIntrinsic::Floor if arg_slots == 1 => W::F64Floor,
        ExternIntrinsic::Ceil if arg_slots == 1 => W::F64Ceil,
        ExternIntrinsic::Trunc if arg_slots == 1 => W::F64Trunc,
        // Core WebAssembly has no fused multiply-add instruction. Routing FMA
        // through mul+add would change IEEE-754 rounding, so it stays on the
        // authenticated runtime path.
        ExternIntrinsic::Fma
        | ExternIntrinsic::Sqrt
        | ExternIntrinsic::Floor
        | ExternIntrinsic::Ceil
        | ExternIntrinsic::Trunc => return false,
    };
    store_prefix(body, instruction.a);
    load_slot(body, instruction.c);
    body.instruction(&W::F64ReinterpretI64)
        .instruction(&operator)
        .instruction(&W::I64ReinterpretF64)
        .instruction(&W::I64Store(memarg(0)));
    true
}

/// Lower the common, layout-identical `copy` path to Core Wasm `memory.copy`.
///
/// Compact primitive array views can legitimately expose different physical
/// strides for the same logical element type. That uncommon case retains the
/// authenticated host implementation, whose staging buffer preserves memmove
/// semantics even when differently-strided views overlap.
pub(super) fn compile_builtin_copy(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    arg_slots: u16,
    source_is_string: bool,
    current_block: u32,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    if arg_slots != 2 {
        return Err(WasmAotError::InvalidModule(format!(
            "builtin copy extern {} has {arg_slots} argument slots",
            instruction.b
        )));
    }

    // The nil/empty result is published first. The fast branch overwrites it
    // with the copied element count; the fallback host branch owns the same
    // destination slot and therefore retains the canonical extern ABI.
    store_const(body, instruction.a, 0);
    load_slot(body, instruction.c);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    load_slot(body, instruction.c + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: if source_is_string { 0 } else { 8 },
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL));
    if source_is_string {
        body.instruction(&W::I32Const(1));
    } else {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 24,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: if source_is_string { 8 } else { 0 },
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(HIGH_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else);
    save_resume_block(body, current_block);
    body.instruction(&W::I32Const(i32::from(instruction.b)))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.a)))
        .instruction(&W::I32Const(i32::from(instruction.c)))
        .instruction(&W::I32Const(i32::from(arg_slots)))
        .instruction(&W::Call(0))
        .instruction(&W::LocalTee(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.host_wait_pending))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL));
    propagate_status(body);
    body.instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    Ok(())
}

pub(super) fn emit_materialized_call_arguments(
    body: &mut Function,
    callee: &FunctionDef,
    arguments: MaterializedCallArguments,
) -> Result<(), WasmAotError> {
    let copy_slots = |body: &mut Function, destination_offset: u16, source: u16, slots: u16| {
        if slots == 0 {
            return;
        }
        body.instruction(&W::LocalGet(ALLOC_LOCAL));
        if destination_offset != 0 {
            body.instruction(&W::I32Const(i32::from(destination_offset) * 8))
                .instruction(&W::I32Add);
        }
        store_prefix(body, source);
        body.instruction(&W::I32Const(i32::from(slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    };

    match arguments {
        MaterializedCallArguments::Contiguous { source } => {
            copy_slots(body, 0, source, callee.param_slots);
        }
        MaterializedCallArguments::Closure {
            closure,
            explicit,
            prefix,
        } => {
            let argument_offset = match prefix {
                ClosureArgumentPrefix::None => 0,
                ClosureArgumentPrefix::ClosureRef => {
                    body.instruction(&W::LocalGet(ALLOC_LOCAL));
                    load_slot(body, closure);
                    body.instruction(&W::I64Store(memarg(0)));
                    1
                }
                ClosureArgumentPrefix::ReceiverCaptures(slots) => {
                    if slots > 0 {
                        body.instruction(&W::LocalGet(ALLOC_LOCAL));
                        load_slot(body, closure);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(8))
                            .instruction(&W::I32Add)
                            .instruction(&W::I32Const(i32::from(slots) * 8))
                            .instruction(&W::MemoryCopy {
                                src_mem: 0,
                                dst_mem: 0,
                            });
                    }
                    slots
                }
            };
            let explicit_slots =
                callee
                    .param_slots
                    .checked_sub(argument_offset)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "closure argument prefix {argument_offset} exceeds {} slots for {}",
                            callee.param_slots, callee.name
                        ))
                    })?;
            copy_slots(body, argument_offset, explicit, explicit_slots);
        }
        MaterializedCallArguments::Interface {
            receiver_data,
            explicit,
            receiver_slots,
        } => {
            let explicit_slots =
                callee
                    .param_slots
                    .checked_sub(receiver_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                    "interface receiver uses {receiver_slots} slots beyond {} parameters for {}",
                    callee.param_slots, callee.name
                ))
                    })?;
            copy_slots(body, 0, receiver_data, receiver_slots);
            copy_slots(body, receiver_slots, explicit, explicit_slots);
        }
    }
    Ok(())
}

/// Reserve a materialized child frame from the current fiber's explicit stack.
///
/// The scheduler may park a materialized frame indefinitely, so its storage
/// must survive suspension. A per-fiber chunk stack provides that durability
/// while retaining constant-time LIFO allocation for ordinary and recursive
/// calls. Opening a new chunk is uncommon and continues to use the traced frame
/// allocator so the GC can discover the whole active frame chain.
pub(super) fn compile_materialized_stack_frame_alloc(globals: RuntimeGlobals) -> Function {
    const FRAME_BYTES: u32 = 0;
    const REQUIRED_CHUNK_BYTES: u32 = 1;
    const BASE_CHUNK_BYTES: u32 = 2;
    const OVERFLOW_CHUNK_BYTES: u32 = 3;
    const PREVIOUS_CHUNK: u32 = 4;
    const PREVIOUS_TOP: u32 = 5;
    const PREVIOUS_LIMIT: u32 = 6;
    const CURRENT_CHUNK: u32 = 7;
    const FRAME_HEADER: u32 = 8;
    const FRAME_TOP: u32 = 9;
    const CHUNK_LIMIT: u32 = 10;

    let mut body = Function::new([(10, ValType::I32)]);
    body.instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(REQUIRED_CHUNK_BYTES))
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End);
    for (minimum, destination) in [
        (SHADOW_STACK_BASE_CHUNK_BYTES, BASE_CHUNK_BYTES),
        (SHADOW_STACK_CHUNK_BYTES, OVERFLOW_CHUNK_BYTES),
    ] {
        body.instruction(&W::LocalGet(REQUIRED_CHUNK_BYTES))
            .instruction(&W::I32Const(minimum as i32))
            .instruction(&W::I32GtU)
            .instruction(&W::If(BlockType::Result(ValType::I32)))
            .instruction(&W::LocalGet(REQUIRED_CHUNK_BYTES))
            .instruction(&W::Else)
            .instruction(&W::I32Const(minimum as i32))
            .instruction(&W::End)
            .instruction(&W::LocalSet(destination));
    }
    for (local, offset) in [
        (PREVIOUS_CHUNK, FIBER_SHADOW_CHUNK_OFFSET),
        (PREVIOUS_TOP, FIBER_SHADOW_TOP_OFFSET),
        (PREVIOUS_LIMIT, FIBER_SHADOW_LIMIT_OFFSET),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Load(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(local));
    }
    body.instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::LocalSet(CURRENT_CHUNK))
        .instruction(&W::LocalGet(PREVIOUS_TOP))
        .instruction(&W::LocalSet(FRAME_HEADER))
        .instruction(&W::LocalGet(PREVIOUS_LIMIT))
        .instruction(&W::LocalSet(CHUNK_LIMIT))
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(BASE_CHUNK_BYTES))
        .instruction(&W::I32Const(FRAME_ALLOC_UNINITIALIZED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(CURRENT_CHUNK))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME_HEADER))
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::LocalGet(BASE_CHUNK_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CHUNK_LIMIT))
        .instruction(&W::End)
        .instruction(&W::LocalGet(FRAME_HEADER))
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(FRAME_TOP))
        .instruction(&W::LocalGet(FRAME_HEADER))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalGet(FRAME_TOP))
        .instruction(&W::LocalGet(CHUNK_LIMIT))
        .instruction(&W::I32GtU)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(OVERFLOW_CHUNK_BYTES))
        .instruction(&W::I32Const(FRAME_ALLOC_UNINITIALIZED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(CURRENT_CHUNK))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME_HEADER))
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::LocalGet(OVERFLOW_CHUNK_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CHUNK_LIMIT))
        .instruction(&W::LocalGet(FRAME_HEADER))
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME_TOP))
        .instruction(&W::End)
        .instruction(&W::LocalGet(FRAME_HEADER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::MemoryFill(0));
    for (offset, local) in [
        (FRAME_PREVIOUS_STACK_CHUNK_OFFSET, PREVIOUS_CHUNK),
        (FRAME_PREVIOUS_STACK_TOP_OFFSET, PREVIOUS_TOP),
        (FRAME_PREVIOUS_STACK_LIMIT_OFFSET, PREVIOUS_LIMIT),
        (FRAME_STACK_CHUNK_OFFSET, CURRENT_CHUNK),
        (FRAME_LIMIT_OFFSET, FRAME_TOP),
    ] {
        body.instruction(&W::LocalGet(FRAME_HEADER))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    for (offset, local) in [
        (FIBER_SHADOW_CHUNK_OFFSET, CURRENT_CHUNK),
        (FIBER_SHADOW_TOP_OFFSET, FRAME_TOP),
        (FIBER_SHADOW_LIMIT_OFFSET, CHUNK_LIMIT),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(FRAME_HEADER))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::End);
    body
}
