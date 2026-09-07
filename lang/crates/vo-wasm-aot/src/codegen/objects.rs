//! Generated object equality, hashing, cloning, and string helpers.
use super::*;

pub(super) fn compile_string_hash() -> Function {
    const LENGTH: u32 = 1;
    const DATA: u32 = 2;
    const INDEX: u32 = 3;
    const HASH: u32 = 4;
    let mut body = Function::new([(4, ValType::I32)]);
    body.instruction(&W::I32Const(0x811c9dc5u32 as i32))
        .instruction(&W::LocalSet(HASH))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HASH))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LENGTH))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(DATA))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(LENGTH))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(HASH))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32Xor)
        .instruction(&W::I32Const(16_777_619))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(HASH))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HASH))
        .instruction(&W::End);
    body
}

pub(super) fn emit_deep_equal_child(
    body: &mut Function,
    left_local: u32,
    right_local: u32,
    slot_offset: u32,
    value: ValueRttid,
) {
    body.instruction(&W::LocalGet(left_local));
    if slot_offset != 0 {
        body.instruction(&W::I32Const((slot_offset * 8) as i32))
            .instruction(&W::I32Add);
    }
    body.instruction(&W::LocalGet(right_local));
    if slot_offset != 0 {
        body.instruction(&W::I32Const((slot_offset * 8) as i32))
            .instruction(&W::I32Add);
    }
    body.instruction(&W::I32Const(value.to_raw() as i32))
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End);
}

pub(super) fn compile_deep_equal(
    module: &VoModule,
    dynamic_compare_failed: u32,
) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 3;
    const LEFT_PTR: u32 = 4;
    const RIGHT_PTR: u32 = 5;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(3, ValType::I32)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            // Tuple-only verifier types never inhabit runtime value slots.
            continue;
        };
        let (_, runtime_type) = resolver.resolve_value_rttid(value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "runtime type {rttid} has an invalid named-type chain"
            ))
        })?;
        body.instruction(&W::LocalGet(2))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match runtime_type {
            RuntimeType::Basic(ValueKind::String) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float32) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::F32ReinterpretI32)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::F32ReinterpretI32)
                    .instruction(&W::F32Eq)
                    .instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float64) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::F64Load(memarg(0)))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::F64Load(memarg(0)))
                    .instruction(&W::F64Eq)
                    .instruction(&W::Return);
            }
            RuntimeType::Basic(_)
            | RuntimeType::Pointer(_)
            | RuntimeType::Chan { .. }
            | RuntimeType::Port { .. }
            | RuntimeType::Island => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Eq)
                    .instruction(&W::Return);
            }
            RuntimeType::Struct { meta_id, .. } => {
                let meta = module.struct_metas.get(*meta_id as usize).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime type {rttid} references missing struct metadata {meta_id}"
                    ))
                })?;
                for field in &meta.fields {
                    emit_deep_equal_child(
                        &mut body,
                        0,
                        1,
                        u32::from(field.offset),
                        field.type_info,
                    );
                }
                body.instruction(&W::I32Const(1)).instruction(&W::Return);
            }
            RuntimeType::Array { len, elem } => {
                let len: u32 = (*len).try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} exceeds the wasm32 element domain"
                    ))
                })?;
                let elem_slots = resolver.slot_count_for_value_rttid(*elem).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} has no finite element layout"
                    ))
                })?;
                let elem_slots: u32 = elem_slots.try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} element layout exceeds wasm32"
                    ))
                })?;
                body.instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(len as i32))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::I32Const(elem.to_raw() as i32))
                    .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::I32Const(1))
                    .instruction(&W::Return);
            }
            RuntimeType::Interface { .. } => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Ne)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load8U(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
                    .instruction(&W::Return)
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load8U(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(ValueKind::Struct as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(LEFT_PTR))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(RIGHT_PTR))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(LEFT_PTR))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(RIGHT_PTR))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(LEFT_PTR))
                    .instruction(&W::LocalGet(RIGHT_PTR))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
                    .instruction(&W::Return);
            }
            RuntimeType::Slice(_)
            | RuntimeType::Map { .. }
            | RuntimeType::Func { .. }
            | RuntimeType::Tuple(_)
            | RuntimeType::Named { .. } => {
                body.instruction(&W::I32Const(1))
                    .instruction(&W::GlobalSet(dynamic_compare_failed))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::Return);
            }
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(dynamic_compare_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    Ok(body)
}

pub(super) fn emit_hash_combine(body: &mut Function, hash_local: u32) {
    body.instruction(&W::LocalGet(hash_local))
        .instruction(&W::I64Xor)
        .instruction(&W::I64Const(1_099_511_628_211))
        .instruction(&W::I64Mul)
        .instruction(&W::LocalSet(hash_local));
}

pub(super) fn emit_deep_hash_child(
    body: &mut Function,
    value_local: u32,
    slot_offset: u32,
    value: ValueRttid,
    hash_local: u32,
) {
    body.instruction(&W::LocalGet(value_local));
    if slot_offset != 0 {
        body.instruction(&W::I32Const((slot_offset * 8) as i32))
            .instruction(&W::I32Add);
    }
    body.instruction(&W::I32Const(value.to_raw() as i32))
        .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
    emit_hash_combine(body, hash_local);
}

pub(super) fn compile_deep_hash(
    module: &VoModule,
    dynamic_compare_failed: u32,
) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 2;
    const VALUE_PTR: u32 = 3;
    const HASH: u32 = 4;
    const TAG: u32 = 5;
    const HASH_SEED: u64 = 0xcbf2_9ce4_8422_2325;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(2, ValType::I32), (2, ValType::I64)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let (_, runtime_type) = resolver.resolve_value_rttid(value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "runtime type {rttid} has an invalid named-type chain"
            ))
        })?;
        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I64Const((HASH_SEED ^ u64::from(value.to_raw())) as i64))
            .instruction(&W::LocalSet(HASH));
        match runtime_type {
            RuntimeType::Basic(ValueKind::String) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(STRING_HASH_FUNCTION_INDEX))
                    .instruction(&W::I64ExtendI32U);
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float32) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::LocalTee(INDEX))
                    .instruction(&W::I32Const(0x7fff_ffff))
                    .instruction(&W::I32And)
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::End);
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float64) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalTee(TAG))
                    .instruction(&W::I64Const(0x7fff_ffff_ffff_ffff))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(TAG))
                    .instruction(&W::End);
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Basic(_)
            | RuntimeType::Pointer(_)
            | RuntimeType::Chan { .. }
            | RuntimeType::Port { .. }
            | RuntimeType::Island => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Struct { meta_id, .. } => {
                let meta = module.struct_metas.get(*meta_id as usize).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime type {rttid} references missing struct metadata {meta_id}"
                    ))
                })?;
                for field in &meta.fields {
                    emit_deep_hash_child(
                        &mut body,
                        0,
                        u32::from(field.offset),
                        field.type_info,
                        HASH,
                    );
                }
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Array { len, elem } => {
                let len: u32 = (*len).try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} exceeds the wasm32 element domain"
                    ))
                })?;
                let elem_slots = resolver.slot_count_for_value_rttid(*elem).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} has no finite element layout"
                    ))
                })?;
                let elem_slots: u32 = elem_slots.try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} element layout exceeds wasm32"
                    ))
                })?;
                body.instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(len as i32))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::I32Const(elem.to_raw() as i32))
                    .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(HASH))
                    .instruction(&W::Return);
            }
            RuntimeType::Interface { .. } => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalTee(TAG))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(TAG));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(TAG))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0xff))
                    .instruction(&W::I32And)
                    .instruction(&W::LocalTee(INDEX))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(TAG))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(SEQUENCE_DEEP_HASH_FUNCTION_INDEX));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH))
                    .instruction(&W::Return)
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(ValueKind::Struct as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(VALUE_PTR))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(VALUE_PTR))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(VALUE_PTR))
                    .instruction(&W::LocalGet(TAG))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Slice(_)
            | RuntimeType::Map { .. }
            | RuntimeType::Func { .. }
            | RuntimeType::Tuple(_)
            | RuntimeType::Named { .. } => {
                body.instruction(&W::I32Const(1))
                    .instruction(&W::GlobalSet(dynamic_compare_failed))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Return);
            }
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(dynamic_compare_failed))
        .instruction(&W::I64Const(0))
        .instruction(&W::End);
    Ok(body)
}

pub(super) fn emit_sequence_element_address(
    body: &mut Function,
    data_local: u32,
    index_local: u32,
    stride_local: u32,
) {
    body.instruction(&W::LocalGet(data_local))
        .instruction(&W::LocalGet(index_local))
        .instruction(&W::LocalGet(stride_local))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
}

/// Compare an array stored behind the sequence header used when an array is
/// boxed in an interface. Compact scalar arrays retain their physical element
/// width; wider elements use the ordinary logical-slot representation.
pub(super) fn compile_sequence_deep_equal(module: &VoModule) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 3;
    const LEFT_DATA: u32 = 4;
    const RIGHT_DATA: u32 = 5;
    const LEFT_STRIDE: u32 = 6;
    const RIGHT_STRIDE: u32 = 7;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(5, ValType::I32)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Array { elem, .. })) = resolver.resolve_value_rttid(value) else {
            continue;
        };
        let result_slots = resolver
            .slot_count_for_value_rttid(value)
            .and_then(|slots| u16::try_from(slots).ok())
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} exceeds the interface slot domain"
                ))
            })?;
        let layout =
            interface_array_assertion_layout(module, rttid, result_slots)?.ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} has no interface sequence layout"
                ))
            })?;

        body.instruction(&W::LocalGet(2))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        if layout.len == 0 {
            body.instruction(&W::I32Const(1)).instruction(&W::Return);
        } else {
            body.instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LEFT_DATA))
                .instruction(&W::LocalGet(1))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(RIGHT_DATA))
                .instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LEFT_STRIDE))
                .instruction(&W::LocalGet(1))
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(RIGHT_STRIDE))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(i32::from(layout.len)))
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            emit_sequence_element_address(&mut body, LEFT_DATA, INDEX, LEFT_STRIDE);
            match elem.value_kind() {
                ValueKind::Bool | ValueKind::Uint8 => {
                    body.instruction(&W::I32Load8U(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load8U(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Int8 => {
                    body.instruction(&W::I32Load8S(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load8S(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Uint16 => {
                    body.instruction(&W::I32Load16U(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load16U(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Int16 => {
                    body.instruction(&W::I32Load16S(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load16S(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Uint32 | ValueKind::Int32 => {
                    body.instruction(&W::I32Load(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Float32 => {
                    body.instruction(&W::F32Load(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::F32Load(packed_memarg()))
                        .instruction(&W::F32Eq);
                }
                _ => {
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Const(elem.to_raw() as i32))
                        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX));
                }
            }
            body.instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(0))
                .instruction(&W::Return)
                .instruction(&W::End)
                .instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::I32Const(1))
                .instruction(&W::Return);
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(0)).instruction(&W::End);
    Ok(body)
}

/// Hash an interface-boxed array using exactly the same logical-value hash as
/// an unboxed array, independent of compact sequence storage.
pub(super) fn compile_sequence_deep_hash(module: &VoModule) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 2;
    const DATA: u32 = 3;
    const BITS: u32 = 4;
    const STRIDE: u32 = 5;
    const HASH: u32 = 6;
    const CHILD_HASH: u32 = 7;
    const HASH_SEED: u64 = 0xcbf2_9ce4_8422_2325;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(4, ValType::I32), (2, ValType::I64)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Array { elem, .. })) = resolver.resolve_value_rttid(value) else {
            continue;
        };
        let result_slots = resolver
            .slot_count_for_value_rttid(value)
            .and_then(|slots| u16::try_from(slots).ok())
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} exceeds the interface slot domain"
                ))
            })?;
        let layout =
            interface_array_assertion_layout(module, rttid, result_slots)?.ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} has no interface sequence layout"
                ))
            })?;

        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I64Const((HASH_SEED ^ u64::from(value.to_raw())) as i64))
            .instruction(&W::LocalSet(HASH));
        if layout.len != 0 {
            body.instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(DATA))
                .instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(STRIDE))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(i32::from(layout.len)))
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            if layout.elem_bytes < 8 {
                body.instruction(&W::I64Const((HASH_SEED ^ u64::from(elem.to_raw())) as i64))
                    .instruction(&W::LocalSet(CHILD_HASH));
                emit_sequence_element_address(&mut body, DATA, INDEX, STRIDE);
                match elem.value_kind() {
                    ValueKind::Bool | ValueKind::Uint8 => {
                        body.instruction(&W::I32Load8U(packed_memarg()))
                            .instruction(&W::I64ExtendI32U);
                    }
                    ValueKind::Int8 => {
                        body.instruction(&W::I32Load8S(packed_memarg()))
                            .instruction(&W::I64ExtendI32S);
                    }
                    ValueKind::Uint16 => {
                        body.instruction(&W::I32Load16U(packed_memarg()))
                            .instruction(&W::I64ExtendI32U);
                    }
                    ValueKind::Int16 => {
                        body.instruction(&W::I32Load16S(packed_memarg()))
                            .instruction(&W::I64ExtendI32S);
                    }
                    ValueKind::Uint32 => {
                        body.instruction(&W::I32Load(packed_memarg()))
                            .instruction(&W::I64ExtendI32U);
                    }
                    ValueKind::Int32 => {
                        body.instruction(&W::I32Load(packed_memarg()))
                            .instruction(&W::I64ExtendI32S);
                    }
                    ValueKind::Float32 => {
                        body.instruction(&W::I32Load(packed_memarg()))
                            .instruction(&W::LocalTee(BITS))
                            .instruction(&W::I32Const(0x7fff_ffff))
                            .instruction(&W::I32And)
                            .instruction(&W::I32Eqz)
                            .instruction(&W::If(BlockType::Result(ValType::I64)))
                            .instruction(&W::I64Const(0))
                            .instruction(&W::Else)
                            .instruction(&W::LocalGet(BITS))
                            .instruction(&W::I64ExtendI32U)
                            .instruction(&W::End);
                    }
                    _ => unreachable!("compact array element layout was validated"),
                }
                emit_hash_combine(&mut body, CHILD_HASH);
                body.instruction(&W::LocalGet(CHILD_HASH));
            } else {
                emit_sequence_element_address(&mut body, DATA, INDEX, STRIDE);
                body.instruction(&W::I32Const(elem.to_raw() as i32))
                    .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
            }
            emit_hash_combine(&mut body, HASH);
            body.instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End);
        }
        body.instruction(&W::LocalGet(HASH))
            .instruction(&W::Return)
            .instruction(&W::End);
    }
    body.instruction(&W::I64Const(0)).instruction(&W::End);
    Ok(body)
}

pub(super) fn compile_clone_begin(globals: RuntimeGlobals) -> Function {
    const CURRENT: u32 = 0;
    const GENERATION: u32 = 1;
    let mut body = Function::new([(2, ValType::I32)]);
    body.instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_work_head))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_active))
        .instruction(&W::GlobalGet(globals.clone_generation))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(GENERATION))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Generation zero is reserved for untouched allocation headers. A
        // full sweep on wrap keeps the alias table correct indefinitely.
        .instruction(&W::GlobalGet(globals.heap_head))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: 28,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(GENERATION))
        .instruction(&W::End)
        .instruction(&W::LocalGet(GENERATION))
        .instruction(&W::LocalTee(GENERATION))
        .instruction(&W::GlobalSet(globals.clone_generation))
        .instruction(&W::LocalGet(GENERATION))
        .instruction(&W::End);
    body
}

pub(super) fn emit_clone_memory_layout(
    body: &mut Function,
    base_local: u32,
    address_local: u32,
    generation_local: u32,
    slot_types: &[u8],
) {
    let mut slot = 0usize;
    while slot < slot_types.len() {
        match slot_types[slot] {
            value
                if value == vo_common_core::SlotType::GcBase as u8
                    || value == vo_common_core::SlotType::GcRef as u8 =>
            {
                body.instruction(&W::LocalGet(base_local));
                if slot != 0 {
                    body.instruction(&W::I32Const((slot * 8) as i32))
                        .instruction(&W::I32Add);
                }
                body.instruction(&W::LocalTee(address_local))
                    .instruction(&W::LocalGet(address_local))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(generation_local))
                    .instruction(&W::Call(DEEP_CLONE_FUNCTION_INDEX))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::I64Store(memarg(0)));
            }
            value if value == vo_common_core::SlotType::Interface0 as u8 => {
                if slot_types.get(slot + 1).copied()
                    == Some(vo_common_core::SlotType::Interface1 as u8)
                {
                    body.instruction(&W::LocalGet(base_local));
                    if slot != 0 {
                        body.instruction(&W::I32Const((slot * 8) as i32))
                            .instruction(&W::I32Add);
                    }
                    body.instruction(&W::I32Load8U(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32GeU)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(base_local))
                    .instruction(&W::I32Const(((slot + 1) * 8) as i32))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalTee(address_local))
                    .instruction(&W::LocalGet(address_local))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(generation_local))
                    .instruction(&W::Call(DEEP_CLONE_FUNCTION_INDEX))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::I64Store(memarg(0)))
                    .instruction(&W::End);
                    slot += 1;
                }
            }
            _ => {}
        }
        slot += 1;
    }
}

pub(super) fn emit_clone_map_entries(
    body: &mut Function,
    locals: CloneMapLocals,
    key_slot_types: &[u8],
    value_slot_types: &[u8],
) {
    const INDEX: u32 = 9;
    let CloneMapLocals {
        entry_local,
        count_local,
        stride_local,
        current_local,
        address_local,
        generation_local,
    } = locals;
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(count_local))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(entry_local))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(stride_local))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(current_local))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(current_local))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(current_local));
    emit_clone_memory_layout(
        body,
        current_local,
        address_local,
        generation_local,
        key_slot_types,
    );
    body.instruction(&W::LocalGet(current_local))
        .instruction(&W::I32Const((key_slot_types.len() * 8) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(current_local));
    emit_clone_memory_layout(
        body,
        current_local,
        address_local,
        generation_local,
        value_slot_types,
    );
    body.instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End);
}

pub(super) fn compile_deep_clone(
    module: &VoModule,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) -> Function {
    const HEADER: u32 = 2;
    const CURRENT: u32 = 3;
    const DESCRIPTOR: u32 = 5;
    const SOURCE_DATA: u32 = 6;
    const CLONE_DATA: u32 = 7;
    const OFFSET: u32 = 8;
    const INDEX: u32 = 9;
    const COUNT: u32 = 10;
    const ENTRY: u32 = 11;
    const STRIDE: u32 = 12;
    const ADDRESS: u32 = 13;
    const ROOT_RESULT: u32 = 14;

    let mut body = Function::new([(13, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Static strings and other immutable image references are safe to
        // share because generated code cannot mutate them.
        .instruction(&W::LocalGet(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SOURCE_DATA))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(DESCRIPTOR));

    // Send-only port capabilities keep endpoint identity across islands.
    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        if matches!(descriptor, AllocationDescriptor::Queue { .. }) {
            body.instruction(&W::LocalGet(DESCRIPTOR))
                .instruction(&W::I32Const(descriptor_id as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(0))
                .instruction(&W::Return)
                .instruction(&W::End);
        }
    }

    body.instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(SOURCE_DATA))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(OFFSET))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 28,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::I32Add)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(DESCRIPTOR))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(CLONE_DATA))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.clone_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        // Publish the source-to-destination edge before walking children so
        // cycles and repeated aliases terminate and preserve identity.
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::LocalGet(CLONE_DATA))
        .instruction(&W::I32Store(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Store(MemArg {
            offset: 28,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CLONE_DATA))
        .instruction(&W::LocalGet(SOURCE_DATA))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalGet(globals.clone_work_head))
        .instruction(&W::I32Store(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalSet(globals.clone_work_head))
        .instruction(&W::LocalGet(CLONE_DATA))
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(ROOT_RESULT))
        .instruction(&W::GlobalGet(globals.clone_active))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ROOT_RESULT))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.clone_active))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.clone_work_head))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.clone_work_head))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(DESCRIPTOR))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CLONE_DATA));

    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        body.instruction(&W::LocalGet(DESCRIPTOR))
            .instruction(&W::I32Const(descriptor_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match descriptor {
            AllocationDescriptor::None | AllocationDescriptor::Queue { .. } => {}
            AllocationDescriptor::Frame => {
                for (function_id, function) in module.functions.iter().enumerate() {
                    body.instruction(&W::LocalGet(CLONE_DATA))
                        .instruction(&W::I32Load(MemArg {
                            offset: FRAME_FUNCTION_ID_OFFSET,
                            align: 2,
                            memory_index: 0,
                        }))
                        .instruction(&W::I32Const(function_id as i32))
                        .instruction(&W::I32Eq)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::LocalGet(CLONE_DATA))
                        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                        .instruction(&W::I32Add)
                        .instruction(&W::LocalSet(CURRENT));
                    emit_clone_memory_layout(
                        &mut body,
                        CURRENT,
                        ADDRESS,
                        1,
                        &encoded_slot_types(&function.slot_types),
                    );
                    body.instruction(&W::End);
                }
            }
            AllocationDescriptor::Fixed { slot_types } => {
                emit_clone_memory_layout(&mut body, CLONE_DATA, ADDRESS, 1, slot_types);
            }
            AllocationDescriptor::Sequence {
                elem_slot_types, ..
            } => {
                emit_clone_memory_layout(
                    &mut body,
                    CLONE_DATA,
                    ADDRESS,
                    1,
                    &[vo_common_core::SlotType::GcRef as u8],
                );
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 24,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(STRIDE))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::LocalGet(COUNT))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(ENTRY))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::LocalGet(STRIDE))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(CURRENT));
                emit_clone_memory_layout(&mut body, CURRENT, ADDRESS, 1, elem_slot_types);
                body.instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End);
            }
            AllocationDescriptor::Map {
                key_slot_types,
                value_slot_types,
            } => {
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I32Const(32))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(CURRENT));
                emit_clone_memory_layout(
                    &mut body,
                    CURRENT,
                    ADDRESS,
                    1,
                    &[vo_common_core::SlotType::GcRef as u8],
                );
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 32,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::I32Const(
                        ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32,
                    ))
                    .instruction(&W::LocalSet(STRIDE));
                emit_clone_map_entries(
                    &mut body,
                    CloneMapLocals {
                        entry_local: ENTRY,
                        count_local: COUNT,
                        stride_local: STRIDE,
                        current_local: CURRENT,
                        address_local: ADDRESS,
                        generation_local: 1,
                    },
                    key_slot_types,
                    value_slot_types,
                );
            }
            AllocationDescriptor::MapEntries {
                key_slot_types,
                value_slot_types,
            } => {
                let stride = ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32;
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(HEADER))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(stride))
                    .instruction(&W::I32DivU)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::I32Const(stride))
                    .instruction(&W::LocalSet(STRIDE));
                emit_clone_map_entries(
                    &mut body,
                    CloneMapLocals {
                        entry_local: ENTRY,
                        count_local: COUNT,
                        stride_local: STRIDE,
                        current_local: CURRENT,
                        address_local: ADDRESS,
                        generation_local: 1,
                    },
                    key_slot_types,
                    value_slot_types,
                );
            }
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_active))
        .instruction(&W::LocalGet(ROOT_RESULT))
        .instruction(&W::End);
    body
}

pub(super) fn compile_map_lookup() -> Function {
    const CAPACITY: u32 = 3;
    const MASK: u32 = 4;
    const INDEX: u32 = 5;
    const ENTRY: u32 = 6;
    const BYTE_INDEX: u32 = 7;
    const KEY_BYTES: u32 = 8;
    const STRIDE: u32 = 9;
    const FIRST_TOMBSTONE: u32 = 10;
    const PROBES: u32 = 11;
    const KEY_KIND: u32 = 12;
    const HASH_BITS: u32 = 13;
    let mut body = Function::new([(10, ValType::I32), (1, ValType::I64)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(CAPACITY))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(MASK))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(KEY_BYTES))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(STRIDE))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load8U(MemArg {
            offset: 40,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(KEY_KIND))
        .instruction(&W::I32Const(17))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_HASH_FUNCTION_INDEX))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Interface as i32))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load(MemArg {
            offset: 48,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Else)
        // Deterministic scalar mixer. Full raw-key equality below resolves
        // collisions for wider, non-managed keys.
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::LocalSet(HASH_BITS))
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HASH_BITS))
        .instruction(&W::I64Const(0x7fff_ffff))
        .instruction(&W::I64And)
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I64Const(0))
        .instruction(&W::LocalSet(HASH_BITS))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(13))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HASH_BITS))
        .instruction(&W::I64Const(0x7fff_ffff_ffff_ffff))
        .instruction(&W::I64And)
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I64Const(0))
        .instruction(&W::LocalSet(HASH_BITS))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HASH_BITS))
        .instruction(&W::I64Const(-49064778989728563))
        .instruction(&W::I64Mul)
        .instruction(&W::I32WrapI64)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MASK))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(FIRST_TOMBSTONE))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(PROBES))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(2))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(2))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(2))
        .instruction(&W::I64Eq)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::LocalSet(FIRST_TOMBSTONE))
        .instruction(&W::End)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_INDEX))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(17))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Load(MemArg {
            offset: 16,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::F32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(13))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::F64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(17))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::F32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(13))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::F64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load(MemArg {
            offset: 48,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(BYTE_INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::LocalGet(KEY_BYTES))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32Ne)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(BYTE_INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(MASK))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::LocalGet(PROBES))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(PROBES))
        .instruction(&W::LocalGet(CAPACITY))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(2))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

pub(super) fn compile_map_grow(globals: RuntimeGlobals) -> Function {
    const OLD_CAPACITY: u32 = 1;
    const NEW_CAPACITY: u32 = 2;
    const STRIDE: u32 = 3;
    const OLD_DATA: u32 = 4;
    const NEW_DATA: u32 = 5;
    const INDEX: u32 = 6;
    const ENTRY: u32 = 7;
    const DESTINATION: u32 = 8;
    const ALLOCATION_BYTES: u32 = 9;
    let mut body = Function::new([(9, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(OLD_CAPACITY))
        .instruction(&W::I32Const(1 << 30))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(OLD_CAPACITY))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(NEW_CAPACITY))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(STRIDE))
        .instruction(&W::LocalGet(NEW_CAPACITY))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Mul)
        .instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64GtU)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(NEW_CAPACITY))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalTee(ALLOCATION_BYTES))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load(MemArg {
            offset: 56,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(NEW_DATA))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(NEW_DATA))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(ALLOCATION_BYTES))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(OLD_DATA))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(NEW_CAPACITY))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(NEW_DATA))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(OLD_CAPACITY))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(OLD_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(1))
        .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
        .instruction(&W::LocalTee(DESTINATION))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(DESTINATION))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::End);
    body
}

/// Allocate an internal frame block.
///
/// Parameter 0 is the byte size and parameter 1 selects eager zeroing. Durable
/// frames use eager zeroing for language zero values. Stack chunks initialize
/// each active frame before publishing it and can safely preserve unused bytes.
pub(super) fn compile_frame_alloc(globals: RuntimeGlobals, frame_descriptor: u32) -> Function {
    const PREVIOUS: u32 = 2;
    const CURRENT: u32 = 3;
    const NEXT: u32 = 4;
    const SIZE: u32 = 5;
    let mut body = Function::new([(4, ValType::I32)]);
    body.instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.free_blocks))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(PREVIOUS))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ALLOCATION_SIZE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(SIZE))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(NEXT))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::GlobalSet(globals.free_blocks))
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ALLOCATION_SIZE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalSet(PREVIOUS))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(frame_descriptor as i32))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(0))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ALLOCATION_SIZE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::End);
    body
}

pub(super) fn compile_string_decode() -> Function {
    const LENGTH: u32 = 2;
    const DATA: u32 = 3;
    const REMAINING: u32 = 4;
    const LEAD: u32 = 5;
    const BYTE_1: u32 = 6;
    const BYTE_2: u32 = 7;
    const BYTE_3: u32 = 8;
    const RUNE: u32 = 9;
    const WIDTH: u32 = 10;
    let mut body = Function::new([(9, ValType::I32)]);
    body.instruction(&W::I32Const(0xfffd))
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(LENGTH))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32LeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(DATA))
        .instruction(&W::LocalGet(LENGTH))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(REMAINING))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(LEAD))
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        // Two-byte UTF-8 sequence.
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xc2))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xdf))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(REMAINING))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 1,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(BYTE_1))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0x1f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(6))
        .instruction(&W::I32Shl)
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Or)
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(2))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::End)
        // Three-byte UTF-8 sequence. The lead-specific BYTE_1 limits reject
        // overlong encodings and UTF-16 surrogates.
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xe0))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xef))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(REMAINING))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 1,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_1))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 2,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_2))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xe0))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0xa0))
        .instruction(&W::I32GeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xed))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x9f))
        .instruction(&W::I32LeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0x0f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Shl)
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(6))
        .instruction(&W::I32Shl)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Or)
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(3))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::End)
        // Four-byte UTF-8 sequence, limited to Unicode scalar values.
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf0))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf4))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(REMAINING))
        .instruction(&W::I32Const(4))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 1,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_1))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 2,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_2))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 3,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_3))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(BYTE_3))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf0))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x90))
        .instruction(&W::I32GeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf4))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x8f))
        .instruction(&W::I32LeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0x07))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(18))
        .instruction(&W::I32Shl)
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Shl)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(6))
        .instruction(&W::I32Shl)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(BYTE_3))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Or)
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(4))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(WIDTH))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Const(32))
        .instruction(&W::I64Shl)
        .instruction(&W::LocalGet(RUNE))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Or)
        .instruction(&W::End);
    body
}

pub(super) fn compile_string_compare() -> Function {
    const A_LEN: u32 = 2;
    const B_LEN: u32 = 3;
    const A_DATA: u32 = 4;
    const B_DATA: u32 = 5;
    const INDEX: u32 = 6;
    const MIN_LEN: u32 = 7;
    const A_BYTE: u32 = 8;
    let mut body = Function::new([(7, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(A_LEN))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(A_DATA))
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(B_LEN))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(B_DATA))
        .instruction(&W::End)
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::End)
        .instruction(&W::LocalSet(MIN_LEN))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(MIN_LEN))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(A_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(A_BYTE))
        .instruction(&W::LocalGet(B_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(A_BYTE))
        .instruction(&W::LocalGet(B_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(1))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(1))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    body
}
