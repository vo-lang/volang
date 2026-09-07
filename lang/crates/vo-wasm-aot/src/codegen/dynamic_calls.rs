//! Dynamic invocation and closure adaptation.
use super::*;

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_invoke_dynamic_child(
    body: &mut Function,
    _module: &VoModule,
    target: u32,
    wasm_target: u32,
    current_block: u32,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
) {
    emit_pending_child_address(body);
    body.instruction(&W::LocalSet(ALLOC_LOCAL));
    save_resume_block(body, current_block);
    if materialized.contains(&target) {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_COMPLETION_STATUS_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(STATUS_LOCAL))
            .instruction(&W::LocalGet(STATUS_LOCAL))
            .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Store(MemArg {
                offset: FIBER_FRAME_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Const(target as i32))
            .instruction(&W::I32Store(MemArg {
                offset: FIBER_FUNCTION_OFFSET,
                align: 2,
                memory_index: 0,
            }));
        mark_scheduler_progress(body, globals);
        return_call_transfer(body, current_block);
        body.instruction(&W::End);
    } else {
        body.instruction(&W::GlobalGet(globals.frame_limit))
            .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_LIMIT_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::GlobalSet(globals.frame_limit))
            // The dynamic child is an isolated invocation boundary. Direct
            // callees root their shadow frames and panic state in this child,
            // allowing the caller to translate a failed dynamic invocation
            // without mutating its own unwind state.
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Load(MemArg {
                offset: FIBER_DIRECT_BUDGET_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::Call(wasm_target))
            .instruction(&W::LocalSet(STATUS_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::GlobalSet(globals.frame_limit));
    }
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    if materialized.contains(&target) {
        save_resume_block(body, current_block);
        return_status(body, STATUS_UNWIND_PENDING);
    } else {
        // A direct-ABI function has no resumable unwind state. Its panic is
        // rooted in the isolated dynamic child frame, so reaching the dynamic
        // boundary completes that unwind and lets the caller translate it to
        // a regular dynamic-call error.
        body.instruction(&W::I32Const(STATUS_PANIC))
            .instruction(&W::LocalSet(STATUS_LOCAL));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    emit_finish_dynamic_child(body, globals);
    body.instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    emit_finish_dynamic_child(body, globals);
    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
}

pub(super) fn emit_clear_caught_panic(body: &mut Function, globals: RuntimeGlobals) {
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    for (context_offset, fiber_offset) in [
        (0, FIBER_PANIC_SLOT0_OFFSET),
        (8, FIBER_PANIC_SLOT1_OFFSET),
        (16, FIBER_ACTIVE_PANIC_GENERATION_OFFSET),
        (24, FIBER_PREVIOUS_PANIC_OFFSET),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: context_offset,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Store(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::Else);
    for offset in [
        FIBER_PANIC_SLOT0_OFFSET,
        FIBER_PANIC_SLOT1_OFFSET,
        FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
        FIBER_PREVIOUS_PANIC_OFFSET,
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::End);
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_caught_panic_error(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    branch_depth: u32,
) -> Result<(), WasmAotError> {
    body.instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    // End the panic epoch before allocating the ordinary dynamic error. This
    // keeps GC and any later call in the caller frame outside the caught
    // unwind context.
    emit_clear_caught_panic(body, globals);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "dynamic call panicked",
    )?;
    body.instruction(&W::Br(branch_depth)).instruction(&W::End);
    Ok(())
}

pub(super) fn dynamic_call_abi(
    caller: &FunctionDef,
    pc: usize,
    fixed_prefix: u16,
) -> Result<DynamicCallAbi, WasmAotError> {
    let (arg_slots, ret_slots) = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing dynamic call layout metadata",
                caller.name
            ))
        })?;
    let suffix = arg_slots.checked_sub(fixed_prefix).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} dynamic call argument prefix is truncated",
            caller.name
        ))
    })?;
    if suffix % 2 != 0 || ret_slots < 2 {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} has an invalid dynamic call ABI",
            caller.name
        )));
    }
    Ok(DynamicCallAbi {
        fixed_prefix,
        ret_count: suffix / 2,
        error_offset: ret_slots - 2,
    })
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_dynamic_call_error(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    kind: DynamicErrorKind,
    message: &str,
) -> Result<(), WasmAotError> {
    for offset in 0..abi.error_offset {
        store_const(body, instruction.a + offset, 0);
    }
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a + abi.error_offset,
        message,
        Some(kind),
    )
}

pub(super) fn emit_dynamic_call_protocol_error(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    source_address_local: u32,
) {
    for offset in 0..abi.error_offset {
        store_const(body, instruction.a + offset, 0);
    }
    store_prefix(body, instruction.a + abi.error_offset);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Store(memarg(0)));
    store_prefix(body, instruction.a + abi.error_offset + 1);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(memarg(0)));
}

pub(super) fn emit_dynamic_call_success(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
) {
    store_const(body, instruction.a + abi.error_offset, 0);
    store_const(body, instruction.a + abi.error_offset + 1, 0);
}

pub(super) fn emit_dynamic_args_len(body: &mut Function, args_slice_slot: u16) {
    load_slot(body, args_slice_slot);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(0))
        .instruction(&W::Else);
    load_slot(body, args_slice_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::End);
}

pub(super) fn emit_load_dynamic_any_argument(
    body: &mut Function,
    args_slice_slot: u16,
    index_local: Option<u32>,
    index: u32,
    scratch_slot: u16,
) {
    load_slot(body, args_slice_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64);
    if let Some(index_local) = index_local {
        body.instruction(&W::LocalGet(index_local));
    } else {
        body.instruction(&W::I32Const(index as i32));
    }
    body.instruction(&W::I32Const(16))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    store_prefix(body, scratch_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Store(memarg(0)));
    store_prefix(body, scratch_slot + 1);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(memarg(0)));
}

pub(super) fn dynamic_variadic_element(
    module: &VoModule,
    signature: &DynamicFunctionSignature,
) -> Result<Option<ValueRttid>, WasmAotError> {
    if !signature.variadic {
        return Ok(None);
    }
    let variadic = signature.params.last().ok_or_else(|| {
        WasmAotError::InvalidModule("variadic dynamic signature has no final parameter".into())
    })?;
    let Some((_, RuntimeType::Slice(element))) = module
        .runtime_type_resolver()
        .resolve_value_rttid(*variadic)
    else {
        return Err(WasmAotError::InvalidModule(
            "variadic dynamic signature does not end in a slice".into(),
        ));
    };
    Ok(Some(*element))
}

pub(super) fn emit_dynamic_arguments_compatible(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    args_slice_slot: u16,
    scratch_slot: u16,
) -> Result<(), WasmAotError> {
    let variadic_element = dynamic_variadic_element(module, signature)?;
    let fixed_count = signature.params.len() - usize::from(variadic_element.is_some());
    emit_dynamic_args_len(body, args_slice_slot);
    body.instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(fixed_count as i32))
        .instruction(&if variadic_element.is_some() {
            W::I32GeU
        } else {
            W::I32Eq
        })
        .instruction(&W::If(BlockType::Result(ValType::I32)));
    body.instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LOW_LOCAL));
    for (index, target) in signature.params.iter().take(fixed_count).enumerate() {
        emit_load_dynamic_any_argument(body, args_slice_slot, None, index as u32, scratch_slot);
        emit_dynamic_value_compatible(body, module, scratch_slot, *target);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else)
            .instruction(&W::I32Const(0))
            .instruction(&W::LocalSet(LOW_LOCAL))
            .instruction(&W::End);
    }
    if let Some(element) = variadic_element {
        body.instruction(&W::I32Const(fixed_count as i32))
            .instruction(&W::LocalSet(CAPACITY_LOCAL))
            .instruction(&W::Block(BlockType::Empty))
            .instruction(&W::Loop(BlockType::Empty))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32GeU)
            .instruction(&W::BrIf(1));
        emit_load_dynamic_any_argument(
            body,
            args_slice_slot,
            Some(CAPACITY_LOCAL),
            0,
            scratch_slot,
        );
        emit_dynamic_value_compatible(body, module, scratch_slot, element);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else)
            .instruction(&W::I32Const(0))
            .instruction(&W::LocalSet(LOW_LOCAL))
            .instruction(&W::Br(2))
            .instruction(&W::End)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32Const(1))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(CAPACITY_LOCAL))
            .instruction(&W::Br(0))
            .instruction(&W::End)
            .instruction(&W::End);
    }
    body.instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    Ok(())
}

pub(super) fn emit_dynamic_child_slot_address(body: &mut Function, slot_offset: u16) {
    emit_pending_child_address(body);
    body.instruction(&W::I32Const(i32::from(slot_offset) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_fill_dynamic_child_arguments(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
    capture_source: DynamicCaptureSource,
    args_slice_slot: u16,
    scratch_slot: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    match target.abi.prefix {
        ClosureArgumentPrefix::None => {}
        ClosureArgumentPrefix::ClosureRef => {
            let DynamicCaptureSource::ClosureInterface(closure_slot) = capture_source else {
                return Err(WasmAotError::InvalidModule(
                    "closure-reference ABI requires a closure interface source".into(),
                ));
            };
            emit_dynamic_child_slot_address(body, 0);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
            load_slot(body, closure_slot + 1);
            body.instruction(&W::I64Store(memarg(0)));
        }
        ClosureArgumentPrefix::ReceiverCaptures(slots) => {
            emit_dynamic_child_slot_address(body, 0);
            match capture_source {
                DynamicCaptureSource::ClosureInterface(closure_slot) => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, closure_slot + 1);
                    body.instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(8))
                        .instruction(&W::I32Add)
                        .instruction(&W::I32Const(i32::from(slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
                DynamicCaptureSource::ReceiverInterfaceData(data_slot) if slots == 1 => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, data_slot);
                    body.instruction(&W::I64Store(memarg(0)));
                }
                DynamicCaptureSource::ReceiverInterfaceData(data_slot) => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, data_slot);
                    body.instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(i32::from(slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
        }
    }

    let variadic_element = dynamic_variadic_element(module, signature)?;
    let fixed_count = signature.params.len() - usize::from(variadic_element.is_some());
    let mut destination_slot = target.abi.arg_offset;
    for (index, parameter) in signature.params.iter().take(fixed_count).enumerate() {
        let (_, bytes) = {
            let (bytes, slots) = dynamic_element_bytes(module, *parameter)?;
            (slots, bytes)
        };
        emit_load_dynamic_any_argument(body, args_slice_slot, None, index as u32, scratch_slot);
        emit_dynamic_child_slot_address(body, destination_slot);
        if bytes > 0 {
            emit_dynamic_store_value(
                body,
                module,
                *parameter,
                scratch_slot,
                scratch_slot + 1,
                SEQUENCE_LOCAL,
                bytes,
            )?;
        }
        destination_slot = destination_slot
            .checked_add(
                u16::try_from(
                    module
                        .slot_layout_for_value_rttid(*parameter)
                        .ok_or_else(|| {
                            WasmAotError::InvalidModule(
                                "dynamic parameter layout is missing".into(),
                            )
                        })?
                        .len(),
                )
                .map_err(|_| {
                    WasmAotError::InvalidModule("dynamic parameter layout exceeds u16".into())
                })?,
            )
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic parameter offset exceeds u16".into())
            })?;
    }

    let Some(element) = variadic_element else {
        return Ok(());
    };
    emit_dynamic_args_len(body, args_slice_slot);
    body.instruction(&W::I32Const(fixed_count as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_child_slot_address(body, destination_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else);
    let (elem_bytes, _) = dynamic_element_bytes(module, element)?;
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(
        body,
        *descriptors
            .sequence_by_value
            .get(&element.to_raw())
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic variadic element descriptor is missing".into())
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
        .instruction(&W::I64Const(i64::from(elem_bytes)))
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));
    emit_dynamic_child_slot_address(body, destination_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(fixed_count as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LOW_LOCAL));
    emit_load_dynamic_any_argument(body, args_slice_slot, Some(LOW_LOCAL), 0, scratch_slot);
    if elem_bytes > 0 {
        emit_dynamic_child_slot_address(body, destination_slot);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32Const(elem_bytes as i32))
            .instruction(&W::I32Mul)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_store_value(
            body,
            module,
            element,
            scratch_slot,
            scratch_slot + 1,
            SEQUENCE_LOCAL,
            elem_bytes,
        )?;
    }
    body.instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    Ok(())
}

pub(super) fn dynamic_call_meta_slots(
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    index: u16,
) -> (u16, u16) {
    let meta = instruction.c + abi.fixed_prefix + index;
    (meta, meta + abi.ret_count)
}

pub(super) fn dynamic_result_output_slots(
    module: &VoModule,
    target: ValueRttid,
    is_any: bool,
) -> u16 {
    let width = module
        .slot_layout_for_value_rttid(target)
        .map_or(0, |layout| layout.len());
    if is_any
        || target.value_kind() == ValueKind::Array
        || (target.value_kind() == ValueKind::Struct && width > 2)
    {
        2
    } else if width == 1 {
        1
    } else {
        2
    }
}

pub(super) fn emit_dynamic_return_contract_matches(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
) {
    body.instruction(&W::I32Const(i32::from(
        abi.ret_count == signature.results.len() as u16,
    )))
    .instruction(&W::LocalSet(LOW_LOCAL));
    for (index, actual) in signature.results.iter().copied().enumerate() {
        let (meta_slot, is_any_slot) = dynamic_call_meta_slots(instruction, abi, index as u16);
        load_slot(body, is_any_slot);
        body.instruction(&W::I64Const(1)).instruction(&W::I64Eq);
        load_slot(body, meta_slot);
        body.instruction(&W::I64Const(0xff))
            .instruction(&W::I64And)
            .instruction(&W::I64Const(i64::from(ValueKind::Interface as u8)))
            .instruction(&W::I64Eq)
            .instruction(&W::I32And);
        let mut emitted_assignable = false;
        for rttid in 0..module.runtime_types.len() as u32 {
            let Some(target) = module.value_rttid_for_rttid(rttid) else {
                continue;
            };
            if !runtime_value_is_assignable(actual, target, module) {
                continue;
            }
            load_slot(body, is_any_slot);
            body.instruction(&W::I64Eqz);
            load_slot(body, meta_slot);
            body.instruction(&W::I64Const(i64::from(target.to_raw())))
                .instruction(&W::I64Eq)
                .instruction(&W::I32And)
                .instruction(&W::I32Or);
            emitted_assignable = true;
        }
        let _ = emitted_assignable;
        body.instruction(&W::LocalGet(LOW_LOCAL))
            .instruction(&W::I32And)
            .instruction(&W::LocalSet(LOW_LOCAL));
    }
    body.instruction(&W::LocalGet(LOW_LOCAL));
}

pub(super) fn emit_prepare_dynamic_boxed_result(
    body: &mut Function,
    module: &VoModule,
    target: ValueRttid,
    scratch_slot: u16,
) -> Result<u16, WasmAotError> {
    let layout = module.slot_layout_for_value_rttid(target).ok_or_else(|| {
        WasmAotError::InvalidModule("dynamic result target layout is missing".into())
    })?;
    let width = layout.len();
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
                        "dynamic result interface metadata is missing".into(),
                    )
                })?;
            if target_meta_id != 0 {
                load_slot(body, scratch_slot);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::Else);
                store_prefix(body, scratch_slot);
                load_slot(body, scratch_slot);
                body.instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Const(i64::from(target_meta_id) << 32))
                    .instruction(&W::I64Or)
                    .instruction(&W::I64Store(memarg(0)))
                    .instruction(&W::End);
            }
        }
        ValueKind::Array => store_const(body, scratch_slot, 0),
        ValueKind::Struct if width > 2 => store_const(body, scratch_slot, 0),
        ValueKind::Struct => {
            load_slot(body, scratch_slot + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            for offset in 0..2u16 {
                if usize::from(offset) < width {
                    store_prefix(body, scratch_slot + offset);
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: u64::from(offset) * 8,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Store(memarg(0)));
                } else {
                    store_const(body, scratch_slot + offset, 0);
                }
            }
        }
        _ => {
            store_prefix(body, scratch_slot);
            load_slot(body, scratch_slot + 1);
            body.instruction(&W::I64Store(memarg(0)));
            store_const(body, scratch_slot + 1, 0);
        }
    }
    Ok(dynamic_result_output_slots(module, target, false))
}

pub(super) fn emit_copy_dynamic_result_scratch(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    scratch_slot: u16,
    output_slots: u16,
) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.a) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
    store_prefix(body, scratch_slot);
    body.instruction(&W::I32Const(i32::from(output_slots) * 8))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(i32::from(output_slots)))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CAPACITY_LOCAL));
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_pack_dynamic_child_returns(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    scratch_slot: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL));
    let function = &module.functions[target.function_id as usize];
    let mut source_slot = function.param_slots;
    for (index, actual) in signature.results.iter().copied().enumerate() {
        let layout = module.slot_layout_for_value_rttid(actual).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic return source layout is missing".into())
        })?;
        let source_bytes = u32::try_from(layout.len().checked_mul(8).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic return source layout overflows wasm32".into())
        })?)
        .map_err(|_| WasmAotError::InvalidModule("dynamic return source exceeds wasm32".into()))?;
        emit_dynamic_child_slot_address(body, source_slot);
        if source_bytes == 0 {
            store_const(body, scratch_slot, i64::from(actual.to_raw()));
            store_const(body, scratch_slot + 1, 0);
        } else {
            emit_dynamic_box_from_address(
                body,
                module,
                actual,
                SEQUENCE_LOCAL,
                source_bytes,
                scratch_slot,
                descriptors,
                globals,
            )?;
        }
        let (meta_slot, is_any_slot) = dynamic_call_meta_slots(instruction, abi, index as u16);
        load_slot(body, is_any_slot);
        body.instruction(&W::I64Const(1))
            .instruction(&W::I64Eq)
            .instruction(&W::If(BlockType::Empty));
        emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, 2);
        body.instruction(&W::Else)
            .instruction(&W::Block(BlockType::Empty));
        for rttid in 0..module.runtime_types.len() as u32 {
            let Some(expected) = module.value_rttid_for_rttid(rttid) else {
                continue;
            };
            if !runtime_value_is_assignable(actual, expected, module) {
                continue;
            }
            load_slot(body, meta_slot);
            body.instruction(&W::I64Const(i64::from(expected.to_raw())))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            let output_slots =
                emit_prepare_dynamic_boxed_result(body, module, expected, scratch_slot)?;
            emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, output_slots);
            body.instruction(&W::Br(1)).instruction(&W::End);
        }
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End).instruction(&W::End);
        source_slot = source_slot
            .checked_add(u16::try_from(layout.len()).map_err(|_| {
                WasmAotError::InvalidModule("dynamic return layout exceeds u16".into())
            })?)
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic return offset exceeds u16".into())
            })?;
    }
    body.instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(i32::from(abi.error_offset)))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_pack_dynamic_boxed_result(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    source_address_local: u32,
    scratch_slot: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    mismatch_message: &str,
) -> Result<(), WasmAotError> {
    store_prefix(body, scratch_slot);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Store(memarg(0)));
    store_prefix(body, scratch_slot + 1);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(memarg(0)));
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::Block(BlockType::Empty));
    let (meta_slot, is_any_slot) = dynamic_call_meta_slots(instruction, abi, 0);
    load_slot(body, is_any_slot);
    body.instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty));
    emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, 2);
    body.instruction(&W::Br(1)).instruction(&W::End);
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(expected) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        load_slot(body, is_any_slot);
        body.instruction(&W::I64Eqz);
        load_slot(body, meta_slot);
        body.instruction(&W::I64Const(i64::from(expected.to_raw())))
            .instruction(&W::I64Eq)
            .instruction(&W::I32And);
        emit_dynamic_value_compatible(body, module, scratch_slot, expected);
        body.instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty));
        let output_slots = emit_prepare_dynamic_boxed_result(body, module, expected, scratch_slot)?;
        emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, output_slots);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::TypeMismatch,
        mismatch_message,
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_call_protocol(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = module.well_known.call_object_iface_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let wasm_target = *function_indices.get(&target).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} CallObject target {target} is outside the AOT image",
                caller.name
            ))
        })?;
        let callee = &module.functions[target as usize];
        if callee.param_slots != 2 || callee.ret_slots != 4 {
            return Err(WasmAotError::InvalidModule(format!(
                "CallObject target {target} has an invalid Core-Wasm ABI"
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        if abi.ret_count > 1 {
            emit_dynamic_call_error(
                body,
                module,
                instruction,
                abi,
                descriptors,
                globals,
                static_data,
                DynamicErrorKind::SigMismatch,
                "CallObject only supports single return",
            )?;
            body.instruction(&W::Br(1)).instruction(&W::End);
            continue;
        }
        emit_prepare_dynamic_child_frame(
            body,
            module,
            target,
            materialized,
            current_block,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
            globals,
            |body| {
                emit_dynamic_child_slot_address(body, 0);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 1);
                body.instruction(&W::I64Store(memarg(0)));
                emit_dynamic_child_slot_address(body, 1);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 2);
                body.instruction(&W::I64Store(memarg(0)));
                Ok(())
            },
        )?;
        emit_invoke_dynamic_child(
            body,
            module,
            target,
            wasm_target,
            current_block,
            materialized,
            globals,
        );
        emit_dynamic_caught_panic_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            2,
        )?;
        emit_dynamic_child_slot_address(body, callee.param_slots + 2);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Or)
            .instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_protocol_error(body, instruction, abi, SEQUENCE_LOCAL);
        emit_finish_dynamic_child(body, globals);
        body.instruction(&W::Br(2)).instruction(&W::End);
        if abi.ret_count == 1 {
            emit_dynamic_child_slot_address(body, callee.param_slots);
            emit_pack_dynamic_boxed_result(
                body,
                module,
                instruction,
                abi,
                SEQUENCE_LOCAL,
                instruction.c,
                descriptors,
                globals,
                static_data,
                "CallObject return type mismatch",
            )?;
        }
        emit_finish_dynamic_child(body, globals);
        if abi.ret_count == 0 {
            for offset in 0..abi.error_offset {
                store_const(body, instruction.a + offset, 0);
            }
        }
        emit_dynamic_call_success(body, instruction, abi);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_known_dynamic_closure_call(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    capture_source: DynamicCaptureSource,
    args_slice_slot: u16,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    if !dynamic_signature_matches_target(module, signature, target)? {
        return Err(WasmAotError::InvalidModule(format!(
            "dynamic target {} does not match signature {}",
            target.function_id,
            signature.value_rttid.rttid()
        )));
    }
    let wasm_target = *function_indices.get(&target.function_id).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} dynamic target {} is outside the AOT image",
            caller.name, target.function_id
        ))
    })?;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c + abi.fixed_prefix - 1);
    body.instruction(&W::I64Const(i64::from(abi.ret_count)))
        .instruction(&W::I64Eq)
        .instruction(&W::I32Const(i32::from(
            usize::from(abi.ret_count) == signature.results.len(),
        )))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::SigMismatch,
        "return count mismatch: hint: adjust LHS variable count to match function signature",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_dynamic_return_contract_matches(body, module, signature, instruction, abi);
    body.instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::TypeMismatch,
        "dynamic return type mismatch",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_dynamic_arguments_compatible(body, module, signature, args_slice_slot, instruction.a)?;
    body.instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::SigMismatch,
        "argument type mismatch",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_prepare_dynamic_child_frame(
        body,
        module,
        target.function_id,
        materialized,
        current_block,
        static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
        globals,
        |body| {
            emit_fill_dynamic_child_arguments(
                body,
                module,
                signature,
                target,
                capture_source,
                args_slice_slot,
                instruction.a,
                descriptors,
                globals,
            )
        },
    )?;
    emit_invoke_dynamic_child(
        body,
        module,
        target.function_id,
        wasm_target,
        current_block,
        materialized,
        globals,
    );
    emit_dynamic_caught_panic_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        1,
    )?;
    emit_pack_dynamic_child_returns(
        body,
        module,
        signature,
        target,
        instruction,
        abi,
        instruction.c,
        descriptors,
        globals,
    )?;
    emit_finish_dynamic_child(body, globals);
    emit_dynamic_call_success(body, instruction, abi);
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_closure_call(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    closure_slot: u16,
    args_slice_slot: u16,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, closure_slot + 1);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "closure is null",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);

    for signature in dynamic_function_signatures(module) {
        let targets = dynamic_closure_targets_for_signature(module, &signature)?;
        if targets.is_empty() {
            continue;
        }
        emit_interface_identity_matches(body, closure_slot, signature.value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + abi.fixed_prefix - 1);
        body.instruction(&W::I64Const(i64::from(abi.ret_count)))
            .instruction(&W::I64Eq)
            .instruction(&W::I32Const(i32::from(
                usize::from(abi.ret_count) == signature.results.len(),
            )))
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "return count mismatch: hint: adjust LHS variable count to match function signature",
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        emit_dynamic_return_contract_matches(body, module, &signature, instruction, abi);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::TypeMismatch,
            "dynamic return type mismatch",
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        emit_dynamic_arguments_compatible(
            body,
            module,
            &signature,
            args_slice_slot,
            instruction.a,
        )?;
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "argument type mismatch",
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        for target in targets {
            let wasm_target = *function_indices.get(&target.function_id).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} dynamic target {} is outside the AOT image",
                    caller.name, target.function_id
                ))
            })?;
            load_slot(body, closure_slot + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(target.encoded_identity()))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            emit_prepare_dynamic_child_frame(
                body,
                module,
                target.function_id,
                materialized,
                current_block,
                static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
                globals,
                |body| {
                    emit_fill_dynamic_child_arguments(
                        body,
                        module,
                        &signature,
                        target,
                        DynamicCaptureSource::ClosureInterface(closure_slot),
                        args_slice_slot,
                        instruction.a,
                        descriptors,
                        globals,
                    )
                },
            )?;
            emit_invoke_dynamic_child(
                body,
                module,
                target.function_id,
                wasm_target,
                current_block,
                materialized,
                globals,
            );
            emit_dynamic_caught_panic_error(
                body,
                module,
                instruction,
                abi,
                descriptors,
                globals,
                static_data,
                3,
            )?;
            emit_pack_dynamic_child_returns(
                body,
                module,
                &signature,
                target,
                instruction,
                abi,
                instruction.c,
                descriptors,
                globals,
            )?;
            emit_finish_dynamic_child(body, globals);
            emit_dynamic_call_success(body, instruction, abi);
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::BadCall,
            "invalid closure signature",
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "invalid closure signature",
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_call(
    body: &mut Function,
    module: &VoModule,
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
    let abi = dynamic_call_abi(caller, pc, 4)?;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::NilBase,
        "cannot call nil",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_call_protocol(
        body,
        module,
        caller,
        pc,
        instruction,
        abi,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
        descriptors,
    )?;
    load_slot(body, instruction.c);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(ValueKind::Closure as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    compile_dynamic_closure_call(
        body,
        module,
        caller,
        pc,
        instruction,
        abi,
        instruction.c,
        instruction.c + 2,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
        descriptors,
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "cannot call value",
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_method_protocol(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = module.well_known.attr_object_iface_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let wasm_target = *function_indices.get(&target).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} AttrObject target {target} is outside the AOT image",
                caller.name
            ))
        })?;
        let callee = &module.functions[target as usize];
        if callee.param_slots != 2 || callee.ret_slots != 4 {
            return Err(WasmAotError::InvalidModule(format!(
                "AttrObject target {target} has an invalid Core-Wasm ABI"
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        emit_prepare_dynamic_child_frame(
            body,
            module,
            target,
            materialized,
            current_block,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
            globals,
            |body| {
                emit_dynamic_child_slot_address(body, 0);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 1);
                body.instruction(&W::I64Store(memarg(0)));
                emit_dynamic_child_slot_address(body, 1);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 2);
                body.instruction(&W::I64Store(memarg(0)));
                Ok(())
            },
        )?;
        emit_invoke_dynamic_child(
            body,
            module,
            target,
            wasm_target,
            current_block,
            materialized,
            globals,
        );
        emit_dynamic_caught_panic_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            2,
        )?;
        emit_dynamic_child_slot_address(body, callee.param_slots + 2);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Or)
            .instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_protocol_error(body, instruction, abi, SEQUENCE_LOCAL);
        emit_finish_dynamic_child(body, globals);
        body.instruction(&W::Br(2)).instruction(&W::End);
        emit_dynamic_child_slot_address(body, callee.param_slots);
        store_prefix(body, instruction.c);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Store(memarg(0)));
        store_prefix(body, instruction.c + 1);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Store(memarg(0)));
        emit_finish_dynamic_child(body, globals);
        load_slot(body, instruction.c);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(0xff))
            .instruction(&W::I32And)
            .instruction(&W::I32Const(ValueKind::Closure as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        compile_dynamic_closure_call(
            body,
            module,
            caller,
            pc,
            instruction,
            abi,
            instruction.c,
            instruction.c + 3,
            current_block,
            function_indices,
            materialized,
            globals,
            static_data,
            descriptors,
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::BadCall,
            "method lookup returned a non-callable value",
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_method(
    body: &mut Function,
    module: &VoModule,
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
    let abi = dynamic_call_abi(caller, pc, 5)?;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::NilBase,
        "cannot call method on nil",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_method_protocol(
        body,
        module,
        caller,
        pc,
        instruction,
        abi,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
        descriptors,
    )?;
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(named_id) = module.named_type_id_for_rttid(rttid) else {
            continue;
        };
        let named = module
            .named_type_metas
            .get(named_id as usize)
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic named method metadata is missing".into())
            })?;
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        for (name, method) in &named.methods {
            if !is_exported_name(name)
                || (method.is_pointer_receiver && value_rttid.value_kind() != ValueKind::Pointer)
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
            if !function_indices.contains_key(&method.func_id) {
                return Err(WasmAotError::InvalidModule(format!(
                    "dynamic method {name} target {} is outside the AOT image",
                    method.func_id
                )));
            }
            emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
            body.instruction(&W::If(BlockType::Empty));
            let signature = dynamic_function_signature(module, method.signature_rttid)?;
            let target_function = &module.functions[method.func_id as usize];
            let target = ClosureCallTarget {
                function_id: method.func_id,
                capture_slots: target_function.recv_slots,
                abi: closure_call_abi(target_function, target_function.recv_slots)?,
            };
            compile_known_dynamic_closure_call(
                body,
                module,
                caller,
                pc,
                instruction,
                abi,
                DynamicCaptureSource::ReceiverInterfaceData(instruction.c + 1),
                instruction.c + 3,
                &signature,
                target,
                current_block,
                function_indices,
                materialized,
                globals,
                static_data,
                descriptors,
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::BadField,
            "method not found",
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadField,
        "method not found",
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_dynamic_pack_any_slice(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    arg_slots: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    if arg_slots < 2 || !(arg_slots - 2).is_multiple_of(2) {
        return Err(WasmAotError::InvalidModule(
            "dyn_pack_any_slice has an invalid argument window".into(),
        ));
    }
    let arg_count = (arg_slots - 2) / 2;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Const(i64::from(arg_count)))
        .instruction(&W::I64Eq);
    load_slot(body, instruction.c + 1);
    body.instruction(&W::I64Const(1))
        .instruction(&W::I64LeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_pack_error(
        body,
        module,
        instruction,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "dynamic packed argument layout is invalid",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);

    load_slot(body, instruction.c + 1);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(i32::from(arg_count)))
        .instruction(&W::LocalSet(LENGTH_LOCAL));
    emit_allocate_dynamic_any_slice(body, instruction, descriptors, globals)?;
    if arg_count > 0 {
        load_slot(body, instruction.a);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(32))
            .instruction(&W::I32Add);
        store_prefix(body, instruction.c + 2);
        body.instruction(&W::I32Const(i32::from(arg_count) * 16))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    store_const(body, instruction.a + 1, 0);
    store_const(body, instruction.a + 2, 0);
    body.instruction(&W::Br(1)).instruction(&W::Else);

    if arg_count == 0 {
        emit_dynamic_pack_error(
            body,
            module,
            instruction,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "spread arg must be slice",
        )?;
        body.instruction(&W::Br(1));
    } else {
        let last_slot0 = instruction.c + 2 + (arg_count - 1) * 2;
        let last_slot1 = last_slot0 + 1;
        for rttid in 0..module.runtime_types.len() as u32 {
            let Some(slice_value) = module.value_rttid_for_rttid(rttid) else {
                continue;
            };
            let Some((_, RuntimeType::Slice(elem))) = module
                .runtime_type_resolver()
                .resolve_value_rttid(slice_value)
            else {
                continue;
            };
            let elem = *elem;
            let (elem_bytes, _) = dynamic_element_bytes(module, elem)?;
            emit_interface_identity_matches(body, last_slot0, slice_value.to_raw());
            body.instruction(&W::If(BlockType::Empty));
            load_slot(body, last_slot1);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, last_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::I32Const(i32::from(arg_count - 1)))
                .instruction(&W::I32Add)
                .instruction(&W::LocalTee(LENGTH_LOCAL))
                .instruction(&W::I32Const(((u32::MAX - 32) / 16) as i32))
                .instruction(&W::I32GtU)
                .instruction(&W::If(BlockType::Empty));
            emit_dynamic_pack_error(
                body,
                module,
                instruction,
                descriptors,
                globals,
                static_data,
                DynamicErrorKind::BadCall,
                "dynamic packed argument length exceeds wasm32",
            )?;
            body.instruction(&W::Br(3)).instruction(&W::End);
            emit_allocate_dynamic_any_slice(body, instruction, descriptors, globals)?;
            if arg_count > 1 {
                load_slot(body, instruction.a);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(32))
                    .instruction(&W::I32Add);
                store_prefix(body, instruction.c + 2);
                body.instruction(&W::I32Const(i32::from(arg_count - 1) * 16))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
            body.instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(CAPACITY_LOCAL));
            load_slot(body, last_slot1);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, last_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            load_slot(body, last_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
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
                instruction.a + 1,
                descriptors,
                globals,
            )?;
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(arg_count - 1)))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            store_prefix(body, instruction.a + 1);
            body.instruction(&W::I32Const(16))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                })
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End);
            store_const(body, instruction.a + 1, 0);
            store_const(body, instruction.a + 2, 0);
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_pack_error(
            body,
            module,
            instruction,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "spread arg must be slice",
        )?;
        body.instruction(&W::Br(1));
    }
    body.instruction(&W::End).instruction(&W::End);
    Ok(())
}
