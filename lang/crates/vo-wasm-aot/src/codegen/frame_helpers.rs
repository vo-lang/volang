//! Frame storage, unwind publication, and scalar synchronization.
use super::*;

pub(super) fn memarg(slot: u16) -> MemArg {
    MemArg {
        offset: u64::from(slot) * 8,
        align: 3,
        memory_index: 0,
    }
}

pub(super) fn packed_memarg() -> MemArg {
    MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }
}

pub(super) fn load_slot(body: &mut Function, slot: u16) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I64Load(memarg(slot)));
}

pub(super) fn store_prefix(body: &mut Function, slot: u16) {
    body.instruction(&W::LocalGet(FRAME_LOCAL));
    if slot != 0 {
        body.instruction(&W::I32Const(i32::from(slot) * 8))
            .instruction(&W::I32Add);
    }
}

pub(super) fn global_slot_address(body: &mut Function, slot: u16, globals: RuntimeGlobals) {
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(8 + i32::from(slot) * 8))
        .instruction(&W::I32Add);
}

pub(super) fn store_const(body: &mut Function, slot: u16, value: i64) {
    store_prefix(body, slot);
    body.instruction(&W::I64Const(value))
        .instruction(&W::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
}

pub(super) fn load_effective_owner_frame(body: &mut Function, temporary: u32) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(temporary))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(temporary))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End);
}

/// Materialize heap-allocated named returns into the function's canonical
/// dense return area. The bytecode keeps one GcRef per named result alive so
/// deferred closures can mutate the result. Callers, however, always consume
/// the flattened `ret_slots` layout beginning at `param_slots`.
pub(super) fn emit_finalize_heap_returns(
    body: &mut Function,
    function: &FunctionDef,
    descriptors: &AllocationDescriptors,
) {
    let mut destination = function.param_slots;
    let destinations: Vec<u16> = function
        .heap_ret_slots
        .iter()
        .copied()
        .map(|slots| {
            let current = destination;
            destination = destination
                .checked_add(slots)
                .expect("verified heap-return destination width");
            current
        })
        .collect();
    // Heap-return references occupy the first local slots in many functions.
    // Materialize right-to-left so writing the dense result area cannot
    // overwrite a later GcRef before it has been dereferenced.
    for (index, (&destination, &slots)) in destinations
        .iter()
        .zip(function.heap_ret_slots.iter())
        .enumerate()
        .rev()
    {
        if slots == 0 {
            continue;
        }
        load_slot(body, function.heap_ret_gcref_start + index as u16);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
            .instruction(&W::LocalTee(ALLOC_LOCAL))
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 12,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(LENGTH_LOCAL))
            .instruction(&W::End)
            .instruction(&W::Block(BlockType::Empty));
        for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
            let AllocationDescriptor::Sequence {
                elem_slot_types,
                elem_bytes,
                needs_sign_extend,
            } = descriptor
            else {
                continue;
            };
            let elem_slots = elem_slot_types.len() as u32;
            body.instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(descriptor_id as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(elem_slots as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Const(i32::from(slots)))
                .instruction(&W::I32Ne)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LOW_LOCAL))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            store_prefix(body, destination);
            body.instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Const((elem_slots * 8) as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            if elem_slots == 1 {
                body.instruction(&match (*elem_bytes, *needs_sign_extend) {
                    (1, false) => W::I64Load8U(packed_memarg()),
                    (1, true) => W::I64Load8S(packed_memarg()),
                    (2, false) => W::I64Load16U(packed_memarg()),
                    (2, true) => W::I64Load16S(packed_memarg()),
                    (4, false) => W::I64Load32U(packed_memarg()),
                    (4, true) => W::I64Load32S(packed_memarg()),
                    (8, _) => W::I64Load(memarg(0)),
                    _ => W::Unreachable,
                })
                .instruction(&W::I64Store(memarg(0)));
            } else {
                body.instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
            body.instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::Br(1))
                .instruction(&W::End);
        }
        store_prefix(body, destination);
        body.instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(i32::from(slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            })
            .instruction(&W::End);
    }
}

pub(super) fn emit_heap_error_is_non_nil(body: &mut Function, function: &FunctionDef) {
    let error_ref = function
        .heap_ret_gcref_start
        .checked_add(function.heap_ret_gcref_count - 1)
        .expect("verified heap return range");
    load_slot(body, error_ref);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(0xff))
        .instruction(&W::I64And)
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz);
}

pub(super) fn set_block_and_branch(body: &mut Function, block: u32, loop_depth: u32) {
    body.instruction(&W::I32Const(block as i32))
        .instruction(&W::LocalSet(BLOCK_LOCAL))
        .instruction(&W::Br(loop_depth));
}

pub(super) fn propagate_status(body: &mut Function) {
    body.instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
}

pub(super) fn save_resume_block(body: &mut Function, block: u32) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(block as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_RESUME_OFFSET,
            align: 2,
            memory_index: 0,
        }));
}

pub(super) fn return_suspended(body: &mut Function, block: u32) {
    save_resume_block(body, block);
    return_status(body, STATUS_WOULD_BLOCK);
}

pub(super) fn return_call_transfer(body: &mut Function, block: u32) {
    save_resume_block(body, block);
    return_status(body, STATUS_CALL_TRANSFER);
}

pub(super) fn mark_scheduler_progress(body: &mut Function, globals: RuntimeGlobals) {
    body.instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.scheduler_progress));
}

pub(super) fn return_status(body: &mut Function, status: i32) {
    body.instruction(&W::I32Const(status))
        .instruction(&W::Return);
}

pub(super) fn emit_fuel_poll(
    body: &mut Function,
    fuel_global: u32,
    typed_return_slots: Option<u16>,
) {
    // Negative fuel means unlimited execution. Non-negative values count
    // guest basic-block entries, so loops and recursion remain interruptible.
    body.instruction(&W::GlobalGet(fuel_global))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64GeS)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(fuel_global))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    if let Some(return_slots) = typed_return_slots {
        return_typed_status(body, STATUS_FUEL_EXHAUSTED, return_slots);
    } else {
        return_status(body, STATUS_FUEL_EXHAUSTED);
    }
    body.instruction(&W::End)
        .instruction(&W::GlobalGet(fuel_global))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Sub)
        .instruction(&W::GlobalSet(fuel_global))
        .instruction(&W::End);
}

pub(super) fn return_runtime_panic(body: &mut Function, message_ref: u32, resume_block: u32) {
    save_resume_block(body, resume_block);
    // Primitive string interface: itab=0, RTTID=String(17), kind=String(17).
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::I64Const(i64::from(message_ref)))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::Return);
}

pub(super) fn return_runtime_panic_local(
    body: &mut Function,
    message_local: u32,
    resume_block: u32,
) {
    save_resume_block(body, resume_block);
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::LocalGet(message_local))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::Return);
}

/// Build and raise the canonical index panic. The caller leaves the raw
/// unsigned index and length as two i64 operands on the Wasm stack.
pub(super) fn return_index_panic(body: &mut Function, resume_block: u32) {
    body.instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::Call(INDEX_PANIC_MESSAGE_FUNCTION_INDEX))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End);
    return_runtime_panic_local(body, ALLOC_LOCAL, resume_block);
}

pub(super) fn return_explicit_panic(body: &mut Function, source: u16, resume_block: u32) {
    save_resume_block(body, resume_block);
    load_slot(body, source);
    load_slot(body, source + 1);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::Return);
}

pub(super) fn instruction_may_suspend(
    module: &ModuleAnalysis<'_>,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    materialized: &BTreeSet<u32>,
) -> Result<bool, WasmAotError> {
    match instruction.opcode() {
        Opcode::CallExtern
        | Opcode::QueueSend
        | Opcode::QueueRecv
        | Opcode::SelectExec
        | Opcode::GoIsland => Ok(true),
        Opcode::Call | Opcode::CallClosure | Opcode::CallIface => {
            instruction_calls_materialized(module, function, pc, instruction, materialized)
        }
        _ => Ok(false),
    }
}

pub(super) fn reload_scalar_writes(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    scalar_locals: &ScalarLocals,
) -> Result<(), WasmAotError> {
    let metadata = function.instruction_metadata.get(pc);
    visit_instruction_register_writes(
        instruction,
        metadata,
        &module.externs,
        &module.functions,
        |start, count| reload_scalar_range(body, scalar_locals, start, count),
    )
    .map_err(|error| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} has invalid register-write effects: {error:?}",
            function.name
        ))
    })?;
    if let FrameMemoryEffect::AliasedRange { start, count } =
        instruction_frame_memory_effect(instruction, metadata).map_err(|error| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} has invalid frame-memory effects: {error:?}",
                function.name
            ))
        })?
    {
        reload_scalar_range(body, scalar_locals, start, count);
    }
    Ok(())
}
