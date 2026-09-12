//! Generated managed allocation, frame storage, and collection.
use super::*;

pub(super) fn compile_allocator(globals: RuntimeGlobals) -> Function {
    let mut body = Function::new([]);
    emit_memory_call(
        &mut body,
        MEMORY_ALLOC,
        &[W::LocalGet(0), W::GlobalGet(globals.allocation_descriptor)],
    );
    body.instruction(&W::End);
    body
}

pub(super) fn compile_host_allocator(globals: RuntimeGlobals) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(1))
        .instruction(&W::End);
    body
}

/// Allocate with a compiler-emitted precise GC descriptor. The descriptor is
/// range-checked so a host adapter cannot make the collector index outside the
/// authenticated descriptor table.
pub(super) fn compile_host_typed_allocator(
    globals: RuntimeGlobals,
    descriptor_count: u32,
) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(1))
        .instruction(&W::I32Const(descriptor_count as i32))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(1))
        .instruction(&W::End);
    body
}

/// Allocate a slice header/backing object with the precise element scanner
/// selected from the compiler-authenticated ValueMeta carried by the append
/// and conversion helper ABI. Unknown metadata fails closed.
pub(super) fn compile_host_sequence_allocator(
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) -> Function {
    let mut body = Function::new([]);
    for (value_meta, descriptor) in &descriptors.sequence_by_meta {
        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(*value_meta as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const(*descriptor as i32))
            .instruction(&W::GlobalSet(globals.allocation_descriptor))
            .instruction(&W::LocalGet(0))
            .instruction(&W::Call(1))
            .instruction(&W::Return)
            .instruction(&W::End);
    }
    for (kind, descriptor) in &descriptors.sequence_by_kind {
        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(0xff))
            .instruction(&W::I32And)
            .instruction(&W::I32Const(i32::from(*kind)))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const(*descriptor as i32))
            .instruction(&W::GlobalSet(globals.allocation_descriptor))
            .instruction(&W::LocalGet(0))
            .instruction(&W::Call(1))
            .instruction(&W::Return)
            .instruction(&W::End);
    }
    body.instruction(&W::I32Const(0)).instruction(&W::End);
    body
}

pub(super) fn compile_frame_free() -> Function {
    let mut body = Function::new([]);
    emit_memory_call(&mut body, MEMORY_FREE, &[W::LocalGet(0)]);
    body.instruction(&W::End);
    body
}

/// Resolve a managed base or interior pointer with a binary predecessor
/// search over the segmented index. No object count switches to a linear walk.
pub(super) fn compile_find_allocation() -> Function {
    let mut body = Function::new([]);
    emit_memory_call(&mut body, MEMORY_FIND, &[W::LocalGet(0)]);
    body.instruction(&W::End);
    body
}

pub(super) fn compile_index_panic_message(
    globals: RuntimeGlobals,
    prefix_ref: u32,
    middle_ref: u32,
) -> Function {
    const INDEX_WORK: u32 = 3;
    const LENGTH_WORK: u32 = 4;
    const MESSAGE: u32 = 5;
    const INDEX_DIGITS: u32 = 6;
    const LENGTH_DIGITS: u32 = 7;
    const CURSOR: u32 = 8;
    const PAYLOAD_LENGTH: u32 = 9;
    const PREFIX: &str = "runtime error: index out of range [";
    const MIDDLE: &str = "] with length ";

    let mut body = Function::new([(2, ValType::I64), (5, ValType::I32)]);
    for (parameter, work, digits) in [
        (0, INDEX_WORK, INDEX_DIGITS),
        (1, LENGTH_WORK, LENGTH_DIGITS),
    ] {
        body.instruction(&W::LocalGet(parameter))
            .instruction(&W::LocalSet(work))
            .instruction(&W::I32Const(1))
            .instruction(&W::LocalSet(digits))
            .instruction(&W::Block(BlockType::Empty))
            .instruction(&W::Loop(BlockType::Empty))
            .instruction(&W::LocalGet(work))
            .instruction(&W::I64Const(10))
            .instruction(&W::I64LtU)
            .instruction(&W::BrIf(1))
            .instruction(&W::LocalGet(work))
            .instruction(&W::I64Const(10))
            .instruction(&W::I64DivU)
            .instruction(&W::LocalSet(work))
            .instruction(&W::LocalGet(digits))
            .instruction(&W::I32Const(1))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(digits))
            .instruction(&W::Br(0))
            .instruction(&W::End)
            .instruction(&W::End);
    }
    body.instruction(&W::I32Const((PREFIX.len() + MIDDLE.len()) as i32))
        .instruction(&W::LocalGet(INDEX_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(LENGTH_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(PAYLOAD_LENGTH))
        .instruction(&W::LocalGet(2))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(PAYLOAD_LENGTH))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(MESSAGE))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::LocalGet(PAYLOAD_LENGTH))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(prefix_ref as i32 + 16))
        .instruction(&W::I32Const(PREFIX.len() as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });

    body.instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const((16 + PREFIX.len()) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(INDEX_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURSOR))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalSet(INDEX_WORK))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURSOR))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(CURSOR))
        .instruction(&W::LocalGet(INDEX_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64RemU)
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(i32::from(b'0')))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(INDEX_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64DivU)
        .instruction(&W::LocalTee(INDEX_WORK))
        .instruction(&W::I64Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const((16 + PREFIX.len()) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(INDEX_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(middle_ref as i32 + 16))
        .instruction(&W::I32Const(MIDDLE.len() as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(PAYLOAD_LENGTH))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURSOR))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalSet(LENGTH_WORK))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURSOR))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(CURSOR))
        .instruction(&W::LocalGet(LENGTH_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64RemU)
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(i32::from(b'0')))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64DivU)
        .instruction(&W::LocalTee(LENGTH_WORK))
        .instruction(&W::I64Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::End);
    body
}

pub(super) fn compile_gc_mark() -> Function {
    let mut body = Function::new([]);
    emit_memory_call(&mut body, MEMORY_MARK, &[W::LocalGet(0)]);
    body.instruction(&W::End);
    body
}

/// Scan only the selected function's root descriptors. Scalar slots do not
/// occur in the table, and identical layouts share their encoded storage.

pub(super) fn compile_gc_collect(globals: RuntimeGlobals) -> Function {
    let mut body = Function::new([]);
    emit_memory_call(&mut body, MEMORY_COLLECT, &[W::I32Const(4096)]);
    // Continue the allocating instruction once, then persist the next block
    // and yield. Collector cursors survive these scheduler boundaries.
    body.instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.execution_quantum));
    body.instruction(&W::End);
    body
}

/// Install a panic value on the current fiber and begin unwinding `frame`.
/// Returning `STATUS_UNWIND_PENDING` keeps the materialized frame alive so the
/// scheduler can resume it through the same defer state machine used by an
/// explicit language-level panic.
pub(super) fn compile_raise_panic(
    globals: RuntimeGlobals,
    panic_context_descriptor: u32,
) -> Function {
    const SLOT0: u32 = 0;
    const SLOT1: u32 = 1;
    const FRAME: u32 = 2;
    const ALLOCATION: u32 = 3;
    const GENERATION: u32 = 4;

    let mut body = Function::new([(1, ValType::I32), (1, ValType::I64)]);
    // A newer panic temporarily displaces the active panic. The defer unwind
    // boundary restores this context after a nested recovery, or drops it
    // when the newer panic escapes and replaces the older one.
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Const(panic_context_descriptor as i32))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOCATION))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End);
    for (fiber_offset, context_offset) in [
        (FIBER_PANIC_SLOT0_OFFSET, 0),
        (FIBER_PANIC_SLOT1_OFFSET, 8),
        (FIBER_ACTIVE_PANIC_GENERATION_OFFSET, 16),
        (FIBER_PREVIOUS_PANIC_OFFSET, 24),
    ] {
        body.instruction(&W::LocalGet(ALLOCATION))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Load(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Store(MemArg {
                offset: context_offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(ALLOCATION))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::LocalTee(GENERATION))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(GENERATION))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(SLOT0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PANIC_SLOT0_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(SLOT1))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PANIC_SLOT1_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::End);
    body
}

pub(super) fn compile_materialized_stack_frame_free(_globals: RuntimeGlobals) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::End);
    body
}

pub(super) fn emit_materialized_stack_frame_alloc(
    body: &mut Function,
    frame_bytes: u32,
    function_id: u32,
    _globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    frame_bytes
        .checked_add(FRAME_STATE_BYTES)
        .ok_or_else(|| WasmAotError::InvalidModule("call-frame chunk size overflows".into()))?;
    body.instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Const(function_id as i32))
        .instruction(&W::Call(MATERIALIZED_FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    Ok(())
}

/// Pop a child created by `emit_materialized_stack_frame_alloc`.
pub(super) fn emit_materialized_stack_frame_free(body: &mut Function, _globals: RuntimeGlobals) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::Call(MATERIALIZED_FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop);
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_call_target(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    target: u32,
    wasm_target: u32,
    caller_base: u16,
    arguments: MaterializedCallArguments,
    current_block: u32,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    stack_overflow_panic_ref: u32,
) -> Result<(), WasmAotError> {
    if !materialized.contains(&target) {
        module.functions.get(target as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} calls missing function {target}",
                caller.name
            ))
        })?;
        if direct_function_may_panic(module, target, materialized, &mut BTreeSet::new()) {
            save_resume_block(body, current_block);
        }
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(i32::from(caller_base) * 8))
            .instruction(&W::I32Add);
        load_effective_owner_frame(body, ALLOC_LOCAL);
        body.instruction(&W::GlobalGet(runtime_globals.current_fiber))
            .instruction(&W::I32Load(MemArg {
                offset: FIBER_DIRECT_BUDGET_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::Call(wasm_target));
        propagate_status(body);
        return Ok(());
    }

    let callee = module.functions.get(target as usize).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} calls missing function {target}",
            caller.name
        ))
    })?;
    let frame_bytes = required_shared_frame_slots(module, target, materialized)?
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} callee frame size overflows wasm32",
                caller.name
            ))
        })?;

    // A suspended call owns one child frame. Dynamic calls use the same slot:
    // the closure/itab dispatch is repeated on resume and deterministically
    // reaches the same target while the caller is parked. The child resides
    // in a managed span cell until this call site resumes.
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
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
    emit_materialized_stack_frame_alloc(body, frame_bytes, target, runtime_globals)?;
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
    emit_materialized_call_arguments(body, callee, arguments)?;
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
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
        .instruction(&W::GlobalGet(runtime_globals.current_fiber))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(runtime_globals.current_fiber))
        .instruction(&W::I32Const(target as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    mark_scheduler_progress(body, runtime_globals);
    return_call_transfer(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    save_resume_block(body, current_block);
    return_status(body, STATUS_UNWIND_PENDING);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    emit_materialized_stack_frame_free(body, runtime_globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    save_resume_block(body, current_block);
    return_status(body, STATUS_UNWIND_PENDING);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    emit_materialized_stack_frame_free(body, runtime_globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
    if callee.ret_slots > 0 {
        store_prefix(body, caller_base + callee.param_slots);
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(i32::from(callee.param_slots) * 8))
            .instruction(&W::I32Add)
            .instruction(&W::I32Const(i32::from(callee.ret_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    emit_materialized_stack_frame_free(body, runtime_globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    Ok(())
}
