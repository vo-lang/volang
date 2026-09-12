//! Generated scheduling, dispatch, defer, and panic control.
use super::*;

/// Convert a host-authored UTF-8 string object into an ordinary language
/// panic. The caller frame is explicit so re-entrant host calls enter the same
/// defer/recover state machine as the originating `CallExtern` instruction.
pub(super) fn compile_raise_host_panic() -> Function {
    let mut body = Function::new([(1, ValType::I32)]);
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(2))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(2))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(0))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::End);
    body
}

pub(super) fn compile_panic_message(globals: RuntimeGlobals) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(0))
        .instruction(&W::Else)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_PANIC_SLOT0_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(ValueKind::String as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_PANIC_SLOT1_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    body
}

pub(super) fn compile_panic_slot(globals: RuntimeGlobals, offset: u64) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I64)))
        .instruction(&W::I64Const(0))
        .instruction(&W::Else)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::End);
    body
}

pub(super) fn compile_materialized_indirect_thunk(target: u32) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::Call(target))
        .instruction(&W::End);
    body
}

pub(super) fn compile_invalid_indirect_thunk() -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    body
}

pub(super) fn compile_dynamic_dispatch_lookup() -> Function {
    const MIDDLE: u32 = 3;
    const ADDRESS: u32 = 4;
    const RECORD_KEY: u32 = 5;
    let mut body = Function::new([(2, ValType::I32), (1, ValType::I64)]);
    body.instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32ShrU)
        .instruction(&W::LocalTee(MIDDLE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(ADDRESS))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::LocalTee(RECORD_KEY))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ADDRESS))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(2))
        .instruction(&W::LocalGet(RECORD_KEY))
        .instruction(&W::I64LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(MIDDLE))
        .instruction(&W::LocalSet(1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ADDRESS))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(0))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(MIDDLE))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(1))
        .instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

pub(super) fn emit_inline_dynamic_dispatch(
    body: &mut Function,
    entries: impl IntoIterator<Item = (u64, u32, u32)>,
) {
    body.instruction(&W::Block(BlockType::Empty));
    for (identity, target, abi_data) in entries {
        body.instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(identity as i64))
            .instruction(&W::I64Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const(target as i32))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::I32Const(abi_data as i32))
            .instruction(&W::LocalSet(LENGTH_LOCAL))
            .instruction(&W::Br(1))
            .instruction(&W::End);
    }
    return_status(body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End);
}

pub(super) fn compile_function_dispatch(table_len: u32) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(table_len as i32))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(0))
        .instruction(&W::CallIndirect {
            type_index: 1,
            table_index: 1,
        })
        .instruction(&W::End);
    body
}

/// Drive a non-suspending durable subtree to completion inside a bounded
/// direct call. The second function table maps every Vo function identity to
/// its resumable body, so calls made after the transition remain entirely on
/// the explicit per-fiber stack. This preserves side effects already performed
/// by the native segment and avoids consuming additional engine stack.
pub(super) fn compile_synchronous_materialized_run(
    dispatch_index: u32,
    globals: RuntimeGlobals,
) -> Function {
    const STATUS: u32 = 3;
    const FIBER: u32 = 4;
    const FRAME: u32 = 5;
    const RAW: u32 = 6;
    const PARENT: u32 = 7;

    let mut body = Function::new([(5, ValType::I32)]);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(FIBER))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.frame_limit))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::Call(dispatch_index))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32Const(GC_DEBT_TRIGGER_BYTES))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Call(GC_COLLECT_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_CALL_TRANSFER))
        .instruction(&W::I32Eq)
        .instruction(&W::BrIf(0))
        // An unwind has resumable state in the current durable frame. The
        // selected subtree cannot suspend, so it is safe to advance that state
        // immediately instead of yielding to the outer fiber scheduler.
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::BrIf(0))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    // Restore the owning scheduler frame before the direct adapter releases
    // its shadow record. The adapter restores the exact previous frame limit.
    body.instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::Return)
        .instruction(&W::End)
        // Suspension here would contradict the transitive capability proof.
        // Fail deterministically while keeping the owning fiber frame valid.
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(PARENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(PARENT))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(PARENT))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    body
}

pub(super) fn compile_run_defer(
    _module: &ModuleAnalysis<'_>,
    _dispatch_index: u32,
    globals: RuntimeGlobals,
    nil_function_panic_ref: u32,
) -> Function {
    const RAW: u32 = 1;
    const ENTRY: u32 = 2;
    const CHILD_RAW: u32 = 3;
    const CHILD: u32 = 4;
    const TARGET: u32 = 5;
    const STATUS: u32 = 6;
    const SAVED_LIMIT: u32 = 7;
    const FLAGS: u32 = 8;
    const ARG_SLOTS: u32 = 9;
    const FRAME_BYTES: u32 = 10;
    const CLOSURE_PREFIX: u32 = 11;

    let mut body = Function::new([(11, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(RAW))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ACTIVE_DEFER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(ENTRY))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::If(BlockType::Empty))
        // A deferred call can suspend while it is unwinding (for example, a
        // recover followed by a new panic). Reconstruct the dispatch flags
        // from the retained active entry on every resume.
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(FLAGS))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ENTRY))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_DEFER_DONE))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FLAGS))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(ENTRY))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ACTIVE_DEFER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::End);
    // The entry is already detached from the pending list. Publish its trap
    // without inventing a child frame or changing the caller's defer depth.
    body.instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ACTIVE_DEFER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(CHILD_RAW))
        .instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::I64Const(i64::from(nil_function_panic_ref)))
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::LocalSet(STATUS))
        // The new panic has already escaped this saved call. Discard the
        // displaced panic context exactly as for a completed panicking defer.
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32ShrU)
        .instruction(&W::End)
        .instruction(&W::LocalSet(TARGET))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ARG_SLOTS))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(FRAME_BYTES))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 48,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(CLOSURE_PREFIX))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(CHILD))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(SAVED_LIMIT))
        .instruction(&W::I32Const(STACK_RESERVE_BYTES as i32))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_STACK_OVERFLOW))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(CHILD_RAW))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_OUT_OF_MEMORY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::LocalGet(TARGET))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    emit_memory_call(&mut body, MEMORY_FRAME_REGISTER, &[W::LocalGet(CHILD_RAW)]);
    body.instruction(&W::Drop)
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::LocalGet(SAVED_LIMIT))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CHILD))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    for (fiber_offset, frame_offset) in [
        (
            FIBER_DIRECT_DEFER_FRAME_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_FRAME_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_PARENT_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_PARENT_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_RECOVERED_OFFSET,
        ),
    ] {
        body.instruction(&W::LocalGet(CHILD_RAW))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Load(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Store(MemArg {
                offset: frame_offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FRAME_PREVIOUS_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(8))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(56))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(ARG_SLOTS))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_FRAME_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_PARENT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(TARGET))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    mark_scheduler_progress(&mut body, globals);
    body.instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(CHILD_RAW))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    for (fiber_offset, frame_offset) in [
        (
            FIBER_DIRECT_DEFER_FRAME_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_FRAME_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_PARENT_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_PARENT_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_RECOVERED_OFFSET,
        ),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(CHILD))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: frame_offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64Store(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I64Load(MemArg {
            offset: FRAME_PREVIOUS_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ACTIVE_DEFER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_RECOVERED_PARENT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_RECOVERED_MODE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_RECOVERED_PARENT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_RECOVERED_MODE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_DEFER_DONE))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::End);
    body
}

pub(super) fn compile_scheduler_start(
    module: &ModuleAnalysis<'_>,
    entry_function_id: u32,
    dispatch_index: u32,
    globals: RuntimeGlobals,
    stack_base: u32,
    stack_limit: u32,
    island_state_descriptor: u32,
) -> Result<Function, WasmAotError> {
    const ROOT_FIBER: u32 = 0;
    const CURRENT_FIBER: u32 = 1;
    const STATUS: u32 = 2;
    const RAW_FRAME: u32 = 3;
    const FRAME: u32 = 4;
    const RECORD: u32 = 5;
    const PREVIOUS_FIBER: u32 = 6;
    const NEXT_FIBER: u32 = 7;
    const ISLAND_STATE: u32 = 8;
    const POPPED_CHILD: u32 = 9;
    const CALL_STEPS: u32 = 10;

    let entry = module
        .functions
        .get(entry_function_id as usize)
        .ok_or_else(|| WasmAotError::InvalidModule("entry function is missing".into()))?;
    let entry_bytes = u32::from(entry.local_slots) * 8 + FRAME_STATE_BYTES;
    let global_slots = module.globals.iter().try_fold(0u32, |total, global| {
        total
            .checked_add(u32::from(global.slots))
            .ok_or_else(|| WasmAotError::InvalidModule("global slot count overflow".into()))
    })?;
    let island_state_bytes = global_slots
        .checked_add(1)
        .and_then(|slots| slots.checked_mul(8))
        .ok_or_else(|| WasmAotError::InvalidModule("island state size exceeds wasm32".into()))?;
    if entry_bytes > STACK_RESERVE_BYTES {
        return Err(WasmAotError::InvalidModule(
            "entry frame exceeds the reserved Core-Wasm stack".into(),
        ));
    }
    let entry_end = stack_base
        .checked_add(entry_bytes)
        .ok_or_else(|| WasmAotError::InvalidModule("Core-Wasm entry frame overflow".into()))?;
    if entry_end > stack_limit {
        return Err(WasmAotError::InvalidModule(
            "Core-Wasm root stack layout is truncated".into(),
        ));
    }

    let mut body = Function::new([(11, ValType::I32)]);
    body.instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.host_yield_requested));
    body.instruction(&W::GlobalGet(globals.scheduler_initialized))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(stack_base as i32))
        .instruction(&W::LocalSet(RAW_FRAME))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(stack_limit as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(entry_function_id as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    emit_memory_call(&mut body, MEMORY_FRAME_REGISTER, &[W::LocalGet(RAW_FRAME)]);
    body.instruction(&W::Drop)
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(entry_bytes as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME));
    emit_memory_call(
        &mut body,
        MEMORY_ISLAND_NEW,
        &[
            W::I32Const(island_state_bytes as i32),
            W::I32Const(island_state_descriptor as i32),
            W::I32Const(1),
        ],
    );
    body.instruction(&W::LocalTee(ISLAND_STATE))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ISLAND_STATE))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(island_state_bytes as i32))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::I32Const(
            (FRAME_STATE_BYTES + FIBER_RECORD_BYTES) as i32,
        ))
        .instruction(&W::I32Const(FRAME_ALLOC_FIBER))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::LocalSet(ROOT_FIBER))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(i64::from(entry_function_id)))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::LocalGet(ISLAND_STATE))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    for (offset, value) in [
        (FIBER_SHADOW_HEAD_OFFSET, 0),
        (FIBER_SHADOW_CHUNK_OFFSET, stack_base),
        (FIBER_SHADOW_TOP_OFFSET, stack_base + entry_bytes),
        (FIBER_SHADOW_LIMIT_OFFSET, stack_limit),
        (FIBER_DIRECT_BUDGET_OFFSET, STACK_RESERVE_BYTES),
    ] {
        body.instruction(&W::LocalGet(RECORD))
            .instruction(&W::I64Const(i64::from(value)))
            .instruction(&W::I64Store(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(RECORD))
        .instruction(&W::GlobalSet(globals.fiber_head))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::GlobalSet(globals.fiber_tail));
    emit_memory_call(&mut body, MEMORY_FIBER_READY, &[W::LocalGet(RECORD)]);
    body.instruction(&W::Drop)
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.scheduler_initialized))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.fiber_head))
        .instruction(&W::LocalSet(ROOT_FIBER))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.scheduler_progress))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.host_wait_pending))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32Const(GC_DEBT_TRIGGER_BYTES))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Call(GC_COLLECT_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End);
    emit_memory_call(&mut body, MEMORY_SCHEDULE, &[]);
    body.instruction(&W::LocalSet(CURRENT_FIBER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(PREVIOUS_FIBER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CALL_STEPS))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Eqz)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Const(-2))
        .instruction(&W::I32GeU)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Const(-1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_YIELD))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Const(-2))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::GlobalSet(globals.current_fiber))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(FRAME))
        // Only pending entry traps have no guest frame. current_fiber already
        // owns their panic value, so report it without entering caller unwind.
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.frame_limit))
        .instruction(&W::LocalGet(CALL_STEPS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(SCHEDULER_BLOCK_QUANTUM))
        .instruction(&W::GlobalSet(globals.execution_quantum))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.memory_failed))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(MEMORY_ISLAND_STATUS))
        .instruction(&W::I32Const(-1))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(0))
        .instruction(&W::Call(0))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(STATUS_OUT_OF_MEMORY))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::Call(dispatch_index))
        .instruction(&W::End)
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::GlobalGet(globals.memory_failed))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(MEMORY_ISLAND_STATUS))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(0))
        .instruction(&W::Call(0))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_YIELD))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.host_yield_requested))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.scheduler_progress))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32Const(GC_DEBT_TRIGGER_BYTES))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Call(GC_COLLECT_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CALL_STEPS))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(CALL_STEPS))
        .instruction(&W::I32Const(SCHEDULER_CALL_QUANTUM))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_CALL_TRANSFER))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_CALL_TRANSFER))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(POPPED_CHILD))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(RAW_FRAME))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Ne)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Ne)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    mark_scheduler_progress(&mut body, globals);
    body.instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(POPPED_CHILD))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(POPPED_CHILD))
        .instruction(&W::LocalGet(CALL_STEPS))
        .instruction(&W::I32Const(SCHEDULER_CALL_QUANTUM))
        .instruction(&W::I32LtU)
        .instruction(&W::I32And)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(POPPED_CHILD))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        // A child island becomes routable only after its complete package
        // initializer fiber has returned. Offset zero in the per-island state
        // is reserved for this publication flag; language globals begin at
        // offset eight.
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.scheduler_progress))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::LocalGet(ROOT_FIBER))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OK);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(NEXT_FIBER));
    emit_memory_call(
        &mut body,
        MEMORY_FIBER_PREVIOUS,
        &[W::LocalGet(CURRENT_FIBER)],
    );
    body.instruction(&W::LocalSet(PREVIOUS_FIBER))
        .instruction(&W::LocalGet(PREVIOUS_FIBER))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS_FIBER))
        .instruction(&W::LocalGet(NEXT_FIBER))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT_FIBER))
        .instruction(&W::GlobalSet(globals.fiber_head))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.fiber_tail))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS_FIBER))
        .instruction(&W::GlobalSet(globals.fiber_tail))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_SHADOW_CHUNK_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(RAW_FRAME))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CALL_STEPS));
    emit_memory_call(&mut body, MEMORY_SCHEDULE, &[]);
    body.instruction(&W::LocalSet(CURRENT_FIBER))
        .instruction(&W::Br(2))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    mark_scheduler_progress(&mut body, globals);
    body.instruction(&W::Else)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    // A quantum returns the current fiber to the tail only when no wait owns
    // it. Preserve that position across an actual host yield.
    body.instruction(&W::GlobalGet(globals.host_yield_requested))
        .instruction(&W::If(BlockType::Empty));
    emit_memory_call(
        &mut body,
        MEMORY_SCHEDULE,
        &[W::LocalGet(CURRENT_FIBER), W::I32Const(1)],
    );
    body.instruction(&W::Drop)
        .instruction(&W::I32Const(STATUS_YIELD))
        .instruction(&W::Return)
        .instruction(&W::End);
    emit_memory_call(&mut body, MEMORY_SCHEDULE, &[W::LocalGet(CURRENT_FIBER)]);
    body.instruction(&W::LocalSet(CURRENT_FIBER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CALL_STEPS))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.host_yield_requested))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_YIELD))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.scheduler_progress))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    body.instruction(&W::GlobalGet(globals.host_wait_pending))
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_WOULD_BLOCK);
    body.instruction(&W::End);
    return_status(&mut body, STATUS_DEADLOCK);
    body.instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    Ok(body)
}
