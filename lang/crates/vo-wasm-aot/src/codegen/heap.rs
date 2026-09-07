//! Generated managed allocation, frame storage, and collection.
use super::*;

pub(super) fn compile_allocator(globals: RuntimeGlobals, allocation_index_base: u32) -> Function {
    const OLD: u32 = 1;
    const END: u32 = 2;
    const PAYLOAD_BYTES: u32 = 3;
    const PREVIOUS: u32 = 4;
    const CURRENT: u32 = 5;
    const NEXT: u32 = 6;
    const SIZE: u32 = 7;
    const REQUIRED_PAGES: u32 = 8;
    let mut body = Function::new([(8, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(-(HEAP_HEADER_BYTES as i32 + 8)))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(7))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(-8))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(PAYLOAD_BYTES))
        .instruction(&W::GlobalGet(globals.free_objects))
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
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(SIZE))
        .instruction(&W::LocalGet(PAYLOAD_BYTES))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(NEXT))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::I32Store(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::GlobalSet(globals.free_objects))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::GlobalGet(globals.allocation_descriptor))
        .instruction(&W::I32Store(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: 16,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(NEXT))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::End)
        .instruction(&W::GlobalSet(globals.gc_debt))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalSet(PREVIOUS))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.heap))
        .instruction(&W::LocalTee(OLD))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(PAYLOAD_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(END))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(END))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(16))
        .instruction(&W::I32ShrU)
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(REQUIRED_PAGES))
        .instruction(&W::MemorySize(0))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(REQUIRED_PAGES))
        .instruction(&W::MemorySize(0))
        .instruction(&W::I32Sub)
        .instruction(&W::MemoryGrow(0))
        .instruction(&W::I32Const(-1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        // Publish a stable, walkable allocation header before returning its
        // payload. The descriptor is selected by the allocating instruction.
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::LocalGet(PAYLOAD_BYTES))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::GlobalGet(globals.allocation_descriptor))
        .instruction(&W::I32Store(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: 16,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.heap_tail))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.heap_tail))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Store(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::GlobalSet(globals.heap_head))
        .instruction(&W::End)
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::GlobalSet(globals.heap_tail))
        // Bump allocations are monotonically addressed, so appending their
        // headers produces a sorted ownership index. Free-list reuse keeps
        // the existing entry and returns before this path.
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(ALLOCATION_INDEX_CAPACITY as i32))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(allocation_index_base as i32))
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(4))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::GlobalSet(globals.allocation_count))
        .instruction(&W::End)
        .instruction(&W::LocalGet(END))
        .instruction(&W::GlobalSet(globals.heap))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(NEXT))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::End)
        .instruction(&W::GlobalSet(globals.gc_debt))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::End);
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

pub(super) fn compile_frame_free(free_blocks_global: u32) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::GlobalGet(free_blocks_global))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(0))
        .instruction(&W::GlobalSet(free_blocks_global))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

pub(super) fn emit_mark_memory_layout(body: &mut Function, base_local: u32, slot_types: &[u8]) {
    let mut slot = 0usize;
    while slot < slot_types.len() {
        match slot_types[slot] {
            value
                if value == vo_common_core::SlotType::GcBase as u8
                    || value == vo_common_core::SlotType::GcRef as u8 =>
            {
                body.instruction(&W::LocalGet(base_local))
                    .instruction(&W::I64Load(MemArg {
                        offset: (slot * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop);
            }
            value if value == vo_common_core::SlotType::Interface0 as u8 => {
                if slot_types.get(slot + 1).copied()
                    == Some(vo_common_core::SlotType::Interface1 as u8)
                {
                    body.instruction(&W::LocalGet(base_local))
                        .instruction(&W::I64Load(MemArg {
                            offset: (slot * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(0xff))
                        .instruction(&W::I32And)
                        .instruction(&W::I32Const(14))
                        .instruction(&W::I32GeU)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::LocalGet(base_local))
                        .instruction(&W::I64Load(MemArg {
                            offset: ((slot + 1) * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                        .instruction(&W::Drop)
                        .instruction(&W::End);
                    slot += 1;
                }
            }
            _ => {}
        }
        slot += 1;
    }
}

/// Resolve a managed base or interior pointer to its live allocation header.
///
/// Bump allocations are indexed in address order, making the common path a
/// binary predecessor search. Once the fixed index is full, later headers are
/// still found through the allocation chain beginning immediately after the
/// last indexed entry. This preserves correctness for the full wasm32 address
/// space without imposing a growing side table on small browser images.
pub(super) fn compile_find_allocation(
    globals: RuntimeGlobals,
    allocation_index_base: u32,
) -> Function {
    const HEADER: u32 = 1;
    const LOW: u32 = 2;
    const HIGH: u32 = 3;
    const MID: u32 = 4;
    const CANDIDATE: u32 = 5;
    const PAYLOAD: u32 = 6;
    const END: u32 = 7;
    const CURRENT: u32 = 8;

    let mut body = Function::new([(8, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(HEADER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LOW))
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::LocalSet(HIGH))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(LOW))
        .instruction(&W::LocalGet(HIGH))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(LOW))
        .instruction(&W::LocalGet(HIGH))
        .instruction(&W::LocalGet(LOW))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(1))
        .instruction(&W::I32ShrU)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(MID))
        .instruction(&W::I32Const(4))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(allocation_index_base as i32))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(CANDIDATE))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32LeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CANDIDATE))
        .instruction(&W::LocalSet(HEADER))
        .instruction(&W::LocalGet(MID))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LOW))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(MID))
        .instruction(&W::LocalSet(HIGH))
        .instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(PAYLOAD))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(END))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(PAYLOAD))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(END))
        .instruction(&W::I32LtU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        // Every existing header is indexed until the fixed table fills.
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(ALLOCATION_INDEX_CAPACITY as i32))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(
            allocation_index_base as i32 + (ALLOCATION_INDEX_CAPACITY as i32 - 1) * 4,
        ))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(PAYLOAD))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(END))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(PAYLOAD))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(END))
        .instruction(&W::I32LtU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::Return)
        .instruction(&W::End)
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
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
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

pub(super) fn compile_gc_mark(
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) -> Function {
    const HEADER: u32 = 1;
    const CURRENT: u32 = 2;
    const DESCRIPTOR: u32 = 4;
    const INDEX: u32 = 5;
    const COUNT: u32 = 6;
    const DATA: u32 = 7;
    const STRIDE: u32 = 8;
    const ENTRY: u32 = 9;
    const CAPACITY: u32 = 10;

    let mut body = Function::new([(10, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Or)
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        // Enqueue the newly marked header. Nested mark calls only append to
        // this work list; the outermost call drains it iteratively.
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalGet(globals.gc_work_head))
        .instruction(&W::I32Store(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalSet(globals.gc_work_head))
        .instruction(&W::GlobalGet(globals.gc_mark_active))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.gc_mark_active))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.gc_work_head))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.gc_work_head))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(DESCRIPTOR))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(DATA));

    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        body.instruction(&W::LocalGet(DESCRIPTOR))
            .instruction(&W::I32Const(descriptor_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match descriptor {
            AllocationDescriptor::None | AllocationDescriptor::Frame => {}
            AllocationDescriptor::Fixed { slot_types } => {
                emit_mark_memory_layout(&mut body, DATA, slot_types);
            }
            AllocationDescriptor::Sequence {
                elem_slot_types, ..
            } => {
                body.instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalTee(ENTRY))
                    // Slice views and append results may point into a separate
                    // backing allocation. Marking the interior data pointer
                    // retains its owner before scanning logical elements.
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop)
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(DATA))
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
                emit_mark_memory_layout(&mut body, CURRENT, elem_slot_types);
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
                // The initial entry area is interior to the map allocation;
                // grown maps point at an independently allocated backing area.
                body.instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 32,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop)
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 32,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::I32Const(
                        ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32,
                    ))
                    .instruction(&W::LocalSet(STRIDE));
                emit_mark_map_entries(
                    &mut body,
                    ENTRY,
                    COUNT,
                    STRIDE,
                    key_slot_types,
                    value_slot_types,
                );
            }
            AllocationDescriptor::MapEntries {
                key_slot_types,
                value_slot_types,
            } => {
                let stride = ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32;
                body.instruction(&W::LocalGet(DATA))
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
                emit_mark_map_entries(
                    &mut body,
                    ENTRY,
                    COUNT,
                    STRIDE,
                    key_slot_types,
                    value_slot_types,
                );
            }
            AllocationDescriptor::Queue { elem_slot_types } => {
                body.instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_HOME_ISLAND_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop)
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_LENGTH_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_CAPACITY_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(CAPACITY))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_DATA_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_ELEMENT_BYTES_OFFSET,
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
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_HEAD_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalGet(CAPACITY))
                    .instruction(&W::I32RemU)
                    .instruction(&W::LocalGet(STRIDE))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(CURRENT));
                emit_mark_memory_layout(&mut body, CURRENT, elem_slot_types);
                body.instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    // An unbuffered sender parks one payload in the queue
                    // until a receiver acknowledges it.
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(ENTRY))
                    .instruction(&W::LocalSet(CURRENT));
                emit_mark_memory_layout(&mut body, CURRENT, elem_slot_types);
                body.instruction(&W::End);
            }
        }
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    body.instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.gc_mark_active))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

pub(super) fn emit_mark_map_entries(
    body: &mut Function,
    entry_local: u32,
    count_local: u32,
    stride_local: u32,
    key_slot_types: &[u8],
    value_slot_types: &[u8],
) {
    const INDEX: u32 = 5;
    const CURRENT: u32 = 2;
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
        .instruction(&W::LocalTee(CURRENT))
        .instruction(&W::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURRENT));
    emit_mark_memory_layout(body, CURRENT, key_slot_types);
    body.instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const((key_slot_types.len() * 8) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURRENT));
    emit_mark_memory_layout(body, CURRENT, value_slot_types);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End);
}

pub(super) fn emit_mark_global_layout(body: &mut Function, first_global: u32, slot_types: &[u8]) {
    let mut slot = 0usize;
    while slot < slot_types.len() {
        match slot_types[slot] {
            value
                if value == vo_common_core::SlotType::GcBase as u8
                    || value == vo_common_core::SlotType::GcRef as u8 =>
            {
                body.instruction(&W::GlobalGet(first_global + slot as u32))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop);
            }
            value if value == vo_common_core::SlotType::Interface0 as u8 => {
                if slot_types.get(slot + 1).copied()
                    == Some(vo_common_core::SlotType::Interface1 as u8)
                {
                    body.instruction(&W::GlobalGet(first_global + slot as u32))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(0xff))
                        .instruction(&W::I32And)
                        .instruction(&W::I32Const(14))
                        .instruction(&W::I32GeU)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::GlobalGet(first_global + slot as u32 + 1))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                        .instruction(&W::Drop)
                        .instruction(&W::End);
                    slot += 1;
                }
            }
            _ => {}
        }
        slot += 1;
    }
}

pub(super) fn compile_gc_collect(
    module: &VoModule,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) -> Function {
    const FIBER: u32 = 0;
    const FRAME: u32 = 1;
    const FUNCTION_ID: u32 = 2;
    const CURRENT: u32 = 3;
    const FLAGS: u32 = 4;
    const DESCRIPTOR: u32 = 5;

    let mut body = Function::new([(6, ValType::I32)]);
    let mut global_index = 0u32;
    for global in &module.globals {
        emit_mark_global_layout(
            &mut body,
            global_index,
            &encoded_slot_types(&global.slot_types),
        );
        global_index += u32::from(global.slots);
    }

    body.instruction(&W::GlobalGet(globals.fiber_head))
        .instruction(&W::LocalSet(FIBER))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FUNCTION_ID));
    for (function_id, function) in module.functions.iter().enumerate() {
        body.instruction(&W::LocalGet(FUNCTION_ID))
            .instruction(&W::I32Const(function_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        emit_mark_memory_layout(&mut body, FRAME, &encoded_slot_types(&function.slot_types));
        body.instruction(&W::End);
    }
    for offset in [FRAME_DEFER_HEAD_OFFSET, FRAME_ACTIVE_DEFER_OFFSET] {
        body.instruction(&W::LocalGet(FRAME))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
            .instruction(&W::Drop);
    }
    body.instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_SHADOW_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FUNCTION_ID));
    for (function_id, function) in module.functions.iter().enumerate() {
        body.instruction(&W::LocalGet(FUNCTION_ID))
            .instruction(&W::I32Const(function_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        emit_mark_memory_layout(&mut body, FRAME, &encoded_slot_types(&function.slot_types));
        body.instruction(&W::End);
    }
    body.instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(SHADOW_FRAME_LINK_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: SHADOW_PREVIOUS_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PANIC_SLOT0_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(14))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PANIC_SLOT1_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FIBER))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.heap_head))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(FLAGS))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(DESCRIPTOR))
        .instruction(&W::I32Const(descriptors.frame as i32))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(-3))
        .instruction(&W::I32And)
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::I32Store(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::GlobalGet(globals.free_objects))
        .instruction(&W::I32Store(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::GlobalSet(globals.free_objects))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
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
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.gc_debt))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
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

pub(super) fn compile_materialized_stack_frame_free(globals: RuntimeGlobals) -> Function {
    const FRAME: u32 = 0;
    const FRAME_HEADER: u32 = 1;
    const PREVIOUS_CHUNK: u32 = 2;
    const PREVIOUS_TOP: u32 = 3;
    const PREVIOUS_LIMIT: u32 = 4;
    const CURRENT_CHUNK: u32 = 5;

    let mut body = Function::new([(5, ValType::I32)]);
    body.instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(FRAME_HEADER));
    for (local, offset) in [
        (PREVIOUS_CHUNK, FRAME_PREVIOUS_STACK_CHUNK_OFFSET),
        (PREVIOUS_TOP, FRAME_PREVIOUS_STACK_TOP_OFFSET),
        (PREVIOUS_LIMIT, FRAME_PREVIOUS_STACK_LIMIT_OFFSET),
        (CURRENT_CHUNK, FRAME_STACK_CHUNK_OFFSET),
    ] {
        body.instruction(&W::LocalGet(FRAME_HEADER))
            .instruction(&W::I32Load(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(local));
    }
    for (offset, local) in [
        (FIBER_SHADOW_CHUNK_OFFSET, PREVIOUS_CHUNK),
        (FIBER_SHADOW_TOP_OFFSET, PREVIOUS_TOP),
        (FIBER_SHADOW_LIMIT_OFFSET, PREVIOUS_LIMIT),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

pub(super) fn emit_materialized_stack_frame_alloc(
    body: &mut Function,
    frame_bytes: u32,
    _globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    frame_bytes
        .checked_add(FRAME_STATE_BYTES)
        .ok_or_else(|| WasmAotError::InvalidModule("call-frame chunk size overflows".into()))?;
    body.instruction(&W::I32Const(frame_bytes as i32))
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
    module: &VoModule,
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
    // on the fiber's explicit chunk stack until this call site resumes.
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
    emit_materialized_stack_frame_alloc(body, frame_bytes, runtime_globals)?;
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(target as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
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
