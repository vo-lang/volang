//! Compiler-authenticated layouts consumed by the Island memory owner.
use super::*;
use std::fmt::Write;

pub(super) const MEMORY_METADATA_SECTION: &str = "volang.memory.v1";
pub(super) const MEMORY_ALLOC: i32 = -1;
pub(super) const MEMORY_FIND: i32 = -2;
pub(super) const MEMORY_COLLECT: i32 = -3;
pub(super) const MEMORY_MARK: i32 = -4;
pub(super) const MEMORY_FREE: i32 = -5;
pub(super) const MEMORY_CLONE_RESET: i32 = -6;
pub(super) const MEMORY_WRITE: i32 = -7;
pub(super) const MEMORY_ISLAND_NEW: i32 = -8;
pub(super) const MEMORY_SCOPE: i32 = -9;
pub(super) const MEMORY_FRAME_REGISTER: i32 = -10;

pub(super) fn emit_memory_call(body: &mut Function, operation: i32, operands: &[W<'_>]) {
    body.instruction(&W::I32Const(operation));
    for operand in operands {
        body.instruction(operand);
    }
    for _ in operands.len()..4 {
        body.instruction(&W::I32Const(0));
    }
    body.instruction(&W::Call(0));
}

pub(super) fn encode_memory_metadata(
    module: &ModuleAnalysis<'_>,
    descriptors: &AllocationDescriptors,
    static_data: &StaticData,
) -> Vec<u8> {
    let mut text = format!(
        "{{\"version\":1,\"frameDescriptor\":{},\"islandDescriptor\":{},\"stackBase\":{},\"stackLimit\":{},\"barrierPages\":{},\"frameBytes\":{},\"frameFunction\":{},\"frameParent\":{},\"frameDefers\":{:?},\"fiberBytes\":{},\"fiberNext\":{},\"fiberFrame\":{},\"fiberIsland\":{},\"fiberPanicGeneration\":{},\"fiberPanic\":{},\"fiberPreviousPanic\":{},\"descriptors\":[",
        descriptors.frame, descriptors.island_state, static_data.stack_base, static_data.stack_limit, static_data.barrier_pages,
        FRAME_STATE_BYTES, FRAME_FUNCTION_ID_OFFSET, FRAME_PARENT_OFFSET,
        [FRAME_DEFER_HEAD_OFFSET, FRAME_ACTIVE_DEFER_OFFSET], FIBER_RECORD_BYTES, FIBER_NEXT_OFFSET,
        FIBER_FRAME_OFFSET, FIBER_ISLAND_STATE_OFFSET, FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
        FIBER_PANIC_SLOT0_OFFSET, FIBER_PREVIOUS_PANIC_OFFSET,
    );
    for (index, descriptor) in descriptors.entries.iter().enumerate() {
        if index != 0 {
            text.push(',');
        }
        match descriptor {
            AllocationDescriptor::None => text.push_str("[0,0,[],[]]"),
            AllocationDescriptor::Frame => text.push_str("[1,0,[],[]]"),
            AllocationDescriptor::Fixed { slot_types } => {
                write!(text, "[2,0,{slot_types:?},[]]").unwrap();
            }
            AllocationDescriptor::Sequence {
                elem_slot_types,
                elem_bytes,
                ..
            } => {
                write!(text, "[3,{elem_bytes},{elem_slot_types:?},[]]").unwrap();
            }
            AllocationDescriptor::Map {
                key_slot_types,
                value_slot_types,
            } => {
                write!(text, "[4,0,{key_slot_types:?},{value_slot_types:?}]").unwrap();
            }
            AllocationDescriptor::MapEntries {
                key_slot_types,
                value_slot_types,
            } => {
                write!(text, "[5,0,{key_slot_types:?},{value_slot_types:?}]").unwrap();
            }
            AllocationDescriptor::Queue { elem_slot_types } => {
                write!(text, "[6,0,{elem_slot_types:?},[]]").unwrap();
            }
        }
    }
    text.push_str("],\"frames\":[");
    for (index, function) in module.functions.iter().enumerate() {
        if index != 0 {
            text.push(',');
        }
        write!(text, "{:?}", encoded_slot_types(&function.slot_types)).unwrap();
    }
    text.push_str("]}");
    text.into_bytes()
}

pub(super) const MEMORY_QUEUE_RESOLVE: i32 = -11;
pub(super) const MEMORY_ISLAND_STATUS: i32 = -12;
pub(super) const MEMORY_CLONE_LOOKUP: i32 = -13;
pub(super) const MEMORY_CLONE_PUBLISH: i32 = -14;

/// Validate the generation before projecting a queue capability to its body.
pub(super) fn load_queue_pointer(body: &mut Function, slot: u16) {
    body.instruction(&W::I32Const(MEMORY_QUEUE_RESOLVE));
    load_slot(body, slot);
    body.instruction(&W::I32WrapI64);
    load_slot(body, slot);
    body.instruction(&W::I64Const(32))
        .instruction(&W::I64ShrU)
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(0))
        .instruction(&W::Call(0));
}

// Scheduler metadata has host ownership; managed frames and queue payloads
// retain their precise heap roots throughout waiting and cancellation.
pub(super) const MEMORY_SCHEDULE: i32 = -15;
pub(super) const MEMORY_FIBER_PREVIOUS: i32 = -16;
pub(super) const MEMORY_QUEUE_WAIT: i32 = -17;
pub(super) const MEMORY_QUEUE_NOTIFY: i32 = -18;
pub(super) const MEMORY_FIBER_WAKE: i32 = -19;
pub(super) const MEMORY_FIBER_PARK: i32 = -20;
pub(super) const MEMORY_FIBER_READY: i32 = -21;
pub(super) const MEMORY_FRAME_NEW: i32 = -22;
