//! Island instructions: IslandNew, GoIsland

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::island;
use vo_runtime::objects::closure;
use vo_runtime::slot::Slot;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::sync::mpsc::Receiver;
#[cfg(feature = "std")]
use vo_runtime::island::IslandCommand;

/// Result of creating a new island.
#[cfg(feature = "std")]
pub struct IslandNewResult {
    pub handle: GcRef,
    pub command_tx: std::sync::mpsc::Sender<IslandCommand>,
    pub command_rx: Receiver<IslandCommand>,
}

/// Result of go @(island) - spawn fiber on remote island.
/// 
/// Contains raw capture and argument data. The VM coordinator will pack these
/// with proper type metadata from FunctionDef before sending.
pub struct GoIslandResult {
    pub island: GcRef,
    /// Function ID in the module
    pub func_id: u32,
    /// Raw capture slots (GcRefs to escaped variables)
    pub capture_data: Vec<Slot>,
    pub capture_slots: u16,
    /// Raw argument slots
    pub arg_data: Vec<Slot>,
    pub arg_slots: u16,
}

/// Create a new island.
/// Returns handle, command sender, and command receiver for the VM coordinator.
#[cfg(feature = "std")]
#[inline]
pub fn exec_island_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    next_island_id: u32,
) -> IslandNewResult {
    // Channel managed by VM, not stored in GC object
    let (tx, rx) = std::sync::mpsc::channel::<IslandCommand>();
    let handle = island::create(gc, next_island_id);
    stack_set(stack, bp + inst.a as usize, handle as u64);
    IslandNewResult {
        handle,
        command_tx: tx,
        command_rx: rx,
    }
}

#[cfg(not(feature = "std"))]
#[inline]
pub fn exec_island_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    _next_island_id: u32,
) -> GcRef {
    // Islands not supported in no_std - create dummy main island handle
    let handle = island::create_main(gc);
    stack_set(stack, bp + inst.a as usize, handle as u64);
    handle
}

/// Start a goroutine on a specific island.
/// 
/// GoIsland instruction: a=island, b=closure, c=args_start, flags=arg_slots
/// 
/// Note: Closures themselves are NOT sendable. This function:
/// 1. Extracts func_id from the closure
/// 2. Reads raw capture slots (GcRefs to escaped variables)
/// 3. Reads raw argument slots from stack
/// 4. Returns data for VM coordinator to handle packing with proper type info
pub fn exec_go_island(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
) -> GoIslandResult {
    let island_handle = stack_get(stack, bp + inst.a as usize) as GcRef;
    let closure_ref = stack_get(stack, bp + inst.b as usize) as GcRef;
    let args_start = bp + inst.c as usize;
    let arg_slots = inst.flags as usize;

    // Extract function ID and capture count from closure itself
    let func_id = closure::func_id(closure_ref);
    let capture_count = closure::capture_count(closure_ref);
    
    // Read raw capture slots
    let mut capture_data = Vec::with_capacity(capture_count);
    for i in 0..capture_count {
        capture_data.push(closure::get_capture(closure_ref, i));
    }
    
    // Read raw argument slots
    let mut arg_data = Vec::with_capacity(arg_slots);
    for i in 0..arg_slots {
        arg_data.push(stack_get(stack, args_start + i));
    }

    GoIslandResult {
        island: island_handle,
        func_id,
        capture_data,
        capture_slots: capture_count as u16,
        arg_data,
        arg_slots: arg_slots as u16,
    }
}

/// Pack closure data for cross-island transfer with proper type serialization.
/// Uses type info from FunctionDef to correctly serialize all sendable types.
#[cfg(feature = "std")]
pub fn pack_closure_for_island(
    gc: &vo_runtime::gc::Gc,
    result: &GoIslandResult,
    capture_types: &[(u32, u16)],
    param_types: &[(u32, u16)],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
) -> Vec<u8> {
    vo_runtime::island_msg::encode_spawn_payload(
        gc,
        result.func_id,
        &result.capture_data,
        capture_types,
        &result.arg_data,
        param_types,
        struct_metas,
        runtime_types,
    )
}
