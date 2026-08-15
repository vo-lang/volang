//! GC root scanning for VM.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, format, string::String, vec::Vec};

use vo_runtime::ffi::SentinelErrorCache;
use vo_runtime::gc::{
    GcMode, GcRef, GcRootScanChunk, GcRootScanKind, GcRootState, GcState, MemoryError, MemoryStats,
};
use vo_runtime::slot::SLOT_BYTES;

#[cfg(feature = "jit")]
use vo_runtime::jit_api::{JitContext, JitNativeFrame};

#[cfg(feature = "jit")]
use crate::vm::{JitManager, NativeRootScanCursor, NativeRootScanStats};

use crate::bytecode::{FunctionDef, GlobalDef, LoadedModule};
use crate::fiber::{DeferEntry, Fiber, PanicState};
use crate::scheduler::FiberId;
use crate::vm::{
    EndpointRegistry, Vm, VmFiberRootScanStage, VmGcCycleReport, VmGcStepReport, VmGcStepStats,
    VmRootScanMode, VmRootScanSnapshot, VmRootScanStage,
};

fn new_vm_root_scan_snapshot(
    kind: GcRootScanKind,
    dirty_epoch: u64,
    dirty_all: bool,
) -> VmRootScanSnapshot {
    let mode = if kind == GcRootScanKind::Sweep && !dirty_all {
        VmRootScanMode::DirtyFibers
    } else {
        VmRootScanMode::Full
    };
    VmRootScanSnapshot {
        kind,
        mode,
        dirty_epoch,
        stage: VmRootScanStage::Globals,
        global_def_cursor: 0,
        global_base_cursor: 0,
        global_slot_cursor: 0,
        fiber_source_cursor: 0,
        fiber_frame_cursor: 0,
        fiber_slot_cursor: 0,
        fiber_aux_stage: VmFiberRootScanStage::Defers,
        fiber_aux_outer_cursor: 0,
        fiber_aux_inner_cursor: 0,
        fiber_aux_slot_cursor: 0,
        io_staging_cursor: 0,
        sentinel_cursor: 0,
        endpoint_cursor: 0,
    }
}

#[inline]
fn typed_slot_root(
    slots: &[u64],
    slot_types: &[vo_runtime::SlotType],
    idx: usize,
) -> Option<GcRef> {
    let raw = *slots.get(idx)?;
    match slot_types.get(idx).copied()? {
        vo_runtime::SlotType::GcRef if raw != 0 => Some(raw as GcRef),
        vo_runtime::SlotType::Interface1
            if raw != 0
                && idx > 0
                && vo_runtime::objects::interface::data_is_gc_ref(slots[idx - 1]) =>
        {
            Some(raw as GcRef)
        }
        _ => None,
    }
}

fn selected_fiber_index(
    snapshot: &VmRootScanSnapshot,
    dirty_fibers: &[u32],
    fibers_len: usize,
    active_fiber_id: Option<u32>,
) -> Option<usize> {
    match snapshot.mode {
        VmRootScanMode::Full => {
            let source_count = fibers_len + usize::from(active_fiber_id.is_some());
            (snapshot.fiber_source_cursor < source_count).then_some(snapshot.fiber_source_cursor)
        }
        VmRootScanMode::DirtyFibers => {
            dirty_fibers
                .get(snapshot.fiber_source_cursor)
                .copied()
                .map(|raw| {
                    if active_fiber_id == Some(raw) {
                        fibers_len
                    } else {
                        raw as usize
                    }
                })
        }
    }
}

enum AuxRootScanStep {
    Consumed(Option<GcRef>),
    BudgetExhausted,
    Done,
}

#[derive(Clone, Copy)]
enum VmRootSource {
    Global {
        definition: usize,
        slot: usize,
    },
    FiberFrame {
        fiber: u32,
        frame: usize,
        func_id: u32,
        pc: usize,
        slot: usize,
    },
    FiberAux {
        fiber: u32,
        stage: VmFiberRootScanStage,
        outer: usize,
        inner: usize,
        slot: usize,
    },
    IoStaging(usize),
    Sentinel(usize),
    Endpoint(usize),
}

impl core::fmt::Display for VmRootSource {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::Global { definition, slot } => {
                write!(formatter, "global definition {definition} slot {slot}")
            }
            Self::FiberFrame {
                fiber,
                frame,
                func_id,
                pc,
                slot,
            } => write!(
                formatter,
                "fiber {fiber} frame {frame} function {func_id} pc {pc} slot {slot}"
            ),
            Self::FiberAux {
                fiber,
                stage,
                outer,
                inner,
                slot,
            } => write!(
                formatter,
                "fiber {fiber} auxiliary {stage:?} indices {outer}/{inner}/{slot}"
            ),
            Self::IoStaging(slot) => write!(formatter, "I/O staging slot {slot}"),
            Self::Sentinel(slot) => write!(formatter, "sentinel slot {slot}"),
            Self::Endpoint(slot) => write!(formatter, "endpoint slot {slot}"),
        }
    }
}

fn interface_value_root(value: vo_runtime::InterfaceSlot) -> Option<GcRef> {
    (value.is_ref_type() && value.slot1 != 0).then_some(value.as_ref())
}

fn defer_entry_root_at(entry: &DeferEntry, cursor: usize) -> Option<Option<GcRef>> {
    match cursor {
        0 => Some((!entry.closure.is_null()).then_some(entry.closure)),
        1 => Some((!entry.args.is_null()).then_some(entry.args)),
        _ if entry.args.is_null() => None,
        _ => {
            let slot = cursor - 2;
            let arg_slots = entry.arg_layout.slot_types.len();
            if slot == 0 {
                assert!(
                    arg_slots <= unsafe { vo_runtime::gc::Gc::header(entry.args) }.slots as usize,
                    "defer root layout exceeds argument object: layout_slots={} object_slots={}",
                    arg_slots,
                    unsafe { vo_runtime::gc::Gc::header(entry.args) }.slots
                );
            }
            if slot >= arg_slots {
                return None;
            }
            let args = unsafe { core::slice::from_raw_parts(entry.args as *const u64, arg_slots) };
            Some(typed_slot_root(args, &entry.arg_layout.slot_types, slot))
        }
    }
}

fn reset_fiber_aux_stage(snapshot: &mut VmRootScanSnapshot, stage: VmFiberRootScanStage) {
    snapshot.fiber_aux_stage = stage;
    snapshot.fiber_aux_outer_cursor = 0;
    snapshot.fiber_aux_inner_cursor = 0;
    snapshot.fiber_aux_slot_cursor = 0;
}

fn scan_fiber_aux_root(
    snapshot: &mut VmRootScanSnapshot,
    fiber: &Fiber,
    budget_available: bool,
) -> AuxRootScanStep {
    loop {
        match snapshot.fiber_aux_stage {
            VmFiberRootScanStage::Defers => {
                let Some(entry) = fiber.defer_stack.get(snapshot.fiber_aux_outer_cursor) else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::UnwindDefers);
                    continue;
                };
                if let Some(root) = defer_entry_root_at(entry, snapshot.fiber_aux_slot_cursor) {
                    if !budget_available {
                        return AuxRootScanStep::BudgetExhausted;
                    }
                    snapshot.fiber_aux_slot_cursor += 1;
                    return AuxRootScanStep::Consumed(root);
                }
                snapshot.fiber_aux_outer_cursor += 1;
                snapshot.fiber_aux_slot_cursor = 0;
            }
            VmFiberRootScanStage::UnwindDefers => {
                let Some(state) = fiber.unwinding.get(snapshot.fiber_aux_outer_cursor) else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::ReturnValues);
                    continue;
                };
                let Some(entry) = state.pending.get(snapshot.fiber_aux_inner_cursor) else {
                    if !budget_available {
                        return AuxRootScanStep::BudgetExhausted;
                    }
                    snapshot.fiber_aux_outer_cursor += 1;
                    snapshot.fiber_aux_inner_cursor = 0;
                    snapshot.fiber_aux_slot_cursor = 0;
                    return AuxRootScanStep::Consumed(None);
                };
                if let Some(root) = defer_entry_root_at(entry, snapshot.fiber_aux_slot_cursor) {
                    if !budget_available {
                        return AuxRootScanStep::BudgetExhausted;
                    }
                    snapshot.fiber_aux_slot_cursor += 1;
                    return AuxRootScanStep::Consumed(root);
                }
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                snapshot.fiber_aux_inner_cursor += 1;
                snapshot.fiber_aux_slot_cursor = 0;
                return AuxRootScanStep::Consumed(None);
            }
            VmFiberRootScanStage::ReturnValues => {
                let Some(state) = fiber.unwinding.get(snapshot.fiber_aux_outer_cursor) else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::UnwindPanics);
                    continue;
                };
                let Some(values) = state.return_values.as_ref() else {
                    if !budget_available {
                        return AuxRootScanStep::BudgetExhausted;
                    }
                    snapshot.fiber_aux_outer_cursor += 1;
                    snapshot.fiber_aux_slot_cursor = 0;
                    return AuxRootScanStep::Consumed(None);
                };
                let root = match values {
                    crate::fiber::ReturnValues::Stack { vals, slot_types } => {
                        if snapshot.fiber_aux_slot_cursor == 0 {
                            assert_eq!(
                                vals.len(),
                                slot_types.len(),
                                "unwinding return root layout mismatch: fiber={} unwind={} values={} slot_types={}",
                                fiber.id,
                                snapshot.fiber_aux_outer_cursor,
                                vals.len(),
                                slot_types.len()
                            );
                        }
                        if snapshot.fiber_aux_slot_cursor >= vals.len() {
                            if !budget_available {
                                return AuxRootScanStep::BudgetExhausted;
                            }
                            snapshot.fiber_aux_outer_cursor += 1;
                            snapshot.fiber_aux_slot_cursor = 0;
                            return AuxRootScanStep::Consumed(None);
                        }
                        typed_slot_root(vals, slot_types, snapshot.fiber_aux_slot_cursor)
                    }
                    crate::fiber::ReturnValues::Heap { gcrefs, .. } => {
                        let Some(&raw) = gcrefs.get(snapshot.fiber_aux_slot_cursor) else {
                            if !budget_available {
                                return AuxRootScanStep::BudgetExhausted;
                            }
                            snapshot.fiber_aux_outer_cursor += 1;
                            snapshot.fiber_aux_slot_cursor = 0;
                            return AuxRootScanStep::Consumed(None);
                        };
                        (raw != 0).then_some(raw as GcRef)
                    }
                };
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                snapshot.fiber_aux_slot_cursor += 1;
                return AuxRootScanStep::Consumed(root);
            }
            VmFiberRootScanStage::UnwindPanics => {
                let Some(state) = fiber.unwinding.get(snapshot.fiber_aux_outer_cursor) else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Panic);
                    continue;
                };
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                snapshot.fiber_aux_outer_cursor += 1;
                let root = state.panic_context.and_then(|context| match context.state {
                    PanicState::Recoverable(value) => interface_value_root(value),
                    PanicState::Fatal => None,
                });
                return AuxRootScanStep::Consumed(root);
            }
            VmFiberRootScanStage::Panic => {
                let Some(PanicState::Recoverable(value)) = fiber.panic_state else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::ClosureReplay);
                    continue;
                };
                if snapshot.fiber_aux_slot_cursor > 0 {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::ClosureReplay);
                    continue;
                }
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                snapshot.fiber_aux_slot_cursor = 1;
                return AuxRootScanStep::Consumed(interface_value_root(value));
            }
            VmFiberRootScanStage::ClosureReplay => {
                let Some((values, slot_types)) = fiber
                    .closure_replay
                    .results
                    .get(snapshot.fiber_aux_outer_cursor)
                else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::JitSuspend);
                    continue;
                };
                if snapshot.fiber_aux_slot_cursor == 0 {
                    assert_eq!(
                        values.len(),
                        slot_types.len(),
                        "closure replay root layout mismatch: fiber={} result={} values={} slot_types={}",
                        fiber.id,
                        snapshot.fiber_aux_outer_cursor,
                        values.len(),
                        slot_types.len()
                    );
                }
                if snapshot.fiber_aux_slot_cursor >= values.len() {
                    if !budget_available {
                        return AuxRootScanStep::BudgetExhausted;
                    }
                    snapshot.fiber_aux_outer_cursor += 1;
                    snapshot.fiber_aux_slot_cursor = 0;
                    return AuxRootScanStep::Consumed(None);
                }
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                let root = typed_slot_root(values, slot_types, snapshot.fiber_aux_slot_cursor);
                snapshot.fiber_aux_slot_cursor += 1;
                return AuxRootScanStep::Consumed(root);
            }
            VmFiberRootScanStage::JitSuspend => {
                #[cfg(feature = "jit")]
                if let Some(crate::fiber::JitExternSuspend::CallClosure {
                    closure_ref, args, ..
                }) = &fiber.jit_extern_suspend
                {
                    if snapshot.fiber_aux_slot_cursor == 0 {
                        assert_eq!(
                            args.values.len(),
                            args.slot_types.len(),
                            "JIT suspend root layout mismatch: fiber={} values={} slot_types={}",
                            fiber.id,
                            args.values.len(),
                            args.slot_types.len()
                        );
                    }
                    let root = match snapshot.fiber_aux_slot_cursor {
                        0 => Some((!closure_ref.is_null()).then_some(*closure_ref)),
                        cursor => {
                            let slot = cursor - 1;
                            (slot < args.values.len())
                                .then(|| typed_slot_root(&args.values, &args.slot_types, slot))
                        }
                    };
                    if let Some(root) = root {
                        if !budget_available {
                            return AuxRootScanStep::BudgetExhausted;
                        }
                        snapshot.fiber_aux_slot_cursor += 1;
                        return AuxRootScanStep::Consumed(root);
                    }
                }
                reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::SelectQueues);
            }
            VmFiberRootScanStage::SelectQueues => {
                let Some(queue) = fiber
                    .select_state
                    .as_ref()
                    .and_then(|state| state.registered_queues.get(snapshot.fiber_aux_outer_cursor))
                else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::SelectResult);
                    continue;
                };
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                snapshot.fiber_aux_outer_cursor += 1;
                return AuxRootScanStep::Consumed((!queue.queue.is_null()).then_some(queue.queue));
            }
            VmFiberRootScanStage::SelectResult => {
                let Some(crate::fiber::SelectWokenResult::Recv {
                    data, slot_types, ..
                }) = fiber
                    .select_state
                    .as_ref()
                    .and_then(|state| state.woken_result.as_ref())
                else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::QueueWait);
                    continue;
                };
                if snapshot.fiber_aux_slot_cursor == 0 {
                    assert_eq!(
                        data.len(),
                        slot_types.len(),
                        "select result root layout mismatch: fiber={} values={} slot_types={}",
                        fiber.id,
                        data.len(),
                        slot_types.len()
                    );
                }
                if snapshot.fiber_aux_slot_cursor >= data.len() {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::QueueWait);
                    continue;
                }
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                let root = typed_slot_root(data, slot_types, snapshot.fiber_aux_slot_cursor);
                snapshot.fiber_aux_slot_cursor += 1;
                return AuxRootScanStep::Consumed(root);
            }
            VmFiberRootScanStage::QueueWait => {
                let Some(state) = fiber.queue_wait_state else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::JitPanic);
                    continue;
                };
                if snapshot.fiber_aux_slot_cursor > 0 {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::JitPanic);
                    continue;
                }
                if !budget_available {
                    return AuxRootScanStep::BudgetExhausted;
                }
                snapshot.fiber_aux_slot_cursor = 1;
                return AuxRootScanStep::Consumed(
                    (!state.queue_ref.is_null()).then_some(state.queue_ref),
                );
            }
            VmFiberRootScanStage::JitPanic => {
                #[cfg(feature = "jit")]
                if fiber.jit_panic_flag && snapshot.fiber_aux_slot_cursor == 0 {
                    if !budget_available {
                        return AuxRootScanStep::BudgetExhausted;
                    }
                    snapshot.fiber_aux_slot_cursor = 1;
                    return AuxRootScanStep::Consumed(interface_value_root(fiber.jit_panic_msg));
                }
                reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Done);
            }
            VmFiberRootScanStage::Done => return AuxRootScanStep::Done,
        }
    }
}

#[derive(Debug)]
struct VmRootScanCompletion {
    mode: VmRootScanMode,
    dirty_epoch: u64,
}

impl From<&VmRootScanSnapshot> for VmRootScanCompletion {
    fn from(snapshot: &VmRootScanSnapshot) -> Self {
        Self {
            mode: snapshot.mode,
            dirty_epoch: snapshot.dirty_epoch,
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn scan_vm_root_snapshot_chunk<F>(
    root_scan: &mut Option<VmRootScanSnapshot>,
    kind: GcRootScanKind,
    limit_bytes: usize,
    dirty_epoch: u64,
    dirty_all: bool,
    dirty_fibers: &[u32],
    globals: &[u64],
    global_defs: &[GlobalDef],
    fibers: &[Box<Fiber>],
    active_fiber: Option<(&Fiber, usize)>,
    functions: &[FunctionDef],
    io_staging_roots: &[Option<GcRef>],
    sentinel_errors: &SentinelErrorCache,
    endpoint_registry: &EndpointRegistry,
    completion: &mut Option<VmRootScanCompletion>,
    mut visit_root: F,
) -> GcRootScanChunk
where
    F: FnMut(GcRef, VmRootSource),
{
    let limit_bytes = limit_bytes.max(SLOT_BYTES);
    let mut work = 0usize;

    loop {
        let needs_new_snapshot = root_scan
            .as_ref()
            .map(|snapshot| snapshot.kind != kind)
            .unwrap_or(true);
        if needs_new_snapshot {
            *root_scan = Some(new_vm_root_scan_snapshot(kind, dirty_epoch, dirty_all));
        }

        let snapshot = root_scan.as_mut().expect("root snapshot initialized");
        if snapshot.dirty_epoch != dirty_epoch {
            *root_scan = None;
            if work >= limit_bytes {
                return GcRootScanChunk::pending(work);
            }
            continue;
        }

        loop {
            match snapshot.stage {
                VmRootScanStage::Globals => {
                    let Some(def) = global_defs.get(snapshot.global_def_cursor) else {
                        snapshot.stage = VmRootScanStage::Fibers;
                        continue;
                    };
                    let slots = def.slots as usize;
                    if snapshot.global_slot_cursor == 0 {
                        assert_eq!(
                            def.slot_types.len(),
                            slots,
                            "global root layout mismatch at definition {}: declared_slots={} slot_types={}",
                            snapshot.global_def_cursor,
                            slots,
                            def.slot_types.len()
                        );
                        assert!(
                            snapshot.global_base_cursor.saturating_add(slots) <= globals.len(),
                            "global root storage mismatch at definition {}: range={}..{} globals={}",
                            snapshot.global_def_cursor,
                            snapshot.global_base_cursor,
                            snapshot.global_base_cursor.saturating_add(slots),
                            globals.len()
                        );
                    }
                    if snapshot.global_slot_cursor >= slots {
                        if slots == 0 {
                            if work >= limit_bytes {
                                return GcRootScanChunk::pending(work);
                            }
                            work += SLOT_BYTES;
                        }
                        snapshot.global_base_cursor =
                            snapshot.global_base_cursor.saturating_add(slots);
                        snapshot.global_def_cursor += 1;
                        snapshot.global_slot_cursor = 0;
                        continue;
                    }
                    let start = snapshot.global_base_cursor;
                    let end = start + slots;
                    let global_slots = &globals[start..end];
                    let idx = snapshot.global_slot_cursor;
                    if work >= limit_bytes {
                        return GcRootScanChunk::pending(work);
                    }
                    if let Some(root) = typed_slot_root(global_slots, &def.slot_types, idx) {
                        visit_root(
                            root,
                            VmRootSource::Global {
                                definition: snapshot.global_def_cursor,
                                slot: idx,
                            },
                        );
                    }
                    snapshot.global_slot_cursor += 1;
                    work += SLOT_BYTES;
                }
                VmRootScanStage::Fibers => {
                    let active_fiber_id = active_fiber.map(|(fiber, _)| fiber.id);
                    let Some(fiber_idx) =
                        selected_fiber_index(snapshot, dirty_fibers, fibers.len(), active_fiber_id)
                    else {
                        snapshot.stage = VmRootScanStage::IoStaging;
                        continue;
                    };
                    let (fiber, frame_limit) = if let Some(fiber) = fibers.get(fiber_idx) {
                        (fiber.as_ref(), fiber.frames.len())
                    } else if fiber_idx == fibers.len() {
                        let Some((fiber, frame_limit)) = active_fiber else {
                            if work >= limit_bytes {
                                return GcRootScanChunk::pending(work);
                            }
                            snapshot.fiber_source_cursor += 1;
                            work += SLOT_BYTES;
                            continue;
                        };
                        (fiber, frame_limit.min(fiber.frames.len()))
                    } else {
                        if work >= limit_bytes {
                            return GcRootScanChunk::pending(work);
                        }
                        snapshot.fiber_source_cursor += 1;
                        snapshot.fiber_frame_cursor = 0;
                        snapshot.fiber_slot_cursor = 0;
                        reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Defers);
                        work += SLOT_BYTES;
                        continue;
                    };
                    if fiber.state.is_dead() {
                        if work >= limit_bytes {
                            return GcRootScanChunk::pending(work);
                        }
                        snapshot.fiber_source_cursor += 1;
                        snapshot.fiber_frame_cursor = 0;
                        snapshot.fiber_slot_cursor = 0;
                        reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Defers);
                        work += SLOT_BYTES;
                        continue;
                    }
                    if snapshot.fiber_frame_cursor < frame_limit {
                        let frame = fiber
                            .frames
                            .get(snapshot.fiber_frame_cursor)
                            .expect("active fiber frame limit was clamped to frame storage");
                        let func = functions.get(frame.func_id as usize).unwrap_or_else(|| {
                            panic!(
                                "fiber root frame references missing function: fiber={} frame={} func_id={} functions={}",
                                fiber.id,
                                snapshot.fiber_frame_cursor,
                                frame.func_id,
                                functions.len()
                            )
                        });
                        let scan_slots = usize::from(frame.scan_slots);
                        if snapshot.fiber_slot_cursor == 0 {
                            assert!(
                                scan_slots <= func.slot_types.len(),
                                "fiber root layout mismatch: fiber={} func_id={} scan_slots={} slot_types={}",
                                fiber.id,
                                frame.func_id,
                                scan_slots,
                                func.slot_types.len()
                            );
                            assert!(
                                frame.bp.saturating_add(scan_slots) <= fiber.stack.len(),
                                "fiber root stack range mismatch: fiber={} func_id={} range={}..{} stack={}",
                                fiber.id,
                                frame.func_id,
                                frame.bp,
                                frame.bp.saturating_add(scan_slots),
                                fiber.stack.len()
                            );
                        }
                        if snapshot.fiber_slot_cursor < scan_slots {
                            if work >= limit_bytes {
                                return GcRootScanChunk::pending(work);
                            }
                            let idx = snapshot.fiber_slot_cursor;
                            let stack_slots = &fiber.stack[frame.bp..frame.bp + scan_slots];
                            if let Some(root) = typed_slot_root(stack_slots, &func.slot_types, idx)
                            {
                                visit_root(
                                    root,
                                    VmRootSource::FiberFrame {
                                        fiber: fiber.id,
                                        frame: snapshot.fiber_frame_cursor,
                                        func_id: frame.func_id,
                                        pc: frame.pc,
                                        slot: idx,
                                    },
                                );
                            }
                            snapshot.fiber_slot_cursor += 1;
                            work += SLOT_BYTES;
                            continue;
                        }
                        if scan_slots == 0 {
                            if work >= limit_bytes {
                                return GcRootScanChunk::pending(work);
                            }
                            work += SLOT_BYTES;
                        }
                        snapshot.fiber_frame_cursor += 1;
                        snapshot.fiber_slot_cursor = 0;
                        continue;
                    }

                    let aux_source = VmRootSource::FiberAux {
                        fiber: fiber.id,
                        stage: snapshot.fiber_aux_stage,
                        outer: snapshot.fiber_aux_outer_cursor,
                        inner: snapshot.fiber_aux_inner_cursor,
                        slot: snapshot.fiber_aux_slot_cursor,
                    };
                    match scan_fiber_aux_root(snapshot, fiber, work < limit_bytes) {
                        AuxRootScanStep::Consumed(root) => {
                            if let Some(root) = root {
                                visit_root(root, aux_source);
                            }
                            work += SLOT_BYTES;
                        }
                        AuxRootScanStep::BudgetExhausted => {
                            return GcRootScanChunk::pending(work);
                        }
                        AuxRootScanStep::Done => {
                            if work >= limit_bytes {
                                return GcRootScanChunk::pending(work);
                            }
                            snapshot.fiber_source_cursor += 1;
                            snapshot.fiber_frame_cursor = 0;
                            snapshot.fiber_slot_cursor = 0;
                            reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Defers);
                            work += SLOT_BYTES;
                        }
                    }
                }
                VmRootScanStage::IoStaging => {
                    let Some(root) = io_staging_roots.get(snapshot.io_staging_cursor) else {
                        snapshot.stage = VmRootScanStage::SentinelErrors;
                        continue;
                    };
                    if work >= limit_bytes {
                        return GcRootScanChunk::pending(work);
                    }
                    if let Some(root) = *root {
                        if !root.is_null() {
                            visit_root(root, VmRootSource::IoStaging(snapshot.io_staging_cursor));
                        }
                    }
                    snapshot.io_staging_cursor += 1;
                    work += SLOT_BYTES;
                }
                VmRootScanStage::SentinelErrors => {
                    let Some(root) = sentinel_errors.gc_root_at(snapshot.sentinel_cursor) else {
                        snapshot.stage = VmRootScanStage::Endpoints;
                        continue;
                    };
                    if work >= limit_bytes {
                        return GcRootScanChunk::pending(work);
                    }
                    visit_root(root, VmRootSource::Sentinel(snapshot.sentinel_cursor));
                    snapshot.sentinel_cursor += 1;
                    work += SLOT_BYTES;
                }
                VmRootScanStage::Endpoints => {
                    let Some(root) = endpoint_registry.live_handle_at(snapshot.endpoint_cursor)
                    else {
                        snapshot.stage = VmRootScanStage::Done;
                        continue;
                    };
                    if work >= limit_bytes {
                        return GcRootScanChunk::pending(work);
                    }
                    if !root.is_null() {
                        visit_root(root, VmRootSource::Endpoint(snapshot.endpoint_cursor));
                    }
                    snapshot.endpoint_cursor += 1;
                    work += SLOT_BYTES;
                }
                VmRootScanStage::Done => {
                    *completion = Some(VmRootScanCompletion::from(&*snapshot));
                    *root_scan = None;
                    return GcRootScanChunk::complete(work);
                }
            }
        }
    }
}

impl Vm {
    #[cfg(feature = "jit")]
    #[inline]
    fn assert_no_pending_runtime_transitions_for_gc(&self) {
        assert!(
            self.pending_runtime_transitions.is_empty(),
            "pending runtime transitions must be attached or discarded before GC"
        );
    }

    #[cfg(not(feature = "jit"))]
    #[inline]
    fn assert_no_pending_runtime_transitions_for_gc(&self) {}

    /// Enable or disable GC stress mode.
    #[inline]
    pub fn set_gc_stress_every_step(&mut self, enabled: bool) {
        self.state.gc.set_stress_every_step(enabled);
    }

    /// Returns whether GC stress mode is enabled.
    #[inline]
    pub fn gc_stress_every_step(&self) -> bool {
        self.state.gc.stress_every_step()
    }

    /// Enable or disable precise GC verification after VM GC steps.
    #[inline]
    pub fn set_gc_verify_after_step(&mut self, enabled: bool) {
        self.state.gc_verify_after_step = enabled;
    }

    /// Returns whether precise GC verification after VM GC steps is enabled.
    #[inline]
    pub fn gc_verify_after_step(&self) -> bool {
        self.state.gc_verify_after_step
    }

    /// Mark all VM roots dirty. Use for host/I/O/island paths that can mutate
    /// blocked fibers, endpoint roots, or globals without going through the
    /// currently running fiber boundary.
    #[inline]
    pub(crate) fn mark_gc_all_roots_dirty(&mut self) {
        self.state.mark_gc_all_roots_dirty();
    }

    #[inline]
    pub(crate) fn mark_gc_fiber_roots_dirty(&mut self, fiber_id: FiberId) {
        self.state.mark_gc_fiber_roots_dirty(fiber_id.to_raw());
    }

    pub(crate) fn apply_gc_root_effect(
        &mut self,
        effect: crate::vm::GcRootEffect,
        current_fiber: Option<FiberId>,
    ) {
        match effect {
            crate::vm::GcRootEffect::None => {}
            crate::vm::GcRootEffect::CurrentFiberDirty => {
                if let Some(fiber_id) = current_fiber {
                    self.mark_gc_fiber_roots_dirty(fiber_id);
                } else {
                    self.mark_gc_all_roots_dirty();
                }
            }
            crate::vm::GcRootEffect::AllRootsDirty => self.mark_gc_all_roots_dirty(),
        }
    }

    /// Run one incremental GC step when debt starts a cycle or a cycle is active.
    ///
    /// Called at scheduling boundaries (between fiber timeslices).
    /// Uses raw pointer to split the borrow: gc.step() takes &mut Gc,
    /// while the scan_roots callback reads scheduler.fibers, state.globals, etc.
    ///
    /// SAFETY: Called only between fiber runs — no fiber is executing,
    /// so all fiber stacks are stable and safe to scan.
    pub fn gc_step(&mut self) {
        self.mark_gc_all_roots_dirty();
        self.gc_step_after_fiber(None);
    }

    /// Explicitly advance collection by at most `work_units`.
    ///
    /// This starts a cycle even when automatic collection is stopped or the
    /// allocation debt threshold has not been reached.
    pub fn gc_step_units(&mut self, work_units: usize) -> VmGcStepReport {
        if work_units == 0 {
            return VmGcStepReport {
                requested_work_units: 0,
                completed_work_units: 0,
                stats: self.state.last_gc_step_stats,
                memory: self.state.gc.memory_stats(),
            };
        }
        self.state.gc.gc_request_cycle();
        self.gc_step_after_fiber_with_budget(None, Some(work_units), true);
        VmGcStepReport {
            requested_work_units: work_units,
            completed_work_units: self.state.last_gc_step_stats.gc.total_work_bytes / SLOT_BYTES,
            stats: self.state.last_gc_step_stats,
            memory: self.state.gc.memory_stats(),
        }
    }

    #[inline]
    pub fn memory_reserve(&mut self, bytes: usize) -> Result<MemoryStats, MemoryError> {
        self.state.gc.memory_reserve(bytes)
    }

    #[inline]
    pub fn memory_set_growth_allowed(&mut self, allowed: bool) -> Result<(), MemoryError> {
        self.state.gc.memory_set_growth_allowed(allowed)
    }

    #[inline]
    pub fn memory_set_allocation_allowed(&mut self, allowed: bool) {
        self.state.gc.memory_set_allocation_allowed(allowed);
    }

    #[inline]
    pub fn memory_set_hard_limit_bytes(&mut self, limit: Option<usize>) -> Result<(), MemoryError> {
        self.state.gc.memory_set_hard_limit_bytes(limit)
    }

    #[inline]
    pub fn memory_set_external_reported(&mut self, bytes: usize, unknown_provider_count: usize) {
        self.state
            .gc
            .memory_set_external_reported(bytes, unknown_provider_count);
    }

    #[inline]
    pub fn memory_set_wasm_pages(&mut self, current: u64, maximum: Option<u64>) {
        self.state.gc.memory_set_wasm_pages(current, maximum);
    }

    #[inline]
    pub fn memory_stats(&self) -> MemoryStats {
        self.state.gc.memory_stats()
    }

    #[inline]
    pub fn gc_set_mode(&mut self, mode: GcMode) -> Result<(), MemoryError> {
        self.state.gc.gc_set_mode(mode)
    }

    #[inline]
    pub fn gc_stop(&mut self) {
        self.state.gc.gc_stop();
    }

    #[inline]
    pub fn gc_restart(&mut self) {
        self.state.gc.gc_restart();
    }

    /// Complete a major cycle while the VM is at a host-controlled boundary.
    pub fn gc_collect(&mut self) -> Result<VmGcCycleReport, MemoryError> {
        if self.module.is_none() {
            if self.state.gc.object_count() != 0 {
                return Err(MemoryError::CollectorBusy);
            }
            let memory = self.state.gc.memory_stats();
            return Ok(VmGcCycleReport {
                cycle_id: memory.cycle_id,
                cycle_kind: vo_runtime::gc::GcCycleKind::Major,
                steps: 0,
                completed_work_units: 0,
                reclaimed_live_bytes: 0,
                memory,
            });
        }
        let before = self.state.gc.memory_stats();
        self.state.gc.gc_request_major();
        let mut steps = 0usize;
        let mut completed_work_units = 0u64;
        loop {
            let report = self.gc_step_units(128 * 1024);
            steps = steps.saturating_add(1);
            completed_work_units =
                completed_work_units.saturating_add(report.completed_work_units as u64);
            let memory = report.memory;
            if memory.major_cycles > before.major_cycles && memory.gc_state == GcState::Pause {
                return Ok(VmGcCycleReport {
                    cycle_id: memory.cycle_id,
                    cycle_kind: report.stats.gc.cycle_kind,
                    steps,
                    completed_work_units,
                    reclaimed_live_bytes: before
                        .managed_live_bytes
                        .saturating_sub(memory.managed_live_bytes),
                    memory,
                });
            }
        }
    }

    pub(crate) fn service_pending_runtime_mem_requests(&mut self) -> Result<bool, MemoryError> {
        let (collect, work_units) = self.state.runtime_mem_requests.take();
        if collect {
            self.gc_collect()?;
            return Ok(true);
        }
        if work_units > 0 {
            self.gc_step_units(work_units);
            return Ok(true);
        }
        Ok(false)
    }

    /// Telemetry for the most recent VM-triggered incremental GC step.
    #[inline]
    pub fn last_gc_step_stats(&self) -> VmGcStepStats {
        self.state.last_gc_step_stats
    }

    pub(crate) fn gc_step_after_fiber(&mut self, mutated_fiber: Option<FiberId>) {
        self.gc_step_after_fiber_with_budget(mutated_fiber, None, false);
    }

    fn gc_step_after_fiber_with_budget(
        &mut self,
        mutated_fiber: Option<FiberId>,
        work_unit_limit: Option<usize>,
        explicit: bool,
    ) {
        if let Err(error) = self.gc_step_with_root_source(
            mutated_fiber,
            work_unit_limit,
            explicit,
            None,
            |_gc, _kind, _limit| GcRootScanChunk::complete(0),
        ) {
            panic!("invalid VM GC root: {error}");
        }
    }

    /// Run an incremental collector slice while the active JIT frame chain is
    /// paused at an exact stack map. Any root pass started by the collector is
    /// completed before native execution resumes, so callback-local machine
    /// addresses never escape their safepoint lifetime.
    #[cfg(feature = "jit")]
    pub(crate) unsafe fn gc_step_while_native(
        &mut self,
        active_fiber: &Fiber,
        ctx: *mut JitContext,
        native_frame: *mut JitNativeFrame,
    ) -> Result<(), vo_jit::JitError> {
        const MAX_NATIVE_FRAMES_PER_POLL: usize = 256;
        const MAX_NATIVE_ROOTS_PER_POLL: usize = 16 * 1024;

        let manager = self.jit_manager().ok_or_else(|| {
            vo_jit::JitError::Internal("GC safepoint reached without a JIT manager".to_string())
        })?;
        let validation = unsafe {
            manager.visit_native_roots(
                native_frame,
                ctx,
                MAX_NATIVE_FRAMES_PER_POLL,
                MAX_NATIVE_ROOTS_PER_POLL,
                |root: *mut u64| {
                    core::hint::black_box(root.read());
                },
            )
        }?;
        if !validation.complete {
            return Err(vo_jit::JitError::Internal(
                "native root map exceeds the GC safepoint scan budget".to_string(),
            ));
        }

        let manager_ptr = manager as *const JitManager;
        let active_frame_limit = active_fiber.frames.len().saturating_sub(1);
        self.state.mark_gc_all_roots_dirty();

        let mut cursor_kind = None;
        let mut cursor = NativeRootScanCursor::new(native_frame, ctx);
        let mut scan_error = None;
        loop {
            self.gc_step_with_root_source(
                None,
                None,
                true,
                Some((active_fiber, active_frame_limit)),
                |gc, kind, limit| {
                    if cursor_kind != Some(kind) {
                        cursor_kind = Some(kind);
                        cursor = NativeRootScanCursor::new(native_frame, ctx);
                    }
                    if scan_error.is_some() {
                        return GcRootScanChunk::pending(SLOT_BYTES.min(limit));
                    }
                    let manager = unsafe { &*manager_ptr };
                    let mut invalid_root = None;
                    match unsafe {
                        manager.visit_native_roots_chunk(
                            &mut cursor,
                            MAX_NATIVE_FRAMES_PER_POLL,
                            MAX_NATIVE_ROOTS_PER_POLL,
                            limit / SLOT_BYTES,
                            |root: *mut u64| {
                                let raw = root.read();
                                if raw == 0 || invalid_root.is_some() {
                                    return;
                                }
                                if gc.try_mark_gray(raw as GcRef).is_err() {
                                    invalid_root = Some(vo_jit::JitError::Internal(format!(
                                        "native stack map exposed invalid GC root {raw:#x}"
                                    )));
                                }
                            },
                        )
                    } {
                        Ok(chunk) => {
                            if let Some(error) = invalid_root {
                                scan_error = Some(error);
                                GcRootScanChunk::pending(SLOT_BYTES.min(limit))
                            } else {
                                GcRootScanChunk {
                                    done: chunk.done,
                                    work_bytes: chunk.work_slots.saturating_mul(SLOT_BYTES),
                                }
                            }
                        }
                        Err(error) => {
                            scan_error = Some(error);
                            GcRootScanChunk::pending(SLOT_BYTES.min(limit))
                        }
                    }
                },
            )
            .map_err(vo_jit::JitError::Internal)?;
            if let Some(error) = scan_error.take() {
                return Err(error);
            }
            if !self.state.gc.root_scan_pending() && self.state.gc_root_scan.is_none() {
                break;
            }
        }

        if self.state.gc_verify_after_step {
            let verify_root_colors = self.state.gc_root_colors_are_verifiable();
            let mut root_error = None;
            let manager = self
                .jit_manager()
                .expect("validated JIT callback must retain its manager");
            unsafe {
                manager.visit_native_roots(
                    native_frame,
                    ctx,
                    MAX_NATIVE_FRAMES_PER_POLL,
                    MAX_NATIVE_ROOTS_PER_POLL,
                    |slot: *mut u64| {
                        if root_error.is_some() {
                            return;
                        }
                        let root = slot.read() as GcRef;
                        if root.is_null() {
                            return;
                        }
                        let Some(root) = self.state.gc.canonicalize_ref(root) else {
                            root_error =
                                Some("native root does not reference a live GC object".to_string());
                            return;
                        };
                        let dangling_white = verify_root_colors
                            && match self.state.gc.state() {
                                GcState::Pause | GcState::Reclaim => false,
                                GcState::Sweep => self.state.gc.is_dead_white(root),
                                _ => self.state.gc.is_white(root),
                            };
                        if dangling_white {
                            root_error = Some(format!(
                                "native root references an unreachable white object during {:?}",
                                self.state.gc.state()
                            ));
                        }
                    },
                )
            }?;
            if let Some(error) = root_error {
                panic!("GC verification failed: {error}");
            }
            let loaded_module = self
                .module
                .as_deref()
                .expect("native GC safepoint requires a loaded module");
            if let Err(error) = self.verify_precise_gc_after_step(
                loaded_module,
                Some((active_fiber, active_frame_limit)),
            ) {
                panic!("GC verification failed: {error}");
            }
        }

        self.jit_manager_mut()
            .expect("validated JIT callback must retain its manager")
            .record_native_root_scan(NativeRootScanStats {
                complete: true,
                ..validation
            });
        Ok(())
    }

    fn gc_step_with_root_source<F>(
        &mut self,
        mutated_fiber: Option<FiberId>,
        work_unit_limit: Option<usize>,
        explicit: bool,
        active_fiber: Option<(&Fiber, usize)>,
        mut scan_extra_roots: F,
    ) -> Result<(), String>
    where
        F: FnMut(&mut vo_runtime::gc::Gc, GcRootScanKind, usize) -> GcRootScanChunk,
    {
        self.assert_no_pending_runtime_transitions_for_gc();
        if !explicit && !self.state.gc.should_step() {
            return Ok(());
        }
        if let Some(fiber_id) = mutated_fiber {
            self.mark_gc_fiber_roots_dirty(fiber_id);
        }
        let module = match &self.module {
            Some(module) => module.as_ref() as *const LoadedModule,
            None => return Ok(()),
        };
        // SAFETY: Split borrow via raw pointer. gc is exclusively accessed by step(),
        // while scan_roots/scan_object/finalize read other fields (globals, fibers, etc).
        // No aliasing because gc is a distinct field from globals/fibers/sentinel_errors.
        let gc_ptr = &mut self.state.gc as *mut vo_runtime::gc::Gc;
        let root_scan_ptr = &mut self.state.gc_root_scan as *mut Option<VmRootScanSnapshot>;
        let globals = &self.state.globals;
        #[cfg(feature = "std")]
        let io_staging_roots = self.state.io.staged_gc_root_slots();
        #[cfg(not(feature = "std"))]
        let io_staging_roots: &[Option<GcRef>] = &[];
        let sentinel_errors = &self.state.sentinel_errors;
        let fibers = &self.scheduler.fibers;
        let loaded_module = unsafe { &*module };
        let module_ref = loaded_module.module();

        let endpoint_registry = &self.state.endpoint_registry;
        let gc_state_before = unsafe { &*gc_ptr }.state();
        let dirty_all = self.state.gc_roots_dirty_all;
        let dirty_epoch = self.state.gc_dirty_epoch;
        let dirty_fiber_count = self.state.gc_dirty_fibers.len();
        let dirty_fibers_ptr = &self.state.gc_dirty_fibers as *const Vec<u32>;
        let root_state = if active_fiber.is_none()
            && gc_state_before == GcState::Sweep
            && !dirty_all
            && dirty_fiber_count == 0
        {
            GcRootState::StableSinceLastScan
        } else {
            GcRootState::MayHaveChanged
        };
        let mut full_roots_scanned = false;
        let mut dirty_roots_scanned = false;
        let mut completed_root_scan: Option<VmRootScanCompletion> = None;
        let mut invalid_vm_root = None;
        let func_closure_scan_layout =
            |func_id: u32| -> vo_runtime::gc_types::ClosureScanLayout<'_> {
                let func = module_ref
                    .functions
                    .get(func_id as usize)
                    .unwrap_or_else(|| {
                        panic!(
                            "closure GC metadata missing: func_id={} functions_len={}",
                            func_id,
                            module_ref.functions.len()
                        )
                    });
                let recv_slots = func.recv_slots as usize;
                let runtime_capture_slot_types = if func.capture_slot_types.is_empty()
                    && recv_slots > 0
                {
                    func.slot_types.get(..recv_slots).unwrap_or_else(|| {
                            panic!(
                                "closure receiver slot metadata missing: func_id={} name={} slot range 0..{} actual slot_types={}",
                                func_id,
                                func.name,
                                recv_slots,
                                func.slot_types.len()
                            )
                        })
                } else {
                    &[]
                };
                vo_runtime::gc_types::ClosureScanLayout::new(
                    func.capture_slot_types.as_slice(),
                    runtime_capture_slot_types,
                )
            };

        unsafe {
            (&mut *gc_ptr).step_with_scanners_budget(
                root_state,
                work_unit_limit.unwrap_or(usize::MAX / SLOT_BYTES),
                |gc, kind, limit| {
                    let extra = scan_extra_roots(gc, kind, limit);
                    if !extra.done {
                        return extra;
                    }
                    if extra.work_bytes >= limit {
                        return GcRootScanChunk::pending(extra.work_bytes);
                    }
                    let roots = scan_vm_root_snapshot_chunk(
                        &mut *root_scan_ptr,
                        kind,
                        limit.saturating_sub(extra.work_bytes),
                        dirty_epoch,
                        dirty_all,
                        &*dirty_fibers_ptr,
                        globals,
                        &module_ref.globals,
                        fibers,
                        active_fiber,
                        &module_ref.functions,
                        io_staging_roots,
                        sentinel_errors,
                        endpoint_registry,
                        &mut completed_root_scan,
                        |root, source| {
                            if invalid_vm_root.is_some() {
                                return;
                            }
                            if gc.try_mark_gray(root).is_err() {
                                invalid_vm_root = Some(format!(
                                    "{source} exposed invalid GC root {:#x}",
                                    root as usize
                                ));
                            }
                        },
                    );
                    GcRootScanChunk {
                        done: roots.done,
                        work_bytes: extra.work_bytes.saturating_add(roots.work_bytes),
                    }
                },
                |gc, obj, cursor, limit| {
                    vo_runtime::gc_types::scan_object_chunk_with_context(
                        gc,
                        obj,
                        vo_runtime::gc_types::GcScanContext::from_loaded_module(loaded_module),
                        &func_closure_scan_layout,
                        cursor,
                        limit,
                    )
                },
                |obj| {
                    vo_runtime::gc_types::finalize_object(obj);
                },
            )
        };

        if let Some(error) = invalid_vm_root {
            self.state.gc_root_scan = None;
            return Err(error);
        }

        if let Some(completion) = &completed_root_scan {
            match completion.mode {
                VmRootScanMode::Full => full_roots_scanned = true,
                VmRootScanMode::DirtyFibers => dirty_roots_scanned = true,
            }
        }

        let gc_stats = self.state.gc.last_step_stats();
        self.state.last_gc_step_stats = VmGcStepStats {
            gc: gc_stats,
            dirty_all_before: dirty_all,
            dirty_fiber_count,
            full_roots_scanned,
            dirty_roots_scanned,
            stable_roots_skipped: root_state == GcRootState::StableSinceLastScan
                && gc_stats.root_scan_skips > 0,
        };

        if let Some(completion) = completed_root_scan {
            match completion.mode {
                VmRootScanMode::Full => {
                    if completion.dirty_epoch == self.state.gc_dirty_epoch {
                        self.state.gc_roots_dirty_all = false;
                        self.state.clear_gc_dirty_fibers();
                    }
                }
                VmRootScanMode::DirtyFibers => {
                    if completion.dirty_epoch == self.state.gc_dirty_epoch {
                        self.state.clear_gc_dirty_fibers();
                    }
                }
            }
        }

        if self.state.gc_verify_after_step && active_fiber.is_none() {
            if let Err(err) = self.verify_precise_gc_after_step(loaded_module, None) {
                panic!("GC verification failed: {err}");
            }
        }
        Ok(())
    }

    fn verify_precise_gc_after_step(
        &self,
        loaded_module: &LoadedModule,
        active_fiber: Option<(&Fiber, usize)>,
    ) -> Result<(), String> {
        let module = loaded_module.module();
        #[cfg(feature = "std")]
        let io_staging_roots = self.state.io.staged_gc_root_slots();
        #[cfg(not(feature = "std"))]
        let io_staging_roots: &[Option<GcRef>] = &[];
        let verify_root_colors = self.state.gc_root_colors_are_verifiable();
        let mut root_scan = None;
        let mut completion = None;
        let mut root_error = None;
        loop {
            let chunk = scan_vm_root_snapshot_chunk(
                &mut root_scan,
                GcRootScanKind::Atomic,
                usize::MAX,
                self.state.gc_dirty_epoch,
                true,
                &[],
                &self.state.globals,
                &module.globals,
                &self.scheduler.fibers,
                active_fiber,
                &module.functions,
                io_staging_roots,
                &self.state.sentinel_errors,
                &self.state.endpoint_registry,
                &mut completion,
                |root, source| {
                    if root_error.is_some() || root.is_null() {
                        return;
                    }
                    let Some(canonical_root) = self.state.gc.canonicalize_ref(root) else {
                        root_error = Some(format!(
                            "root {root:?} from {source} does not reference a live GC object"
                        ));
                        return;
                    };
                    let dangling_white = verify_root_colors
                        && match self.state.gc.state() {
                            GcState::Pause | GcState::Reclaim => false,
                            GcState::Sweep => self.state.gc.is_dead_white(canonical_root),
                            _ => self.state.gc.is_white(canonical_root),
                        };
                    if dangling_white {
                        root_error = Some(format!(
                            "root {root:?} from {source} references unreachable white object {canonical_root:?} during {:?}",
                            self.state.gc.state(),
                        ));
                    }
                },
            );
            if chunk.done {
                break;
            }
        }
        if let Some(err) = root_error {
            return Err(err);
        }

        let func_closure_scan_layout =
            |func_id: u32| -> vo_runtime::gc_types::ClosureScanLayout<'_> {
                let func = module.functions.get(func_id as usize).unwrap_or_else(|| {
                    panic!(
                        "closure GC metadata missing: func_id={} functions_len={}",
                        func_id,
                        module.functions.len()
                    )
                });
                let recv_slots = func.recv_slots as usize;
                let runtime_capture_slot_types = if func.capture_slot_types.is_empty()
                    && recv_slots > 0
                {
                    func.slot_types.get(..recv_slots).unwrap_or_else(|| {
                        panic!(
                            "closure receiver slot metadata missing: func_id={} name={} slot range 0..{} actual slot_types={}",
                            func_id,
                            func.name,
                            recv_slots,
                            func.slot_types.len()
                        )
                    })
                } else {
                    &[]
                };
                vo_runtime::gc_types::ClosureScanLayout::new(
                    func.capture_slot_types.as_slice(),
                    runtime_capture_slot_types,
                )
            };

        for parent in self.state.gc.objects() {
            if !self.state.gc.is_black(parent) {
                continue;
            }

            let mut violation: Option<String> = None;
            // Safety: `parent` comes from the collector's live object table.
            unsafe {
                vo_runtime::gc_types::trace_object_children_with_context(
                    parent,
                    vo_runtime::gc_types::GcScanContext::from_loaded_module(loaded_module),
                    &func_closure_scan_layout,
                    |child| {
                        if violation.is_some() || child.is_null() {
                            return;
                        }
                        let Some(child) = self.state.gc.canonicalize_ref(child) else {
                            violation = Some(format!(
                                "black object {:?} references non-live child {:?}",
                                parent, child
                            ));
                            return;
                        };
                        let dangling_white = if self.state.gc.state() == GcState::Sweep {
                            self.state.gc.is_dead_white(child)
                        } else {
                            self.state.gc.is_white(child)
                        };
                        if dangling_white {
                            violation = Some(format!(
                            "black object {:?} references unreachable white child {:?} during {:?}",
                            parent,
                            child,
                            self.state.gc.state()
                        ));
                        }
                    },
                )
            };
            if let Some(err) = violation {
                return Err(err);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::{DeferArgLayout, UnwindingMode, UnwindingState};

    fn pending_defer(frame_depth: usize, func_id: u32) -> DeferEntry {
        DeferEntry {
            frame_depth,
            func_id,
            closure: core::ptr::null_mut(),
            args: core::ptr::null_mut(),
            arg_layout: DeferArgLayout {
                slot_types: Vec::new(),
            },
            is_closure: false,
            is_errdefer: false,
            registered_at_generation: 0,
        }
    }

    fn unwind_state(target_depth: usize, pending: Vec<DeferEntry>) -> UnwindingState {
        UnwindingState {
            pending,
            target_depth,
            mode: UnwindingMode::Return,
            current_defer_generation: 0,
            panic_context: None,
            return_values: None,
            return_func_id: 0,
            return_pc: 0,
            caller_ret_reg: 0,
            caller_ret_count: 0,
            resume_parent_after_recovery: false,
            is_closure_replay: false,
        }
    }

    fn unwind_snapshot(stage: VmFiberRootScanStage) -> VmRootScanSnapshot {
        let mut snapshot = new_vm_root_scan_snapshot(GcRootScanKind::Atomic, 0, true);
        reset_fiber_aux_stage(&mut snapshot, stage);
        snapshot
    }

    #[test]
    fn unwind_defer_scan_keeps_constant_time_state_and_entry_cursors() {
        const STATES: usize = 64;
        const ENTRIES: usize = 32;

        let mut fiber = Fiber::new(0);
        for state_index in 0..STATES {
            let pending = (0..ENTRIES)
                .map(|entry_index| {
                    pending_defer(state_index, (state_index * ENTRIES + entry_index) as u32)
                })
                .collect();
            fiber.unwinding.push(unwind_state(state_index, pending));
        }
        let mut snapshot = unwind_snapshot(VmFiberRootScanStage::UnwindDefers);

        for state_index in 0..STATES {
            for entry_index in 0..ENTRIES {
                for slot_cursor in 0..2 {
                    assert!(matches!(
                        scan_fiber_aux_root(&mut snapshot, &fiber, true),
                        AuxRootScanStep::Consumed(None)
                    ));
                    assert_eq!(snapshot.fiber_aux_outer_cursor, state_index);
                    assert_eq!(snapshot.fiber_aux_inner_cursor, entry_index);
                    assert_eq!(snapshot.fiber_aux_slot_cursor, slot_cursor + 1);
                }

                assert!(matches!(
                    scan_fiber_aux_root(&mut snapshot, &fiber, false),
                    AuxRootScanStep::BudgetExhausted
                ));
                assert_eq!(snapshot.fiber_aux_inner_cursor, entry_index);
                assert!(matches!(
                    scan_fiber_aux_root(&mut snapshot, &fiber, true),
                    AuxRootScanStep::Consumed(None)
                ));
                assert_eq!(snapshot.fiber_aux_inner_cursor, entry_index + 1);
                assert_eq!(snapshot.fiber_aux_slot_cursor, 0);
            }

            assert!(matches!(
                scan_fiber_aux_root(&mut snapshot, &fiber, false),
                AuxRootScanStep::BudgetExhausted
            ));
            assert_eq!(snapshot.fiber_aux_outer_cursor, state_index);
            assert!(matches!(
                scan_fiber_aux_root(&mut snapshot, &fiber, true),
                AuxRootScanStep::Consumed(None)
            ));
            assert_eq!(snapshot.fiber_aux_outer_cursor, state_index + 1);
            assert_eq!(snapshot.fiber_aux_inner_cursor, 0);
        }
    }

    #[test]
    fn empty_unwind_hosts_each_consume_one_bounded_work_unit() {
        const STATES: usize = 1_024;

        let mut fiber = Fiber::new(0);
        for state_index in 0..STATES {
            fiber.unwinding.push(unwind_state(state_index, Vec::new()));
        }

        for stage in [
            VmFiberRootScanStage::UnwindDefers,
            VmFiberRootScanStage::ReturnValues,
            VmFiberRootScanStage::UnwindPanics,
        ] {
            let mut snapshot = unwind_snapshot(stage);
            for state_index in 0..STATES {
                assert!(matches!(
                    scan_fiber_aux_root(&mut snapshot, &fiber, false),
                    AuxRootScanStep::BudgetExhausted
                ));
                assert_eq!(snapshot.fiber_aux_outer_cursor, state_index);
                assert!(matches!(
                    scan_fiber_aux_root(&mut snapshot, &fiber, true),
                    AuxRootScanStep::Consumed(None)
                ));
                assert_eq!(snapshot.fiber_aux_outer_cursor, state_index + 1);
            }
        }
    }
}
