//! GC root scanning for VM.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, format, string::String, vec::Vec};

use vo_runtime::ffi::SentinelErrorCache;
use vo_runtime::gc::{
    trace_slots_by_types, Gc, GcRef, GcRootScanChunk, GcRootScanKind, GcRootState, GcState,
};
use vo_runtime::slot::SLOT_BYTES;

use crate::bytecode::{FunctionDef, GlobalDef, Module};
use crate::fiber::{DeferEntry, Fiber, PanicState};
use crate::scheduler::FiberId;
use crate::vm::{
    EndpointRegistry, Vm, VmFiberRootScanStage, VmGcStepStats, VmRootScanMode, VmRootScanSnapshot,
    VmRootScanStage,
};

#[inline]
fn collect_gcref(roots: &mut Vec<GcRef>, gcref: GcRef) {
    if !gcref.is_null() {
        roots.push(gcref);
    }
}

#[inline]
fn collect_gcrefs(roots: &mut Vec<GcRef>, gcrefs: &[u64]) {
    for &raw in gcrefs {
        collect_gcref(roots, raw as GcRef);
    }
}

#[inline]
fn collect_slots_by_types(
    roots: &mut Vec<GcRef>,
    slots: &[u64],
    slot_types: &[vo_runtime::SlotType],
) {
    trace_slots_by_types(slots, slot_types, |child| collect_gcref(roots, child));
}

#[inline]
fn collect_defer_entry_roots(roots: &mut Vec<GcRef>, entry: &DeferEntry) {
    collect_gcref(roots, entry.closure);
    if entry.args.is_null() {
        return;
    }

    collect_gcref(roots, entry.args);
    let arg_slots = entry.arg_layout.arg_slots() as usize;
    if arg_slots == 0 {
        return;
    }
    let args_data = unsafe { core::slice::from_raw_parts(entry.args as *const u64, arg_slots) };
    collect_slots_by_types(roots, args_data, &entry.arg_layout.slot_types);
}

fn collect_sentinel_error_roots(roots: &mut Vec<GcRef>, cache: &SentinelErrorCache) {
    for errors in cache.iter_values() {
        for &(slot0, slot1) in errors {
            if vo_runtime::objects::interface::data_is_gc_ref(slot0) && slot1 != 0 {
                collect_gcref(roots, slot1 as GcRef);
            }
        }
    }
}

fn collect_global_roots(roots: &mut Vec<GcRef>, globals: &[u64], global_defs: &[GlobalDef]) {
    let mut global_idx = 0;
    for def in global_defs {
        let global_slice = &globals[global_idx..global_idx + def.slots as usize];
        collect_slots_by_types(roots, global_slice, &def.slot_types);
        global_idx += def.slots as usize;
    }
}

#[cfg_attr(not(debug_assertions), allow(unused_variables))]
fn collect_fiber_roots(gc: &Gc, roots: &mut Vec<GcRef>, fiber: &Fiber, functions: &[FunctionDef]) {
    if fiber.state.is_dead() {
        return;
    }

    for frame in &fiber.frames {
        let func = &functions[frame.func_id as usize];
        let scan_slots = frame.scan_slots as usize;
        let stack_slice = &fiber.stack[frame.bp..frame.bp + scan_slots];
        let slot_types = &func.slot_types[..func.slot_types.len().min(scan_slots)];
        #[cfg(debug_assertions)]
        {
            for (slot_idx, slot_type) in slot_types.iter().enumerate() {
                match *slot_type {
                    vo_runtime::SlotType::GcRef => {
                        let raw = stack_slice[slot_idx];
                        if raw != 0 && gc.canonicalize_ref(raw as GcRef).is_none() {
                            panic!(
                                "collect_fiber_roots: invalid GcRef in fiber={} func={} name={} frame_bp={} frame_pc={} frame_scan_slots={} scan_slot={} raw=0x{:016x}",
                                fiber.id,
                                frame.func_id,
                                func.name,
                                frame.bp,
                                frame.pc,
                                frame.scan_slots,
                                slot_idx,
                                raw,
                            );
                        }
                    }
                    vo_runtime::SlotType::Interface0 => {
                        if slot_idx + 1 < stack_slice.len()
                            && vo_runtime::objects::interface::data_is_gc_ref(stack_slice[slot_idx])
                        {
                            let raw = stack_slice[slot_idx + 1];
                            if raw != 0 && gc.canonicalize_ref(raw as GcRef).is_none() {
                                panic!(
                                    "collect_fiber_roots: invalid interface GcRef in fiber={} func={} name={} frame_bp={} frame_pc={} frame_scan_slots={} scan_slot={} slot0=0x{:016x} raw=0x{:016x}",
                                    fiber.id,
                                    frame.func_id,
                                    func.name,
                                    frame.bp,
                                    frame.pc,
                                    frame.scan_slots,
                                    slot_idx,
                                    stack_slice[slot_idx],
                                    raw,
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        collect_slots_by_types(roots, stack_slice, slot_types);
    }

    for entry in &fiber.defer_stack {
        collect_defer_entry_roots(roots, entry);
    }

    if let Some(state) = &fiber.unwinding {
        for entry in &state.pending {
            collect_defer_entry_roots(roots, entry);
        }
        if let Some(ref rv) = state.return_values {
            match rv {
                crate::fiber::ReturnValues::Stack { vals, slot_types } => {
                    collect_slots_by_types(roots, vals, slot_types);
                }
                crate::fiber::ReturnValues::Heap { gcrefs, .. } => {
                    collect_gcrefs(roots, gcrefs);
                }
            }
        }
    }

    if let Some(PanicState::Recoverable(val)) = fiber.panic_state {
        if val.is_ref_type() && val.slot1 != 0 {
            collect_gcref(roots, val.as_ref());
        }
    }

    for (vals, slot_types) in &fiber.closure_replay.results {
        collect_slots_by_types(roots, vals, slot_types);
    }

    #[cfg(feature = "jit")]
    if let Some(crate::fiber::JitExternSuspend::CallClosure {
        closure_ref, args, ..
    }) = &fiber.jit_extern_suspend
    {
        collect_gcref(roots, *closure_ref);
        collect_slots_by_types(roots, &args.values, &args.slot_types);
    }

    if let Some(ref ss) = fiber.select_state {
        for registered in &ss.registered_queues {
            collect_gcref(roots, registered.queue);
        }
        if let Some(crate::fiber::SelectWokenResult::Recv {
            data, slot_types, ..
        }) = &ss.woken_result
        {
            collect_slots_by_types(roots, data, slot_types);
        }
    }

    if let Some(state) = fiber.queue_wait_state {
        collect_gcref(roots, state.queue_ref);
    }

    #[cfg(feature = "jit")]
    if fiber.jit_panic_flag {
        let val = fiber.jit_panic_msg;
        if val.is_ref_type() && val.slot1 != 0 {
            collect_gcref(roots, val.as_ref());
        }
    }
}

fn collect_all_fiber_roots(
    gc: &Gc,
    roots: &mut Vec<GcRef>,
    fibers: &[Box<Fiber>],
    functions: &[FunctionDef],
) {
    for fiber in fibers {
        collect_fiber_roots(gc, roots, fiber, functions);
    }
}

#[allow(clippy::too_many_arguments)]
fn build_vm_root_scan_snapshot(
    gc: &Gc,
    kind: GcRootScanKind,
    dirty_epoch: u64,
    dirty_all: bool,
    dirty_fibers: &[u32],
    globals: &[u64],
    global_defs: &[GlobalDef],
    fibers: &[Box<Fiber>],
    functions: &[FunctionDef],
    sentinel_errors: &SentinelErrorCache,
    endpoint_registry: &EndpointRegistry,
) -> VmRootScanSnapshot {
    let mode = if kind == GcRootScanKind::Sweep && !dirty_all {
        VmRootScanMode::DirtyFibers
    } else {
        VmRootScanMode::Full
    };
    let mut roots = Vec::new();

    collect_global_roots(&mut roots, globals, global_defs);
    match mode {
        VmRootScanMode::Full => {
            collect_all_fiber_roots(gc, &mut roots, fibers, functions);
        }
        VmRootScanMode::DirtyFibers => {
            for raw in dirty_fibers {
                if let Some(fiber) = fibers.get(*raw as usize) {
                    collect_fiber_roots(gc, &mut roots, fiber, functions);
                }
            }
        }
    }
    collect_sentinel_error_roots(&mut roots, sentinel_errors);
    for ch in endpoint_registry.live_handles() {
        collect_gcref(&mut roots, ch);
    }

    VmRootScanSnapshot {
        kind,
        mode,
        dirty_epoch,
        dirty_fibers: if mode == VmRootScanMode::DirtyFibers {
            dirty_fibers.to_vec()
        } else {
            Vec::new()
        },
        roots,
        cursor: 0,
        stage: VmRootScanStage::Done,
        global_def_cursor: global_defs.len(),
        global_base_cursor: globals.len(),
        global_slot_cursor: 0,
        fiber_source_cursor: fibers.len(),
        fiber_frame_cursor: 0,
        fiber_slot_cursor: 0,
        fiber_aux_stage: VmFiberRootScanStage::Done,
        fiber_aux_outer_cursor: 0,
        fiber_aux_slot_cursor: 0,
        sentinel_cursor: 0,
        endpoint_cursor: 0,
    }
}

fn new_vm_root_scan_snapshot(
    kind: GcRootScanKind,
    dirty_epoch: u64,
    dirty_all: bool,
    dirty_fibers: &[u32],
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
        dirty_fibers: if mode == VmRootScanMode::DirtyFibers {
            dirty_fibers.to_vec()
        } else {
            Vec::new()
        },
        roots: Vec::new(),
        cursor: 0,
        stage: VmRootScanStage::Globals,
        global_def_cursor: 0,
        global_base_cursor: 0,
        global_slot_cursor: 0,
        fiber_source_cursor: 0,
        fiber_frame_cursor: 0,
        fiber_slot_cursor: 0,
        fiber_aux_stage: VmFiberRootScanStage::Defers,
        fiber_aux_outer_cursor: 0,
        fiber_aux_slot_cursor: 0,
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

fn selected_fiber_index(snapshot: &VmRootScanSnapshot, fibers_len: usize) -> Option<usize> {
    match snapshot.mode {
        VmRootScanMode::Full => {
            (snapshot.fiber_source_cursor < fibers_len).then_some(snapshot.fiber_source_cursor)
        }
        VmRootScanMode::DirtyFibers => snapshot
            .dirty_fibers
            .get(snapshot.fiber_source_cursor)
            .copied()
            .map(|raw| raw as usize),
    }
}

enum AuxRootScanStep {
    Consumed(Option<GcRef>),
    BudgetExhausted,
    Done,
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
                let Some(pending) = fiber
                    .unwinding
                    .as_ref()
                    .map(|state| state.pending.as_slice())
                else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::ReturnValues);
                    continue;
                };
                let Some(entry) = pending.get(snapshot.fiber_aux_outer_cursor) else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::ReturnValues);
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
            VmFiberRootScanStage::ReturnValues => {
                let Some(values) = fiber
                    .unwinding
                    .as_ref()
                    .and_then(|state| state.return_values.as_ref())
                else {
                    reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Panic);
                    continue;
                };
                let root = match values {
                    crate::fiber::ReturnValues::Stack { vals, slot_types } => {
                        if snapshot.fiber_aux_slot_cursor == 0 {
                            assert_eq!(
                                vals.len(),
                                slot_types.len(),
                                "unwinding return root layout mismatch: fiber={} values={} slot_types={}",
                                fiber.id,
                                vals.len(),
                                slot_types.len()
                            );
                        }
                        if snapshot.fiber_aux_slot_cursor >= vals.len() {
                            reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Panic);
                            continue;
                        }
                        typed_slot_root(vals, slot_types, snapshot.fiber_aux_slot_cursor)
                    }
                    crate::fiber::ReturnValues::Heap { gcrefs, .. } => {
                        let Some(&raw) = gcrefs.get(snapshot.fiber_aux_slot_cursor) else {
                            reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Panic);
                            continue;
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
                    snapshot.fiber_aux_outer_cursor += 1;
                    snapshot.fiber_aux_slot_cursor = 0;
                    continue;
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
    kind: GcRootScanKind,
    mode: VmRootScanMode,
    dirty_epoch: u64,
    dirty_fibers: Vec<u32>,
}

impl From<&VmRootScanSnapshot> for VmRootScanCompletion {
    fn from(snapshot: &VmRootScanSnapshot) -> Self {
        Self {
            kind: snapshot.kind,
            mode: snapshot.mode,
            dirty_epoch: snapshot.dirty_epoch,
            dirty_fibers: snapshot.dirty_fibers.clone(),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn scan_vm_root_snapshot_chunk(
    gc: &mut Gc,
    root_scan: &mut Option<VmRootScanSnapshot>,
    kind: GcRootScanKind,
    limit_bytes: usize,
    dirty_epoch: u64,
    dirty_all: bool,
    dirty_fibers: &[u32],
    globals: &[u64],
    global_defs: &[GlobalDef],
    fibers: &[Box<Fiber>],
    functions: &[FunctionDef],
    sentinel_errors: &SentinelErrorCache,
    endpoint_registry: &EndpointRegistry,
    completion: &mut Option<VmRootScanCompletion>,
) -> GcRootScanChunk {
    let limit_bytes = limit_bytes.max(SLOT_BYTES);
    let mut work = 0usize;

    loop {
        let needs_new_snapshot = root_scan
            .as_ref()
            .map(|snapshot| snapshot.kind != kind)
            .unwrap_or(true);
        if needs_new_snapshot {
            *root_scan = Some(new_vm_root_scan_snapshot(
                kind,
                dirty_epoch,
                dirty_all,
                dirty_fibers,
            ));
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
            if let Some(&root) = snapshot.roots.get(snapshot.cursor) {
                if work >= limit_bytes {
                    return GcRootScanChunk::pending(work);
                }
                gc.mark_gray(root);
                snapshot.cursor += 1;
                work += SLOT_BYTES;
                continue;
            }
            snapshot.roots.clear();
            snapshot.cursor = 0;

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
                        gc.mark_gray(root);
                    }
                    snapshot.global_slot_cursor += 1;
                    work += SLOT_BYTES;
                }
                VmRootScanStage::Fibers => {
                    let Some(fiber_idx) = selected_fiber_index(snapshot, fibers.len()) else {
                        snapshot.stage = VmRootScanStage::SentinelErrors;
                        continue;
                    };
                    if fiber_idx >= fibers.len() {
                        snapshot.fiber_source_cursor += 1;
                        snapshot.fiber_frame_cursor = 0;
                        snapshot.fiber_slot_cursor = 0;
                        reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Defers);
                        continue;
                    }
                    let fiber = &fibers[fiber_idx];
                    if fiber.state.is_dead() {
                        snapshot.fiber_source_cursor += 1;
                        snapshot.fiber_frame_cursor = 0;
                        snapshot.fiber_slot_cursor = 0;
                        reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Defers);
                        continue;
                    }
                    if let Some(frame) = fiber.frames.get(snapshot.fiber_frame_cursor) {
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
                                gc.mark_gray(root);
                            }
                            snapshot.fiber_slot_cursor += 1;
                            work += SLOT_BYTES;
                            continue;
                        }
                        snapshot.fiber_frame_cursor += 1;
                        snapshot.fiber_slot_cursor = 0;
                        continue;
                    }

                    match scan_fiber_aux_root(snapshot, fiber, work < limit_bytes) {
                        AuxRootScanStep::Consumed(root) => {
                            if let Some(root) = root {
                                gc.mark_gray(root);
                            }
                            work += SLOT_BYTES;
                        }
                        AuxRootScanStep::BudgetExhausted => {
                            return GcRootScanChunk::pending(work);
                        }
                        AuxRootScanStep::Done => {
                            snapshot.fiber_source_cursor += 1;
                            snapshot.fiber_frame_cursor = 0;
                            snapshot.fiber_slot_cursor = 0;
                            reset_fiber_aux_stage(snapshot, VmFiberRootScanStage::Defers);
                        }
                    }
                }
                VmRootScanStage::SentinelErrors => {
                    let Some(root) = sentinel_errors.gc_root_at(snapshot.sentinel_cursor) else {
                        snapshot.stage = VmRootScanStage::Endpoints;
                        continue;
                    };
                    if work >= limit_bytes {
                        return GcRootScanChunk::pending(work);
                    }
                    gc.mark_gray(root);
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
                        gc.mark_gray(root);
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
            self.state.pending_runtime_transitions.is_empty(),
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
        let raw = fiber_id.to_raw();
        let already_dirty =
            self.state.gc_roots_dirty_all || self.state.gc_dirty_fibers.contains(&raw);
        if self.state.gc_root_scan.is_some() || !already_dirty {
            self.state.gc_dirty_epoch = self.state.gc_dirty_epoch.wrapping_add(1);
        }
        if self.state.gc_roots_dirty_all {
            return;
        }
        if !self.state.gc_dirty_fibers.contains(&raw) {
            self.state.gc_dirty_fibers.push(raw);
        }
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

    /// Telemetry for the most recent VM-triggered incremental GC step.
    #[inline]
    pub fn last_gc_step_stats(&self) -> VmGcStepStats {
        self.state.last_gc_step_stats
    }

    pub(crate) fn gc_step_after_fiber(&mut self, mutated_fiber: Option<FiberId>) {
        self.assert_no_pending_runtime_transitions_for_gc();
        if !self.state.gc.should_step() {
            return;
        }
        if let Some(fiber_id) = mutated_fiber {
            self.mark_gc_fiber_roots_dirty(fiber_id);
        }
        let module = match &self.module {
            Some(module) => module.as_ref() as *const crate::bytecode::Module,
            None => return,
        };
        // SAFETY: Split borrow via raw pointer. gc is exclusively accessed by step(),
        // while scan_roots/scan_object/finalize read other fields (globals, fibers, etc).
        // No aliasing because gc is a distinct field from globals/fibers/sentinel_errors.
        let gc_ptr = &mut self.state.gc as *mut vo_runtime::gc::Gc;
        let root_scan_ptr = &mut self.state.gc_root_scan as *mut Option<VmRootScanSnapshot>;
        let globals = &self.state.globals;
        let sentinel_errors = &self.state.sentinel_errors;
        let fibers = &self.scheduler.fibers;
        let module_ref = unsafe { &*module };

        let endpoint_registry = &self.state.endpoint_registry;
        let gc_state_before = unsafe { &*gc_ptr }.state();
        let dirty_all = self.state.gc_roots_dirty_all;
        let dirty_epoch = self.state.gc_dirty_epoch;
        let dirty_fibers = self.state.gc_dirty_fibers.clone();
        let root_state =
            if gc_state_before == GcState::Sweep && !dirty_all && dirty_fibers.is_empty() {
                GcRootState::StableSinceLastScan
            } else {
                GcRootState::MayHaveChanged
            };
        let mut full_roots_scanned = false;
        let mut dirty_roots_scanned = false;
        let mut completed_root_scan: Option<VmRootScanCompletion> = None;
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
            (&mut *gc_ptr).step_with_root_scanner(
                root_state,
                |gc, kind, limit| {
                    scan_vm_root_snapshot_chunk(
                        gc,
                        &mut *root_scan_ptr,
                        kind,
                        limit,
                        dirty_epoch,
                        dirty_all,
                        &dirty_fibers,
                        globals,
                        &module_ref.globals,
                        fibers,
                        &module_ref.functions,
                        sentinel_errors,
                        endpoint_registry,
                        &mut completed_root_scan,
                    )
                },
                |gc, obj| {
                    vo_runtime::gc_types::scan_object_with_context(
                        gc,
                        obj,
                        vo_runtime::gc_types::GcScanContext::from_module_parts(
                            &module_ref.struct_metas,
                            &module_ref.named_type_metas,
                            &module_ref.runtime_types,
                        ),
                        &func_closure_scan_layout,
                    );
                },
                |obj| {
                    vo_runtime::gc_types::finalize_object(obj);
                },
            )
        };

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
            dirty_fiber_count: dirty_fibers.len(),
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
                        self.state.gc_dirty_fibers.clear();
                    }
                }
                VmRootScanMode::DirtyFibers => {
                    if completion.dirty_epoch == self.state.gc_dirty_epoch {
                        self.state.gc_dirty_fibers.clear();
                    } else if completion.kind == GcRootScanKind::Sweep {
                        for raw in completion.dirty_fibers {
                            if !self.state.gc_dirty_fibers.contains(&raw) {
                                self.state.gc_dirty_fibers.push(raw);
                            }
                        }
                    }
                }
            }
        }

        if self.state.gc_verify_after_step {
            if let Err(err) = self.verify_precise_gc_after_step(module_ref) {
                panic!("GC verification failed: {err}");
            }
        }
    }

    fn verify_precise_gc_after_step(&self, module: &Module) -> Result<(), String> {
        let snapshot = build_vm_root_scan_snapshot(
            &self.state.gc,
            GcRootScanKind::Atomic,
            self.state.gc_dirty_epoch,
            true,
            &[],
            &self.state.globals,
            &module.globals,
            &self.scheduler.fibers,
            &module.functions,
            &self.state.sentinel_errors,
            &self.state.endpoint_registry,
        );

        for root in snapshot.roots {
            if root.is_null() {
                continue;
            }
            let Some(canonical_root) = self.state.gc.canonicalize_ref(root) else {
                return Err(format!(
                    "root {:?} does not reference a live GC object",
                    root
                ));
            };
            let dangling_white = match self.state.gc.state() {
                GcState::Pause => false,
                GcState::Sweep => self.state.gc.is_dead_white(canonical_root),
                _ => self.state.gc.is_white(canonical_root),
            };
            if dangling_white {
                return Err(format!(
                    "root {:?} references unreachable white object {:?} during {:?}",
                    root,
                    canonical_root,
                    self.state.gc.state()
                ));
            }
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
                    vo_runtime::gc_types::GcScanContext::from_module_parts(
                        &module.struct_metas,
                        &module.named_type_metas,
                        &module.runtime_types,
                    ),
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
    fn select_woken_recv_payload_scanned_003(select_region: &[u8]) -> bool {
        let select_region =
            vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(
                select_region,
            );
        let select_region = select_region.as_slice();
        let markers = [
            "ifletSome(crate::fiber::SelectWokenResult::Recv{data,slot_types,..})=&ss.woken_result{",
            "ifletSome(SelectWokenResult::Recv{data,slot_types,..})=&ss.woken_result{",
        ];
        let Some((marker_pos, marker)) = markers.iter().find_map(|marker| {
            vo_source_contract::compact_pattern_position(select_region, marker)
                .map(|pos| (pos, *marker))
        }) else {
            return false;
        };
        let open = marker_pos + marker.len() - 1;
        let Some(close) = vo_source_contract::compact_delimiter_close(select_region, open) else {
            return false;
        };
        let recv_body = &select_region[open + 1..close];
        let Some(scan_pos) = vo_source_contract::compact_pattern_position(
            recv_body,
            "collect_slots_by_types(roots,data,slot_types)",
        ) else {
            return false;
        };
        let before_scan = &recv_body[..scan_pos];
        !vo_source_contract::compact_contains(before_scan, "letdata")
            && !vo_source_contract::compact_contains(before_scan, "letmutdata")
            && !vo_source_contract::compact_contains(before_scan, "letrefdata")
            && !vo_source_contract::compact_contains(before_scan, "letslot_types")
            && !vo_source_contract::compact_contains(before_scan, "letmutslot_types")
            && !vo_source_contract::compact_contains(before_scan, "letrefslot_types")
            && !vo_source_contract::compact_contains(before_scan, "let(data")
            && !vo_source_contract::compact_contains(before_scan, "let(mutdata")
            && !vo_source_contract::compact_contains(before_scan, "let(refdata")
    }

    #[test]
    fn vm_gc_select_woken_payload_root_003_collects_materialized_recv_payload() {
        let src = crate::source_contract::production_source_without_test_modules(include_str!(
            "gc_roots.rs"
        ));
        let select_region = vo_source_contract::compact_region_between(
            &src,
            "ifletSome(refss)=fiber.select_state{",
            "ifletSome(state)=fiber.queue_wait_state{",
        )
        .expect("select root scan should precede queue wait scan");

        assert!(
            select_woken_recv_payload_scanned_003(&select_region),
            "select wake recv payloads must use their recorded slot metadata"
        );
    }

    #[test]
    fn vm_gc_select_woken_payload_root_003_rejects_comment_spoofed_payload_scan() {
        let spoof = r#"
            if let Some(ref ss) = fiber.select_state {
                // ss.woken_result
                // SelectWokenResult::Recv { data, slot_types, .. }
                // collect_slots_by_types(roots, data, slot_types)
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let select_region = vo_source_contract::compact_region_between(
            spoof,
            "ifletSome(refss)=fiber.select_state{",
            "ifletSome(state)=fiber.queue_wait_state{",
        )
        .expect("probe select root scan");

        assert!(
            !select_woken_recv_payload_scanned_003(&select_region),
            "comment-only select wake payload root-scan facts must not satisfy the contract"
        );
    }

    #[test]
    fn vm_gc_select_woken_payload_root_003_rejects_sibling_payload_scan() {
        let spoof = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                }
                collect_slots_by_types(roots, data, slot_types);
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let select_region = vo_source_contract::compact_region_between(
            spoof,
            "ifletSome(refss)=fiber.select_state{",
            "ifletSome(state)=fiber.queue_wait_state{",
        )
        .expect("probe select root scan");

        assert!(
            !select_woken_recv_payload_scanned_003(&select_region),
            "a sibling scan must not satisfy the select Recv payload root contract"
        );
    }

    #[test]
    fn vm_gc_select_woken_payload_root_003_rejects_unreachable_payload_scan() {
        let spoof = r#"
            if let Some(ref ss) = fiber.select_state {
                if false {
                    if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                        collect_slots_by_types(roots, data, slot_types);
                    }
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let select_region = vo_source_contract::compact_region_between(
            spoof,
            "ifletSome(refss)=fiber.select_state{",
            "ifletSome(state)=fiber.queue_wait_state{",
        )
        .expect("probe select root scan");

        assert!(
            !select_woken_recv_payload_scanned_003(&select_region),
            "unreachable select wake payload root-scan facts must not satisfy the contract"
        );
    }

    #[test]
    fn vm_gc_select_woken_payload_root_003_rejects_rebound_payload_names() {
        let rebound_data = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let data = &[];
                    collect_slots_by_types(roots, data, slot_types);
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let rebound_slot_types = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let mut slot_types = &[];
                    collect_slots_by_types(roots, data, slot_types);
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let tuple_rebound = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let (data, slot_types) = (&[][..], &[][..]);
                    collect_slots_by_types(roots, data, slot_types);
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let typed_rebound = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let data: &[u64] = &[];
                    collect_slots_by_types(roots, data, slot_types);
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let ref_rebound = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let ref data = &[];
                    collect_slots_by_types(roots, data, slot_types);
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let mut_tuple_rebound = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let (mut data, slot_types) = (&[][..], &[][..]);
                    collect_slots_by_types(roots, data, slot_types);
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let typed_tuple_rebound = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let (data, slot_types): (&[u64], &[SlotType]) = (&[][..], &[][..]);
                    collect_slots_by_types(roots, data, slot_types);
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;

        for spoof in [
            rebound_data,
            rebound_slot_types,
            tuple_rebound,
            typed_rebound,
            ref_rebound,
            mut_tuple_rebound,
            typed_tuple_rebound,
        ] {
            let select_region = vo_source_contract::compact_region_between(
                spoof,
                "ifletSome(refss)=fiber.select_state{",
                "ifletSome(state)=fiber.queue_wait_state{",
            )
            .expect("probe select root scan");

            assert!(
                !select_woken_recv_payload_scanned_003(&select_region),
                "rebound Recv payload names must not satisfy the select root-scan contract"
            );
        }
    }

    #[test]
    fn vm_gc_select_woken_payload_root_003_rejects_closure_payload_scan() {
        let spoof = r#"
            if let Some(ref ss) = fiber.select_state {
                if let Some(SelectWokenResult::Recv { data, slot_types, .. }) = &ss.woken_result {
                    let _unused = || {
                        collect_slots_by_types(roots, data, slot_types);
                    };
                }
            }
            if let Some(state) = fiber.queue_wait_state {
            }
        "#;
        let select_region = vo_source_contract::compact_region_between(
            spoof,
            "ifletSome(refss)=fiber.select_state{",
            "ifletSome(state)=fiber.queue_wait_state{",
        )
        .expect("probe select root scan");

        assert!(
            !select_woken_recv_payload_scanned_003(&select_region),
            "unused closure payload scans must not satisfy the select root-scan contract"
        );
    }
}
