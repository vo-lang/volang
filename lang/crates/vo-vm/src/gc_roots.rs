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
use crate::vm::{EndpointRegistry, Vm, VmGcStepStats, VmRootScanMode, VmRootScanSnapshot};

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

    if let Some(ref ss) = fiber.select_state {
        for &ch in &ss.registered_queues {
            collect_gcref(roots, ch);
        }
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
            *root_scan = Some(build_vm_root_scan_snapshot(
                gc,
                kind,
                dirty_epoch,
                dirty_all,
                dirty_fibers,
                globals,
                global_defs,
                fibers,
                functions,
                sentinel_errors,
                endpoint_registry,
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

        let slots_budget = ((limit_bytes - work) / SLOT_BYTES).max(1);
        let start = snapshot.cursor;
        let end = start.saturating_add(slots_budget).min(snapshot.roots.len());
        for &root in &snapshot.roots[start..end] {
            gc.mark_gray(root);
        }
        snapshot.cursor = end;
        work += (end - start) * SLOT_BYTES;

        if snapshot.cursor < snapshot.roots.len() {
            return GcRootScanChunk::pending(work);
        }

        *completion = Some(VmRootScanCompletion::from(&*snapshot));
        *root_scan = None;
        return GcRootScanChunk::complete(work);
    }
}

impl Vm {
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
        if !self.state.gc.should_step() {
            return;
        }
        if let Some(fiber_id) = mutated_fiber {
            self.mark_gc_fiber_roots_dirty(fiber_id);
        }
        let module = match &self.module {
            Some(m) => m as *const crate::bytecode::Module,
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
                let runtime_capture_slot_types = if func.capture_slot_types.is_empty()
                    && func.recv_slots == 1
                {
                    func.slot_types.get(..1).unwrap_or_else(|| {
                            panic!(
                                "closure receiver slot metadata missing: func_id={} name={} slot range 0..1 actual slot_types={}",
                                func_id,
                                func.name,
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

        unsafe { &mut *gc_ptr }.step_with_root_scanner(
            root_state,
            |gc, kind, limit| {
                scan_vm_root_snapshot_chunk(
                    gc,
                    unsafe { &mut *root_scan_ptr },
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
                vo_runtime::gc_types::scan_object(
                    gc,
                    obj,
                    &module_ref.struct_metas,
                    &func_closure_scan_layout,
                );
            },
            |obj| {
                vo_runtime::gc_types::finalize_object(obj);
            },
        );

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
                let runtime_capture_slot_types = if func.capture_slot_types.is_empty()
                    && func.recv_slots == 1
                {
                    func.slot_types.get(..1).unwrap_or_else(|| {
                        panic!(
                            "closure receiver slot metadata missing: func_id={} name={} slot range 0..1 actual slot_types={}",
                            func_id,
                            func.name,
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
            vo_runtime::gc_types::trace_object_children(
                parent,
                &module.struct_metas,
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
            );
            if let Some(err) = violation {
                return Err(err);
            }
        }

        Ok(())
    }
}
