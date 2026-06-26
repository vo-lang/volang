#![allow(dead_code)]

use std::time::Instant;

use vo_runtime::gc::{
    scan_slots_by_types, Gc, GcRef, GcRootScanChunk, GcRootScanKind, GcRootState, GcState,
};
use vo_runtime::slot::SLOT_BYTES;
use vo_runtime::{SlotType, ValueKind, ValueMeta};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcScenarioKind {
    DeadSweep,
    LiveChain,
    RootTable,
    SparseRootTable,
    InteriorRootTable,
}

#[derive(Debug, Clone, Copy)]
pub struct GcScenarioConfig {
    pub kind: GcScenarioKind,
    pub objects: usize,
    pub max_steps: usize,
}

#[derive(Debug, Clone)]
pub struct GcPerfReport {
    pub name: &'static str,
    pub objects: usize,
    pub root_slots: usize,
    pub live_roots: usize,
    pub steps: usize,
    pub root_scans: usize,
    pub root_scan_skips: usize,
    pub root_scan_work_bytes: usize,
    pub object_scans: usize,
    pub finalized: usize,
    pub sweep_freed_bytes: usize,
    pub total_work_bytes: usize,
    pub max_step_work_bytes: usize,
    pub max_step_ms: f64,
    pub total_ms: f64,
    pub pause_steps: usize,
    pub propagate_steps: usize,
    pub atomic_steps: usize,
    pub sweep_steps: usize,
    pub final_objects: usize,
    pub final_bytes: usize,
    pub completed: bool,
}

impl GcScenarioKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::DeadSweep => "dead-sweep",
            Self::LiveChain => "live-chain",
            Self::RootTable => "root-table",
            Self::SparseRootTable => "sparse-root-table",
            Self::InteriorRootTable => "interior-root-table",
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "dead-sweep" | "dead" | "sweep" => Some(Self::DeadSweep),
            "live-chain" | "chain" | "live" => Some(Self::LiveChain),
            "root-table" | "roots" => Some(Self::RootTable),
            "sparse-root-table" | "sparse-roots" | "sparse" => Some(Self::SparseRootTable),
            "interior-root-table" | "interior-roots" | "interior" => Some(Self::InteriorRootTable),
            _ => None,
        }
    }
}

impl GcScenarioConfig {
    pub fn new(kind: GcScenarioKind, objects: usize) -> Self {
        Self {
            kind,
            objects,
            max_steps: objects.saturating_mul(8).saturating_add(128),
        }
    }
}

pub fn default_scenarios(objects: usize) -> Vec<GcScenarioConfig> {
    vec![
        GcScenarioConfig::new(GcScenarioKind::DeadSweep, objects),
        GcScenarioConfig::new(GcScenarioKind::LiveChain, objects),
        GcScenarioConfig::new(GcScenarioKind::RootTable, objects),
        GcScenarioConfig::new(GcScenarioKind::SparseRootTable, objects),
        GcScenarioConfig::new(GcScenarioKind::InteriorRootTable, objects),
    ]
}

pub fn run_gc_scenario(config: GcScenarioConfig) -> GcPerfReport {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let slots = match config.kind {
        GcScenarioKind::LiveChain => 1,
        GcScenarioKind::InteriorRootTable => 8,
        _ => 0,
    };
    let mut objects = Vec::with_capacity(config.objects);

    for _ in 0..config.objects {
        objects.push(gc.alloc(meta, slots));
    }

    if config.kind == GcScenarioKind::LiveChain {
        for pair in objects.windows(2) {
            unsafe {
                Gc::write_slot(pair[0], 0, pair[1] as u64);
            }
        }
    }

    let root_slots = build_root_slots(config.kind, &objects);
    let root_slot_types = vec![SlotType::GcRef; root_slots.len()];
    let live_roots = root_slots.iter().filter(|&&raw| raw != 0).count();
    let mut report = GcPerfReport {
        name: config.kind.name(),
        objects: config.objects,
        root_slots: root_slots.len(),
        live_roots,
        steps: 0,
        root_scans: 0,
        root_scan_skips: 0,
        root_scan_work_bytes: 0,
        object_scans: 0,
        finalized: 0,
        sweep_freed_bytes: 0,
        total_work_bytes: 0,
        max_step_work_bytes: 0,
        max_step_ms: 0.0,
        total_ms: 0.0,
        pause_steps: 0,
        propagate_steps: 0,
        atomic_steps: 0,
        sweep_steps: 0,
        final_objects: 0,
        final_bytes: 0,
        completed: false,
    };

    let mut root_scanner = RootSlotScanner::new();
    let start = Instant::now();
    while gc.should_step() {
        count_phase(&mut report, gc.state());
        let step_start = Instant::now();
        let work = unsafe {
            gc.step_with_root_scanner(
                GcRootState::StableSinceLastScan,
                |gc, kind, limit| root_scanner.scan(gc, kind, limit, &root_slots, &root_slot_types),
                |gc, obj| {
                    scan_struct_like_object(gc, obj);
                },
                |_| {},
            )
        };
        let step_stats = gc.last_step_stats();
        let step_ms = step_start.elapsed().as_secs_f64() * 1000.0;
        report.steps += 1;
        report.root_scans += step_stats.root_scan_calls;
        report.root_scan_skips += step_stats.root_scan_skips;
        report.root_scan_work_bytes += step_stats.root_scan_work_bytes;
        report.object_scans += step_stats.object_scans;
        report.finalized += step_stats.finalized_objects;
        report.sweep_freed_bytes += step_stats.sweep_freed_bytes;
        report.total_work_bytes += work;
        report.max_step_work_bytes = report.max_step_work_bytes.max(work);
        report.max_step_ms = report.max_step_ms.max(step_ms);
        if report.steps > config.max_steps {
            break;
        }
    }
    report.total_ms = start.elapsed().as_secs_f64() * 1000.0;
    report.final_objects = gc.object_count();
    report.final_bytes = gc.total_bytes();
    report.completed = !gc.should_step() && gc.state() == GcState::Pause;

    cleanup_gc(gc, config.max_steps);
    report
}

pub fn format_reports_table(reports: &[GcPerfReport]) -> String {
    let mut out = String::new();
    out.push_str(
        "scenario           objs    roots steps root_scans root_skips root_work obj_scans finalized max_work max_ms total_ms final_objs\n",
    );
    for report in reports {
        out.push_str(&format!(
            "{:<17} {:>7} {:>7} {:>5} {:>10} {:>10} {:>9} {:>9} {:>9} {:>8} {:>6.3} {:>8.3} {:>10}\n",
            report.name,
            report.objects,
            report.live_roots,
            report.steps,
            report.root_scans,
            report.root_scan_skips,
            report.root_scan_work_bytes,
            report.object_scans,
            report.finalized,
            report.max_step_work_bytes,
            report.max_step_ms,
            report.total_ms,
            report.final_objects,
        ));
    }
    out
}

pub fn reports_to_json(reports: &[GcPerfReport]) -> String {
    let mut out = String::from("[");
    for (idx, report) in reports.iter().enumerate() {
        if idx > 0 {
            out.push(',');
        }
        out.push_str(&report_to_json(report));
    }
    out.push(']');
    out
}

fn report_to_json(report: &GcPerfReport) -> String {
    format!(
        concat!(
            "{{",
            "\"name\":\"{}\",",
            "\"objects\":{},",
            "\"rootSlots\":{},",
            "\"liveRoots\":{},",
            "\"steps\":{},",
            "\"rootScans\":{},",
            "\"rootScanSkips\":{},",
            "\"rootScanWorkBytes\":{},",
            "\"objectScans\":{},",
            "\"finalized\":{},",
            "\"sweepFreedBytes\":{},",
            "\"totalWorkBytes\":{},",
            "\"maxStepWorkBytes\":{},",
            "\"maxStepMs\":{:.6},",
            "\"totalMs\":{:.6},",
            "\"pauseSteps\":{},",
            "\"propagateSteps\":{},",
            "\"atomicSteps\":{},",
            "\"sweepSteps\":{},",
            "\"finalObjects\":{},",
            "\"finalBytes\":{},",
            "\"completed\":{}",
            "}}"
        ),
        report.name,
        report.objects,
        report.root_slots,
        report.live_roots,
        report.steps,
        report.root_scans,
        report.root_scan_skips,
        report.root_scan_work_bytes,
        report.object_scans,
        report.finalized,
        report.sweep_freed_bytes,
        report.total_work_bytes,
        report.max_step_work_bytes,
        report.max_step_ms,
        report.total_ms,
        report.pause_steps,
        report.propagate_steps,
        report.atomic_steps,
        report.sweep_steps,
        report.final_objects,
        report.final_bytes,
        report.completed,
    )
}

#[derive(Debug, Default)]
struct RootSlotScanner {
    kind: Option<GcRootScanKind>,
    cursor: usize,
}

impl RootSlotScanner {
    fn new() -> Self {
        Self::default()
    }

    fn scan(
        &mut self,
        gc: &mut Gc,
        kind: GcRootScanKind,
        limit_bytes: usize,
        root_slots: &[u64],
        root_slot_types: &[SlotType],
    ) -> GcRootScanChunk {
        if self.kind != Some(kind) {
            self.kind = Some(kind);
            self.cursor = 0;
        }

        if root_slots.is_empty() {
            self.kind = None;
            self.cursor = 0;
            return GcRootScanChunk::complete(0);
        }

        let slots_budget = (limit_bytes / SLOT_BYTES).max(1);
        let start = self.cursor;
        let end = start.saturating_add(slots_budget).min(root_slots.len());
        scan_slots_by_types(gc, &root_slots[start..end], &root_slot_types[start..end]);
        self.cursor = end;

        let work_bytes = (end - start) * SLOT_BYTES;
        if self.cursor >= root_slots.len() {
            self.kind = None;
            self.cursor = 0;
            GcRootScanChunk::complete(work_bytes)
        } else {
            GcRootScanChunk::pending(work_bytes)
        }
    }
}

fn build_root_slots(kind: GcScenarioKind, objects: &[GcRef]) -> Vec<u64> {
    match kind {
        GcScenarioKind::DeadSweep => Vec::new(),
        GcScenarioKind::LiveChain => objects
            .first()
            .map(|&obj| vec![obj as u64])
            .unwrap_or_default(),
        GcScenarioKind::RootTable => objects.iter().map(|&obj| obj as u64).collect(),
        GcScenarioKind::SparseRootTable => {
            let mut roots = vec![0; objects.len().saturating_mul(4).max(1)];
            for (idx, &obj) in objects.iter().enumerate() {
                roots[idx * 4] = obj as u64;
            }
            roots
        }
        GcScenarioKind::InteriorRootTable => objects
            .iter()
            .map(|&obj| unsafe { obj.add(1) as u64 })
            .collect(),
    }
}

fn count_phase(report: &mut GcPerfReport, state: GcState) {
    match state {
        GcState::Pause => report.pause_steps += 1,
        GcState::Propagate => report.propagate_steps += 1,
        GcState::Atomic => report.atomic_steps += 1,
        GcState::Sweep => report.sweep_steps += 1,
    }
}

fn scan_struct_like_object(gc: &mut Gc, obj: GcRef) {
    let slots = Gc::header(obj).slots as usize;
    for idx in 0..slots {
        let raw = unsafe { Gc::read_slot(obj, idx) };
        if raw != 0 {
            gc.mark_gray(raw as GcRef);
        }
    }
}

fn gc_step<R, S, F>(gc: &mut Gc, scan_roots: R, scan_object: S, finalize_object: F) -> usize
where
    R: FnMut(&mut Gc),
    S: FnMut(&mut Gc, GcRef),
    F: FnMut(GcRef),
{
    unsafe { gc.step(scan_roots, scan_object, finalize_object) }
}

fn cleanup_gc(mut gc: Gc, max_steps: usize) {
    if gc.object_count() == 0 {
        return;
    }
    gc.set_stress_every_step(true);
    for _ in 0..max_steps.max(128) {
        gc_step(&mut gc, |_| {}, scan_struct_like_object, |_| {});
        if gc.object_count() == 0 && gc.state() == GcState::Pause {
            break;
        }
    }
    gc.set_stress_every_step(false);
}
