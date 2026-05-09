#[path = "support/gc_perf_harness.rs"]
mod gc_perf_harness;

use gc_perf_harness::{run_gc_scenario, GcScenarioConfig, GcScenarioKind};

#[test]
fn gc_perf_framework_completes_core_scenarios() {
    for kind in [
        GcScenarioKind::DeadSweep,
        GcScenarioKind::LiveChain,
        GcScenarioKind::RootTable,
        GcScenarioKind::SparseRootTable,
        GcScenarioKind::InteriorRootTable,
    ] {
        let report = run_gc_scenario(GcScenarioConfig::new(kind, 2048));
        assert!(report.completed, "{kind:?} did not complete: {report:?}");
        match kind {
            GcScenarioKind::DeadSweep => assert_eq!(report.final_objects, 0),
            GcScenarioKind::LiveChain
            | GcScenarioKind::RootTable
            | GcScenarioKind::SparseRootTable
            | GcScenarioKind::InteriorRootTable => {
                assert_eq!(report.final_objects, report.objects);
                assert_eq!(report.object_scans, report.objects);
            }
        }
    }
}

#[test]
fn gc_perf_framework_keeps_large_live_graph_incremental() {
    let report = run_gc_scenario(GcScenarioConfig::new(GcScenarioKind::LiveChain, 8192));

    assert!(report.completed, "live chain did not complete: {report:?}");
    assert_eq!(report.final_objects, report.objects);
    assert!(
        report.steps > 8,
        "large live graph should be sliced across scheduler boundaries: {report:?}"
    );
    assert!(
        report.max_step_work_bytes <= 16 * 1024,
        "single GC step processed too much heap work: {report:?}"
    );
}

#[test]
fn gc_perf_framework_slices_large_root_table_scan() {
    let report = run_gc_scenario(GcScenarioConfig::new(GcScenarioKind::RootTable, 8192));

    assert!(report.completed, "root table did not complete: {report:?}");
    assert_eq!(report.final_objects, report.objects);
    assert!(
        report.root_scans > 2,
        "large root table should be scanned in bounded chunks: {report:?}"
    );
    assert!(
        report.root_scan_work_bytes >= report.root_slots * 2 * 8,
        "start and atomic root scans should both be counted: {report:?}"
    );
    assert!(
        report.max_step_work_bytes <= 16 * 1024,
        "single GC step processed too much root/heap work: {report:?}"
    );
}
