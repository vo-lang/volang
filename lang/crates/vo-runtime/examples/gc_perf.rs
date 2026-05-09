#[path = "../tests/support/gc_perf_harness.rs"]
mod gc_perf_harness;

use gc_perf_harness::{
    default_scenarios, format_reports_table, reports_to_json, run_gc_scenario, GcScenarioConfig,
    GcScenarioKind,
};

fn main() {
    let mut objects = 20_000usize;
    let mut json = false;
    let mut selected = Vec::new();

    for arg in std::env::args().skip(1) {
        if arg == "--json" {
            json = true;
        } else if arg == "--small" {
            objects = 4_096;
        } else if arg == "--large" {
            objects = 100_000;
        } else if let Some(raw) = arg.strip_prefix("--objects=") {
            objects = raw
                .parse()
                .unwrap_or_else(|_| panic!("invalid --objects value: {raw}"));
        } else if let Some(kind) = GcScenarioKind::from_name(&arg) {
            selected.push(kind);
        } else {
            panic!("unknown gc perf argument: {arg}");
        }
    }

    let configs: Vec<GcScenarioConfig> = if selected.is_empty() {
        default_scenarios(objects)
    } else {
        selected
            .into_iter()
            .map(|kind| GcScenarioConfig::new(kind, objects))
            .collect()
    };
    let reports: Vec<_> = configs.into_iter().map(run_gc_scenario).collect();

    if json {
        println!("{}", reports_to_json(&reports));
    } else {
        print!("{}", format_reports_table(&reports));
    }
}
