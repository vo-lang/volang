//! Host-return and guest-resumption diagnostics. Compile fixtures separately;
//! run this release example with no concurrent builds or profiles for timings.
use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use std::time::Instant;
use vo_engine::{new_vm_for_mode, CaptureSink, GcMode, Module, RunMode, VmMemoryConfig};
use vo_vm::vm::SchedulingOutcome;

fn main() {
    let arguments: Vec<_> = std::env::args().collect();
    assert_eq!(
        arguments.len(),
        7,
        "usage: gc_root_latency input.vob vm|jit generational|incremental forced|control samples output.json"
    );
    let mode = match arguments[2].as_str() {
        "vm" => RunMode::Vm,
        "jit" => RunMode::Jit,
        _ => panic!("invalid execution mode"),
    };
    let gc_mode = match arguments[3].as_str() {
        "generational" => GcMode::Generational,
        "incremental" => GcMode::Incremental,
        _ => panic!("invalid GC mode"),
    };
    let forced = match arguments[4].as_str() {
        "forced" => true,
        "control" => false,
        _ => panic!("invalid admission mode"),
    };
    let samples: usize = arguments[5].parse().unwrap();
    assert!((1..=100_000).contains(&samples));
    let bytes =
        vo_common_core::serialize::read_vob_file(std::path::Path::new(&arguments[1])).unwrap();
    let vob_sha256 = format!("{:x}", Sha256::digest(&bytes));
    let module = Module::deserialize(&bytes).unwrap();
    let function = |name: &str| {
        module
            .functions
            .iter()
            .enumerate()
            .find(|(_, f)| f.name == name || f.name.ends_with(&format!(".{name}")))
            .map(|(i, _)| i as u32)
            .expect(name)
    };
    let probe = function("LatencyProbe");
    let release = function("ReleaseRoots");
    let mut vm = new_vm_for_mode(
        VmMemoryConfig {
            gc_mode,
            ..Default::default()
        },
        mode,
    )
    .unwrap();
    let output = CaptureSink::new();
    vm.set_output_sink(output.clone());
    vm.load(module).unwrap();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::Blocked);
    assert_eq!(output.take_bytes(), b"ROOTS_READY\n");
    let mut records = Vec::with_capacity(samples);
    let mut warm_stats = None;
    let mut retained_turns = 0;
    // Reuse observation storage. Formatting and retained JSON allocation happen
    // after the wall-clock interval, outside every timed VM call.
    let mut observations = Vec::with_capacity(256);
    const WARMUP: usize = 128;
    for sample in 0..samples + WARMUP {
        // Normalize the starting collector state outside the measured interval.
        // All fixtures retain their witness objects until the final release.
        vm.gc_collect().unwrap();
        vm.spawn_call(probe, &[]).unwrap();
        let before = vm.memory_stats();
        observations.clear();
        let capacity = observations.capacity();
        let started = Instant::now();
        let first = forced.then(|| vm.gc_step_units(1));
        let first_ns = if forced {
            started.elapsed().as_nanos() as u64
        } else {
            0
        };
        if let Some(first) = first {
            assert!(first.completed_work_units <= 1);
        }
        let mut active_ns = first_ns;
        for turn in 0..100_000 {
            let before_turn = vm.memory_stats();
            let clock = Instant::now();
            let outcome = vm.run_scheduled_with_budget(1).unwrap();
            let elapsed_ns = clock.elapsed().as_nanos() as u64;
            active_ns += elapsed_ns;
            let after_turn = vm.memory_stats();
            let last = vm.last_gc_step_stats();
            let guest_bytes = output.take_bytes();
            assert!(guest_bytes.is_empty() || guest_bytes == b"ROOTS_PROBE\n");
            let work_units = after_turn.work_units_total - before_turn.work_units_total;
            // Last-step stats represent one collector step only. Total work
            // differences cover every step serviced in this scheduling turn.
            observations.push((
                elapsed_ns,
                work_units,
                (work_units != 0).then_some(last),
                outcome,
                !guest_bytes.is_empty(),
            ));
            if !guest_bytes.is_empty() {
                break;
            }
            assert_ne!(outcome, SchedulingOutcome::Completed);
            assert!(turn + 1 < 100_000, "root pass failed to make progress");
        }
        let wall_ns = started.elapsed().as_nanos() as u64;
        let after = vm.memory_stats();
        let stats = vm.jit_execution_stats();
        if sample + 1 == WARMUP {
            warm_stats = Some(stats);
        }
        if sample >= WARMUP {
            assert_eq!(
                observations.capacity(),
                capacity,
                "observation buffer grew after warmup; increase its initial capacity and rerun"
            );
            retained_turns += observations.len();
            assert!(
                retained_turns <= 1_000_000,
                "diagnostic record limit exceeded"
            );
            let warm = warm_stats.as_ref().unwrap();
            assert_eq!(
                stats.function_compilations, warm.function_compilations,
                "JIT compilation leaked into warmed measurements"
            );
            assert_eq!(
                stats.loop_compilations, warm.loop_compilations,
                "OSR compilation leaked into warmed measurements"
            );
            let turns: Vec<Value> = observations
                .iter()
                .enumerate()
                .map(
                    |(turn, (elapsed_ns, work_units, last, outcome, observed))| {
                        json!({
                            "turn":turn, "host_return_ns":elapsed_ns, "work_units":work_units,
                            "last_step_root_work_bytes":last.map(|s|s.gc.root_scan_work_bytes),
                            "last_step_total_work_bytes":last.map(|s|s.gc.total_work_bytes),
                            "last_step_cycle_kind":last.map(|s|format!("{:?}",s.gc.cycle_kind)),
                            "last_step_full_roots_scanned":last.map(|s|s.full_roots_scanned),
                            "last_step_dirty_roots_scanned":last.map(|s|s.dirty_roots_scanned),
                            "last_step_phase_before":last.map(|s|format!("{:?}",s.gc.phase_before)),
                            "last_step_phase_after":last.map(|s|format!("{:?}",s.gc.phase_after)),
                            "outcome":format!("{outcome:?}"), "probe_observed":observed
                        })
                    },
                )
                .collect();
            records.push(json!({"sample":sample-WARMUP,"initial_step_ns":first_ns,
                "initial_completed_work_units":first.map_or(0, |s| s.completed_work_units),
                "initial_step_root_work_bytes":first.map(|s| s.stats.gc.root_scan_work_bytes),
                "initial_cycle_kind":first.map(|s|format!("{:?}",s.stats.gc.cycle_kind)),
                "completed_minor_cycles":after.minor_cycles-before.minor_cycles,
                "completed_major_cycles":after.major_cycles-before.major_cycles,
                "before_young_live_bytes":before.young_live_bytes,"before_old_live_bytes":before.old_live_bytes,
                "guest_resume_active_ns":active_ns,"guest_resume_wall_ns":wall_ns,
                "work_units":after.work_units_total-before.work_units_total,
                "managed_live_bytes":after.managed_live_bytes,"turns":turns}));
        }
    }
    let measured_stats = vm.jit_execution_stats();
    let warm_stats = warm_stats.unwrap();
    if arguments[2] == "jit" {
        assert!(
            measured_stats.function_entries - warm_stats.function_entries >= samples as u64,
            "measured probes must enter function JIT after warmup"
        );
    }
    vm.spawn_call(release, &[]).unwrap();
    assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Completed);
    assert_eq!(output.take_bytes(), b"ROOTS_OK\n");
    let stats = vm.jit_execution_stats();
    if arguments[2] == "jit" {
        assert!(stats.function_entries + stats.loop_entries > 0);
    }
    let result = json!({"schema":"volang.gc-root-latency.v1","source_vob":arguments[1],"vob_sha256":vob_sha256,
        "mode":arguments[2],"gc_mode":arguments[3],"admission":arguments[4],"samples":samples,"warmup":WARMUP,
        "scope":"Both arms normalize via major collection and queue the same probe outside timing. Forced arm starts one explicit GC unit; control requests no cycle. Automatic collection remains enabled. One scheduler turn per host call. Active latency sums measured VM calls; wall latency also includes fixed host-side observation. JSON formatting is outside timing. Last-step root bytes are not aggregate per-turn root bytes. All blocked roots verified after release.",
        "function_entries":stats.function_entries,"loop_entries":stats.loop_entries,
        "measured_function_entries":measured_stats.function_entries-warm_stats.function_entries,
        "measured_loop_entries":measured_stats.loop_entries-warm_stats.loop_entries,
        "records":records});
    if arguments[6] == "-" {
        println!("{}", serde_json::to_string(&result).unwrap());
    } else {
        std::fs::write(&arguments[6], serde_json::to_vec_pretty(&result).unwrap()).unwrap();
    }
}
