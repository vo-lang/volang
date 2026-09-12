//! Parent scheduling work with independent idle child Islands. Compile the
//! shared fixture and freeze this example before running timing comparisons.
use serde_json::json;
use sha2::{Digest, Sha256};
use std::time::Instant;
use vo_engine::{CaptureSink, Module};
use vo_vm::vm::{JitConfig, JitExecutionStats, SchedulingOutcome, Vm};

fn stable_compilation(before: JitExecutionStats, after: JitExecutionStats) {
    assert_eq!(before.function_compilations, after.function_compilations);
    assert_eq!(before.loop_compilations, after.loop_compilations);
    assert_eq!(
        before.optimizing_compilations,
        after.optimizing_compilations
    );
    assert_eq!(before.optimizing_failures, after.optimizing_failures);
    assert_eq!(before.deopts, after.deopts);
}

fn main() {
    assert_eq!(std::env::var("VOWORK").as_deref(), Ok("off"));
    let args: Vec<_> = std::env::args().collect();
    assert_eq!(args.len(), 7, "usage: island_poll INPUT.vob vm|baseline|optimizing CHILDREN idle|busy SAMPLES OUTPUT.json");
    let children: usize = args[3].parse().unwrap();
    assert!([0, 1, 8, 32, 128].contains(&children));
    let busy = match args[4].as_str() {
        "idle" => false,
        "busy" => true,
        _ => panic!("invalid workload"),
    };
    let samples: usize = args[5].parse().unwrap();
    assert!((1..=10000).contains(&samples));
    let bytes = vo_common_core::serialize::read_vob_file(std::path::Path::new(&args[1])).unwrap();
    let vob_sha256 = format!("{:x}", Sha256::digest(&bytes));
    let module = Module::deserialize(&bytes).unwrap();
    let function = |name: &str| {
        let matches: Vec<_> = module
            .functions
            .iter()
            .enumerate()
            .filter(|(_, function)| {
                function.name == name || function.name.ends_with(&format!(".{name}"))
            })
            .map(|(id, _)| id as u32)
            .collect();
        assert_eq!(
            matches.len(),
            1,
            "ambiguous or absent probe function {name}"
        );
        matches[0]
    };
    let configure = function("Configure");
    let probe = function("PollProbe");
    let validate = function("Validate");
    let release = function("Release");
    let mut vm = match args[2].as_str() {
        "vm" => Vm::new(),
        "baseline" | "optimizing" => Vm::try_with_jit_config(JitConfig {
            call_threshold: 4,
            loop_threshold: u32::MAX,
            optimizing_threshold: if args[2] == "optimizing" {
                32
            } else {
                u64::MAX
            },
            ..Default::default()
        })
        .unwrap(),
        _ => panic!("invalid execution mode"),
    };
    let sink = CaptureSink::new();
    vm.set_output_sink(sink.clone());
    vm.load(module).unwrap();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::Blocked);
    vm.spawn_call(configure, &[children as u64]).unwrap();
    assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Blocked);
    assert_eq!(sink.take_bytes(), b"POLL_READY\n");
    vm.gc_collect().unwrap();
    // Prepare all scratch storage before the measured intervals. Each busy
    // batch queues identical guest work outside the clock; idle batches run
    // the same public bounded scheduler without runnable guest work.
    const WARMUP: usize = 128;
    const BUSY_BATCH: usize = 32;
    const IDLE_BATCH: usize = 4096;
    let mut records = Vec::with_capacity(samples);
    let mut expected = 7_u64;
    let mut warm = None;
    let mut measured_entries = 0;
    for sample in 0..WARMUP + samples {
        if busy {
            for _ in 0..BUSY_BATCH {
                vm.spawn_call(probe, &[]).unwrap();
                for _ in 0..64 {
                    expected = (expected * 1664525 + 1013904223) & 2147483647;
                }
            }
        }
        // Output validation creates guest string objects between batches.
        // Finish that collection outside every interval, including the roots
        // of newly queued calls. Keep the real GC policy and assert that the
        // measured scheduler work itself performs no collection.
        vm.gc_collect().unwrap();
        let before = vm.jit_execution_stats();
        let memory_before = vm.memory_stats();
        let started = Instant::now();
        if busy {
            assert_eq!(
                vm.run_scheduled_with_budget(BUSY_BATCH).unwrap(),
                SchedulingOutcome::Blocked
            );
        } else {
            for _ in 0..IDLE_BATCH {
                assert_eq!(
                    vm.run_scheduled_with_budget(1).unwrap(),
                    SchedulingOutcome::Blocked
                );
            }
        }
        let elapsed_ns = started.elapsed().as_nanos();
        let after = vm.jit_execution_stats();
        let memory_after = vm.memory_stats();
        assert!(!vm.has_runnable_fibers());
        assert!(sink.take_bytes().is_empty());
        vm.spawn_call(validate, &[expected]).unwrap();
        assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Blocked);
        assert_eq!(sink.take_bytes(), b"POLL_OK\n");
        if sample + 1 == WARMUP {
            warm = Some(vm.jit_execution_stats());
        }
        if sample >= WARMUP {
            stable_compilation(warm.unwrap(), after);
            assert_eq!(
                memory_before.work_units_total, memory_after.work_units_total,
                "GC work leaked into the scheduler-only probe"
            );
            let entries = after.function_entries - before.function_entries;
            if busy && args[2] != "vm" {
                assert_eq!(entries, BUSY_BATCH as u64);
            } else {
                assert_eq!(entries, 0);
            }
            assert_eq!(before.loop_entries, after.loop_entries);
            measured_entries += entries;
            records.push(json!({"sample":sample-WARMUP,"elapsed_ns":elapsed_ns,
                "scheduler_turns":if busy {BUSY_BATCH} else {IDLE_BATCH},
                "parent_function_entries":entries,"state":expected}));
        }
    }
    let measured = vm.jit_execution_stats();
    if busy && args[2] == "optimizing" {
        // The only repeatedly invoked guest functions are PollProbe and
        // Validate; Configure/main/init execute below the four-call threshold.
        // Requiring exactly both hot functions prevents Validate alone from
        // being mistaken for proof that the measured probe reached that tier.
        // Each function has one baseline and one optimizing compilation.
        assert_eq!(measured.function_compilations, 4);
        assert_eq!(measured.optimizing_compilations, 2);
        assert_eq!(measured.optimizing_functions_executed, 2);
        assert_eq!(measured.optimizing_failures, 0);
    }
    if args[2] == "baseline" {
        assert_eq!(measured.optimizing_compilations, 0);
    }
    vm.spawn_call(release, &[]).unwrap();
    assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Completed);
    assert_eq!(sink.take_bytes(), b"POLL_DONE\n");
    // VM teardown joins child workers before publishing successful completion.
    drop(vm);
    let result = json!({"schema":"volang.island-poll.v1","vob_sha256":vob_sha256,
        "mode":args[2],"children":children,"workload":args[4],"samples":samples,"warmup":WARMUP,
        "parent_function_entries":measured_entries,"function_compilations":measured.function_compilations,
        "optimizing_compilations":measured.optimizing_compilations,
        "optimizing_functions_executed":measured.optimizing_functions_executed,
        "scope":"Parent scheduler time only. Child creation, compilation, bytecode loading, host spawn, output validation and teardown are outside intervals. Fixed work batches, zero timed GC/compilation, checked guest checksum, strict warmed parent JIT entry proof; no claim about child compiled entries. All children retain independent heaps and threads.",
        "records":records});
    if args[6] == "-" {
        println!("{}", serde_json::to_string(&result).unwrap());
    } else {
        std::fs::write(&args[6], serde_json::to_vec_pretty(&result).unwrap()).unwrap();
    }
}
