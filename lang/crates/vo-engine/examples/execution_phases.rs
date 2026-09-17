//! Warm execution intervals and separate opt-in interpreter/scheduler work.
//! Compile/serialize inputs before invoking this process. Work builds contain
//! counters; timing builds do not enable the VM execution-profile feature.
use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use std::time::Instant;
use vo_engine::{CaptureSink, Module};
use vo_vm::vm::{JitConfig, JitExecutionStats, SchedulingOutcome, Vm};

const WORK: bool = cfg!(feature = "execution-profile");
const WARMUP: usize = 64;

fn start() -> Option<Instant> {
    (!WORK).then(Instant::now)
}
fn elapsed(start: Option<Instant>) -> Option<u128> {
    start.map(|time| time.elapsed().as_nanos())
}

fn stable_compilation(before: JitExecutionStats, after: JitExecutionStats) {
    assert_eq!(before.function_compilations, after.function_compilations);
    assert_eq!(before.loop_compilations, after.loop_compilations);
    assert_eq!(
        before.optimizing_compilations,
        after.optimizing_compilations
    );
    assert_eq!(before.optimizing_failures, after.optimizing_failures);
    assert_eq!(before.compiled_code_bytes, after.compiled_code_bytes);
}

#[cfg(feature = "execution-profile")]
fn work(vm: &Vm) -> Value {
    use vo_common_core::{execution_effects::opcode_may_allocate, instruction::Opcode};
    let p = vm.execution_profile();
    let allocating: u64 = p
        .opcode_counts
        .iter()
        .enumerate()
        .filter(|(opcode, _)| opcode_may_allocate(Opcode::from_u8(*opcode as u8)))
        .map(|(_, count)| *count)
        .sum();
    assert_eq!(p.allocation_checks, allocating + p.allocation_yields);
    assert_eq!(
        p.execution_slices,
        p.timeslice_expirations
            + p.boundary_transitions
            + p.completed_fibers
            + p.other_slice_results
    );
    json!({
        "opcode_counts": p.opcode_counts.as_slice(),
        "instruction_dispatches": p.instruction_count(),
        "nonallocating_dispatches": p.instruction_count() - allocating,
        "interpreter_entries": p.interpreter_entries,
        "frame_refetch_attempts": p.frame_refetch_attempts,
        "allocation_checks": p.allocation_checks,
        "allocation_retries": p.allocation_retries,
        "allocation_yields": p.allocation_yields,
        "queue_continues": p.queue_continues,
        "queue_blocks": p.queue_blocks,
        "queue_transitions": p.queue_transitions,
        "execution_slices": p.execution_slices,
        "timeslice_expirations": p.timeslice_expirations,
        "boundary_transitions": p.boundary_transitions,
        "completed_fibers": p.completed_fibers,
        "other_slice_results": p.other_slice_results,
    })
}
#[cfg(not(feature = "execution-profile"))]
fn work(_: &Vm) -> Value {
    Value::Null
}

fn main() {
    assert_eq!(std::env::var("VOWORK").as_deref(), Ok("off"));
    let args: Vec<_> = std::env::args().collect();
    assert_eq!(
        args.len(),
        6,
        "usage: execution_phases INPUT.vob MODE WORKLOAD SAMPLES timing|work"
    );
    assert_eq!(args[5], if WORK { "work" } else { "timing" });
    let samples: usize = args[4].parse().unwrap();
    assert!((1..=128).contains(&samples));
    let bytes = std::fs::read(&args[1]).unwrap();
    let vob_sha256 = format!("{:x}", Sha256::digest(&bytes));
    let timer = start();
    let payload = vo_common_core::serialize::read_vob_file(std::path::Path::new(&args[1])).unwrap();
    let module = Module::deserialize(&payload).unwrap();
    let decoding_ns = elapsed(timer);
    let ids: Vec<_> = module
        .functions
        .iter()
        .enumerate()
        .filter(|(_, f)| f.name == args[3] || f.name.ends_with(&format!(".{}", args[3])))
        .map(|(id, _)| id as u32)
        .collect();
    assert_eq!(ids.len(), 1, "missing or ambiguous execution fixture");
    assert!([
        "Arithmetic",
        "Calls",
        "Maps",
        "Buffered",
        "Rendezvous",
        "Select",
        "Tasks"
    ]
    .contains(&args[3].as_str()));
    let static_instructions: usize = module.functions.iter().map(|f| f.code.len()).sum();
    let timer = start();
    let mut vm = match args[2].as_str() {
        "vm" => Vm::new(),
        "baseline" | "optimizing" | "osr" => Vm::try_with_jit_config(JitConfig {
            call_threshold: if args[2] == "osr" { u32::MAX } else { 4 },
            loop_threshold: if args[2] == "osr" { 1 } else { u32::MAX },
            optimizing_threshold: if args[2] == "optimizing" {
                16
            } else {
                u64::MAX
            },
            ..Default::default()
        })
        .unwrap(),
        _ => panic!("unknown mode"),
    };
    let construction_ns = elapsed(timer);
    let sink = CaptureSink::new();
    vm.set_output_sink(sink.clone());
    let timer = start();
    vm.load(module).unwrap();
    let loading_ns = elapsed(timer);
    let timer = start();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::Completed);
    let initialization_ns = elapsed(timer);
    assert!(sink.take_bytes().is_empty());
    for _ in 0..WARMUP {
        vm.spawn_call(ids[0], &[]).unwrap();
        assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Completed);
        assert_eq!(sink.take_bytes(), b"EXEC_OK\n");
    }
    vm.gc_collect().unwrap();
    let warmed = vm.jit_execution_stats();
    if args[2] != "vm" {
        assert!(warmed.executed_jit_code());
    }
    // Blocking workloads may disable a low-progress entry before promotion.
    // Keep their actual tier coverage visible instead of forcing a threshold
    // that would hide the runtime's feedback decision.
    let mut records = Vec::with_capacity(samples);
    for iteration in 0..samples {
        vm.spawn_call(ids[0], &[]).unwrap();
        #[cfg(feature = "execution-profile")]
        vm.reset_execution_profile();
        let before_jit = vm.jit_execution_stats();
        let before_memory = vm.memory_stats();
        let timer = start();
        assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Completed);
        let elapsed_ns = elapsed(timer);
        let after_jit = vm.jit_execution_stats();
        let after_memory = vm.memory_stats();
        assert_eq!(sink.take_bytes(), b"EXEC_OK\n");
        stable_compilation(warmed, after_jit);
        let work = work(&vm);
        if WORK {
            assert!(work["execution_slices"].as_u64().unwrap() > 0);
            if args[2] == "vm" {
                assert!(work["instruction_dispatches"].as_u64().unwrap() > 0);
            }
        }
        records.push(json!({
            "iteration": iteration, "elapsed_ns": elapsed_ns, "work": work,
            "jit": {
                "function_entries": after_jit.function_entries - before_jit.function_entries,
                "loop_entries": after_jit.loop_entries - before_jit.loop_entries,
                "closure_prepares": after_jit.closure_prepare_callbacks - before_jit.closure_prepare_callbacks,
                "interface_prepares": after_jit.iface_prepare_callbacks - before_jit.iface_prepare_callbacks,
                "ic_publications": after_jit.dynamic_ic_publications - before_jit.dynamic_ic_publications,
                "feedback_disabled_exits": after_jit.side_exit_reasons.get(vo_vm::vm::JitSideExitReason::InterpretedFeedbackDisabled)
                    - before_jit.side_exit_reasons.get(vo_vm::vm::JitSideExitReason::InterpretedFeedbackDisabled),
            },
            "memory": {
                "allocation_bytes": after_memory.allocation_bytes_total - before_memory.allocation_bytes_total,
                "gc_work_units": after_memory.work_units_total - before_memory.work_units_total,
                "minor_cycles": after_memory.minor_cycles - before_memory.minor_cycles,
                "major_cycles": after_memory.major_cycles - before_memory.major_cycles,
                "committed_bytes": after_memory.managed_committed_bytes,
                "live_bytes": after_memory.managed_live_bytes,
            },
        }));
    }
    println!(
        "{}",
        json!({
            "schema": "volang.execution-phases.v1", "work_diagnostics": WORK,
            "mode": args[2], "workload": args[3], "samples": samples, "warmup": WARMUP,
            "vob_sha256": vob_sha256, "vob_bytes": bytes.len(),
            "static_instructions": static_instructions,
            "warmup_jit": {
                "function_entries": warmed.function_entries,
                "loop_entries": warmed.loop_entries,
                "function_compilations": warmed.function_compilations,
                "loop_compilations": warmed.loop_compilations,
                "optimizing_compilations": warmed.optimizing_compilations,
                "optimizing_functions_executed": warmed.optimizing_functions_executed,
                "low_progress_function_disables": warmed.low_progress_function_disables,
                "low_progress_loop_disables": warmed.low_progress_loop_disables,
            },
            "stages": {"decoding_ns": decoding_ns, "construction_ns": construction_ns,
                "loading_ns": loading_ns, "initialization_ns": initialization_ns},
            "records": records,
        })
    );
}
