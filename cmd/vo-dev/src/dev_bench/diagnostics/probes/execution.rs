//! Positive-count execution observations; instrumented clocks are never admitted.
use super::*;
use std::collections::BTreeSet;
use vo_common_core::{execution_effects::opcode_may_allocate, instruction::Opcode};

fn unsigned(value: &Value, key: &str) -> Result<u64> {
    value[key]
        .as_u64()
        .ok_or_else(|| anyhow!("missing execution count {key}"))
}

pub(super) fn validate(artifact: &Artifact, data: &Value) -> Result<()> {
    if data["schema"] != "volang.execution-phases.v1"
        || data["work_diagnostics"] != artifact.allocations
        || data["samples"] != 32
        || data["warmup"] != 64
        || !matches!(
            data["mode"].as_str(),
            Some("vm" | "baseline" | "optimizing" | "osr")
        )
        || !matches!(
            data["workload"].as_str(),
            Some("Arithmetic" | "Calls" | "Maps" | "Buffered" | "Rendezvous" | "Select" | "Tasks")
        )
        || unsigned(data, "static_instructions")? == 0
    {
        bail!("execution probe identity or shape mismatch");
    }
    let warmup = &data["warmup_jit"];
    for key in [
        "function_entries",
        "loop_entries",
        "function_compilations",
        "loop_compilations",
        "optimizing_compilations",
        "optimizing_functions_executed",
        "low_progress_function_disables",
        "low_progress_loop_disables",
    ] {
        unsigned(warmup, key)?;
    }
    let entered =
        unsigned(warmup, "function_entries")? != 0 || unsigned(warmup, "loop_entries")? != 0;
    if entered == (data["mode"] == "vm") {
        bail!("configured execution mode lacks matching native coverage");
    }
    if unsigned(warmup, "optimizing_functions_executed")?
        > unsigned(warmup, "optimizing_compilations")?
    {
        bail!("optimized function coverage exceeds published compilations");
    }
    for phase in [
        "decoding_ns",
        "construction_ns",
        "loading_ns",
        "initialization_ns",
    ] {
        let value = &data["stages"][phase];
        if if artifact.allocations {
            !value.is_null()
        } else {
            value.as_u64().is_none()
        } {
            bail!("execution phase clock/counter build mismatch");
        }
    }
    let rows = data["records"]
        .as_array()
        .ok_or_else(|| anyhow!("missing execution records"))?;
    let mut seen = BTreeSet::new();
    for row in rows {
        let iteration = unsigned(row, "iteration")?;
        if iteration >= 32 || !seen.insert(iteration) {
            bail!("duplicate or invalid execution iteration");
        }
        for key in [
            "function_entries",
            "loop_entries",
            "closure_prepares",
            "interface_prepares",
            "ic_publications",
            "feedback_disabled_exits",
        ] {
            unsigned(&row["jit"], key)?;
        }
        if data["mode"] == "vm"
            && (row["jit"]["function_entries"] != 0 || row["jit"]["loop_entries"] != 0)
        {
            bail!("VM-only observation entered native code");
        }
        for key in [
            "allocation_bytes",
            "gc_work_units",
            "minor_cycles",
            "major_cycles",
            "committed_bytes",
            "live_bytes",
        ] {
            unsigned(&row["memory"], key)?;
        }
        if !artifact.allocations {
            unsigned(row, "elapsed_ns")?;
            if !row["work"].is_null() {
                bail!("work counters leaked into timing executable");
            }
            continue;
        }
        if !row["elapsed_ns"].is_null() {
            bail!("instrumented execution clock was reported");
        }
        let work = &row["work"];
        let histogram = work["opcode_counts"]
            .as_array()
            .filter(|v| v.len() == 256)
            .ok_or_else(|| anyhow!("missing opcode histogram"))?;
        let mut sum = 0_u64;
        let mut allocating = 0_u64;
        for (index, value) in histogram.iter().enumerate() {
            let count = value
                .as_u64()
                .ok_or_else(|| anyhow!("invalid opcode count"))?;
            let opcode = Opcode::from_u8(index as u8);
            if count != 0 && opcode == Opcode::Invalid {
                bail!("invalid opcode was dispatched");
            }
            if opcode_may_allocate(opcode) {
                allocating = allocating
                    .checked_add(count)
                    .ok_or_else(|| anyhow!("allocation histogram overflow"))?;
            }
            sum = sum
                .checked_add(count)
                .ok_or_else(|| anyhow!("opcode histogram total overflow"))?;
        }
        let dispatches = unsigned(work, "instruction_dispatches")?;
        let nonallocating = unsigned(work, "nonallocating_dispatches")?;
        let checks = unsigned(work, "allocation_checks")?;
        let yields = unsigned(work, "allocation_yields")?;
        let retries = unsigned(work, "allocation_retries")?;
        if sum != dispatches
            || nonallocating != sum - allocating
            || retries > checks
            || checks.checked_sub(yields) != Some(dispatches - nonallocating)
            || (data["mode"] == "vm" && dispatches == 0)
        {
            bail!("inconsistent interpreter work counts");
        }
        let mut slices = 0_u64;
        for key in [
            "timeslice_expirations",
            "boundary_transitions",
            "completed_fibers",
            "other_slice_results",
        ] {
            slices = slices
                .checked_add(unsigned(work, key)?)
                .ok_or_else(|| anyhow!("slice total overflow"))?;
        }
        if slices == 0 || slices != unsigned(work, "execution_slices")? {
            bail!("execution slice accounting mismatch");
        }
        for key in [
            "interpreter_entries",
            "frame_refetch_attempts",
            "queue_continues",
            "queue_blocks",
            "queue_transitions",
        ] {
            unsigned(work, key)?;
        }
    }
    if seen.len() != 32 {
        bail!("incomplete execution observations");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn artifact(counters: bool) -> Artifact {
        Artifact {
            probe: Probe::Execution,
            allocations: counters,
            features: Probe::Execution.artifact_features(counters),
            file: String::new(),
            sha256: String::new(),
        }
    }
    fn valid(counters: bool) -> Value {
        let mut histogram = vec![0_u64; 256];
        histogram[Opcode::LoadInt as usize] = 8;
        histogram[Opcode::StrNew as usize] = 2;
        let work = json!({"opcode_counts": histogram, "instruction_dispatches": 10,
            "nonallocating_dispatches": 8, "allocation_checks": 3, "allocation_yields": 1,
            "allocation_retries": 1, "interpreter_entries": 2, "frame_refetch_attempts": 3,
            "queue_continues": 0, "queue_blocks": 0, "queue_transitions": 0,
            "execution_slices": 2, "timeslice_expirations": 0, "boundary_transitions": 1,
            "completed_fibers": 1, "other_slice_results": 0});
        let clock = if counters { Value::Null } else { json!(123) };
        json!({"schema":"volang.execution-phases.v1", "work_diagnostics":counters,
            "mode":"vm", "workload":"Arithmetic", "samples":32, "warmup":64,
            "warmup_jit":{"function_entries":0,"loop_entries":0,"function_compilations":0,
                "loop_compilations":0,"optimizing_compilations":0,"optimizing_functions_executed":0,
                "low_progress_function_disables":0,"low_progress_loop_disables":0},
            "static_instructions":19, "stages":{"decoding_ns":clock,"construction_ns":clock,"loading_ns":clock,"initialization_ns":clock},
            "records": (0..32).map(|iteration| json!({"iteration":iteration,"elapsed_ns":clock,
                "work":if counters {work.clone()} else {Value::Null},
                "jit":{"function_entries":0,"loop_entries":0,"closure_prepares":0,"interface_prepares":0,"ic_publications":0,"feedback_disabled_exits":0},
                "memory":{"allocation_bytes":0,"gc_work_units":0,"minor_cycles":0,"major_cycles":0,"committed_bytes":65536,"live_bytes":64}})).collect::<Vec<_>>()})
    }
    #[test]
    fn rejects_empty_counter_runs_and_instrumented_clocks() {
        let a = artifact(true);
        let mut v = valid(true);
        assert!(validate(&a, &v).is_ok());
        v["records"][0]["elapsed_ns"] = json!(1);
        assert!(validate(&a, &v).is_err());
        v = valid(true);
        v["records"][0]["work"]["instruction_dispatches"] = json!(0);
        assert!(validate(&a, &v).is_err());
        v = valid(true);
        v["records"][0]["work"]["execution_slices"] = json!(0);
        assert!(validate(&a, &v).is_err());
        v = valid(true);
        v["records"][1]["iteration"] = json!(0);
        assert!(validate(&a, &v).is_err());
        v = valid(true);
        v["records"][0]["work"]["opcode_counts"][Opcode::StrNew as usize] = json!(0);
        v["records"][0]["work"]["opcode_counts"][Opcode::LoadInt as usize] = json!(10);
        assert!(validate(&a, &v).is_err());
    }
    #[test]
    fn rejects_counter_state_in_timing_runs() {
        let a = artifact(false);
        let mut v = valid(false);
        assert!(validate(&a, &v).is_ok());
        v["records"][0]["work"] = json!({});
        assert!(validate(&a, &v).is_err());
        v = valid(false);
        v["work_diagnostics"] = json!(true);
        assert!(validate(&a, &v).is_err());
    }

    #[test]
    fn records_feedback_fallback_without_claiming_optimized_execution() {
        let a = artifact(true);
        let mut v = valid(true);
        v["mode"] = json!("optimizing");
        assert!(validate(&a, &v).is_err());
        v["warmup_jit"]["function_entries"] = json!(4);
        v["warmup_jit"]["function_compilations"] = json!(1);
        v["warmup_jit"]["low_progress_function_disables"] = json!(1);
        assert!(validate(&a, &v).is_ok());
        v["warmup_jit"]["optimizing_functions_executed"] = json!(1);
        assert!(validate(&a, &v).is_err());
        v["warmup_jit"]["optimizing_compilations"] = json!(1);
        assert!(validate(&a, &v).is_ok());
    }
}
