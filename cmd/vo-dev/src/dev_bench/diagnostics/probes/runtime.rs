//! Strict contracts for the maintained layout, Island and GC-root examples.
use super::*;

fn unsigned(value: &Value, key: &str) -> Result<u64> {
    value[key]
        .as_u64()
        .ok_or_else(|| anyhow!("missing unsigned runtime diagnostic field {key}"))
}

fn records<'a>(value: &'a Value, name: &str, count: u64, index: &str) -> Result<&'a Vec<Value>> {
    let rows = value[name]
        .as_array()
        .ok_or_else(|| anyhow!("missing runtime diagnostic records"))?;
    if rows.len() as u64 != count
        || rows
            .iter()
            .enumerate()
            .any(|(i, r)| r[index].as_u64() != Some(i as u64))
    {
        bail!("incomplete or reordered runtime diagnostic records");
    }
    Ok(rows)
}

pub(super) fn validate(artifact: &Artifact, value: &Value) -> Result<()> {
    match artifact.probe {
        Probe::Layouts => {
            if value["schema"] != "volang.layout-resources.v1"
                || value["allocation_diagnostics"] != artifact.allocations
                || !matches!(
                    value["view"].as_str(),
                    Some("pointers" | "elements" | "both")
                )
                || unsigned(value, "vob_bytes")? == 0
            {
                bail!("layout diagnostic identity mismatch");
            }
            let rows = records(value, "samples", LAYOUT_SAMPLES, "iteration")?;
            let digest = unsigned(&rows[0], "layout_digest")?;
            for row in rows {
                if unsigned(row, "layout_digest")? != digest {
                    bail!("layout facts changed between samples");
                }
                let retained = unsigned(row, "view_retained_bytes")?;
                if !artifact.allocations && retained != 0 {
                    bail!("layout counters leaked into timing build");
                }
                for stage in ["load", "clone"] {
                    let cost = &row[stage];
                    unsigned(cost, "elapsed_ns")?;
                    for key in ["allocation_requests", "requested_bytes", "peak_extra_bytes"] {
                        if unsigned(cost, key)? != 0 && !artifact.allocations {
                            bail!("layout counters leaked into timing build");
                        }
                    }
                    let live = cost["live_delta_bytes"]
                        .as_i64()
                        .ok_or_else(|| anyhow!("missing signed layout delta"))?;
                    if !artifact.allocations && live != 0 {
                        bail!("layout counters leaked into timing build");
                    }
                }
            }
        }
        Probe::Island => {
            if value["schema"] != "volang.island-poll.v1"
                || artifact.allocations
                || value["samples"] != ISLAND_SAMPLES
                || value["warmup"] != 128
                || ![0, 1, 8, 32, 128].contains(&unsigned(value, "children")?)
                || !matches!(
                    value["mode"].as_str(),
                    Some("vm" | "baseline" | "optimizing")
                )
                || !matches!(value["workload"].as_str(), Some("idle" | "busy"))
            {
                bail!("Island diagnostic identity mismatch");
            }
            let busy = value["workload"] == "busy";
            let entries = if busy && value["mode"] != "vm" { 32 } else { 0 };
            if unsigned(value, "parent_function_entries")? != entries * ISLAND_SAMPLES {
                bail!("Island measured entry count mismatch");
            }
            if busy
                && value["mode"] == "optimizing"
                && (value["function_compilations"] != 4
                    || value["optimizing_compilations"] != 2
                    || value["optimizing_functions_executed"] != 2)
            {
                bail!("Island probe did not enter the optimizing tier");
            }
            if value["mode"] == "baseline" && value["optimizing_compilations"] != 0 {
                bail!("baseline Island probe entered the optimizing tier");
            }
            let mut state = 7_u64;
            let rows = records(value, "records", ISLAND_SAMPLES, "sample")?;
            for sample in 0..128 + ISLAND_SAMPLES {
                if busy {
                    for _ in 0..32 * 64 {
                        state = (state * 1664525 + 1013904223) & 2147483647;
                    }
                }
                if sample >= 128 {
                    let row = &rows[(sample - 128) as usize];
                    unsigned(row, "elapsed_ns")?;
                    if unsigned(row, "parent_function_entries")? != entries
                        || row["scheduler_turns"] != if busy { 32 } else { 4096 }
                        || unsigned(row, "state")? != state
                    {
                        bail!("Island work or checksum mismatch");
                    }
                }
            }
        }
        Probe::Roots => {
            if value["schema"] != "volang.gc-root-latency.v1"
                || artifact.allocations
                || value["samples"] != ROOT_SAMPLES
                || value["warmup"] != 128
                || !matches!(value["mode"].as_str(), Some("vm" | "jit"))
                || !matches!(
                    value["gc_mode"].as_str(),
                    Some("generational" | "incremental")
                )
                || !matches!(value["admission"].as_str(), Some("forced" | "control"))
            {
                bail!("GC-root diagnostic identity mismatch");
            }
            let entries = unsigned(value, "measured_function_entries")?;
            if value["mode"] == "jit" && entries < ROOT_SAMPLES {
                bail!("GC-root probe did not enter function JIT");
            }
            if value["mode"] == "vm"
                && (entries != 0 || unsigned(value, "measured_loop_entries")? != 0)
            {
                bail!("VM root probe entered native code");
            }
            let mut count = 0;
            for row in records(value, "records", ROOT_SAMPLES, "sample")? {
                let first = unsigned(row, "initial_step_ns")?;
                let initial_work = unsigned(row, "initial_completed_work_units")?;
                if initial_work > 1 || (value["admission"] == "control" && initial_work != 0) {
                    bail!("initial root collection exceeded the requested work");
                }
                if value["admission"] == "control"
                    && (first != 0 || !row["initial_step_root_work_bytes"].is_null())
                {
                    bail!("control root probe requested a collection");
                }
                let turns = row["turns"]
                    .as_array()
                    .ok_or_else(|| anyhow!("missing root host turns"))?;
                count += turns.len();
                if turns.is_empty() || count > 1_000_000 {
                    bail!("incomplete or excessive root host turns");
                }
                let mut active = first;
                let mut work = 0_u64;
                for (index, turn) in turns.iter().enumerate() {
                    let last = index + 1 == turns.len();
                    if unsigned(turn, "turn")? != index as u64
                        || turn["probe_observed"] != last
                        || !matches!(
                            turn["outcome"].as_str(),
                            Some("Blocked" | "Suspended" | "SuspendedForHostEvents")
                        )
                    {
                        bail!(
                            "root host-turn sequence did not reach exactly one guest observation"
                        );
                    }
                    active = active
                        .checked_add(unsigned(turn, "host_return_ns")?)
                        .ok_or_else(|| anyhow!("root timer overflow"))?;
                    work = work
                        .checked_add(unsigned(turn, "work_units")?)
                        .ok_or_else(|| anyhow!("root work overflow"))?;
                }
                if active != unsigned(row, "guest_resume_active_ns")?
                    || active > unsigned(row, "guest_resume_wall_ns")?
                    || unsigned(row, "work_units")?.checked_sub(work) != Some(initial_work)
                {
                    bail!("root timer or collector work attribution mismatch");
                }
                for key in [
                    "managed_live_bytes",
                    "completed_minor_cycles",
                    "completed_major_cycles",
                    "before_young_live_bytes",
                    "before_old_live_bytes",
                ] {
                    unsigned(row, key)?;
                }
            }
        }
        _ => unreachable!(),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn artifact(probe: Probe) -> Artifact {
        Artifact {
            probe,
            allocations: false,
            features: probe.features().iter().map(|s| (*s).into()).collect(),
            file: String::new(),
            sha256: String::new(),
        }
    }

    fn roots() -> Value {
        json!({"schema":"volang.gc-root-latency.v1", "mode":"jit", "gc_mode":"incremental", "admission":"forced", "warmup":128,
            "samples":ROOT_SAMPLES, "measured_function_entries":ROOT_SAMPLES, "measured_loop_entries":0,
            "records":(0..ROOT_SAMPLES).map(|sample| json!({"sample":sample,"initial_step_ns":2,"initial_completed_work_units":1,
                "guest_resume_active_ns":12,"guest_resume_wall_ns":15,"work_units":2,"managed_live_bytes":8,
                "completed_minor_cycles":0,"completed_major_cycles":0,"before_young_live_bytes":8,"before_old_live_bytes":0,
                "turns":[{"turn":0,"probe_observed":true,"outcome":"Blocked","host_return_ns":10,"work_units":1}]})).collect::<Vec<_>>()})
    }

    #[test]
    fn root_contract_rejects_missing_compiled_entries_observations_and_unaccounted_work() {
        let a = artifact(Probe::Roots);
        let valid = roots();
        assert!(validate(&a, &valid).is_ok());
        for (key, replacement) in [
            ("measured_function_entries", json!(0)),
            ("samples", json!(1)),
        ] {
            let mut bad = valid.clone();
            bad[key] = replacement;
            assert!(validate(&a, &bad).is_err());
        }
        let mut bad = valid.clone();
        bad["records"][0]["turns"][0]["probe_observed"] = json!(false);
        assert!(validate(&a, &bad).is_err());
        let mut bad = valid.clone();
        bad["records"][0]["work_units"] = json!(1);
        assert!(validate(&a, &bad).is_err());
        let mut bad = valid;
        bad["records"][0]["guest_resume_active_ns"] = json!(10);
        assert!(validate(&a, &bad).is_err());
    }

    #[test]
    fn layout_contract_rejects_partial_samples_stale_facts_and_counter_leaks() {
        let cost = json!({"elapsed_ns":5,"allocation_requests":0,"requested_bytes":0,"live_delta_bytes":0,"peak_extra_bytes":0});
        let valid = json!({"schema":"volang.layout-resources.v1","allocation_diagnostics":false,"view":"both","vob_bytes":200,
            "samples":(0..LAYOUT_SAMPLES).map(|iteration|json!({"iteration":iteration,"layout_digest":7,"view_retained_bytes":0,"load":cost,"clone":cost})).collect::<Vec<_>>()});
        let a = artifact(Probe::Layouts);
        assert!(validate(&a, &valid).is_ok());
        let mut bad = valid.clone();
        bad["samples"].as_array_mut().unwrap().pop();
        assert!(validate(&a, &bad).is_err());
        let mut bad = valid.clone();
        bad["samples"][1]["layout_digest"] = json!(8);
        assert!(validate(&a, &bad).is_err());
        let mut bad = valid;
        bad["samples"][0]["load"]["allocation_requests"] = json!(1);
        assert!(validate(&a, &bad).is_err());
    }

    #[test]
    fn island_contract_rejects_changed_work_and_missing_native_entries() {
        let valid = json!({"schema":"volang.island-poll.v1","mode":"vm","children":128,"workload":"idle","samples":ISLAND_SAMPLES,
            "warmup":128,"parent_function_entries":0,"function_compilations":0,"optimizing_compilations":0,"optimizing_functions_executed":0,
            "records":(0..ISLAND_SAMPLES).map(|sample|json!({"sample":sample,"elapsed_ns":100,"scheduler_turns":4096,"parent_function_entries":0,"state":7})).collect::<Vec<_>>()});
        let a = artifact(Probe::Island);
        assert!(validate(&a, &valid).is_ok());
        let mut bad = valid.clone();
        bad["records"][0]["scheduler_turns"] = json!(1);
        assert!(validate(&a, &bad).is_err());
        let mut bad = valid.clone();
        bad["records"][0]["state"] = json!(8);
        assert!(validate(&a, &bad).is_err());
        let mut bad = valid;
        bad["mode"] = json!("optimizing");
        bad["workload"] = json!("busy");
        assert!(validate(&a, &bad).is_err());
    }
}
