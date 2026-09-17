//! The real Engine probe's complete workload and exclusive-accounting contract.
use anyhow::{anyhow, bail, Result};
use serde_json::Value;
use std::collections::BTreeSet;

const PHASES: [&str; 13] = [
    "source_map",
    "lex_parse",
    "import_resolution",
    "type_check",
    "escape_capture",
    "sendability",
    "input_capture",
    "input_fingerprint",
    "cache_lookup",
    "cache_publish",
    "codegen",
    "verification",
    "bytecode_decode",
];
const ALLOCATOR: [&str; 3] = ["allocations", "reallocations", "requested_bytes"];

fn unsigned(value: &Value, key: &str) -> Result<u64> {
    value[key]
        .as_u64()
        .ok_or_else(|| anyhow!("missing unsigned pipeline field {key}"))
}

fn hash(value: &Value) -> bool {
    value.as_str().is_some_and(|s| {
        s.len() == 64
            && s.bytes()
                .all(|b| b.is_ascii_hexdigit() && !b.is_ascii_uppercase())
    })
}

pub(super) fn validate(value: &Value, allocations: bool) -> Result<()> {
    if !value["reference_compiler"].is_null() {
        bail!("cross-binary equivalence mode cannot enter diagnostic timing samples");
    }
    if value["compiler_profile"] != true {
        bail!("pipeline probe requires compiler-profile instrumentation");
    }
    // Snapshots created before explicit profile schemas retain all 13 original
    // phases. New snapshots add context ownership without invalidating replay.
    let context_phases = match value["compiler_profile_schema"].as_str() {
        None if value["compiler_profile_schema"].is_null() => false,
        Some("volang.compiler-profile.v2") => true,
        _ => bail!("unsupported compiler profile schema"),
    };
    let mut expected_phases = PHASES.into_iter().collect::<BTreeSet<_>>();
    if context_phases {
        expected_phases.extend(["workspace_context", "snapshot_context"]);
    }
    let rows = value["samples"]
        .as_array()
        .ok_or_else(|| anyhow!("missing pipeline samples"))?;
    let mut seen = BTreeSet::new();
    for row in rows {
        let name = row["case"]
            .as_str()
            .ok_or_else(|| anyhow!("missing pipeline workload"))?;
        let (packages, parts, functions) = match name {
            "small" => (1_u64, 1_u64, 4_u64),
            "medium" => (8, 4, 4),
            "large" => (32, 4, 8),
            _ => bail!("unknown pipeline workload"),
        };
        let scenario = row["scenario"]
            .as_str()
            .ok_or_else(|| anyhow!("missing pipeline scenario"))?;
        let iteration = unsigned(row, "iteration")?;
        if !["cache-miss", "cache-hit", "changed-file"].contains(&scenario)
            || iteration >= 5
            || !seen.insert((name, scenario, iteration))
            || row["project_packages"] != packages + 1
            || row["project_source_files"] != packages * parts + 1
            || row["project_functions"] != packages * parts * functions + 1
            || row["missed_allocator_events"] != 0
        {
            bail!("invalid or duplicate pipeline workload");
        }
        let expected = 4 * packages * (packages - 1)
            + 28 * packages
            + if scenario == "changed-file" {
                iteration + 101
            } else {
                0
            };
        if row["output"] != format!("{expected}\n") || !hash(&row["vob_sha256"]) {
            bail!("pipeline output or bytecode identity mismatch");
        }
        for key in [
            "module_functions",
            "instructions",
            "frame_slots",
            "vob_bytes",
        ] {
            if unsigned(row, key)? == 0 {
                bail!("empty pipeline product metric {key}");
            }
        }
        let sources = row["source_manifest"]
            .as_object()
            .ok_or_else(|| anyhow!("missing pipeline source identity"))?;
        if sources.len() as u64 != packages * parts + 2
            || !sources.contains_key("vo.mod")
            || !sources.contains_key("main.vo")
        {
            bail!("incomplete pipeline source identity");
        }
        for identity in sources.values() {
            let identity = identity
                .as_array()
                .ok_or_else(|| anyhow!("invalid pipeline source identity"))?;
            if identity.len() != 2 || !hash(&identity[0]) || identity[1].as_u64().is_none() {
                bail!("invalid pipeline source digest or modification time");
            }
        }
        let phases = row["phases"]
            .as_object()
            .ok_or_else(|| anyhow!("missing pipeline phases"))?;
        if phases.keys().map(String::as_str).collect::<BTreeSet<_>>() != expected_phases {
            bail!("pipeline phase set changed or is incomplete");
        }
        if context_phases {
            for phase in ["workspace_context", "snapshot_context"] {
                if unsigned(&phases[phase], "calls")? == 0 {
                    bail!("pipeline context phase was not captured");
                }
            }
        }
        let cache_hit = scenario == "cache-hit";
        let parsed_files = unsigned(row, "parsed_files")?;
        let parsed_bytes = unsigned(row, "parsed_bytes")?;
        if cache_hit && (parsed_files != 0 || parsed_bytes != 0)
            || !cache_hit && (parsed_files < packages * parts + 1 || parsed_bytes == 0)
        {
            bail!("pipeline parser work does not match cache outcome");
        }
        for phase in ["lex_parse", "type_check", "codegen"] {
            if (unsigned(&phases[phase], "calls")? != 0) == cache_hit {
                bail!("pipeline cache hit/miss phase contract failed");
            }
        }
        let mut accounted_ns = unsigned(row, "unattributed_ns")?;
        let mut counts = [0_u64; 3];
        for (index, key) in ALLOCATOR.into_iter().enumerate() {
            counts[index] = unsigned(&row["unattributed_allocator"], key)?;
        }
        for phase in phases.values() {
            let calls = unsigned(phase, "calls")?;
            let ns = unsigned(phase, "exclusive_ns")?;
            if calls == 0 && ns != 0 {
                bail!("uncalled pipeline phase has time");
            }
            accounted_ns = accounted_ns
                .checked_add(ns)
                .ok_or_else(|| anyhow!("pipeline time overflow"))?;
            for (index, key) in ALLOCATOR.into_iter().enumerate() {
                let count = unsigned(&phase["allocator"], key)?;
                if calls == 0 && count != 0 {
                    bail!("uncalled pipeline phase has allocations");
                }
                counts[index] = counts[index]
                    .checked_add(count)
                    .ok_or_else(|| anyhow!("pipeline allocation overflow"))?;
            }
        }
        if accounted_ns != unsigned(row, "elapsed_ns")? {
            bail!("exclusive pipeline time does not sum to total");
        }
        for (index, key) in ALLOCATOR.into_iter().enumerate() {
            let total = unsigned(&row["allocator"], key)?;
            if counts[index] != total || !allocations && total != 0 {
                bail!("pipeline allocation accounting or build mismatch");
            }
        }
        if allocations && unsigned(&row["allocator"], "allocations")? == 0 {
            bail!("allocation pipeline build did not record allocation requests");
        }
    }
    if seen.len() != 45 {
        bail!("incomplete pipeline diagnostic matrix");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn fixture() -> Value {
        let mut rows = Vec::new();
        for (name, packages, parts, functions) in
            [("small", 1, 1, 4), ("medium", 8, 4, 4), ("large", 32, 4, 8)]
        {
            for scenario in ["cache-miss", "cache-hit", "changed-file"] {
                for iteration in 0..5 {
                    let active = scenario != "cache-hit";
                    let mut sources = serde_json::Map::new();
                    for name in ["vo.mod", "main.vo"] {
                        sources.insert(name.into(), json!(["a".repeat(64), 1]));
                    }
                    for package in 0..packages {
                        for part in 0..parts {
                            sources.insert(
                                format!("p{package}/part{part}.vo"),
                                json!(["a".repeat(64), 1]),
                            );
                        }
                    }
                    let zero = json!({"allocations":0,"reallocations":0,"requested_bytes":0});
                    let phases: serde_json::Map<_, _> = PHASES.into_iter().map(|phase| (phase.into(),
                        json!({"calls": if active {1} else {0}, "exclusive_ns":if active {1} else {0}, "allocator":zero}))).collect();
                    let expected = 4 * packages * (packages - 1)
                        + 28 * packages
                        + if scenario == "changed-file" {
                            iteration + 101
                        } else {
                            0
                        };
                    rows.push(json!({"case":name,"scenario":scenario,"iteration":iteration,
                        "project_packages":packages+1,"project_source_files":packages*parts+1,
                        "project_functions":packages*parts*functions+1,"missed_allocator_events":0,
                        "output":format!("{expected}\n"),"vob_sha256":"b".repeat(64),
                        "module_functions":1,"instructions":1,"frame_slots":1,"vob_bytes":1,
                        "source_manifest":sources,"phases":phases,
                        "parsed_files":if active {packages*parts+1} else {0},"parsed_bytes":if active {1} else {0},
                        "elapsed_ns":if active {20} else {7},"unattributed_ns":7,
                        "allocator":zero,"unattributed_allocator":zero}));
                }
            }
        }
        json!({"compiler_profile":true,"samples":rows})
    }

    #[test]
    fn rejects_incomplete_accounting_and_work_that_leaks_into_a_cache_hit() {
        let valid = fixture();
        validate(&valid, false).unwrap();
        let mut changed = valid.clone();
        changed["samples"][0]["phases"]
            .as_object_mut()
            .unwrap()
            .remove("input_capture");
        assert!(validate(&changed, false).is_err());
        let mut changed = valid.clone();
        changed["samples"][0]["unattributed_ns"] = json!(8);
        assert!(validate(&changed, false).is_err());
        let mut changed = valid.clone();
        changed["samples"][5]["phases"]["type_check"]["calls"] = json!(1);
        assert!(validate(&changed, false).is_err());
        let mut changed = valid.clone();
        changed["samples"][0]["allocator"]["allocations"] = json!(1);
        changed["samples"][0]["unattributed_allocator"]["allocations"] = json!(1);
        assert!(validate(&changed, false).is_err());
        let mut changed = valid;
        changed["samples"].as_array_mut().unwrap().pop();
        assert!(validate(&changed, false).is_err());
    }
    #[test]
    fn new_context_schema_preserves_legacy_replay_and_requires_complete_attribution() {
        let legacy = fixture();
        validate(&legacy, false).unwrap();
        let mut current = legacy;
        current["compiler_profile_schema"] = json!("volang.compiler-profile.v2");
        assert!(validate(&current, false).is_err());
        for row in current["samples"].as_array_mut().unwrap() {
            for name in ["workspace_context", "snapshot_context"] {
                row["phases"][name] = json!({"calls":1,"exclusive_ns":1,
                    "allocator":{"allocations":0,"reallocations":0,"requested_bytes":0}});
            }
            row["unattributed_ns"] = json!(5);
        }
        validate(&current, false).unwrap();
        current["samples"][0]["phases"]["snapshot_context"]["calls"] = json!(0);
        assert!(validate(&current, false).is_err());
        current["compiler_profile_schema"] = json!("future");
        assert!(validate(&current, false).is_err());
    }
}
