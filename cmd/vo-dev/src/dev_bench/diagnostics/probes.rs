//! Probe workloads and strict output contracts.
use super::inputs::Input;
use super::{Artifact, COUNTERS};
use anyhow::{anyhow, bail, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeSet;

mod cases;
mod execution;
mod pipeline;
mod runtime;
pub(super) use cases::Case;
pub(super) const LAYOUT_SAMPLES: u64 = 25;
pub(super) const ISLAND_SAMPLES: u64 = 256;
pub(super) const ROOT_SAMPLES: u64 = 512;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(super) enum Probe {
    Compile,
    Pipeline,
    Regions,
    Transfer,
    Recycling,
    Layouts,
    Island,
    Roots,
    Execution,
}

impl Probe {
    pub(super) fn all() -> [Self; 9] {
        [
            Self::Compile,
            Self::Pipeline,
            Self::Regions,
            Self::Transfer,
            Self::Recycling,
            Self::Layouts,
            Self::Island,
            Self::Roots,
            Self::Execution,
        ]
    }
    pub(super) fn name(self) -> &'static str {
        match self {
            Self::Compile => "compile",
            Self::Pipeline => "pipeline",
            Self::Regions => "regions",
            Self::Transfer => "transfer",
            Self::Recycling => "recycling",
            Self::Layouts => "layouts",
            Self::Island => "island",
            Self::Roots => "roots",
            Self::Execution => "execution",
        }
    }
    pub(super) fn example(self) -> &'static str {
        match self {
            Self::Compile => "compile_phases",
            Self::Pipeline => "compiler_pipeline",
            Self::Regions => "allocation_regions",
            Self::Transfer => "transfer_packets",
            Self::Recycling => "allocation_recycling",
            Self::Layouts => "layout_resources",
            Self::Island => "island_poll",
            Self::Roots => "gc_root_latency",
            Self::Execution => "execution_phases",
        }
    }
    pub(super) fn package(self) -> &'static str {
        match self {
            Self::Compile => "vo-codegen",
            Self::Pipeline | Self::Island | Self::Roots | Self::Execution => "vo-engine",
            Self::Layouts => "vo-common-core",
            _ => "vo-runtime",
        }
    }
    pub(super) fn features(self) -> &'static [&'static str] {
        match self {
            Self::Pipeline => &["compiler-profile"],
            Self::Island | Self::Roots | Self::Execution => &["jit"],
            _ => &[],
        }
    }
    pub(super) fn artifact_features(self, counters: bool) -> Vec<String> {
        let mut features: Vec<String> = self.features().iter().map(|&f| f.into()).collect();
        if self == Self::Execution && counters {
            features.push("execution-profile".into());
        }
        features
    }
    pub(super) fn counter(self) -> Option<&'static str> {
        match self {
            Self::Compile => Some(COUNTERS[0]),
            Self::Pipeline => Some(COUNTERS[3]),
            Self::Transfer => Some(COUNTERS[1]),
            Self::Recycling => Some(COUNTERS[2]),
            Self::Layouts => Some(COUNTERS[4]),
            Self::Execution => Some(COUNTERS[5]),
            Self::Regions | Self::Island | Self::Roots => None,
        }
    }
    pub(super) fn inputs(self) -> Vec<Input> {
        match self {
            Self::Layouts => vec![
                Input::LayoutNumeric,
                Input::LayoutPointers,
                Input::LayoutAggregates,
            ],
            Self::Island => vec![Input::IslandPoll],
            Self::Execution => vec![Input::Execution],
            Self::Roots => vec![
                Input::Globals0,
                Input::Globals256,
                Input::Globals8192,
                Input::Fibers16,
                Input::Fibers1024,
                Input::Defers16,
                Input::Defers512,
            ],
            _ => Vec::new(),
        }
    }
    pub(super) fn cases(self) -> Vec<Case> {
        if !self.inputs().is_empty() {
            return cases::runtime_cases(self);
        }
        if self == Self::Recycling {
            ["class32", "class1024", "classes", "two-blocks", "lane"]
                .into_iter()
                .flat_map(|shape| {
                    ["generational", "incremental"]
                        .into_iter()
                        .map(move |gc| Case {
                            input: None,
                            args: vec![shape.into(), gc.into(), "1000".into()],
                        })
                })
                .collect()
        } else {
            vec![Case {
                input: None,
                args: Vec::new(),
            }]
        }
    }
}

pub(super) fn validate(artifact: &Artifact, stdout: &str) -> Result<Value> {
    let unsigned = |v: &Value, key: &str| {
        v[key]
            .as_u64()
            .ok_or_else(|| anyhow!("missing unsigned diagnostic field {key}"))
    };
    let timer = |v: &Value| -> Result<()> {
        if !v["seconds"]
            .as_f64()
            .is_some_and(|s| s.is_finite() && s >= 0.0)
        {
            bail!("invalid diagnostic phase timer");
        }
        for key in ["allocations", "allocated_bytes"] {
            let count = unsigned(v, key)?;
            if !artifact.allocations && count != 0 {
                bail!("allocator counters leaked into timing probe");
            }
        }
        Ok(())
    };
    if artifact.probe == Probe::Regions {
        let rows = stdout
            .lines()
            .map(serde_json::from_str::<Value>)
            .collect::<std::result::Result<Vec<_>, _>>()?;
        let mut seen = BTreeSet::new();
        for row in &rows {
            let iteration = unsigned(row, "iteration")?;
            let batch = unsigned(row, "batch")?;
            let region = row["region"]
                .as_bool()
                .ok_or_else(|| anyhow!("missing region selector"))?;
            let mixed = row["mixed_sizes"]
                .as_bool()
                .ok_or_else(|| anyhow!("missing shape selector"))?;
            if iteration >= 7
                || ![1, 2, 64, 262144].contains(&batch)
                || !seen.insert((iteration, batch, region, mixed))
                || row["n"] != 262144
                || row["objects"] != 262144
                || row["checksum"] != 34359607296_u64
            {
                bail!("region diagnostic result contract failed");
            }
            unsigned(row, "elapsed_ns")?;
            unsigned(row, "allocation_bytes")?;
        }
        if seen.len() != 112 {
            bail!("incomplete region diagnostic matrix");
        }
        return Ok(Value::Array(rows));
    }
    let value: Value = serde_json::from_str(stdout)?;
    if artifact.probe == Probe::Execution {
        execution::validate(artifact, &value)?;
        return Ok(value);
    }
    if matches!(
        artifact.probe,
        Probe::Layouts | Probe::Island | Probe::Roots
    ) {
        runtime::validate(artifact, &value)?;
        return Ok(value);
    }
    if value["allocation_diagnostics"] != artifact.allocations {
        bail!("diagnostic counter build mismatch");
    }
    match artifact.probe {
        Probe::Compile => {
            let rows = value["samples"]
                .as_array()
                .ok_or_else(|| anyhow!("missing compiler samples"))?;
            let mut seen = BTreeSet::new();
            for row in rows {
                let name = row["case"]
                    .as_str()
                    .ok_or_else(|| anyhow!("missing compiler workload"))?;
                let iteration = unsigned(row, "iteration")?;
                if !["small", "medium", "wide"].contains(&name)
                    || !(2..7).contains(&iteration)
                    || !seen.insert((name, iteration))
                {
                    bail!("invalid or duplicate compiler sample");
                }
                for key in ["source_bytes", "instructions", "frame_slots", "vob_bytes"] {
                    unsigned(row, key)?;
                }
                for stage in [
                    "analysis",
                    "codegen",
                    "verification",
                    "serialization",
                    "decoding",
                    "load_verification",
                ] {
                    timer(&row["stages"][stage])?;
                }
            }
            if seen.len() != 15 {
                bail!("incomplete compiler diagnostic matrix");
            }
        }
        Probe::Pipeline => pipeline::validate(&value, artifact.allocations)?,
        Probe::Transfer => {
            let iterations = unsigned(&value, "iterations")?;
            if iterations != 2000 {
                bail!("transfer workload count changed");
            }
            let rows = value["cases"]
                .as_array()
                .ok_or_else(|| anyhow!("missing transfer cases"))?;
            let mut seen = BTreeSet::new();
            for row in rows {
                let name = row["name"]
                    .as_str()
                    .ok_or_else(|| anyhow!("missing transfer shape"))?;
                if ![
                    "empty",
                    "one-int",
                    "captures16",
                    "captures128",
                    "arguments128",
                    "mixed64",
                    "array64-one",
                    "array64-many",
                ]
                .contains(&name)
                    || !seen.insert(name)
                {
                    bail!("unknown or duplicate transfer shape");
                }
                if unsigned(row, "wire_bytes")?.checked_mul(iterations)
                    != Some(unsigned(row, "checksum")?)
                {
                    bail!("transfer checksum mismatch");
                }
                timer(row)?;
            }
            if seen.len() != 8 {
                bail!("incomplete transfer diagnostic matrix");
            }
        }
        Probe::Recycling => {
            let name = value["case"]
                .as_str()
                .ok_or_else(|| anyhow!("missing recycling shape"))?;
            let count = match name {
                "two-blocks" => 4096_u64,
                "class32" | "class1024" | "classes" | "lane" => 64,
                _ => bail!("unknown recycling shape"),
            };
            if !matches!(
                value["gc_mode"].as_str(),
                Some("generational" | "incremental")
            ) || value["iterations"] != 1000
                || value["checksum"] != 1000 * count * (count - 1) / 2
                || value["teardown_requested_bytes_balance"] != 0
            {
                bail!("recycling diagnostic result contract failed");
            }
            for key in [
                "elapsed_ns",
                "managed_allocation_bytes",
                "managed_committed_bytes",
                "gc_work_units",
            ] {
                unsigned(&value, key)?;
            }
            for key in [
                "host_allocation_calls",
                "host_requested_bytes",
                "host_free_calls",
                "host_freed_bytes",
                "warm_retained_requested_bytes_excluding_pages",
            ] {
                let count = unsigned(&value, key)?;
                if !artifact.allocations && count != 0 {
                    bail!("recycling counters leaked into timing probe");
                }
            }
        }
        Probe::Regions | Probe::Layouts | Probe::Island | Probe::Roots | Probe::Execution => {
            unreachable!()
        }
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    fn artifact(probe: Probe, allocations: bool) -> Artifact {
        Artifact {
            probe,
            allocations,
            features: probe.artifact_features(allocations),
            file: String::new(),
            sha256: String::new(),
        }
    }
    #[test]
    fn transfer_requires_every_unique_shape_and_exact_wire_checksum() {
        let names = [
            "empty",
            "one-int",
            "captures16",
            "captures128",
            "arguments128",
            "mixed64",
            "array64-one",
            "array64-many",
        ];
        let mut value = json!({"allocation_diagnostics":false,"iterations":2000,"cases":names.map(|name|json!({"name":name,"seconds":0.1,"allocations":0,"allocated_bytes":0,"wire_bytes":10,"checksum":20000}))});
        let a = artifact(Probe::Transfer, false);
        assert!(validate(&a, &value.to_string()).is_ok());
        value["cases"][1]["name"] = json!("empty");
        assert!(validate(&a, &value.to_string()).is_err());
        value["cases"][1]["name"] = json!("one-int");
        value["cases"][0]["checksum"] = json!(19999);
        assert!(validate(&a, &value.to_string()).is_err());
    }
    #[test]
    fn incomplete_compiler_reports_and_wrong_counter_builds_fail() {
        let a = artifact(Probe::Compile, false);
        for text in [
            "{}",
            r#"{"allocation_diagnostics":false,"samples":[]}"#,
            r#"{"allocation_diagnostics":false,"samples":[{}]}"#,
        ] {
            assert!(validate(&a, text).is_err());
        }
        assert!(validate(
            &artifact(Probe::Transfer, true),
            r#"{"allocation_diagnostics":false,"cases":[]}"#
        )
        .is_err());
        assert!(validate(&artifact(Probe::Regions, false), "{}").is_err());
    }
}
