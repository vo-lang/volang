//! Each frozen-probe process contributes one sample per metric. Inner repeats
//! are reduced first; they never increase the independent sample count.
use super::probes::Probe;
use anyhow::{anyhow, bail, Result};
use serde_json::{json, Value};
use std::collections::BTreeMap;

type Samples = BTreeMap<String, BTreeMap<usize, BTreeMap<usize, f64>>>;

fn median(values: &[f64]) -> f64 {
    let mut sorted = values.to_vec();
    sorted.sort_by(f64::total_cmp);
    let middle = sorted.len() / 2;
    if sorted.len() % 2 == 0 {
        sorted[middle - 1] / 2.0 + sorted[middle] / 2.0
    } else {
        sorted[middle]
    }
}

fn number(value: &Value) -> Result<f64> {
    value
        .as_f64()
        .filter(|v| v.is_finite() && *v >= 0.0)
        .ok_or_else(|| anyhow!("invalid diagnostic metric"))
}

fn metrics(probe: Probe, data: &Value) -> Result<BTreeMap<String, f64>> {
    let mut groups = BTreeMap::<String, Vec<f64>>::new();
    let mut add = |key: String, value: f64| groups.entry(key).or_default().push(value);
    match probe {
        Probe::Compile => {
            for row in data["samples"].as_array().unwrap() {
                for (stage, value) in row["stages"].as_object().unwrap() {
                    add(
                        format!("compile/{}/{stage}", row["case"].as_str().unwrap()),
                        number(&value["seconds"])? * 1e9,
                    );
                }
            }
        }
        Probe::Pipeline => {
            for row in data["samples"].as_array().unwrap() {
                let prefix = format!(
                    "pipeline/{}/{}",
                    row["case"].as_str().unwrap(),
                    row["scenario"].as_str().unwrap()
                );
                add(format!("{prefix}/total"), number(&row["elapsed_ns"])?);
                add(
                    format!("{prefix}/unattributed"),
                    number(&row["unattributed_ns"])?,
                );
                for (phase, value) in row["phases"].as_object().unwrap() {
                    add(format!("{prefix}/{phase}"), number(&value["exclusive_ns"])?);
                }
            }
        }
        Probe::Regions => {
            for row in data.as_array().unwrap() {
                if row["iteration"].as_u64().unwrap() < 2 {
                    continue;
                }
                add(
                    format!(
                        "regions/batch={}/region={}/mixed={}",
                        row["batch"], row["region"], row["mixed_sizes"]
                    ),
                    number(&row["elapsed_ns"])? / number(&row["n"])?,
                );
            }
        }
        Probe::Transfer => {
            for row in data["cases"].as_array().unwrap() {
                add(
                    format!("transfer/{}", row["name"].as_str().unwrap()),
                    number(&row["seconds"])? * 1e9 / number(&data["iterations"])?,
                );
            }
        }
        Probe::Recycling => add(
            format!(
                "recycling/{}/{}",
                data["case"].as_str().unwrap(),
                data["gc_mode"].as_str().unwrap()
            ),
            number(&data["elapsed_ns"])? / number(&data["iterations"])?,
        ),
        Probe::Layouts => {
            let fixture = data["fixture"]
                .as_str()
                .ok_or_else(|| anyhow!("missing layout input identity"))?;
            for row in data["samples"].as_array().unwrap() {
                for stage in ["load", "clone"] {
                    add(
                        format!(
                            "layouts/{fixture}/{}/{stage}",
                            data["view"].as_str().unwrap()
                        ),
                        number(&row[stage]["elapsed_ns"])?,
                    );
                }
            }
        }
        Probe::Island => {
            let prefix = format!(
                "island/{}/{}/{}",
                data["mode"].as_str().unwrap(),
                data["children"],
                data["workload"].as_str().unwrap()
            );
            for row in data["records"].as_array().unwrap() {
                add(
                    prefix.clone(),
                    number(&row["elapsed_ns"])? / number(&row["scheduler_turns"])?,
                );
            }
        }
        Probe::Execution => {
            let prefix = format!(
                "execution/{}/{}",
                data["workload"].as_str().unwrap(),
                data["mode"].as_str().unwrap()
            );
            for (stage, value) in data["stages"].as_object().unwrap() {
                add(format!("{prefix}/{stage}"), number(value)?);
            }
            for row in data["records"].as_array().unwrap() {
                add(
                    format!("{prefix}/warmed_execution_ns"),
                    number(&row["elapsed_ns"])?,
                );
            }
        }
        Probe::Roots => {
            let fixture = data["fixture"]
                .as_str()
                .ok_or_else(|| anyhow!("missing root input identity"))?;
            let prefix = format!(
                "roots/{fixture}/{}/{}/{}",
                data["mode"].as_str().unwrap(),
                data["gc_mode"].as_str().unwrap(),
                data["admission"].as_str().unwrap()
            );
            for row in data["records"].as_array().unwrap() {
                for metric in [
                    "initial_step_ns",
                    "guest_resume_active_ns",
                    "guest_resume_wall_ns",
                ] {
                    add(format!("{prefix}/{metric}"), number(&row[metric])?);
                }
                for turn in row["turns"].as_array().unwrap() {
                    add(
                        format!("{prefix}/host_return_ns"),
                        number(&turn["host_return_ns"])?,
                    );
                }
            }
        }
    }
    Ok(groups
        .into_iter()
        .map(|(key, values)| (key, median(&values)))
        .collect())
}

fn change(log_ratio: f64) -> f64 {
    log_ratio.exp_m1() * 100.0
}

fn comparison(before: &BTreeMap<usize, f64>, after: &BTreeMap<usize, f64>) -> Result<Value> {
    if !before.keys().eq(after.keys()) {
        bail!("diagnostic comparison is missing a process pair");
    }
    if before.values().chain(after.values()).any(|&v| v == 0.0) {
        return Ok(
            json!({"paired_processes":before.len(),"reason":"zero-duration sample; relative change undefined"}),
        );
    }
    let logs = before
        .iter()
        .map(|(round, value)| after[round].ln() - value.ln())
        .collect::<Vec<_>>();
    let mut result =
        json!({"paired_processes":logs.len(),"median_paired_change_percent":change(median(&logs))});
    // Fixed-seed paired bootstrap. At least six independent process pairs are
    // required; this interval describes the median ratio, never a tail latency.
    if logs.len() >= 6 {
        let mut state = 20260910_u64;
        let mut draws = Vec::with_capacity(20_000);
        let mut sample = vec![0.0; logs.len()];
        for _ in 0..20_000 {
            for value in &mut sample {
                // SplitMix64 supplies reproducible resampling independent of
                // platform RNG implementations and randomized hash seeds.
                state = state.wrapping_add(0x9e3779b97f4a7c15);
                let mut bits = state;
                bits = (bits ^ (bits >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
                bits = (bits ^ (bits >> 27)).wrapping_mul(0x94d049bb133111eb);
                bits ^= bits >> 31;
                *value = logs[(bits % logs.len() as u64) as usize];
            }
            draws.push(median(&sample));
        }
        draws.sort_by(f64::total_cmp);
        result["paired_bootstrap_95_percent"] = json!([change(draws[499]), change(draws[19499])]);
    }
    Ok(result)
}

pub(super) fn summarize(rows: &[Value]) -> Result<Value> {
    let mut samples = Samples::new();
    for row in rows {
        if row["allocations"] == true || row["warmup"] == true {
            continue;
        }
        let version = row["version"].as_u64().unwrap() as usize;
        let round = row["round"].as_u64().unwrap() as usize;
        let probe: Probe = serde_json::from_value(row["probe"].clone())?;
        for (key, value) in metrics(probe, &row["data"])? {
            if !value.is_finite()
                || samples
                    .entry(key)
                    .or_default()
                    .entry(version)
                    .or_default()
                    .insert(round, value)
                    .is_some()
            {
                bail!("invalid or duplicate diagnostic process sample");
            }
        }
    }
    let mut result = Vec::new();
    for (metric, versions) in samples {
        let unit = match metric.split('/').next() {
            Some("compile" | "pipeline") => "ns/stage",
            Some("regions") => "ns/object",
            Some("transfer") => "ns/packet",
            Some("island") => "ns/scheduler-turn",
            Some("layouts") => "ns/operation",
            Some("roots") => "ns/observation",
            Some("recycling") => "ns/collector-cycle",
            Some("execution") if metric.ends_with("/warmed_execution_ns") => "ns/invocation",
            Some("execution") => "ns/stage",
            _ => bail!("missing unit for diagnostic metric {metric}"),
        };
        let mut row = json!({"metric":metric,"unit":unit,"versions":versions.iter().map(|(version, rounds)| {
            let values=rounds.values().copied().collect::<Vec<_>>();
            json!({"version":version,"process_samples":values.len(),"median":median(&values),"min":values.iter().copied().min_by(f64::total_cmp).unwrap(),"max":values.iter().copied().max_by(f64::total_cmp).unwrap()})
        }).collect::<Vec<_>>()});
        if versions.len() == 2 {
            row["comparison"] = comparison(&versions[&0], &versions[&1])?;
        }
        result.push(row);
    }
    Ok(
        json!({"scope":"Probe-owned durations. Inner repeats reduce to one median per process; counter builds and warmups excluded. Paired bootstrap resamples independent process pairs; 20000 draws, seed 20260910, nearest-rank 2.5/97.5 percentiles. Negative change means faster. No p95/p99 latency estimate.","metrics":result}),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paired_change_uses_matching_processes_and_handles_zero_durations() {
        let a = (0..12)
            .map(|i| (i, i as f64 + 1.0))
            .collect::<BTreeMap<_, _>>();
        let mut b = a.iter().map(|(&i, &v)| (i, v * 0.75)).collect();
        let result = comparison(&a, &b).unwrap();
        assert!((result["median_paired_change_percent"].as_f64().unwrap() + 25.0).abs() < 1e-10);
        for bound in result["paired_bootstrap_95_percent"].as_array().unwrap() {
            assert!((bound.as_f64().unwrap() + 25.0).abs() < 1e-10);
        }
        b.insert(0, 0.0);
        assert!(comparison(&a, &b).unwrap()["reason"].is_string());
        b.remove(&0);
        assert!(comparison(&a, &b).is_err());
    }

    #[test]
    fn region_inner_repeats_count_as_one_process_and_both_warmups_are_excluded() {
        let data = (0..7).map(|i|json!({"iteration":i,"batch":1,"region":true,"mixed_sizes":false,"elapsed_ns":if i<2 {900000} else {100},"n":10})).collect::<Vec<_>>();
        let valid = json!({"probe":"regions","allocations":false,"warmup":false,"version":0,"round":2,"data":data});
        let mut outer_warmup = valid.clone();
        outer_warmup["warmup"] = json!(true);
        let mut counter = valid.clone();
        counter["allocations"] = json!(true);
        let result = summarize(&[valid.clone(), outer_warmup, counter]).unwrap();
        let metric = &result["metrics"][0]["versions"][0];
        assert_eq!(metric["process_samples"], 1);
        assert_eq!(metric["median"], 10.0);
        assert!(summarize(&[valid.clone(), valid]).is_err());
    }

    #[test]
    fn root_observations_retain_one_independent_process_sample_per_metric() {
        let data = json!({"fixture":"globals-8192","mode":"jit","gc_mode":"incremental","admission":"forced",
            "records":[{"initial_step_ns":1,"guest_resume_active_ns":5,"guest_resume_wall_ns":9,"turns":[{"host_return_ns":2},{"host_return_ns":2}]},
                {"initial_step_ns":3,"guest_resume_active_ns":7,"guest_resume_wall_ns":11,"turns":[{"host_return_ns":4}]}]});
        let row = json!({"probe":"roots","version":0,"round":0,"allocations":false,"warmup":false,"data":data});
        let result = summarize(&[row]).unwrap();
        let metrics = result["metrics"].as_array().unwrap();
        assert_eq!(metrics.len(), 4);
        assert!(metrics
            .iter()
            .all(|m| m["versions"][0]["process_samples"] == 1));
        let host = metrics
            .iter()
            .find(|m| m["metric"].as_str().unwrap().ends_with("host_return_ns"))
            .unwrap();
        assert_eq!(host["versions"][0]["median"], 2.0);
    }

    #[test]
    fn execution_intervals_have_invocation_units_and_one_process_sample() {
        let row = json!({"probe":"execution","version":0,"round":0,
            "allocations":false,"warmup":false,"data":{
                "workload":"Arithmetic","mode":"vm",
                "stages":{"loading_ns":17},
                "records":[{"elapsed_ns":11},{"elapsed_ns":13}]}});
        let result = summarize(&[row]).unwrap();
        let metrics = result["metrics"].as_array().unwrap();
        assert_eq!(metrics.len(), 2);
        let execution = metrics
            .iter()
            .find(|m| m["unit"] == "ns/invocation")
            .unwrap();
        assert_eq!(execution["versions"][0]["median"], 12.0);
        assert_eq!(execution["versions"][0]["process_samples"], 1);
        let stage = metrics.iter().find(|m| m["unit"] == "ns/stage").unwrap();
        assert_eq!(stage["versions"][0]["median"], 17.0);
    }
}
