//! Maintained native diagnostics: freeze first, execute frozen probes separately.
//! Probe-owned clocks and counters retain their scopes; process waiting is only
//! orchestration and must not be interpreted as a cold-start timing sample.

use super::evidence;
use crate::release_config::sha256_file;
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

mod inputs;
mod probes;
mod process;
mod summary;
use probes::{validate, Probe};
use process::execute;

const SCHEMA: &str = "volang.benchmark.diagnostics.v4";
const INPUT_SCHEMA: &str = "volang.benchmark.diagnostics.v3";
const FEATURE_SCHEMA: &str = "volang.benchmark.diagnostics.v2";
const LEGACY_SCHEMA: &str = "volang.benchmark.diagnostics.v1";
const COUNTERS: [&str; 6] = [
    "VO_COMPILER_ALLOCATION_DIAGNOSTICS",
    "VO_TRANSFER_ALLOCATION_DIAGNOSTICS",
    "VO_RECYCLING_ALLOCATION_DIAGNOSTICS",
    "VO_PIPELINE_ALLOCATION_DIAGNOSTICS",
    "VO_LAYOUT_ALLOCATION_DIAGNOSTICS",
    "VO_EXECUTION_WORK_DIAGNOSTICS",
];

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Artifact {
    probe: Probe,
    allocations: bool,
    #[serde(default)]
    features: Vec<String>,
    file: String,
    sha256: String,
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Snapshot {
    schema: String,
    sources: Value,
    rustc: String,
    cargo: String,
    architecture: String,
    os: String,
    environment: BTreeMap<String, String>,
    artifacts: Vec<Artifact>,
    #[serde(default)]
    inputs: Vec<inputs::FrozenInput>,
    #[serde(default)]
    input_compiler: Option<inputs::Compiler>,
}

pub(super) fn command(root: &Path, args: &[String]) -> Result<()> {
    let Some(operation) = args.first().map(String::as_str) else {
        bail!("usage: vo-dev bench diagnostics list | prepare [SUITE|all] | run <snapshot> [--runs N] [--warmup N] | compare <before> <after> [--runs N] [--warmup N]");
    };
    if operation == "list" {
        if args.len() != 1 {
            bail!("diagnostics list takes no arguments");
        }
        for probe in Probe::all() {
            println!(
                "{}: {} / {}",
                probe.name(),
                probe.package(),
                probe.example()
            );
        }
        return Ok(());
    }
    let cancelled = Arc::new(AtomicBool::new(false));
    let signal = Arc::clone(&cancelled);
    ctrlc::set_handler(move || signal.store(true, Ordering::Relaxed))?;
    match operation {
        "prepare" => {
            if args.len() > 2 {
                bail!("diagnostics prepare accepts one suite");
            }
            let selected = args.get(1).map(String::as_str).unwrap_or("all");
            let probes = Probe::all()
                .into_iter()
                .filter(|p| selected == "all" || selected == p.name())
                .collect::<Vec<_>>();
            if probes.is_empty() {
                bail!("unknown diagnostic suite: {selected}");
            }
            prepare(root, &probes, &cancelled)
        }
        "run" | "compare" => {
            let count = if operation == "compare" { 2 } else { 1 };
            let paths = args
                .get(1..=count)
                .ok_or_else(|| {
                    anyhow!("diagnostics {operation} requires {count} snapshot directories")
                })?
                .iter()
                .map(|p| root.join(p))
                .collect::<Vec<_>>();
            let mut runs = 12;
            let mut warmup = 2;
            for pair in args[1 + count..].chunks(2) {
                let [key, value] = pair else {
                    bail!("diagnostic option requires a value");
                };
                let n: usize = value
                    .parse()
                    .context("invalid diagnostic repetition count")?;
                if n > 100 {
                    bail!("diagnostic repetitions must not exceed 100");
                }
                match key.as_str() {
                    "--runs" if n > 0 => runs = n,
                    "--warmup" => warmup = n,
                    _ => bail!("invalid diagnostic option: {key}"),
                }
            }
            run(root, &paths, runs, warmup, &cancelled)
        }
        _ => bail!("unknown diagnostics operation: {operation}"),
    }
}

fn new_directory(parent: &Path) -> Result<PathBuf> {
    fs::create_dir_all(parent)?;
    let path = parent.join(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)?
            .as_nanos()
            .to_string(),
    );
    fs::create_dir(&path)?;
    Ok(path)
}

fn clean_command(program: impl AsRef<std::ffi::OsStr>) -> Command {
    let mut command = Command::new(program);
    for (key, _) in std::env::vars_os() {
        let text = key.to_string_lossy();
        if text.starts_with("VO_JIT")
            || text.starts_with("VO_GC")
            || text.starts_with("VO_AOT")
            || text == "VO_BENCH_METRICS"
            || COUNTERS.contains(&text.as_ref())
        {
            command.env_remove(key);
        }
    }
    command.env("VOWORK", "off");
    command
}

fn version(program: &str) -> Result<String> {
    let output = Command::new(program).arg("--version").output()?;
    if !output.status.success() {
        bail!("{program} --version failed");
    }
    Ok(String::from_utf8(output.stdout)?.trim().into())
}

fn prepare(root: &Path, probes: &[Probe], cancelled: &AtomicBool) -> Result<()> {
    let output = new_directory(&root.join("target/bench/diagnostics"))?;
    let sources = evidence::source_identity(root)?;
    evidence::write_json(&output.join("sources.json"), &sources)?;
    // Preserve the dirty overlay as well as hashes so this snapshot remains reviewable.
    let diff = Command::new("git")
        .args(["diff", "--binary", "HEAD"])
        .current_dir(root)
        .output()?;
    if !diff.status.success() {
        bail!("could not capture diagnostic source overlay");
    }
    fs::write(output.join("source.diff"), diff.stdout)?;
    let untracked = Command::new("git")
        .args(["ls-files", "--others", "--exclude-standard", "-z"])
        .current_dir(root)
        .output()?;
    if !untracked.status.success() {
        bail!("could not capture new diagnostic sources");
    }
    for name in untracked
        .stdout
        .split(|&b| b == 0)
        .filter(|p| !p.is_empty())
    {
        let name = std::str::from_utf8(name)?;
        let source = root.join(name);
        let kind = fs::symlink_metadata(&source)?.file_type();
        if kind.is_symlink() {
            // Preserve the link identity without reading any external target.
            let target = fs::read_link(&source)?;
            let output_path = output
                .join("untracked-links")
                .join(format!("{name}.link.json"));
            fs::create_dir_all(output_path.parent().unwrap())?;
            evidence::write_json(
                &output_path,
                &json!({"path":name,"target_bytes":target.as_os_str().as_encoded_bytes()}),
            )?;
        } else if kind.is_file() {
            let target = output.join("untracked").join(name);
            fs::create_dir_all(target.parent().unwrap())?;
            fs::copy(source, target)?;
        }
    }
    let (inputs, input_compiler) = inputs::prepare(root, &output, probes, cancelled)?;
    if sources != evidence::source_identity(root)? {
        bail!("source changed while preparing diagnostic inputs; partial snapshot retained");
    }
    let mut artifacts = Vec::new();
    let environment = [
        "RUSTFLAGS",
        "CARGO_ENCODED_RUSTFLAGS",
        "CARGO_BUILD_JOBS",
        "CC",
        "CXX",
    ]
    .into_iter()
    .filter_map(|key| std::env::var(key).ok().map(|v| (key.into(), v)))
    .collect();
    for &probe in probes {
        for allocations in [false, true] {
            if allocations && probe.counter().is_none() {
                continue;
            }
            let label = format!(
                "{}-{}",
                probe.name(),
                if allocations { "allocations" } else { "timing" }
            );
            let directory = output.join(&label);
            fs::create_dir(&directory)?;
            let mut command = clean_command("cargo");
            command
                .current_dir(root)
                .args([
                    "build",
                    "--locked",
                    "--profile",
                    "release-native",
                    "-p",
                    probe.package(),
                    "--example",
                    probe.example(),
                ])
                .env_remove("CARGO_BUILD_TARGET")
                .env_remove("CARGO_TARGET_DIR");
            let features = probe.artifact_features(allocations);
            if !features.is_empty() {
                command.args(["--features", &features.join(",")]);
            }
            if allocations {
                command.env(probe.counter().unwrap(), "1");
            }
            execute(command, &directory, cancelled, Duration::from_secs(1800))?;
            if sources != evidence::source_identity(root)? {
                bail!("source changed while preparing diagnostics; retained partial snapshot is invalid");
            }
            let name = format!("{}{}", probe.example(), std::env::consts::EXE_SUFFIX);
            let file = format!("{label}/{name}");
            fs::copy(
                root.join("target/release-native/examples").join(&name),
                output.join(&file),
            )?;
            artifacts.push(Artifact {
                probe,
                allocations,
                features,
                sha256: sha256_file(&output.join(&file))?,
                file,
            });
        }
    }
    let snapshot = Snapshot {
        schema: SCHEMA.into(),
        sources: serde_json::to_value(&sources)?,
        rustc: version("rustc")?,
        cargo: version("cargo")?,
        architecture: std::env::consts::ARCH.into(),
        os: std::env::consts::OS.into(),
        environment,
        artifacts,
        inputs,
        input_compiler,
    };
    evidence::write_json(&output.join("snapshot.json"), &snapshot)?;
    println!("Frozen diagnostic snapshot: {}", output.display());
    Ok(())
}

fn load_snapshot(directory: &Path) -> Result<Snapshot> {
    let snapshot: Snapshot = serde_json::from_slice(&fs::read(directory.join("snapshot.json"))?)?;
    if ![SCHEMA, INPUT_SCHEMA, FEATURE_SCHEMA, LEGACY_SCHEMA].contains(&snapshot.schema.as_str())
        || snapshot.architecture != std::env::consts::ARCH
        || snapshot.os != std::env::consts::OS
    {
        bail!("diagnostic snapshot schema or host target mismatch");
    }
    if ![SCHEMA, INPUT_SCHEMA].contains(&snapshot.schema.as_str())
        && (!snapshot.inputs.is_empty()
            || snapshot.input_compiler.is_some()
            || snapshot
                .artifacts
                .iter()
                .any(|a| !a.probe.inputs().is_empty()))
    {
        bail!("legacy diagnostic schemas cannot describe frozen runtime inputs");
    }
    if snapshot.schema == LEGACY_SCHEMA
        && snapshot
            .artifacts
            .iter()
            .any(|a| !a.features.is_empty() || a.probe == Probe::Pipeline)
    {
        bail!("legacy diagnostic schema cannot describe feature-bearing pipeline probes");
    }
    if snapshot.schema != SCHEMA
        && snapshot
            .artifacts
            .iter()
            .any(|a| a.probe == Probe::Execution)
    {
        bail!("execution work variants require diagnostics schema v4");
    }
    validate_variants(&snapshot.artifacts)?;
    inputs::validate(&snapshot, directory)?;
    for artifact in &snapshot.artifacts {
        let expected = format!(
            "{}-{}/{}{}",
            artifact.probe.name(),
            if artifact.allocations {
                "allocations"
            } else {
                "timing"
            },
            artifact.probe.example(),
            std::env::consts::EXE_SUFFIX
        );
        let path = directory.join(&artifact.file).canonicalize()?;
        if artifact.file != expected
            || !path.starts_with(directory)
            || sha256_file(&path)? != artifact.sha256
        {
            bail!("diagnostic artifact identity mismatch");
        }
    }
    Ok(snapshot)
}

fn validate_variants(artifacts: &[Artifact]) -> Result<()> {
    let mut seen = BTreeSet::new();
    let maximum: usize = Probe::all()
        .iter()
        .map(|p| 1 + usize::from(p.counter().is_some()))
        .sum();
    if artifacts.is_empty() || artifacts.len() > maximum {
        bail!("invalid diagnostic artifact count");
    }
    for artifact in artifacts {
        if !seen.insert((artifact.probe.name(), artifact.allocations))
            || (artifact.allocations && artifact.probe.counter().is_none())
            || artifact.features != artifact.probe.artifact_features(artifact.allocations)
        {
            bail!("duplicate or unsupported diagnostic artifact");
        }
    }
    for probe in Probe::all() {
        let timing = seen.contains(&(probe.name(), false));
        let counters = seen.contains(&(probe.name(), true));
        if counters && !timing || timing && probe.counter().is_some() && !counters {
            bail!(
                "incomplete diagnostic timing/counter suite: {}",
                probe.name()
            );
        }
    }
    Ok(())
}

fn matching_probes(a: &Snapshot, b: &Snapshot) -> Result<()> {
    let keys = |s: &Snapshot| {
        s.artifacts
            .iter()
            .map(|a| (a.probe.name(), a.allocations))
            .collect::<BTreeSet<_>>()
    };
    if keys(a) != keys(b) {
        bail!("diagnostic snapshots must contain the same probe/counter variants");
    }
    if a.environment != b.environment || a.rustc != b.rustc || a.cargo != b.cargo {
        bail!("diagnostic toolchain or build environment changed; compare matching builds");
    }
    inputs::matching(a, b)?;
    for artifact in &a.artifacts {
        let source = format!(
            "lang/crates/{}/examples/{}.rs",
            artifact.probe.package(),
            artifact.probe.example()
        );
        let hash = &a.sources["files"][&source];
        if !hash.as_str().is_some_and(|s| s.len() == 64) || hash != &b.sources["files"][&source] {
            bail!("diagnostic workload source changed: {source}");
        }
    }
    Ok(())
}

fn run(
    root: &Path,
    directories: &[PathBuf],
    runs: usize,
    warmup: usize,
    cancelled: &AtomicBool,
) -> Result<()> {
    let directories = directories
        .iter()
        .map(|p| p.canonicalize())
        .collect::<std::io::Result<Vec<_>>>()?;
    let snapshots = directories
        .iter()
        .map(|p| load_snapshot(p))
        .collect::<Result<Vec<_>>>()?;
    for snapshot in &snapshots[1..] {
        matching_probes(&snapshots[0], snapshot)?;
    }
    let output = new_directory(&root.join("target/bench/diagnostic-runs"))?;
    let driver = std::env::current_exe()?.canonicalize()?;
    let driver_sha256 = sha256_file(&driver)?;
    let mut identities = Vec::new();
    for (index, (directory, snapshot)) in directories.iter().zip(&snapshots).enumerate() {
        identities.push(json!({"version":index,"snapshot":directory,"sha256":sha256_file(&directory.join("snapshot.json"))?}));
        // Identical relative names and one-digit version components make paired argv lengths equal.
        for artifact in &snapshot.artifacts {
            let target = output
                .join("bin")
                .join(index.to_string())
                .join(&artifact.file);
            fs::create_dir_all(target.parent().unwrap())?;
            fs::copy(directory.join(&artifact.file), &target)?;
            if sha256_file(&target)? != artifact.sha256 {
                bail!("diagnostic copy identity mismatch");
            }
        }
        for input in &snapshot.inputs {
            let target = output
                .join("bin")
                .join(index.to_string())
                .join(input.input.bytecode_file());
            fs::create_dir_all(target.parent().unwrap())?;
            fs::copy(directory.join(input.input.bytecode_file()), &target)?;
            if sha256_file(&target)? != input.bytecode_sha256 {
                bail!("diagnostic input copy identity mismatch");
            }
        }
    }
    evidence::write_json(
        &output.join("identity.json"),
        &json!({"schema":SCHEMA,"driver":driver,"driver_sha256":driver_sha256,"snapshots":identities,"runs":runs,"warmup":warmup,"scope":"Native probe-owned phases only. Process durations are orchestration diagnostics. Counter builds run separately and their time fields are excluded. No builds run here. Two versions use paired AB/BA order, equal-length argv and rotating probe order. Original within-process warmups are preserved."}),
    )?;
    let mut results = Vec::new();
    for allocations in [true, false] {
        let rounds = if allocations { 1 } else { warmup + runs };
        for round in 0..rounds {
            let artifacts = snapshots[0]
                .artifacts
                .iter()
                .filter(|a| a.allocations == allocations)
                .collect::<Vec<_>>();
            if artifacts.is_empty() {
                continue;
            }
            for index in 0..artifacts.len() {
                let first = artifacts[(index + round) % artifacts.len()];
                for case in first.probe.cases() {
                    for position in 0..snapshots.len() {
                        let version = if round % 2 == 0 {
                            position
                        } else {
                            snapshots.len() - 1 - position
                        };
                        let artifact = snapshots[version]
                            .artifacts
                            .iter()
                            .find(|a| a.probe == first.probe && a.allocations == allocations)
                            .unwrap();
                        let attempt = output.join(format!("{:06}", results.len()));
                        fs::create_dir(&attempt)?;
                        let input_directory = output.join("bin").join(version.to_string());
                        let args = case.arguments(first.probe, allocations, &input_directory);
                        let mut command = clean_command(
                            output
                                .join("bin")
                                .join(version.to_string())
                                .join(&artifact.file),
                        );
                        command.current_dir(root).args(&args);
                        execute(command, &attempt, cancelled, Duration::from_secs(300))?;
                        let stdout = fs::read_to_string(attempt.join("stdout.log"))?;
                        let validation = validate(artifact, &stdout).and_then(|mut data| {
                            case.validate(artifact, &data, &args, &snapshots[version])?;
                            if let Some(input) = case.input {
                                data["fixture"] = json!(input.name());
                            }
                            Ok(data)
                        });
                        evidence::write_json(
                            &attempt.join("validation.json"),
                            &json!({"passed":validation.is_ok(),"error":validation.as_ref().err().map(|e|format!("{e:#}"))}),
                        )?;
                        let data = validation.with_context(|| {
                            format!("diagnostic result invalid: {}", attempt.display())
                        })?;
                        results.push(json!({"version":version,"probe":artifact.probe,"allocations":allocations,"round":round,"warmup":!allocations && round<warmup,"argv":args,"attempt":attempt.file_name().unwrap().to_string_lossy(),"data":data}));
                        evidence::write_json(&output.join("results.json"), &results)?;
                    }
                }
            }
            println!(
                "{} round {round} complete",
                if allocations { "counters" } else { "timing" }
            );
        }
    }
    for (index, snapshot) in snapshots.iter().enumerate() {
        for artifact in &snapshot.artifacts {
            if sha256_file(
                &output
                    .join("bin")
                    .join(index.to_string())
                    .join(&artifact.file),
            )? != artifact.sha256
            {
                bail!("frozen probe changed during measurement");
            }
        }
        for input in &snapshot.inputs {
            if sha256_file(
                &output
                    .join("bin")
                    .join(index.to_string())
                    .join(input.input.bytecode_file()),
            )? != input.bytecode_sha256
            {
                bail!("frozen diagnostic input changed during measurement");
            }
        }
    }
    let summary = summary::summarize(&results)?;
    if sha256_file(&driver)? != driver_sha256 {
        bail!("diagnostic driver changed during measurement");
    }
    evidence::write_json(&output.join("summary.json"), &summary)?;
    evidence::write_json(
        &output.join("completed.json"),
        &json!({"processes":results.len(),"results_sha256":sha256_file(&output.join("results.json"))?,"summary_sha256":sha256_file(&output.join("summary.json"))?}),
    )?;
    println!("Diagnostic results: {}", output.display());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn snapshot() -> Snapshot {
        Snapshot {
            schema: SCHEMA.into(),
            sources: json!({"files":{"lang/crates/vo-runtime/examples/transfer_packets.rs":"a".repeat(64)}}),
            rustc: "compiler".into(),
            cargo: "builder".into(),
            architecture: std::env::consts::ARCH.into(),
            os: std::env::consts::OS.into(),
            environment: BTreeMap::new(),
            artifacts: vec![Artifact {
                probe: Probe::Transfer,
                allocations: false,
                features: Vec::new(),
                file: String::new(),
                sha256: String::new(),
            }],
            inputs: Vec::new(),
            input_compiler: None,
        }
    }
    #[test]
    fn snapshots_require_complete_unique_timing_and_counter_suites() {
        let mut s = snapshot();
        assert!(validate_variants(&s.artifacts).is_err());
        let mut counter = snapshot().artifacts.pop().unwrap();
        counter.allocations = true;
        s.artifacts.push(counter);
        assert!(validate_variants(&s.artifacts).is_ok());
        s.artifacts.remove(0);
        assert!(validate_variants(&s.artifacts).is_err());
        s.artifacts[0].allocations = false;
        s.artifacts[0].probe = Probe::Regions;
        assert!(validate_variants(&s.artifacts).is_ok());
        let mut duplicate = snapshot().artifacts.pop().unwrap();
        duplicate.probe = Probe::Regions;
        s.artifacts.push(duplicate);
        assert!(validate_variants(&s.artifacts).is_err());
        assert!(validate_variants(&[]).is_err());
    }

    #[test]
    fn comparison_rejects_changed_workload_toolchain_and_probe_variants() {
        let a = snapshot();
        let mut b = snapshot();
        assert!(matching_probes(&a, &b).is_ok());
        b.sources["files"]["lang/crates/vo-runtime/examples/transfer_packets.rs"] =
            json!("b".repeat(64));
        assert!(matching_probes(&a, &b).is_err());
        b = snapshot();
        b.rustc = "different compiler".into();
        assert!(matching_probes(&a, &b).is_err());
        b = snapshot();
        b.artifacts[0].allocations = true;
        assert!(matching_probes(&a, &b).is_err());
    }

    #[test]
    fn legacy_artifacts_decode_without_features_and_pipeline_requires_exact_features() {
        let legacy: Artifact = serde_json::from_value(json!({
            "probe":"regions", "allocations":false, "file":"unused", "sha256":"unused"
        }))
        .unwrap();
        assert!(legacy.features.is_empty());
        validate_variants(&[legacy]).unwrap();

        let mut pipeline: Vec<Artifact> = [false, true]
            .into_iter()
            .map(|allocations| Artifact {
                probe: Probe::Pipeline,
                allocations,
                features: vec!["compiler-profile".into()],
                file: String::new(),
                sha256: String::new(),
            })
            .collect();
        validate_variants(&pipeline).unwrap();
        pipeline[0].features.clear();
        assert!(validate_variants(&pipeline).is_err());
        pipeline[0].features = vec!["jit".into()];
        assert!(validate_variants(&pipeline).is_err());
    }

    #[test]
    fn runtime_comparison_rejects_changed_frozen_bytecode_and_selector_substitution() {
        let mut a = snapshot();
        a.inputs.push(inputs::FrozenInput {
            input: inputs::Input::IslandPoll,
            source_sha256: "a".repeat(64),
            bytecode_sha256: "b".repeat(64),
            bytecode_bytes: 200,
        });
        let mut b: Snapshot = serde_json::from_value(serde_json::to_value(&a).unwrap()).unwrap();
        inputs::matching(&a, &b).unwrap();
        b.inputs[0].bytecode_sha256 = "c".repeat(64);
        assert!(inputs::matching(&a, &b).is_err());
        let case = Probe::Island.cases().remove(0);
        let artifact = Artifact {
            probe: Probe::Island,
            allocations: false,
            features: vec!["jit".into()],
            file: String::new(),
            sha256: String::new(),
        };
        let args = case.arguments(Probe::Island, false, Path::new("/snapshot/0"));
        let mut data =
            json!({"mode":"vm","children":0,"workload":"idle","vob_sha256":"b".repeat(64)});
        case.validate(&artifact, &data, &args, &a).unwrap();
        data["children"] = json!(128);
        assert!(case.validate(&artifact, &data, &args, &a).is_err());
        data["children"] = json!(0);
        data["vob_sha256"] = json!("c".repeat(64));
        assert!(case.validate(&artifact, &data, &args, &a).is_err());
    }

    #[test]
    fn old_snapshot_fields_remain_optional_and_runtime_inputs_are_closed() {
        let mut raw = serde_json::to_value(snapshot()).unwrap();
        raw.as_object_mut().unwrap().remove("inputs");
        raw.as_object_mut().unwrap().remove("input_compiler");
        let legacy: Snapshot = serde_json::from_value(raw).unwrap();
        assert!(legacy.inputs.is_empty());
        assert!(legacy.input_compiler.is_none());
        assert!(inputs::validate(&legacy, Path::new("/unused")).is_ok());
        let mut invalid = legacy;
        invalid.artifacts[0].probe = Probe::Roots;
        assert!(inputs::validate(&invalid, Path::new("/unused")).is_err());
        assert!(serde_json::from_str::<inputs::FrozenInput>(
            r#"{"input":"unknown","source_sha256":"a","bytecode_sha256":"b","bytecode_bytes":1}"#
        )
        .is_err());
    }
}
