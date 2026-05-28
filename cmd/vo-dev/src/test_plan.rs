use crate::test_config::{load_test_config, resolve_target_specs, TestTarget};
use crate::test_manifest::{
    has_target_name, load_manifest, manifest_case_path, parse_case_expect, CaseExpect,
    ManifestCase, ManifestFile,
};
use anyhow::{anyhow, bail, Context, Result};
use serde::Serialize;
use serde_json::json;
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

#[derive(Debug, Serialize)]
pub(crate) struct TestPlan {
    schema: &'static str,
    suite: String,
    pub(crate) jobs: Vec<TestJob>,
}

#[derive(Debug, Serialize)]
pub(crate) struct TestJob {
    pub(crate) id: String,
    case_id: String,
    pub(crate) kind: String,
    pub(crate) path: String,
    pub(crate) target: String,
    backend: String,
    env: BTreeMap<String, String>,
    timeout_sec: u64,
    expect: serde_json::Value,
}

#[derive(Debug)]
pub(crate) struct TestArgs {
    pub(crate) suite: String,
    pub(crate) targets: Vec<String>,
    pub(crate) format: String,
    pub(crate) jobs: Option<usize>,
    pub(crate) paths: Vec<String>,
    pub(crate) verbose: bool,
    pub(crate) release: bool,
}

impl TestArgs {
    pub(crate) fn parse(root: &Path, args: Vec<String>) -> Result<Self> {
        let test_config = load_test_config(root)?;
        let mut suite = "lang".to_string();
        let mut target_specs: Option<Vec<String>> = None;
        let mut format = "text".to_string();
        let mut jobs = None;
        let mut paths = Vec::new();
        let mut verbose = false;
        let mut release = false;
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--release" => {
                    release = true;
                }
                "--suite" => {
                    i += 1;
                    suite = args
                        .get(i)
                        .ok_or_else(|| anyhow!("--suite requires a value"))?
                        .clone();
                }
                arg if arg.starts_with("--suite=") => {
                    suite = arg["--suite=".len()..].to_string();
                }
                "--targets" => {
                    i += 1;
                    target_specs = Some(
                        args.get(i)
                            .ok_or_else(|| anyhow!("--targets requires a value"))?
                            .split(',')
                            .filter(|item| !item.is_empty())
                            .map(ToOwned::to_owned)
                            .collect(),
                    );
                }
                arg if arg.starts_with("--targets=") => {
                    target_specs = Some(
                        arg["--targets=".len()..]
                            .split(',')
                            .filter(|item| !item.is_empty())
                            .map(ToOwned::to_owned)
                            .collect(),
                    );
                }
                "--format" => {
                    i += 1;
                    format = args
                        .get(i)
                        .ok_or_else(|| anyhow!("--format requires a value"))?
                        .clone();
                }
                arg if arg.starts_with("--format=") => {
                    format = arg["--format=".len()..].to_string();
                }
                "-j" | "--jobs" => {
                    i += 1;
                    jobs = Some(
                        args.get(i)
                            .ok_or_else(|| anyhow!("{} requires a value", args[i - 1]))?
                            .parse()
                            .context("invalid jobs value")?,
                    );
                }
                arg if arg.starts_with("--jobs=") => {
                    jobs = Some(
                        arg["--jobs=".len()..]
                            .parse()
                            .context("invalid jobs value")?,
                    );
                }
                arg if arg.starts_with("-j") && arg.len() > 2 => {
                    jobs = Some(arg[2..].parse().context("invalid jobs value")?);
                }
                "--path" => {
                    i += 1;
                    paths.push(
                        args.get(i)
                            .ok_or_else(|| anyhow!("--path requires a value"))?
                            .clone(),
                    );
                }
                arg if arg.starts_with("--path=") => {
                    paths.push(arg["--path=".len()..].to_string());
                }
                "-v" | "--verbose" => {
                    verbose = true;
                }
                other if !other.starts_with('-') => {
                    paths.push(other.to_string());
                }
                other => bail!("unknown test argument: {other}"),
            }
            i += 1;
        }
        if suite != "lang" {
            bail!("only suite=lang is implemented");
        }
        let targets = resolve_target_specs(
            &test_config,
            target_specs.unwrap_or_else(|| test_config.default_targets.clone()),
        )?;
        if targets.is_empty() {
            bail!("--targets cannot be empty");
        }
        if format != "text" && format != "json" {
            bail!("--format must be text or json");
        }
        if jobs == Some(0) {
            bail!("--jobs must be > 0");
        }
        Ok(Self {
            suite,
            targets,
            format,
            jobs,
            paths,
            verbose,
            release,
        })
    }
}

pub(crate) fn build_plan(root: &Path, opts: &TestArgs) -> Result<TestPlan> {
    let test_config = load_test_config(root)?;
    let targets = &test_config.targets;
    let manifest = load_manifest(root)?;
    if manifest.suite != opts.suite {
        bail!(
            "manifest suite {} does not match requested suite {}",
            manifest.suite,
            opts.suite
        );
    }
    let requested: BTreeSet<_> = opts.targets.iter().cloned().collect();
    for target_name in &requested {
        if !targets.contains_key(target_name) {
            bail!("unknown test target: {target_name}");
        }
    }

    let mut jobs = Vec::new();
    let mut matched_cases = 0usize;
    for case in &manifest.cases {
        if !opts.paths.is_empty() && !case_matches_path_filters(root, &manifest, case, &opts.paths)?
        {
            continue;
        }
        matched_cases += 1;
        let expect = parse_case_expect(case)?;
        if expect.kind == "fail" {
            if requested
                .iter()
                .any(|target| target_runs_native_compile_fail(target))
            {
                let target = targets
                    .get("compile")
                    .ok_or_else(|| anyhow!("eng/tests.toml must define target compile"))?;
                jobs.push(compile_fail_job(
                    root, &manifest, case, target, &expect, "compile",
                )?);
            }
            for target_name in &opts.targets {
                let target = targets
                    .get(target_name)
                    .ok_or_else(|| anyhow!("unknown test target: {target_name}"))?;
                if target.kind != "wasm" {
                    continue;
                }
                jobs.push(compile_fail_job(
                    root,
                    &manifest,
                    case,
                    target,
                    &expect,
                    &format!("{target_name}-compile"),
                )?);
            }
            continue;
        }

        for target_name in &opts.targets {
            if !case.targets.iter().any(|target| target == target_name) {
                continue;
            }
            if has_target_name(&case.skip, target_name) {
                continue;
            }
            let target = targets
                .get(target_name)
                .ok_or_else(|| anyhow!("unknown test target: {target_name}"))?;
            jobs.push(TestJob {
                id: format!("{}::{}", case.id, target_name),
                case_id: case.id.clone(),
                kind: case.kind.clone(),
                path: manifest_case_path(root, &manifest, case)?,
                target: target.name.clone(),
                backend: target.backend.clone(),
                env: target.env.clone(),
                timeout_sec: case_timeout(case, target_name, target.default_timeout_sec),
                expect: json!({
                    "kind": "pass",
                    "jit_regular_call_fallbacks_min": expect.jit_regular_call_fallbacks_min,
                }),
            });
        }
    }

    if !opts.paths.is_empty() && matched_cases == 0 {
        bail!("no test cases matched --path {}", opts.paths.join(", "));
    }
    if !opts.paths.is_empty() && jobs.is_empty() {
        bail!(
            "matched --path {}, but no jobs were selected for targets {}",
            opts.paths.join(", "),
            opts.targets.join(",")
        );
    }

    Ok(TestPlan {
        schema: "volang.test-plan.v1",
        suite: opts.suite.clone(),
        jobs,
    })
}

fn case_matches_path_filters(
    root: &Path,
    manifest: &ManifestFile,
    case: &ManifestCase,
    filters: &[String],
) -> Result<bool> {
    let manifest_path = normalize_test_filter(root, &format!("{}/{}", manifest.root, case.path));
    let case_path = normalize_test_filter(root, &case.path);
    let planned_path = normalize_test_filter(root, &manifest_case_path(root, manifest, case)?);
    let candidates = [manifest_path, case_path, planned_path];

    for filter in filters {
        let filter = normalize_test_filter(root, filter);
        if candidates.iter().any(|candidate| {
            path_filter_matches(candidate, &filter) || path_filter_matches(&filter, candidate)
        }) {
            return Ok(true);
        }
    }
    Ok(false)
}

fn path_filter_matches(candidate: &str, filter: &str) -> bool {
    if candidate == filter {
        return true;
    }
    candidate
        .strip_prefix(filter)
        .map(|rest| rest.starts_with('/'))
        .unwrap_or(false)
}

fn normalize_test_filter(root: &Path, raw: &str) -> String {
    let (path_part, suffix) = raw.split_once(':').unwrap_or((raw, ""));
    let path = Path::new(path_part);
    let mut normalized = if path.is_absolute() {
        path.strip_prefix(root)
            .map(|p| p.display().to_string())
            .unwrap_or_else(|_| path_part.to_string())
    } else {
        path_part.to_string()
    };
    normalized = normalized.replace('\\', "/");
    while normalized.starts_with("./") {
        normalized = normalized[2..].to_string();
    }
    normalized = normalized.trim_end_matches('/').to_string();
    if suffix.is_empty() {
        normalized
    } else {
        format!("{normalized}:{suffix}")
    }
}

fn compile_fail_job(
    root: &Path,
    manifest: &ManifestFile,
    case: &ManifestCase,
    target: &TestTarget,
    expect: &CaseExpect,
    suffix: &str,
) -> Result<TestJob> {
    Ok(TestJob {
        id: format!("{}::{suffix}", case.id),
        case_id: case.id.clone(),
        kind: case.kind.clone(),
        path: manifest_case_path(root, manifest, case)?,
        target: target.name.clone(),
        backend: target.backend.clone(),
        env: target.env.clone(),
        timeout_sec: case_timeout(case, &target.name, target.default_timeout_sec),
        expect: json!({ "kind": "fail", "patterns": expect.patterns }),
    })
}

fn case_timeout(case: &ManifestCase, target: &str, default_timeout_sec: u64) -> u64 {
    case.timeout
        .get(target)
        .copied()
        .unwrap_or(default_timeout_sec)
}

fn target_runs_native_compile_fail(target: &str) -> bool {
    matches!(target, "compile" | "vm" | "jit" | "osr" | "nostd")
}
