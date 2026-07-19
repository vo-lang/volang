use crate::test_config::{load_test_config, resolve_target_specs, TestTarget};
use crate::test_manifest::{
    has_target_name, load_manifest, manifest_case_path, parse_case_expect, resolved_case_targets,
    CaseExpect, ManifestCase, ManifestFile,
};
use anyhow::{anyhow, bail, Context, Result};
use serde::Serialize;
use serde_json::json;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

#[derive(Debug, Serialize)]
pub(crate) struct TestPlan {
    schema: &'static str,
    suite: String,
    pub(crate) jobs: Vec<TestJob>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct TestJob {
    pub(crate) id: String,
    case_id: String,
    pub(crate) kind: String,
    pub(crate) path: String,
    pub(crate) target: String,
    backend: String,
    matrix: Option<String>,
    tags: Vec<String>,
    owner: Option<String>,
    env: BTreeMap<String, String>,
    timeout_sec: u64,
    expect: serde_json::Value,
    pub(crate) selection_reasons: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TestShard {
    index: usize,
    total: usize,
}

#[derive(Debug)]
pub(crate) struct TestArgs {
    pub(crate) suite: String,
    pub(crate) targets: Vec<String>,
    pub(crate) targets_explicit: bool,
    pub(crate) matrices: Vec<String>,
    pub(crate) tags: Vec<String>,
    pub(crate) owners: Vec<String>,
    pub(crate) format: String,
    pub(crate) jobs: Option<usize>,
    pub(crate) paths: Vec<String>,
    pub(crate) verbose: bool,
    pub(crate) release: bool,
    pub(crate) explain: bool,
    pub(crate) repeat: usize,
    pub(crate) shard: Option<TestShard>,
}

impl TestArgs {
    pub(crate) fn parse(root: &Path, args: Vec<String>) -> Result<Self> {
        let test_config = load_test_config(root)?;
        let mut suite = "lang".to_string();
        let mut target_specs: Option<Vec<String>> = None;
        let mut matrices = Vec::new();
        let mut tags = Vec::new();
        let mut owners = Vec::new();
        let mut format = "text".to_string();
        let mut jobs = None;
        let mut paths = Vec::new();
        let mut verbose = false;
        let mut release = false;
        let mut explain = false;
        let mut repeat = 1usize;
        let mut shard = None;
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--release" => {
                    release = true;
                }
                "--explain" => {
                    explain = true;
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
                "--matrix" => {
                    i += 1;
                    extend_filter_values(
                        &mut matrices,
                        "--matrix",
                        args.get(i)
                            .ok_or_else(|| anyhow!("--matrix requires a value"))?,
                    )?;
                }
                arg if arg.starts_with("--matrix=") => {
                    extend_filter_values(&mut matrices, "--matrix", &arg["--matrix=".len()..])?;
                }
                "--tag" | "--tags" => {
                    let flag = args[i].clone();
                    i += 1;
                    extend_filter_values(
                        &mut tags,
                        &flag,
                        args.get(i)
                            .ok_or_else(|| anyhow!("{flag} requires a value"))?,
                    )?;
                }
                arg if arg.starts_with("--tag=") => {
                    extend_filter_values(&mut tags, "--tag", &arg["--tag=".len()..])?;
                }
                arg if arg.starts_with("--tags=") => {
                    extend_filter_values(&mut tags, "--tags", &arg["--tags=".len()..])?;
                }
                "--owner" => {
                    i += 1;
                    extend_filter_values(
                        &mut owners,
                        "--owner",
                        args.get(i)
                            .ok_or_else(|| anyhow!("--owner requires a value"))?,
                    )?;
                }
                arg if arg.starts_with("--owner=") => {
                    extend_filter_values(&mut owners, "--owner", &arg["--owner=".len()..])?;
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
                "--repeat" => {
                    i += 1;
                    repeat = args
                        .get(i)
                        .ok_or_else(|| anyhow!("--repeat requires a value"))?
                        .parse()
                        .context("invalid repeat value")?;
                }
                arg if arg.starts_with("--repeat=") => {
                    repeat = arg["--repeat=".len()..]
                        .parse()
                        .context("invalid repeat value")?;
                }
                "--shard" => {
                    i += 1;
                    let value = args
                        .get(i)
                        .ok_or_else(|| anyhow!("--shard requires a value"))?;
                    if shard.is_some() {
                        bail!("--shard may only be specified once");
                    }
                    shard = Some(parse_shard(value)?);
                }
                arg if arg.starts_with("--shard=") => {
                    if shard.is_some() {
                        bail!("--shard may only be specified once");
                    }
                    shard = Some(parse_shard(&arg["--shard=".len()..])?);
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
        let targets_explicit = target_specs.is_some();
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
        if repeat == 0 {
            bail!("--repeat must be > 0");
        }
        normalize_filter_values("matrix", &mut matrices)?;
        normalize_filter_values("tag", &mut tags)?;
        normalize_filter_values("owner", &mut owners)?;
        for matrix in &matrices {
            if !test_config.matrices.contains_key(matrix) {
                bail!("unknown test matrix: {matrix}");
            }
        }
        Ok(Self {
            suite,
            targets,
            targets_explicit,
            matrices,
            tags,
            owners,
            format,
            jobs,
            paths,
            verbose,
            release,
            explain,
            repeat,
            shard,
        })
    }
}

fn parse_shard(value: &str) -> Result<TestShard> {
    let Some((index, total)) = value.split_once('/') else {
        bail!("--shard must use INDEX/TOTAL");
    };
    let index = index.parse::<usize>().context("invalid shard index")?;
    let total = total.parse::<usize>().context("invalid shard count")?;
    if total == 0 {
        bail!("--shard total must be > 0");
    }
    if index == 0 || index > total {
        bail!("--shard index must be between 1 and {total}");
    }
    Ok(TestShard { index, total })
}

fn case_belongs_to_shard(case_id: &str, shard: TestShard) -> bool {
    let digest = Sha256::digest(case_id.as_bytes());
    let prefix = u64::from_be_bytes(
        digest[..8]
            .try_into()
            .expect("SHA-256 always has an eight-byte prefix"),
    );
    let total = u64::try_from(shard.total).expect("test shard count must fit in u64");
    let bucket = usize::try_from(prefix % total).expect("test shard bucket must fit in usize") + 1;
    bucket == shard.index
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
    let effective_targets = effective_targets_for_manifest(root, &test_config, &manifest, opts)?;
    let requested: BTreeSet<_> = effective_targets.iter().cloned().collect();
    for target_name in &requested {
        if !targets.contains_key(target_name) {
            bail!("unknown test target: {target_name}");
        }
    }

    let mut jobs = Vec::new();
    let mut matched_cases = 0usize;
    let mut sharded_cases = 0usize;
    for case in &manifest.cases {
        if !opts.paths.is_empty() && !case_matches_path_filters(root, &manifest, case, &opts.paths)?
        {
            continue;
        }
        if !case_matches_metadata_filters(case, opts) {
            continue;
        }
        matched_cases += 1;
        if opts
            .shard
            .is_some_and(|shard| !case_belongs_to_shard(&case.id, shard))
        {
            continue;
        }
        sharded_cases += 1;
        let expect = parse_case_expect(case)?;
        if expect.kind == "fail" {
            let case_targets = resolved_case_targets(case, &test_config)?;
            if requested
                .iter()
                .any(|target| target_runs_native_compile_fail(target))
                && compile_fail_enabled_for_targets(&case_targets, "compile")
            {
                let target = targets
                    .get("compile")
                    .ok_or_else(|| anyhow!("eng/tests.toml must define target compile"))?;
                jobs.push(compile_fail_job(
                    root,
                    &manifest,
                    case,
                    target,
                    &expect,
                    "compile",
                    &selection_reasons_for_case(case, opts, &case_targets, "compile"),
                )?);
            }
            for target_name in &effective_targets {
                let target = targets
                    .get(target_name)
                    .ok_or_else(|| anyhow!("unknown test target: {target_name}"))?;
                if target.kind != "wasm" {
                    continue;
                }
                if !compile_fail_enabled_for_targets(&case_targets, target_name) {
                    continue;
                }
                jobs.push(compile_fail_job(
                    root,
                    &manifest,
                    case,
                    target,
                    &expect,
                    &format!("{target_name}-compile"),
                    &selection_reasons_for_case(case, opts, &case_targets, target_name),
                )?);
            }
            continue;
        }

        let case_targets = resolved_case_targets(case, &test_config)?;
        for target_name in &effective_targets {
            if !case_targets.iter().any(|target| target == target_name) {
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
                matrix: case.matrix.clone(),
                tags: case.tags.clone(),
                owner: case.owner.clone(),
                env: target.env.clone(),
                timeout_sec: case_timeout(case, target_name, target.default_timeout_sec),
                expect: json!({
                    "kind": "pass",
                    "jit_regular_call_side_exits_min": expect.jit_regular_call_side_exits_min,
                    "jit_loop_entries_min": expect.jit_loop_entries_min,
                }),
                selection_reasons: selection_reasons_for_case(
                    case,
                    opts,
                    &case_targets,
                    target_name,
                ),
            });
        }
    }

    if opts.repeat > 1 {
        let originals = jobs;
        jobs = Vec::with_capacity(originals.len() * opts.repeat);
        for iteration in 1..=opts.repeat {
            for job in &originals {
                let mut repeated = job.clone();
                repeated.id = format!("{}#{}", repeated.id, iteration);
                repeated
                    .selection_reasons
                    .push(format!("repeat iteration {iteration}/{}", opts.repeat));
                jobs.push(repeated);
            }
        }
    }

    let has_case_filters = opts.has_case_filters();
    if has_case_filters && matched_cases == 0 {
        bail!("no test cases matched {}", describe_case_filters(opts));
    }
    if let Some(shard) = opts.shard.filter(|_| sharded_cases == 0) {
        bail!(
            "test shard {}/{} contains no cases after applying case filters",
            shard.index,
            shard.total,
        );
    }
    if has_case_filters && sharded_cases > 0 && jobs.is_empty() {
        bail!(
            "matched {}, but no jobs were selected for targets {}",
            describe_case_filters(opts),
            effective_targets.join(",")
        );
    }

    Ok(TestPlan {
        schema: "volang.test-plan.v1",
        suite: opts.suite.clone(),
        jobs,
    })
}

pub(crate) fn effective_test_targets(root: &Path, opts: &TestArgs) -> Result<Vec<String>> {
    let test_config = load_test_config(root)?;
    let manifest = load_manifest(root)?;
    effective_targets_for_manifest(root, &test_config, &manifest, opts)
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
    selection_reasons: &[String],
) -> Result<TestJob> {
    Ok(TestJob {
        id: format!("{}::{suffix}", case.id),
        case_id: case.id.clone(),
        kind: case.kind.clone(),
        path: manifest_case_path(root, manifest, case)?,
        target: target.name.clone(),
        backend: target.backend.clone(),
        matrix: case.matrix.clone(),
        tags: case.tags.clone(),
        owner: case.owner.clone(),
        env: target.env.clone(),
        timeout_sec: case_timeout(case, &target.name, target.default_timeout_sec),
        expect: json!({ "kind": "fail", "patterns": expect.patterns }),
        selection_reasons: selection_reasons.to_vec(),
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

fn compile_fail_enabled_for_targets(case_targets: &[String], target: &str) -> bool {
    case_targets.is_empty()
        || case_targets.iter().any(|case_target| case_target == target)
        || case_targets
            .iter()
            .any(|case_target| case_target == "compile")
            && target_runs_native_compile_fail(target)
}

fn selection_reasons_for_case(
    case: &ManifestCase,
    opts: &TestArgs,
    case_targets: &[String],
    target_name: &str,
) -> Vec<String> {
    let mut reasons = Vec::new();
    if opts.paths.is_empty()
        && opts.matrices.is_empty()
        && opts.tags.is_empty()
        && opts.owners.is_empty()
    {
        reasons.push("selected by suite defaults".to_string());
    }
    if !opts.paths.is_empty() {
        reasons.push(format!("matched path filter {}", opts.paths.join(",")));
    }
    if !opts.matrices.is_empty() {
        reasons.push(format!("matched matrix filter {}", opts.matrices.join(",")));
    }
    if !opts.tags.is_empty() {
        reasons.push(format!("matched tag filter {}", opts.tags.join(",")));
    }
    if !opts.owners.is_empty() {
        reasons.push(format!("matched owner filter {}", opts.owners.join(",")));
    }
    if let Some(shard) = opts.shard {
        reasons.push(format!(
            "case assigned to shard {}/{}",
            shard.index, shard.total
        ));
    }
    if opts.targets_explicit {
        reasons.push(format!(
            "target {target_name} selected by explicit --targets"
        ));
    } else if let Some(matrix) = &case.matrix {
        reasons.push(format!("target {target_name} selected by matrix {matrix}"));
    } else if !case_targets.is_empty() {
        reasons.push(format!(
            "target {target_name} selected by explicit case target override"
        ));
    }
    if let Some(owner) = &case.owner {
        reasons.push(format!("owned by {owner}"));
    }
    if !case.tags.is_empty() {
        reasons.push(format!("case tags {}", case.tags.join(",")));
    }
    if has_target_name(&case.skip, target_name) {
        reasons.push(format!(
            "target {target_name} skipped because {}",
            case.reason.as_deref().unwrap_or("(missing reason)")
        ));
    }
    if let Some(timeout) = case.timeout.get(target_name) {
        reasons.push(format!("target {target_name} timeout set to {timeout}s"));
    }
    if case
        .expect
        .as_ref()
        .is_some_and(|value| value.as_table().is_some())
    {
        reasons.push(format!(
            "case has structured expectation because {}",
            case.reason
                .as_deref()
                .unwrap_or("the manifest declares one")
        ));
    }
    reasons
}

impl TestArgs {
    fn has_case_filters(&self) -> bool {
        !self.paths.is_empty()
            || !self.matrices.is_empty()
            || !self.tags.is_empty()
            || !self.owners.is_empty()
    }

    fn has_metadata_filters(&self) -> bool {
        !self.matrices.is_empty() || !self.tags.is_empty() || !self.owners.is_empty()
    }
}

fn effective_targets_for_manifest(
    root: &Path,
    test_config: &crate::test_config::TestConfig,
    manifest: &ManifestFile,
    opts: &TestArgs,
) -> Result<Vec<String>> {
    if opts.targets_explicit || !opts.has_metadata_filters() {
        return Ok(opts.targets.clone());
    }

    let mut seen = BTreeSet::new();
    let mut targets = Vec::new();
    for case in &manifest.cases {
        if !opts.paths.is_empty() && !case_matches_path_filters(root, manifest, case, &opts.paths)?
        {
            continue;
        }
        if !case_matches_metadata_filters(case, opts) {
            continue;
        }
        let case_targets = resolved_case_targets(case, test_config)?;
        if parse_case_expect(case)?.kind == "fail" && case_targets.is_empty() {
            if seen.insert("compile".to_string()) {
                targets.push("compile".to_string());
            }
            continue;
        }
        for target in case_targets {
            if seen.insert(target.clone()) {
                targets.push(target);
            }
        }
    }

    if targets.is_empty() {
        Ok(opts.targets.clone())
    } else {
        Ok(targets)
    }
}

fn extend_filter_values(filters: &mut Vec<String>, flag: &str, value: &str) -> Result<()> {
    let values: Vec<_> = value
        .split(',')
        .filter(|item| !item.is_empty())
        .map(ToOwned::to_owned)
        .collect();
    if values.is_empty() {
        bail!("{flag} requires a non-empty value");
    }
    filters.extend(values);
    Ok(())
}

fn normalize_filter_values(field: &str, values: &mut Vec<String>) -> Result<()> {
    let mut seen = BTreeSet::new();
    values.retain(|value| seen.insert(value.clone()));
    for value in values {
        validate_filter_label(field, value)?;
    }
    Ok(())
}

fn validate_filter_label(field: &str, value: &str) -> Result<()> {
    if value.trim().is_empty() {
        bail!("{field} filter cannot be empty");
    }
    if value.trim() != value {
        bail!("{field} filter cannot contain surrounding whitespace");
    }
    if !value
        .chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || matches!(ch, '-' | '_' | '.'))
    {
        bail!("{field} filter {value} must use lowercase ASCII letters, digits, dot, hyphen, or underscore");
    }
    Ok(())
}

fn case_matches_metadata_filters(case: &ManifestCase, opts: &TestArgs) -> bool {
    if !opts.matrices.is_empty()
        && !case
            .matrix
            .as_ref()
            .is_some_and(|matrix| opts.matrices.iter().any(|filter| filter == matrix))
    {
        return false;
    }
    if !opts
        .tags
        .iter()
        .all(|tag| case.tags.iter().any(|case_tag| case_tag == tag))
    {
        return false;
    }
    if !opts.owners.is_empty()
        && !case
            .owner
            .as_ref()
            .is_some_and(|owner| opts.owners.iter().any(|filter| filter == owner))
    {
        return false;
    }
    true
}

fn describe_case_filters(opts: &TestArgs) -> String {
    let mut parts = Vec::new();
    if !opts.paths.is_empty() {
        parts.push(format!("--path {}", opts.paths.join(",")));
    }
    if !opts.matrices.is_empty() {
        parts.push(format!("--matrix {}", opts.matrices.join(",")));
    }
    if !opts.tags.is_empty() {
        parts.push(format!("--tags {}", opts.tags.join(",")));
    }
    if !opts.owners.is_empty() {
        parts.push(format!("--owner {}", opts.owners.join(",")));
    }
    parts.join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn workspace_root() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("workspace root")
    }

    fn assert_two_shards_partition(extra_args: &[&str]) {
        let root = workspace_root();
        let build = |shard: Option<&str>| {
            let mut args = vec![
                "--targets".to_string(),
                "native,gc,embed,compile".to_string(),
            ];
            args.extend(extra_args.iter().map(|arg| (*arg).to_string()));
            if let Some(shard) = shard {
                args.extend(["--shard".to_string(), shard.to_string()]);
            }
            let opts = TestArgs::parse(&root, args).expect("parse test plan arguments");
            build_plan(&root, &opts).expect("build test plan")
        };

        let full = build(None);
        let first = build(Some("1/2"));
        let second = build(Some("2/2"));
        let full_jobs = full
            .jobs
            .iter()
            .map(|job| job.id.clone())
            .collect::<BTreeSet<_>>();
        let first_jobs = first
            .jobs
            .iter()
            .map(|job| job.id.clone())
            .collect::<BTreeSet<_>>();
        let second_jobs = second
            .jobs
            .iter()
            .map(|job| job.id.clone())
            .collect::<BTreeSet<_>>();

        assert!(first_jobs.is_disjoint(&second_jobs));
        assert_eq!(
            first_jobs
                .union(&second_jobs)
                .cloned()
                .collect::<BTreeSet<_>>(),
            full_jobs
        );

        let first_cases = first
            .jobs
            .iter()
            .map(|job| job.case_id.clone())
            .collect::<BTreeSet<_>>();
        let second_cases = second
            .jobs
            .iter()
            .map(|job| job.case_id.clone())
            .collect::<BTreeSet<_>>();
        assert!(first_cases.is_disjoint(&second_cases));
    }

    #[test]
    fn test_plan_job_json_includes_explain_reasons() {
        let plan = TestPlan {
            schema: "volang.test-plan.v1",
            suite: "lang".to_string(),
            jobs: vec![TestJob {
                id: "case::vm".to_string(),
                case_id: "case".to_string(),
                kind: "file".to_string(),
                path: "tests/lang/cases/runtime/case.vo".to_string(),
                target: "vm".to_string(),
                backend: "vm".to_string(),
                matrix: Some("default".to_string()),
                tags: vec!["runtime".to_string()],
                owner: Some("runtime".to_string()),
                env: BTreeMap::new(),
                timeout_sec: 20,
                expect: json!({ "kind": "pass" }),
                selection_reasons: vec!["target vm selected by matrix default".to_string()],
            }],
        };
        let value = serde_json::to_value(plan).unwrap();
        assert_eq!(value["schema"], "volang.test-plan.v1");
        assert_eq!(
            value["jobs"][0]["selection_reasons"][0],
            "target vm selected by matrix default"
        );
    }

    #[test]
    fn shard_values_are_strict_and_one_based() {
        assert_eq!(
            parse_shard("1/2").unwrap(),
            TestShard { index: 1, total: 2 }
        );
        for invalid in ["0/2", "3/2", "1/0", "1", "x/2", "1/x", "1/2/3"] {
            assert!(parse_shard(invalid).is_err(), "{invalid}");
        }
    }

    #[test]
    fn empty_shards_fail_during_plan_construction() {
        let root = workspace_root();
        let manifest = load_manifest(&root).expect("load test manifest");
        let total = manifest.cases.len() + 1;
        let index = (1..=total)
            .find(|index| {
                let shard = TestShard {
                    index: *index,
                    total,
                };
                manifest
                    .cases
                    .iter()
                    .all(|case| !case_belongs_to_shard(&case.id, shard))
            })
            .expect("more shards than cases must leave an empty shard");
        let opts = TestArgs::parse(
            &root,
            vec!["--shard".to_string(), format!("{index}/{total}")],
        )
        .expect("parse empty shard");

        let error = build_plan(&root, &opts).expect_err("empty shard must fail before execution");
        assert!(
            format!("{error:#}").contains(&format!("test shard {index}/{total} contains no cases"))
        );
    }

    #[test]
    fn shards_partition_complete_cases_without_splitting_targets() {
        assert_two_shards_partition(&[]);
    }

    #[test]
    fn shards_compose_with_metadata_filters() {
        assert_two_shards_partition(&["--tags", "smoke"]);
    }
}
