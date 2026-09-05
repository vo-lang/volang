use super::model::{sha256_hex, task_digest, CiTask, FileDigest, SourceIdentity};
use super::plan::{canonical_plan_bytes, read_plan, source_identity, validate_plan, CiPlan};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

const EVIDENCE_SCHEMA: &str = "volang.ci.evidence.v1";
const BUNDLE_SCHEMA: &str = "volang.ci.certification.v1";
const MAX_RESULT_BYTES: u64 = 64 * 1024 * 1024;
const MAX_ARTIFACT_FILES: usize = 16_384;
const MAX_ARTIFACT_ENTRIES: usize = 65_536;
const MAX_ARTIFACT_DEPTH: usize = 64;
const MAX_ARTIFACT_BYTES: u64 = 1024 * 1024 * 1024;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CiEvidence {
    schema: String,
    pub(crate) task_id: String,
    task_definition_sha256: String,
    evidence_kind: String,
    source: SourceIdentity,
    ci_manifest_sha256: String,
    ci_plan_sha256: String,
    toolchain_sha256: String,
    test_manifest_sha256: String,
    runner: RunnerIdentity,
    workflow: WorkflowIdentity,
    started_at_unix_millis: u64,
    finished_at_unix_millis: u64,
    duration_millis: u64,
    results: Vec<FileDigest>,
    artifacts: Vec<FileDigest>,
    passed: bool,
    certifiable: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    execution: Option<super::run::ExecutionReceipt>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RunnerIdentity {
    os: String,
    arch: String,
    image: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct WorkflowIdentity {
    provider: String,
    run_id: String,
    run_attempt: String,
    job: String,
    event: String,
    repository: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CertificationBundle {
    schema: String,
    pub(crate) status: String,
    pub(crate) profile: String,
    source: SourceIdentity,
    ci_manifest_sha256: String,
    ci_plan_sha256: String,
    toolchain_sha256: String,
    test_manifest_sha256: String,
    certified_at_unix_millis: u64,
    pub(crate) plan: CiPlan,
    pub(crate) evidence: Vec<CiEvidence>,
}

pub(crate) struct RecordOptions<'a> {
    pub(crate) plan_path: &'a Path,
    pub(crate) task_id: &'a str,
    pub(crate) output: &'a Path,
}

pub(crate) fn record(root: &Path, options: RecordOptions<'_>) -> Result<()> {
    record_inner(root, options, None)
}

pub(super) fn record_execution(
    root: &Path,
    options: RecordOptions<'_>,
    execution: super::run::ExecutionReceipt,
) -> Result<()> {
    record_inner(root, options, Some(execution))
}

fn record_inner(
    root: &Path,
    options: RecordOptions<'_>,
    execution: Option<super::run::ExecutionReceipt>,
) -> Result<()> {
    let (plan, plan_bytes) = read_plan(root, options.plan_path)?;
    let task = plan
        .tasks
        .iter()
        .find(|task| task.id == options.task_id)
        .ok_or_else(|| anyhow!("task {} is absent from the CI plan", options.task_id))?;
    if !task.commands.is_empty() && execution.is_none() {
        bail!(
            "task {} requires ci run; arbitrary ci record is disabled",
            task.id
        );
    }
    let source = source_identity(root)?;
    if source.commit != plan.source.commit || source.tree != plan.source.tree {
        bail!("CI evidence source does not match the immutable plan source");
    }
    if source.tracked_dirty {
        let status = std::process::Command::new("git")
            .args(["status", "--short", "--untracked-files=all"])
            .current_dir(root)
            .output()?;
        bail!(
            "CI evidence cannot certify a dirty source worktree:\n{}",
            String::from_utf8_lossy(&status.stdout)
        );
    }

    let workflow = workflow_identity()?;
    let github_sha = env::var("GITHUB_SHA").context("GITHUB_SHA is required in GitHub Actions")?;
    if github_sha != source.commit {
        bail!("GITHUB_SHA does not match checked-out source commit");
    }
    if workflow.job != task.workflow_job {
        bail!(
            "CI task {} belongs to workflow job {}, current job is {}",
            task.id,
            task.workflow_job,
            workflow.job
        );
    }
    let runner = runner_identity();
    validate_runner_for_task(task, &runner)?;

    let results = task
        .results
        .iter()
        .map(|path| {
            validate_result(root, path)?;
            digest_path(root, path)
        })
        .collect::<Result<Vec<_>>>()?;
    let artifacts = task
        .artifacts
        .iter()
        .map(|path| digest_path(root, path))
        .collect::<Result<Vec<_>>>()?;

    let finished_at_unix_millis = unix_millis(SystemTime::now())?;
    let started_at_unix_millis = execution
        .as_ref()
        .map(|receipt| receipt.started_at_unix_millis)
        .unwrap_or_else(|| {
            env::var("VO_CI_STARTED_AT")
                .ok()
                .and_then(|value| value.parse::<u64>().ok())
                .and_then(|seconds| seconds.checked_mul(1000))
                .unwrap_or(finished_at_unix_millis)
        });
    if started_at_unix_millis > finished_at_unix_millis {
        bail!("VO_CI_STARTED_AT is later than the evidence finish time");
    }
    let evidence = CiEvidence {
        schema: EVIDENCE_SCHEMA.to_string(),
        task_id: task.id.clone(),
        task_definition_sha256: task_digest(task)?,
        evidence_kind: task.evidence_kind.clone(),
        source,
        ci_manifest_sha256: plan.manifest_sha256.clone(),
        ci_plan_sha256: sha256_hex(&plan_bytes),
        toolchain_sha256: input_set_digest(
            root,
            &["rust-toolchain.toml", "eng/toolchains.toml", "Cargo.lock"],
        )?,
        test_manifest_sha256: input_set_digest(
            root,
            &["eng/tests.toml", "tests/lang/manifest.toml"],
        )?,
        runner,
        workflow,
        started_at_unix_millis,
        finished_at_unix_millis,
        duration_millis: finished_at_unix_millis - started_at_unix_millis,
        results,
        artifacts,
        passed: true,
        certifiable: true,
        execution,
    };
    validate_evidence(root, &plan, &plan_bytes, task, &evidence)?;
    write_json(options.output, &evidence)
}

pub(crate) fn certify(
    root: &Path,
    plan_path: &Path,
    evidence_dir: &Path,
    output: &Path,
) -> Result<CertificationBundle> {
    let (plan, plan_bytes) = read_plan(root, plan_path)?;
    if plan.source.tracked_dirty {
        bail!("a CI plan from a tracked dirty worktree cannot be certified");
    }
    let mut by_task = BTreeMap::<String, CiEvidence>::new();
    for path in evidence_files(evidence_dir)? {
        let bytes = fs::read(&path)
            .with_context(|| format!("could not read CI evidence {}", path.display()))?;
        if bytes.len() > 16 * 1024 * 1024 {
            bail!("CI evidence exceeds 16 MiB: {}", path.display());
        }
        let evidence: CiEvidence = serde_json::from_slice(&bytes)
            .with_context(|| format!("could not parse CI evidence {}", path.display()))?;
        let task = plan
            .tasks
            .iter()
            .find(|task| task.id == evidence.task_id)
            .ok_or_else(|| anyhow!("evidence references unplanned task {}", evidence.task_id))?;
        validate_evidence(root, &plan, &plan_bytes, task, &evidence)?;
        if env::var("GITHUB_ACTIONS").as_deref() == Ok("true")
            && (evidence.workflow.run_id != required_env("GITHUB_RUN_ID")?
                || evidence.workflow.run_attempt != required_env("GITHUB_RUN_ATTEMPT")?)
        {
            bail!("certification cannot reuse evidence from a different workflow run or attempt");
        }
        if by_task.insert(evidence.task_id.clone(), evidence).is_some() {
            bail!("duplicate CI evidence for task {}", task.id);
        }
    }
    let expected = plan
        .tasks
        .iter()
        .map(|task| task.id.as_str())
        .collect::<BTreeSet<_>>();
    let actual = by_task.keys().map(String::as_str).collect::<BTreeSet<_>>();
    if expected != actual {
        let missing = expected.difference(&actual).copied().collect::<Vec<_>>();
        let extra = actual.difference(&expected).copied().collect::<Vec<_>>();
        bail!(
            "CI evidence coverage mismatch; missing=[{}], extra=[{}]",
            missing.join(","),
            extra.join(",")
        );
    }
    let mut evidence = Vec::with_capacity(plan.tasks.len());
    for task in &plan.tasks {
        evidence.push(
            by_task
                .remove(&task.id)
                .expect("evidence coverage was checked"),
        );
    }
    let bundle = CertificationBundle {
        schema: BUNDLE_SCHEMA.to_string(),
        status: "certified".to_string(),
        profile: plan.profile.clone(),
        source: plan.source.clone(),
        ci_manifest_sha256: plan.manifest_sha256.clone(),
        ci_plan_sha256: sha256_hex(&plan_bytes),
        toolchain_sha256: evidence[0].toolchain_sha256.clone(),
        test_manifest_sha256: evidence[0].test_manifest_sha256.clone(),
        certified_at_unix_millis: unix_millis(SystemTime::now())?,
        plan,
        evidence,
    };
    validate_bundle(root, &bundle, None)?;
    write_json(output, &bundle)?;
    Ok(bundle)
}

pub(crate) fn read_and_verify_bundle(
    root: &Path,
    path: &Path,
    expected_profile: Option<&str>,
) -> Result<CertificationBundle> {
    let bytes = fs::read(path).with_context(|| format!("could not read {}", path.display()))?;
    if bytes.len() > 64 * 1024 * 1024 {
        bail!("CI certification bundle exceeds 64 MiB: {}", path.display());
    }
    let bundle: CertificationBundle = serde_json::from_slice(&bytes)
        .with_context(|| format!("could not parse {}", path.display()))?;
    validate_bundle(root, &bundle, expected_profile)?;
    Ok(bundle)
}

pub(crate) fn verify_artifact(
    root: &Path,
    bundle: &CertificationBundle,
    task_id: &str,
    artifact: &Path,
) -> Result<()> {
    let evidence = bundle
        .evidence
        .iter()
        .find(|evidence| evidence.task_id == task_id)
        .ok_or_else(|| anyhow!("certification bundle has no evidence for task {task_id}"))?;
    if evidence.artifacts.len() != 1 {
        bail!("task {task_id} must certify exactly one promotable artifact");
    }
    let actual = digest_absolute_path(root, artifact, &evidence.artifacts[0].path)?;
    if actual != evidence.artifacts[0] {
        bail!("promoted artifact digest does not match task {task_id} evidence");
    }
    Ok(())
}

pub(crate) fn require_ui_evidence(bundle: &CertificationBundle) -> Result<()> {
    let tasks = bundle
        .evidence
        .iter()
        .map(|evidence| evidence.task_id.as_str())
        .collect::<BTreeSet<_>>();
    let web = tasks.contains("wasm-web-full");
    let linux = tasks.contains("ui-platform-linux-full");
    let macos = tasks.contains("ui-platform-macos-full");
    let windows = tasks.contains("ui-platform-windows-full");
    if !(web && linux && macos && windows) {
        bail!("CI bundle lacks the full Web, Linux, macOS, and Windows UI evidence set");
    }
    Ok(())
}

fn validate_bundle(
    root: &Path,
    bundle: &CertificationBundle,
    expected_profile: Option<&str>,
) -> Result<()> {
    if bundle.schema != BUNDLE_SCHEMA || bundle.status != "certified" {
        bail!("invalid CI certification bundle status or schema");
    }
    if expected_profile.is_some_and(|profile| bundle.profile != profile) {
        bail!(
            "CI certification profile {} does not match expected {}",
            bundle.profile,
            expected_profile.unwrap_or_default()
        );
    }
    validate_plan(root, &bundle.plan)?;
    if bundle.profile != bundle.plan.profile
        || bundle.source != bundle.plan.source
        || bundle.ci_manifest_sha256 != bundle.plan.manifest_sha256
    {
        bail!("CI certification bundle identity differs from its embedded plan");
    }
    let plan_bytes = canonical_plan_bytes(&bundle.plan)?;
    if bundle.ci_plan_sha256 != sha256_hex(&plan_bytes) {
        bail!("CI certification plan digest is invalid");
    }
    let current = source_identity(root)?;
    if current.commit != bundle.source.commit
        || current.tree != bundle.source.tree
        || current.tracked_dirty
    {
        bail!("CI certification bundle belongs to different source");
    }
    let mut actual = BTreeSet::new();
    let mut workflow_identity = None;
    let mut latest_finish = 0u64;
    for evidence in &bundle.evidence {
        let task = bundle
            .plan
            .tasks
            .iter()
            .find(|task| task.id == evidence.task_id)
            .ok_or_else(|| {
                anyhow!(
                    "bundle evidence references unknown task {}",
                    evidence.task_id
                )
            })?;
        validate_evidence(root, &bundle.plan, &plan_bytes, task, evidence)?;
        if evidence.toolchain_sha256 != bundle.toolchain_sha256
            || evidence.test_manifest_sha256 != bundle.test_manifest_sha256
        {
            bail!(
                "bundle evidence input digests disagree for task {}",
                task.id
            );
        }
        let identity = (
            evidence.workflow.run_id.as_str(),
            evidence.workflow.run_attempt.as_str(),
            evidence.workflow.repository.as_str(),
            evidence.workflow.event.as_str(),
        );
        if workflow_identity.is_some_and(|expected| expected != identity) {
            bail!("bundle mixes evidence from different workflow runs");
        }
        workflow_identity = Some(identity);
        latest_finish = latest_finish.max(evidence.finished_at_unix_millis);
        if !actual.insert(evidence.task_id.as_str()) {
            bail!("bundle contains duplicate evidence for task {}", task.id);
        }
    }
    let expected = bundle
        .plan
        .tasks
        .iter()
        .map(|task| task.id.as_str())
        .collect::<BTreeSet<_>>();
    if actual != expected {
        bail!("bundle evidence does not cover its complete CI plan");
    }
    if bundle.certified_at_unix_millis < latest_finish {
        bail!("CI certification predates task completion");
    }
    Ok(())
}

fn validate_evidence(
    root: &Path,
    plan: &CiPlan,
    plan_bytes: &[u8],
    task: &CiTask,
    evidence: &CiEvidence,
) -> Result<()> {
    match (&evidence.execution, task.commands.is_empty()) {
        (Some(receipt), false) => {
            super::run::validate_receipt(root, plan, task, receipt)?;
            if receipt.results != evidence.results || receipt.artifacts != evidence.artifacts {
                bail!(
                    "execution and certification output digests differ for {}",
                    task.id
                );
            }
        }
        (None, true) => {}
        _ => bail!("CI task {} execution contract mismatch", task.id),
    }
    if evidence.schema != EVIDENCE_SCHEMA
        || !evidence.passed
        || !evidence.certifiable
        || evidence.task_id != task.id
        || evidence.evidence_kind != task.evidence_kind
        || evidence.task_definition_sha256 != task_digest(task)?
        || evidence.source != plan.source
        || evidence.source.tracked_dirty
        || evidence.ci_manifest_sha256 != plan.manifest_sha256
        || evidence.ci_plan_sha256 != sha256_hex(plan_bytes)
    {
        bail!("invalid or uncertifiable CI evidence for task {}", task.id);
    }
    if evidence.finished_at_unix_millis < evidence.started_at_unix_millis
        || evidence.duration_millis
            != evidence.finished_at_unix_millis - evidence.started_at_unix_millis
    {
        bail!("CI evidence has invalid timing for task {}", task.id);
    }
    if evidence.workflow.provider != "github-actions"
        || evidence.workflow.job != task.workflow_job
        || !ascii_digits(&evidence.workflow.run_id, 32)
        || !ascii_digits(&evidence.workflow.run_attempt, 16)
        || !github_repository(&evidence.workflow.repository)
        || !safe_identity_value(&evidence.workflow.event, 64)
    {
        bail!(
            "CI evidence has invalid workflow identity for task {}",
            task.id
        );
    }
    validate_runner_for_task(task, &evidence.runner)?;
    let expected_results = task.results.iter().map(String::as_str).collect::<Vec<_>>();
    let actual_results = evidence
        .results
        .iter()
        .map(|digest| digest.path.as_str())
        .collect::<Vec<_>>();
    if actual_results != expected_results {
        bail!("CI evidence result list differs for task {}", task.id);
    }
    if evidence
        .results
        .iter()
        .any(|digest| digest.kind != "file" || digest.size == 0 || digest.size > MAX_RESULT_BYTES)
    {
        bail!(
            "CI evidence has invalid result metadata for task {}",
            task.id
        );
    }
    let expected_artifacts = task
        .artifacts
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>();
    let actual_artifacts = evidence
        .artifacts
        .iter()
        .map(|digest| digest.path.as_str())
        .collect::<Vec<_>>();
    if actual_artifacts != expected_artifacts {
        bail!("CI evidence artifact list differs for task {}", task.id);
    }
    if evidence
        .artifacts
        .iter()
        .any(|digest| digest.size == 0 || digest.size > MAX_ARTIFACT_BYTES)
    {
        bail!(
            "CI evidence has invalid artifact metadata for task {}",
            task.id
        );
    }
    let toolchain = input_set_digest(
        root,
        &["rust-toolchain.toml", "eng/toolchains.toml", "Cargo.lock"],
    )?;
    let tests = input_set_digest(root, &["eng/tests.toml", "tests/lang/manifest.toml"])?;
    if evidence.toolchain_sha256 != toolchain || evidence.test_manifest_sha256 != tests {
        bail!("CI evidence input digest differs for task {}", task.id);
    }
    for digest in evidence.results.iter().chain(&evidence.artifacts) {
        validate_file_digest(digest)?;
    }
    Ok(())
}

fn validate_runner_for_task(task: &CiTask, runner: &RunnerIdentity) -> Result<()> {
    let expected_os = if task.runner.starts_with("ubuntu-") {
        "Linux"
    } else if task.runner.starts_with("macos-") {
        "macOS"
    } else if task.runner.starts_with("windows-") {
        "Windows"
    } else {
        bail!("unsupported runner label {}", task.runner);
    };
    if runner.os != expected_os {
        bail!(
            "CI task {} requires {}, current runner reports {}",
            task.id,
            task.runner,
            runner.os
        );
    }
    if !safe_identity_value(&runner.arch, 64) || !safe_identity_value(&runner.image, 256) {
        bail!("CI task {} has invalid runner identity", task.id);
    }
    Ok(())
}

fn ascii_digits(value: &str, max_len: usize) -> bool {
    !value.is_empty() && value.len() <= max_len && value.bytes().all(|byte| byte.is_ascii_digit())
}

fn safe_identity_value(value: &str, max_len: usize) -> bool {
    !value.is_empty()
        && value.len() <= max_len
        && !value.bytes().any(|byte| byte.is_ascii_control())
}

fn github_repository(value: &str) -> bool {
    value.len() <= 200
        && value.split('/').count() == 2
        && value.split('/').all(|part| {
            !part.is_empty()
                && part
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
        })
}

fn workflow_identity() -> Result<WorkflowIdentity> {
    if env::var("GITHUB_ACTIONS").as_deref() != Ok("true") {
        bail!("certifiable evidence must be recorded inside GitHub Actions");
    }
    Ok(WorkflowIdentity {
        provider: "github-actions".to_string(),
        run_id: required_env("GITHUB_RUN_ID")?,
        run_attempt: required_env("GITHUB_RUN_ATTEMPT")?,
        job: required_env("GITHUB_JOB")?,
        event: required_env("GITHUB_EVENT_NAME")?,
        repository: required_env("GITHUB_REPOSITORY")?,
    })
}

fn runner_identity() -> RunnerIdentity {
    RunnerIdentity {
        os: env::var("RUNNER_OS").unwrap_or_else(|_| env::consts::OS.to_string()),
        arch: env::var("RUNNER_ARCH").unwrap_or_else(|_| env::consts::ARCH.to_string()),
        image: env::var("ImageOS")
            .or_else(|_| env::var("RUNNER_IMAGE"))
            .unwrap_or_else(|_| "local".to_string()),
    }
}

fn required_env(name: &str) -> Result<String> {
    let value = env::var(name).with_context(|| format!("{name} is required"))?;
    if value.is_empty() || value.bytes().any(|byte| byte.is_ascii_control()) {
        bail!("{name} has an invalid value");
    }
    Ok(value)
}

pub(super) fn validate_result(root: &Path, relative: &str) -> Result<()> {
    let path = root.join(relative);
    let metadata = fs::symlink_metadata(&path)
        .with_context(|| format!("required CI result is missing: {}", path.display()))?;
    if !metadata.file_type().is_file() || metadata.len() == 0 || metadata.len() > MAX_RESULT_BYTES {
        bail!(
            "CI result must be a bounded regular file: {}",
            path.display()
        );
    }
    let bytes = fs::read(&path).with_context(|| format!("could not read {}", path.display()))?;
    let value: serde_json::Value = serde_json::from_slice(&bytes)
        .with_context(|| format!("CI result must be JSON: {}", path.display()))?;
    let object = value
        .as_object()
        .ok_or_else(|| anyhow!("CI result must be a JSON object: {}", path.display()))?;
    match object.get("schema").and_then(serde_json::Value::as_str) {
        Some("volang.test-result.v1") => {
            let jobs = value["jobs"]
                .as_array()
                .ok_or_else(|| anyhow!("test result has no jobs: {relative}"))?;
            let mut ids = BTreeSet::new();
            if jobs.is_empty()
                || value["suite"] != "lang"
                || value["failed"] != 0
                || value["skipped"] != 0
                || value["passed"].as_u64() != Some(jobs.len() as u64)
            {
                bail!("test result counters do not prove complete success: {relative}");
            }
            for job in jobs {
                let id = job["id"]
                    .as_str()
                    .ok_or_else(|| anyhow!("test result lacks job identity: {relative}"))?;
                if id.is_empty()
                    || !ids.insert(id)
                    || job["status"] != "passed"
                    || ["case_id", "target", "backend", "path"]
                        .iter()
                        .any(|field| job[field].as_str().is_none_or(str::is_empty))
                {
                    bail!("test result has duplicate, failed, or unidentified jobs: {relative}");
                }
            }
        }
        Some("volang.browser-result.v1") => {
            let report = &value["report"];
            let has_checks = report["checks"]
                .as_array()
                .is_some_and(|checks| !checks.is_empty());
            let has_checkpoints = report["checkpoints"]
                .as_object()
                .is_some_and(|checks| !checks.is_empty());
            if value["passed"] != true
                || report["passed"] != true
                || report["complete"] != true
                || !(has_checks || has_checkpoints)
            {
                bail!("browser result does not prove completed scenario assertions: {relative}");
            }
        }
        Some("volang.dependency-result.v1") => {
            let policy: serde_json::Value =
                serde_json::from_slice(&fs::read(root.join("eng/dependency-policy.json"))?)?;
            let required = ["rust_lockfiles", "npm_workspaces"]
                .into_iter()
                .flat_map(|key| policy[key].as_array().into_iter().flatten())
                .map(|entry| entry.as_str().unwrap_or_default())
                .collect::<BTreeSet<_>>();
            let audited = value["audited"]
                .as_array()
                .ok_or_else(|| anyhow!("dependency result lacks audited inputs: {relative}"))?;
            let executions = value["executions"]
                .as_array()
                .ok_or_else(|| anyhow!("dependency result lacks executions: {relative}"))?;
            let actual = audited
                .iter()
                .filter_map(serde_json::Value::as_str)
                .collect::<BTreeSet<_>>();
            let executed = executions
                .iter()
                .filter_map(|entry| entry["input"].as_str())
                .collect::<BTreeSet<_>>();
            if value["passed"] != true
                || value["complete"] != true
                || value["failures"]
                    .as_array()
                    .is_none_or(|failures| !failures.is_empty())
                || required.is_empty()
                || required.contains("")
                || actual != required
                || audited.len() != required.len()
                || executed != required
                || executions.len() != required.len()
                || executions.iter().any(|entry| {
                    entry["code"] != 0
                        || !entry["signal"].is_null()
                        || entry["duration_ms"].as_u64().is_none()
                })
            {
                bail!("dependency result does not prove complete successful audit coverage: {relative}");
            }
        }
        Some("volang.ui.performance.v1") => {
            if value["passed"] != true
                || value["profile"] != "release"
                || value["target"].as_str().is_none_or(str::is_empty)
                || value["direct_scalar_allocations"] != 0
                || [
                    "frame_samples",
                    "interaction_samples",
                    "component_samples",
                    "component_rows",
                ]
                .iter()
                .any(|field| value[field].as_u64().is_none_or(|count| count == 0))
            {
                bail!("UI performance result lacks successful release measurements: {relative}");
            }
            for metric in ["frame", "interaction", "component"] {
                let values = [50, 95, 99]
                    .map(|percentile| value[format!("{metric}_p{percentile}_ns")].as_u64());
                let [Some(p50), Some(p95), Some(p99)] = values else {
                    bail!("UI performance result lacks {metric} percentiles: {relative}");
                };
                if p50 > p95 || p95 > p99 {
                    bail!(
                        "UI performance result has inconsistent {metric} percentiles: {relative}"
                    );
                }
            }
        }
        _ => bail!("unsupported CI domain result schema: {relative}"),
    }
    Ok(())
}

pub(super) fn digest_path(root: &Path, relative: &str) -> Result<FileDigest> {
    digest_absolute_path(root, &root.join(relative), relative)
}

fn digest_absolute_path(root: &Path, path: &Path, logical_path: &str) -> Result<FileDigest> {
    let metadata = fs::symlink_metadata(path)
        .with_context(|| format!("required CI artifact is missing: {}", path.display()))?;
    if metadata.file_type().is_symlink() {
        bail!("CI artifacts cannot contain symlinks: {}", path.display());
    }
    if metadata.file_type().is_file() {
        let (size, sha256) = hash_regular_file(path, MAX_ARTIFACT_BYTES, None)?;
        return Ok(FileDigest {
            path: logical_path.to_string(),
            kind: "file".to_string(),
            size,
            sha256: format!("{sha256:x}"),
        });
    }
    if !metadata.file_type().is_dir() {
        bail!("CI artifact has unsupported type: {}", path.display());
    }
    let mut files = Vec::new();
    let mut entries_seen = 0usize;
    let mut total = 0u64;
    collect_regular_files(path, path, &mut files, &mut entries_seen, &mut total, 0)?;
    if files.is_empty() {
        bail!("CI artifact directory is empty: {}", path.display());
    }
    files.sort_by(|left, right| left.0.cmp(&right.0));
    let mut hasher = Sha256::new();
    hasher.update(b"volang.ci.directory.v1\0");
    for (relative, file, expected_size) in files {
        let (size, sha256) = hash_regular_file(&file, expected_size, Some(expected_size))?;
        hasher.update((relative.len() as u64).to_le_bytes());
        hasher.update(relative.as_bytes());
        hasher.update(size.to_le_bytes());
        hasher.update(sha256);
    }
    let _ = root;
    Ok(FileDigest {
        path: logical_path.to_string(),
        kind: "directory".to_string(),
        size: total,
        sha256: format!("{:x}", hasher.finalize()),
    })
}

fn collect_regular_files(
    base: &Path,
    directory: &Path,
    files: &mut Vec<(String, PathBuf, u64)>,
    entries_seen: &mut usize,
    total_bytes: &mut u64,
    depth: usize,
) -> Result<()> {
    if depth > MAX_ARTIFACT_DEPTH {
        bail!(
            "CI artifact directory nesting exceeds {MAX_ARTIFACT_DEPTH} levels: {}",
            base.display()
        );
    }
    let mut entries = fs::read_dir(directory)
        .with_context(|| format!("could not read {}", directory.display()))?
        .collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.file_name());
    for entry in entries {
        *entries_seen += 1;
        if *entries_seen > MAX_ARTIFACT_ENTRIES {
            bail!(
                "CI artifact directory contains more than {MAX_ARTIFACT_ENTRIES} entries: {}",
                base.display()
            );
        }
        let path = entry.path();
        let metadata = fs::symlink_metadata(&path)?;
        if metadata.file_type().is_symlink() {
            bail!("CI artifact directory contains symlink: {}", path.display());
        }
        if metadata.file_type().is_dir() {
            collect_regular_files(base, &path, files, entries_seen, total_bytes, depth + 1)?;
        } else if metadata.file_type().is_file() {
            if files.len() >= MAX_ARTIFACT_FILES {
                bail!(
                    "CI artifact directory contains too many files: {}",
                    base.display()
                );
            }
            let relative = path
                .strip_prefix(base)
                .expect("collected artifact stays below its root")
                .to_string_lossy()
                .replace('\\', "/");
            *total_bytes = total_bytes
                .checked_add(metadata.len())
                .ok_or_else(|| anyhow!("artifact size overflow"))?;
            if *total_bytes > MAX_ARTIFACT_BYTES {
                bail!("CI artifact directory exceeds 1 GiB: {}", base.display());
            }
            files.push((relative, path, metadata.len()));
        } else {
            bail!(
                "CI artifact directory contains unsupported entry: {}",
                path.display()
            );
        }
    }
    Ok(())
}

fn hash_regular_file(
    path: &Path,
    max_bytes: u64,
    expected_size: Option<u64>,
) -> Result<(u64, sha2::digest::Output<Sha256>)> {
    let metadata = fs::symlink_metadata(path)
        .with_context(|| format!("could not inspect {}", path.display()))?;
    if !metadata.file_type().is_file() {
        bail!("CI artifact changed type while hashing: {}", path.display());
    }
    if metadata.len() > max_bytes {
        bail!(
            "CI artifact file exceeds its byte budget: {}",
            path.display()
        );
    }
    if expected_size.is_some_and(|size| size != metadata.len()) {
        bail!("CI artifact changed size while hashing: {}", path.display());
    }
    let mut file =
        fs::File::open(path).with_context(|| format!("could not open {}", path.display()))?;
    let opened = file
        .metadata()
        .with_context(|| format!("could not inspect opened artifact {}", path.display()))?;
    if !opened.file_type().is_file() || opened.len() != metadata.len() {
        bail!("CI artifact changed while opening: {}", path.display());
    }

    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 64 * 1024];
    let mut total = 0u64;
    loop {
        let read = file
            .read(&mut buffer)
            .with_context(|| format!("could not read {}", path.display()))?;
        if read == 0 {
            break;
        }
        total = total
            .checked_add(u64::try_from(read).context("artifact size overflow")?)
            .ok_or_else(|| anyhow!("artifact size overflow"))?;
        if total > max_bytes {
            bail!(
                "CI artifact file exceeds its byte budget: {}",
                path.display()
            );
        }
        hasher.update(&buffer[..read]);
    }
    if total != metadata.len() {
        bail!("CI artifact changed size while hashing: {}", path.display());
    }
    Ok((total, hasher.finalize()))
}

pub(super) fn validate_file_digest(digest: &FileDigest) -> Result<()> {
    if !matches!(digest.kind.as_str(), "file" | "directory")
        || digest.path.is_empty()
        || digest.sha256.len() != 64
        || !digest
            .sha256
            .bytes()
            .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
    {
        bail!("invalid CI file digest for {}", digest.path);
    }
    Ok(())
}

fn input_set_digest(root: &Path, paths: &[&str]) -> Result<String> {
    let mut hasher = Sha256::new();
    hasher.update(b"volang.ci.inputs.v1\0");
    for relative in paths {
        let path = root.join(relative);
        let metadata = fs::symlink_metadata(&path)
            .with_context(|| format!("could not inspect CI input {relative}"))?;
        if !metadata.file_type().is_file() || metadata.len() > 64 * 1024 * 1024 {
            bail!("CI input must be a bounded regular file: {relative}");
        }
        let bytes =
            fs::read(&path).with_context(|| format!("could not read CI input {relative}"))?;
        hasher.update((relative.len() as u64).to_le_bytes());
        hasher.update(relative.as_bytes());
        hasher.update((bytes.len() as u64).to_le_bytes());
        hasher.update(&bytes);
    }
    Ok(format!("{:x}", hasher.finalize()))
}

fn evidence_files(directory: &Path) -> Result<Vec<PathBuf>> {
    fn collect(
        directory: &Path,
        files: &mut Vec<PathBuf>,
        entries_seen: &mut usize,
        depth: usize,
    ) -> Result<()> {
        if depth > 16 {
            bail!("CI evidence directory nesting exceeds 16 levels");
        }
        let mut entries = fs::read_dir(directory)
            .with_context(|| format!("could not read {}", directory.display()))?
            .collect::<std::io::Result<Vec<_>>>()?;
        entries.sort_by_key(|entry| entry.file_name());
        for entry in entries {
            *entries_seen += 1;
            if *entries_seen > 1024 {
                bail!("CI evidence directory contains more than 1024 entries");
            }
            let path = entry.path();
            let kind = entry.file_type()?;
            if kind.is_symlink() {
                bail!("CI evidence directory contains symlink: {}", path.display());
            }
            if kind.is_dir() {
                collect(&path, files, entries_seen, depth + 1)?;
            } else if kind.is_file()
                && path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name.ends_with(".evidence.json"))
            {
                files.push(path);
            }
        }
        Ok(())
    }
    let mut files = Vec::new();
    let mut entries_seen = 0usize;
    collect(directory, &mut files, &mut entries_seen, 0)?;
    files.sort();
    if files.is_empty() {
        bail!("CI evidence directory contains no *.evidence.json files");
    }
    Ok(files)
}

pub(super) fn unix_millis(time: SystemTime) -> Result<u64> {
    let millis = time
        .duration_since(UNIX_EPOCH)
        .context("system time predates Unix epoch")?
        .as_millis();
    u64::try_from(millis).context("system timestamp overflow")
}

pub(super) fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    let mut bytes = serde_json::to_vec_pretty(value)?;
    bytes.push(b'\n');
    let parent = path
        .parent()
        .ok_or_else(|| anyhow!("output path has no parent: {}", path.display()))?;
    fs::create_dir_all(parent).with_context(|| format!("could not create {}", parent.display()))?;
    let temporary = parent.join(format!(
        ".{}.{}.tmp",
        path.file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("ci"),
        std::process::id()
    ));
    fs::write(&temporary, bytes)
        .with_context(|| format!("could not write {}", temporary.display()))?;
    fs::rename(&temporary, path)
        .with_context(|| format!("could not replace {}", path.display()))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temporary_test_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("test clock follows the Unix epoch")
            .as_nanos();
        env::temp_dir().join(format!(
            "volang-ci-evidence-{label}-{}-{nonce}",
            std::process::id()
        ))
    }

    #[test]
    fn domain_evidence_rejects_generic_and_incomplete_success() {
        let root = temporary_test_dir("domain");
        fs::create_dir_all(&root).unwrap();
        for invalid in [
            serde_json::json!({"passed": true}),
            serde_json::json!({"result": "success"}),
            serde_json::json!({"schema": "volang.test-result.v1", "suite": "lang", "passed": 1, "failed": 0, "skipped": 0, "jobs": []}),
            serde_json::json!({"schema": "volang.browser-result.v1", "passed": true, "report": {"passed": true, "complete": true}}),
        ] {
            fs::write(
                root.join("result.json"),
                serde_json::to_vec(&invalid).unwrap(),
            )
            .unwrap();
            assert!(validate_result(&root, "result.json").is_err());
        }
        let valid = serde_json::json!({"schema": "volang.browser-result.v1", "passed": true, "report": {"passed": true, "complete": true, "checks": ["input contract"]}});
        fs::write(
            root.join("result.json"),
            serde_json::to_vec(&valid).unwrap(),
        )
        .unwrap();
        validate_result(&root, "result.json").unwrap();
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn dependency_evidence_rejects_missing_and_failed_audits() {
        let root = temporary_test_dir("dependencies");
        fs::create_dir_all(root.join("eng")).unwrap();
        fs::write(
            root.join("eng/dependency-policy.json"),
            br#"{"rust_lockfiles":["Cargo.lock"],"npm_workspaces":["web"]}"#,
        )
        .unwrap();
        let valid = serde_json::json!({
            "schema": "volang.dependency-result.v1", "passed": true, "complete": true,
            "audited": ["Cargo.lock", "web"], "failures": [],
            "executions": [
                {"input": "Cargo.lock", "code": 0, "signal": null, "duration_ms": 1},
                {"input": "web", "code": 0, "signal": null, "duration_ms": 1}
            ]
        });
        fs::write(
            root.join("result.json"),
            serde_json::to_vec(&valid).unwrap(),
        )
        .unwrap();
        validate_result(&root, "result.json").unwrap();
        for (field, value) in [
            ("audited", serde_json::json!(["Cargo.lock"])),
            ("executions", serde_json::json!([])),
            ("failures", serde_json::json!(["unreviewed advisory"])),
            ("complete", serde_json::json!(false)),
        ] {
            let mut invalid = valid.clone();
            invalid[field] = value;
            fs::write(
                root.join("result.json"),
                serde_json::to_vec(&invalid).unwrap(),
            )
            .unwrap();
            assert!(validate_result(&root, "result.json").is_err(), "{field}");
        }
        let mut invalid = valid;
        invalid["executions"][0]["code"] = serde_json::json!(7);
        fs::write(
            root.join("result.json"),
            serde_json::to_vec(&invalid).unwrap(),
        )
        .unwrap();
        assert!(validate_result(&root, "result.json").is_err());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn performance_evidence_requires_actual_ordered_release_measurements() {
        let root = temporary_test_dir("performance");
        fs::create_dir_all(&root).unwrap();
        let mut valid = serde_json::json!({
            "schema": "volang.ui.performance.v1", "passed": true,
            "target": "linux-x86_64", "profile": "release", "direct_scalar_allocations": 0,
            "frame_samples": 600, "interaction_samples": 300, "component_samples": 300,
            "component_rows": 256,
        });
        for metric in ["frame", "interaction", "component"] {
            for percentile in [50, 95, 99] {
                valid[format!("{metric}_p{percentile}_ns")] = serde_json::json!(percentile);
            }
        }
        fs::write(
            root.join("result.json"),
            serde_json::to_vec(&valid).unwrap(),
        )
        .unwrap();
        validate_result(&root, "result.json").unwrap();
        for (field, value) in [
            ("passed", serde_json::json!(false)),
            ("profile", serde_json::json!("debug")),
            ("frame_samples", serde_json::json!(0)),
            ("frame_p95_ns", serde_json::json!(100)),
            ("interaction_p99_ns", serde_json::Value::Null),
            ("direct_scalar_allocations", serde_json::json!(1)),
        ] {
            let mut invalid = valid.clone();
            invalid[field] = value;
            fs::write(
                root.join("result.json"),
                serde_json::to_vec(&invalid).unwrap(),
            )
            .unwrap();
            assert!(validate_result(&root, "result.json").is_err(), "{field}");
        }
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn digest_validation_rejects_uppercase_and_unknown_kinds() {
        let mut digest = FileDigest {
            path: "target/ci/artifacts/site".to_string(),
            kind: "directory".to_string(),
            size: 1,
            sha256: "a".repeat(64),
        };
        assert!(validate_file_digest(&digest).is_ok());
        digest.sha256 = "A".repeat(64);
        assert!(validate_file_digest(&digest).is_err());
        digest.sha256 = "a".repeat(64);
        digest.kind = "symlink".to_string();
        assert!(validate_file_digest(&digest).is_err());
    }

    #[test]
    fn artifact_file_hash_is_streamed_and_byte_bounded() {
        let directory = temporary_test_dir("stream");
        fs::create_dir_all(&directory).unwrap();
        let path = directory.join("artifact.bin");
        fs::write(&path, b"artifact").unwrap();

        let (size, digest) = hash_regular_file(&path, 8, Some(8)).unwrap();
        assert_eq!(size, 8);
        assert_eq!(format!("{digest:x}"), sha256_hex(b"artifact"));
        let error = hash_regular_file(&path, 7, None).unwrap_err();
        assert!(error.to_string().contains("byte budget"));

        fs::remove_dir_all(directory).unwrap();
    }

    #[test]
    fn artifact_directory_walk_rejects_excessive_nesting() {
        let root = temporary_test_dir("depth");
        let mut directory = root.clone();
        for index in 0..=MAX_ARTIFACT_DEPTH {
            directory.push(format!("d{index}"));
        }
        fs::create_dir_all(&directory).unwrap();

        let mut files = Vec::new();
        let mut entries = 0usize;
        let mut bytes = 0u64;
        let error = collect_regular_files(&root, &root, &mut files, &mut entries, &mut bytes, 0)
            .unwrap_err();
        assert!(error.to_string().contains("nesting exceeds"));

        fs::remove_dir_all(root).unwrap();
    }
}
