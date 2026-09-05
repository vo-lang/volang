//! A finite, ordered task executor. GitHub Actions owns lane scheduling.
use super::evidence::{digest_path, unix_millis, validate_result, write_json};
use super::model::{load_manifest, sha256_hex, task_digest, CiTask, FileDigest, SourceIdentity};
use super::plan::{canonical_plan_bytes, glob_matches, read_plan, source_identity, CiPlan};
use super::process::{run_command, CommandResult};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant, SystemTime};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ExecutionReceipt {
    schema: String,
    task_id: String,
    task_definition_sha256: String,
    plan_sha256: String,
    source: SourceIdentity,
    attempt: String,
    resource_group: String,
    pub(super) started_at_unix_millis: u64,
    finished_at_unix_millis: u64,
    duration_millis: u64,
    complete: bool,
    passed: bool,
    certifiable: bool,
    failure_kind: Option<String>,
    error: Option<String>,
    inputs: Vec<FileDigest>,
    commands: Vec<CommandResult>,
    diagnostics: Vec<FileDigest>,
    pub(super) results: Vec<FileDigest>,
    pub(super) artifacts: Vec<FileDigest>,
}

pub(super) fn run_task(
    root: &Path,
    plan_path: &Path,
    task_id: &str,
    cancelled: &AtomicBool,
) -> Result<()> {
    run_task_inner(root, plan_path, task_id, cancelled, true)
}

fn run_task_inner(
    root: &Path,
    plan_path: &Path,
    task_id: &str,
    cancelled: &AtomicBool,
    publish: bool,
) -> Result<()> {
    let (plan, plan_bytes) = read_plan(root, plan_path)?;
    let task = plan
        .tasks
        .iter()
        .find(|task| task.id == task_id)
        .ok_or_else(|| anyhow!("task {task_id} is absent from the CI plan"))?;
    if task.commands.is_empty() {
        bail!("task {task_id} has no executor commands");
    }
    let source = source_identity(root)?;
    if source != plan.source {
        bail!("CI execution source differs from the plan; regenerate the plan");
    }
    let manifest = load_manifest(root)?;
    let commands = manifest
        .commands
        .iter()
        .map(|command| (command.id.as_str(), command))
        .collect::<BTreeMap<_, _>>();
    let started = unix_millis(SystemTime::now())?;
    let attempt_relative = format!(
        "target/ci/executions/{task_id}/{started}-{}",
        std::process::id()
    );
    let attempt = root.join(&attempt_relative);
    fs::create_dir_all(attempt.parent().unwrap())?;
    fs::create_dir(&attempt).context("CI attempt already exists")?;
    let mut receipt = ExecutionReceipt {
        schema: "volang.ci.execution.v1".into(),
        task_id: task.id.clone(),
        task_definition_sha256: task_digest(task)?,
        plan_sha256: sha256_hex(&plan_bytes),
        source,
        attempt: attempt_relative,
        resource_group: task.resource_group.clone(),
        started_at_unix_millis: started,
        finished_at_unix_millis: started,
        duration_millis: 0,
        complete: false,
        passed: false,
        certifiable: false,
        failure_kind: None,
        error: None,
        inputs: Vec::new(),
        commands: Vec::new(),
        diagnostics: Vec::new(),
        results: Vec::new(),
        artifacts: Vec::new(),
    };
    write_json(&attempt.join("started.json"), &receipt)?;
    let clock = Instant::now();
    let evidence_path = root.join(format!("target/ci/evidence/{task_id}.evidence.json"));
    // Keep the lock alive through result capture, certification and completion.
    let mut _resource_lock = None;
    let execution = (|| -> Result<()> {
        _resource_lock = Some(lock_resource(root, &task.resource_group)?);
        archive_existing(root, &attempt, &evidence_path)?;
        receipt.inputs = input_digests(root, task)?;
        for path in task.results.iter().chain(&task.artifacts) {
            archive_existing(root, &attempt, &root.join(path))?;
        }
        let common_env = BTreeMap::from([
            ("VOWORK".into(), "off".into()),
            ("CARGO_TERM_COLOR".into(), "never".into()),
            ("RUST_BACKTRACE".into(), "1".into()),
            (
                "VO_CI_ATTEMPT_DIR".into(),
                attempt.to_string_lossy().into_owned(),
            ),
        ]);
        for id in &task.commands {
            let spec = commands[id.as_str()];
            let remaining = Duration::from_secs(u64::from(task.timeout_minutes) * 60)
                .saturating_sub(clock.elapsed());
            eprintln!("CI {task_id}: {id} (logs: {})", receipt.attempt);
            let result = run_command(root, spec, &attempt, &common_env, cancelled, remaining)?;
            let passed = result.passed();
            if !passed {
                receipt.failure_kind = result.failure_kind.clone();
            }
            let failure = result.error.clone();
            receipt.commands.push(result);
            write_json(
                &attempt.join(format!("command-{}.json", receipt.commands.len())),
                receipt.commands.last().unwrap(),
            )?;
            if !passed {
                bail!(
                    "command {id}: {}",
                    failure.as_deref().unwrap_or("unsuccessful result")
                );
            }
            if !spec.stdout_result.is_empty() {
                let stdout = &receipt.commands.last().unwrap().stdout;
                publish_stdout_result(root, &attempt, stdout, &spec.stdout_result)?;
            }
        }
        if source_identity(root)? != receipt.source || input_digests(root, task)? != receipt.inputs
        {
            bail!("CI task changed declared source inputs; inspect generated changes");
        }
        receipt.results = task
            .results
            .iter()
            .map(|path| {
                validate_result(root, path)?;
                digest_path(root, path)
            })
            .collect::<Result<_>>()?;
        receipt.artifacts = task
            .artifacts
            .iter()
            .map(|path| digest_path(root, path))
            .collect::<Result<_>>()?;
        Ok(())
    })();
    receipt.duration_millis = u64::try_from(clock.elapsed().as_millis())?;
    receipt.finished_at_unix_millis = unix_millis(SystemTime::now())?;
    receipt.complete = true;
    receipt.passed = execution.is_ok();
    if let Err(error) = execution {
        receipt.error = Some(format!("{error:#}"));
        receipt
            .failure_kind
            .get_or_insert_with(|| "infrastructure".into());
    }
    // Raw domain outputs are copied into this attempt before another task can
    // change their conventional paths. Their original logical paths remain bound.
    let capture = (|| -> Result<()> {
        for result in &receipt.results {
            let destination = attempt.join("results").join(&result.path);
            fs::create_dir_all(destination.parent().unwrap())?;
            fs::copy(root.join(&result.path), &destination)?;
        }
        receipt.diagnostics = receipt
            .commands
            .iter()
            .flat_map(|command| [&command.stdout, &command.stderr])
            .map(|path| digest_path(root, path))
            .collect::<Result<_>>()?;
        Ok(())
    })();
    if let Err(error) = capture {
        receipt.passed = false;
        receipt.error = Some(format!("could not preserve diagnostics: {error:#}"));
        receipt.failure_kind = Some("infrastructure".into());
    }
    receipt.certifiable = publish
        && receipt.passed
        && !receipt.source.tracked_dirty
        && !matches!(plan.selection, super::plan::SelectionBasis::Explicit { .. })
        && std::env::var("GITHUB_ACTIONS").as_deref() == Ok("true");
    write_json(&attempt.join("result.json"), &receipt)?;
    let result_digest = digest_path(root, &format!("{}/result.json", receipt.attempt))?;
    write_json(&attempt.join("completion.json"), &result_digest)?;
    // Publishing certifiable evidence is the final commit point. A hard kill
    // before it can only leave an incomplete/unpublished execution attempt.
    if receipt.certifiable {
        if let Err(error) = super::evidence::record_execution(
            root,
            super::evidence::RecordOptions {
                plan_path,
                task_id,
                output: &evidence_path,
            },
            receipt.clone(),
        ) {
            receipt.passed = false;
            receipt.certifiable = false;
            receipt.failure_kind = Some("infrastructure".into());
            receipt.error = Some(format!("execution could not be certified: {error:#}"));
            if evidence_path.exists() {
                fs::remove_file(&evidence_path)?;
            }
        }
    }
    if receipt.error.is_some() {
        // Certification itself can fail after command execution succeeds.
        // The published typed result and completion must describe that failure.
        write_json(&attempt.join("result.json"), &receipt)?;
        let result_digest = digest_path(root, &format!("{}/result.json", receipt.attempt))?;
        write_json(&attempt.join("completion.json"), &result_digest)?;
        write_json(&attempt.join("failure.json"), &receipt)?;
    }
    if publish {
        append_summary(task, &receipt)?;
    }
    eprintln!(
        "CI {task_id}: {} ({:.1}s), receipt: {}/result.json",
        if receipt.passed { "passed" } else { "failed" },
        receipt.duration_millis as f64 / 1000.0,
        receipt.attempt
    );
    if !receipt.passed {
        bail!("{}", receipt.error.as_deref().unwrap_or("task failed"));
    }
    Ok(())
}

struct ResourceLock(File);

impl Drop for ResourceLock {
    fn drop(&mut self) {
        // Explicitly unlock the shared file description. A concurrent fork can
        // briefly inherit a descriptor before exec closes it; close alone can
        // otherwise delay release beyond this guard's lifetime.
        let _ = self.0.unlock();
    }
}

fn lock_resource(root: &Path, group: &str) -> Result<ResourceLock> {
    let path = root.join(format!("target/ci/locks/{group}.lock"));
    fs::create_dir_all(path.parent().unwrap())?;
    let file = File::options()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)?;
    file.try_lock().with_context(|| {
        format!("CI resource group {group} is already in use or cannot be locked")
    })?;
    Ok(ResourceLock(file))
}

fn archive_existing(root: &Path, attempt: &Path, path: &Path) -> Result<()> {
    match fs::symlink_metadata(path) {
        Ok(metadata) => {
            if metadata.file_type().is_symlink() {
                bail!("CI output is a symlink: {}", path.display());
            }
            let destination = attempt.join("previous").join(path.strip_prefix(root)?);
            fs::create_dir_all(destination.parent().unwrap())?;
            fs::rename(path, destination).context("could not archive previous CI output")?;
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
    }
    Ok(())
}

fn publish_stdout_result(root: &Path, attempt: &Path, stdout: &str, result: &str) -> Result<()> {
    // Validate the child's exact bytes before publishing. A successful process
    // with malformed, empty or unsuccessful JSON never creates a domain result.
    validate_result(root, stdout)?;
    let destination = root.join(result);
    fs::create_dir_all(destination.parent().unwrap())?;
    if fs::symlink_metadata(&destination).is_ok() {
        bail!("CI command unexpectedly wrote its stdout result destination: {result}");
    }
    let staged = attempt.join("stdout-result.pending");
    fs::copy(root.join(stdout), &staged)?;
    fs::rename(staged, destination)?;
    Ok(())
}

fn input_digests(root: &Path, task: &CiTask) -> Result<Vec<FileDigest>> {
    let output = Command::new("git")
        .args([
            "ls-files",
            "--cached",
            "--others",
            "--exclude-standard",
            "-z",
        ])
        .current_dir(root)
        .output()?;
    if !output.status.success() {
        bail!("could not enumerate CI task inputs");
    }
    let mut paths = output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|path| !path.is_empty())
        .map(|path| std::str::from_utf8(path).map(str::to_owned))
        .collect::<std::result::Result<Vec<_>, _>>()?;
    paths.sort();
    paths.dedup();
    let digests = paths
        .iter()
        .filter(|path| {
            task.inputs
                .iter()
                .any(|pattern| glob_matches(pattern, path))
        })
        .map(|path| digest_path(root, path))
        .collect::<Result<Vec<_>>>()?;
    if digests.is_empty() {
        bail!("CI task {} resolved no declared inputs", task.id);
    }
    Ok(digests)
}

pub(super) fn validate_receipt(
    root: &Path,
    plan: &CiPlan,
    task: &CiTask,
    receipt: &ExecutionReceipt,
) -> Result<()> {
    if receipt.schema != "volang.ci.execution.v1"
        || !receipt.complete
        || !receipt.passed
        || !receipt.certifiable
        || receipt.failure_kind.is_some()
        || receipt.error.is_some()
        || receipt.inputs.is_empty()
        || receipt.source != plan.source
        || receipt.source.tracked_dirty
        || receipt.task_id != task.id
        || receipt.task_definition_sha256 != task_digest(task)?
        || receipt.plan_sha256 != sha256_hex(&canonical_plan_bytes(plan)?)
        || receipt.resource_group != task.resource_group
        || receipt.commands.len() != task.commands.len()
        || receipt.commands.is_empty()
        || receipt.finished_at_unix_millis < receipt.started_at_unix_millis
    {
        bail!("invalid execution receipt for {}", task.id);
    }
    let manifest = load_manifest(root)?;
    let prefix = format!("target/ci/executions/{}/", task.id);
    let nonce = receipt.attempt.strip_prefix(&prefix).unwrap_or_default();
    if nonce.is_empty()
        || !nonce
            .bytes()
            .all(|byte| byte.is_ascii_digit() || byte == b'-')
    {
        bail!("execution attempt path is invalid");
    }
    for (result, id) in receipt.commands.iter().zip(&task.commands) {
        let spec = manifest
            .commands
            .iter()
            .find(|command| &command.id == id)
            .unwrap();
        if !result.passed()
            || &result.id != id
            || result.argv != spec.argv
            || result.cwd != spec.cwd
        {
            bail!("execution command differs from declared command {id}");
        }
        if spec.report == "cargo-test"
            && !result.test_counts.as_ref().is_some_and(|counts| {
                counts.passed > 0 && counts.failed == 0 && counts.binaries > 0
            })
        {
            bail!("execution command {id} has no successful Rust tests");
        }
        if spec.report == "libfuzzer" {
            let expected = super::process::fuzz_run_budget(&spec.argv)?;
            if !result.test_counts.as_ref().is_some_and(|counts| {
                counts.passed == expected
                    && counts.failed == 0
                    && counts.ignored == 0
                    && counts.binaries == 1
            }) {
                bail!("execution command {id} has no completed fuzz budget");
            }
        }
    }
    if receipt.diagnostics.len() != receipt.commands.len() * 2 {
        bail!("incomplete execution diagnostics");
    }
    for (result, id) in receipt.commands.iter().zip(&task.commands) {
        let spec = manifest
            .commands
            .iter()
            .find(|command| &command.id == id)
            .unwrap();
        if spec.stdout_result.is_empty() {
            continue;
        }
        let output = receipt
            .results
            .iter()
            .find(|digest| digest.path == spec.stdout_result)
            .ok_or_else(|| anyhow!("execution command {id} has no declared stdout result"))?;
        let stdout = receipt
            .diagnostics
            .iter()
            .find(|digest| digest.path == result.stdout)
            .ok_or_else(|| anyhow!("execution command {id} has no stdout diagnostic"))?;
        if output.sha256 != stdout.sha256 || output.size != stdout.size || output.kind != "file" {
            bail!("execution command {id} stdout does not match its domain result");
        }
    }
    for (diagnostic, expected) in receipt.diagnostics.iter().zip(
        receipt
            .commands
            .iter()
            .flat_map(|command| [&command.stdout, &command.stderr]),
    ) {
        super::evidence::validate_file_digest(diagnostic)?;
        if &diagnostic.path != expected
            || diagnostic.kind != "file"
            || !expected.starts_with(&format!("{}/", receipt.attempt))
            || Path::new(expected)
                .components()
                .any(|component| matches!(component, std::path::Component::ParentDir))
        {
            bail!("execution diagnostic binding is invalid");
        }
    }
    // Certification runs on fresh checkouts; source inputs can be recomputed,
    // while log/result digests travel inside the immutable evidence artifact.
    if receipt.inputs != input_digests(root, task)? {
        bail!("execution input digest mismatch for {}", task.id);
    }
    Ok(())
}

fn append_summary(task: &CiTask, receipt: &ExecutionReceipt) -> Result<()> {
    let Ok(path) = std::env::var("GITHUB_STEP_SUMMARY") else {
        return Ok(());
    };
    let mut file = File::options()
        .append(true)
        .create(true)
        .open(PathBuf::from(path))?;
    writeln!(
        file,
        "\n### {} — {}\n\nOwner: {} · {:.1}s · diagnostics: `{}`\n",
        task.id,
        if receipt.passed { "passed" } else { "failed" },
        task.owners.join(", "),
        receipt.duration_millis as f64 / 1000.0,
        receipt.attempt
    )?;
    writeln!(
        file,
        "| Command | Status | Duration | Tests passed |\n|---|---|---:|---:|"
    )?;
    for command in &receipt.commands {
        writeln!(
            file,
            "| {} | {:?} | {:.1}s | {} |",
            command.id,
            command.status,
            command.duration_millis as f64 / 1000.0,
            command
                .test_counts
                .as_ref()
                .map_or(String::from("—"), |counts| counts.passed.to_string())
        )?;
    }
    if let Some(error) = &receipt.error {
        writeln!(file, "\nFailure class: `{}`\n\n```text\n{}\n```\n\nReproduce: `vo-dev ci run --plan target/ci/plan.json --task {}`\n",
            receipt.failure_kind.as_deref().unwrap_or("infrastructure"), error.replace("```", "'''"), task.id)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::model::{CiCommand, CiManifest, CiProfile};
    use super::*;

    struct Repository(PathBuf);
    impl Repository {
        fn new(report: &str, result: bool, mode: &str) -> Self {
            let nonce = SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let root = std::env::temp_dir()
                .join(format!("vo-ci-execution-{}-{nonce}", std::process::id()));
            fs::create_dir_all(root.join("eng")).unwrap();
            let root = root.canonicalize().unwrap();
            let manifest = CiManifest {
                components: Vec::new(),
                version: 1,
                profiles: vec![CiProfile {
                    name: "test".into(),
                    tier: "feedback".into(),
                    changed_only: false,
                    tasks: vec!["fixture".into()],
                }],
                tasks: vec![CiTask {
                    id: "fixture".into(),
                    tier: "feedback".into(),
                    workflow_job: "quality".into(),
                    runner: "ubuntu-24.04".into(),
                    always: false,
                    depends_on: Vec::new(),
                    owners: vec!["engineering".into()],
                    impact: vec!["*".into()],
                    platforms: vec!["linux".into()],
                    capabilities: vec!["contracts".into()],
                    timeout_minutes: 2,
                    budget_minutes: 1,
                    evidence_kind: "contract".into(),
                    results: if result {
                        vec!["target/ci/results/domain.json".into()]
                    } else {
                        Vec::new()
                    },
                    artifacts: Vec::new(),
                    commands: vec!["fixture-command".into()],
                    inputs: vec!["*".into()],
                    resource_group: "fixture".into(),
                }],
                commands: vec![CiCommand {
                    id: "fixture-command".into(),
                    argv: vec![
                        std::env::current_exe()
                            .unwrap()
                            .to_string_lossy()
                            .into_owned(),
                        "--exact".into(),
                        if mode == "zero" {
                            "no_such_test".into()
                        } else {
                            "ci::process::tests::child_process_fixture".into()
                        },
                        "--nocapture".into(),
                    ],
                    cwd: String::new(),
                    env: BTreeMap::from([
                        (
                            "VO_CI_PROCESS_FIXTURE".into(),
                            if mode == "zero" {
                                "output".into()
                            } else {
                                mode.into()
                            },
                        ),
                        (
                            "VO_CI_PROCESS_FIXTURE_DIR".into(),
                            root.to_string_lossy().into_owned(),
                        ),
                    ]),
                    timeout_seconds: 40,
                    failure_kind: "product".into(),
                    report: report.into(),
                    stdout_result: String::new(),
                }],
            };
            fs::write(
                root.join("eng/ci.toml"),
                toml::to_string(&manifest).unwrap(),
            )
            .unwrap();
            fs::write(root.join(".gitignore"), "/target/\n").unwrap();
            for argv in [
                vec!["-c", "init.templateDir=", "init", "--quiet"],
                vec!["add", "eng/ci.toml", ".gitignore"],
                vec![
                    "-c",
                    "user.name=CI Test",
                    "-c",
                    "user.email=ci-test@example.invalid",
                    "-c",
                    "commit.gpgSign=false",
                    "-c",
                    "core.hooksPath=nonexistent-hooks",
                    "commit",
                    "--quiet",
                    "-m",
                    "fixture",
                ],
            ] {
                let output = Command::new("git")
                    .args(argv)
                    .current_dir(&root)
                    .output()
                    .unwrap();
                assert!(
                    output.status.success(),
                    "{}",
                    String::from_utf8_lossy(&output.stderr)
                );
            }
            // This deliberate local input keeps fixtures noncertifiable even when
            // tests inherit a real GitHub Actions environment.
            fs::write(root.join("local-input.txt"), "local test source").unwrap();
            let plan = super::super::plan::build_plan(&root, "test", None, None, &[]).unwrap();
            super::super::plan::write_plan(&root.join("target/ci/plan.json"), &plan).unwrap();
            Self(root)
        }
        fn run(&self) -> Result<()> {
            run_task_inner(
                &self.0,
                &self.0.join("target/ci/plan.json"),
                "fixture",
                &AtomicBool::new(false),
                false,
            )
        }
        fn receipt(&self) -> (PathBuf, ExecutionReceipt) {
            let attempts = fs::read_dir(self.0.join("target/ci/executions/fixture"))
                .unwrap()
                .collect::<std::io::Result<Vec<_>>>()
                .unwrap();
            assert_eq!(attempts.len(), 1);
            let path = attempts[0].path();
            let receipt =
                serde_json::from_slice(&fs::read(path.join("result.json")).unwrap()).unwrap();
            let completion: FileDigest =
                serde_json::from_slice(&fs::read(path.join("completion.json")).unwrap()).unwrap();
            assert_eq!(completion, digest_path(&self.0, &completion.path).unwrap());
            (path, receipt)
        }
    }
    impl Drop for Repository {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn successful_local_execution_has_bound_logs_and_cannot_be_certified() {
        let repository = Repository::new("cargo-test", false, "output");
        repository.run().unwrap();
        let (_, receipt) = repository.receipt();
        assert!(receipt.complete && receipt.passed && !receipt.certifiable);
        assert_eq!(receipt.commands[0].test_counts.as_ref().unwrap().passed, 1);
        assert_eq!(receipt.diagnostics.len(), 2);
        assert!(!repository
            .0
            .join("target/ci/evidence/fixture.evidence.json")
            .exists());
        let (plan, _) =
            read_plan(&repository.0, &repository.0.join("target/ci/plan.json")).unwrap();
        assert!(validate_receipt(&repository.0, &plan, &plan.tasks[0], &receipt).is_err());
    }

    #[test]
    fn stdout_domain_publication_rejects_empty_malformed_failed_and_substituted_outputs() {
        let repository = Repository::new("", false, "output");
        let attempt = repository.0.join("target/ci/stdout-fixture");
        fs::create_dir_all(&attempt).unwrap();
        let stdout = "target/ci/stdout-fixture/child.log";
        let result = "target/ci/results/child.json";
        for invalid in [
            "",
            "truncated {",
            r#"{"passed":true}"#,
            r#"{"schema":"volang.browser-result.v1","passed":false,"report":{"passed":false,"complete":false}}"#,
        ] {
            fs::write(repository.0.join(stdout), invalid).unwrap();
            assert!(publish_stdout_result(&repository.0, &attempt, stdout, result).is_err());
            assert!(!repository.0.join(result).exists());
        }
        let bytes = br#"{"schema":"volang.browser-result.v1","passed":true,"report":{"passed":true,"complete":true,"checks":["executed"]}}"#;
        fs::write(repository.0.join(stdout), bytes).unwrap();
        publish_stdout_result(&repository.0, &attempt, stdout, result).unwrap();
        assert_eq!(fs::read(repository.0.join(result)).unwrap(), bytes);
        fs::write(repository.0.join(result), "substituted").unwrap();
        assert!(publish_stdout_result(&repository.0, &attempt, stdout, result).is_err());
        assert_eq!(fs::read(repository.0.join(result)).unwrap(), b"substituted");
    }

    #[test]
    fn stale_domain_result_and_arbitrary_record_cannot_turn_a_task_green() {
        let repository = Repository::new("", true, "output");
        let result = repository.0.join("target/ci/results/domain.json");
        fs::create_dir_all(result.parent().unwrap()).unwrap();
        fs::write(&result, br#"{"schema":"volang.browser-result.v1","passed":true,"report":{"passed":true,"complete":true,"checks":["old success"]}}"#).unwrap();
        assert!(repository.run().is_err());
        let (attempt, receipt) = repository.receipt();
        assert!(!receipt.passed);
        assert!(!result.exists());
        assert!(attempt
            .join("previous/target/ci/results/domain.json")
            .exists());
        assert!(receipt
            .error
            .unwrap()
            .contains("required CI result is missing"));
        let error = super::super::evidence::record(
            &repository.0,
            super::super::evidence::RecordOptions {
                plan_path: &repository.0.join("target/ci/plan.json"),
                task_id: "fixture",
                output: &repository
                    .0
                    .join("target/ci/evidence/fixture.evidence.json"),
            },
        )
        .unwrap_err();
        assert!(error.to_string().contains("requires ci run"));
    }

    #[test]
    fn zero_tests_and_nonzero_exit_produce_complete_failure_receipts() {
        for (mode, report, kind) in [
            ("zero", "cargo-test", "infrastructure"),
            ("nonzero", "", "product"),
        ] {
            let repository = Repository::new(report, false, mode);
            assert!(repository.run().is_err());
            let (_, receipt) = repository.receipt();
            assert!(receipt.complete && !receipt.passed && !receipt.certifiable);
            assert_eq!(receipt.failure_kind.as_deref(), Some(kind));
            assert_eq!(receipt.commands.len(), 1);
        }
    }

    #[test]
    fn resource_group_prevents_concurrent_output_mutation() {
        let repository = Repository::new("", false, "output");
        let lock = lock_resource(&repository.0, "fixture").unwrap();
        assert!(repository.run().is_err());
        let (_, receipt) = repository.receipt();
        assert!(!receipt.passed && receipt.commands.is_empty());
        assert!(receipt.error.unwrap().contains("already in use"));
        // Keep a shared descriptor alive to deterministically cover the fork
        // inheritance window while requiring release at the guard boundary.
        let inherited_description = lock.0.try_clone().unwrap();
        drop(lock);
        lock_resource(&repository.0, "fixture").unwrap();
        drop(inherited_description);
    }

    #[test]
    fn receipt_validation_rejects_incomplete_or_substituted_execution() {
        let repository = Repository::new("cargo-test", false, "output");
        let add = Command::new("git")
            .args(["add", "local-input.txt"])
            .current_dir(&repository.0)
            .status()
            .unwrap();
        assert!(add.success());
        let commit = Command::new("git")
            .args([
                "-c",
                "user.name=CI Test",
                "-c",
                "user.email=ci-test@example.invalid",
                "-c",
                "commit.gpgSign=false",
                "-c",
                "core.hooksPath=nonexistent-hooks",
                "commit",
                "--quiet",
                "-m",
                "clean fixture",
            ])
            .current_dir(&repository.0)
            .status()
            .unwrap();
        assert!(commit.success());
        let plan = super::super::plan::build_plan(&repository.0, "test", None, None, &[]).unwrap();
        super::super::plan::write_plan(&repository.0.join("target/ci/plan.json"), &plan).unwrap();
        repository.run().unwrap();
        let (_, mut receipt) = repository.receipt();
        // The fixture suppresses publication. Exercise certification's structural
        // checks using an otherwise complete execution from the clean source.
        receipt.certifiable = true;
        validate_receipt(&repository.0, &plan, &plan.tasks[0], &receipt).unwrap();
        for variant in 0..8 {
            let mut invalid = receipt.clone();
            match variant {
                0 => invalid.complete = false,
                1 => invalid.commands.clear(),
                2 => invalid.commands[0].argv.push("--unrecorded".into()),
                3 => invalid.commands[0].test_counts.as_mut().unwrap().passed = 0,
                4 => invalid.diagnostics.clear(),
                5 => invalid.inputs[0].sha256 = "0".repeat(64),
                6 => invalid.plan_sha256 = "0".repeat(64),
                7 => invalid.diagnostics[0].path = "target/ci/unrelated.log".into(),
                _ => unreachable!(),
            }
            assert!(
                validate_receipt(&repository.0, &plan, &plan.tasks[0], &invalid).is_err(),
                "variant {variant}"
            );
        }
    }
}
