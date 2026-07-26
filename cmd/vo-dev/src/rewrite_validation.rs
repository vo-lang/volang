use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

#[derive(Deserialize)]
struct ValidationManifest {
    schema_version: u32,
    case: Vec<ValidationCase>,
}

#[derive(Clone, Deserialize)]
struct ValidationCase {
    id: String,
    working_directory: String,
    evidence_kinds: Vec<String>,
    commands: Vec<String>,
    required_artifacts: Vec<String>,
}

#[derive(Deserialize)]
struct TraceabilityManifest {
    requirement: Vec<TraceRequirement>,
    test: Vec<TraceTest>,
    acceptance: Vec<TraceAcceptance>,
}

#[derive(Deserialize)]
struct TraceTest {
    id: String,
    requirement_ids: Vec<String>,
    target_profile: String,
    timeout_seconds: u64,
}

#[derive(Deserialize)]
struct TraceRequirement {
    id: String,
    acceptance_refs: Vec<String>,
    required_evidence_kinds: Vec<String>,
}

#[derive(Deserialize)]
struct TraceAcceptance {
    id: String,
    test_refs: Vec<String>,
}

#[derive(Deserialize, Serialize)]
struct ValidationReport {
    schema_version: u32,
    generated_at_unix_millis: u128,
    case_id: String,
    requirement_ids: Vec<String>,
    evidence_kinds: Vec<String>,
    target_profile: String,
    working_directory: String,
    toolchain: BTreeMap<String, String>,
    operator: String,
    device_os: String,
    source_identities: Vec<SourceIdentity>,
    certifiable: bool,
    commands: Vec<CommandResult>,
    artifacts: Vec<ArtifactDigest>,
    passed: bool,
}

#[derive(Clone, Deserialize, Serialize)]
struct SourceIdentity {
    repository: String,
    path: String,
    commit_sha: String,
    dirty: bool,
}

#[derive(Deserialize, Serialize)]
struct CommandResult {
    command: String,
    duration_millis: u128,
    exit_code: Option<i32>,
    timed_out: bool,
    passed: bool,
}

#[derive(Deserialize, Serialize)]
struct ArtifactDigest {
    path: String,
    bytes: u64,
    sha256: String,
}

#[derive(Serialize)]
struct CertificationBundle {
    schema_version: u32,
    generated_from: String,
    source_identities: Vec<SourceIdentity>,
    evidence: Vec<GeneratedEvidence>,
}

#[derive(Clone, Serialize)]
struct GeneratedEvidence {
    id: String,
    requirement_ids: Vec<String>,
    test_ids: Vec<String>,
    acceptance_ids: Vec<String>,
    kind: String,
    commit_sha: String,
    artifact_sha256: String,
    target_profile: String,
    toolchain: String,
    run_at: String,
    duration_millis: u64,
    result: String,
    operator: String,
    device_os: String,
    attachments: Vec<String>,
}

pub(crate) fn cmd_rewrite(root: &Path, mut args: Vec<String>) -> Result<()> {
    let command = args
        .first()
        .map(String::as_str)
        .ok_or_else(|| anyhow!(
            "usage: vo-dev rewrite validate --case <TEST-ID> | certify --reports <dir> [--output <path>] [--traceability-output <path>]"
        ))?;
    match command {
        "validate" => {
            args.remove(0);
            cmd_validate(root, args)
        }
        "certify" => {
            args.remove(0);
            cmd_certify(root, args)
        }
        _ => bail!(
            "usage: vo-dev rewrite validate --case <TEST-ID> | certify --reports <dir> [--output <path>] [--traceability-output <path>]"
        ),
    }
}

fn cmd_validate(root: &Path, args: Vec<String>) -> Result<()> {
    let mut case_id = None;
    let mut format = "text";
    let mut output_path = None;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--case" => {
                case_id = Some(
                    args.get(index + 1)
                        .ok_or_else(|| anyhow!("--case requires a TEST ID"))?
                        .clone(),
                );
                index += 2;
            }
            "--format" => {
                format = args
                    .get(index + 1)
                    .ok_or_else(|| anyhow!("--format requires text or json"))?;
                if !matches!(format, "text" | "json") {
                    bail!("--format must be text or json");
                }
                index += 2;
            }
            "--output" => {
                output_path = Some(PathBuf::from(
                    args.get(index + 1)
                        .ok_or_else(|| anyhow!("--output requires a path"))?,
                ));
                index += 2;
            }
            argument => bail!("unknown rewrite validate argument: {argument}"),
        }
    }
    let case_id = case_id.ok_or_else(|| anyhow!("rewrite validate requires --case <TEST-ID>"))?;
    let cases = load_validation_cases(root)?;
    let case = cases
        .get(&case_id)
        .ok_or_else(|| anyhow!("unknown rewrite validation case {case_id}"))?;
    let tests = load_trace_tests(root)?;
    let test = tests
        .get(&case_id)
        .ok_or_else(|| anyhow!("validation case {case_id} has no traceability test"))?;
    let report = run_case(root, case, test)?;
    let json = serde_json::to_string_pretty(&report)?;
    if let Some(path) = output_path {
        let path = if path.is_absolute() {
            path
        } else {
            root.join(path)
        };
        fs::write(&path, format!("{json}\n"))
            .with_context(|| format!("could not write validation report {}", path.display()))?;
    }
    if format == "json" {
        println!("{json}");
    } else {
        println!(
            "rewrite validation {}: {} (certifiable={})",
            report.case_id,
            if report.passed { "passed" } else { "failed" },
            report.certifiable
        );
        for command in &report.commands {
            println!(
                "  {} [{}ms, exit={}]",
                command.command,
                command.duration_millis,
                if command.timed_out {
                    "timeout".to_string()
                } else {
                    command
                        .exit_code
                        .map_or_else(|| "signal".to_string(), |code| code.to_string())
                }
            );
        }
        for artifact in &report.artifacts {
            println!(
                "  artifact {} sha256:{} ({} bytes)",
                artifact.path, artifact.sha256, artifact.bytes
            );
        }
    }
    if !report.passed {
        bail!("rewrite validation {} failed", report.case_id);
    }
    Ok(())
}

fn cmd_certify(root: &Path, args: Vec<String>) -> Result<()> {
    let mut reports = root.join("target/rewrite-validation");
    let mut output_path = None;
    let mut traceability_output_path = None;
    let mut format = "toml";
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--reports" => {
                reports = PathBuf::from(
                    args.get(index + 1)
                        .ok_or_else(|| anyhow!("--reports requires a directory"))?,
                );
                if !reports.is_absolute() {
                    reports = root.join(reports);
                }
                index += 2;
            }
            "--output" => {
                output_path = Some(PathBuf::from(
                    args.get(index + 1)
                        .ok_or_else(|| anyhow!("--output requires a path"))?,
                ));
                index += 2;
            }
            "--traceability-output" => {
                traceability_output_path =
                    Some(PathBuf::from(args.get(index + 1).ok_or_else(|| {
                        anyhow!("--traceability-output requires a path")
                    })?));
                index += 2;
            }
            "--format" => {
                format = args
                    .get(index + 1)
                    .ok_or_else(|| anyhow!("--format requires toml or json"))?;
                if !matches!(format, "toml" | "json") {
                    bail!("--format must be toml or json");
                }
                index += 2;
            }
            argument => bail!("unknown rewrite certify argument: {argument}"),
        }
    }

    let cases = load_validation_cases(root)?;
    let tests = load_trace_tests(root)?;
    let traceability = load_traceability(root)?;
    let requirements = unique_by_id("requirement", traceability.requirement, |record| &record.id)?;
    let acceptances = unique_by_id("acceptance", traceability.acceptance, |record| &record.id)?;
    let current_identities = ["volang", "vogui", "voplay"]
        .into_iter()
        .map(|repository| source_identity(root, repository))
        .collect::<Result<Vec<_>>>()?;
    if let Some(identity) = current_identities.iter().find(|identity| identity.dirty) {
        bail!(
            "rewrite certification requires clean source identity; {} is dirty",
            identity.repository
        );
    }
    let volang_commit = current_identities
        .iter()
        .find(|identity| identity.repository == "volang")
        .map(|identity| identity.commit_sha.clone())
        .ok_or_else(|| anyhow!("rewrite certification has no Volang source identity"))?;

    let mut evidence = Vec::new();
    let mut evidence_ids = std::collections::BTreeSet::new();
    for (case_id, case) in &cases {
        let test = tests
            .get(case_id)
            .ok_or_else(|| anyhow!("validation case {case_id} has no traceability test"))?;
        let report_path = reports.join(format!("{case_id}.json"));
        let report_bytes = fs::read(&report_path).with_context(|| {
            format!(
                "could not read rewrite validation report {}",
                report_path.display()
            )
        })?;
        let report: ValidationReport =
            serde_json::from_slice(&report_bytes).with_context(|| {
                format!(
                    "could not parse rewrite validation report {}",
                    report_path.display()
                )
            })?;
        validate_certifiable_report(root, case, test, &report, &current_identities, &report_path)?;
        let report_sha = sha256_hex(&report_bytes);
        let duration_millis = report
            .commands
            .iter()
            .try_fold(0_u128, |total, command| {
                total.checked_add(command.duration_millis)
            })
            .and_then(|duration| u64::try_from(duration).ok())
            .ok_or_else(|| anyhow!("{case_id} duration does not fit u64"))?;
        let toolchain = report
            .toolchain
            .iter()
            .map(|(tool, version)| format!("{tool}={version}"))
            .collect::<Vec<_>>()
            .join("; ");
        for kind in &case.evidence_kinds {
            let requirement_ids = test
                .requirement_ids
                .iter()
                .filter(|requirement_id| {
                    requirements
                        .get(*requirement_id)
                        .is_some_and(|requirement| {
                            requirement.required_evidence_kinds.contains(kind)
                        })
                })
                .cloned()
                .collect::<Vec<_>>();
            if requirement_ids.is_empty() {
                bail!("{case_id} evidence kind {kind} has no linked requirement");
            }
            let mut acceptance_ids = requirement_ids
                .iter()
                .flat_map(|requirement_id| {
                    requirements
                        .get(requirement_id)
                        .expect("requirement IDs were filtered")
                        .acceptance_refs
                        .iter()
                })
                .filter(|acceptance_id| {
                    acceptances
                        .get(*acceptance_id)
                        .is_some_and(|acceptance| acceptance.test_refs.contains(case_id))
                })
                .cloned()
                .collect::<Vec<_>>();
            acceptance_ids.sort();
            acceptance_ids.dedup();
            if acceptance_ids.is_empty() {
                bail!(
                    "{case_id} evidence kind {kind} has no acceptance that directly references the test"
                );
            }
            let id = evidence_id(case_id, kind);
            if !evidence_ids.insert(id.clone()) {
                bail!("duplicate generated evidence ID {id}");
            }
            evidence.push(GeneratedEvidence {
                id,
                requirement_ids,
                test_ids: vec![case_id.clone()],
                acceptance_ids,
                kind: kind.clone(),
                commit_sha: volang_commit.clone(),
                artifact_sha256: report_sha.clone(),
                target_profile: report.target_profile.clone(),
                toolchain: toolchain.clone(),
                run_at: format!("unix-ms:{}", report.generated_at_unix_millis),
                duration_millis,
                result: "passed".to_string(),
                operator: report.operator.clone(),
                device_os: report.device_os.clone(),
                attachments: report
                    .artifacts
                    .iter()
                    .map(|artifact| artifact.path.clone())
                    .collect(),
            });
        }
    }
    validate_generated_evidence_coverage(&tests, &requirements, &acceptances, &evidence)?;
    evidence.sort_by(|left, right| left.id.cmp(&right.id));
    let bundle = CertificationBundle {
        schema_version: 1,
        generated_from: reports.display().to_string(),
        source_identities: current_identities,
        evidence: evidence.clone(),
    };
    let serialized = if format == "json" {
        serde_json::to_string_pretty(&bundle)?
    } else {
        toml::to_string_pretty(&bundle)?
    };
    let output_path = output_path.map(|path| resolve_output_path(root, path));
    let finalized_traceability = if let Some(path) = traceability_output_path {
        let path = resolve_output_path(root, path);
        if output_path.as_ref().is_some_and(|output| output == &path) {
            bail!("certification bundle and traceability output paths must differ");
        }
        let source_path = root.join("rewrite-traceability.toml");
        let source = fs::read_to_string(&source_path)
            .with_context(|| format!("could not read {}", source_path.display()))?;
        Some((path, finalize_traceability(&source, &evidence)?))
    } else {
        None
    };
    if let Some(path) = output_path {
        fs::write(&path, format!("{serialized}\n"))
            .with_context(|| format!("could not write certification bundle {}", path.display()))?;
        println!("rewrite certification bundle: {}", path.display());
    } else {
        println!("{serialized}");
    }
    if let Some((path, finalized)) = finalized_traceability {
        fs::write(&path, finalized).with_context(|| {
            format!("could not write finalized traceability {}", path.display())
        })?;
        println!("finalized rewrite traceability: {}", path.display());
    }
    Ok(())
}

fn resolve_output_path(root: &Path, path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        path
    } else {
        root.join(path)
    }
}

fn validate_generated_evidence_coverage(
    tests: &BTreeMap<String, TraceTest>,
    requirements: &BTreeMap<String, TraceRequirement>,
    acceptances: &BTreeMap<String, TraceAcceptance>,
    evidence: &[GeneratedEvidence],
) -> Result<()> {
    for requirement in requirements.values() {
        for kind in &requirement.required_evidence_kinds {
            if !evidence.iter().any(|record| {
                record.result == "passed"
                    && record.kind == *kind
                    && record.requirement_ids.contains(&requirement.id)
            }) {
                bail!(
                    "{} has no generated passing evidence kind {}",
                    requirement.id,
                    kind
                );
            }
        }
        for (test_id, _) in tests
            .iter()
            .filter(|(_, test)| test.requirement_ids.contains(&requirement.id))
        {
            if !evidence.iter().any(|record| {
                record.result == "passed"
                    && record.requirement_ids.contains(&requirement.id)
                    && record.test_ids.contains(test_id)
            }) {
                bail!(
                    "{} test {} has no generated passing evidence",
                    requirement.id,
                    test_id
                );
            }
        }
    }
    for acceptance in acceptances.values() {
        for test_id in &acceptance.test_refs {
            if !evidence.iter().any(|record| {
                record.result == "passed"
                    && record.test_ids.contains(test_id)
                    && record.acceptance_ids.contains(&acceptance.id)
            }) {
                bail!(
                    "{} test {} has no generated direct evidence",
                    acceptance.id,
                    test_id
                );
            }
        }
    }
    Ok(())
}

fn finalize_traceability(source: &str, evidence: &[GeneratedEvidence]) -> Result<String> {
    let mut manifest: toml::Value =
        toml::from_str(source).context("could not parse rewrite traceability for finalization")?;
    let table = manifest
        .as_table_mut()
        .ok_or_else(|| anyhow!("rewrite traceability root must be a table"))?;
    if evidence.is_empty() {
        bail!("rewrite traceability finalization requires generated evidence");
    }

    let mut evidence_by_acceptance = BTreeMap::<String, Vec<String>>::new();
    let mut evidence_by_test_acceptance = BTreeMap::<(String, String), Vec<String>>::new();
    for record in evidence {
        for acceptance_id in &record.acceptance_ids {
            evidence_by_acceptance
                .entry(acceptance_id.clone())
                .or_default()
                .push(record.id.clone());
            for test_id in &record.test_ids {
                evidence_by_test_acceptance
                    .entry((test_id.clone(), acceptance_id.clone()))
                    .or_default()
                    .push(record.id.clone());
            }
        }
    }
    for ids in evidence_by_acceptance.values_mut() {
        ids.sort();
        ids.dedup();
    }

    finalize_status_records(table, "requirement")?;
    finalize_status_records(table, "test")?;
    let acceptances = table
        .get_mut("acceptance")
        .and_then(toml::Value::as_array_mut)
        .ok_or_else(|| anyhow!("rewrite traceability has no acceptance array"))?;
    for acceptance in acceptances {
        let acceptance = acceptance
            .as_table_mut()
            .ok_or_else(|| anyhow!("acceptance record must be a table"))?;
        let id = required_string_field(acceptance, "id", "acceptance")?.to_string();
        require_finalizable_status(acceptance, &id)?;
        let test_refs = acceptance
            .get("test_refs")
            .and_then(toml::Value::as_array)
            .ok_or_else(|| anyhow!("{id} has no test_refs array"))?;
        for test_id in test_refs {
            let test_id = test_id
                .as_str()
                .ok_or_else(|| anyhow!("{id} has a non-string test reference"))?;
            if !evidence_by_test_acceptance.contains_key(&(test_id.to_string(), id.clone())) {
                bail!("{id} test {test_id} has no generated direct evidence");
            }
        }
        let evidence_refs = evidence_by_acceptance
            .get(&id)
            .ok_or_else(|| anyhow!("{id} has no generated evidence"))?;
        acceptance.insert(
            "status".to_string(),
            toml::Value::String("accepted".to_string()),
        );
        acceptance.insert(
            "evidence_refs".to_string(),
            toml::Value::Array(
                evidence_refs
                    .iter()
                    .cloned()
                    .map(toml::Value::String)
                    .collect(),
            ),
        );
    }

    table.insert(
        "evidence".to_string(),
        toml::Value::try_from(evidence).context("could not serialize generated evidence")?,
    );
    let serialized =
        toml::to_string_pretty(&manifest).context("could not serialize finalized traceability")?;
    Ok(format!("{serialized}\n"))
}

fn finalize_status_records(
    manifest: &mut toml::map::Map<String, toml::Value>,
    record_kind: &str,
) -> Result<()> {
    let records = manifest
        .get_mut(record_kind)
        .and_then(toml::Value::as_array_mut)
        .ok_or_else(|| anyhow!("rewrite traceability has no {record_kind} array"))?;
    for record in records {
        let record = record
            .as_table_mut()
            .ok_or_else(|| anyhow!("{record_kind} record must be a table"))?;
        let id = required_string_field(record, "id", record_kind)?.to_string();
        require_finalizable_status(record, &id)?;
        record.insert(
            "status".to_string(),
            toml::Value::String("accepted".to_string()),
        );
    }
    Ok(())
}

fn require_finalizable_status(
    record: &toml::map::Map<String, toml::Value>,
    id: &str,
) -> Result<()> {
    let status = required_string_field(record, "status", id)?;
    if !matches!(status, "planned" | "implementing" | "verified" | "accepted") {
        bail!("{id} status {status} cannot be finalized automatically");
    }
    Ok(())
}

fn required_string_field<'a>(
    record: &'a toml::map::Map<String, toml::Value>,
    field: &str,
    label: &str,
) -> Result<&'a str> {
    record
        .get(field)
        .and_then(toml::Value::as_str)
        .ok_or_else(|| anyhow!("{label} has no string {field}"))
}

fn validate_certifiable_report(
    root: &Path,
    case: &ValidationCase,
    test: &TraceTest,
    report: &ValidationReport,
    current_identities: &[SourceIdentity],
    report_path: &Path,
) -> Result<()> {
    if report.schema_version != 1
        || report.case_id != case.id
        || report.requirement_ids != test.requirement_ids
        || report.evidence_kinds != case.evidence_kinds
        || report.target_profile != test.target_profile
    {
        bail!(
            "rewrite validation report {} does not match current manifests",
            report_path.display()
        );
    }
    if !report.passed
        || !report.certifiable
        || report.commands.len() != case.commands.len()
        || report.commands.iter().any(|command| !command.passed)
    {
        bail!(
            "rewrite validation report {} is not passing and certifiable",
            report_path.display()
        );
    }
    for (expected, actual) in case.commands.iter().zip(&report.commands) {
        if actual.command != *expected || actual.timed_out || actual.exit_code != Some(0) {
            bail!(
                "rewrite validation report {} command results do not match the current case",
                report_path.display()
            );
        }
    }
    if report.source_identities.len() != current_identities.len() {
        bail!(
            "rewrite validation report {} has incomplete source identities",
            report_path.display()
        );
    }
    for current in current_identities {
        let recorded = report
            .source_identities
            .iter()
            .find(|identity| identity.repository == current.repository)
            .ok_or_else(|| {
                anyhow!(
                    "rewrite validation report {} omits {}",
                    report_path.display(),
                    current.repository
                )
            })?;
        if recorded.dirty
            || recorded.commit_sha != current.commit_sha
            || recorded.path != current.path
        {
            bail!(
                "rewrite validation report {} source identity drifted for {}",
                report_path.display(),
                current.repository
            );
        }
    }
    if report.operator.trim().is_empty()
        || report.device_os.trim().is_empty()
        || report.toolchain.is_empty()
    {
        bail!(
            "rewrite validation report {} has incomplete execution identity",
            report_path.display()
        );
    }
    if report.artifacts.len() != case.required_artifacts.len() {
        bail!(
            "rewrite validation report {} has incomplete artifact inventory",
            report_path.display()
        );
    }
    for expected_path in &case.required_artifacts {
        let recorded = report
            .artifacts
            .iter()
            .find(|artifact| artifact.path == *expected_path)
            .ok_or_else(|| {
                anyhow!(
                    "rewrite validation report {} omits artifact {}",
                    report_path.display(),
                    expected_path
                )
            })?;
        let current = artifact_digest(root, expected_path)?;
        if recorded.bytes != current.bytes || recorded.sha256 != current.sha256 {
            bail!(
                "rewrite validation report {} artifact {} drifted",
                report_path.display(),
                expected_path
            );
        }
    }
    Ok(())
}

fn evidence_id(test_id: &str, kind: &str) -> String {
    let test = test_id.strip_prefix("TEST-").unwrap_or(test_id);
    let kind = kind
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() {
                character.to_ascii_uppercase()
            } else {
                '-'
            }
        })
        .collect::<String>();
    format!("EVID-{test}-{kind}")
}

fn sha256_hex(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    format!("{digest:x}")
}

fn load_validation_cases(root: &Path) -> Result<BTreeMap<String, ValidationCase>> {
    let path = root.join("eng/rewrite-validation.toml");
    let source =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let manifest: ValidationManifest =
        toml::from_str(&source).with_context(|| format!("could not parse {}", path.display()))?;
    if manifest.schema_version != 1 {
        bail!("rewrite validation schema_version must be 1");
    }
    unique_by_id("validation case", manifest.case, |record| &record.id)
}

fn load_trace_tests(root: &Path) -> Result<BTreeMap<String, TraceTest>> {
    let manifest = load_traceability(root)?;
    unique_by_id("traceability test", manifest.test, |record| &record.id)
}

fn load_traceability(root: &Path) -> Result<TraceabilityManifest> {
    let path = root.join("rewrite-traceability.toml");
    let source =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    toml::from_str(&source).with_context(|| format!("could not parse {}", path.display()))
}

fn unique_by_id<T>(
    label: &str,
    records: Vec<T>,
    id: impl Fn(&T) -> &String,
) -> Result<BTreeMap<String, T>> {
    let mut indexed = BTreeMap::new();
    for record in records {
        let key = id(&record).clone();
        if indexed.insert(key.clone(), record).is_some() {
            bail!("duplicate {label} ID {key}");
        }
    }
    Ok(indexed)
}

fn run_case(root: &Path, case: &ValidationCase, test: &TraceTest) -> Result<ValidationReport> {
    if case.commands.is_empty() {
        bail!("validation case {} has no commands", case.id);
    }
    let working_directory = repository_path(root, &case.working_directory)?;
    let toolchain = toolchain_identity(&working_directory, &case.commands)?;
    let (operator, device_os) = execution_environment(&working_directory)?;
    let source_identities = ["volang", "vogui", "voplay"]
        .into_iter()
        .map(|repository| source_identity(root, repository))
        .collect::<Result<Vec<_>>>()?;
    let certifiable = source_identities.iter().all(|identity| {
        !identity.dirty
            && identity.commit_sha.len() == 40
            && identity
                .commit_sha
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit())
    });
    let mut command_results = Vec::with_capacity(case.commands.len());
    let mut passed = true;
    let case_started = Instant::now();
    let timeout = Duration::from_secs(test.timeout_seconds);
    for command in &case.commands {
        let words = split_command(command)?;
        let started = Instant::now();
        let mut child = Command::new(&words[0])
            .args(&words[1..])
            .current_dir(&working_directory)
            .spawn()
            .with_context(|| format!("could not execute validation command {command}"))?;
        let (status, timed_out) = loop {
            if let Some(status) = child
                .try_wait()
                .with_context(|| format!("could not poll validation command {command}"))?
            {
                break (Some(status), false);
            }
            if case_started.elapsed() >= timeout {
                child
                    .kill()
                    .with_context(|| format!("could not terminate timed-out command {command}"))?;
                let status = child
                    .wait()
                    .with_context(|| format!("could not reap timed-out command {command}"))?;
                break (Some(status), true);
            }
            std::thread::sleep(Duration::from_millis(25));
        };
        let command_passed = status.is_some_and(|status| status.success()) && !timed_out;
        command_results.push(CommandResult {
            command: command.clone(),
            duration_millis: started.elapsed().as_millis(),
            exit_code: status.and_then(|status| status.code()),
            timed_out,
            passed: command_passed,
        });
        if !command_passed {
            passed = false;
            break;
        }
    }
    let artifacts = case
        .required_artifacts
        .iter()
        .map(|path| artifact_digest(root, path))
        .collect::<Result<Vec<_>>>()?;
    Ok(ValidationReport {
        schema_version: 1,
        generated_at_unix_millis: SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .context("system time precedes Unix epoch")?
            .as_millis(),
        case_id: case.id.clone(),
        requirement_ids: test.requirement_ids.clone(),
        evidence_kinds: case.evidence_kinds.clone(),
        target_profile: test.target_profile.clone(),
        working_directory: working_directory.display().to_string(),
        toolchain,
        operator,
        device_os,
        source_identities,
        certifiable,
        commands: command_results,
        artifacts,
        passed,
    })
}

fn execution_environment(working_directory: &Path) -> Result<(String, String)> {
    let operator = std::env::var("USER")
        .or_else(|_| std::env::var("USERNAME"))
        .unwrap_or_else(|_| "local-operator".to_string());
    let mut device_os = format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH);
    if cfg!(target_family = "unix") {
        let uname = command_stdout(working_directory, "uname", &["-s", "-r", "-m"])?;
        device_os = uname;
    }
    Ok((operator, device_os))
}

fn toolchain_identity(
    working_directory: &Path,
    commands: &[String],
) -> Result<BTreeMap<String, String>> {
    let programs = commands
        .iter()
        .map(|command| split_command(command))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .filter_map(|words| words.into_iter().next())
        .collect::<std::collections::BTreeSet<_>>();
    let mut identity = BTreeMap::new();
    for program in programs {
        let arguments = match program.as_str() {
            "cargo" | "npm" | "node" => ["--version"].as_slice(),
            _ => continue,
        };
        identity.insert(
            program.clone(),
            command_stdout(working_directory, &program, arguments)?,
        );
        if program == "cargo" {
            identity.insert(
                "rustc".to_string(),
                command_stdout(working_directory, "rustc", &["--version"])?,
            );
        }
    }
    Ok(identity)
}

fn repository_path(root: &Path, repository: &str) -> Result<PathBuf> {
    match repository {
        "volang" => Ok(root.to_path_buf()),
        "vogui" | "voplay" => root
            .parent()
            .map(|parent| parent.join(repository))
            .ok_or_else(|| anyhow!("Volang root has no sibling repository directory")),
        _ => bail!("unknown validation working_directory {repository}"),
    }
}

fn source_identity(root: &Path, repository: &str) -> Result<SourceIdentity> {
    let path = repository_path(root, repository)?;
    let commit_sha = command_stdout(&path, "git", &["rev-parse", "HEAD"])?;
    let dirty = !command_stdout(&path, "git", &["status", "--porcelain=v1"])?.is_empty();
    Ok(SourceIdentity {
        repository: repository.to_string(),
        path: path.display().to_string(),
        commit_sha,
        dirty,
    })
}

fn command_stdout(directory: &Path, program: &str, arguments: &[&str]) -> Result<String> {
    let output = Command::new(program)
        .args(arguments)
        .current_dir(directory)
        .output()
        .with_context(|| format!("could not run {program} in {}", directory.display()))?;
    if !output.status.success() {
        bail!(
            "{program} failed in {}: {}",
            directory.display(),
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    String::from_utf8(output.stdout)
        .context("command output is not UTF-8")
        .map(|output| output.trim().to_string())
}

fn split_command(command: &str) -> Result<Vec<String>> {
    if command.contains(['\'', '"', '\\', '$', '`', ';', '|', '&', '<', '>']) {
        bail!("validation command uses unsupported shell syntax: {command}");
    }
    let words = command
        .split_ascii_whitespace()
        .map(str::to_string)
        .collect::<Vec<_>>();
    if words.is_empty() {
        bail!("validation command is empty");
    }
    Ok(words)
}

fn artifact_digest(root: &Path, relative_path: &str) -> Result<ArtifactDigest> {
    let path = root.join(relative_path);
    let metadata =
        fs::metadata(&path).with_context(|| format!("could not stat {}", path.display()))?;
    if !metadata.is_file() {
        bail!(
            "required artifact is not a regular file: {}",
            path.display()
        );
    }
    let bytes = fs::read(&path).with_context(|| format!("could not read {}", path.display()))?;
    let digest = Sha256::digest(&bytes);
    Ok(ArtifactDigest {
        path: relative_path.to_string(),
        bytes: metadata.len(),
        sha256: format!("{digest:x}"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn command_split_rejects_shell_syntax_and_preserves_plain_arguments() {
        assert_eq!(
            split_command("cargo test -p vo-app-runtime --locked").unwrap(),
            vec!["cargo", "test", "-p", "vo-app-runtime", "--locked"]
        );
        assert!(split_command("cargo test && echo forged").is_err());
        assert!(split_command("node 'script with spaces.mjs'").is_err());
    }

    #[test]
    fn evidence_ids_are_stable_and_canonical() {
        assert_eq!(
            evidence_id("TEST-RENDEREXT-001", "native-artifact"),
            "EVID-RENDEREXT-001-NATIVE-ARTIFACT"
        );
    }

    #[test]
    fn finalization_accepts_records_and_binds_direct_evidence() {
        let source = r#"
schema_version = 1

[[requirement]]
id = "REQ-X-001"
status = "implementing"

[[test]]
id = "TEST-X-001"
status = "implementing"

[[acceptance]]
id = "ACC-X-001"
status = "planned"
test_refs = ["TEST-X-001"]
evidence_refs = []
"#;
        let evidence = vec![GeneratedEvidence {
            id: "EVID-X-001-UNIT".to_string(),
            requirement_ids: vec!["REQ-X-001".to_string()],
            test_ids: vec!["TEST-X-001".to_string()],
            acceptance_ids: vec!["ACC-X-001".to_string()],
            kind: "unit".to_string(),
            commit_sha: "a".repeat(40),
            artifact_sha256: "b".repeat(64),
            target_profile: "test".to_string(),
            toolchain: "cargo=test".to_string(),
            run_at: "unix-ms:1".to_string(),
            duration_millis: 1,
            result: "passed".to_string(),
            operator: "tester".to_string(),
            device_os: "test-os".to_string(),
            attachments: Vec::new(),
        }];
        let finalized = finalize_traceability(source, &evidence).unwrap();
        let manifest: toml::Value = toml::from_str(&finalized).unwrap();
        assert_eq!(
            manifest["requirement"][0]["status"].as_str(),
            Some("accepted")
        );
        assert_eq!(manifest["test"][0]["status"].as_str(), Some("accepted"));
        assert_eq!(
            manifest["acceptance"][0]["status"].as_str(),
            Some("accepted")
        );
        assert_eq!(
            manifest["acceptance"][0]["evidence_refs"][0].as_str(),
            Some("EVID-X-001-UNIT")
        );
        assert_eq!(
            manifest["evidence"][0]["id"].as_str(),
            Some("EVID-X-001-UNIT")
        );
    }

    #[test]
    fn finalization_rejects_acceptance_without_direct_test_evidence() {
        let source = r#"
[[requirement]]
id = "REQ-X-001"
status = "implementing"

[[test]]
id = "TEST-X-001"
status = "implementing"

[[acceptance]]
id = "ACC-X-001"
status = "planned"
test_refs = ["TEST-X-001"]
evidence_refs = []
"#;
        let evidence = vec![GeneratedEvidence {
            id: "EVID-X-002-UNIT".to_string(),
            requirement_ids: vec!["REQ-X-001".to_string()],
            test_ids: vec!["TEST-X-002".to_string()],
            acceptance_ids: vec!["ACC-X-001".to_string()],
            kind: "unit".to_string(),
            commit_sha: "a".repeat(40),
            artifact_sha256: "b".repeat(64),
            target_profile: "test".to_string(),
            toolchain: "cargo=test".to_string(),
            run_at: "unix-ms:1".to_string(),
            duration_millis: 1,
            result: "passed".to_string(),
            operator: "tester".to_string(),
            device_os: "test-os".to_string(),
            attachments: Vec::new(),
        }];
        let error = finalize_traceability(source, &evidence).unwrap_err();
        assert!(error
            .to_string()
            .contains("TEST-X-001 has no generated direct evidence"));
    }
}
