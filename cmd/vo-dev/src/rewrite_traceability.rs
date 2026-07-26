use anyhow::{bail, Context, Result};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

const REQUIREMENT_STATES: &[&str] = &[
    "planned",
    "implementing",
    "verified",
    "accepted",
    "deferred",
    "superseded",
];
const WORK_STATES: &[&str] = &["planned", "implementing", "verified", "accepted"];

#[derive(Deserialize)]
struct Manifest {
    schema_version: u32,
    plan: String,
    requirement: Vec<Requirement>,
    test: Vec<TestRecord>,
    acceptance: Vec<Acceptance>,
    #[serde(default)]
    evidence: Vec<Evidence>,
}

#[derive(Deserialize)]
struct Requirement {
    id: String,
    source_refs: Vec<String>,
    design_refs: Vec<String>,
    milestone_refs: Vec<String>,
    test_refs: Vec<String>,
    acceptance_refs: Vec<String>,
    required_evidence_kinds: Vec<String>,
    owner_repo: String,
    owner_module: String,
    status: String,
}

#[derive(Deserialize)]
struct TestRecord {
    id: String,
    requirement_ids: Vec<String>,
    owner: String,
    scope: String,
    target_profile: String,
    fixture: String,
    timeout_seconds: u64,
    ci_tier: String,
    status: String,
}

#[derive(Deserialize)]
struct Acceptance {
    id: String,
    source_ref: String,
    status: String,
    test_refs: Vec<String>,
    evidence_refs: Vec<String>,
}

#[derive(Deserialize)]
struct Evidence {
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
    #[serde(default)]
    operator: String,
    #[serde(default)]
    device_os: String,
    #[serde(default)]
    attachments: Vec<String>,
}

#[derive(Deserialize)]
struct ValidationManifest {
    schema_version: u32,
    case: Vec<ValidationCase>,
}

#[derive(Deserialize)]
struct ValidationCase {
    id: String,
    working_directory: String,
    evidence_kinds: Vec<String>,
    commands: Vec<String>,
    required_artifacts: Vec<String>,
}

pub(crate) fn lint(root: &Path) -> Result<()> {
    let path = root.join("rewrite-traceability.toml");
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let manifest: Manifest =
        toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
    if manifest.schema_version != 1 {
        bail!("rewrite traceability schema_version must be 1");
    }
    let plan_path = root.join(&manifest.plan);
    let plan = fs::read_to_string(&plan_path)
        .with_context(|| format!("could not read traceability plan {}", plan_path.display()))?;
    let planned_acceptances = acceptance_ids(&plan);
    if planned_acceptances.is_empty() {
        bail!("rewrite plan contains no ACC identifiers");
    }

    let requirements = unique_by_id("requirement", &manifest.requirement, |item| &item.id)?;
    let tests = unique_by_id("test", &manifest.test, |item| &item.id)?;
    let acceptances = unique_by_id("acceptance", &manifest.acceptance, |item| &item.id)?;
    let evidence = unique_by_id("evidence", &manifest.evidence, |item| &item.id)?;
    let validation_path = root.join("eng/rewrite-validation.toml");
    let validation_text = fs::read_to_string(&validation_path)
        .with_context(|| format!("could not read {}", validation_path.display()))?;
    let validation: ValidationManifest = toml::from_str(&validation_text)
        .with_context(|| format!("could not parse {}", validation_path.display()))?;
    if validation.schema_version != 1 {
        bail!("rewrite validation schema_version must be 1");
    }
    let validation_cases = unique_by_id("validation case", &validation.case, |item| &item.id)?;
    let recorded_tests = tests.keys().cloned().collect::<BTreeSet<_>>();
    let executable_tests = validation_cases.keys().cloned().collect::<BTreeSet<_>>();
    report_set_difference(
        "tests without an executable validation case",
        &recorded_tests,
        &executable_tests,
    )?;

    let recorded_acceptances = acceptances.keys().cloned().collect::<BTreeSet<_>>();
    report_set_difference(
        "acceptance records missing from manifest",
        &planned_acceptances,
        &recorded_acceptances,
    )?;
    report_set_difference(
        "acceptance records absent from plan",
        &recorded_acceptances,
        &planned_acceptances,
    )?;

    let mut covered_acceptances = BTreeSet::new();
    let mut acceptance_requirements = BTreeMap::<String, BTreeSet<String>>::new();
    for requirement in requirements.values() {
        validate_id("requirement", &requirement.id, "REQ-")?;
        validate_state(
            "requirement",
            &requirement.id,
            &requirement.status,
            REQUIREMENT_STATES,
        )?;
        require_nonempty(&requirement.id, "source_refs", &requirement.source_refs)?;
        require_nonempty(&requirement.id, "design_refs", &requirement.design_refs)?;
        require_nonempty(
            &requirement.id,
            "milestone_refs",
            &requirement.milestone_refs,
        )?;
        require_nonempty(&requirement.id, "test_refs", &requirement.test_refs)?;
        require_nonempty(
            &requirement.id,
            "acceptance_refs",
            &requirement.acceptance_refs,
        )?;
        require_nonempty(
            &requirement.id,
            "required_evidence_kinds",
            &requirement.required_evidence_kinds,
        )?;
        if requirement.owner_repo.trim().is_empty() || requirement.owner_module.trim().is_empty() {
            bail!(
                "{} must declare owner_repo and owner_module",
                requirement.id
            );
        }
        for test in &requirement.test_refs {
            let record = tests.get(test).ok_or_else(|| {
                anyhow::anyhow!("{} references unknown test {test}", requirement.id)
            })?;
            if !record.requirement_ids.contains(&requirement.id) {
                bail!(
                    "{} references test {} without a matching reverse requirement_id",
                    requirement.id,
                    test
                );
            }
        }
        for acceptance in &requirement.acceptance_refs {
            if !acceptances.contains_key(acceptance) {
                bail!(
                    "{} references unknown acceptance {acceptance}",
                    requirement.id
                );
            }
            covered_acceptances.insert(acceptance.clone());
            acceptance_requirements
                .entry(acceptance.clone())
                .or_default()
                .insert(requirement.id.clone());
        }
    }
    report_set_difference(
        "acceptances without a requirement",
        &recorded_acceptances,
        &covered_acceptances,
    )?;

    for test in tests.values() {
        validate_id("test", &test.id, "TEST-")?;
        validate_state("test", &test.id, &test.status, WORK_STATES)?;
        require_nonempty(&test.id, "requirement_ids", &test.requirement_ids)?;
        if test.owner.trim().is_empty()
            || test.scope.trim().is_empty()
            || test.target_profile.trim().is_empty()
            || test.fixture.trim().is_empty()
            || test.ci_tier.trim().is_empty()
            || test.timeout_seconds == 0
        {
            bail!("{} has incomplete execution metadata", test.id);
        }
        if test.fixture == "pending" {
            bail!("{} still uses a pending fixture", test.id);
        }
        let fixture = root.join(&test.fixture);
        if !test.fixture.starts_with("../") && !fixture.exists() {
            bail!("{} fixture does not exist: {}", test.id, fixture.display());
        }
        if test.fixture == "eng/rewrite-validation.toml" && !validation_cases.contains_key(&test.id)
        {
            bail!(
                "{} uses the rewrite validation manifest without a matching case",
                test.id
            );
        }
        for requirement in &test.requirement_ids {
            let record = requirements.get(requirement).ok_or_else(|| {
                anyhow::anyhow!("{} references unknown requirement {requirement}", test.id)
            })?;
            if !record.test_refs.contains(&test.id) {
                bail!(
                    "{} references requirement {} without a matching reverse test_ref",
                    test.id,
                    requirement
                );
            }
        }
    }
    for case in validation_cases.values() {
        let test = tests.get(&case.id).ok_or_else(|| {
            anyhow::anyhow!("validation case {} has no traceability test", case.id)
        })?;
        if case.evidence_kinds.is_empty()
            || case
                .evidence_kinds
                .iter()
                .any(|kind| kind.trim().is_empty())
        {
            bail!(
                "validation case {} must declare non-empty evidence_kinds",
                case.id
            );
        }
        for kind in &case.evidence_kinds {
            let linked_requirements = test
                .requirement_ids
                .iter()
                .filter_map(|requirement_id| {
                    requirements
                        .get(requirement_id)
                        .filter(|requirement| requirement.required_evidence_kinds.contains(kind))
                        .copied()
                })
                .collect::<Vec<_>>();
            if linked_requirements.is_empty() {
                bail!(
                    "validation case {} evidence kind {} is not required by any linked requirement",
                    case.id,
                    kind
                );
            }
            let directly_accepted = linked_requirements.iter().any(|requirement| {
                requirement.acceptance_refs.iter().any(|acceptance_id| {
                    acceptances
                        .get(acceptance_id)
                        .is_some_and(|acceptance| acceptance.test_refs.contains(&case.id))
                })
            });
            if !directly_accepted {
                bail!(
                    "validation case {} evidence kind {} has no directly linked acceptance",
                    case.id,
                    kind
                );
            }
            if (kind.ends_with("-smoke") || kind == "real-platform")
                && case.required_artifacts.is_empty()
            {
                bail!(
                    "validation case {} evidence kind {} requires an attachment artifact",
                    case.id,
                    kind
                );
            }
        }
        if case.working_directory.trim().is_empty() || case.commands.is_empty() {
            bail!("validation case {} is incomplete", case.id);
        }
        if case
            .commands
            .iter()
            .any(|command| command.trim().is_empty())
            || case
                .required_artifacts
                .iter()
                .any(|artifact| artifact.trim().is_empty())
        {
            bail!(
                "validation case {} contains an empty command or artifact",
                case.id
            );
        }
        if !matches!(
            case.working_directory.as_str(),
            "volang" | "vogui" | "voplay"
        ) {
            bail!(
                "validation case {} has unknown working_directory {}",
                case.id,
                case.working_directory
            );
        }
        for artifact in &case.required_artifacts {
            if artifact.starts_with("../") || artifact.starts_with("target/rewrite-validation/") {
                continue;
            }
            let artifact_path = root.join(artifact);
            if !artifact_path.exists() {
                bail!(
                    "validation case {} requires missing artifact {}",
                    case.id,
                    artifact_path.display()
                );
            }
        }
    }
    for requirement in requirements.values() {
        for test_id in &requirement.test_refs {
            let case = validation_cases
                .get(test_id)
                .expect("requirement test references were validated");
            if !case
                .evidence_kinds
                .iter()
                .any(|kind| requirement.required_evidence_kinds.contains(kind))
            {
                bail!(
                    "{} test {} cannot generate direct evidence for the requirement",
                    requirement.id,
                    test_id
                );
            }
        }
        let planned_kinds = requirement
            .test_refs
            .iter()
            .filter_map(|test_id| validation_cases.get(test_id))
            .flat_map(|case| case.evidence_kinds.iter().cloned())
            .collect::<BTreeSet<_>>();
        let missing = requirement
            .required_evidence_kinds
            .iter()
            .filter(|kind| !planned_kinds.contains(*kind))
            .cloned()
            .collect::<Vec<_>>();
        if !missing.is_empty() {
            bail!(
                "{} has no validation case planned for evidence kinds: {}",
                requirement.id,
                missing.join(", ")
            );
        }
    }

    for acceptance in acceptances.values() {
        validate_id("acceptance", &acceptance.id, "ACC-")?;
        validate_state(
            "acceptance",
            &acceptance.id,
            &acceptance.status,
            WORK_STATES,
        )?;
        if acceptance.source_ref.trim().is_empty() {
            bail!("{} must declare source_ref", acceptance.id);
        }
        require_nonempty(&acceptance.id, "test_refs", &acceptance.test_refs)?;
        let owning_requirements = acceptance_requirements
            .get(&acceptance.id)
            .ok_or_else(|| anyhow::anyhow!("{} has no owning requirement", acceptance.id))?;
        for test in &acceptance.test_refs {
            let record = tests.get(test).ok_or_else(|| {
                anyhow::anyhow!("{} references unknown test {test}", acceptance.id)
            })?;
            let shared_requirements = record
                .requirement_ids
                .iter()
                .filter(|requirement| owning_requirements.contains(*requirement))
                .filter_map(|requirement| requirements.get(requirement))
                .collect::<Vec<_>>();
            if shared_requirements.is_empty() {
                bail!(
                    "{} references test {} without a shared requirement",
                    acceptance.id,
                    test
                );
            }
            let case = validation_cases
                .get(test)
                .expect("validation case coverage was checked");
            if !case.evidence_kinds.iter().any(|kind| {
                shared_requirements
                    .iter()
                    .any(|requirement| requirement.required_evidence_kinds.contains(kind))
            }) {
                bail!(
                    "{} test {} cannot generate direct evidence through a shared requirement",
                    acceptance.id,
                    test
                );
            }
        }
        for evidence_id in &acceptance.evidence_refs {
            let record = evidence.get(evidence_id).ok_or_else(|| {
                anyhow::anyhow!(
                    "{} references unknown evidence {evidence_id}",
                    acceptance.id
                )
            })?;
            if !record.acceptance_ids.contains(&acceptance.id) {
                bail!(
                    "{} references evidence {} without a matching reverse acceptance reference",
                    acceptance.id,
                    evidence_id
                );
            }
            if record.result != "passed" {
                bail!(
                    "{} references non-passing evidence {}",
                    acceptance.id,
                    evidence_id
                );
            }
        }
        if matches!(acceptance.status.as_str(), "verified" | "accepted")
            && acceptance.evidence_refs.is_empty()
        {
            bail!(
                "{} cannot be {} without evidence",
                acceptance.id,
                acceptance.status
            );
        }
        if matches!(acceptance.status.as_str(), "verified" | "accepted") {
            for test_id in &acceptance.test_refs {
                let covered = acceptance.evidence_refs.iter().any(|evidence_id| {
                    evidence.get(evidence_id).is_some_and(|record| {
                        record.result == "passed" && record.test_ids.contains(test_id)
                    })
                });
                if !covered {
                    bail!(
                        "{} cannot be {} because test {} has no passing direct evidence",
                        acceptance.id,
                        acceptance.status,
                        test_id
                    );
                }
            }
        }
    }
    for record in evidence.values() {
        validate_id("evidence", &record.id, "EVID-")?;
        require_nonempty(&record.id, "requirement_ids", &record.requirement_ids)?;
        require_nonempty(&record.id, "test_ids", &record.test_ids)?;
        require_nonempty(&record.id, "acceptance_ids", &record.acceptance_ids)?;
        if record.kind.trim().is_empty()
            || record.target_profile.trim().is_empty()
            || record.toolchain.trim().is_empty()
            || record.run_at.trim().is_empty()
            || record.duration_millis == 0
        {
            bail!("{} has incomplete execution metadata", record.id);
        }
        validate_lower_hex(&record.id, "commit_sha", &record.commit_sha, 40)?;
        validate_lower_hex(&record.id, "artifact_sha256", &record.artifact_sha256, 64)?;
        if !matches!(record.result.as_str(), "passed" | "failed") {
            bail!("{} has invalid result {}", record.id, record.result);
        }
        let manual_smoke = record.kind.ends_with("-smoke") || record.kind == "manual";
        if manual_smoke
            && (record.operator.trim().is_empty()
                || record.device_os.trim().is_empty()
                || record.attachments.is_empty()
                || record
                    .attachments
                    .iter()
                    .any(|attachment| attachment.trim().is_empty()))
        {
            bail!(
                "{} manual/platform smoke must declare operator, device_os and attachments",
                record.id
            );
        }
        for requirement_id in &record.requirement_ids {
            let requirement = requirements.get(requirement_id).ok_or_else(|| {
                anyhow::anyhow!(
                    "{} references unknown requirement {requirement_id}",
                    record.id
                )
            })?;
            if !requirement.required_evidence_kinds.contains(&record.kind) {
                bail!(
                    "{} kind {} is not required by {}",
                    record.id,
                    record.kind,
                    requirement_id
                );
            }
        }
        for test_id in &record.test_ids {
            let test = tests.get(test_id).ok_or_else(|| {
                anyhow::anyhow!("{} references unknown test {test_id}", record.id)
            })?;
            if !test
                .requirement_ids
                .iter()
                .any(|requirement| record.requirement_ids.contains(requirement))
            {
                bail!(
                    "{} test {} has no requirement in common with the evidence",
                    record.id,
                    test_id
                );
            }
        }
        for acceptance_id in &record.acceptance_ids {
            let acceptance = acceptances.get(acceptance_id).ok_or_else(|| {
                anyhow::anyhow!(
                    "{} references unknown acceptance {acceptance_id}",
                    record.id
                )
            })?;
            if !acceptance.evidence_refs.contains(&record.id) {
                bail!(
                    "{} references acceptance {} without a matching evidence_refs entry",
                    record.id,
                    acceptance_id
                );
            }
        }
    }
    for requirement in requirements.values() {
        if !matches!(requirement.status.as_str(), "verified" | "accepted") {
            continue;
        }
        let covered_kinds = evidence
            .values()
            .filter(|record| {
                record.result == "passed" && record.requirement_ids.contains(&requirement.id)
            })
            .map(|record| record.kind.clone())
            .collect::<BTreeSet<_>>();
        let missing_kinds = requirement
            .required_evidence_kinds
            .iter()
            .filter(|kind| !covered_kinds.contains(*kind))
            .cloned()
            .collect::<Vec<_>>();
        if !missing_kinds.is_empty() {
            bail!(
                "{} cannot be {} without passing evidence kinds: {}",
                requirement.id,
                requirement.status,
                missing_kinds.join(", ")
            );
        }
        for test_id in &requirement.test_refs {
            let covered = evidence.values().any(|record| {
                record.result == "passed"
                    && record.requirement_ids.contains(&requirement.id)
                    && record.test_ids.contains(test_id)
            });
            if !covered {
                bail!(
                    "{} cannot be {} because test {} has no passing evidence",
                    requirement.id,
                    requirement.status,
                    test_id
                );
            }
        }
        let required_acceptance_state = if requirement.status == "accepted" {
            "accepted"
        } else {
            "verified or accepted"
        };
        for acceptance_id in &requirement.acceptance_refs {
            let acceptance = acceptances
                .get(acceptance_id)
                .expect("acceptance references were validated");
            let accepted = if requirement.status == "accepted" {
                acceptance.status == "accepted"
            } else {
                matches!(acceptance.status.as_str(), "verified" | "accepted")
            };
            if !accepted {
                bail!(
                    "{} cannot be {} while {} is {}; expected {}",
                    requirement.id,
                    requirement.status,
                    acceptance.id,
                    acceptance.status,
                    required_acceptance_state
                );
            }
        }
    }
    Ok(())
}

fn unique_by_id<'a, T>(
    kind: &str,
    items: &'a [T],
    id: impl Fn(&'a T) -> &'a String,
) -> Result<BTreeMap<String, &'a T>> {
    let mut result = BTreeMap::new();
    for item in items {
        let key = id(item);
        if result.insert(key.clone(), item).is_some() {
            bail!("duplicate {kind} id {key}");
        }
    }
    Ok(result)
}

fn acceptance_ids(text: &str) -> BTreeSet<String> {
    text.split(|character: char| {
        !(character.is_ascii_uppercase() || character.is_ascii_digit() || character == '-')
    })
    .filter(|token| token.starts_with("ACC-") && token.len() >= 10)
    .map(str::to_owned)
    .collect()
}

fn validate_id(kind: &str, id: &str, prefix: &str) -> Result<()> {
    if !id.starts_with(prefix)
        || id.chars().any(|character| {
            !(character.is_ascii_uppercase() || character.is_ascii_digit() || character == '-')
        })
    {
        bail!("invalid {kind} id {id}");
    }
    Ok(())
}

fn validate_lower_hex(owner: &str, field: &str, value: &str, length: usize) -> Result<()> {
    if value.len() != length
        || !value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f'))
    {
        bail!("{owner} {field} must be exactly {length} lowercase hexadecimal characters");
    }
    Ok(())
}

fn validate_state(kind: &str, id: &str, state: &str, allowed: &[&str]) -> Result<()> {
    if !allowed.contains(&state) {
        bail!("{kind} {id} has invalid state {state}");
    }
    Ok(())
}

fn require_nonempty(id: &str, field: &str, values: &[String]) -> Result<()> {
    if values.is_empty() || values.iter().any(|value| value.trim().is_empty()) {
        bail!("{id} must declare non-empty {field}");
    }
    Ok(())
}

fn report_set_difference(
    message: &str,
    expected: &BTreeSet<String>,
    actual: &BTreeSet<String>,
) -> Result<()> {
    let missing = expected.difference(actual).cloned().collect::<Vec<_>>();
    if !missing.is_empty() {
        bail!("{message}: {}", missing.join(", "));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{acceptance_ids, validate_lower_hex};

    #[test]
    fn extracts_unique_acceptance_ids_from_markdown() {
        let ids = acceptance_ids("`ACC-RT-001` x ACC-RT-001; ACC-GUI-020");
        assert_eq!(
            ids.into_iter().collect::<Vec<_>>(),
            ["ACC-GUI-020", "ACC-RT-001"]
        );
    }

    #[test]
    fn evidence_hashes_require_exact_lowercase_hex() {
        assert!(validate_lower_hex("EVID-X", "commit_sha", &"a".repeat(40), 40).is_ok());
        assert!(validate_lower_hex("EVID-X", "commit_sha", &"A".repeat(40), 40).is_err());
        assert!(validate_lower_hex("EVID-X", "artifact_sha256", &"0".repeat(63), 64).is_err());
        assert!(validate_lower_hex("EVID-X", "artifact_sha256", &"g".repeat(64), 64).is_err());
    }
}
