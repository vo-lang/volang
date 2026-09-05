use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Component, Path};

pub(crate) const CI_MANIFEST_PATH: &str = "eng/ci.toml";

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CiManifest {
    pub(crate) version: u32,
    #[serde(rename = "profile")]
    pub(crate) profiles: Vec<CiProfile>,
    #[serde(rename = "task")]
    pub(crate) tasks: Vec<CiTask>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CiProfile {
    pub(crate) name: String,
    pub(crate) tier: String,
    #[serde(default)]
    pub(crate) changed_only: bool,
    pub(crate) tasks: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CiTask {
    pub(crate) id: String,
    pub(crate) tier: String,
    pub(crate) workflow_job: String,
    pub(crate) runner: String,
    #[serde(default)]
    pub(crate) always: bool,
    #[serde(default)]
    pub(crate) depends_on: Vec<String>,
    pub(crate) owners: Vec<String>,
    pub(crate) impact: Vec<String>,
    pub(crate) platforms: Vec<String>,
    pub(crate) capabilities: Vec<String>,
    pub(crate) timeout_minutes: u32,
    pub(crate) budget_minutes: u32,
    pub(crate) evidence_kind: String,
    #[serde(default)]
    pub(crate) results: Vec<String>,
    #[serde(default)]
    pub(crate) artifacts: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct SourceIdentity {
    pub(crate) commit: String,
    pub(crate) tree: String,
    pub(crate) tracked_dirty: bool,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FileDigest {
    pub(crate) path: String,
    pub(crate) kind: String,
    pub(crate) size: u64,
    pub(crate) sha256: String,
}

pub(crate) fn load_manifest(root: &Path) -> Result<CiManifest> {
    let path = root.join(CI_MANIFEST_PATH);
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let manifest: CiManifest =
        toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
    validate_manifest(&manifest)?;
    Ok(manifest)
}

pub(crate) fn manifest_digest(root: &Path) -> Result<String> {
    let path = root.join(CI_MANIFEST_PATH);
    let bytes = fs::read(&path).with_context(|| format!("could not read {}", path.display()))?;
    Ok(sha256_hex(&bytes))
}

pub(crate) fn task_digest(task: &CiTask) -> Result<String> {
    Ok(sha256_hex(&serde_json::to_vec(task)?))
}

pub(crate) fn validate_source_identity(source: &SourceIdentity) -> Result<()> {
    for (name, value) in [("commit", &source.commit), ("tree", &source.tree)] {
        if !matches!(value.len(), 40 | 64)
            || !value
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
        {
            bail!("CI source {name} must be a lowercase Git object id");
        }
    }
    Ok(())
}

pub(crate) fn task_map(manifest: &CiManifest) -> BTreeMap<&str, &CiTask> {
    manifest
        .tasks
        .iter()
        .map(|task| (task.id.as_str(), task))
        .collect()
}

fn validate_manifest(manifest: &CiManifest) -> Result<()> {
    if manifest.version != 1 {
        bail!("eng/ci.toml version must be 1");
    }
    if manifest.profiles.is_empty() || manifest.tasks.is_empty() {
        bail!("eng/ci.toml must declare profiles and tasks");
    }

    let mut task_ids = BTreeSet::new();
    for task in &manifest.tasks {
        validate_token("CI task id", &task.id)?;
        if !task_ids.insert(task.id.as_str()) {
            bail!("duplicate CI task id {}", task.id);
        }
        validate_token("CI task tier", &task.tier)?;
        validate_token("CI workflow job", &task.workflow_job)?;
        validate_runner(&task.runner)?;
        validate_nonempty_tokens(&task.id, "owners", &task.owners)?;
        validate_nonempty_patterns(&task.id, &task.impact)?;
        validate_nonempty_tokens(&task.id, "platforms", &task.platforms)?;
        validate_nonempty_tokens(&task.id, "capabilities", &task.capabilities)?;
        validate_token("CI evidence kind", &task.evidence_kind)?;
        if task.timeout_minutes == 0 || task.budget_minutes == 0 {
            bail!("CI task {} budgets must be positive", task.id);
        }
        if task.budget_minutes > task.timeout_minutes {
            bail!(
                "CI task {} budget {} exceeds timeout {}",
                task.id,
                task.budget_minutes,
                task.timeout_minutes
            );
        }
        for path in task.results.iter().chain(&task.artifacts) {
            validate_output_path(&task.id, path)?;
        }
        reject_duplicates(&task.id, "dependencies", &task.depends_on)?;
        reject_duplicates(&task.id, "results", &task.results)?;
        reject_duplicates(&task.id, "artifacts", &task.artifacts)?;
    }

    let tasks = task_map(manifest);
    for task in &manifest.tasks {
        for dependency in &task.depends_on {
            if !tasks.contains_key(dependency.as_str()) {
                bail!("CI task {} depends on unknown task {dependency}", task.id);
            }
            if dependency == &task.id {
                bail!("CI task {} cannot depend on itself", task.id);
            }
        }
    }
    validate_dependency_cycles(&tasks)?;

    let mut profile_names = BTreeSet::new();
    for profile in &manifest.profiles {
        validate_token("CI profile name", &profile.name)?;
        validate_token("CI profile tier", &profile.tier)?;
        if !profile_names.insert(profile.name.as_str()) {
            bail!("duplicate CI profile {}", profile.name);
        }
        if profile.tasks.is_empty() {
            bail!("CI profile {} has no tasks", profile.name);
        }
        reject_duplicates(&profile.name, "tasks", &profile.tasks)?;
        for task in &profile.tasks {
            if !tasks.contains_key(task.as_str()) {
                bail!("CI profile {} references unknown task {task}", profile.name);
            }
        }
    }
    Ok(())
}

fn validate_dependency_cycles(tasks: &BTreeMap<&str, &CiTask>) -> Result<()> {
    fn visit<'a>(
        id: &'a str,
        tasks: &BTreeMap<&'a str, &'a CiTask>,
        visiting: &mut BTreeSet<&'a str>,
        visited: &mut BTreeSet<&'a str>,
    ) -> Result<()> {
        if visited.contains(id) {
            return Ok(());
        }
        if !visiting.insert(id) {
            bail!("CI task dependency cycle includes {id}");
        }
        for dependency in &tasks[id].depends_on {
            visit(dependency, tasks, visiting, visited)?;
        }
        visiting.remove(id);
        visited.insert(id);
        Ok(())
    }

    let mut visiting = BTreeSet::new();
    let mut visited = BTreeSet::new();
    for id in tasks.keys() {
        visit(id, tasks, &mut visiting, &mut visited)?;
    }
    Ok(())
}

fn validate_runner(runner: &str) -> Result<()> {
    if runner.is_empty()
        || runner.len() > 64
        || !runner
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'.'))
    {
        bail!("invalid CI runner {runner:?}");
    }
    Ok(())
}

fn validate_token(field: &str, value: &str) -> Result<()> {
    if value.is_empty()
        || value.len() > 96
        || !value.bytes().all(|byte| {
            byte.is_ascii_lowercase() || byte.is_ascii_digit() || matches!(byte, b'-' | b'_')
        })
    {
        bail!("{field} has invalid token {value:?}");
    }
    Ok(())
}

fn validate_nonempty_tokens(owner: &str, field: &str, values: &[String]) -> Result<()> {
    if values.is_empty() {
        bail!("CI task {owner} has empty {field}");
    }
    reject_duplicates(owner, field, values)?;
    for value in values {
        validate_token(&format!("CI task {owner} {field}"), value)?;
    }
    Ok(())
}

fn validate_nonempty_patterns(owner: &str, values: &[String]) -> Result<()> {
    if values.is_empty() {
        bail!("CI task {owner} has no impact patterns");
    }
    reject_duplicates(owner, "impact patterns", values)?;
    for value in values {
        if value.is_empty()
            || value.starts_with('/')
            || value.contains('\\')
            || value.split('/').any(|part| part == "..")
        {
            bail!("CI task {owner} has invalid impact pattern {value:?}");
        }
    }
    Ok(())
}

fn validate_output_path(owner: &str, value: &str) -> Result<()> {
    let path = Path::new(value);
    if value.is_empty()
        || path.is_absolute()
        || !value.starts_with("target/ci/")
        || path.components().any(|part| {
            matches!(
                part,
                Component::ParentDir | Component::RootDir | Component::Prefix(_)
            )
        })
    {
        bail!("CI task {owner} has unsafe output path {value:?}");
    }
    Ok(())
}

fn reject_duplicates(owner: &str, field: &str, values: &[String]) -> Result<()> {
    let mut unique = BTreeSet::new();
    for value in values {
        if !unique.insert(value) {
            bail!("{owner} has duplicate {field} value {value}");
        }
    }
    Ok(())
}

pub(crate) fn sha256_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn task(id: &str, dependency: &[&str]) -> CiTask {
        CiTask {
            id: id.to_string(),
            tier: "feedback".to_string(),
            workflow_job: "quality".to_string(),
            runner: "ubuntu-24.04".to_string(),
            always: false,
            depends_on: dependency.iter().map(|item| (*item).to_string()).collect(),
            owners: vec!["engineering".to_string()],
            impact: vec!["cmd/".to_string()],
            platforms: vec!["linux".to_string()],
            capabilities: vec!["rust".to_string()],
            timeout_minutes: 10,
            budget_minutes: 5,
            evidence_kind: "contract".to_string(),
            results: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    #[test]
    fn dependency_cycles_are_rejected() {
        let manifest = CiManifest {
            version: 1,
            profiles: vec![CiProfile {
                name: "pull-request".to_string(),
                tier: "feedback".to_string(),
                changed_only: true,
                tasks: vec!["one".to_string()],
            }],
            tasks: vec![task("one", &["two"]), task("two", &["one"])],
        };
        assert!(validate_manifest(&manifest).is_err());
    }

    #[test]
    fn evidence_outputs_stay_under_ci_target() {
        assert!(validate_output_path("task", "target/ci/results/out.json").is_ok());
        for invalid in ["target/out.json", "target/ci/../secret", "/target/ci/out"] {
            assert!(validate_output_path("task", invalid).is_err(), "{invalid}");
        }
    }

    #[test]
    fn source_identity_accepts_sha1_and_sha256_object_ids() {
        for length in [40, 64] {
            assert!(validate_source_identity(&SourceIdentity {
                commit: "a".repeat(length),
                tree: "0".repeat(length),
                tracked_dirty: false,
            })
            .is_ok());
        }
        assert!(validate_source_identity(&SourceIdentity {
            commit: "A".repeat(40),
            tree: "0".repeat(39),
            tracked_dirty: false,
        })
        .is_err());
    }
}
