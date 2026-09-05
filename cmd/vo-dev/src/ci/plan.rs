use super::model::{
    load_manifest, manifest_digest, task_map, validate_source_identity, CiManifest, CiTask,
    SourceIdentity,
};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Component, Path};
use std::process::Command;

pub(crate) const PLAN_SCHEMA: &str = "volang.ci.plan.v1";

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CiPlan {
    pub(crate) schema: String,
    pub(crate) profile: String,
    pub(crate) tier: String,
    pub(crate) manifest_sha256: String,
    pub(crate) source: SourceIdentity,
    pub(crate) changed_files: Vec<String>,
    pub(crate) decisions: BTreeMap<String, Vec<String>>,
    pub(crate) tasks: Vec<CiTask>,
    pub(crate) workflow_jobs: BTreeMap<String, Vec<String>>,
}

pub(crate) fn build_plan(
    root: &Path,
    profile_name: &str,
    base: Option<&str>,
    head: Option<&str>,
    explicit_changed_files: &[String],
) -> Result<CiPlan> {
    let manifest = load_manifest(root)?;
    let profile = manifest
        .profiles
        .iter()
        .find(|profile| profile.name == profile_name)
        .ok_or_else(|| anyhow!("unknown CI profile {profile_name}"))?;

    let mut changed_files = if explicit_changed_files.is_empty() {
        match (base, head) {
            (Some(base), Some(head)) => git_changed_files(root, base, head)?,
            (None, None) if !profile.changed_only => Vec::new(),
            (None, None) => bail!(
                "CI profile {profile_name} selects by impact and requires --base/--head or --changed-file"
            ),
            _ => bail!("--base and --head must be supplied together"),
        }
    } else {
        if base.is_some() || head.is_some() {
            bail!("--changed-file cannot be combined with --base or --head");
        }
        explicit_changed_files.to_vec()
    };
    for path in &mut changed_files {
        *path = normalize_repo_path(path)?;
    }
    changed_files.sort();
    changed_files.dedup();

    let selected = selected_task_ids(&manifest, profile_name, &changed_files)?;
    let tasks = manifest
        .tasks
        .iter()
        .filter(|task| selected.contains(task.id.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    if tasks.is_empty() {
        bail!("CI profile {profile_name} selected no tasks");
    }
    let mut workflow_jobs = BTreeMap::<String, Vec<String>>::new();
    for task in &tasks {
        workflow_jobs
            .entry(task.workflow_job.clone())
            .or_default()
            .push(task.id.clone());
    }

    Ok(CiPlan {
        schema: PLAN_SCHEMA.to_string(),
        profile: profile.name.clone(),
        tier: profile.tier.clone(),
        manifest_sha256: manifest_digest(root)?,
        source: source_identity(root)?,
        decisions: selection_decisions(&manifest, profile_name, &changed_files)?,
        changed_files,
        tasks,
        workflow_jobs,
    })
}

pub(crate) fn write_plan(path: &Path, plan: &CiPlan) -> Result<()> {
    let bytes = canonical_plan_bytes(plan)?;
    write_atomic(path, &bytes)
}

pub(crate) fn read_plan(root: &Path, path: &Path) -> Result<(CiPlan, Vec<u8>)> {
    let bytes = fs::read(path).with_context(|| format!("could not read {}", path.display()))?;
    if bytes.len() > 8 * 1024 * 1024 {
        bail!("CI plan exceeds 8 MiB: {}", path.display());
    }
    let plan: CiPlan = serde_json::from_slice(&bytes)
        .with_context(|| format!("could not parse {}", path.display()))?;
    validate_plan(root, &plan)?;
    if bytes != canonical_plan_bytes(&plan)? {
        bail!(
            "CI plan is not in canonical serialized form: {}",
            path.display()
        );
    }
    Ok((plan, bytes))
}

pub(crate) fn validate_plan(root: &Path, plan: &CiPlan) -> Result<()> {
    if plan.schema != PLAN_SCHEMA {
        bail!("unsupported CI plan schema {}", plan.schema);
    }
    validate_source_identity(&plan.source)?;
    let manifest = load_manifest(root)?;
    let current_digest = manifest_digest(root)?;
    if plan.manifest_sha256 != current_digest {
        bail!("CI plan manifest digest does not match eng/ci.toml");
    }
    let profile = manifest
        .profiles
        .iter()
        .find(|profile| profile.name == plan.profile)
        .ok_or_else(|| anyhow!("CI plan references unknown profile {}", plan.profile))?;
    if plan.tier != profile.tier {
        bail!("CI plan tier does not match profile {}", plan.profile);
    }
    let mut canonical_changed_files = plan
        .changed_files
        .iter()
        .map(|path| normalize_repo_path(path))
        .collect::<Result<Vec<_>>>()?;
    canonical_changed_files.sort();
    canonical_changed_files.dedup();
    if canonical_changed_files != plan.changed_files {
        bail!("CI plan changed files must be normalized, sorted, and unique");
    }
    if !profile.changed_only && !plan.changed_files.is_empty() {
        bail!("complete CI profiles cannot carry impact-only changed paths");
    }
    let expected = selected_task_ids(&manifest, &plan.profile, &plan.changed_files)?;
    if plan.decisions != selection_decisions(&manifest, &plan.profile, &plan.changed_files)? {
        bail!("CI plan explanations differ from task selection rules");
    }
    let actual_ids = plan
        .tasks
        .iter()
        .map(|task| task.id.as_str())
        .collect::<Vec<_>>();
    let actual = actual_ids.iter().copied().collect::<BTreeSet<_>>();
    let expected_ids = manifest
        .tasks
        .iter()
        .filter(|task| expected.contains(task.id.as_str()))
        .map(|task| task.id.as_str())
        .collect::<Vec<_>>();
    if actual.len() != plan.tasks.len() || actual != expected || actual_ids != expected_ids {
        bail!("CI plan task selection does not match profile and impact rules");
    }
    let canonical_tasks = task_map(&manifest);
    for task in &plan.tasks {
        let canonical = canonical_tasks
            .get(task.id.as_str())
            .ok_or_else(|| anyhow!("CI plan references unknown task {}", task.id))?;
        if serde_json::to_vec(task)? != serde_json::to_vec(canonical)? {
            bail!("CI plan task {} differs from eng/ci.toml", task.id);
        }
    }
    let mut expected_jobs = BTreeMap::<String, Vec<String>>::new();
    for task in &plan.tasks {
        expected_jobs
            .entry(task.workflow_job.clone())
            .or_default()
            .push(task.id.clone());
    }
    if plan.workflow_jobs != expected_jobs {
        bail!("CI plan workflow job index is inconsistent");
    }
    Ok(())
}

pub(crate) fn canonical_plan_bytes(plan: &CiPlan) -> Result<Vec<u8>> {
    let mut bytes = serde_json::to_vec_pretty(plan)?;
    bytes.push(b'\n');
    Ok(bytes)
}

fn selection_decisions(
    manifest: &CiManifest,
    profile_name: &str,
    paths: &[String],
) -> Result<BTreeMap<String, Vec<String>>> {
    let profile = manifest
        .profiles
        .iter()
        .find(|p| p.name == profile_name)
        .ok_or_else(|| anyhow!("unknown CI profile {profile_name}"))?;
    let selected = selected_task_ids(manifest, profile_name, paths)?;
    let mut decisions = BTreeMap::new();
    for task in &manifest.tasks {
        let mut reasons = Vec::new();
        if !profile.tasks.contains(&task.id) {
            reasons.push(format!("skip: task is outside profile {profile_name}"));
        } else if !profile.changed_only {
            reasons.push(format!("run: complete profile {profile_name}"));
        } else if task.always {
            reasons.push("run: mandatory repository contracts".into());
        } else {
            for path in paths {
                if !documentation_only_path(path) {
                    reasons.push(format!(
                        "run: conservative coverage for executable or unknown input {path}"
                    ));
                } else {
                    for pattern in &task.impact {
                        if glob_matches(pattern, path) {
                            reasons.push(format!("run: {path} matches {pattern}"));
                        }
                    }
                }
            }
            if reasons.is_empty() {
                if selected.contains(task.id.as_str()) {
                    reasons.extend(
                        manifest
                            .tasks
                            .iter()
                            .filter(|dependent| {
                                selected.contains(dependent.id.as_str())
                                    && dependent.depends_on.contains(&task.id)
                            })
                            .map(|dependent| {
                                format!("run: required dependency of {}", dependent.id)
                            }),
                    );
                } else {
                    reasons.push(
                        "skip: no changed input or selected dependent requires this task".into(),
                    );
                }
            }
        }
        decisions.insert(task.id.clone(), reasons);
    }
    Ok(decisions)
}

fn selected_task_ids<'a>(
    manifest: &'a CiManifest,
    profile_name: &str,
    changed_files: &[String],
) -> Result<BTreeSet<&'a str>> {
    let profile = manifest
        .profiles
        .iter()
        .find(|profile| profile.name == profile_name)
        .ok_or_else(|| anyhow!("unknown CI profile {profile_name}"))?;
    let tasks = task_map(manifest);
    let eligible = profile
        .tasks
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let mut selected = BTreeSet::new();
    for id in &profile.tasks {
        let task = tasks[id.as_str()];
        if !profile.changed_only
            || task.always
            || changed_files
                .iter()
                .any(|path| !documentation_only_path(path))
            || changed_files.iter().any(|path| {
                task.impact
                    .iter()
                    .any(|pattern| glob_matches(pattern, path))
            })
        {
            selected.insert(id.as_str());
        }
    }

    let mut pending = selected.iter().copied().collect::<Vec<_>>();
    while let Some(id) = pending.pop() {
        for dependency in &tasks[id].depends_on {
            if !eligible.contains(dependency.as_str()) {
                bail!(
                    "CI profile {} omits dependency {} required by {}",
                    profile.name,
                    dependency,
                    id
                );
            }
            if selected.insert(dependency.as_str()) {
                pending.push(dependency);
            }
        }
    }
    Ok(selected)
}

// Until the component graph is verified, only inert prose may narrow coverage.
// Generated Studio documentation is executable product input.
fn documentation_only_path(path: &str) -> bool {
    (path.starts_with("docs/") && path.ends_with(".md"))
        || matches!(
            path,
            "README.md" | "CHANGELOG.md" | "CONTRIBUTING.md" | "GOVERNANCE.md" | "SECURITY.md"
        )
}

pub(super) fn glob_matches(pattern: &str, path: &str) -> bool {
    let pattern = pattern.as_bytes();
    let path = path.as_bytes();
    let mut current = vec![false; path.len() + 1];
    current[0] = true;
    for &token in pattern {
        let mut next = vec![false; path.len() + 1];
        if token == b'*' {
            let mut reachable = false;
            for index in 0..=path.len() {
                reachable |= current[index];
                next[index] = reachable;
            }
        } else {
            for index in 0..path.len() {
                if current[index] && token == path[index] {
                    next[index + 1] = true;
                }
            }
        }
        current = next;
    }
    current[path.len()]
}

fn git_changed_files(root: &Path, base: &str, head: &str) -> Result<Vec<String>> {
    validate_git_revision(base)?;
    validate_git_revision(head)?;
    let output = Command::new("git")
        .args([
            "diff",
            "--name-only",
            "--no-renames",
            "-z",
            base,
            head,
            "--",
        ])
        .current_dir(root)
        .output()
        .context("could not inspect changed files")?;
    if !output.status.success() {
        bail!(
            "git diff failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    String::from_utf8(output.stdout)
        .context("git diff returned non-UTF-8 paths")?
        .split('\0')
        .filter(|path| !path.is_empty())
        .map(normalize_repo_path)
        .collect()
}

fn validate_git_revision(value: &str) -> Result<()> {
    if value.is_empty()
        || value.starts_with('-')
        || value.len() > 256
        || value
            .bytes()
            .any(|byte| byte.is_ascii_control() || byte.is_ascii_whitespace())
    {
        bail!("invalid git revision {value:?}");
    }
    Ok(())
}

fn normalize_repo_path(value: &str) -> Result<String> {
    let normalized = value.strip_prefix("./").unwrap_or(value).replace('\\', "/");
    let path = Path::new(&normalized);
    if normalized.is_empty()
        || path.is_absolute()
        || path.components().any(|part| {
            matches!(
                part,
                Component::ParentDir | Component::RootDir | Component::Prefix(_)
            )
        })
    {
        bail!("invalid repository path {value:?}");
    }
    Ok(normalized)
}

pub(crate) fn source_identity(root: &Path) -> Result<SourceIdentity> {
    let commit = git_stdout(root, &["rev-parse", "--verify", "HEAD^{commit}"])?;
    let tree = git_stdout(root, &["rev-parse", "--verify", "HEAD^{tree}"])?;
    let status = git_stdout(root, &["status", "--porcelain=v1", "--untracked-files=all"])?;
    let source = SourceIdentity {
        commit,
        tree,
        tracked_dirty: !status.is_empty(),
    };
    validate_source_identity(&source)?;
    Ok(source)
}

fn git_stdout(root: &Path, args: &[&str]) -> Result<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(root)
        .output()
        .with_context(|| format!("could not run git {}", args.join(" ")))?;
    if !output.status.success() {
        bail!(
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(String::from_utf8(output.stdout)
        .context("git returned non-UTF-8 output")?
        .trim()
        .to_string())
}

fn write_atomic(path: &Path, bytes: &[u8]) -> Result<()> {
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

    #[test]
    fn shared_and_unknown_inputs_select_every_eligible_task() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let manifest = load_manifest(&root).unwrap();
        let profile = manifest
            .profiles
            .iter()
            .find(|p| p.name == "pull-request")
            .unwrap();
        for path in [
            "rust-toolchain.toml",
            "eng/tests.toml",
            "lang/crates/vo-runtime/src/gc.rs",
            "unknown/input",
            "apps/studio/documentation/catalog.vo",
        ] {
            assert_eq!(
                selected_task_ids(&manifest, "pull-request", &[path.into()])
                    .unwrap()
                    .len(),
                profile.tasks.len(),
                "{path}"
            );
        }
        assert_eq!(
            selected_task_ids(&manifest, "pull-request", &["README.md".into()]).unwrap(),
            BTreeSet::from(["contracts"])
        );
    }

    #[test]
    fn deleted_and_renamed_paths_are_preserved() {
        let root = std::env::temp_dir().join(format!(
            "vo-ci-diff-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&root).unwrap();
        let git = |args: &[&str]| {
            assert!(Command::new("git")
                .args(args)
                .current_dir(&root)
                .output()
                .unwrap()
                .status
                .success())
        };
        git(&["init", "-q"]);
        fs::write(root.join("deleted.rs"), "deleted").unwrap();
        fs::write(root.join("old name.rs"), "renamed").unwrap();
        git(&["add", "."]);
        git(&[
            "-c",
            "user.name=CI test",
            "-c",
            "user.email=ci@example.invalid",
            "commit",
            "-qm",
            "base",
        ]);
        fs::remove_file(root.join("deleted.rs")).unwrap();
        fs::rename(root.join("old name.rs"), root.join("new name.rs")).unwrap();
        git(&["add", "-A"]);
        git(&[
            "-c",
            "user.name=CI test",
            "-c",
            "user.email=ci@example.invalid",
            "commit",
            "-qm",
            "head",
        ]);
        let paths = git_changed_files(&root, "HEAD~1", "HEAD").unwrap();
        assert_eq!(paths, vec!["deleted.rs", "new name.rs", "old name.rs"]);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn glob_matching_is_anchored_and_supports_wildcards() {
        assert!(glob_matches("ui/*", "ui/crates/core/src/lib.rs"));
        assert!(glob_matches("Cargo.*", "Cargo.lock"));
        assert!(glob_matches("*", "docs/ci.md"));
        assert!(!glob_matches("ui/*", "apps/studio/main.vo"));
        assert!(!glob_matches("Cargo.*", "nested/Cargo.toml"));
    }

    #[test]
    fn repo_paths_reject_escape_and_absolute_forms() {
        assert_eq!(normalize_repo_path("./ui/main.vo").unwrap(), "ui/main.vo");
        assert_eq!(normalize_repo_path("ui\\main.vo").unwrap(), "ui/main.vo");
        for invalid in ["../secret", "./../secret", "/etc/passwd", ""] {
            assert!(normalize_repo_path(invalid).is_err(), "{invalid}");
        }
    }
}
