//! Bounded display-only diagnostics. Certification never consumes summaries.
use super::evidence::write_json;
use super::model::{sha256_hex, task_digest, CiTask, SourceIdentity};
use super::plan::{read_plan, CiPlan};
use anyhow::{bail, Result};
use serde::{Deserialize, Serialize};
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::Path;

const MAX_SUMMARY_BYTES: u64 = 32 * 1024;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct TaskSummary {
    pub schema: String,
    pub source: SourceIdentity,
    pub plan_sha256: String,
    pub task_definition_sha256: String,
    pub task_id: String,
    pub run_id: String,
    pub run_attempt: String,
    pub attempt: String,
    pub started_at_unix_millis: u64,
    pub updated_at_unix_millis: u64,
    pub duration_millis: u64,
    pub state: SummaryState,
    pub command: Option<String>,
    pub failure_kind: Option<String>,
    pub error: Option<String>,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(super) enum SummaryState {
    Running,
    Passed,
    Failed,
}

pub(super) fn store(root: &Path, summary: &TaskSummary) -> Result<()> {
    write_json(
        &root.join(format!("target/ci/summaries/{}.json", summary.task_id)),
        summary,
    )
}

fn validate(
    summary: &TaskSummary,
    plan: &CiPlan,
    plan_sha: &str,
    task: &CiTask,
    run_id: &str,
    run_attempt: &str,
) -> Result<()> {
    let prefix = format!("target/ci/executions/{}/", task.id);
    let nonce = summary.attempt.strip_prefix(&prefix).unwrap_or_default();
    if summary.schema != "volang.ci.task-summary.v1"
        || summary.source != plan.source
        || summary.plan_sha256 != plan_sha
        || summary.task_definition_sha256 != task_digest(task)?
        || summary.task_id != task.id
        || summary.run_id != run_id
        || summary.run_attempt != run_attempt
        || summary.updated_at_unix_millis < summary.started_at_unix_millis
        || nonce.is_empty()
        || !nonce
            .bytes()
            .all(|byte| byte.is_ascii_digit() || byte == b'-')
        || summary
            .command
            .as_ref()
            .is_some_and(|id| !task.commands.contains(id))
        || summary
            .error
            .as_ref()
            .is_some_and(|error| error.chars().count() > 4096)
        || summary.failure_kind.as_deref().is_some_and(|kind| {
            !matches!(
                kind,
                "product" | "infrastructure" | "portability" | "dependency-policy"
            )
        })
        || (summary.state == SummaryState::Passed
            && (summary.error.is_some() || summary.failure_kind.is_some()))
        || (summary.state == SummaryState::Failed
            && (summary.error.is_none() || summary.failure_kind.is_none()))
    {
        bail!("summary identity or status is invalid");
    }
    Ok(())
}

fn read_summary(path: &Path) -> Result<TaskSummary> {
    let metadata = fs::symlink_metadata(path)?;
    if !metadata.is_file()
        || metadata.file_type().is_symlink()
        || metadata.len() > MAX_SUMMARY_BYTES
    {
        bail!("summary must be a bounded regular file");
    }
    let mut bytes = Vec::new();
    File::open(path)?
        .take(MAX_SUMMARY_BYTES + 1)
        .read_to_end(&mut bytes)?;
    if bytes.len() as u64 > MAX_SUMMARY_BYTES {
        bail!("summary exceeded its size limit");
    }
    Ok(serde_json::from_slice(&bytes)?)
}

// Encode markup, control characters and table separators before rendering.
fn display(value: &str) -> String {
    value
        .chars()
        .take(4096)
        .map(|character| match character {
            '&' => "&amp;".into(),
            '<' => "&lt;".into(),
            '>' => "&gt;".into(),
            '|' => "&#124;".into(),
            '`' => "&#96;".into(),
            '*' => "&#42;".into(),
            '_' => "&#95;".into(),
            '[' => "&#91;".into(),
            ']' => "&#93;".into(),
            '\\' => "&#92;".into(),
            '#' => "&#35;".into(),
            character if character.is_control() => " ".into(),
            character => character.to_string(),
        })
        .collect()
}

pub(super) fn render(root: &Path, plan_path: &Path, directory: &Path) -> Result<()> {
    let (plan, bytes) = read_plan(root, plan_path)?;
    let run_id = std::env::var("GITHUB_RUN_ID").unwrap_or_default();
    let run_attempt = std::env::var("GITHUB_RUN_ATTEMPT").unwrap_or_default();
    let markdown = render_plan(&plan, &sha256_hex(&bytes), directory, &run_id, &run_attempt)?;
    print!("{markdown}");
    if let Ok(path) = std::env::var("GITHUB_STEP_SUMMARY") {
        File::options()
            .append(true)
            .create(true)
            .open(path)?
            .write_all(markdown.as_bytes())?;
    }
    Ok(())
}

fn render_plan(
    plan: &CiPlan,
    plan_sha: &str,
    directory: &Path,
    run_id: &str,
    run_attempt: &str,
) -> Result<String> {
    let mut rows = Vec::new();
    let mut failures = Vec::new();
    for task in &plan.tasks {
        let summary =
            read_summary(&directory.join(format!("{}.json", task.id))).and_then(|summary| {
                validate(&summary, plan, plan_sha, task, run_id, run_attempt)?;
                Ok(summary)
            });
        match summary {
            Ok(summary) => {
                let status = match summary.state {
                    SummaryState::Running => "incomplete",
                    SummaryState::Passed => "passed",
                    SummaryState::Failed => "failed",
                };
                rows.push(format!(
                    "| {} | {} | {:.1}s | {} | {} |",
                    display(&task.id),
                    status,
                    summary.duration_millis as f64 / 1000.0,
                    display(&task.owners.join(", ")),
                    display(summary.command.as_deref().unwrap_or("—"))
                ));
                if summary.state == SummaryState::Failed {
                    failures.push((summary.updated_at_unix_millis, task, summary));
                }
            }
            Err(_) => rows.push(format!(
                "| {} | missing or invalid summary | — | {} | — |",
                display(&task.id),
                display(&task.owners.join(", "))
            )),
        }
    }
    failures.sort_by(|left, right| (left.0, &left.1.id).cmp(&(right.0, &right.1.id)));
    let mut output = String::from("\n### Task diagnostics\n\nDisplay only; job status and certification determine the check result.\n\n");
    if let Some((_, task, summary)) = failures.first() {
        output.push_str(&format!("First recorded failure: **{}** · {} · owner: {}\n\nCommand: {}\n\n{}\n\nDiagnostics: {}\n\nDownload the immutable plan artifact for commit {} to target/ci/plan.json, then run: `vo-dev ci run --plan target/ci/plan.json --task {}`\n\n",
            display(&task.id), display(summary.failure_kind.as_deref().unwrap_or("infrastructure")),
            display(&task.owners.join(", ")), display(summary.command.as_deref().unwrap_or("task setup or certification")),
            display(summary.error.as_deref().unwrap_or("task failed")), display(&summary.attempt), display(&plan.source.commit), task.id));
    }
    output.push_str("| Task | State | Duration | Owner | Last command |\n|---|---|---:|---|---|\n");
    output.push_str(&rows.join("\n"));
    output.push('\n');
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::super::plan::SelectionBasis;
    use super::*;
    use std::collections::BTreeMap;

    fn fixture() -> (CiPlan, TaskSummary) {
        let source = SourceIdentity {
            commit: "a".repeat(40),
            tree: "b".repeat(40),
            tracked_dirty: false,
        };
        let task = CiTask {
            id: "fixture".into(),
            tier: "feedback".into(),
            workflow_job: "fixture".into(),
            runner: "ubuntu-24.04".into(),
            always: false,
            depends_on: vec![],
            owners: vec!["engineering".into()],
            impact: vec![],
            platforms: vec!["linux".into()],
            capabilities: vec!["contracts".into()],
            timeout_minutes: 1,
            budget_minutes: 1,
            evidence_kind: "contract".into(),
            results: vec![],
            artifacts: vec![],
            commands: vec!["fixture-command".into()],
            inputs: vec![],
            resource_group: "fixture".into(),
        };
        let summary = TaskSummary {
            schema: "volang.ci.task-summary.v1".into(),
            source: source.clone(),
            plan_sha256: "c".repeat(64),
            task_definition_sha256: task_digest(&task).unwrap(),
            task_id: task.id.clone(),
            run_id: "42".into(),
            run_attempt: "1".into(),
            attempt: "target/ci/executions/fixture/1000-42".into(),
            started_at_unix_millis: 1000,
            updated_at_unix_millis: 1100,
            duration_millis: 100,
            state: SummaryState::Failed,
            command: Some("fixture-command".into()),
            failure_kind: Some("product".into()),
            error: Some("assertion failed".into()),
        };
        let plan = CiPlan {
            schema: "volang.ci.plan.v2".into(),
            profile: "test".into(),
            tier: "feedback".into(),
            manifest_sha256: "d".repeat(64),
            source,
            selection: SelectionBasis::Complete,
            changed_files: vec![],
            decisions: BTreeMap::new(),
            tasks: vec![task],
            workflow_jobs: BTreeMap::new(),
        };
        (plan, summary)
    }

    #[test]
    fn rejects_stale_identity_and_false_success() {
        let (plan, summary) = fixture();
        let check = |summary: &TaskSummary| {
            validate(summary, &plan, &"c".repeat(64), &plan.tasks[0], "42", "1")
        };
        check(&summary).unwrap();
        for variant in 0..10 {
            let mut invalid = summary.clone();
            match variant {
                0 => invalid.source.commit = "e".repeat(40),
                1 => invalid.run_id = "41".into(),
                2 => invalid.run_attempt = "2".into(),
                3 => invalid.plan_sha256 = "e".repeat(64),
                4 => invalid.task_id = "other".into(),
                5 => invalid.state = SummaryState::Passed,
                6 => invalid.attempt.push_str("/../../outside"),
                7 => invalid.command = Some("undeclared".into()),
                8 => invalid.error = Some("x".repeat(4097)),
                _ => invalid.failure_kind = Some("unknown".into()),
            }
            assert!(check(&invalid).is_err(), "variant {variant}");
        }
    }

    #[test]
    fn markup_is_inert_and_bounded() {
        let rendered = display("<img src=x onerror=bad>|[link](https://example.com)\n```\r*#");
        for forbidden in ['<', '>', '|', '[', ']', '\n', '\r', '`', '*', '#'] {
            if forbidden == '#' {
                continue;
            } // Numeric entities contain their own #.
            assert!(!rendered.contains(forbidden));
        }
        assert_eq!(display(&"x".repeat(10000)).len(), 4096);
    }

    #[test]
    fn preserves_failure_then_replaces_stale_success_with_incomplete_start() {
        let (plan, mut summary) = fixture();
        let directory = std::env::temp_dir().join(format!(
            "vo-summary-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let result = (|| -> Result<()> {
            store(&directory, &summary)?;
            let input = directory.join("target/ci/summaries");
            let rendered = render_plan(&plan, &"c".repeat(64), &input, "42", "1")?;
            assert!(rendered.contains("First recorded failure: **fixture**"));
            assert!(rendered.contains("engineering"));
            assert!(rendered.contains("--task fixture"));
            summary.state = SummaryState::Passed;
            summary.failure_kind = None;
            summary.error = None;
            store(&directory, &summary)?;
            summary.state = SummaryState::Running;
            summary.attempt = "target/ci/executions/fixture/2000-42".into();
            store(&directory, &summary)?;
            let rendered = render_plan(&plan, &"c".repeat(64), &input, "42", "1")?;
            assert!(rendered.contains("| incomplete |"));
            assert!(!rendered.contains("| passed |"));
            fs::write(
                input.join("fixture.json"),
                "x".repeat(MAX_SUMMARY_BYTES as usize + 1),
            )?;
            let rendered = render_plan(&plan, &"c".repeat(64), &input, "42", "1")?;
            assert!(rendered.contains("missing or invalid summary"));
            fs::remove_file(input.join("fixture.json"))?;
            assert!(render_plan(&plan, &"c".repeat(64), &input, "42", "1")?
                .contains("missing or invalid summary"));
            Ok(())
        })();
        fs::remove_dir_all(directory).unwrap();
        result.unwrap();
    }
}
