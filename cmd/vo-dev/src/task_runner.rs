use crate::config::{load_project, load_tasks, load_toolchains, Task, TaskOutputPolicy};
use crate::first_party::{ci_checkout_untracked_prefixes, project_repo_path};
use crate::task_graph::{
    resolve_selector, task_map, task_repos_recursive_from_map, task_tools_recursive,
};
use crate::tool_lint::lint_toolchain_file;
use crate::tool_system::check_tools;
use anyhow::{anyhow, bail, Context, Result};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fs;
use std::io;
#[cfg(unix)]
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

#[cfg(unix)]
const SIGKILL: i32 = 9;

#[cfg(unix)]
unsafe extern "C" {
    fn kill(pid: i32, sig: i32) -> i32;
    fn setpgid(pid: i32, pgid: i32) -> i32;
}

pub(crate) fn run_tasks(root: &Path, task_names: &[String]) -> Result<()> {
    ensure_volang_ci_alias(root)?;
    let config = load_tasks(root)?;
    let task_map = task_map(&config)?;
    let gate_plan_env = gate_plan_env(&config)?;
    let ci_run_id = ci_run_id()?;
    for name in task_names {
        let task = task_map
            .get(name)
            .ok_or_else(|| anyhow!("unknown task: {name}"))?;
        ensure_task_tools(root, name)?;
        prepare_task_outputs(root, task)?;
        let cwd = root.join(task.cwd.as_deref().unwrap_or("."));
        println!("\n==> {}", task.title);
        println!(
            "{}$ {}",
            cwd.strip_prefix(root).unwrap_or(&cwd).display(),
            task.command.join(" ")
        );
        let start = Instant::now();
        let mut command = command_for_task(&task.command)?;
        command.current_dir(&cwd);
        for (key, value) in &task.env {
            command.env(key, value);
        }
        for (key, value) in &gate_plan_env {
            command.env(key, value);
        }
        command.env("VO_DEV_CI_RUN_ID", &ci_run_id);
        for (key, value) in task_root_env(root, &task_map, name)? {
            command.env(key, value);
        }
        let status = run_command_with_optional_timeout(&mut command, task.timeout_sec)
            .with_context(|| format!("could not run task {name}"))?;
        let elapsed = start.elapsed().as_secs_f32();
        if !status.success() {
            bail!("task {name} failed after {elapsed:.1}s");
        }
        validate_task_outputs(root, task)
            .with_context(|| format!("task {name} did not produce declared outputs"))?;
        println!("ok: {name} ({elapsed:.1}s)");
    }
    Ok(())
}

fn command_for_task(task_command: &[String]) -> Result<Command> {
    let (program, args) = task_command
        .split_first()
        .ok_or_else(|| anyhow!("task command cannot be empty"))?;
    let mut command = if program == "vo-dev" {
        let executable =
            std::env::current_exe().context("could not resolve current vo-dev executable")?;
        Command::new(executable)
    } else {
        Command::new(program)
    };
    command.args(args);
    Ok(command)
}

fn ensure_volang_ci_alias(root: &Path) -> Result<()> {
    let ci_modules = root.join("ci_modules");
    let alias = ci_modules.join("volang");
    match fs::symlink_metadata(&alias) {
        Ok(_) => {
            let actual = alias
                .canonicalize()
                .with_context(|| format!("could not resolve {}", alias.display()))?;
            let expected = root
                .canonicalize()
                .with_context(|| format!("could not resolve {}", root.display()))?;
            if actual != expected {
                bail!(
                    "{} must resolve to the volang root {}; found {}",
                    alias.display(),
                    expected.display(),
                    actual.display()
                );
            }
            Ok(())
        }
        Err(error) if error.kind() == io::ErrorKind::NotFound => {
            fs::create_dir_all(&ci_modules).with_context(|| {
                format!(
                    "could not create CI module directory {}",
                    ci_modules.display()
                )
            })?;
            create_volang_ci_alias(&alias).with_context(|| {
                format!(
                    "could not create {} as an alias for the volang root",
                    alias.display()
                )
            })
        }
        Err(error) => Err(error)
            .with_context(|| format!("could not inspect CI module alias {}", alias.display())),
    }
}

#[cfg(unix)]
fn create_volang_ci_alias(alias: &Path) -> io::Result<()> {
    std::os::unix::fs::symlink("..", alias)
}

#[cfg(windows)]
fn create_volang_ci_alias(alias: &Path) -> io::Result<()> {
    std::os::windows::fs::symlink_dir("..", alias)
}

fn ci_run_id() -> Result<String> {
    if let Ok(existing) = std::env::var("VO_DEV_CI_RUN_ID") {
        if !existing.trim().is_empty() && existing.trim() == existing {
            return Ok(existing);
        }
        bail!("VO_DEV_CI_RUN_ID must be non-empty and cannot contain surrounding whitespace");
    }
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .context("system clock is before UNIX epoch")?
        .as_nanos();
    Ok(format!("vo-dev-{nanos}-{}", std::process::id()))
}

fn prepare_task_outputs(root: &Path, task: &Task) -> Result<()> {
    for output in &task.outputs {
        if output != "target" && !output.starts_with("target/") {
            continue;
        }
        // Checked-in outputs are never pre-cleared. For target outputs, a
        // transactional producer owns same-parent staging, rollback, and
        // restart recovery,
        // so the runner must preserve its last verified generation.
        if task.output_policy == TaskOutputPolicy::Transactional {
            continue;
        }
        let path = root.join(output);
        if path.is_dir() {
            fs::remove_dir_all(&path)
                .with_context(|| format!("could not clear stale task output {output}"))?;
        } else if path.exists() {
            fs::remove_file(&path)
                .with_context(|| format!("could not clear stale task output {output}"))?;
        }
    }
    Ok(())
}

fn task_root_env(
    root: &Path,
    task_map: &std::collections::BTreeMap<String, Task>,
    task_name: &str,
) -> Result<Vec<(String, String)>> {
    let required_repos = task_repos_recursive_from_map(task_map, task_name)?;
    let project = load_project(root)?;
    let mut env = vec![(
        "VOLANG_ROOT".to_string(),
        absolute_path(root)?.display().to_string(),
    )];
    for repo in project
        .first_party
        .iter()
        .chain(project.external_project.iter())
    {
        let env_name = repo_root_env_name(&repo.name);
        let path = match project_repo_path(root, &repo.name) {
            Ok(path) => path,
            Err(error) if required_repos.contains(&repo.name) => {
                bail!(
                    "task {task_name} requires project repo {}; provision a CI checkout or local_hint before running it: {error:#}",
                    repo.name
                );
            }
            Err(_) => fallback_repo_path(root, &repo.name, repo.local_hint.as_deref()),
        };
        env.push((env_name, path.display().to_string()));
        if let Some(expected_commit) = &repo.expected_commit {
            env.push((
                repo_expected_commit_env_name(&repo.name),
                expected_commit.clone(),
            ));
        }
    }
    Ok(env)
}

fn repo_root_env_name(repo: &str) -> String {
    let mut out = String::new();
    for ch in repo.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_uppercase());
        } else {
            out.push('_');
        }
    }
    out.push_str("_ROOT");
    out
}

fn repo_expected_commit_env_name(repo: &str) -> String {
    let mut out = String::new();
    for ch in repo.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_uppercase());
        } else {
            out.push('_');
        }
    }
    out.push_str("_EXPECTED_COMMIT");
    out
}

fn fallback_repo_path(root: &Path, repo: &str, local_hint: Option<&str>) -> PathBuf {
    local_hint
        .map(|hint| root.join(hint))
        .unwrap_or_else(|| root.join("ci_modules").join(repo))
}

fn absolute_path(path: &Path) -> Result<PathBuf> {
    path.canonicalize()
        .with_context(|| format!("could not canonicalize {}", path.display()))
}

fn gate_plan_env(config: &crate::config::TaskFile) -> Result<Vec<(String, String)>> {
    final_gate_selectors(config)?
        .into_iter()
        .map(|selector| {
            let tasks = resolve_selector(config, &selector)?;
            Ok((
                task_plan_env_name(&selector),
                serde_json::to_string(&tasks)
                    .with_context(|| format!("could not encode {selector} task plan"))?,
            ))
        })
        .collect()
}

pub(crate) fn final_gate_selectors(config: &crate::config::TaskFile) -> Result<Vec<String>> {
    if config.final_selectors.is_empty() {
        bail!("eng/tasks.toml final_selectors cannot be empty");
    }
    let mut seen = BTreeSet::new();
    for selector in &config.final_selectors {
        if selector.trim().is_empty() || selector.trim() != selector {
            bail!("eng/tasks.toml final_selectors contains an empty or padded selector");
        }
        if !seen.insert(selector) {
            bail!("eng/tasks.toml final_selectors contains duplicate selector {selector}");
        }
        resolve_selector(config, selector)
            .with_context(|| format!("eng/tasks.toml final_selectors references {selector}"))?;
    }
    Ok(config.final_selectors.clone())
}

fn task_plan_env_name(selector: &str) -> String {
    format!(
        "VO_DEV_EXPECTED_TASKS_{}",
        selector.to_ascii_uppercase().replace('-', "_")
    )
}

#[derive(Debug, Serialize)]
struct TaskRunEvidence<'a> {
    schema: &'static str,
    selector: &'a str,
    changed: bool,
    passed: bool,
    source_state: String,
    tasks: &'a [String],
}

pub(crate) fn write_task_run_evidence(
    root: &Path,
    selector: &str,
    changed: bool,
    task_names: &[String],
) -> Result<()> {
    let config = load_tasks(root)?;
    let final_selectors = final_gate_selectors(&config)?;
    if changed || !final_selectors.iter().any(|item| item == selector) {
        return Ok(());
    }
    let evidence = TaskRunEvidence {
        schema: "vo-dev-task-run-evidence-v1",
        selector,
        changed,
        passed: true,
        source_state: format!(
            "vm-production-current-source:{}",
            current_vm_production_source_state_hash(root)?
        ),
        tasks: task_names,
    };
    let dir = root.join("lang/docs/dev/vm-production-gate-evidence");
    fs::create_dir_all(&dir)?;
    fs::write(
        dir.join(format!("{selector}.json")),
        serde_json::to_string_pretty(&evidence)?,
    )?;
    Ok(())
}

pub(crate) fn current_vm_production_source_state_hash(root: &Path) -> Result<String> {
    ensure_no_untracked_vm_production_source(root)?;
    let mut hasher = Sha256::new();
    hasher.update(b"vm-production-readiness-source-state-v1\0");
    let mut files = git_z(root, &["ls-files", "--cached", "-z"])?;
    files.retain(|path| {
        path != "lang/docs/dev/vm-production-readiness.md"
            && !path.starts_with("lang/docs/dev/vm-production-gate-evidence/")
    });
    files.sort();
    for rel_path in files {
        update_hash_field(&mut hasher, b"path", rel_path.as_bytes());
        match fs::read(root.join(&rel_path)) {
            Ok(content) => update_hash_field(&mut hasher, b"content", &content),
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                update_hash_field(&mut hasher, b"deleted", b"")
            }
            Err(error) => return Err(error).with_context(|| format!("could not read {rel_path}")),
        }
    }
    Ok(hex_encode(&hasher.finalize()))
}

fn ensure_no_untracked_vm_production_source(root: &Path) -> Result<()> {
    let mut files = git_z(
        root,
        &["status", "--porcelain=v1", "-z", "--untracked-files=all"],
    )?;
    files.retain(|entry| {
        let bytes = entry.as_bytes();
        bytes.len() >= 4
            && bytes[2] == b' '
            && ((bytes[0] == b'?' && bytes[1] == b'?') || (bytes[1] == b'A' && bytes[0] != b'A'))
    });
    let mut files: Vec<String> = files
        .into_iter()
        .map(|entry| entry[3..].to_string())
        .collect();
    files.retain(|path| {
        path != "lang/docs/dev/vm-production-readiness.md"
            && !path.starts_with("lang/docs/dev/vm-production-gate-evidence/")
    });
    let ci_checkout_prefixes = ci_checkout_untracked_prefixes(root)?;
    files.retain(|path| {
        !ci_checkout_prefixes
            .iter()
            .any(|prefix| path.starts_with(prefix))
    });
    files.sort();
    if files.is_empty() {
        return Ok(());
    }
    bail!(
        "vm-production final evidence requires tracked source files; untracked source paths: {}",
        files.join(", ")
    )
}

fn update_hash_field(hasher: &mut Sha256, label: &[u8], value: &[u8]) {
    hasher.update(label);
    hasher.update(b"\0");
    hasher.update(value);
    hasher.update(b"\0");
}

fn git_z(root: &Path, args: &[&str]) -> Result<Vec<String>> {
    let output = Command::new("git").args(args).current_dir(root).output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stderr = stderr.trim();
        bail!(
            "git {} failed: {}",
            args.join(" "),
            if stderr.is_empty() {
                "no stderr"
            } else {
                stderr
            }
        );
    }
    Ok(output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|entry| !entry.is_empty())
        .map(|entry| String::from_utf8_lossy(entry).into_owned())
        .collect())
}

fn hex_encode(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        out.push(HEX[(byte >> 4) as usize] as char);
        out.push(HEX[(byte & 0x0f) as usize] as char);
    }
    out
}

#[cfg(test)]
#[allow(clippy::items_after_test_module)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn task_command_reuses_the_current_vo_dev_executable() {
        let command =
            command_for_task(&["vo-dev".to_string(), "lint".to_string(), "all".to_string()])
                .expect("command");
        assert_eq!(
            Path::new(command.get_program()),
            std::env::current_exe().expect("current executable")
        );
        assert_eq!(
            command.get_args().collect::<Vec<_>>(),
            ["lint", "all"].map(std::ffi::OsStr::new)
        );
    }

    #[test]
    fn task_command_preserves_external_programs() {
        let command =
            command_for_task(&["npm".to_string(), "run".to_string(), "build".to_string()])
                .expect("command");
        assert_eq!(command.get_program(), "npm");
        assert_eq!(
            command.get_args().collect::<Vec<_>>(),
            ["run", "build"].map(std::ffi::OsStr::new)
        );
    }

    #[cfg(unix)]
    #[test]
    fn volang_ci_alias_is_created_and_rejects_wrong_targets() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-ci-alias-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(&root).expect("root");

        ensure_volang_ci_alias(&root).expect("create self alias");
        assert_eq!(
            root.join("ci_modules/volang")
                .canonicalize()
                .expect("alias target"),
            root.canonicalize().expect("root target")
        );

        fs::remove_file(root.join("ci_modules/volang")).expect("remove alias");
        fs::create_dir_all(root.join("wrong")).expect("wrong target");
        std::os::unix::fs::symlink("../wrong", root.join("ci_modules/volang"))
            .expect("wrong alias");
        let error = ensure_volang_ci_alias(&root).expect_err("wrong alias must fail");
        assert!(error
            .to_string()
            .contains("must resolve to the volang root"));

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn prepare_task_outputs_removes_only_ephemeral_target_outputs() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-stale-output-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("target/report")).expect("target report dir");
        fs::create_dir_all(root.join("checked-in/artifact")).expect("checked-in artifact dir");
        fs::write(root.join("target/report/report.json"), "stale").expect("stale report");
        fs::write(root.join("checked-in/artifact/data.json"), "source").expect("source artifact");
        let task: crate::config::Task = toml::from_str(
            r#"
name = "evidence"
title = "Evidence"
command = ["true"]
outputs = ["target/report", "checked-in/artifact"]
tier = "contract"
"#,
        )
        .expect("task");

        prepare_task_outputs(&root, &task).expect("prepare outputs");

        assert!(!root.join("target/report").exists());
        assert!(root.join("checked-in/artifact/data.json").exists());
        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn transactional_task_outputs_preserve_last_known_good_until_producer_commit() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-transactional-output-{stamp}-{}",
            std::process::id()
        ));
        let output = root.join("target/current-wasm");
        fs::create_dir_all(&output).expect("old output");
        fs::write(output.join("module.wasm"), b"last-known-good-wasm").expect("old wasm");
        fs::write(
            output.join("producer-manifest.json"),
            b"{\"generation\":\"last-known-good\"}\n",
        )
        .expect("old manifest");
        fs::write(output.join("old-only.txt"), b"old").expect("old-only marker");
        let task: crate::config::Task = toml::from_str(
            r#"
name = "transactional-producer"
title = "Transactional producer"
command = ["false"]
outputs = ["target/current-wasm"]
output_policy = "transactional"
tier = "contract"
"#,
        )
        .expect("task");

        prepare_task_outputs(&root, &task).expect("prepare transactional output");

        // A preflight or build failure occurs before the producer commits its
        // sibling staging directory. Both payload and manifest remain intact.
        assert_eq!(
            fs::read(output.join("module.wasm")).expect("preserved wasm"),
            b"last-known-good-wasm"
        );
        assert_eq!(
            fs::read(output.join("producer-manifest.json")).expect("preserved manifest"),
            b"{\"generation\":\"last-known-good\"}\n"
        );

        // Simulate the producer's successful same-parent directory commit and
        // prove consumers cannot observe a payload/manifest generation mix.
        let staging = root.join("target/.current-wasm.staging-test");
        let backup = root.join("target/.current-wasm.backup-test");
        fs::create_dir_all(&staging).expect("staging");
        fs::write(staging.join("module.wasm"), b"new-wasm").expect("new wasm");
        fs::write(
            staging.join("producer-manifest.json"),
            b"{\"generation\":\"new\"}\n",
        )
        .expect("new manifest");
        fs::write(staging.join("new-only.txt"), b"new").expect("new-only marker");
        fs::rename(&output, &backup).expect("backup previous output");
        fs::rename(&staging, &output).expect("commit staging output");
        assert_eq!(fs::read(output.join("module.wasm")).unwrap(), b"new-wasm");
        assert_eq!(
            fs::read(output.join("producer-manifest.json")).unwrap(),
            b"{\"generation\":\"new\"}\n"
        );
        assert!(!output.join("old-only.txt").exists());
        assert_eq!(fs::read(output.join("new-only.txt")).unwrap(), b"new");

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn gate_plan_env_includes_resolved_task_dependencies_035() {
        let config: crate::config::TaskFile = toml::from_str(
            r#"
version = 1
final_selectors = ["contract", "vm-production", "site", "release-verify"]

[groups]
contract = ["main"]
vm-production = ["main", "extra"]
site = ["site-task"]
release-verify = ["release-task"]

[[task]]
name = "setup"
title = "Setup"
command = ["true"]
tier = "contract"

[[task]]
name = "main"
title = "Main"
command = ["true"]
tier = "contract"
needs = ["setup"]

[[task]]
name = "extra"
title = "Extra"
command = ["true"]
tier = "contract"

[[task]]
name = "site-task"
title = "Site"
command = ["true"]
tier = "site"

[[task]]
name = "release-task"
title = "Release"
command = ["true"]
tier = "release"
"#,
        )
        .expect("task config");

        let env: BTreeMap<_, _> = gate_plan_env(&config)
            .expect("gate plan env")
            .into_iter()
            .collect();

        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_CONTRACT")
                .map(String::as_str),
            Some(r#"["setup","main"]"#),
        );
        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_VM_PRODUCTION")
                .map(String::as_str),
            Some(r#"["setup","main","extra"]"#),
        );
        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_SITE").map(String::as_str),
            Some(r#"["site-task"]"#),
        );
        assert_eq!(
            env.get("VO_DEV_EXPECTED_TASKS_RELEASE_VERIFY")
                .map(String::as_str),
            Some(r#"["release-task"]"#),
        );
    }

    #[test]
    fn vm_production_source_state_rejects_untracked_source_039() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-source-state-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("lang/docs/dev")).expect("docs dir");
        fs::write(root.join("src.vo"), "package main\nfunc main() {}\n").expect("src");
        fs::write(
            root.join("lang/docs/dev/vm-production-readiness.md"),
            "# VM Production Readiness Plan\n",
        )
        .expect("readiness");
        run_git(&root, &["init", "-q"]);
        run_git(&root, &["add", "."]);

        fs::create_dir_all(root.join(".github/workflows")).expect("workflow dir");
        fs::write(
            root.join(".github/workflows/production-readiness.yml"),
            "name: Production Readiness\n",
        )
        .expect("workflow");

        let err = current_vm_production_source_state_hash(&root)
            .expect_err("untracked source must not be accepted as final evidence state");
        let msg = err.to_string();
        assert!(msg.contains("requires tracked source files"), "{msg}");
        assert!(
            msg.contains(".github/workflows/production-readiness.yml"),
            "{msg}"
        );

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn vm_production_source_state_ignores_declared_ci_checkout_039() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-source-state-ci-checkout-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("eng")).expect("eng dir");
        fs::create_dir_all(root.join("lang/docs/dev")).expect("docs dir");
        fs::write(root.join("src.vo"), "package main\nfunc main() {}\n").expect("src");
        fs::write(
            root.join("lang/docs/dev/vm-production-readiness.md"),
            "# VM Production Readiness Plan\n",
        )
        .expect("readiness");
        fs::write(
            root.join("eng/project.toml"),
            r#"version = 1

[repo]
name = "volang"
module = "github.com/vo-lang/volang"

[[first_party]]
name = "vogui"
repository = "vo-lang/vogui"
ci_checkout = true
"#,
        )
        .expect("project config");
        run_git(&root, &["init", "-q"]);
        run_git(&root, &["add", "."]);

        fs::create_dir_all(root.join("ci_modules/vogui")).expect("ci checkout dir");
        fs::write(
            root.join("ci_modules/vogui/vo.mod"),
            "module = \"github.com/vo-lang/vogui\"\nvo = \"^0.1.0\"\n",
        )
        .expect("ci checkout source");

        current_vm_production_source_state_hash(&root)
            .expect("declared CI checkout should not dirty final evidence state");

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn vm_production_source_state_rejects_intent_to_add_source_041() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-source-state-intent-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("lang/docs/dev")).expect("docs dir");
        fs::write(root.join("src.vo"), "package main\nfunc main() {}\n").expect("src");
        fs::write(
            root.join("lang/docs/dev/vm-production-readiness.md"),
            "# VM Production Readiness Plan\n",
        )
        .expect("readiness");
        run_git(&root, &["init", "-q"]);
        run_git(&root, &["add", "."]);

        fs::create_dir_all(root.join("src")).expect("src dir");
        fs::write(
            root.join("src/intent.vo"),
            "package main\nfunc intent() {}\n",
        )
        .expect("intent source");
        run_git(&root, &["add", "-N", "src/intent.vo"]);

        let err = current_vm_production_source_state_hash(&root)
            .expect_err("intent-to-add source must not be accepted as final evidence state");
        let msg = err.to_string();
        assert!(msg.contains("requires tracked source files"), "{msg}");
        assert!(msg.contains("src/intent.vo"), "{msg}");

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn final_signoff_evidence_writes_all_required_selectors_047() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-vo-dev-final-evidence-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("eng")).expect("eng dir");
        fs::create_dir_all(root.join("lang/docs/dev")).expect("docs dir");
        fs::write(
            root.join("eng/tasks.toml"),
            r#"version = 1
final_selectors = ["contract", "vm-production", "site", "release-verify"]

[groups]
contract = ["contract-task"]
vm-production = ["vm-production-task"]
site = ["site-task"]
release-verify = ["release-verify-task"]

[[task]]
name = "contract-task"
title = "contract"
command = ["true"]
inputs = ["Cargo.toml"]
tier = "contract"

[[task]]
name = "vm-production-task"
title = "vm-production"
command = ["true"]
inputs = ["Cargo.toml"]
tier = "contract"

[[task]]
name = "site-task"
title = "site"
command = ["true"]
inputs = ["Cargo.toml"]
tier = "site"

[[task]]
name = "release-verify-task"
title = "release-verify"
command = ["true"]
inputs = ["Cargo.toml"]
tier = "release"
"#,
        )
        .expect("tasks config");
        fs::write(root.join("src.vo"), "package main\nfunc main() {}\n").expect("src");
        fs::write(
            root.join("lang/docs/dev/vm-production-readiness.md"),
            "# VM Production Readiness Plan\n",
        )
        .expect("readiness");
        run_git(&root, &["init", "-q"]);
        run_git(&root, &["add", "."]);

        for selector in ["contract", "vm-production", "site", "release-verify"] {
            write_task_run_evidence(&root, selector, false, &[format!("{selector}-task")])
                .unwrap_or_else(|error| panic!("{selector} evidence failed: {error}"));
            let evidence = root
                .join("lang/docs/dev/vm-production-gate-evidence")
                .join(format!("{selector}.json"));
            assert!(
                evidence.exists(),
                "{selector} final signoff evidence was not written"
            );
            let source = fs::read_to_string(&evidence).expect("evidence JSON");
            assert!(
                source.contains(&format!(r#""selector": "{selector}""#)),
                "{selector} evidence recorded the wrong selector: {source}"
            );
        }

        fs::remove_dir_all(root).ok();
    }

    fn run_git(root: &Path, args: &[&str]) {
        let status = Command::new("git")
            .args(args)
            .current_dir(root)
            .status()
            .expect("git command");
        assert!(status.success(), "git {args:?} failed");
    }
}

fn validate_task_outputs(root: &Path, task: &Task) -> Result<()> {
    for output in &task.outputs {
        let path = root.join(output);
        if !path.exists() {
            bail!("missing output {}", output);
        }
        if path.is_dir() && fs::read_dir(&path)?.next().is_none() {
            bail!("output directory {} is empty", output);
        }
    }
    Ok(())
}

fn run_command_with_optional_timeout(
    command: &mut Command,
    timeout_sec: Option<u64>,
) -> Result<ExitStatus> {
    #[cfg(unix)]
    unsafe {
        command.pre_exec(|| {
            if setpgid(0, 0) == 0 {
                Ok(())
            } else {
                Err(io::Error::last_os_error())
            }
        });
    }

    let mut child = command.spawn().context("could not start process")?;
    let Some(timeout_sec) = timeout_sec else {
        return child.wait().context("could not wait for process");
    };

    let timeout = Duration::from_secs(timeout_sec);
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait().context("could not poll process")? {
            return Ok(status);
        }
        if start.elapsed() >= timeout {
            kill_process_tree(&mut child);
            bail!("timed out after {timeout_sec}s");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn kill_process_tree(child: &mut Child) {
    #[cfg(unix)]
    unsafe {
        let process_group = -(child.id() as i32);
        if kill(process_group, SIGKILL) == 0 {
            let _ = child.wait();
            return;
        }
    }

    let _ = child.kill();
    let _ = child.wait();
}

fn ensure_task_tools(root: &Path, task_name: &str) -> Result<()> {
    let tools = load_toolchains(root)?;
    lint_toolchain_file(root, &tools)?;
    let statuses = check_tools(&tools, task_tools_recursive(root, task_name)?)?;
    let missing: Vec<_> = statuses
        .into_iter()
        .filter(|status| !status.ok)
        .map(|status| format!("{} ({})", status.name, status.message))
        .collect();
    if !missing.is_empty() {
        bail!("missing task tools for {task_name}: {}", missing.join(", "));
    }
    Ok(())
}
