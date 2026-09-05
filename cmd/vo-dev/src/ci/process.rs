//! One bounded command and its process tree. Cross-runner scheduling stays in Actions.
use super::model::CiCommand;
use anyhow::{anyhow, bail, Context, Result};
use process_wrap::std::*;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

const MAX_LOG_BYTES: u64 = 128 * 1024 * 1024;

struct ProcessTree {
    child: Box<dyn ChildWrapper>,
    reaped: bool,
}

impl Drop for ProcessTree {
    fn drop(&mut self) {
        if !self.reaped {
            let _ = self.child.start_kill();
            let _ = self.child.wait();
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum CommandStatus {
    Passed,
    Failed,
    TimedOut,
    Cancelled,
    SpawnFailed,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct TestCounts {
    pub(crate) passed: u64,
    pub(crate) failed: u64,
    pub(crate) ignored: u64,
    pub(crate) binaries: u64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CommandResult {
    pub(crate) id: String,
    pub(crate) argv: Vec<String>,
    pub(crate) cwd: String,
    pub(crate) status: CommandStatus,
    pub(crate) exit_code: Option<i32>,
    pub(crate) duration_millis: u64,
    pub(crate) failure_kind: Option<String>,
    pub(crate) error: Option<String>,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
    pub(crate) test_counts: Option<TestCounts>,
}

impl CommandResult {
    pub(crate) fn passed(&self) -> bool {
        matches!(self.status, CommandStatus::Passed)
            && self.exit_code == Some(0)
            && self.failure_kind.is_none()
            && self.error.is_none()
    }
}

pub(crate) fn run_command(
    root: &Path,
    spec: &CiCommand,
    attempt: &Path,
    common_env: &BTreeMap<String, String>,
    cancelled: &AtomicBool,
    remaining: Duration,
) -> Result<CommandResult> {
    let stdout = attempt.join(format!("{}.stdout.log", spec.id));
    let stderr = attempt.join(format!("{}.stderr.log", spec.id));
    let cwd = root
        .join(&spec.cwd)
        .canonicalize()
        .with_context(|| format!("CI command {} working directory is unavailable", spec.id))?;
    if !cwd.starts_with(root.canonicalize()?) {
        bail!("CI command cwd resolves outside repository");
    }
    let mut command = Command::new(&spec.argv[0]);
    command
        .args(&spec.argv[1..])
        .current_dir(&cwd)
        .envs(common_env)
        .envs(&spec.env)
        .stdin(Stdio::null())
        .stdout(File::create(&stdout)?)
        .stderr(File::create(&stderr)?);
    for (key, relative) in &spec.repo_env {
        command.env(key, root.canonicalize()?.join(relative));
    }
    let mut command = CommandWrap::from(command);
    #[cfg(unix)]
    command.wrap(ProcessGroup::leader());
    #[cfg(windows)]
    command.wrap(JobObject);
    let start = Instant::now();
    let mut result = CommandResult {
        id: spec.id.clone(),
        argv: spec.argv.clone(),
        cwd: spec.cwd.clone(),
        status: CommandStatus::SpawnFailed,
        exit_code: None,
        duration_millis: 0,
        failure_kind: Some("infrastructure".into()),
        error: None,
        stdout: stdout
            .strip_prefix(root)?
            .to_string_lossy()
            .replace('\\', "/"),
        stderr: stderr
            .strip_prefix(root)?
            .to_string_lossy()
            .replace('\\', "/"),
        test_counts: None,
    };
    if cancelled.load(Ordering::Relaxed) || remaining.is_zero() {
        result.status = if remaining.is_zero() {
            CommandStatus::TimedOut
        } else {
            CommandStatus::Cancelled
        };
        result.error =
            Some("task was cancelled or its deadline expired before this command started".into());
        return Ok(result);
    }
    let mut tree = match command.spawn() {
        Ok(child) => ProcessTree {
            child,
            reaped: false,
        },
        Err(error) => {
            result.error = Some(error.to_string());
            return Ok(result);
        }
    };
    let timeout = Duration::from_secs(spec.timeout_seconds).min(remaining);
    loop {
        let over_limit = fs::metadata(&stdout)?.len() > MAX_LOG_BYTES
            || fs::metadata(&stderr)?.len() > MAX_LOG_BYTES;
        if cancelled.load(Ordering::Relaxed) || start.elapsed() >= timeout || over_limit {
            result.status = if cancelled.load(Ordering::Relaxed) {
                CommandStatus::Cancelled
            } else if over_limit {
                CommandStatus::Failed
            } else {
                CommandStatus::TimedOut
            };
            result.error = Some(if over_limit {
                "command exceeded the diagnostic output limit".into()
            } else {
                format!(
                    "command {:?} after {:.3}s",
                    result.status,
                    start.elapsed().as_secs_f64()
                )
            });
            // start_kill targets the POSIX group or Windows Job Object, including descendants.
            tree.child
                .start_kill()
                .context("could not terminate CI command process tree")?;
            let status = tree
                .child
                .wait()
                .context("could not reap CI command process tree")?;
            tree.reaped = true;
            result.exit_code = status.code();
            break;
        }
        if let Some(status) = tree.child.try_wait()? {
            // A successful parent may leave a server or worker behind. End the
            // command's group before releasing its resources; an empty group is OK.
            let _ = tree.child.start_kill();
            tree.reaped = true;
            result.exit_code = status.code();
            result.status = if status.success() {
                CommandStatus::Passed
            } else {
                CommandStatus::Failed
            };
            result.failure_kind = (!status.success()).then(|| spec.failure_kind.clone());
            result.error = (!status.success()).then(|| format!("command exited with {status}"));
            break;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    result.duration_millis = u64::try_from(start.elapsed().as_millis())?;
    if result.passed() && !spec.report.is_empty() {
        let counts = match spec.report.as_str() {
            "cargo-test" => cargo_test_counts(&String::from_utf8_lossy(&fs::read(&stdout)?)),
            "libfuzzer" => libfuzzer_counts(
                &String::from_utf8_lossy(&fs::read(&stderr)?),
                fuzz_run_budget(&spec.argv)?,
            ),
            _ => bail!("unknown command report contract"),
        };
        match counts {
            Ok(counts) => result.test_counts = Some(counts),
            Err(error) => {
                result.status = CommandStatus::Failed;
                result.failure_kind = Some("infrastructure".into());
                result.error = Some(error.to_string());
            }
        }
    }
    Ok(result)
}

pub(crate) fn fuzz_run_budget(argv: &[String]) -> Result<u64> {
    let budgets = argv
        .iter()
        .filter_map(|arg| arg.strip_prefix("-runs="))
        .collect::<Vec<_>>();
    match budgets.as_slice() {
        [value] => value
            .parse::<u64>()
            .ok()
            .filter(|count| *count > 0)
            .ok_or_else(|| anyhow!("fuzz command requires a positive bounded run count")),
        _ => bail!("fuzz command requires exactly one -runs budget"),
    }
}

fn libfuzzer_counts(log: &str, expected: u64) -> Result<TestCounts> {
    let completed = log
        .lines()
        .filter_map(|line| {
            line.strip_prefix("Done ")?
                .split_once(" runs in ")?
                .0
                .parse::<u64>()
                .ok()
        })
        .collect::<Vec<_>>();
    let done = log
        .lines()
        .filter_map(|line| {
            let mut fields = line.strip_prefix('#')?.split_whitespace();
            let count = fields.next()?.parse::<u64>().ok()?;
            (fields.next()? == "DONE").then_some(count)
        })
        .collect::<Vec<_>>();
    if expected == 0 || completed != [expected] || done != [expected] {
        bail!("fuzzer did not complete its declared {expected} inputs: done={done:?}, summaries={completed:?}");
    }
    Ok(TestCounts {
        passed: expected,
        failed: 0,
        ignored: 0,
        binaries: 1,
    })
}

pub(crate) fn cargo_test_counts(log: &str) -> Result<TestCounts> {
    let mut counts = TestCounts::default();
    for line in log.lines().filter(|line| line.starts_with("test result: ")) {
        let summary = line
            .strip_prefix("test result: ok. ")
            .ok_or_else(|| anyhow!("Rust test harness reported a non-successful suite"))?;
        let parts = summary.split(';').map(str::trim).collect::<Vec<_>>();
        let number = |index: usize, suffix: &str| -> Result<u64> {
            parts
                .get(index)
                .and_then(|value| value.strip_suffix(suffix))
                .and_then(|value| value.parse().ok())
                .ok_or_else(|| anyhow!("malformed Rust test harness counter: {line}"))
        };
        counts.passed = counts
            .passed
            .checked_add(number(0, " passed")?)
            .ok_or_else(|| anyhow!("test count overflow"))?;
        counts.failed = counts
            .failed
            .checked_add(number(1, " failed")?)
            .ok_or_else(|| anyhow!("test count overflow"))?;
        counts.ignored = counts
            .ignored
            .checked_add(number(2, " ignored")?)
            .ok_or_else(|| anyhow!("test count overflow"))?;
        counts.binaries += 1;
    }
    if counts.passed == 0 || counts.failed != 0 {
        bail!("Rust test command did not execute a nonempty successful test set");
    }
    Ok(counts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::path::PathBuf;
    use std::sync::atomic::AtomicU64;

    static NEXT_FIXTURE: AtomicU64 = AtomicU64::new(0);

    struct Fixture(PathBuf);
    impl Fixture {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!(
                "vo-ci-process-{}-{}",
                std::process::id(),
                NEXT_FIXTURE.fetch_add(1, Ordering::Relaxed)
            ));
            fs::create_dir(&path).unwrap();
            Self(path.canonicalize().unwrap())
        }
        fn spec(&self, mode: &str) -> CiCommand {
            CiCommand {
                id: mode.into(),
                argv: vec![
                    std::env::current_exe()
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                    "--exact".into(),
                    "ci::process::tests::child_process_fixture".into(),
                    "--nocapture".into(),
                ],
                cwd: String::new(),
                env: BTreeMap::from([
                    ("VO_CI_PROCESS_FIXTURE".into(), mode.into()),
                    (
                        "VO_CI_PROCESS_FIXTURE_DIR".into(),
                        self.0.to_string_lossy().into_owned(),
                    ),
                ]),
                timeout_seconds: 40,
                repo_env: BTreeMap::new(),
                failure_kind: "product".into(),
                report: String::new(),
                stdout_result: String::new(),
            }
        }
        fn run(&self, spec: &CiCommand, cancel: &AtomicBool, timeout: Duration) -> CommandResult {
            run_command(&self.0, spec, &self.0, &BTreeMap::new(), cancel, timeout).unwrap()
        }
    }
    impl Drop for Fixture {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    // The same test executable is a portable subprocess fixture. Normal test
    // collection runs no fixture behavior; no extra runtime/tool is required.
    #[test]
    fn child_process_fixture() {
        let Ok(mode) = std::env::var("VO_CI_PROCESS_FIXTURE") else {
            return;
        };
        let path = PathBuf::from(std::env::var("VO_CI_PROCESS_FIXTURE_DIR").unwrap());
        match mode.as_str() {
            "output" => {
                std::io::stdout()
                    .write_all(&vec![b'x'; 1024 * 1024])
                    .unwrap();
                std::io::stderr()
                    .write_all(&vec![b'y'; 1024 * 1024])
                    .unwrap();
            }
            "nonzero" => std::process::exit(7),
            "repo-env" => println!("{}", env_path_for_fixture()),
            "sleep" => std::thread::sleep(Duration::from_secs(60)),
            "descendant" => {
                let lock = File::options()
                    .read(true)
                    .write(true)
                    .create(true)
                    .truncate(false)
                    .open(path.join("held.lock"))
                    .unwrap();
                lock.lock().unwrap();
                fs::write(path.join("ready"), "ready").unwrap();
                std::thread::sleep(Duration::from_secs(60));
                drop(lock);
            }
            "tree" | "orphan" => {
                #[expect(
                    clippy::zombie_processes,
                    reason = "this fixture deliberately leaves a descendant for the executor to terminate"
                )]
                let mut child = Command::new(std::env::current_exe().unwrap())
                    .args([
                        "--exact",
                        "ci::process::tests::child_process_fixture",
                        "--nocapture",
                    ])
                    .env("VO_CI_PROCESS_FIXTURE", "descendant")
                    .spawn()
                    .unwrap();
                if mode == "orphan" {
                    let start = Instant::now();
                    while !path.join("ready").exists() {
                        assert!(
                            start.elapsed() < Duration::from_secs(30),
                            "descendant did not start"
                        );
                        std::thread::sleep(Duration::from_millis(10));
                    }
                    // Deliberately simulate a launcher returning before its server.
                } else {
                    child.wait().unwrap();
                }
            }
            _ => panic!("unknown process fixture"),
        }
    }

    fn env_path_for_fixture() -> String {
        std::env::var("VO_CI_FIXTURE_REPO_PATH").unwrap()
    }

    #[test]
    fn declared_repository_environment_uses_an_absolute_candidate_path() {
        let fixture = Fixture::new();
        let mut spec = fixture.spec("repo-env");
        spec.repo_env
            .insert("VO_CI_FIXTURE_REPO_PATH".into(), "vo.work".into());
        let result = fixture.run(&spec, &AtomicBool::new(false), Duration::from_secs(40));
        assert!(result.passed());
        let output = fs::read_to_string(fixture.0.join(result.stdout)).unwrap();
        assert!(output.contains(&fixture.0.join("vo.work").to_string_lossy().to_string()));
    }

    #[test]
    fn fuzz_report_requires_the_complete_unique_declared_budget() {
        let log = "#100000\tDONE cov: 308\nDone 100000 runs in 0 second(s)\n";
        assert_eq!(libfuzzer_counts(log, 100000).unwrap().passed, 100000);
        for invalid in [
            "",
            "#100000 DONE\n",
            "Done 100000 runs in 0 second(s)\n",
            "#2 DONE\nDone 2 runs in 0 second(s)\n",
        ] {
            assert!(libfuzzer_counts(invalid, 100000).is_err());
        }
        assert!(libfuzzer_counts(&format!("{log}{log}"), 100000).is_err());
        assert!(fuzz_run_budget(&["-runs=-1".into()]).is_err());
        assert!(fuzz_run_budget(&["-runs=1".into(), "-runs=2".into()]).is_err());
    }

    #[test]
    fn command_exit_spawn_failure_and_large_logs_are_preserved() {
        let fixture = Fixture::new();
        let cancelled = AtomicBool::new(false);
        let timeout = Duration::from_secs(40);
        let output = fixture.run(&fixture.spec("output"), &cancelled, timeout);
        assert!(output.passed(), "{output:?}");
        assert!(fs::metadata(fixture.0.join(&output.stdout)).unwrap().len() >= 1024 * 1024);
        assert!(fs::metadata(fixture.0.join(&output.stderr)).unwrap().len() >= 1024 * 1024);
        let nonzero = fixture.run(&fixture.spec("nonzero"), &cancelled, timeout);
        assert!(!nonzero.passed());
        assert_eq!(nonzero.exit_code, Some(7));
        assert_eq!(nonzero.failure_kind.as_deref(), Some("product"));
        let mut missing = fixture.spec("missing");
        missing.argv[0] = fixture
            .0
            .join("nonexistent-command")
            .to_string_lossy()
            .into_owned();
        let failed = fixture.run(&missing, &cancelled, timeout);
        assert!(matches!(failed.status, CommandStatus::SpawnFailed));
        assert_eq!(failed.exit_code, None);
    }

    #[test]
    fn task_deadline_stops_a_command_without_success_evidence() {
        let fixture = Fixture::new();
        let result = fixture.run(
            &fixture.spec("sleep"),
            &AtomicBool::new(false),
            Duration::from_millis(100),
        );
        assert!(
            matches!(result.status, CommandStatus::TimedOut),
            "{result:?}"
        );
        assert!(!result.passed());
    }

    #[test]
    fn cancellation_and_parent_exit_release_descendant_resources() {
        for mode in ["tree", "orphan"] {
            let fixture = Fixture::new();
            let cancelled = AtomicBool::new(false);
            let result = std::thread::scope(|scope| {
                if mode == "tree" {
                    scope.spawn(|| {
                        let start = Instant::now();
                        while !fixture.0.join("ready").exists()
                            && start.elapsed() < Duration::from_secs(30)
                        {
                            std::thread::sleep(Duration::from_millis(10));
                        }
                        cancelled.store(true, Ordering::Relaxed);
                    });
                }
                fixture.run(&fixture.spec(mode), &cancelled, Duration::from_secs(40))
            });
            assert!(
                fixture.0.join("ready").exists(),
                "descendant must actually start: {result:?}"
            );
            if mode == "tree" {
                assert!(matches!(result.status, CommandStatus::Cancelled));
            } else {
                assert!(result.passed(), "{result:?}");
            }
            let lock = File::options()
                .read(true)
                .write(true)
                .open(fixture.0.join("held.lock"))
                .unwrap();
            let start = Instant::now();
            while lock.try_lock().is_err() {
                assert!(
                    start.elapsed() < Duration::from_secs(5),
                    "descendant survived {mode}"
                );
                std::thread::sleep(Duration::from_millis(10));
            }
        }
    }

    #[test]
    fn zero_filtered_failed_and_malformed_rust_suites_cannot_pass() {
        for log in [
            "",
            "test result: ok. 0 passed; 0 failed; 0 ignored; 84 filtered out; finished in 0.00s",
            "test result: FAILED. 3 passed; 1 failed; 0 ignored; 0 filtered out; finished in 0.00s",
            "test result: ok. 1 passed; 1 failed; 0 ignored; 0 filtered out; finished in 0.00s",
            "test result: ok. 2 passed",
        ] {
            assert!(cargo_test_counts(log).is_err(), "{log}");
        }
        let counts = cargo_test_counts("test result: ok. 4 passed; 0 failed; 0 ignored; 0 filtered out; finished in 0.01s\n\ntest result: ok. 0 passed; 0 failed; 0 ignored; 0 filtered out; finished in 0.00s\n").unwrap();
        assert_eq!(counts.passed, 4);
        assert_eq!(counts.binaries, 2);
    }
}
