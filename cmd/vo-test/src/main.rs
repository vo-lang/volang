//! vo-test launcher - compiles and runs the Vo test runner (cmd/vo-test).

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::{self, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};

const DEFAULT_MAX_WORKERS: usize = 8;
const PROGRESS_INTERVAL: Duration = Duration::from_secs(30);
const RUNNER_OWNED_JIT_ENV_KEYS: &[&str] = &[
    "VO_JIT_CALL_THRESHOLD",
    "VO_JIT_LOOP_THRESHOLD",
    "VO_JIT_DEBUG",
];
const RUNNER_OWNED_GC_ENV_KEYS: &[&str] = &["VO_GC_STRESS", "VO_GC_VERIFY", "VO_GC_DEBUG"];

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1).map(String::as_str) {
        Some("run-plan") => {
            let plan_path = args.get(2).unwrap_or_else(|| {
                eprintln!(
                    "usage: vo-test run-plan <plan.json> [--jobs N] [--format text|json] [--verbose]"
                );
                process::exit(2);
            });
            match RunPlanArgs::parse(&args[3..]).and_then(|opts| run_plan(plan_path, &opts)) {
                Ok(code) => process::exit(code),
                Err(err) => {
                    eprintln!("vo-test run-plan failed: {err}");
                    process::exit(1);
                }
            }
        }
        Some("run-plan-job") => {
            let job_path = args.get(2).unwrap_or_else(|| {
                eprintln!("usage: vo-test run-plan-job <job.json>");
                process::exit(2);
            });
            match run_plan_job(job_path) {
                Ok(()) => process::exit(0),
                Err(err) => {
                    eprintln!("{err}");
                    process::exit(1);
                }
            }
        }
        _ => {
            eprintln!(
                "usage: vo-test run-plan <plan.json> [--jobs N] [--format text|json] [--verbose]"
            );
            eprintln!(
                "test selection is owned by vo-dev; use `cargo run -q -p vo-dev -- test run ...`"
            );
            process::exit(2);
        }
    }
}

#[derive(Debug)]
struct RunPlanArgs {
    jobs: usize,
    format: String,
    verbose: bool,
}

impl RunPlanArgs {
    fn parse(args: &[String]) -> Result<Self, Box<dyn std::error::Error>> {
        let mut jobs = default_jobs();
        let mut format = "text".to_string();
        let mut verbose = false;
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "-j" | "--jobs" => {
                    i += 1;
                    jobs = parse_jobs(args.get(i).ok_or("missing jobs value")?)?;
                }
                arg if arg.starts_with("-j") && arg.len() > 2 => {
                    jobs = parse_jobs(&arg[2..])?;
                }
                arg if arg.starts_with("--jobs=") => {
                    jobs = parse_jobs(&arg["--jobs=".len()..])?;
                }
                "--format" => {
                    i += 1;
                    format = args.get(i).ok_or("missing format value")?.clone();
                    if format != "text" && format != "json" {
                        return Err("--format must be text or json".into());
                    }
                }
                arg if arg.starts_with("--format=") => {
                    format = arg["--format=".len()..].to_string();
                    if format != "text" && format != "json" {
                        return Err("--format must be text or json".into());
                    }
                }
                "-v" | "--verbose" => {
                    verbose = true;
                }
                other => return Err(format!("unknown run-plan argument: {other}").into()),
            }
            i += 1;
        }
        Ok(Self {
            jobs,
            format,
            verbose,
        })
    }
}

fn default_jobs() -> usize {
    std::thread::available_parallelism()
        .map(usize::from)
        .unwrap_or(DEFAULT_MAX_WORKERS)
        .clamp(1, DEFAULT_MAX_WORKERS)
}

fn parse_jobs(raw: &str) -> Result<usize, Box<dyn std::error::Error>> {
    let jobs = raw.parse::<usize>()?;
    if jobs == 0 {
        return Err("--jobs must be > 0".into());
    }
    Ok(jobs)
}

#[derive(Debug, Deserialize)]
struct TestPlan {
    schema: String,
    suite: String,
    jobs: Vec<TestJob>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct TestJob {
    id: String,
    case_id: String,
    kind: String,
    path: String,
    target: String,
    backend: String,
    #[serde(default)]
    matrix: Option<String>,
    #[serde(default)]
    tags: Vec<String>,
    #[serde(default)]
    owner: Option<String>,
    #[serde(default)]
    env: BTreeMap<String, String>,
    timeout_sec: u64,
    expect: Expect,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Expect {
    kind: String,
    #[serde(default)]
    patterns: Vec<String>,
    pattern: Option<String>,
    #[serde(alias = "jit_regular_call_fallbacks_min")]
    jit_regular_call_side_exits_min: Option<u64>,
    jit_loop_entries_min: Option<u64>,
}

#[derive(Debug, Serialize)]
struct PlanResult {
    id: String,
    case_id: String,
    kind: String,
    path: String,
    target: String,
    backend: String,
    matrix: Option<String>,
    tags: Vec<String>,
    owner: Option<String>,
    passed: bool,
    elapsed_ms: u128,
    stdout: String,
    stderr: String,
    error: String,
    expect: Expect,
    baseline: Option<String>,
}

#[derive(Debug, Serialize)]
struct JsonRunOutput {
    schema: &'static str,
    suite: String,
    passed: usize,
    failed: usize,
    skipped: usize,
    jobs: Vec<JsonJobResult>,
}

#[derive(Debug, Serialize)]
struct JsonJobResult {
    id: String,
    case_id: String,
    kind: String,
    path: String,
    target: String,
    backend: String,
    matrix: Option<String>,
    tags: Vec<String>,
    owner: Option<String>,
    expect: Expect,
    status: String,
    elapsed_ms: u128,
    stdout: String,
    stderr: String,
    error: String,
    skip_reason: Option<String>,
    failure_reason: Option<String>,
    baseline: Option<String>,
    artifacts: Vec<String>,
}

fn run_plan(path: &str, opts: &RunPlanArgs) -> Result<i32, Box<dyn std::error::Error>> {
    let text = fs::read_to_string(path)?;
    let plan: TestPlan = serde_json::from_str(&text)?;
    if plan.schema != "volang.test-plan.v1" {
        return Err(format!("unsupported plan schema: {}", plan.schema).into());
    }
    if plan.jobs.is_empty() {
        return Err("test plan contains no jobs".into());
    }
    if opts.format == "text" {
        println!(
            "Running {} test plan ({} jobs, {} workers)...\n",
            plan.suite,
            plan.jobs.len(),
            opts.jobs
        );
    }

    let mut results = run_jobs_parallel(plan.jobs, opts.jobs, opts.format == "text")?;
    validate_differential_results(&mut results);
    let passed = results.iter().filter(|result| result.passed).count();
    let failed = results.len() - passed;

    if opts.format == "json" {
        let output = JsonRunOutput {
            schema: "volang.test-result.v1",
            suite: plan.suite,
            passed,
            failed,
            skipped: 0,
            jobs: results
                .iter()
                .map(|result| JsonJobResult {
                    id: result.id.clone(),
                    case_id: result.case_id.clone(),
                    kind: result.kind.clone(),
                    path: result.path.clone(),
                    target: result.target.clone(),
                    backend: result.backend.clone(),
                    matrix: result.matrix.clone(),
                    tags: result.tags.clone(),
                    owner: result.owner.clone(),
                    expect: result.expect.clone(),
                    status: if result.passed {
                        "passed".to_string()
                    } else {
                        "failed".to_string()
                    },
                    elapsed_ms: result.elapsed_ms,
                    stdout: result.stdout.clone(),
                    stderr: result.stderr.clone(),
                    error: result.error.clone(),
                    skip_reason: None,
                    failure_reason: if result.passed || result.error.trim().is_empty() {
                        None
                    } else {
                        Some(result.error.clone())
                    },
                    baseline: result.baseline.clone(),
                    artifacts: Vec::new(),
                })
                .collect(),
        };
        println!("{}", serde_json::to_string_pretty(&output)?);
    } else {
        for result in &results {
            if result.passed {
                println!(
                    "  PASS {} {} [{}] ({}ms)",
                    result.id, result.path, result.target, result.elapsed_ms
                );
                if opts.verbose && !result.stdout.trim().is_empty() {
                    println!("{}", result.stdout.trim_end());
                }
                if opts.verbose && !result.stderr.trim().is_empty() {
                    eprintln!("{}", result.stderr.trim_end());
                }
            } else {
                println!(
                    "  FAIL {} {} [{}] {} ({}ms)",
                    result.id, result.path, result.target, result.error, result.elapsed_ms
                );
                if !result.stdout.trim().is_empty() {
                    println!("{}", result.stdout.trim_end());
                }
                if !result.stderr.trim().is_empty() {
                    eprintln!("{}", result.stderr.trim());
                }
            }
        }
        if failed > 0 {
            println!("\nFailures:");
            for result in results.iter().filter(|result| !result.passed) {
                println!(
                    "  FAIL {} {} [{}] {}",
                    result.id, result.path, result.target, result.error
                );
            }
        }
        println!("\n{} passed, {} failed", passed, failed);
    }
    Ok(if failed == 0 { 0 } else { 1 })
}

fn validate_differential_results(results: &mut [PlanResult]) {
    let mut by_case: BTreeMap<String, BTreeMap<String, usize>> = BTreeMap::new();
    for (index, result) in results.iter().enumerate() {
        by_case
            .entry(result.case_id.clone())
            .or_default()
            .insert(result.target.clone(), index);
    }

    let mut baselines = Vec::new();
    let mut failures = Vec::new();
    for targets in by_case.values() {
        for (target, baseline_candidates) in [
            ("jit", &["vm"][..]),
            ("osr", &["vm"][..]),
            ("gc-jit", &["gc-vm", "vm"][..]),
        ] {
            let Some(&target_index) = targets.get(target) else {
                continue;
            };
            let Some((&baseline_name, &baseline_index)) = baseline_candidates
                .iter()
                .find_map(|name| targets.get(*name).map(|index| (name, index)))
            else {
                continue;
            };
            baselines.push((target_index, baseline_name.to_string()));

            if let Some(detail) =
                differential_mismatch(&results[baseline_index], &results[target_index])
            {
                failures.push((
                    target_index,
                    format!("differential mismatch against {baseline_name}: {detail}"),
                ));
            }
        }
    }

    for (index, baseline) in baselines {
        results[index].baseline = Some(baseline);
    }
    for (index, detail) in failures {
        let result = &mut results[index];
        result.passed = false;
        if result.error.trim().is_empty() {
            result.error = detail;
        } else {
            result.error = format!("{}; {}", result.error.trim(), detail);
        }
    }
}

fn differential_mismatch(baseline: &PlanResult, candidate: &PlanResult) -> Option<String> {
    let mut parts = Vec::new();
    if baseline.passed != candidate.passed {
        parts.push(format!(
            "exit status expected {}, got {}",
            pass_status(baseline.passed),
            pass_status(candidate.passed)
        ));
    }
    if baseline.stdout != candidate.stdout {
        parts.push(format!(
            "stdout expected {:?}, got {:?}",
            summarize_text(&baseline.stdout),
            summarize_text(&candidate.stdout)
        ));
    }
    let baseline_error = baseline.error.trim();
    let candidate_error = candidate.error.trim();
    if baseline_error != candidate_error {
        parts.push(format!(
            "panic/error expected {:?}, got {:?}",
            summarize_text(baseline_error),
            summarize_text(candidate_error)
        ));
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("; "))
    }
}

fn pass_status(passed: bool) -> &'static str {
    if passed {
        "pass"
    } else {
        "fail"
    }
}

fn summarize_text(text: &str) -> String {
    const MAX_CHARS: usize = 160;
    let normalized = text.replace('\n', "\\n");
    if normalized.chars().count() <= MAX_CHARS {
        normalized
    } else {
        let prefix: String = normalized.chars().take(MAX_CHARS).collect();
        format!("{prefix}...")
    }
}

fn run_jobs_parallel(
    jobs: Vec<TestJob>,
    workers: usize,
    show_progress: bool,
) -> Result<Vec<PlanResult>, Box<dyn std::error::Error>> {
    let jobs = Arc::new(jobs);
    let total = jobs.len();
    let next = Arc::new(AtomicUsize::new(0));
    let (tx, rx) = mpsc::channel();
    let worker_count = workers.min(total.max(1));

    for _ in 0..worker_count {
        let jobs = Arc::clone(&jobs);
        let next = Arc::clone(&next);
        let tx = tx.clone();
        std::thread::spawn(move || loop {
            let index = next.fetch_add(1, Ordering::SeqCst);
            let Some(job) = jobs.get(index) else {
                break;
            };
            let result = run_job_subprocess(job).unwrap_or_else(|err| PlanResult {
                id: job.id.clone(),
                case_id: job.case_id.clone(),
                kind: job.kind.clone(),
                path: job.path.clone(),
                target: job.target.clone(),
                backend: job.backend.clone(),
                matrix: job.matrix.clone(),
                tags: job.tags.clone(),
                owner: job.owner.clone(),
                passed: false,
                elapsed_ms: 0,
                stdout: String::new(),
                stderr: String::new(),
                error: err.to_string(),
                expect: job.expect.clone(),
                baseline: None,
            });
            if tx.send((index, result)).is_err() {
                break;
            }
        });
    }
    drop(tx);

    let mut out: Vec<Option<PlanResult>> = (0..total).map(|_| None).collect();
    let mut completed = 0usize;
    let mut last_progress = Instant::now();
    while completed < total {
        match rx.recv_timeout(Duration::from_millis(250)) {
            Ok((index, result)) => {
                out[index] = Some(result);
                completed += 1;
                if show_progress
                    && (completed == total || last_progress.elapsed() >= PROGRESS_INTERVAL)
                {
                    println!("  progress {completed}/{total} jobs complete");
                    last_progress = Instant::now();
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {
                if show_progress && last_progress.elapsed() >= PROGRESS_INTERVAL {
                    println!("  progress {completed}/{total} jobs complete");
                    last_progress = Instant::now();
                }
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => break,
        }
    }
    let results = out
        .into_iter()
        .enumerate()
        .map(|(index, result)| {
            result.unwrap_or_else(|| {
                let job = &jobs[index];
                PlanResult {
                    id: job.id.clone(),
                    case_id: job.case_id.clone(),
                    kind: job.kind.clone(),
                    path: job.path.clone(),
                    target: job.target.clone(),
                    backend: job.backend.clone(),
                    matrix: job.matrix.clone(),
                    tags: job.tags.clone(),
                    owner: job.owner.clone(),
                    passed: false,
                    elapsed_ms: 0,
                    stdout: String::new(),
                    stderr: String::new(),
                    error: "worker exited without reporting a result".to_string(),
                    expect: job.expect.clone(),
                    baseline: None,
                }
            })
        })
        .collect();
    Ok(results)
}

fn run_job_subprocess(job: &TestJob) -> Result<PlanResult, Box<dyn std::error::Error>> {
    let start = Instant::now();
    let path = env::temp_dir().join(format!(
        "volang-test-job-{}-{}.json",
        process::id(),
        stable_id(&job.id)
    ));
    fs::write(&path, serde_json::to_vec(job)?)?;
    let child = Command::new(env::current_exe()?)
        .arg("run-plan-job")
        .arg(&path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn();
    let mut child = match child {
        Ok(child) => child,
        Err(err) => {
            let _ = fs::remove_file(path);
            return Err(err.into());
        }
    };
    let timeout = Duration::from_secs(job.timeout_sec.max(1));
    let timed_out;
    loop {
        match child.try_wait() {
            Ok(Some(_)) => {
                timed_out = false;
                break;
            }
            Ok(None) => {}
            Err(err) => {
                let _ = fs::remove_file(path);
                return Err(err.into());
            }
        }
        if start.elapsed() > timeout {
            let _ = child.kill();
            timed_out = true;
            break;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    let output = child.wait_with_output();
    let _ = fs::remove_file(path);
    let output = output?;
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let error = if timed_out {
        format!("timed out after {}s", job.timeout_sec)
    } else if output.status.success() {
        String::new()
    } else {
        stderr.trim().to_string()
    };
    Ok(PlanResult {
        id: job.id.clone(),
        case_id: job.case_id.clone(),
        kind: job.kind.clone(),
        path: job.path.clone(),
        target: job.target.clone(),
        backend: job.backend.clone(),
        matrix: job.matrix.clone(),
        tags: job.tags.clone(),
        owner: job.owner.clone(),
        passed: output.status.success() && !timed_out,
        elapsed_ms: start.elapsed().as_millis(),
        stdout,
        stderr,
        error,
        expect: job.expect.clone(),
        baseline: None,
    })
}

fn stable_id(value: &str) -> String {
    value
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '-' })
        .collect()
}

fn run_plan_job(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let text = fs::read_to_string(path)?;
    let job: TestJob = serde_json::from_str(&text)?;
    let result = run_job(&job);
    // `main` terminates workers with `process::exit`, which skips stdio
    // destructor flushing. Each worker is piped by the plan coordinator, so
    // even short successful output must be flushed explicitly.
    std::io::stdout().flush()?;
    result.map_err(|err| err.into())
}

fn run_job(job: &TestJob) -> Result<(), String> {
    let saved_env = apply_job_env(job);
    let result = run_job_inner(job);
    restore_job_env(saved_env);
    result
}

fn apply_job_env(job: &TestJob) -> Vec<(String, Option<String>)> {
    let keys = job_env_restore_keys(job);
    let saved_env: Vec<_> = keys
        .into_iter()
        .map(|key| (key.clone(), env::var(key).ok()))
        .collect();
    for (key, _) in &saved_env {
        env::remove_var(key);
    }
    for (key, value) in &job.env {
        env::set_var(key, value);
    }
    saved_env
}

fn restore_job_env(saved_env: Vec<(String, Option<String>)>) {
    for (key, value) in saved_env {
        match value {
            Some(value) => env::set_var(key, value),
            None => env::remove_var(key),
        }
    }
}

fn job_env_restore_keys(job: &TestJob) -> Vec<String> {
    let mut keys: Vec<String> = job.env.keys().cloned().collect();
    if job.backend == "jit" {
        keys.extend(
            RUNNER_OWNED_JIT_ENV_KEYS
                .iter()
                .map(|key| (*key).to_string()),
        );
    }
    if matches!(job.backend.as_str(), "vm" | "jit") {
        keys.extend(
            RUNNER_OWNED_GC_ENV_KEYS
                .iter()
                .map(|key| (*key).to_string()),
        );
    }
    keys.sort();
    keys.dedup();
    keys
}

fn run_job_inner(job: &TestJob) -> Result<(), String> {
    let compiled = match vo_engine::compile(&job.path) {
        Ok(compiled) => compiled,
        Err(err) => {
            let msg = err.to_string();
            if job.expect.kind == "fail" && patterns_match(&msg, &job.expect) {
                return Ok(());
            }
            return Err(format!("compile failed: {msg}"));
        }
    };

    if job.expect.kind == "fail" {
        return Err("expected failure, but compile passed".to_string());
    }

    let mode = match job.backend.as_str() {
        "vm" => vo_engine::RunMode::Vm,
        "jit" => vo_engine::RunMode::Jit,
        "compile" => return Ok(()),
        "vo-embed" => return run_vo_embed(compiled),
        other => return Err(format!("unsupported backend in run-plan: {other}")),
    };
    let sink = vo_engine::CaptureSink::new();
    let result = vo_engine::run_with_output_observed(compiled, mode, Vec::new(), sink.clone());
    let output = sink.take();
    if !output.is_empty() {
        print!("{output}");
    }
    let observation = result.map_err(|err| err.to_string())?;
    if matches!(job.target.as_str(), "jit" | "gc-jit") && !observation.executed_jit_code() {
        return Err(
            "JIT backend completed without entering JIT-compiled function or loop code".to_string(),
        );
    }
    if mode == vo_engine::RunMode::Jit {
        if let Some(min) = job.expect.jit_loop_entries_min {
            if observation.jit_loop_entries < min {
                return Err(format!(
                    "expected at least {min} JIT loop entries, got {}",
                    observation.jit_loop_entries
                ));
            }
        }
        if let Some(min) = job.expect.jit_regular_call_side_exits_min {
            if observation.jit_regular_call_side_exits < min {
                return Err(format!(
                    "expected at least {min} JIT regular-call side exits, got {}",
                    observation.jit_regular_call_side_exits
                ));
            }
        }
    }
    Ok(())
}

fn run_vo_embed(compiled: vo_engine::CompileOutput) -> Result<(), String> {
    let path = env::temp_dir().join(format!("vo_nostd_{}_{}.vob", process::id(), nanos()));
    let bytes = compiled
        .module
        .serialize()
        .map_err(|err| format!("failed to serialize bytecode: {err}"))?;
    fs::write(&path, bytes).map_err(|err| err.to_string())?;
    let output = Command::new(vo_embed_bin()).arg(&path).output();
    let _ = fs::remove_file(path);
    let output = output.map_err(|err| err.to_string())?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !output.status.success() {
        return Err(format!("vo-embed failed: {stderr}{stdout}"));
    }
    if !stdout.contains("[VO:OK]") {
        return Err("vo-embed did not output [VO:OK]".to_string());
    }
    Ok(())
}

fn vo_embed_bin() -> PathBuf {
    if let Ok(path) = env::var("VO_EMBED_BIN") {
        return PathBuf::from(path);
    }
    env::current_exe()
        .ok()
        .and_then(|path| path.parent().map(|parent| parent.join("vo-embed")))
        .unwrap_or_else(|| PathBuf::from("target/debug/vo-embed"))
}

fn nanos() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0)
}

fn pattern_matches(message: &str, pattern: Option<&str>) -> bool {
    let Some(pattern) = pattern else { return true };
    let pattern = pattern.trim();
    if pattern.is_empty() {
        return true;
    }
    let mut index = 0usize;
    for part in pattern.split('X').filter(|part| !part.is_empty()) {
        let Some(pos) = message[index..].find(part) else {
            return false;
        };
        index += pos + part.len();
    }
    true
}

fn patterns_match(message: &str, expect: &Expect) -> bool {
    if !expect.patterns.is_empty() {
        return expect
            .patterns
            .iter()
            .all(|pattern| pattern_matches(message, Some(pattern)));
    }
    let Some(pattern) = expect.pattern.as_deref() else {
        return false;
    };
    pattern_matches(message, Some(pattern))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Mutex, OnceLock};

    static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

    struct EnvSnapshot(Vec<(String, Option<String>)>);

    impl EnvSnapshot {
        fn capture(keys: &[&str]) -> Self {
            Self(
                keys.iter()
                    .map(|key| ((*key).to_string(), env::var(key).ok()))
                    .collect(),
            )
        }
    }

    impl Drop for EnvSnapshot {
        fn drop(&mut self) {
            for (key, value) in self.0.drain(..) {
                match value {
                    Some(value) => env::set_var(key, value),
                    None => env::remove_var(key),
                }
            }
        }
    }

    fn result(case_id: &str, target: &str, passed: bool, stdout: &str, error: &str) -> PlanResult {
        PlanResult {
            id: format!("{case_id}::{target}"),
            case_id: case_id.to_string(),
            kind: "file".to_string(),
            path: format!("tests/lang/cases/runtime/{case_id}.vo"),
            target: target.to_string(),
            backend: if target == "vm" || target == "gc-vm" {
                "vm".to_string()
            } else {
                "jit".to_string()
            },
            matrix: None,
            tags: Vec::new(),
            owner: None,
            passed,
            elapsed_ms: 1,
            stdout: stdout.to_string(),
            stderr: String::new(),
            error: error.to_string(),
            expect: Expect {
                kind: "pass".to_string(),
                patterns: Vec::new(),
                pattern: None,
                jit_regular_call_side_exits_min: None,
                jit_loop_entries_min: None,
            },
            baseline: None,
        }
    }

    fn test_job(target: &str, backend: &str, env: &[(&str, &str)]) -> TestJob {
        TestJob {
            id: format!("case::{target}"),
            case_id: "case".to_string(),
            kind: "file".to_string(),
            path: "tests/lang/cases/runtime/case.vo".to_string(),
            target: target.to_string(),
            backend: backend.to_string(),
            matrix: None,
            tags: Vec::new(),
            owner: None,
            env: env
                .iter()
                .map(|(key, value)| ((*key).to_string(), (*value).to_string()))
                .collect(),
            timeout_sec: 1,
            expect: Expect {
                kind: "pass".to_string(),
                patterns: Vec::new(),
                pattern: None,
                jit_regular_call_side_exits_min: None,
                jit_loop_entries_min: None,
            },
        }
    }

    #[test]
    fn vm_test_runner_expect_parses_loop_entry_contract_061() {
        let expect: Expect = serde_json::from_value(serde_json::json!({
            "kind": "pass",
            "jit_loop_entries_min": 1
        }))
        .expect("expect schema should accept JIT loop-entry contract");

        assert_eq!(expect.jit_loop_entries_min, Some(1));
    }

    #[test]
    fn vm_test_runner_jit_jobs_restore_all_runner_owned_jit_env_059() {
        let job = test_job("jit", "jit", &[("VO_JIT_CALL_THRESHOLD", "1")]);
        let keys = job_env_restore_keys(&job);

        assert!(keys.contains(&"VO_JIT_CALL_THRESHOLD".to_string()));
        assert!(
            keys.contains(&"VO_JIT_LOOP_THRESHOLD".to_string()),
            "jit jobs must clear inherited loop threshold when the target does not set it"
        );
        assert!(
            keys.contains(&"VO_JIT_DEBUG".to_string()),
            "jit jobs must clear inherited debug flag when the target does not set it"
        );
    }

    #[test]
    fn vm_test_runner_apply_job_env_clears_unset_jit_env_059() {
        let _guard = ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock poisoned");
        let _snapshot = EnvSnapshot::capture(RUNNER_OWNED_JIT_ENV_KEYS);
        env::set_var("VO_JIT_CALL_THRESHOLD", "999");
        env::set_var("VO_JIT_LOOP_THRESHOLD", "1");
        env::set_var("VO_JIT_DEBUG", "1");
        let job = test_job("jit", "jit", &[("VO_JIT_CALL_THRESHOLD", "1")]);

        let saved_env = apply_job_env(&job);

        assert_eq!(env::var("VO_JIT_CALL_THRESHOLD").as_deref(), Ok("1"));
        assert!(env::var("VO_JIT_LOOP_THRESHOLD").is_err());
        assert!(env::var("VO_JIT_DEBUG").is_err());

        restore_job_env(saved_env);
        assert_eq!(env::var("VO_JIT_CALL_THRESHOLD").as_deref(), Ok("999"));
        assert_eq!(env::var("VO_JIT_LOOP_THRESHOLD").as_deref(), Ok("1"));
        assert_eq!(env::var("VO_JIT_DEBUG").as_deref(), Ok("1"));
    }

    #[test]
    fn vm_test_runner_apply_job_env_clears_unset_gc_env_060() {
        let _guard = ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock poisoned");
        let _snapshot = EnvSnapshot::capture(RUNNER_OWNED_GC_ENV_KEYS);
        env::set_var("VO_GC_STRESS", "1");
        env::set_var("VO_GC_VERIFY", "1");
        env::set_var("VO_GC_DEBUG", "1");
        let job = test_job("vm", "vm", &[]);

        let saved_env = apply_job_env(&job);

        assert!(env::var("VO_GC_STRESS").is_err());
        assert!(env::var("VO_GC_VERIFY").is_err());
        assert!(env::var("VO_GC_DEBUG").is_err());

        restore_job_env(saved_env);
        assert_eq!(env::var("VO_GC_STRESS").as_deref(), Ok("1"));
        assert_eq!(env::var("VO_GC_VERIFY").as_deref(), Ok("1"));
        assert_eq!(env::var("VO_GC_DEBUG").as_deref(), Ok("1"));
    }

    #[test]
    fn vm_test_runner_apply_job_env_preserves_explicit_gc_target_env_060() {
        let _guard = ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock poisoned");
        let _snapshot = EnvSnapshot::capture(RUNNER_OWNED_GC_ENV_KEYS);
        env::set_var("VO_GC_STRESS", "ambient");
        let job = test_job("gc-vm", "vm", &[("VO_GC_STRESS", "1")]);

        let saved_env = apply_job_env(&job);

        assert_eq!(env::var("VO_GC_STRESS").as_deref(), Ok("1"));

        restore_job_env(saved_env);
        assert_eq!(env::var("VO_GC_STRESS").as_deref(), Ok("ambient"));
    }

    #[test]
    fn differential_runner_rejects_jit_stdout_divergence() {
        let mut results = vec![
            result("case", "vm", true, "ok\n", ""),
            result("case", "jit", true, "wrong\n", ""),
        ];

        validate_differential_results(&mut results);

        assert!(results[0].passed);
        assert!(!results[1].passed);
        assert!(results[1].error.contains("stdout expected"));
    }

    #[test]
    fn differential_runner_uses_gc_vm_baseline_for_gc_jit() {
        let mut results = vec![
            result("case", "vm", true, "normal\n", ""),
            result("case", "gc-vm", true, "gc\n", ""),
            result("case", "gc-jit", true, "normal\n", ""),
        ];

        validate_differential_results(&mut results);

        assert!(!results[2].passed);
        assert_eq!(results[2].baseline.as_deref(), Some("gc-vm"));
        assert!(results[2].error.contains("against gc-vm"));
    }

    #[test]
    fn differential_runner_rejects_panic_error_divergence() {
        let mut results = vec![
            result("case", "vm", false, "", "runtime error: nil pointer"),
            result("case", "osr", false, "", "runtime error: JIT panic"),
        ];

        validate_differential_results(&mut results);

        assert!(!results[1].passed);
        assert!(results[1].error.contains("panic/error expected"));
    }

    #[test]
    fn json_result_job_includes_v1_schema_fields() {
        let result = result("case", "jit", false, "", "boom");
        let job = JsonJobResult {
            id: result.id.clone(),
            case_id: result.case_id.clone(),
            kind: result.kind.clone(),
            path: result.path.clone(),
            target: result.target.clone(),
            backend: result.backend.clone(),
            matrix: result.matrix.clone(),
            tags: result.tags.clone(),
            owner: result.owner.clone(),
            expect: result.expect.clone(),
            status: "failed".to_string(),
            elapsed_ms: result.elapsed_ms,
            stdout: result.stdout.clone(),
            stderr: result.stderr.clone(),
            error: result.error.clone(),
            skip_reason: None,
            failure_reason: Some(result.error.clone()),
            baseline: Some("vm".to_string()),
            artifacts: Vec::new(),
        };
        let value = serde_json::to_value(job).unwrap();
        for key in [
            "id",
            "case_id",
            "kind",
            "path",
            "target",
            "backend",
            "matrix",
            "tags",
            "owner",
            "expect",
            "status",
            "elapsed_ms",
            "stdout",
            "stderr",
            "error",
            "skip_reason",
            "failure_reason",
            "baseline",
            "artifacts",
        ] {
            assert!(value.get(key).is_some(), "missing {key}");
        }
    }
}
