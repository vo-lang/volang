//! vo-test launcher - compiles and runs the Vo test runner (cmd/vo-test).

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::{self, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};

const DEFAULT_MAX_WORKERS: usize = 8;
const PROGRESS_INTERVAL: Duration = Duration::from_secs(30);

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
}

#[derive(Debug, Serialize)]
struct PlanResult {
    id: String,
    case_id: String,
    kind: String,
    path: String,
    target: String,
    backend: String,
    passed: bool,
    elapsed_ms: u128,
    stdout: String,
    stderr: String,
    error: String,
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
    status: String,
    elapsed_ms: u128,
    stdout: String,
    stderr: String,
    error: String,
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

    let results = run_jobs_parallel(plan.jobs, opts.jobs, opts.format == "text")?;
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
                    status: if result.passed {
                        "passed".to_string()
                    } else {
                        "failed".to_string()
                    },
                    elapsed_ms: result.elapsed_ms,
                    stdout: result.stdout.clone(),
                    stderr: result.stderr.clone(),
                    error: result.error.clone(),
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
                passed: false,
                elapsed_ms: 0,
                stdout: String::new(),
                stderr: String::new(),
                error: err.to_string(),
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
                    passed: false,
                    elapsed_ms: 0,
                    stdout: String::new(),
                    stderr: String::new(),
                    error: "worker exited without reporting a result".to_string(),
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
        passed: output.status.success() && !timed_out,
        elapsed_ms: start.elapsed().as_millis(),
        stdout,
        stderr,
        error,
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
    run_job(&job).map_err(|err| err.into())
}

fn run_job(job: &TestJob) -> Result<(), String> {
    let saved_env: Vec<_> = job
        .env
        .keys()
        .map(|key| (key.clone(), env::var(key).ok()))
        .collect();
    for (key, value) in &job.env {
        env::set_var(key, value);
    }
    let result = run_job_inner(job);
    for (key, value) in saved_env {
        match value {
            Some(value) => env::set_var(key, value),
            None => env::remove_var(key),
        }
    }
    result
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
    Ok(())
}

fn run_vo_embed(compiled: vo_engine::CompileOutput) -> Result<(), String> {
    let path = env::temp_dir().join(format!("vo_nostd_{}_{}.vob", process::id(), nanos()));
    fs::write(&path, compiled.module.serialize()).map_err(|err| err.to_string())?;
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
