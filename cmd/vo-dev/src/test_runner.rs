use crate::test_config::load_test_config;
use crate::test_plan::{build_plan, effective_test_targets, TestArgs, TestPlan};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use std::process::Command;

#[derive(Debug, Deserialize, Serialize)]
struct JsonRunOutput {
    schema: String,
    suite: String,
    passed: usize,
    failed: usize,
    skipped: usize,
    jobs: Vec<serde_json::Value>,
}

pub(crate) fn run_tests(root: &Path, opts: &TestArgs) -> Result<()> {
    let config = load_test_config(root)?;
    let effective_targets = effective_test_targets(root, opts)?;
    let mut wasm_targets = Vec::new();
    let mut run_plan_targets = Vec::new();
    for target_name in &effective_targets {
        let target = config
            .targets
            .get(target_name)
            .ok_or_else(|| anyhow!("unknown test target: {target_name}"))?;
        if target.kind == "wasm" {
            wasm_targets.push(target_name.clone());
        } else {
            run_plan_targets.push(target_name.clone());
        }
    }
    let has_wasm = !wasm_targets.is_empty();
    let has_native = !run_plan_targets.is_empty();
    if has_wasm && !has_native {
        if opts.jobs.is_some() {
            bail!("--jobs is only used by native test targets");
        }
        if opts.verbose {
            bail!("--verbose is only used by native test targets");
        }
    }
    if opts.format == "json" {
        let mut outputs = Vec::new();
        if !run_plan_targets.is_empty() {
            outputs.push(run_native_tests_json(root, opts, &run_plan_targets)?);
        }
        for target_name in &wasm_targets {
            outputs.push(run_wasm_tests_json(root, opts, target_name)?);
        }
        let output = aggregate_json_outputs(outputs)?;
        let failed = output.failed;
        println!("{}", serde_json::to_string_pretty(&output)?);
        if failed > 0 {
            bail!("{} test job(s) failed", failed);
        }
        return Ok(());
    }

    for target_name in &wasm_targets {
        run_wasm_tests(root, opts, target_name)?;
    }

    if run_plan_targets.is_empty() {
        return Ok(());
    }
    run_native_tests_text(root, opts, &run_plan_targets)
}

fn run_native_tests_text(root: &Path, opts: &TestArgs, run_plan_targets: &[String]) -> Result<()> {
    let config = load_test_config(root)?;
    if run_plan_targets.iter().any(|target| {
        config
            .targets
            .get(target)
            .is_some_and(|target| target.kind == "embed")
    }) {
        build_vo_embed(root, opts.release)?;
    }

    let native_opts = TestArgs {
        suite: opts.suite.clone(),
        targets: run_plan_targets.to_vec(),
        targets_explicit: true,
        matrices: opts.matrices.clone(),
        tags: opts.tags.clone(),
        owners: opts.owners.clone(),
        format: "json".to_string(),
        jobs: opts.jobs,
        paths: opts.paths.clone(),
        verbose: opts.verbose,
        release: opts.release,
        explain: opts.explain,
        repeat: opts.repeat,
    };
    let plan = build_plan(root, &native_opts)?;
    if plan_needs_loopback_preflight(&plan) {
        check_localhost_loopback().context(
            "localhost loopback preflight failed before running selected net/http tests; \
             local sandboxing can block 127.0.0.1 sockets, so rerun outside the sandbox or allow \
             local networking for this test command",
        )?;
    }
    let plan_path =
        std::env::temp_dir().join(format!("volang-test-plan-{}.json", std::process::id()));
    fs::write(&plan_path, serde_json::to_string_pretty(&plan)?)?;
    let mut command = Command::new("cargo");
    command.args(["run", "-q"]);
    if opts.release {
        command.arg("--release");
    }
    command.args(["-p", "vo-test", "--", "run-plan"]);
    command.arg(&plan_path);
    if let Some(jobs) = opts.jobs {
        command.args(["--jobs", &jobs.to_string()]);
    }
    command.args(["--format", &opts.format]);
    if opts.verbose {
        command.arg("--verbose");
    }
    let status = command.current_dir(root).status();
    let _ = fs::remove_file(&plan_path);
    let status = status.context("could not run vo-test run-plan")?;
    if !status.success() {
        bail!("vo-test run-plan failed");
    }
    Ok(())
}

fn run_native_tests_json(
    root: &Path,
    opts: &TestArgs,
    run_plan_targets: &[String],
) -> Result<JsonRunOutput> {
    let config = load_test_config(root)?;
    if run_plan_targets.iter().any(|target| {
        config
            .targets
            .get(target)
            .is_some_and(|target| target.kind == "embed")
    }) {
        build_vo_embed(root, opts.release)?;
    }

    let native_opts = TestArgs {
        suite: opts.suite.clone(),
        targets: run_plan_targets.to_vec(),
        targets_explicit: true,
        matrices: opts.matrices.clone(),
        tags: opts.tags.clone(),
        owners: opts.owners.clone(),
        format: "json".to_string(),
        jobs: opts.jobs,
        paths: opts.paths.clone(),
        verbose: opts.verbose,
        release: opts.release,
        explain: opts.explain,
        repeat: opts.repeat,
    };
    let plan = build_plan(root, &native_opts)?;
    if plan_needs_loopback_preflight(&plan) {
        check_localhost_loopback().context(
            "localhost loopback preflight failed before running selected net/http tests; \
             local sandboxing can block 127.0.0.1 sockets, so rerun outside the sandbox or allow \
             local networking for this test command",
        )?;
    }
    let plan_path =
        std::env::temp_dir().join(format!("volang-test-plan-{}.json", std::process::id()));
    fs::write(&plan_path, serde_json::to_string_pretty(&plan)?)?;
    let mut command = Command::new("cargo");
    command.args(["run", "-q"]);
    if opts.release {
        command.arg("--release");
    }
    command.args(["-p", "vo-test", "--", "run-plan"]);
    command.arg(&plan_path);
    if let Some(jobs) = opts.jobs {
        command.args(["--jobs", &jobs.to_string()]);
    }
    command.args(["--format", "json"]);
    if opts.verbose {
        command.arg("--verbose");
    }
    let output = command
        .current_dir(root)
        .output()
        .context("could not run vo-test run-plan")?;
    let _ = fs::remove_file(&plan_path);
    parse_json_run_output(&output.stdout, &output.stderr, "vo-test run-plan")
}

fn run_wasm_tests(root: &Path, opts: &TestArgs, wasm_target_name: &str) -> Result<()> {
    let config = load_test_config(root)?;
    let wasm_target = config
        .targets
        .get(wasm_target_name)
        .ok_or_else(|| anyhow!("unknown test target: {wasm_target_name}"))?;
    if wasm_target.kind != "wasm" {
        bail!("test target {wasm_target_name} is not a wasm target");
    }
    let wasm_opts = TestArgs {
        suite: opts.suite.clone(),
        targets: vec![wasm_target_name.to_string()],
        targets_explicit: true,
        matrices: opts.matrices.clone(),
        tags: opts.tags.clone(),
        owners: opts.owners.clone(),
        format: "json".to_string(),
        jobs: None,
        paths: opts.paths.clone(),
        verbose: false,
        release: opts.release,
        explain: opts.explain,
        repeat: opts.repeat,
    };
    let plan = build_plan(root, &wasm_opts)?;
    if let Some(job) = plan.jobs.iter().find(|job| job.kind != "file") {
        bail!(
            "WASM test runner only supports file cases, but selected {} ({})",
            job.id,
            job.kind
        );
    }
    if plan.jobs.is_empty() {
        bail!("no WASM tests selected");
    }
    let mut build = command_from_args(&wasm_target.build_command, "WASM build command")?;
    if opts.release {
        build.args(&wasm_target.release_build_args);
    }
    let status = build.current_dir(root).status()?;
    if !status.success() {
        bail!(
            "test command failed: {}",
            command_description(&wasm_target.build_command)
        );
    }
    let plan_path =
        std::env::temp_dir().join(format!("volang-wasm-test-plan-{}.json", std::process::id()));
    fs::write(&plan_path, serde_json::to_string_pretty(&plan)?)?;
    let mut command = command_from_args(&wasm_target.runner_command, "WASM runner command")?;
    command.arg("--plan");
    command.arg(&plan_path);
    command.args(["--format", "text"]);
    command.current_dir(root);
    for (key, value) in wasm_target.env.clone() {
        command.env(key, value);
    }
    let status = command.status();
    let _ = fs::remove_file(&plan_path);
    let status = status?;
    if !status.success() {
        bail!(
            "test command failed: {}",
            command_description(&wasm_target.runner_command)
        );
    }
    Ok(())
}

fn run_wasm_tests_json(
    root: &Path,
    opts: &TestArgs,
    wasm_target_name: &str,
) -> Result<JsonRunOutput> {
    let config = load_test_config(root)?;
    let wasm_target = config
        .targets
        .get(wasm_target_name)
        .ok_or_else(|| anyhow!("unknown test target: {wasm_target_name}"))?;
    if wasm_target.kind != "wasm" {
        bail!("test target {wasm_target_name} is not a wasm target");
    }
    let wasm_opts = TestArgs {
        suite: opts.suite.clone(),
        targets: vec![wasm_target_name.to_string()],
        targets_explicit: true,
        matrices: opts.matrices.clone(),
        tags: opts.tags.clone(),
        owners: opts.owners.clone(),
        format: "json".to_string(),
        jobs: None,
        paths: opts.paths.clone(),
        verbose: false,
        release: opts.release,
        explain: opts.explain,
        repeat: opts.repeat,
    };
    let plan = build_plan(root, &wasm_opts)?;
    if let Some(job) = plan.jobs.iter().find(|job| job.kind != "file") {
        bail!(
            "WASM test runner only supports file cases, but selected {} ({})",
            job.id,
            job.kind
        );
    }
    if plan.jobs.is_empty() {
        bail!("no WASM tests selected");
    }
    let mut build = command_from_args(&wasm_target.build_command, "WASM build command")?;
    if opts.release {
        build.args(&wasm_target.release_build_args);
    }
    let output = build.current_dir(root).output()?;
    if !output.status.success() {
        bail!(
            "test command failed: {}\nstdout:\n{}\nstderr:\n{}",
            command_description(&wasm_target.build_command),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    let plan_path =
        std::env::temp_dir().join(format!("volang-wasm-test-plan-{}.json", std::process::id()));
    fs::write(&plan_path, serde_json::to_string_pretty(&plan)?)?;
    let mut command = command_from_args(&wasm_target.runner_command, "WASM runner command")?;
    command.arg("--plan");
    command.arg(&plan_path);
    command.args(["--format", "json"]);
    command.current_dir(root);
    for (key, value) in wasm_target.env.clone() {
        command.env(key, value);
    }
    let output = command.output();
    let _ = fs::remove_file(&plan_path);
    let output = output?;
    parse_json_run_output(
        &output.stdout,
        &output.stderr,
        &command_description(&wasm_target.runner_command),
    )
}

fn aggregate_json_outputs(outputs: Vec<JsonRunOutput>) -> Result<JsonRunOutput> {
    let mut aggregate = JsonRunOutput {
        schema: "volang.test-result.v1".to_string(),
        suite: "lang".to_string(),
        passed: 0,
        failed: 0,
        skipped: 0,
        jobs: Vec::new(),
    };
    for output in outputs {
        if output.schema != "volang.test-result.v1" {
            bail!("unsupported test result schema: {}", output.schema);
        }
        aggregate.suite = output.suite;
        aggregate.passed += output.passed;
        aggregate.failed += output.failed;
        aggregate.skipped += output.skipped;
        aggregate.jobs.extend(output.jobs);
    }
    Ok(aggregate)
}

fn parse_json_run_output(stdout: &[u8], stderr: &[u8], command: &str) -> Result<JsonRunOutput> {
    let text = String::from_utf8_lossy(stdout);
    let start = text
        .find('{')
        .ok_or_else(|| anyhow!("{command} did not emit JSON result on stdout"))?;
    let end = text
        .rfind('}')
        .ok_or_else(|| anyhow!("{command} emitted truncated JSON result"))?;
    let json = &text[start..=end];
    serde_json::from_str(json).with_context(|| {
        format!(
            "could not parse {command} JSON result; stderr: {}",
            String::from_utf8_lossy(stderr).trim()
        )
    })
}

fn plan_needs_loopback_preflight(plan: &TestPlan) -> bool {
    plan.jobs.iter().any(|job| {
        let path = job.path.replace('\\', "/").to_ascii_lowercase();
        let id = job.id.to_ascii_lowercase();
        path.contains("/net/")
            || path.contains("http")
            || path.contains("socket")
            || id.contains("http")
            || id.contains("socket")
            || id.contains("net_")
    })
}

fn check_localhost_loopback() -> Result<()> {
    let listener = TcpListener::bind(("127.0.0.1", 0)).context("could not bind 127.0.0.1:0")?;
    let addr = listener
        .local_addr()
        .context("could not inspect loopback listener address")?;
    let client = TcpStream::connect(addr)
        .with_context(|| format!("could not connect to loopback listener at {addr}"))?;
    let (_server, _) = listener
        .accept()
        .with_context(|| format!("could not accept loopback connection at {addr}"))?;
    drop(client);
    Ok(())
}

fn command_from_args(args: &[String], description: &str) -> Result<Command> {
    let Some(program) = args.first() else {
        bail!("{description} cannot be empty");
    };
    let mut command = Command::new(program);
    command.args(&args[1..]);
    Ok(command)
}

fn command_description(args: &[String]) -> String {
    args.join(" ")
}

fn build_vo_embed(root: &Path, release: bool) -> Result<()> {
    let mut command = Command::new("cargo");
    command.args(["build", "-p", "vo-embed"]);
    if release {
        command.arg("--release");
    }
    let status = command.current_dir(root).status()?;
    if !status.success() {
        bail!("test command failed: cargo build -p vo-embed");
    }
    Ok(())
}
