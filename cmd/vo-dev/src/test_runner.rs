use crate::test_config::load_test_config;
use crate::test_plan::{build_plan, effective_test_targets, TestArgs, TestPlan};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

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
        shard: opts.shard,
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
    let mut command = vo_test_command(root, opts.release);
    prepare_native_aot_command(root, opts.release, &plan, &mut command)?;
    command.arg("run-plan");
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
        shard: opts.shard,
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
    let mut command = vo_test_command(root, opts.release);
    prepare_native_aot_command(root, opts.release, &plan, &mut command)?;
    command.arg("run-plan");
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
    checked_json_run_output(&output, &plan, "vo-test run-plan")
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
        shard: opts.shard,
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
    let mut build = wasm_build_command(wasm_target, opts.release)?;
    let status = build.current_dir(root).status()?;
    if !status.success() {
        bail!(
            "test command failed: {}",
            command_description(&wasm_target.build_command)
        );
    }
    run_wasm_prepare_commands(root, wasm_target)?;
    let plan_path =
        std::env::temp_dir().join(format!("volang-wasm-test-plan-{}.json", std::process::id()));
    fs::write(&plan_path, serde_json::to_string_pretty(&plan)?)?;
    let mut command = command_from_args(&wasm_target.runner_command, "WASM runner command")?;
    command.arg("--plan");
    command.arg(&plan_path);
    command.args(["--format", "text"]);
    command.current_dir(root);
    command.env(
        "VO_TEST_PROFILE",
        if opts.release { "release" } else { "debug" },
    );
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
        shard: opts.shard,
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
    let mut build = wasm_build_command(wasm_target, opts.release)?;
    let output = build.current_dir(root).output()?;
    if !output.status.success() {
        bail!(
            "test command failed: {}\nstdout:\n{}\nstderr:\n{}",
            command_description(&wasm_target.build_command),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    run_wasm_prepare_commands(root, wasm_target)?;
    let plan_path =
        std::env::temp_dir().join(format!("volang-wasm-test-plan-{}.json", std::process::id()));
    fs::write(&plan_path, serde_json::to_string_pretty(&plan)?)?;
    let mut command = command_from_args(&wasm_target.runner_command, "WASM runner command")?;
    command.arg("--plan");
    command.arg(&plan_path);
    command.args(["--format", "json"]);
    command.current_dir(root);
    command.env(
        "VO_TEST_PROFILE",
        if opts.release { "release" } else { "debug" },
    );
    for (key, value) in wasm_target.env.clone() {
        command.env(key, value);
    }
    let output = command.output();
    let _ = fs::remove_file(&plan_path);
    let output = output?;
    checked_json_run_output(
        &output,
        &plan,
        &command_description(&wasm_target.runner_command),
    )
}

fn run_wasm_prepare_commands(root: &Path, target: &crate::test_config::TestTarget) -> Result<()> {
    for args in &target.prepare_commands {
        let mut command = command_from_args(args, "WASM prepare command")?;
        let output = command.current_dir(root).output()?;
        if !output.status.success() {
            bail!(
                "test command failed: {}\nstdout:\n{}\nstderr:\n{}",
                command_description(args),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }
    Ok(())
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

fn checked_json_run_output(
    output: &std::process::Output,
    plan: &TestPlan,
    command: &str,
) -> Result<JsonRunOutput> {
    let result = parse_json_run_output(&output.stdout, &output.stderr, command)?;
    validate_json_run_output(&result, plan)?;
    if output.status.success() != (result.failed == 0) {
        bail!(
            "{command} exit status {} contradicts its result ({} failed)\nstdout:\n{}\nstderr:\n{}",
            output.status,
            result.failed,
            summarize_process_output(&output.stdout),
            summarize_process_output(&output.stderr)
        );
    }
    Ok(result)
}

fn validate_json_run_output(result: &JsonRunOutput, plan: &TestPlan) -> Result<()> {
    if result.schema != "volang.test-result.v1" || result.suite != plan.suite {
        bail!("test result schema or suite differs from the selected plan");
    }
    let expected = plan
        .jobs
        .iter()
        .map(|job| (job.id.as_str(), job))
        .collect::<BTreeMap<_, _>>();
    if expected.is_empty()
        || expected.len() != plan.jobs.len()
        || result.jobs.len() != expected.len()
    {
        bail!("test result job count differs from the nonempty selected plan");
    }
    let mut seen = BTreeSet::new();
    let (mut passed, mut failed) = (0, 0);
    for job in &result.jobs {
        let id = job["id"]
            .as_str()
            .ok_or_else(|| anyhow!("test result lacks job id"))?;
        let planned = expected
            .get(id)
            .ok_or_else(|| anyhow!("unexpected test result job {id}"))?;
        if !seen.insert(id) {
            bail!("duplicate test result job {id}");
        }
        for (field, value) in [
            ("case_id", &planned.case_id),
            ("target", &planned.target),
            ("backend", &planned.backend),
            ("kind", &planned.kind),
            ("path", &planned.path),
        ] {
            if job[field].as_str() != Some(value.as_str()) {
                bail!("test result {id} differs from plan field {field}");
            }
        }
        match job["status"].as_str() {
            Some("passed") => passed += 1,
            Some("failed") => failed += 1,
            _ => bail!("test result {id} was not executed; skips must be resolved by the planner"),
        }
    }
    if (result.passed, result.failed, result.skipped) != (passed, failed, 0) {
        bail!("test result counters contradict individual job outcomes");
    }
    Ok(())
}

fn parse_json_run_output(stdout: &[u8], stderr: &[u8], command: &str) -> Result<JsonRunOutput> {
    if stdout.iter().all(u8::is_ascii_whitespace) {
        bail!(
            "{command} did not emit JSON result on stdout\nstdout:\n{}\nstderr:\n{}",
            summarize_process_output(stdout),
            summarize_process_output(stderr)
        );
    }
    serde_json::from_slice(stdout).with_context(|| {
        format!(
            "could not parse {command} JSON result\nstdout:\n{}\nstderr:\n{}",
            summarize_process_output(stdout),
            summarize_process_output(stderr)
        )
    })
}

fn summarize_process_output(output: &[u8]) -> String {
    const MAX_CHARS: usize = 4000;

    let text = String::from_utf8_lossy(output);
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return "(empty)".to_string();
    }

    let mut summary: String = trimmed.chars().take(MAX_CHARS).collect();
    if trimmed.chars().count() > MAX_CHARS {
        summary.push_str("\n... <truncated>");
    }
    summary
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
    let accept = std::thread::spawn(move || listener.accept());
    let client = TcpStream::connect_timeout(&addr, Duration::from_secs(1))
        .with_context(|| format!("could not connect to loopback listener at {addr}"))?;
    let (_server, _) = accept
        .join()
        .map_err(|_| anyhow!("loopback accept thread panicked"))?
        .with_context(|| format!("could not accept loopback connection at {addr}"))?;
    drop(client);
    Ok(())
}

fn prepare_native_aot_command(
    root: &Path,
    release: bool,
    plan: &TestPlan,
    runner: &mut Command,
) -> Result<()> {
    if !plan.jobs.iter().any(|job| job.backend == "native-aot") {
        return Ok(());
    }
    let features = native_aot_runtime_features(root, plan)?;
    let mut command = Command::new("cargo");
    command
        .current_dir(root)
        .args([
            "build",
            "--locked",
            "--timings",
            "--message-format=json",
            "--no-default-features",
            "-p",
            "vo",
            "-p",
            "vo-aot-runtime",
            "-p",
            "vo-test",
        ])
        .stderr(std::process::Stdio::inherit());
    if release {
        command.arg("--release");
    }
    if !features.is_empty() {
        command.arg("--features").arg(
            features
                .iter()
                .map(|feature| format!("vo-aot-runtime/{feature}"))
                .collect::<Vec<_>>()
                .join(","),
        );
    }
    let output = command
        .output()
        .context("could not build shared Native AOT tools")?;
    if !output.status.success() {
        bail!("shared Native AOT tools build failed");
    }
    let mut compiler = None;
    let mut runtime = None;
    let mut test_runner = None;
    for line in output
        .stdout
        .split(|byte| *byte == b'\n')
        .filter(|line| !line.is_empty())
    {
        let artifact: serde_json::Value = serde_json::from_slice(line)?;
        if artifact["reason"] != "compiler-artifact" {
            continue;
        }
        match artifact["target"]["name"].as_str() {
            Some("vo") => compiler = artifact["executable"].as_str().map(PathBuf::from),
            Some("vo-test") => test_runner = artifact["executable"].as_str().map(PathBuf::from),
            Some("vo_aot_runtime") => {
                runtime = artifact["filenames"]
                    .as_array()
                    .and_then(|files| {
                        files
                            .iter()
                            .filter_map(|file| file.as_str())
                            .find(|file| file.ends_with(".a") || file.ends_with(".lib"))
                    })
                    .map(PathBuf::from);
            }
            _ => {}
        }
    }
    // The compiler, runtime and runner share one Cargo feature resolution.
    // Use the actual artifact instead of a timestamp-based sibling guess or
    // another cargo run that would re-resolve the engine's JIT features.
    *runner = Command::new(
        test_runner
            .context("Cargo did not report the native test runner")?
            .canonicalize()?,
    );
    runner
        .env(
            "VO_TEST_NATIVE_AOT_COMPILER",
            compiler
                .context("Cargo did not report the Native AOT compiler")?
                .canonicalize()?,
        )
        .env(
            "VO_TEST_NATIVE_AOT_RUNTIME",
            runtime
                .context("Cargo did not report the Native AOT runtime")?
                .canonicalize()?,
        );
    Ok(())
}

fn native_aot_runtime_features(root: &Path, plan: &TestPlan) -> Result<Vec<String>> {
    let config = load_test_config(root)?;
    let variants: BTreeSet<_> = plan
        .jobs
        .iter()
        .filter(|job| job.backend == "native-aot")
        .map(|job| {
            config.targets[&job.target]
                .native_aot_runtime_features
                .clone()
        })
        .collect();
    if variants.len() > 1 {
        bail!("Native AOT runtime variants require separate test runs to preserve capability contracts");
    }
    Ok(variants.into_iter().next().unwrap_or_default())
}

fn vo_test_command(root: &Path, release: bool) -> Command {
    if let Some(path) = sibling_tool(root, "vo-test", release) {
        return Command::new(path);
    }

    let mut command = Command::new("cargo");
    command.args(["run", "--locked", "-q"]);
    if release {
        command.arg("--release");
    }
    command.args(["-p", "vo-test", "--"]);
    command
}

fn sibling_tool(root: &Path, name: &str, release: bool) -> Option<PathBuf> {
    let exe_name = if cfg!(windows) {
        format!("{name}.exe")
    } else {
        name.to_string()
    };
    let current = std::env::current_exe().ok()?;
    sibling_tool_candidate(&current, root, &exe_name, release)
}

fn sibling_tool_candidate(
    current: &Path,
    root: &Path,
    exe_name: &str,
    release: bool,
) -> Option<PathBuf> {
    let dir = current.parent()?;
    let expected_profile = if release { "release" } else { "debug" };
    if dir.file_name()?.to_str()? != expected_profile {
        return None;
    }
    let candidate = dir.join(exe_name);
    if !candidate.is_file() {
        return None;
    }
    let current_modified = current.metadata().ok()?.modified().ok()?;
    let candidate_modified = candidate.metadata().ok()?.modified().ok()?;
    if candidate_modified < current_modified {
        return None;
    }
    if candidate_modified < latest_native_runner_input_mtime(root)? {
        return None;
    }
    Some(candidate)
}

fn latest_native_runner_input_mtime(root: &Path) -> Option<SystemTime> {
    let mut latest = UNIX_EPOCH;
    for input in [
        "Cargo.lock",
        "Cargo.toml",
        "rust-toolchain.toml",
        "cmd/vo-test",
        "lang/crates",
        "lang/stdlib",
    ] {
        latest_mtime(&root.join(input), &mut latest)?;
    }
    Some(latest)
}

fn latest_mtime(path: &Path, latest: &mut SystemTime) -> Option<()> {
    let metadata = fs::metadata(path).ok()?;
    if let Ok(modified) = metadata.modified() {
        *latest = (*latest).max(modified);
    }
    if metadata.is_dir() {
        for entry in fs::read_dir(path).ok()? {
            latest_mtime(&entry.ok()?.path(), latest)?;
        }
    }
    Some(())
}

fn command_from_args(args: &[String], description: &str) -> Result<Command> {
    let Some(program) = args.first() else {
        bail!("{description} cannot be empty");
    };
    let mut command = Command::new(program);
    command.args(&args[1..]);
    Ok(command)
}

fn wasm_build_command(target: &crate::test_config::TestTarget, release: bool) -> Result<Command> {
    let args = &target.build_command;
    if args.len() < 2 {
        bail!("WASM build command requires a program and subcommand");
    }
    let profile_args = if release {
        &target.release_build_args
    } else {
        &target.debug_build_args
    };
    let mut command = Command::new(&args[0]);
    command.arg(&args[1]);
    // Build-tool options must precede trailing Cargo arguments (wasm-pack).
    // Comparing tokens cannot distinguish options consumed at different levels.
    command.args(profile_args);
    command.args(&args[2..]);
    Ok(command)
}

fn command_description(args: &[String]) -> String {
    args.join(" ")
}

fn build_vo_embed(root: &Path, release: bool) -> Result<()> {
    let mut command = Command::new("cargo");
    command.args(["build", "--locked", "-p", "vo-embed"]);
    if release {
        command.arg("--release");
    }
    let status = command.current_dir(root).status()?;
    if !status.success() {
        bail!("test command failed: cargo build -p vo-embed");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_aot_core_and_compiler_host_cannot_share_a_runtime() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        for (targets, expected) in [
            ("vm,native-aot", Some(Vec::<String>::new())),
            ("vm,native-aot-host", Some(vec!["toolchain-host".into()])),
            ("native-aot,native-aot-host", None),
        ] {
            let opts = TestArgs::parse(
                &root,
                vec![
                    "--targets".into(),
                    targets.into(),
                    "--tags".into(),
                    "compiler-host".into(),
                ],
            )
            .unwrap();
            let plan = build_plan(&root, &opts).unwrap();
            let result = native_aot_runtime_features(&root, &plan);
            match expected {
                Some(features) => assert_eq!(result.unwrap(), features),
                None => assert!(result
                    .unwrap_err()
                    .to_string()
                    .contains("separate test runs")),
            }
        }
    }

    #[test]
    fn wasm_profiles_precede_forwarded_cargo_options() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let config = load_test_config(&root).unwrap();
        for (release, flag) in [(false, "--dev"), (true, "--release")] {
            let command = wasm_build_command(&config.targets["wasm"], release).unwrap();
            let args = command
                .get_args()
                .map(|s| s.to_str().unwrap())
                .collect::<Vec<_>>();
            assert_eq!(args[1], flag);
            assert_eq!(args.iter().filter(|s| **s == flag).count(), 1);
            assert!(args.iter().position(|s| *s == "--").unwrap() > 1);
        }
    }

    #[test]
    fn result_must_cover_exactly_the_plan_and_match_exit_status() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let opts = TestArgs::parse(
            &root,
            vec![
                "--targets".into(),
                "vm".into(),
                "--path".into(),
                "tests/lang/cases/runtime/array_len_cap_runtime.vo".into(),
            ],
        )
        .unwrap();
        let plan = build_plan(&root, &opts).unwrap();
        let mut payload = serde_json::json!({
            "schema": "volang.test-result.v1", "suite": plan.suite,
            "passed": plan.jobs.len(), "failed": 0, "skipped": 0,
            "jobs": plan.jobs.iter().map(|job| {
                let mut value = serde_json::to_value(job).unwrap();
                value["status"] = "passed".into();
                value
            }).collect::<Vec<_>>()
        });
        let decode =
            |v: &serde_json::Value| serde_json::from_value::<JsonRunOutput>(v.clone()).unwrap();
        validate_json_run_output(&decode(&payload), &plan).unwrap();
        #[cfg(unix)]
        let status = {
            use std::os::unix::process::ExitStatusExt;
            std::process::ExitStatus::from_raw(7 << 8)
        };
        #[cfg(windows)]
        let status = {
            use std::os::windows::process::ExitStatusExt;
            std::process::ExitStatus::from_raw(7)
        };
        let output = std::process::Output {
            status,
            stdout: serde_json::to_vec(&payload).unwrap(),
            stderr: b"injected failure".to_vec(),
        };
        assert!(checked_json_run_output(&output, &plan, "fixture")
            .unwrap_err()
            .to_string()
            .contains("exit status"));
        for (field, bad) in [
            ("schema", serde_json::json!("wrong")),
            ("passed", serde_json::json!(42)),
            ("jobs", serde_json::json!([])),
        ] {
            let mut invalid = payload.clone();
            invalid[field] = bad;
            assert!(validate_json_run_output(&decode(&invalid), &plan).is_err());
        }
        for (field, bad) in [("target", "jit"), ("id", "extra"), ("status", "skipped")] {
            let mut invalid = payload.clone();
            invalid["jobs"][0][field] = bad.into();
            assert!(validate_json_run_output(&decode(&invalid), &plan).is_err());
        }
        payload["jobs"][0]["status"] = "failed".into();
        payload["passed"] = 0.into();
        payload["failed"] = 1.into();
        validate_json_run_output(&decode(&payload), &plan).unwrap();
    }

    #[test]
    fn parse_json_run_output_reports_stderr_when_json_is_missing() {
        let err = parse_json_run_output(
            b"",
            b"dyld: Library not loaded: libsimdjson.30.dylib",
            "node lang/crates/vo-web/test_runner.mjs",
        )
        .unwrap_err();
        let message = format!("{err:#}");

        assert!(message.contains("did not emit JSON result on stdout"));
        assert!(message.contains("stderr:"));
        assert!(message.contains("libsimdjson.30.dylib"));
    }

    #[test]
    fn parse_json_run_output_rejects_wrapped_or_truncated_payloads() {
        for invalid in [
            &br#"prefix {"schema":"volang.test-result.v1","suite":"lang","passed":1,"failed":0,"skipped":0,"jobs":[]} suffix"#[..],
            &br#"{"schema":"volang.test-result.v1","suite":"lang","passed":1"#[..],
        ] {
            assert!(parse_json_run_output(invalid, b"diagnostic", "runner").is_err());
        }
    }

    #[test]
    fn sibling_vo_test_requires_matching_profile_and_fresh_binary() {
        let unique = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-sibling-vo-test-{}-{unique}",
            std::process::id()
        ));
        let debug_dir = root.join("debug");
        fs::create_dir_all(&debug_dir).unwrap();
        fs::create_dir_all(root.join("cmd/vo-test/src")).unwrap();
        fs::create_dir_all(root.join("lang/crates/vo-vm/src")).unwrap();
        fs::create_dir_all(root.join("lang/stdlib")).unwrap();
        fs::write(root.join("Cargo.lock"), b"# lock").unwrap();
        fs::write(root.join("Cargo.toml"), b"[workspace]\n").unwrap();
        fs::write(root.join("rust-toolchain.toml"), b"[toolchain]\n").unwrap();
        fs::write(root.join("cmd/vo-test/src/main.rs"), b"fn main() {}").unwrap();
        fs::write(root.join("lang/crates/vo-vm/src/lib.rs"), b"").unwrap();
        fs::write(root.join("lang/stdlib/core.vo"), b"package core\n").unwrap();

        let current = debug_dir.join("vo-dev");
        let sibling = debug_dir.join(if cfg!(windows) {
            "vo-test.exe"
        } else {
            "vo-test"
        });
        fs::write(&current, b"vo-dev").unwrap();
        fs::write(&sibling, b"vo-test").unwrap();

        assert_eq!(
            sibling_tool_candidate(
                &current,
                &root,
                sibling.file_name().unwrap().to_str().unwrap(),
                false
            ),
            Some(sibling.clone())
        );
        assert_eq!(
            sibling_tool_candidate(
                &current,
                &root,
                sibling.file_name().unwrap().to_str().unwrap(),
                true
            ),
            None,
            "debug sibling must not satisfy a --release run"
        );

        std::thread::sleep(Duration::from_millis(50));
        fs::write(
            root.join("lang/stdlib/core.vo"),
            b"package core\nconst X = 1\n",
        )
        .unwrap();
        assert_eq!(
            sibling_tool_candidate(
                &current,
                &root,
                sibling.file_name().unwrap().to_str().unwrap(),
                false
            ),
            None,
            "stale sibling must fall back to cargo run when embedded stdlib source is newer"
        );

        std::thread::sleep(Duration::from_millis(50));
        fs::write(&sibling, b"newer vo-test after stdlib").unwrap();
        assert_eq!(
            sibling_tool_candidate(
                &current,
                &root,
                sibling.file_name().unwrap().to_str().unwrap(),
                false
            ),
            Some(sibling.clone())
        );

        std::thread::sleep(Duration::from_millis(50));
        fs::write(
            root.join("cmd/vo-test/src/main.rs"),
            b"fn main() { println!(\"new\"); }",
        )
        .unwrap();
        assert_eq!(
            sibling_tool_candidate(
                &current,
                &root,
                sibling.file_name().unwrap().to_str().unwrap(),
                false
            ),
            None,
            "stale sibling must fall back to cargo run when vo-test source is newer"
        );

        std::thread::sleep(Duration::from_millis(50));
        fs::write(&sibling, b"newer vo-test").unwrap();
        assert_eq!(
            sibling_tool_candidate(
                &current,
                &root,
                sibling.file_name().unwrap().to_str().unwrap(),
                false
            ),
            Some(sibling.clone())
        );

        std::thread::sleep(Duration::from_millis(50));
        fs::write(&current, b"newer vo-dev").unwrap();
        assert_eq!(
            sibling_tool_candidate(
                &current,
                &root,
                sibling.file_name().unwrap().to_str().unwrap(),
                false
            ),
            None,
            "stale sibling must fall back to cargo run so current source is rebuilt"
        );

        let _ = fs::remove_dir_all(root);
    }
}
