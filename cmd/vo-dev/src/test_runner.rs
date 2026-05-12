use crate::test_config::load_test_config;
use crate::test_plan::{build_plan, TestArgs};
use anyhow::{anyhow, bail, Context, Result};
use std::fs;
use std::path::Path;
use std::process::Command;

pub(crate) fn run_tests(root: &Path, opts: &TestArgs) -> Result<()> {
    let config = load_test_config(root)?;
    let mut wasm_targets = Vec::new();
    let mut run_plan_targets = Vec::new();
    for target_name in &opts.targets {
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
    if has_wasm {
        if opts.format != "text" {
            bail!("vo-dev test run --format json does not support the wasm target");
        }
        for target_name in &wasm_targets {
            run_wasm_tests(root, opts, target_name)?;
        }
    }

    if run_plan_targets.is_empty() {
        return Ok(());
    }
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
        targets: run_plan_targets,
        format: "json".to_string(),
        jobs: opts.jobs,
        paths: opts.paths.clone(),
        verbose: opts.verbose,
        release: opts.release,
    };
    let plan = build_plan(root, &native_opts)?;
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
        format: "json".to_string(),
        jobs: None,
        paths: opts.paths.clone(),
        verbose: false,
        release: opts.release,
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
