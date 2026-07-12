use crate::dev_bench;
use crate::dev_clean;
use crate::dev_gc_perf;
use crate::dev_loc;
use crate::dev_studio;
use crate::task_system;
use crate::test_config::load_test_config;
use crate::test_system;
use anyhow::{anyhow, bail, Context, Result};
use std::path::{Path, PathBuf};
use std::process::Command;

pub(crate) fn cmd_dpy(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        print_dpy_usage();
        return Ok(());
    }

    match args.remove(0).as_str() {
        "test" => run_test_compat(root, args),
        "run" => {
            let (args, release) = extract_release_flag(args);
            let jit_mode = vo_run_uses_jit(&args);
            run_vo_cli(root, prepend("run", args), jit_mode, release)
        }
        "vo" => run_vo_cli(root, args, false, false),
        "gc-perf" => dev_gc_perf::cmd_gc_perf(root, args),
        "ci" => run_ci_compat(root, args),
        "bench" => dev_bench::cmd_bench(root, args),
        "loc" => dev_loc::cmd_loc(root, args),
        "clean" => dev_clean::cmd_clean(root, args),
        "studio" => dev_studio::cmd_studio(root, args, false),
        "studio-native" => dev_studio::cmd_studio(root, args, true),
        "studio-stop" => dev_studio::cmd_studio_stop(root),
        "-h" | "--help" | "help" => {
            print_dpy_usage();
            Ok(())
        }
        other => run_vo_cli(root, prepend(other, args), false, false),
    }
}

fn run_test_compat(root: &Path, args: Vec<String>) -> Result<()> {
    let (args, release) = extract_release_flag(args);
    let (args, repeat, jobs, verbose) = parse_test_options(args)?;
    let selectors = test_selectors(root)?;

    let (selector, test_paths) = if args.is_empty() {
        (None, Vec::new())
    } else if selectors.contains(&args[0]) {
        (Some(args[0].clone()), args[1..].to_vec())
    } else {
        (None, args)
    };

    for path in &test_paths {
        if path.starts_with('-') {
            bail!("unknown test argument: {path}");
        }
    }

    let mut test_args = vec!["run".to_string(), "--suite".to_string(), "lang".to_string()];
    if let Some(selector) = selector {
        test_args.extend(["--targets".to_string(), selector]);
    }
    if release {
        test_args.push("--release".to_string());
    }
    if let Some(jobs) = jobs {
        test_args.extend(["--jobs".to_string(), jobs.to_string()]);
    }
    if verbose {
        test_args.push("--verbose".to_string());
    }
    for path in test_paths {
        test_args.extend(["--path".to_string(), path]);
    }

    run_repeated(repeat, "test", || {
        test_system::cmd_test(root, test_args.clone())
    })
}

fn run_ci_compat(root: &Path, args: Vec<String>) -> Result<()> {
    let (selector, extra_args) = if args.is_empty() {
        ("smart".to_string(), Vec::new())
    } else if args[0] == "task" {
        if args.len() < 2 {
            bail!("./d.py ci task requires a task name");
        }
        (format!("task:{}", args[1]), args[2..].to_vec())
    } else {
        (args[0].clone(), args[1..].to_vec())
    };
    let mut task_args = vec!["run".to_string(), selector];
    task_args.extend(extra_args);
    task_system::cmd_task(root, task_args)
}

fn run_vo_cli(root: &Path, args: Vec<String>, jit_mode: bool, release: bool) -> Result<()> {
    ensure_vo_cli_built(root, release)?;
    let mut command = Command::new(vo_bin(root, release));
    command.args(args).current_dir(root);
    if jit_mode {
        command.env("VO_JIT_CALL_THRESHOLD", "1");
    }
    let status = command.status().context("could not run vo CLI")?;
    if !status.success() {
        bail!("vo CLI exited with status {status}");
    }
    Ok(())
}

fn ensure_vo_cli_built(root: &Path, release: bool) -> Result<()> {
    let mut command = Command::new("cargo");
    command.args(["build", "-p", "vo"]);
    if release {
        command.arg("--release");
    }
    let status = command
        .current_dir(root)
        .status()
        .context("could not build vo CLI")?;
    if !status.success() {
        let profile = if release { "release" } else { "debug" };
        bail!("cargo build -p vo ({profile}) failed with status {status}");
    }
    Ok(())
}

fn vo_bin(root: &Path, release: bool) -> PathBuf {
    root.join(if release {
        "target/release/vo"
    } else {
        "target/debug/vo"
    })
}

fn test_selectors(root: &Path) -> Result<Vec<String>> {
    let config = load_test_config(root)?;
    let mut selectors: Vec<String> = config.targets.keys().cloned().collect();
    selectors.extend(config.aliases.keys().cloned());
    selectors.extend(config.matrices.keys().cloned());
    Ok(selectors)
}

fn extract_release_flag(args: Vec<String>) -> (Vec<String>, bool) {
    let mut release = false;
    let mut filtered = Vec::new();
    for arg in args {
        if arg == "--release" {
            release = true;
        } else {
            filtered.push(arg);
        }
    }
    (filtered, release)
}

fn parse_test_options(args: Vec<String>) -> Result<(Vec<String>, usize, Option<usize>, bool)> {
    let mut repeat = 1usize;
    let mut jobs = None;
    let mut verbose = false;
    let mut forward_args = Vec::new();
    let mut i = 0usize;
    while i < args.len() {
        match args[i].as_str() {
            "--repeat" | "-n" => {
                i += 1;
                repeat = parse_positive_usize(args.get(i), "repeat count")?;
            }
            arg if arg.starts_with("--repeat=") => {
                repeat = parse_positive_usize_value(&arg["--repeat=".len()..], "repeat count")?;
            }
            "--jobs" | "-j" => {
                i += 1;
                jobs = Some(parse_positive_usize(args.get(i), "job count")?);
            }
            arg if arg.starts_with("--jobs=") => {
                jobs = Some(parse_positive_usize_value(
                    &arg["--jobs=".len()..],
                    "job count",
                )?);
            }
            arg if arg.starts_with("-j") && arg.len() > 2 => {
                jobs = Some(parse_positive_usize_value(&arg[2..], "job count")?);
            }
            "-v" | "--verbose" => verbose = true,
            _ => forward_args.push(args[i].clone()),
        }
        i += 1;
    }
    Ok((forward_args, repeat, jobs, verbose))
}

fn parse_positive_usize(raw: Option<&String>, label: &str) -> Result<usize> {
    let raw = raw.ok_or_else(|| anyhow!("{label} requires a positive integer"))?;
    parse_positive_usize_value(raw, label)
}

fn parse_positive_usize_value(raw: &str, label: &str) -> Result<usize> {
    let parsed: usize = raw
        .parse()
        .with_context(|| format!("invalid {label}: {raw}"))?;
    if parsed == 0 {
        bail!("{label} must be > 0, got {parsed}");
    }
    Ok(parsed)
}

fn run_repeated<F>(repeat: usize, label: &str, mut run: F) -> Result<()>
where
    F: FnMut() -> Result<()>,
{
    for i in 1..=repeat {
        if repeat > 1 {
            println!("[d.py] {label} run {i}/{repeat}");
        }
        if let Err(err) = run() {
            if repeat > 1 {
                eprintln!("[d.py] {label} failed at run {i}/{repeat}");
            }
            return Err(err);
        }
    }
    Ok(())
}

fn vo_run_uses_jit(args: &[String]) -> bool {
    args.iter()
        .any(|arg| arg == "--mode=jit" || arg == "-mode=jit")
}

fn prepend(first: &str, args: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(args.len() + 1);
    out.push(first.to_string());
    out.extend(args);
    out
}

fn print_dpy_usage() {
    println!(
        "usage:\n  ./d.py test [target|alias] [--release] [-v] [-j N|--jobs N] [--repeat N|-n N] [file-or-dir]\n  ./d.py gc-perf [--release] [--json] [--objects=N|--small|--large] [dead-sweep|live-chain|root-table|sparse-root-table|interior-root-table]\n  ./d.py bench [all|vo|<name>|score] [--all-langs] [--runs N] [--warmup N]\n  ./d.py loc [--with-tests]\n  ./d.py clean [all|vo|rust|bench|junk]\n  ./d.py studio [--build-wasm] [--build-only] [--runner] [project]\n  ./d.py studio-native [--build-wasm] [--runner] [project]\n  ./d.py studio-stop\n  ./d.py ci [smart|quality|test|site|pr|full|release-verify|task <task-name>|task:<task-name>]\n  ./d.py run <file.vo> [--mode=vm|jit] [--release] [--codegen]\n  ./d.py vo <args...>"
    );
}
