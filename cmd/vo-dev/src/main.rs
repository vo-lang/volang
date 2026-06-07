use anyhow::{bail, Context, Result};
use std::env;
use std::path::PathBuf;

mod artifact_lint;
mod artifact_repo_lint;
mod ci_system;
mod command_lint;
mod config;
mod dev_bench;
mod dev_clean;
mod dev_common;
mod dev_gc_perf;
mod dev_loc;
mod dev_studio;
mod dpy_compat;
mod first_party;
mod github_output;
mod glob;
mod lint_policy;
mod lint_system;
mod release_config;
mod release_homebrew;
mod release_system;
mod task_graph;
mod task_planner;
mod task_runner;
mod task_system;
mod test_config;
mod test_manifest;
mod test_plan;
mod test_runner;
mod test_system;
mod tool_lint;
mod tool_system;
mod verify_system;

fn main() {
    if let Err(err) = real_main() {
        eprintln!("vo-dev: {err:#}");
        std::process::exit(1);
    }
}

fn real_main() -> Result<()> {
    let root = repo_root()?;
    let mut args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        print_usage();
        return Ok(());
    }

    match args.remove(0).as_str() {
        "task" => task_system::cmd_task(&root, args),
        "ci" => ci_system::cmd_ci(&root, args),
        "lint" => lint_system::cmd_lint(&root, args),
        "tool" => tool_system::cmd_tool(&root, args),
        "verify" => verify_system::cmd_verify(&root, args),
        "release" => release_system::cmd_release(&root, args),
        "test" => test_system::cmd_test(&root, args),
        "dpy" => dpy_compat::cmd_dpy(&root, args),
        "bench" => dev_bench::cmd_bench(&root, args),
        "gc-perf" => dev_gc_perf::cmd_gc_perf(&root, args),
        "loc" => dev_loc::cmd_loc(&root, args),
        "clean" => dev_clean::cmd_clean(&root, args),
        "studio" => dev_studio::cmd_studio(&root, args, false),
        "studio-native" => dev_studio::cmd_studio(&root, args, true),
        "studio-stop" => dev_studio::cmd_studio_stop(&root),
        "first-party" => first_party::cmd_first_party(&root, args),
        "studio-install-local-vogui" => first_party::cmd_studio_install_local_vogui(&root),
        "-h" | "--help" | "help" => {
            print_usage();
            Ok(())
        }
        other => bail!("unknown command: {other}"),
    }
}

fn print_usage() {
    println!(
        r#"usage:
  vo-dev task list
  vo-dev task show <task>
  vo-dev task stats
  vo-dev task plan <selector> [--changed] [--base <sha>] [--head <sha>] [--format text|json]
  vo-dev task run <selector> [--changed] [--base <sha>] [--head <sha>]
  vo-dev ci matrix <selector> [--base <sha>] [--head <sha>] [--github-output]
  vo-dev ci metadata <selector> [--github-output]
  vo-dev lint tasks|artifacts|repo-boundaries|layout|docs|examples|benchmarks|release|all
  vo-dev tool check [--task <task>] [--json]
  vo-dev tool bootstrap [--task <task>] [--apply] [--json]
  vo-dev tool version <tool>
  vo-dev verify plan|run <selector> [--changed] [--base <sha>] [--head <sha>]
  vo-dev release matrix [--github-output]
  vo-dev release cross-version
  vo-dev release version [--tag <tag>] [--github-output]
  vo-dev release homebrew-repository [--github-output]
  vo-dev release homebrew-metadata [--github-output]
  vo-dev release build <target>
  vo-dev release package <target>
  vo-dev release notes --tag <tag> --out <path>
  vo-dev release publish --tag <tag> --artifacts <dir> --notes <file>
  vo-dev release update-homebrew --repo <path> --artifacts <dir> --version <version>
  vo-dev test plan|run [--suite lang] [--targets <list>] [--matrix <name>] [--tags <list>] [--owner <name>] [--path <file-or-dir>] [--jobs <n>] [--format text|json] [--verbose] [--release]
  vo-dev test run --format json is native-target only; wasm runs are text output
  vo-dev dpy <d.py-compatible command...>
  vo-dev first-party path <repo> [subdir]
  vo-dev first-party ci-checkout <repo> [--github-output]
  vo-dev first-party run <repo> <subdir> -- <command...>
  vo-dev first-party run-workspace <repo> <workspace> -- <command...>
  vo-dev first-party release-verify <repo>
  vo-dev studio-install-local-vogui
  vo-dev test lint --suite lang
  vo-dev test stats --suite lang
  vo-dev test catalog --suite lang [--format text|json]
  vo-dev bench [all|vo|score|<name>] [--all-langs] [--runs N] [--warmup N] [--arch 32|64] [--jit-hot]
  vo-dev gc-perf [--release] [--json] [--objects=N|--small|--large] [dead-sweep|live-chain|root-table|sparse-root-table|interior-root-table]
  vo-dev loc [--with-tests]
  vo-dev clean [all|vo|rust|bench|junk]
  vo-dev studio [--build-wasm] [--build-only] [--runner] [project]
  vo-dev studio-native [--build-wasm] [--runner] [project]
  vo-dev studio-stop"#
    );
}

fn repo_root() -> Result<PathBuf> {
    let mut dir = env::current_dir().context("could not read current directory")?;
    loop {
        if dir.join("Cargo.toml").exists() && dir.join("eng").exists() {
            return Ok(dir);
        }
        if !dir.pop() {
            bail!("could not find volang repo root from current directory");
        }
    }
}
