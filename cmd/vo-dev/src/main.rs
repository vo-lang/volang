use anyhow::{bail, Context, Result};
use std::env;
use std::path::PathBuf;

mod artifact_lint;
mod artifact_repo_lint;
mod ci;
mod command_lint;
mod config;
mod dev_bench;
mod dev_clean;
mod dev_common;
mod dev_gc_perf;
mod dev_loc;
mod dpy_compat;
mod first_party;
mod generate_docs;
mod generate_protocol;
mod lint_policy;
mod lint_system;
mod node_audit;
mod release_archive;
mod release_config;
mod release_homebrew;
mod release_identity;
mod release_sdk;
mod release_system;
mod test_config;
mod test_format;
mod test_manifest;
mod test_plan;
mod test_runner;
mod test_system;
mod tool_lint;
mod tool_system;
mod ui_certification;

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
        "lint" => lint_system::cmd_lint(&root, args),
        "generate" => generate_protocol::cmd_generate(&root, args),
        "node-audit" => node_audit::cmd_node_audit(&root, args),
        "tool" => tool_system::cmd_tool(&root, args),
        "release" => release_system::cmd_release(&root, args),
        "ci" => ci::cmd_ci(&root, args),
        "test" => test_system::cmd_test(&root, args),
        "dpy" => dpy_compat::cmd_dpy(&root, args),
        "bench" => dev_bench::cmd_bench(&root, args),
        "gc-perf" => dev_gc_perf::cmd_gc_perf(&root, args),
        "loc" => dev_loc::cmd_loc(&root, args),
        "clean" => dev_clean::cmd_clean(&root, args),
        "first-party" => first_party::cmd_first_party(&root, args),
        "ui-certify" => ui_certification::cmd_ui_certify(&root, args),
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
  vo-dev lint artifacts|repo-boundaries|layout|docs|skill|examples|benchmarks|release|ci|all
  vo-dev generate app-protocol|studio-docs --check|--write
  vo-dev node-audit current|list
  vo-dev tool check [--json]
  vo-dev tool bootstrap [--apply] [--json]
  vo-dev tool version <tool>
  vo-dev release matrix
  vo-dev ci lint
  vo-dev ci plan --profile <name> [--base <rev> --head <rev> | --changed-file <path>...] [--output target/ci/plan.json]
  vo-dev ci explain --base <rev> --head <rev>
  vo-dev ci run --plan <path> --task <id>
  vo-dev ci record --plan <path> --task <id> --output target/ci/evidence/<id>.evidence.json
  vo-dev ci certify --plan <path> --evidence-dir <dir> --output target/ci/certification.json
  vo-dev ci verify --bundle <path> [--profile <name>] [--artifact-task <id> --artifact <path>]
  vo-dev release metadata [--tag <tag>] [--commit <commit>]
  vo-dev release version [--tag <tag>]
  vo-dev release sdk-plan [--check|--json]
  vo-dev release homebrew-repository
  vo-dev release homebrew-metadata
  vo-dev release build-web-runtime
  vo-dev release build <target>
  vo-dev release package <target>
  vo-dev release verify --tag <tag> --artifacts <dir>
  vo-dev release notes --tag <tag> --out <path>
  vo-dev release publish --tag <tag> --artifacts <dir> --notes <file>
  vo-dev release update-homebrew --repo <path> --artifacts <dir> --version <version> [--allow-superseded]
  vo-dev test plan|run [--suite lang] [--targets <list>] [--matrix <name>] [--tags <list>] [--owner <name>] [--path <file-or-dir>] [--shard <index>/<total>] [--jobs <n>] [--repeat <n>] [--format text|json] [--verbose] [--release] [--explain]
  vo-dev dpy <d.py-compatible command...>
  vo-dev first-party path <repo> [subdir]
  vo-dev first-party run <repo> <subdir> -- <command...>
  vo-dev first-party run-workspace <repo> <workspace> -- <command...>
  vo-dev first-party release-verify <repo>
  vo-dev ui-certify --check|--json|--evidence
  vo-dev test lint --suite lang [--strict]
  vo-dev test fmt --suite lang
  vo-dev test stats --suite lang [--format text|json]
  vo-dev test coverage --suite lang [--format text|json]
  vo-dev test explain --suite lang --case <id> [--format text|json]
  vo-dev test catalog --suite lang [--format text|json]
  vo-dev bench [all|vo|score|<name>] [--all-langs] [--runs N] [--warmup N] [--arch 32|64] [--jit-hot]
  vo-dev gc-perf [--release] [--json] [--objects=N|--small|--large] [dead-sweep|live-chain|root-table|sparse-root-table|interior-root-table]
  vo-dev loc [--with-tests]
  vo-dev clean [all|vo|rust|bench|junk]"#
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
