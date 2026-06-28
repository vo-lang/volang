use crate::test_manifest::{
    explain_test_case, lint_tests, print_test_catalog, print_test_coverage, print_test_stats,
};
use crate::test_plan::{build_plan, TestArgs};
use crate::test_runner::run_tests;
use anyhow::{anyhow, bail, Result};
use std::path::Path;

pub(crate) fn cmd_test(root: &Path, mut args: Vec<String>) -> Result<()> {
    if args.is_empty() {
        bail!("usage: vo-dev test plan|run|lint|stats|catalog ...");
    }
    match args.remove(0).as_str() {
        "plan" => {
            let opts = TestArgs::parse(root, args)?;
            let plan = build_plan(root, &opts)?;
            if opts.format == "json" {
                println!("{}", serde_json::to_string_pretty(&plan)?);
            } else {
                for job in &plan.jobs {
                    println!("{}\t{}\t{}", job.id, job.target, job.path);
                    if opts.explain {
                        for reason in &job.selection_reasons {
                            println!("  - {reason}");
                        }
                    }
                }
            }
            Ok(())
        }
        "run" => {
            let opts = TestArgs::parse(root, args)?;
            run_tests(root, &opts)
        }
        "lint" => {
            let opts = parse_suite_format_args("test lint", args, false)?;
            lint_tests(root, &opts.suite, opts.strict)?;
            println!("vo-dev test lint {}: ok", opts.suite);
            Ok(())
        }
        "stats" => {
            let opts = parse_suite_format_args("test stats", args, true)?;
            print_test_stats(root, &opts.suite, &opts.format)
        }
        "coverage" => {
            let opts = parse_suite_format_args("test coverage", args, true)?;
            print_test_coverage(root, &opts.suite, &opts.format)
        }
        "catalog" => {
            let opts = parse_catalog_args(args)?;
            print_test_catalog(root, &opts.suite, &opts.format)
        }
        "explain" => {
            let opts = parse_explain_args(args)?;
            explain_test_case(root, &opts.suite, &opts.case_id, &opts.format)
        }
        other => bail!("unknown test command: {other}"),
    }
}

struct CatalogArgs {
    suite: String,
    format: String,
}

struct SuiteFormatArgs {
    suite: String,
    format: String,
    strict: bool,
}

struct ExplainArgs {
    suite: String,
    case_id: String,
    format: String,
}

fn parse_catalog_args(args: Vec<String>) -> Result<CatalogArgs> {
    let mut suite = "lang".to_string();
    let mut format = "text".to_string();
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--suite" => {
                i += 1;
                suite = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--suite requires a value"))?
                    .clone();
            }
            arg if arg.starts_with("--suite=") => {
                suite = arg["--suite=".len()..].to_string();
            }
            "--format" => {
                i += 1;
                format = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--format requires a value"))?
                    .clone();
            }
            arg if arg.starts_with("--format=") => {
                format = arg["--format=".len()..].to_string();
            }
            other => bail!("unknown test catalog argument: {other}"),
        }
        i += 1;
    }
    if suite != "lang" {
        bail!("only suite=lang is implemented");
    }
    if format != "text" && format != "json" {
        bail!("--format must be text or json");
    }
    Ok(CatalogArgs { suite, format })
}

fn parse_suite_format_args(
    command: &str,
    args: Vec<String>,
    allow_format: bool,
) -> Result<SuiteFormatArgs> {
    let mut suite = "lang".to_string();
    let mut format = "text".to_string();
    let mut strict = false;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--strict" => strict = true,
            "--suite" => {
                i += 1;
                suite = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--suite requires a value"))?
                    .clone();
            }
            arg if arg.starts_with("--suite=") => {
                suite = arg["--suite=".len()..].to_string();
            }
            "--format" if allow_format => {
                i += 1;
                format = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--format requires a value"))?
                    .clone();
            }
            arg if allow_format && arg.starts_with("--format=") => {
                format = arg["--format=".len()..].to_string();
            }
            other => bail!("unknown {command} argument: {other}"),
        }
        i += 1;
    }
    if suite != "lang" {
        bail!("only suite=lang is implemented");
    }
    if format != "text" && format != "json" {
        bail!("--format must be text or json");
    }
    Ok(SuiteFormatArgs {
        suite,
        format,
        strict,
    })
}

fn parse_explain_args(args: Vec<String>) -> Result<ExplainArgs> {
    let mut suite = "lang".to_string();
    let mut format = "text".to_string();
    let mut case_id = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--suite" => {
                i += 1;
                suite = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--suite requires a value"))?
                    .clone();
            }
            arg if arg.starts_with("--suite=") => suite = arg["--suite=".len()..].to_string(),
            "--case" => {
                i += 1;
                case_id = Some(
                    args.get(i)
                        .ok_or_else(|| anyhow!("--case requires a value"))?
                        .clone(),
                );
            }
            arg if arg.starts_with("--case=") => {
                case_id = Some(arg["--case=".len()..].to_string());
            }
            "--format" => {
                i += 1;
                format = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--format requires a value"))?
                    .clone();
            }
            arg if arg.starts_with("--format=") => format = arg["--format=".len()..].to_string(),
            other => bail!("unknown test explain argument: {other}"),
        }
        i += 1;
    }
    if suite != "lang" {
        bail!("only suite=lang is implemented");
    }
    if format != "text" && format != "json" {
        bail!("--format must be text or json");
    }
    Ok(ExplainArgs {
        suite,
        format,
        case_id: case_id.ok_or_else(|| anyhow!("test explain requires --case <id>"))?,
    })
}
