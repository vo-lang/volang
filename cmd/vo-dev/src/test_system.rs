use crate::test_manifest::{lint_tests, print_test_catalog, print_test_stats};
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
                }
            }
            Ok(())
        }
        "run" => {
            let opts = TestArgs::parse(root, args)?;
            run_tests(root, &opts)
        }
        "lint" => {
            let suite = parse_lint_suite(args)?;
            lint_tests(root, &suite)?;
            println!("vo-dev test lint {}: ok", suite);
            Ok(())
        }
        "stats" => {
            let suite = parse_lint_suite(args)?;
            print_test_stats(root, &suite)
        }
        "catalog" => {
            let opts = parse_catalog_args(args)?;
            print_test_catalog(root, &opts.suite, &opts.format)
        }
        other => bail!("unknown test command: {other}"),
    }
}

struct CatalogArgs {
    suite: String,
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

fn parse_lint_suite(args: Vec<String>) -> Result<String> {
    let mut suite = "lang".to_string();
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
            other => bail!("unknown test lint argument: {other}"),
        }
        i += 1;
    }
    if suite != "lang" {
        bail!("only suite=lang is implemented");
    }
    Ok(suite)
}
