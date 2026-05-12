use crate::dev_common::walk_files;
use anyhow::{bail, Context, Result};
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

pub(crate) fn cmd_loc(root: &Path, args: Vec<String>) -> Result<()> {
    let mut with_tests = false;
    for arg in args {
        match arg.as_str() {
            "--with-tests" => with_tests = true,
            other => bail!("unknown loc argument: {other}"),
        }
    }

    let crates_dir = root.join("lang/crates");
    let mut crate_stats = BTreeMap::new();
    let mut total_lines = 0usize;
    let mut total_files = 0usize;

    println!("Vo Project Statistics");
    println!();
    println!(
        "{:<30} | {:>7} | {:>9} | {:>8}",
        "Crate", "Files", "Lines", "Avg/File"
    );
    println!("{}", "-".repeat(64));

    let mut crate_dirs = Vec::new();
    for entry in fs::read_dir(&crates_dir)
        .with_context(|| format!("could not read {}", crates_dir.display()))?
    {
        crate_dirs.push(entry?.path());
    }
    crate_dirs.sort();

    for path in crate_dirs {
        let src = path.join("src");
        if !src.is_dir() {
            continue;
        }
        let name = path.file_name().unwrap().to_string_lossy().to_string();
        if !with_tests && name == "vo-tests" {
            continue;
        }
        let (lines, files) = count_rs_lines(&src, with_tests)?;
        let avg = if files == 0 { 0 } else { lines / files };
        println!("{name:<30} | {files:>7} | {lines:>9} | {avg:>8}");
        crate_stats.insert(name, lines);
        total_lines += lines;
        total_files += files;
    }

    println!("{}", "-".repeat(64));
    let total_avg = if total_files == 0 {
        0
    } else {
        total_lines / total_files
    };
    println!(
        "{:<30} | {:>7} | {:>9} | {:>8}",
        "TOTAL", total_files, total_lines, total_avg
    );
    println!();

    if !with_tests {
        print_test_stats(root, &crates_dir)?;
    }
    print_categories(&crate_stats);
    if !with_tests {
        println!("Note: Run with --with-tests to include test files");
    }
    Ok(())
}

fn count_rs_lines(dir: &Path, with_tests: bool) -> Result<(usize, usize)> {
    let mut lines = 0;
    let mut files = 0;
    for path in walk_files(dir)? {
        if path.extension().and_then(|ext| ext.to_str()) != Some("rs") {
            continue;
        }
        if !with_tests
            && path
                .file_name()
                .is_some_and(|name| name.to_string_lossy().ends_with("_test.rs"))
        {
            continue;
        }
        files += 1;
        lines += fs::read_to_string(&path)
            .unwrap_or_default()
            .lines()
            .count();
    }
    Ok((lines, files))
}

fn print_test_stats(root: &Path, crates_dir: &Path) -> Result<()> {
    println!("Test Statistics (excluded from above):");
    let test_data = root.join("tests/lang");
    if test_data.exists() {
        let mut files = 0;
        let mut lines = 0;
        for path in walk_files(&test_data)? {
            if path.extension().and_then(|ext| ext.to_str()) == Some("vo") {
                files += 1;
                lines += fs::read_to_string(&path)
                    .unwrap_or_default()
                    .lines()
                    .count();
            }
        }
        println!("  Test data files (.vo):     {files} files, {lines} lines");
    }
    let vo_tests = crates_dir.join("vo-tests/src");
    if vo_tests.exists() {
        let (lines, _) = count_rs_lines(&vo_tests, true)?;
        println!("  Test runner (Rust):         {lines} lines");
    }
    println!();
    Ok(())
}

fn print_categories(stats: &BTreeMap<String, usize>) {
    let categories: [(&str, &[&str]); 7] = [
        ("Frontend (syntax)", &["vo-syntax"]),
        ("Common", &["vo-common", "vo-common-core"]),
        ("Analysis (type checker)", &["vo-analysis"]),
        ("Code Generation", &["vo-codegen"]),
        ("Runtime", &["vo-vm", "vo-runtime"]),
        ("JIT", &["vo-jit"]),
        ("Tools (CLI/Module)", &["vo", "vo-module"]),
    ];
    println!("Category Breakdown:");
    for (name, crates) in categories {
        let lines: usize = crates.iter().filter_map(|krate| stats.get(*krate)).sum();
        println!("  {name:<25} {lines:>8} lines");
    }
    println!();
}
