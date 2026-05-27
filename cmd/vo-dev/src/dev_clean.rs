use crate::dev_common::TARGET_32;
use anyhow::{bail, Context, Result};
use std::fs;
use std::path::Path;
use std::process::Command;

pub(crate) fn cmd_clean(root: &Path, args: Vec<String>) -> Result<()> {
    let target = args.first().map(String::as_str).unwrap_or("all");
    if !matches!(target, "all" | "vo" | "rust" | "bench" | "junk") {
        bail!("usage: vo-dev clean [all|vo|rust|bench|junk]");
    }

    let mut cleaned = Vec::new();
    if matches!(target, "all" | "junk") {
        clean_junk(root, &mut cleaned)?;
    }

    if matches!(target, "all" | "vo") {
        clean_vo_generated(root, &mut cleaned)?;
    }

    if matches!(target, "all" | "bench") {
        clean_bench_generated(root, &mut cleaned)?;
    }

    if matches!(target, "all" | "rust") {
        println!("Running cargo clean...");
        let status = Command::new("cargo")
            .arg("clean")
            .current_dir(root)
            .status()
            .context("could not start cargo clean")?;
        if !status.success() {
            bail!("cargo clean failed");
        }
        cleaned.push("target/".to_string());
    }

    if cleaned.is_empty() {
        println!("Nothing to clean.");
    } else {
        println!("Cleaned:");
        for path in cleaned {
            println!("  - {path}");
        }
    }
    Ok(())
}

fn clean_vo_generated(root: &Path, cleaned: &mut Vec<String>) -> Result<()> {
    remove_dir_if_exists(
        root,
        &root.join("target").join(TARGET_32).join("cmd/vo"),
        cleaned,
    )?;
    remove_file_if_exists(root, &root.join("main.vob"), cleaned)?;
    remove_dir_if_exists(root, &root.join(".volang/cache/vo"), cleaned)?;
    clean_named_dirs_recursive(root, root, ".vo-cache", cleaned)
}

fn clean_bench_generated(root: &Path, cleaned: &mut Vec<String>) -> Result<()> {
    let benchmarks = root.join("benchmarks");
    remove_dir_if_exists(root, &root.join("target/bench"), cleaned)?;
    // Keep legacy cleanup so older benchmark artifacts do not linger after the
    // runner's output moved under target/bench.
    remove_dir_if_exists(root, &benchmarks.join("results"), cleaned)?;
    if !benchmarks.exists() {
        return Ok(());
    }
    clean_named_dirs_recursive(root, &benchmarks, ".vo-cache", cleaned)?;
    clean_benchmark_files_recursive(root, &benchmarks, cleaned)
}

fn clean_junk(root: &Path, cleaned: &mut Vec<String>) -> Result<()> {
    let tmp = root.join(".tmp");
    remove_dir_if_exists(root, &tmp, cleaned)?;
    remove_dir_if_exists(root, &root.join(".volang/tmp"), cleaned)?;
    clean_junk_recursive(root, root, cleaned)
}

fn clean_junk_recursive(root: &Path, dir: &Path, cleaned: &mut Vec<String>) -> Result<()> {
    for entry in fs::read_dir(dir).with_context(|| format!("could not read {}", dir.display()))? {
        let path = entry?.path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default();
        if path.is_dir() {
            if name == "__pycache__" {
                fs::remove_dir_all(&path)
                    .with_context(|| format!("could not remove {}", path.display()))?;
                cleaned.push(path_display(root, &path));
                continue;
            }
            if should_skip_junk_scan_dir(name) {
                continue;
            }
            clean_junk_recursive(root, &path, cleaned)?;
        } else if path.is_file()
            && (name == ".DS_Store" || path.extension().is_some_and(|ext| ext == "pyc"))
        {
            fs::remove_file(&path)
                .with_context(|| format!("could not remove {}", path.display()))?;
            cleaned.push(path_display(root, &path));
        }
    }
    Ok(())
}

fn clean_named_dirs_recursive(
    root: &Path,
    dir: &Path,
    target_name: &str,
    cleaned: &mut Vec<String>,
) -> Result<()> {
    for entry in fs::read_dir(dir).with_context(|| format!("could not read {}", dir.display()))? {
        let path = entry?.path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default();
        if !path.is_dir() {
            continue;
        }
        if name == target_name {
            remove_dir_if_exists(root, &path, cleaned)?;
            continue;
        }
        if should_skip_clean_scan_dir(name) {
            continue;
        }
        clean_named_dirs_recursive(root, &path, target_name, cleaned)?;
    }
    Ok(())
}

fn clean_benchmark_files_recursive(
    root: &Path,
    dir: &Path,
    cleaned: &mut Vec<String>,
) -> Result<()> {
    for entry in fs::read_dir(dir).with_context(|| format!("could not read {}", dir.display()))? {
        let path = entry?.path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default();
        if path.is_dir() {
            if should_skip_clean_scan_dir(name) {
                continue;
            }
            clean_benchmark_files_recursive(root, &path, cleaned)?;
        } else if path.is_file()
            && (matches!(name, "go_bench" | "c_bench")
                || path.extension().is_some_and(|ext| ext == "class"))
        {
            remove_file_if_exists(root, &path, cleaned)?;
        }
    }
    Ok(())
}

fn should_skip_junk_scan_dir(name: &str) -> bool {
    should_skip_clean_scan_dir(name)
}

fn should_skip_clean_scan_dir(name: &str) -> bool {
    matches!(
        name,
        ".git" | ".volang" | "target" | "node_modules" | "dist" | "pkg" | ".vo-cache"
    )
}

fn remove_dir_if_exists(root: &Path, path: &Path, cleaned: &mut Vec<String>) -> Result<()> {
    if path.exists() {
        fs::remove_dir_all(path).with_context(|| format!("could not remove {}", path.display()))?;
        cleaned.push(path_display(root, path));
    }
    Ok(())
}

fn remove_file_if_exists(root: &Path, path: &Path, cleaned: &mut Vec<String>) -> Result<()> {
    if path.exists() {
        fs::remove_file(path).with_context(|| format!("could not remove {}", path.display()))?;
        cleaned.push(path_display(root, path));
    }
    Ok(())
}

fn path_display(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .to_string()
}
