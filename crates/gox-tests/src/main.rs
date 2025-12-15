//! GoX Test Runner CLI
//!
//! Usage:
//!   gox-tests                    # Run all tests in test_data/
//!   gox-tests <path>             # Run tests at path (file or directory)
//!   gox-tests --codegen <file>   # Output bytecode for a .gox file

use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

use gox_tests::{run_all, run_single_file};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    
    // Check for --codegen flag
    if args.len() >= 3 && args[1] == "--codegen" {
        return cmd_codegen(&args[2]);
    }
    
    let path = if args.len() > 1 {
        Path::new(&args[1]).to_path_buf()
    } else {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data")
    };
    
    if !path.exists() {
        eprintln!("error: path does not exist: {}", path.display());
        return ExitCode::FAILURE;
    }
    
    println!("Running tests from {}...\n", path.display());
    
    let summary = if path.is_file() {
        let result = run_single_file(&path);
        let mut s = gox_tests::TestSummary::default();
        s.total = 1;
        if result.passed {
            s.passed = 1;
            println!("  ✓ {}", result.path);
        } else {
            s.failed = 1;
            println!("  ✗ {}", result.path);
            if let Some(err) = &result.error {
                for line in err.lines().take(10) {
                    println!("    {}", line);
                }
            }
            s.failures.push(result);
        }
        s
    } else {
        let summary = run_all(&path);
        
        // Print failures
        for failure in &summary.failures {
            println!("  ✗ {}", failure.path);
            if let Some(err) = &failure.error {
                for line in err.lines().take(5) {
                    println!("    {}", line);
                }
            }
        }
        
        summary
    };
    
    println!("\nResults: {} passed, {} failed", summary.passed, summary.failed);
    
    if summary.success() {
        println!("OK");
        ExitCode::SUCCESS
    } else {
        println!("FAILED");
        ExitCode::FAILURE
    }
}

/// Output bytecode for a .gox file
fn cmd_codegen(file_path: &str) -> ExitCode {
    use gox_common::vfs::{FileSet, RealFs};
    use gox_analysis::analyze_project;
    use gox_module::VfsConfig;
    
    let path = Path::new(file_path);
    if !path.exists() {
        eprintln!("error: file does not exist: {}", file_path);
        return ExitCode::FAILURE;
    }
    
    // Read source and extract code before any === section
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("error: failed to read file: {}", e);
            return ExitCode::FAILURE;
        }
    };
    
    let source = content
        .lines()
        .take_while(|l| !l.trim().starts_with("==="))
        .collect::<Vec<_>>()
        .join("\n");
    
    // Create temp dir and compile
    let temp_dir = std::env::temp_dir().join("gox_codegen_tmp");
    let _ = fs::remove_dir_all(&temp_dir);
    if let Err(e) = fs::create_dir_all(&temp_dir) {
        eprintln!("error: failed to create temp dir: {}", e);
        return ExitCode::FAILURE;
    }
    
    if let Err(e) = fs::write(temp_dir.join("main.gox"), &source) {
        eprintln!("error: failed to write temp file: {}", e);
        let _ = fs::remove_dir_all(&temp_dir);
        return ExitCode::FAILURE;
    }
    
    let real_fs = RealFs;
    let file_set = match FileSet::collect(&real_fs, &temp_dir) {
        Ok(fs) => fs,
        Err(e) => {
            eprintln!("error: failed to collect files: {}", e);
            let _ = fs::remove_dir_all(&temp_dir);
            return ExitCode::FAILURE;
        }
    };
    
    let vfs_config = VfsConfig::from_env(temp_dir.clone());
    let analysis_vfs = vfs_config.to_vfs();
    let project = match analyze_project(file_set, &analysis_vfs) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: analysis failed: {}", e);
            let _ = fs::remove_dir_all(&temp_dir);
            return ExitCode::FAILURE;
        }
    };
    
    let module = match gox_codegen_vm::compile_project(&project) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: codegen failed: {:?}", e);
            let _ = fs::remove_dir_all(&temp_dir);
            return ExitCode::FAILURE;
        }
    };
    
    let _ = fs::remove_dir_all(&temp_dir);
    
    // Output bytecode
    let bytecode = gox_cli::bytecode_text::format_text(&module);
    println!("=== codegen ===");
    print!("{}", bytecode);
    
    ExitCode::SUCCESS
}
