//! GoX Test Runner
//!
//! Unified test runner for all GoX tests:
//! - Single-file tests (*.gox)
//! - Multi-file tests (directories with main.gox)
//!
//! Test files can have optional sections:
//! - `=== parser ===` - Expected AST output
//! - `=== typecheck ===` - Expected type errors or "OK"
//! - `=== codegen ===` - Expected bytecode text
//!
//! All tests must run successfully. Use `assert()` to verify correctness.

use std::fs;
use std::path::{Path, PathBuf};

use gox_common::diagnostics::DiagnosticSink;
use gox_common::source::FileId;
use gox_common::vfs::{FileSet, RealFs};
use gox_syntax::parser;
use gox_analysis::analyze_project;
use gox_module::VfsConfig;

use crate::printer::AstPrinter;

/// Result of a single test.
#[derive(Debug)]
pub struct TestResult {
    pub path: String,
    pub passed: bool,
    pub error: Option<String>,
}

impl TestResult {
    fn pass(path: impl Into<String>) -> Self {
        Self { path: path.into(), passed: true, error: None }
    }
    
    fn fail(path: impl Into<String>, error: impl Into<String>) -> Self {
        Self { path: path.into(), passed: false, error: Some(error.into()) }
    }
}

/// Summary of test run.
#[derive(Debug, Default)]
pub struct TestSummary {
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
    pub failures: Vec<TestResult>,
}

impl TestSummary {
    pub fn success(&self) -> bool {
        self.failed == 0
    }
}

/// Parsed sections from a test file.
#[derive(Debug, Default)]
struct TestFile {
    source: String,
    parser: Option<String>,
    typecheck: Option<String>,
    codegen: Option<String>,
    vm: Option<String>,
}

/// Parse test file into source and optional sections.
fn parse_test_file(content: &str) -> TestFile {
    let mut result = TestFile::default();
    let mut current_section = "source";
    let mut current_content = String::new();
    
    for line in content.lines() {
        let trimmed = line.trim();
        
        if trimmed.starts_with("=== ") && trimmed.ends_with(" ===") {
            // Save previous section
            let text = current_content.trim().to_string();
            match current_section {
                "source" => result.source = text,
                "parser" => result.parser = Some(text),
                "typecheck" => result.typecheck = Some(text),
                "codegen" => result.codegen = Some(text),
                "vm" => result.vm = Some(text),
                _ => {}
            }
            current_content.clear();
            current_section = trimmed.trim_start_matches("=== ").trim_end_matches(" ===");
        } else {
            current_content.push_str(line);
            current_content.push('\n');
        }
    }
    
    // Save last section
    let text = current_content.trim().to_string();
    match current_section {
        "source" => result.source = text,
        "parser" => result.parser = Some(text),
        "typecheck" => result.typecheck = Some(text),
        "codegen" => result.codegen = Some(text),
        "vm" => result.vm = Some(text),
        _ => {}
    }
    
    result
}

/// Run a single-file test.
pub fn run_single_file(path: &Path) -> TestResult {
    let path_str = path.display().to_string();
    
    // Read file
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => return TestResult::fail(&path_str, format!("read error: {}", e)),
    };
    
    let test = parse_test_file(&content);
    
    // 1. Parse
    let (file, parse_diag, interner) = parser::parse(FileId::new(0), &test.source);
    
    // Check parser section if present
    if let Some(expected) = &test.parser {
        let mut printer = AstPrinter::new(&interner);
        let actual = printer.print_file(&file);
        if !ast_matches(&actual, expected) {
            return TestResult::fail(&path_str, format!(
                "parser mismatch\nExpected:\n{}\nActual:\n{}", expected, actual
            ));
        }
    }
    
    // Check for parse errors
    if parse_diag.has_errors() {
        // If no typecheck section and no parser section, this is unexpected
        if test.typecheck.is_none() && test.parser.is_none() {
            return TestResult::fail(&path_str, format!(
                "parse error: {}", format_diagnostics(&parse_diag)
            ));
        }
        // If has parser section, we already verified AST above
        // Parser-only tests don't need to pass type checking
    }
    
    // If test only has parser section (no typecheck/codegen), we're done
    if test.parser.is_some() && test.typecheck.is_none() && test.codegen.is_none() {
        return TestResult::pass(&path_str);
    }
    
    // 2. Type check (only if not parser-only test)
    let mut typecheck_diag = DiagnosticSink::new();
    let _typecheck_result = gox_analysis::typecheck_file(&file, &interner, &mut typecheck_diag);
    
    // Check typecheck section if present
    if let Some(expected) = &test.typecheck {
        let actual = format_diagnostics(&typecheck_diag);
        if expected.trim() == "OK" {
            if typecheck_diag.has_errors() {
                return TestResult::fail(&path_str, format!(
                    "unexpected type error: {}", actual
                ));
            }
        } else {
            if !errors_match(&actual, expected) {
                return TestResult::fail(&path_str, format!(
                    "typecheck mismatch\nExpected:\n{}\nActual:\n{}", expected, actual
                ));
            }
            // If expecting errors, don't try to run
            return TestResult::pass(&path_str);
        }
        // Typecheck-only test done
        return TestResult::pass(&path_str);
    } else if typecheck_diag.has_errors() {
        return TestResult::fail(&path_str, format!(
            "type error: {}", format_diagnostics(&typecheck_diag)
        ));
    }
    
    // 3. Compile & Run (using temp directory)
    let name = path.file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "test".to_string());
    
    let temp_dir = std::env::temp_dir().join(format!("gox_test_{}", name));
    let _ = fs::remove_dir_all(&temp_dir);
    if let Err(e) = fs::create_dir_all(&temp_dir) {
        return TestResult::fail(&path_str, format!("temp dir error: {}", e));
    }
    
    // Write source to temp dir
    if let Err(e) = fs::write(temp_dir.join("main.gox"), &test.source) {
        let _ = fs::remove_dir_all(&temp_dir);
        return TestResult::fail(&path_str, format!("write error: {}", e));
    }
    
    // Compile
    let module = match compile_project_dir(&temp_dir) {
        Ok(m) => m,
        Err(e) => {
            let _ = fs::remove_dir_all(&temp_dir);
            return TestResult::fail(&path_str, e);
        }
    };
    
    // Check codegen section if present
    if let Some(expected_codegen) = &test.codegen {
        let actual_bytecode = gox_cli::bytecode_text::format_text(&module);
        if !bytecode_matches(&actual_bytecode, expected_codegen) {
            let _ = fs::remove_dir_all(&temp_dir);
            return TestResult::fail(&path_str, format!(
                "codegen mismatch\nExpected:\n{}\nActual:\n{}", expected_codegen, actual_bytecode
            ));
        }
    }
    
    // Run
    let result = run_module(module);
    let _ = fs::remove_dir_all(&temp_dir);
    
    // Check vm section for expected VM errors (by error code)
    if let Some(expected_vm) = &test.vm {
        match result {
            Ok(_) => TestResult::fail(&path_str, format!(
                "expected VM error: {}", expected_vm
            )),
            Err(actual) => {
                if vm_error_matches(&actual, expected_vm) {
                    TestResult::pass(&path_str)
                } else {
                    TestResult::fail(&path_str, format!(
                        "vm error mismatch\nExpected: {}\nActual: {}", expected_vm, actual
                    ))
                }
            }
        }
    } else {
        match result {
            Ok(_) => TestResult::pass(&path_str),
            Err(e) => TestResult::fail(&path_str, e),
        }
    }
}

/// Run a multi-file test (directory).
pub fn run_multi_file(dir: &Path) -> TestResult {
    let path_str = dir.display().to_string();
    
    match compile_and_run(dir) {
        Ok(_) => TestResult::pass(&path_str),
        Err(e) => TestResult::fail(&path_str, e),
    }
}

/// Compile a project directory, returns the module.
fn compile_project_dir(dir: &Path) -> Result<gox_vm::Module, String> {
    // Collect source files
    let real_fs = RealFs;
    let file_set = FileSet::collect(&real_fs, dir)
        .map_err(|e| format!("collect error: {}", e))?;
    
    // Analyze
    let vfs_config = VfsConfig::from_env(dir.to_path_buf());
    let analysis_vfs = vfs_config.to_vfs();
    let project = analyze_project(file_set, &analysis_vfs)
        .map_err(|e| format!("analysis error: {}", e))?;
    
    // Compile
    gox_codegen_vm::compile_project(&project)
        .map_err(|e| format!("codegen error: {:?}", e))
}

/// Run a compiled module.
fn run_module(module: gox_vm::Module) -> Result<(), String> {
    let mut vm = gox_vm::Vm::new();
    vm.load_module(module);
    
    match vm.run() {
        gox_vm::VmResult::Done | gox_vm::VmResult::Ok => Ok(()),
        gox_vm::VmResult::Panic(msg) => Err(msg),
        gox_vm::VmResult::Yield => Err("unexpected yield".to_string()),
    }
}

/// Compile and run a project directory.
fn compile_and_run(dir: &Path) -> Result<(), String> {
    let module = compile_project_dir(dir)?;
    run_module(module)
}

/// Run all tests in a directory (recursively).
pub fn run_all(test_dir: &Path) -> TestSummary {
    let mut summary = TestSummary::default();
    collect_and_run(test_dir, &mut summary);
    summary
}

fn collect_and_run(dir: &Path, summary: &mut TestSummary) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    
    for entry in entries.flatten() {
        let path = entry.path();
        
        if path.is_dir() {
            // Check if it's a multi-file test (starts with "proj_")
            let dir_name = path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("");
            
            if dir_name.starts_with("proj_") {
                summary.total += 1;
                let result = run_multi_file(&path);
                if result.passed {
                    summary.passed += 1;
                } else {
                    summary.failed += 1;
                    summary.failures.push(result);
                }
            } else {
                // Recurse into subdirectory
                collect_and_run(&path, summary);
            }
        } else if path.extension().map_or(false, |e| e == "gox") {
            // Single-file test
            summary.total += 1;
            let result = run_single_file(&path);
            if result.passed {
                summary.passed += 1;
            } else {
                summary.failed += 1;
                summary.failures.push(result);
            }
        }
    }
}

/// Format diagnostics for comparison.
fn format_diagnostics(diag: &DiagnosticSink) -> String {
    diag.iter()
        .filter_map(|d| d.code.map(|c| format!("[E{:04}] {}", c, d.message)))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Check if actual errors match expected.
fn errors_match(actual: &str, expected: &str) -> bool {
    let actual: Vec<&str> = actual.lines().map(|l| l.trim()).filter(|l| !l.is_empty()).collect();
    let expected: Vec<&str> = expected.lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty() && !l.starts_with("//"))
        .collect();
    
    if actual.len() != expected.len() {
        return false;
    }
    
    actual.iter().zip(expected.iter()).all(|(a, e)| error_matches(a, e))
}

fn error_matches(actual: &str, expected: &str) -> bool {
    // Match by error code: "E2100" or "[E2100]"
    let code = expected.trim_start_matches('[').trim_end_matches(']');
    if code.starts_with('E') && code[1..].chars().all(|c| c.is_ascii_digit()) {
        return actual.contains(&format!("[{}]", code));
    }
    
    // Match "[Exxxx] message"
    if expected.starts_with("[E") {
        if let Some(i) = expected.find(']') {
            let code = &expected[1..i];
            if !actual.contains(&format!("[{}]", code)) {
                return false;
            }
            let msg = expected[i + 1..].trim();
            return msg.is_empty() || actual.contains(msg);
        }
    }
    
    actual.contains(expected)
}

/// Check if AST output matches.
fn ast_matches(actual: &str, expected: &str) -> bool {
    let norm = |s: &str| s.lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty() && !l.starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n");
    
    norm(actual) == norm(expected)
}

/// Check if bytecode output matches expected.
/// Ignores comments (lines starting with #) and normalizes whitespace.
fn bytecode_matches(actual: &str, expected: &str) -> bool {
    let norm = |s: &str| -> String {
        s.lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty() && !l.starts_with('#') && !l.starts_with("//"))
            .collect::<Vec<_>>()
            .join("\n")
    };
    
    norm(actual) == norm(expected)
}

/// Check if VM error matches expected (by error code).
/// Expected format: "[V1001] message" or just "[V1001]"
fn vm_error_matches(actual: &str, expected: &str) -> bool {
    let expected = expected.trim();
    
    // Extract error code from expected: [Vxxxx]
    if expected.starts_with("[V") {
        if let Some(i) = expected.find(']') {
            let code = &expected[1..i];
            // Check if actual contains the error code
            if actual.contains(&format!("[{}]", code)) {
                // If there's a message part, check it too
                let msg = expected[i + 1..].trim();
                return msg.is_empty() || actual.contains(msg);
            }
            return false;
        }
    }
    
    // Fallback: substring match
    actual.contains(expected)
}
