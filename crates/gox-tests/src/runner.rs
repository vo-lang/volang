//! GoX Test Runner
//!
//! Unified test runner for all GoX tests:
//! - Single-file tests (*.gox)
//! - Multi-file tests (directories with main.gox)
//!
//! ## Test Tags
//!
//! Each test file must have at least one tag at the beginning:
//! - `// +vm` - Run in VM mode
//! - `// +native` - Run in Native (JIT) mode  
//! - `// +skip` or `// +skip: reason` - Skip the test
//!
//! Multiple tags can be on the same line: `// +vm +native`
//!
//! ## Test Sections
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
use gox_common::vfs::{FileSet, RealFs};
use gox_syntax::parser;
use gox_analysis::analyze_project;
use gox_module::VfsConfig;

use crate::printer::AstPrinter;

/// Execution mode for running tests.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RunMode {
    /// Run tests using the VM interpreter.
    #[default]
    Vm,
    /// Run tests using the JIT compiler (native code).
    Native,
}

impl std::fmt::Display for RunMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunMode::Vm => write!(f, "vm"),
            RunMode::Native => write!(f, "native"),
        }
    }
}

impl std::str::FromStr for RunMode {
    type Err = String;
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "vm" => Ok(RunMode::Vm),
            "native" | "jit" => Ok(RunMode::Native),
            _ => Err(format!("unknown mode '{}', expected 'vm' or 'native'", s)),
        }
    }
}

/// Result of a single test.
#[derive(Debug)]
pub struct TestResult {
    pub path: String,
    pub passed: bool,
    pub skipped: bool,
    pub error: Option<String>,
}

impl TestResult {
    fn pass(path: impl Into<String>) -> Self {
        Self { path: path.into(), passed: true, skipped: false, error: None }
    }
    
    fn fail(path: impl Into<String>, error: impl Into<String>) -> Self {
        Self { path: path.into(), passed: false, skipped: false, error: Some(error.into()) }
    }
    
    fn skip(path: impl Into<String>, reason: Option<String>) -> Self {
        Self { path: path.into(), passed: true, skipped: true, error: reason }
    }
}

/// Summary of test run.
#[derive(Debug, Default)]
pub struct TestSummary {
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
    pub skipped: usize,
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

/// Parsed tags from a test file.
#[derive(Debug, Default)]
struct TestTags {
    vm: bool,
    native: bool,
    skip: bool,
    skip_reason: Option<String>,
}

impl TestTags {
    /// Check if any tag is present.
    fn has_any(&self) -> bool {
        self.vm || self.native || self.skip
    }
    
    /// Check if the test should run in the given mode.
    fn should_run(&self, mode: RunMode) -> bool {
        if self.skip {
            return false;
        }
        match mode {
            RunMode::Vm => self.vm,
            RunMode::Native => self.native,
        }
    }
}

/// Parse tags from the beginning of a test file.
/// Tags are in the format `// +tag` and must appear before any non-comment, non-empty line.
fn parse_tags(content: &str) -> TestTags {
    let mut tags = TestTags::default();
    
    for line in content.lines() {
        let trimmed = line.trim();
        
        // Stop at first non-comment, non-empty line
        if !trimmed.is_empty() && !trimmed.starts_with("//") {
            break;
        }
        
        // Parse tags from comment lines
        if let Some(comment) = trimmed.strip_prefix("//") {
            let comment = comment.trim();
            
            // Look for +tag patterns
            for part in comment.split_whitespace() {
                if part == "+vm" {
                    tags.vm = true;
                } else if part == "+native" {
                    tags.native = true;
                } else if part == "+skip" {
                    tags.skip = true;
                } else if part.starts_with("+skip:") {
                    tags.skip = true;
                    tags.skip_reason = Some(part.trim_start_matches("+skip:").trim().to_string());
                }
            }
            
            // Also handle "+skip: reason" format (with space after colon)
            if comment.starts_with("+skip:") {
                tags.skip = true;
                let reason = comment.trim_start_matches("+skip:").trim();
                if !reason.is_empty() {
                    tags.skip_reason = Some(reason.to_string());
                }
            }
        }
    }
    
    tags
}

/// Run a single-file test with the default mode (VM).
pub fn run_single_file(path: &Path) -> TestResult {
    run_single_file_with_mode(path, RunMode::Vm)
}

/// Run a single-file test with the specified mode.
pub fn run_single_file_with_mode(path: &Path, mode: RunMode) -> TestResult {
    let path_str = path.display().to_string();
    
    // Read file
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => return TestResult::fail(&path_str, format!("read error: {}", e)),
    };
    
    // Parse and validate tags
    let tags = parse_tags(&content);
    
    if !tags.has_any() {
        return TestResult::fail(&path_str, "test file must have at least one tag: +vm, +native, or +skip");
    }
    
    // Check if should skip
    if tags.skip {
        return TestResult::skip(&path_str, tags.skip_reason);
    }
    
    // Check if should run in this mode
    if !tags.should_run(mode) {
        return TestResult::skip(&path_str, Some(format!("not enabled for {} mode", mode)));
    }
    
    let test = parse_test_file(&content);
    
    // 1. Parse
    let (file, parse_diag, interner) = parser::parse(&test.source, 0);
    
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
    
    // Check if source has imports - if so, skip single-file typecheck
    // and go directly to project compilation which handles imports
    let has_imports = test.source.lines().any(|line| {
        let trimmed = line.trim();
        trimmed.starts_with("import ")
            || trimmed.starts_with("import\"")
            || trimmed.starts_with("import(")
    });
    
    // 2. Type check (only if not parser-only test and no imports)
    if !has_imports {
        let mut typecheck_diag = DiagnosticSink::new();
        let _typecheck_result = gox_analysis::typecheck_file(&file, &interner, &mut typecheck_diag);
        
        // Combine parse errors and typecheck errors for checking
        let mut all_errors = format_diagnostics(&parse_diag);
        let typecheck_errors = format_diagnostics(&typecheck_diag);
        if !all_errors.is_empty() && !typecheck_errors.is_empty() {
            all_errors.push('\n');
        }
        all_errors.push_str(&typecheck_errors);
        
        // Check typecheck section if present
        if let Some(expected) = &test.typecheck {
            let actual = all_errors;
            if expected.trim() == "OK" {
                if parse_diag.has_errors() || typecheck_diag.has_errors() {
                    return TestResult::fail(&path_str, format!(
                        "unexpected error: {}", actual
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
        } else if parse_diag.has_errors() || typecheck_diag.has_errors() {
            return TestResult::fail(&path_str, format!(
                "error: {}", all_errors
            ));
        }
    } else if parse_diag.has_errors() {
        // Still report parse errors even for import tests
        return TestResult::fail(&path_str, format!(
            "parse error: {}", format_diagnostics(&parse_diag)
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
            // Check if we expected a codegen error
            if let Some(expected_codegen) = &test.codegen {
                if expected_codegen.starts_with("ERROR:") {
                    let expected_error = expected_codegen.trim_start_matches("ERROR:").trim();
                    if e.contains(expected_error) {
                        return TestResult::pass(&path_str);
                    } else {
                        return TestResult::fail(&path_str, format!(
                            "codegen error mismatch\nExpected: {}\nActual: {}", expected_error, e
                        ));
                    }
                }
            }
            return TestResult::fail(&path_str, e);
        }
    };
    
    // Check codegen section if present
    if let Some(expected_codegen) = &test.codegen {
        // If expecting an error but compilation succeeded
        if expected_codegen.starts_with("ERROR:") {
            let _ = fs::remove_dir_all(&temp_dir);
            let expected_error = expected_codegen.trim_start_matches("ERROR:").trim();
            return TestResult::fail(&path_str, format!(
                "expected codegen error '{}' but compilation succeeded", expected_error
            ));
        }
        let actual_bytecode = gox_cli::bytecode_text::format_text(&module);
        if !bytecode_matches(&actual_bytecode, expected_codegen) {
            let _ = fs::remove_dir_all(&temp_dir);
            return TestResult::fail(&path_str, format!(
                "codegen mismatch\nExpected:\n{}\nActual:\n{}", expected_codegen, actual_bytecode
            ));
        }
    }
    
    // Run
    let result = run_module(module, mode);
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

/// Run a multi-file test (directory) with the default mode (VM).
pub fn run_multi_file(dir: &Path) -> TestResult {
    run_multi_file_with_mode(dir, RunMode::Vm)
}

/// Run a multi-file test (directory) with the specified mode.
pub fn run_multi_file_with_mode(dir: &Path, mode: RunMode) -> TestResult {
    let path_str = dir.display().to_string();
    
    // Check tags from main.gox
    let main_file = dir.join("main.gox");
    if main_file.exists() {
        if let Ok(content) = fs::read_to_string(&main_file) {
            let tags = parse_tags(&content);
            
            if !tags.has_any() {
                return TestResult::fail(&path_str, "test file must have at least one tag: +vm, +native, or +skip");
            }
            
            if tags.skip {
                return TestResult::skip(&path_str, tags.skip_reason);
            }
            
            if !tags.should_run(mode) {
                return TestResult::skip(&path_str, Some(format!("not enabled for {} mode", mode)));
            }
        }
    }
    
    match compile_and_run(dir, mode) {
        Ok(_) => TestResult::pass(&path_str),
        Err(e) => TestResult::fail(&path_str, e),
    }
}

/// Get the stdlib root path (relative to workspace root).
fn get_stdlib_root() -> PathBuf {
    // Try CARGO_MANIFEST_DIR first (available during cargo test)
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        // manifest_dir is crates/gox-tests, go up to workspace root
        let workspace = PathBuf::from(manifest_dir)
            .parent().unwrap()
            .parent().unwrap()
            .to_path_buf();
        return workspace.join("stdlib");
    }
    
    // Fallback: try GOX_STD environment variable
    if let Ok(std_root) = std::env::var("GOX_STD") {
        return PathBuf::from(std_root);
    }
    
    // Last resort: current directory + stdlib
    PathBuf::from("stdlib")
}

/// Compile a project directory, returns the module.
fn compile_project_dir(dir: &Path) -> Result<gox_vm::Module, String> {
    // Collect source files
    let real_fs = RealFs;
    let file_set = FileSet::collect(&real_fs, dir)
        .map_err(|e| format!("collect error: {}", e))?;
    
    // Analyze with correct stdlib path
    let std_root = get_stdlib_root();
    let mod_root = std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".gox/mod"))
        .unwrap_or_else(|_| dir.join(".gox/mod"));
    
    let vfs_config = VfsConfig::new(std_root, dir.to_path_buf(), mod_root);
    let analysis_vfs = vfs_config.to_vfs();
    let project = analyze_project(file_set, &analysis_vfs)
        .map_err(|e| format!("analysis error: {}", e))?;
    
    // Compile
    gox_codegen_vm::compile_project(&project)
        .map_err(|e| format!("codegen error: {:?}", e))
}

/// Run a compiled module with the specified mode.
fn run_module(module: gox_vm::Module, mode: RunMode) -> Result<(), String> {
    match mode {
        RunMode::Vm => run_module_vm(module),
        RunMode::Native => run_module_native(module),
    }
}

/// Run a compiled module using the VM interpreter.
fn run_module_vm(module: gox_vm::Module) -> Result<(), String> {
    // Create native function registry with stdlib natives
    let mut natives = gox_vm::NativeRegistry::new();
    gox_runtime_vm::stdlib::register_all(&mut natives);
    
    let mut vm = gox_vm::Vm::with_natives(natives);
    vm.load_module(module);
    
    match vm.run() {
        gox_vm::VmResult::Done | gox_vm::VmResult::Ok => Ok(()),
        gox_vm::VmResult::Panic(msg) => Err(msg),
        gox_vm::VmResult::Yield => Err("unexpected yield".to_string()),
    }
}

/// Run a compiled module using the JIT compiler.
fn run_module_native(module: gox_vm::Module) -> Result<(), String> {
    let mut jit = gox_jit::JitCompiler::new()
        .map_err(|e| format!("JIT init error: {}", e))?;
    
    jit.compile_module(&module)
        .map_err(|e| format!("JIT compile error: {}", e))?;
    
    // Get the entry function and call it
    // The entry point is "$entry" which calls $init and main
    let entry_fn: fn() = unsafe {
        jit.get_function("$entry")
            .ok_or_else(|| "entry function not found".to_string())?
    };
    
    entry_fn();
    Ok(())
}

/// Compile and run a project directory with the specified mode.
fn compile_and_run(dir: &Path, mode: RunMode) -> Result<(), String> {
    let module = compile_project_dir(dir)?;
    run_module(module, mode)
}

/// Run all tests in a directory (recursively) with the default mode (VM).
pub fn run_all(test_dir: &Path) -> TestSummary {
    run_all_with_mode(test_dir, RunMode::Vm)
}

/// Run all tests in a directory (recursively) with the specified mode.
pub fn run_all_with_mode(test_dir: &Path, mode: RunMode) -> TestSummary {
    let mut summary = TestSummary::default();
    collect_and_run(test_dir, mode, &mut summary);
    summary
}

fn collect_and_run(dir: &Path, mode: RunMode, summary: &mut TestSummary) {
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
                let result = run_multi_file_with_mode(&path, mode);
                if result.skipped {
                    summary.skipped += 1;
                } else {
                    summary.total += 1;
                    if result.passed {
                        summary.passed += 1;
                    } else {
                        summary.failed += 1;
                        summary.failures.push(result);
                    }
                }
            } else {
                // Recurse into subdirectory
                collect_and_run(&path, mode, summary);
            }
        } else if path.extension().map_or(false, |e| e == "gox") {
            // Single-file test
            let result = run_single_file_with_mode(&path, mode);
            if result.skipped {
                summary.skipped += 1;
            } else {
                summary.total += 1;
                if result.passed {
                    summary.passed += 1;
                } else {
                    summary.failed += 1;
                    summary.failures.push(result);
                }
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
