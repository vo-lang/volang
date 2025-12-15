//! Test runner for file-based tests.

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use gox_common::diagnostics::DiagnosticSink;
use gox_common::source::FileId;
use gox_syntax::parser;

use crate::printer::AstPrinter;

/// Virtual file system for testing single files without disk I/O.
#[derive(Debug, Clone, Default)]
pub struct VirtualFs {
    files: HashMap<String, String>,
}

impl VirtualFs {
    pub fn new() -> Self {
        Self { files: HashMap::new() }
    }
    
    /// Add a file to the virtual filesystem.
    pub fn add_file(&mut self, path: impl Into<String>, content: impl Into<String>) {
        self.files.insert(path.into(), content.into());
    }
    
    /// Get file content by path.
    pub fn get(&self, path: &str) -> Option<&str> {
        self.files.get(path).map(|s| s.as_str())
    }
    
    /// Check if a file exists.
    pub fn exists(&self, path: &str) -> bool {
        self.files.contains_key(path)
    }
    
    /// List all files.
    pub fn files(&self) -> impl Iterator<Item = (&str, &str)> {
        self.files.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }
}

/// Result of running a single source test.
#[derive(Debug)]
pub struct CodegenTestResult {
    pub name: String,
    pub passed: bool,
    pub output: String,
    pub error: Option<String>,
}

/// Run a single .gox source from string (for codegen tests).
/// Returns the test result with output capture.
pub fn run_source(name: &str, source: &str) -> CodegenTestResult {
    // Create virtual filesystem with the source
    let mut vfs = VirtualFs::new();
    vfs.add_file("main.gox", source);
    
    run_source_with_vfs(name, &vfs)
}

/// Run source from virtual filesystem.
pub fn run_source_with_vfs(name: &str, vfs: &VirtualFs) -> CodegenTestResult {
    // Get main.gox content
    let source = match vfs.get("main.gox") {
        Some(s) => s,
        None => {
            return CodegenTestResult {
                name: name.to_string(),
                passed: false,
                output: String::new(),
                error: Some("main.gox not found in virtual filesystem".to_string()),
            };
        }
    };
    
    // Parse
    let (file, parse_diag, interner) = parser::parse(FileId::new(0), source);
    if parse_diag.has_errors() {
        return CodegenTestResult {
            name: name.to_string(),
            passed: false,
            output: String::new(),
            error: Some(format!("Parse error: {}", format_diagnostics(&parse_diag))),
        };
    }
    
    // Type check
    let mut typecheck_diag = DiagnosticSink::new();
    let typecheck_result = gox_analysis::typecheck_file(&file, &interner, &mut typecheck_diag);
    if typecheck_diag.has_errors() {
        return CodegenTestResult {
            name: name.to_string(),
            passed: false,
            output: String::new(),
            error: Some(format!("Type error: {}", format_diagnostics(&typecheck_diag))),
        };
    }
    
    // Compile single file
    let module = match gox_codegen_vm::compile(&file, &typecheck_result, &interner) {
        Ok(m) => m,
        Err(e) => {
            return CodegenTestResult {
                name: name.to_string(),
                passed: false,
                output: String::new(),
                error: Some(format!("Codegen error: {:?}", e)),
            };
        }
    };
    
    // Run
    let mut vm = gox_vm::Vm::new();
    vm.load_module(module);
    
    match vm.run() {
        gox_vm::VmResult::Done | gox_vm::VmResult::Ok => {
            CodegenTestResult {
                name: name.to_string(),
                passed: true,
                output: String::new(),
                error: None,
            }
        }
        gox_vm::VmResult::Panic(msg) => {
            CodegenTestResult {
                name: name.to_string(),
                passed: false,
                output: String::new(),
                error: Some(msg),
            }
        }
        gox_vm::VmResult::Yield => {
            CodegenTestResult {
                name: name.to_string(),
                passed: false,
                output: String::new(),
                error: Some("Unexpected yield".to_string()),
            }
        }
    }
}

/// Result of running a single test.
#[derive(Debug)]
pub struct TestResult {
    pub file_name: String,
    pub passed: bool,
    pub message: String,
    pub expected: String,
    pub actual: String,
}

/// Test runner for file-based tests.
pub struct TestRunner {
    test_dir: std::path::PathBuf,
}

impl TestRunner {
    pub fn new(test_dir: impl AsRef<Path>) -> Self {
        Self {
            test_dir: test_dir.as_ref().to_path_buf(),
        }
    }

    pub fn run_all(&self) -> Vec<TestResult> {
        let mut results = Vec::new();
        
        if let Ok(entries) = fs::read_dir(&self.test_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().map_or(false, |e| e == "gox") {
                    results.push(run_test_file(&path));
                }
            }
        }
        
        results
    }
}

/// Runs all tests in a directory.
pub fn run_all_tests(test_dir: &Path) -> Vec<TestResult> {
    TestRunner::new(test_dir).run_all()
}

/// Runs a single test file.
pub fn run_test_file(path: &Path) -> TestResult {
    let file_name = path.file_name()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "unknown".to_string());
    
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            return TestResult {
                file_name,
                passed: false,
                message: format!("Failed to read file: {}", e),
                expected: String::new(),
                actual: String::new(),
            };
        }
    };
    
    let sections = parse_test_file(&content);
    
    // Run parser and compare output
    if let Some(expected_parser) = sections.parser {
        let (file, diagnostics, interner) = parser::parse(FileId::new(0), &sections.source);
        
        // Check for expected errors
        if let Some(expected_errors) = &sections.errors {
            let actual_errors = format_diagnostics(&diagnostics);
            if !errors_match(&actual_errors, expected_errors) {
                return TestResult {
                    file_name,
                    passed: false,
                    message: "Error messages don't match".to_string(),
                    expected: expected_errors.clone(),
                    actual: actual_errors,
                };
            }
        }
        
        // Compare AST output
        let mut printer = AstPrinter::new(&interner);
        let actual_ast = printer.print_file(&file);
        
        if !ast_matches(&actual_ast, &expected_parser) {
            return TestResult {
                file_name,
                passed: false,
                message: "Parser output doesn't match".to_string(),
                expected: expected_parser,
                actual: actual_ast,
            };
        }
    }
    
    // Check for unexpected errors
    if sections.errors.is_none() && sections.typecheck.is_none() {
        let (_, diagnostics, _) = parser::parse(FileId::new(0), &sections.source);
        if diagnostics.has_errors() {
            let actual_errors = format_diagnostics(&diagnostics);
            return TestResult {
                file_name,
                passed: false,
                message: "Unexpected parse errors".to_string(),
                expected: String::new(),
                actual: actual_errors,
            };
        }
    }
    
    // Run type checker if typecheck section exists
    if let Some(expected_typecheck) = sections.typecheck {
        let (file, parse_diag, interner) = parser::parse(FileId::new(0), &sections.source);
        
        if parse_diag.has_errors() {
            let actual_errors = format_diagnostics(&parse_diag);
            return TestResult {
                file_name,
                passed: false,
                message: "Parse errors before type checking".to_string(),
                expected: String::new(),
                actual: actual_errors,
            };
        }
        
        let mut typecheck_diag = DiagnosticSink::new();
        let _result = gox_analysis::typecheck_file(&file, &interner, &mut typecheck_diag);
        
        let actual_errors = format_diagnostics(&typecheck_diag);
        
        // If expected_typecheck starts with "OK", expect no errors
        if expected_typecheck.trim() == "OK" {
            if typecheck_diag.has_errors() {
                return TestResult {
                    file_name,
                    passed: false,
                    message: "Unexpected type errors".to_string(),
                    expected: "OK (no errors)".to_string(),
                    actual: actual_errors,
                };
            }
        } else {
            // Expect specific errors
            if !errors_match(&actual_errors, &expected_typecheck) {
                return TestResult {
                    file_name,
                    passed: false,
                    message: "Type check errors don't match".to_string(),
                    expected: expected_typecheck,
                    actual: actual_errors,
                };
            }
        }
    }
    
    TestResult {
        file_name,
        passed: true,
        message: "OK".to_string(),
        expected: String::new(),
        actual: String::new(),
    }
}

/// Parsed sections from a test file.
struct TestSections {
    source: String,
    parser: Option<String>,
    typecheck: Option<String>,
    errors: Option<String>,
}

/// Parses a test file into sections.
fn parse_test_file(content: &str) -> TestSections {
    let mut source = String::new();
    let mut parser = None;
    let mut typecheck = None;
    let mut errors = None;
    
    let mut current_section = "source";
    let mut current_content = String::new();
    
    for line in content.lines() {
        let trimmed = line.trim();
        
        if trimmed.starts_with("=== ") && trimmed.ends_with(" ===") {
            // Save previous section
            match current_section {
                "source" => source = current_content.trim().to_string(),
                "parser" => parser = Some(current_content.trim().to_string()),
                "typecheck" => typecheck = Some(current_content.trim().to_string()),
                "errors" => errors = Some(current_content.trim().to_string()),
                _ => {}
            }
            current_content.clear();
            
            // Parse new section name
            let section_name = trimmed.trim_start_matches("=== ").trim_end_matches(" ===");
            current_section = section_name;
        } else {
            current_content.push_str(line);
            current_content.push('\n');
        }
    }
    
    // Save last section
    match current_section {
        "source" => source = current_content.trim().to_string(),
        "parser" => parser = Some(current_content.trim().to_string()),
        "typecheck" => typecheck = Some(current_content.trim().to_string()),
        "errors" => errors = Some(current_content.trim().to_string()),
        _ => {}
    }
    
    TestSections { source, parser, typecheck, errors }
}

/// Formats diagnostics for comparison.
/// Only outputs diagnostics with error codes (skips notes without codes).
/// Format: "[E{code}] {message}"
fn format_diagnostics(diagnostics: &gox_common::diagnostics::DiagnosticSink) -> String {
    let mut output = String::new();
    for diag in diagnostics.iter() {
        if let Some(code) = diag.code {
            output.push_str(&format!("[E{:04}] {}", code, diag.message));
            output.push('\n');
        }
        // Skip diagnostics without codes (notes, etc.)
    }
    output.trim().to_string()
}

/// Checks if actual errors match expected errors.
/// Expected format can be:
/// - "E2100" - match by error code only
/// - "[E2100]" - match by error code only  
/// - "[E2100] message" - match by error code and message contains
/// - "message" - legacy: match by message contains
fn errors_match(actual: &str, expected: &str) -> bool {
    let actual_lines: Vec<&str> = actual.lines().map(|l| l.trim()).filter(|l| !l.is_empty()).collect();
    let expected_lines: Vec<&str> = expected.lines().map(|l| l.trim()).filter(|l| !l.is_empty() && !l.starts_with("//")).collect();
    
    if actual_lines.len() != expected_lines.len() {
        return false;
    }
    
    for (a, e) in actual_lines.iter().zip(expected_lines.iter()) {
        if !error_line_matches(a, e) {
            return false;
        }
    }
    
    true
}

/// Checks if a single actual error line matches an expected pattern.
fn error_line_matches(actual: &str, expected: &str) -> bool {
    // Check if expected is just an error code like "E2100" or "[E2100]"
    let expected_trimmed = expected.trim_start_matches('[').trim_end_matches(']');
    if expected_trimmed.starts_with('E') && expected_trimmed[1..].chars().all(|c| c.is_ascii_digit()) {
        // Match by error code only
        return actual.contains(&format!("[{}]", expected_trimmed));
    }
    
    // Check if expected starts with "[Exxxx]" pattern
    if expected.starts_with("[E") {
        if let Some(bracket_end) = expected.find(']') {
            let code = &expected[1..bracket_end];
            // Check code matches
            if !actual.contains(&format!("[{}]", code)) {
                return false;
            }
            // If there's a message part, check it too
            let message_part = expected[bracket_end + 1..].trim();
            if !message_part.is_empty() {
                return actual.contains(message_part);
            }
            return true;
        }
    }
    
    // Legacy: match by message contains
    actual.contains(expected) || expected.contains(actual)
}

/// Checks if actual AST matches expected AST.
fn ast_matches(actual: &str, expected: &str) -> bool {
    let normalize = |s: &str| -> String {
        s.lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty() && !l.starts_with("//"))
            .collect::<Vec<_>>()
            .join("\n")
    };
    
    normalize(actual) == normalize(expected)
}
