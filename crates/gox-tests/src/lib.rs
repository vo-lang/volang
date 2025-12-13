//! File-based integration tests for the GoX compiler.
//!
//! This crate provides a test framework that reads `.gox` test files containing
//! both source code and expected output from various compiler phases.
//!
//! # Test File Format
//!
//! Test files use a simple format with sections separated by markers:
//!
//! ```text
//! // Test description (optional)
//! package main
//!
//! func main() {
//!     x := 1
//! }
//!
//! === parser ===
//! File {
//!     package: Some("main"),
//!     decls: [
//!         Func { name: "main", ... }
//!     ]
//! }
//!
//! === errors ===
//! // Expected error messages (if any)
//! ```
//!
//! Supported sections:
//! - `=== parser ===` - Expected parser AST output
//! - `=== errors ===` - Expected error messages
//! - `=== typecheck ===` - Expected type checker output (future)
//! - `=== bytecode ===` - Expected bytecode output (future)

mod printer;
mod runner;

pub use printer::AstPrinter;
pub use runner::{TestRunner, TestResult, run_test_file, run_all_tests};

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_run_all_parser_tests() {
        let test_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data");
        if !test_dir.exists() {
            return; // No test files yet
        }
        
        let results = run_all_tests(&test_dir);
        let mut failures = Vec::new();
        
        for result in &results {
            if !result.passed {
                failures.push(format!(
                    "{}: {}\n  Expected:\n{}\n  Actual:\n{}",
                    result.file_name,
                    result.message,
                    result.expected.lines().map(|l| format!("    {}", l)).collect::<Vec<_>>().join("\n"),
                    result.actual.lines().map(|l| format!("    {}", l)).collect::<Vec<_>>().join("\n"),
                ));
            }
        }
        
        if !failures.is_empty() {
            panic!("Test failures:\n{}", failures.join("\n\n"));
        }
    }

    #[test]
    fn test_run_all_typecheck_tests() {
        let test_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data").join("typecheck");
        if !test_dir.exists() {
            return; // No typecheck test files yet
        }
        
        let results = run_all_tests(&test_dir);
        let mut failures = Vec::new();
        
        for result in &results {
            if !result.passed {
                failures.push(format!(
                    "{}: {}\n  Expected:\n{}\n  Actual:\n{}",
                    result.file_name,
                    result.message,
                    result.expected.lines().map(|l| format!("    {}", l)).collect::<Vec<_>>().join("\n"),
                    result.actual.lines().map(|l| format!("    {}", l)).collect::<Vec<_>>().join("\n"),
                ));
            }
        }
        
        if !failures.is_empty() {
            panic!("Typecheck test failures:\n{}", failures.join("\n\n"));
        }
    }
}
