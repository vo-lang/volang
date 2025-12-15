//! GoX Test Framework
//!
//! File-based integration tests for the GoX compiler.
//! See README.md for test file format documentation.

mod printer;
mod runner;

pub use printer::AstPrinter;
pub use runner::{TestResult, TestSummary, run_all, run_single_file, run_multi_file};

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_all() {
        let test_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data");
        if !test_dir.exists() {
            return;
        }
        
        let summary = run_all(&test_dir);
        
        if !summary.success() {
            let mut msg = format!("\n{} tests failed:\n", summary.failed);
            for failure in &summary.failures {
                msg.push_str(&format!("  ✗ {}\n", failure.path));
                if let Some(err) = &failure.error {
                    for line in err.lines().take(5) {
                        msg.push_str(&format!("    {}\n", line));
                    }
                }
            }
            msg.push_str(&format!("\nResults: {} passed, {} failed\n", summary.passed, summary.failed));
            panic!("{}", msg);
        }
    }
}
