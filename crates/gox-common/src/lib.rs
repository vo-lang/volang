//! GoX Common - Shared utilities for the GoX compiler.
//!
//! This crate provides common types and utilities used across all compiler stages:
//!
//! - [`span`]: Source code span representation
//! - [`source`]: Source file management
//! - [`diagnostic`]: Rich error/warning reporting
//! - [`error`]: Common error types

pub mod diagnostic;
pub mod error;
pub mod source;
pub mod span;

// Re-export commonly used types at crate root
pub use diagnostic::{Diagnostic, DiagnosticBag, Severity};
pub use error::{GoxError, GoxResult};
pub use source::{FileId, SourceManager};
pub use span::Span;

/// Print help for this crate (placeholder).
pub fn help() {
    println!("gox-common: Shared utilities for the GoX compiler");
}

#[cfg(test)]
mod tests {
    use super::*;
    use codespan_reporting::term;
    use codespan_reporting::term::termcolor::Buffer;

    #[test]
    fn test_basic_error_reporting() {
        let mut sm = SourceManager::new();
        let file_id = sm.add_file("test.gox", "func main() {\n    x := 1\n}");

        // Create an error pointing to 'x'
        let diag = Diagnostic::error("undefined variable")
            .with_code("E0001")
            .with_label(Span::new(18, 19), file_id, "not found in this scope")
            .with_note("variables must be declared before use");

        // Render to buffer
        let config = term::Config::default();
        let mut buffer = Buffer::no_color();
        term::emit(&mut buffer, &config, sm.files(), &diag.into_codespan()).unwrap();

        let output = String::from_utf8(buffer.into_inner()).unwrap();
        println!("{}", output);

        assert!(output.contains("undefined variable"));
        assert!(output.contains("E0001"));
        assert!(output.contains("test.gox:2:5"));
    }

    #[test]
    fn test_error_with_secondary_label() {
        let mut sm = SourceManager::new();
        let file_id = sm.add_file("test.gox", "var x int = 1;\nvar x int = 2;");

        let diag = Diagnostic::error("duplicate definition of `x`")
            .with_label(Span::new(19, 20), file_id, "redefined here")
            .with_secondary_label(Span::new(4, 5), file_id, "first defined here")
            .with_help("consider using a different variable name");

        let config = term::Config::default();
        let mut buffer = Buffer::no_color();
        term::emit(&mut buffer, &config, sm.files(), &diag.into_codespan()).unwrap();

        let output = String::from_utf8(buffer.into_inner()).unwrap();
        println!("{}", output);

        assert!(output.contains("duplicate definition"));
        assert!(output.contains("redefined here"));
        assert!(output.contains("first defined here"));
    }

    #[test]
    fn test_diagnostic_bag() {
        let mut bag = DiagnosticBag::new();

        bag.add(Diagnostic::error("first error"));
        bag.add(Diagnostic::warning("a warning"));
        bag.add(Diagnostic::error("second error"));

        assert!(bag.has_errors());
        assert_eq!(bag.error_count(), 2);
        assert_eq!(bag.warning_count(), 1);
        assert_eq!(bag.len(), 3);
    }

    #[test]
    fn test_gox_error_to_diagnostic() {
        let mut sm = SourceManager::new();
        let file_id = sm.add_file("test.gox", "x := 1");

        let err = GoxError::with_span("test error", Span::new(0, 1), file_id);
        let diag = err.to_diagnostic();

        assert!(diag.is_error());
        assert_eq!(diag.message(), "test error");
    }

    #[test]
    fn test_span_operations() {
        let a = Span::new(10, 20);
        let b = Span::new(25, 30);

        assert_eq!(a.len(), 10);
        assert!(a.contains(15));
        assert!(!a.contains(20)); // exclusive end

        let merged = a.merge(&b);
        assert_eq!(merged, Span::new(10, 30));

        let to = a.to(&b);
        assert_eq!(to, Span::new(10, 30));
    }
}
