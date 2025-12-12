//! Error types for the GoX compiler.

use crate::diagnostic::Diagnostic;
use crate::source::FileId;
use crate::span::Span;

/// A compiler error with optional source location.
#[derive(Debug, Clone)]
pub struct GoxError {
    /// Error message.
    pub message: String,
    /// Optional span pointing to the error location.
    pub span: Option<Span>,
    /// Optional file ID.
    pub file_id: Option<FileId>,
}

impl GoxError {
    /// Create a simple error with just a message.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
            file_id: None,
        }
    }

    /// Create an error with a source location.
    pub fn with_span(message: impl Into<String>, span: Span, file_id: FileId) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
            file_id: Some(file_id),
        }
    }

    /// Convert to a Diagnostic for rich rendering.
    pub fn to_diagnostic(&self) -> Diagnostic {
        let mut diag = Diagnostic::error(&self.message);

        if let (Some(span), Some(file_id)) = (self.span, self.file_id) {
            diag = diag.with_label(span, file_id, "here");
        }

        diag
    }
}

impl std::fmt::Display for GoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for GoxError {}

impl From<String> for GoxError {
    fn from(message: String) -> Self {
        Self::new(message)
    }
}

impl From<&str> for GoxError {
    fn from(message: &str) -> Self {
        Self::new(message)
    }
}

/// Result type alias for GoX compiler operations.
pub type GoxResult<T> = Result<T, GoxError>;
