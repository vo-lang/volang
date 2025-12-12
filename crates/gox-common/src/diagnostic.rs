//! Rich diagnostic reporting for compiler errors, warnings, and notes.
//!
//! This module provides a builder-style API for creating diagnostics with:
//! - Multiple labels (primary and secondary)
//! - Error codes for documentation lookup
//! - Help and note messages
//! - Integration with codespan-reporting for rendering

use crate::source::FileId;
use crate::span::Span;
use codespan_reporting::diagnostic::{Diagnostic as CsDiagnostic, Label, LabelStyle};

/// Severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// A fatal error that prevents compilation.
    Error,
    /// A warning that doesn't prevent compilation.
    Warning,
    /// An informational note.
    Note,
    /// A help message with suggestions.
    Help,
}

impl Severity {
    fn to_codespan(self) -> codespan_reporting::diagnostic::Severity {
        match self {
            Severity::Error => codespan_reporting::diagnostic::Severity::Error,
            Severity::Warning => codespan_reporting::diagnostic::Severity::Warning,
            Severity::Note => codespan_reporting::diagnostic::Severity::Note,
            Severity::Help => codespan_reporting::diagnostic::Severity::Help,
        }
    }
}

/// A compiler diagnostic with rich context.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    severity: Severity,
    message: String,
    code: Option<String>,
    labels: Vec<DiagnosticLabel>,
    notes: Vec<String>,
}

/// A label attached to a diagnostic, pointing to source code.
#[derive(Debug, Clone)]
struct DiagnosticLabel {
    style: LabelStyle,
    file_id: FileId,
    span: Span,
    message: String,
}

impl Diagnostic {
    /// Create a new diagnostic with the given severity and message.
    pub fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self {
            severity,
            message: message.into(),
            code: None,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create an error diagnostic.
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(Severity::Error, message)
    }

    /// Create a warning diagnostic.
    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, message)
    }

    /// Create a note diagnostic.
    pub fn note(message: impl Into<String>) -> Self {
        Self::new(Severity::Note, message)
    }

    /// Set an error code (e.g., "E0001" for documentation lookup).
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Add a primary label pointing to the main error location.
    pub fn with_label(mut self, span: Span, file_id: FileId, message: impl Into<String>) -> Self {
        self.labels.push(DiagnosticLabel {
            style: LabelStyle::Primary,
            file_id,
            span,
            message: message.into(),
        });
        self
    }

    /// Add a secondary label for related context.
    pub fn with_secondary_label(
        mut self,
        span: Span,
        file_id: FileId,
        message: impl Into<String>,
    ) -> Self {
        self.labels.push(DiagnosticLabel {
            style: LabelStyle::Secondary,
            file_id,
            span,
            message: message.into(),
        });
        self
    }

    /// Add a note (appears at the bottom of the diagnostic).
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Add a help message.
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.notes.push(format!("help: {}", help.into()));
        self
    }

    /// Check if this is an error.
    pub fn is_error(&self) -> bool {
        matches!(self.severity, Severity::Error)
    }

    /// Get the severity.
    pub fn severity(&self) -> Severity {
        self.severity
    }

    /// Get the primary message.
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Get the error code if set.
    pub fn code(&self) -> Option<&str> {
        self.code.as_deref()
    }

    /// Convert to a codespan-reporting Diagnostic for rendering.
    pub fn into_codespan(self) -> CsDiagnostic<FileId> {
        let mut diag = CsDiagnostic::new(self.severity.to_codespan()).with_message(self.message);

        if let Some(code) = self.code {
            diag = diag.with_code(code);
        }

        let labels: Vec<Label<FileId>> = self
            .labels
            .into_iter()
            .map(|l| Label::new(l.style, l.file_id, l.span.to_range()).with_message(l.message))
            .collect();

        if !labels.is_empty() {
            diag = diag.with_labels(labels);
        }

        if !self.notes.is_empty() {
            diag = diag.with_notes(self.notes);
        }

        diag
    }

    /// Backwards compatibility: alias for into_codespan.
    pub fn into_inner(self) -> CsDiagnostic<FileId> {
        self.into_codespan()
    }
}

/// A collection of diagnostics with helper methods.
#[derive(Debug, Default)]
pub struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    /// Create an empty diagnostic bag.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a diagnostic.
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.is_error())
    }

    /// Get the number of diagnostics.
    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Iterate over all diagnostics.
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    /// Get error count.
    pub fn error_count(&self) -> usize {
        self.diagnostics.iter().filter(|d| d.is_error()).count()
    }

    /// Get warning count.
    pub fn warning_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| matches!(d.severity, Severity::Warning))
            .count()
    }
}

impl IntoIterator for DiagnosticBag {
    type Item = Diagnostic;
    type IntoIter = std::vec::IntoIter<Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.into_iter()
    }
}

impl<'a> IntoIterator for &'a DiagnosticBag {
    type Item = &'a Diagnostic;
    type IntoIter = std::slice::Iter<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.iter()
    }
}

impl Extend<Diagnostic> for DiagnosticBag {
    fn extend<T: IntoIterator<Item = Diagnostic>>(&mut self, iter: T) {
        self.diagnostics.extend(iter);
    }
}
