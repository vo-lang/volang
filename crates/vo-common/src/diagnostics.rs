//! Diagnostic reporting system.
//!
//! This module provides comprehensive error and warning reporting with
//! source spans, labels, and rich formatting for both internal use and end users.
//!
//! # Global Position Space
//!
//! With the global position space design, `Span` alone uniquely identifies both
//! the file and location. Labels no longer need a separate `FileId` field.

use std::fmt;

use codespan_reporting::diagnostic::{
    Diagnostic as CSDiagnostic, Label as CSLabel, LabelStyle, Severity as CSSeverity,
};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream, WriteColor},
    Config,
};

use crate::source::SourceMap;
use crate::span::Span;

/// Severity level of a diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Severity {
    /// A fatal error that prevents compilation.
    Error,
    /// A warning that doesn't prevent compilation.
    Warning,
    /// Additional information about a diagnostic.
    Note,
    /// A suggestion for fixing an issue.
    Help,
}

impl Severity {
    /// Returns true if this is an error.
    #[inline]
    pub const fn is_error(self) -> bool {
        matches!(self, Severity::Error)
    }

    /// Returns true if this is a warning.
    #[inline]
    pub const fn is_warning(self) -> bool {
        matches!(self, Severity::Warning)
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Note => write!(f, "note"),
            Severity::Help => write!(f, "help"),
        }
    }
}

impl From<Severity> for CSSeverity {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Error => CSSeverity::Error,
            Severity::Warning => CSSeverity::Warning,
            Severity::Note => CSSeverity::Note,
            Severity::Help => CSSeverity::Help,
        }
    }
}

/// A label attached to a diagnostic, pointing to a specific location in source code.
/// 
/// With global position space, the span alone identifies the file.
#[derive(Clone, Debug)]
pub struct Label {
    /// The style of the label (primary or secondary).
    pub style: LabelStyle,
    /// The span this label points to (global position).
    pub span: Span,
    /// An optional message for this label.
    pub message: Option<String>,
}

impl Label {
    /// Creates a primary label (the main location of the diagnostic).
    pub fn primary(span: impl Into<Span>) -> Self {
        Self {
            style: LabelStyle::Primary,
            span: span.into(),
            message: None,
        }
    }

    /// Creates a secondary label (additional context).
    pub fn secondary(span: impl Into<Span>) -> Self {
        Self {
            style: LabelStyle::Secondary,
            span: span.into(),
            message: None,
        }
    }

    /// Adds a message to this label.
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }
}

/// A suggested fix for a diagnostic.
#[derive(Clone, Debug)]
pub struct Suggestion {
    /// The span to replace (global position).
    pub span: Span,
    /// The replacement text.
    pub replacement: String,
    /// A description of the suggestion.
    pub message: String,
}

impl Suggestion {
    /// Creates a new suggestion.
    pub fn new(
        span: impl Into<Span>,
        replacement: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            span: span.into(),
            replacement: replacement.into(),
            message: message.into(),
        }
    }
}

/// A diagnostic message with severity, location, and optional labels.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    /// The severity of this diagnostic.
    pub severity: Severity,
    /// A unique numeric error code.
    pub code: Option<u16>,
    /// The main message.
    pub message: String,
    /// Labels pointing to relevant source locations.
    pub labels: Vec<Label>,
    /// Additional notes.
    pub notes: Vec<String>,
    /// Suggested fixes.
    pub suggestions: Vec<Suggestion>,
}

impl Diagnostic {
    /// Creates a new diagnostic with the given severity and message.
    pub fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self {
            severity,
            code: None,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    /// Creates an error diagnostic.
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(Severity::Error, message)
    }

    /// Creates a warning diagnostic.
    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, message)
    }

    /// Creates a note diagnostic.
    pub fn note(message: impl Into<String>) -> Self {
        Self::new(Severity::Note, message)
    }

    /// Creates a help diagnostic.
    pub fn help(message: impl Into<String>) -> Self {
        Self::new(Severity::Help, message)
    }

    /// Sets the error code.
    pub fn with_code(mut self, code: u16) -> Self {
        self.code = Some(code);
        self
    }

    /// Adds a label to this diagnostic.
    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    /// Adds multiple labels to this diagnostic.
    pub fn with_labels(mut self, labels: impl IntoIterator<Item = Label>) -> Self {
        self.labels.extend(labels);
        self
    }

    /// Adds a note to this diagnostic.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Adds multiple notes to this diagnostic.
    pub fn with_notes(mut self, notes: impl IntoIterator<Item = String>) -> Self {
        self.notes.extend(notes);
        self
    }

    /// Adds a suggestion to this diagnostic.
    pub fn with_suggestion(mut self, suggestion: Suggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    /// Returns true if this is an error.
    #[inline]
    pub fn is_error(&self) -> bool {
        self.severity.is_error()
    }

    /// Returns true if this is a warning.
    #[inline]
    pub fn is_warning(&self) -> bool {
        self.severity.is_warning()
    }
}

/// A collector for diagnostics during compilation.
#[derive(Default)]
pub struct DiagnosticSink {
    diagnostics: Vec<Diagnostic>,
    error_count: usize,
    warning_count: usize,
}

impl DiagnosticSink {
    /// Creates a new empty diagnostic sink.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a diagnostic to the sink.
    pub fn emit(&mut self, diagnostic: Diagnostic) {
        match diagnostic.severity {
            Severity::Error => self.error_count += 1,
            Severity::Warning => self.warning_count += 1,
            _ => {}
        }
        self.diagnostics.push(diagnostic);
    }

    /// Emits an error diagnostic.
    pub fn error(&mut self, message: impl Into<String>) -> &mut Diagnostic {
        self.emit(Diagnostic::error(message));
        self.diagnostics.last_mut().unwrap()
    }

    /// Emits a warning diagnostic.
    pub fn warning(&mut self, message: impl Into<String>) -> &mut Diagnostic {
        self.emit(Diagnostic::warning(message));
        self.diagnostics.last_mut().unwrap()
    }

    /// Returns the number of errors.
    #[inline]
    pub fn error_count(&self) -> usize {
        self.error_count
    }

    /// Returns the number of warnings.
    #[inline]
    pub fn warning_count(&self) -> usize {
        self.warning_count
    }

    /// Returns true if any errors were emitted.
    #[inline]
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Returns true if any warnings were emitted.
    #[inline]
    pub fn has_warnings(&self) -> bool {
        self.warning_count > 0
    }

    /// Returns true if no diagnostics were emitted.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Returns the number of diagnostics.
    #[inline]
    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }

    /// Returns an iterator over all diagnostics.
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    /// Returns all diagnostics.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Takes all diagnostics, leaving the sink empty.
    pub fn take(&mut self) -> Vec<Diagnostic> {
        self.error_count = 0;
        self.warning_count = 0;
        std::mem::take(&mut self.diagnostics)
    }

    /// Clears all diagnostics.
    pub fn clear(&mut self) {
        self.diagnostics.clear();
        self.error_count = 0;
        self.warning_count = 0;
    }

    /// Extends this sink with diagnostics from another sink.
    pub fn extend(&mut self, other: DiagnosticSink) {
        self.error_count += other.error_count;
        self.warning_count += other.warning_count;
        self.diagnostics.extend(other.diagnostics);
    }
}

impl fmt::Debug for DiagnosticSink {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DiagnosticSink")
            .field("errors", &self.error_count)
            .field("warnings", &self.warning_count)
            .field("total", &self.diagnostics.len())
            .finish()
    }
}

/// A diagnostic emitter that renders diagnostics to output.
pub struct DiagnosticEmitter<'a> {
    source_map: &'a SourceMap,
    config: Config,
}

impl<'a> DiagnosticEmitter<'a> {
    /// Creates a new diagnostic emitter.
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            config: Config::default(),
        }
    }

    /// Creates a new diagnostic emitter with custom config.
    pub fn with_config(source_map: &'a SourceMap, config: Config) -> Self {
        Self { source_map, config }
    }

    /// Emits a diagnostic to stderr with colors.
    pub fn emit(&self, diagnostic: &Diagnostic) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        self.emit_to(&mut writer.lock(), diagnostic);
    }

    /// Emits a diagnostic to a writer.
    pub fn emit_to<W: WriteColor>(&self, writer: &mut W, diagnostic: &Diagnostic) {
        let files = self.build_files();
        let cs_diagnostic = self.to_codespan(diagnostic);
        
        let _ = term::emit(writer, &self.config, &files, &cs_diagnostic);
    }

    /// Emits a diagnostic to a string.
    pub fn emit_to_string(&self, diagnostic: &Diagnostic) -> String {
        let mut buffer = termcolor::Buffer::no_color();
        self.emit_to(&mut buffer, diagnostic);
        String::from_utf8_lossy(buffer.as_slice()).into_owned()
    }

    /// Emits all diagnostics from a sink.
    pub fn emit_all(&self, sink: &DiagnosticSink) {
        for diagnostic in sink.iter() {
            self.emit(diagnostic);
        }
    }

    /// Emits all diagnostics to a string.
    pub fn emit_all_to_string(&self, sink: &DiagnosticSink) -> String {
        let mut output = String::new();
        for diagnostic in sink.iter() {
            output.push_str(&self.emit_to_string(diagnostic));
        }
        output
    }

    /// Builds a SimpleFiles structure for codespan-reporting.
    fn build_files(&self) -> SimpleFiles<&str, &str> {
        let mut files = SimpleFiles::new();
        for file in self.source_map.files() {
            files.add(file.name(), file.source());
        }
        files
    }

    /// Converts our diagnostic to codespan-reporting format.
    fn to_codespan(&self, diagnostic: &Diagnostic) -> CSDiagnostic<usize> {
        let mut cs_diagnostic = CSDiagnostic::new(diagnostic.severity.into())
            .with_message(&diagnostic.message);

        if let Some(code) = diagnostic.code {
            cs_diagnostic = cs_diagnostic.with_code(format!("E{:04}", code));
        }

        let labels: Vec<CSLabel<usize>> = diagnostic
            .labels
            .iter()
            .filter_map(|label| {
                // Look up file from global span
                let file = self.source_map.lookup_span(label.span)?;
                let file_id = file.id().as_u32() as usize;
                // Convert global span to local range for codespan-reporting
                let start = file.local_offset(label.span.start) as usize;
                let end = file.local_offset(label.span.end) as usize;
                
                let mut cs_label = CSLabel::new(label.style, file_id, start..end);
                if let Some(msg) = &label.message {
                    cs_label = cs_label.with_message(msg);
                }
                Some(cs_label)
            })
            .collect();

        cs_diagnostic = cs_diagnostic.with_labels(labels);

        let mut notes = diagnostic.notes.clone();
        for suggestion in &diagnostic.suggestions {
            notes.push(format!("help: {}: `{}`", suggestion.message, suggestion.replacement));
        }
        cs_diagnostic = cs_diagnostic.with_notes(notes);

        cs_diagnostic
    }
}

/// A result type that carries diagnostics.
pub type DiagnosticResult<T> = Result<T, DiagnosticSink>;

/// Extension trait for adding diagnostic context to results.
pub trait DiagnosticResultExt<T> {
    /// Adds a diagnostic if the result is an error.
    fn with_diagnostic(self, diagnostic: Diagnostic) -> DiagnosticResult<T>;
}

impl<T, E> DiagnosticResultExt<T> for Result<T, E> {
    fn with_diagnostic(self, diagnostic: Diagnostic) -> DiagnosticResult<T> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => {
                let mut sink = DiagnosticSink::new();
                sink.emit(diagnostic);
                Err(sink)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_severity() {
        assert!(Severity::Error.is_error());
        assert!(!Severity::Error.is_warning());
        assert!(Severity::Warning.is_warning());
        assert!(!Severity::Warning.is_error());
        
        assert_eq!(format!("{}", Severity::Error), "error");
        assert_eq!(format!("{}", Severity::Warning), "warning");
        assert_eq!(format!("{}", Severity::Note), "note");
        assert_eq!(format!("{}", Severity::Help), "help");
    }

    #[test]
    fn test_label_creation() {
        let label = Label::primary(10u32..20u32)
            .with_message("test message");
        
        assert_eq!(label.style, LabelStyle::Primary);
        assert_eq!(label.span.start.0, 10);
        assert_eq!(label.span.end.0, 20);
        assert_eq!(label.message, Some("test message".to_string()));
    }

    #[test]
    fn test_label_secondary() {
        let label = Label::secondary(5u32..15u32);
        
        assert_eq!(label.style, LabelStyle::Secondary);
    }

    #[test]
    fn test_suggestion() {
        let suggestion = Suggestion::new(
            10u32..15u32,
            "replacement",
            "try this instead",
        );
        
        assert_eq!(suggestion.span.start.0, 10);
        assert_eq!(suggestion.replacement, "replacement");
        assert_eq!(suggestion.message, "try this instead");
    }

    #[test]
    fn test_diagnostic_creation() {
        let diag = Diagnostic::error("test error")
            .with_code(1)
            .with_label(Label::primary(0u32..5u32))
            .with_note("this is a note");
        
        assert!(diag.is_error());
        assert!(!diag.is_warning());
        assert_eq!(diag.message, "test error");
        assert_eq!(diag.code, Some(1));
        assert_eq!(diag.labels.len(), 1);
        assert_eq!(diag.notes.len(), 1);
    }

    #[test]
    fn test_diagnostic_warning() {
        let diag = Diagnostic::warning("test warning");
        
        assert!(diag.is_warning());
        assert!(!diag.is_error());
    }

    #[test]
    fn test_diagnostic_with_multiple_labels() {
        let diag = Diagnostic::error("multiple labels")
            .with_labels([
                Label::primary(0u32..5u32),
                Label::secondary(10u32..15u32),
            ]);
        
        assert_eq!(diag.labels.len(), 2);
    }

    #[test]
    fn test_diagnostic_sink_basic() {
        let mut sink = DiagnosticSink::new();
        
        assert!(sink.is_empty());
        assert_eq!(sink.len(), 0);
        assert!(!sink.has_errors());
        assert!(!sink.has_warnings());
        
        sink.emit(Diagnostic::error("error 1"));
        sink.emit(Diagnostic::warning("warning 1"));
        sink.emit(Diagnostic::error("error 2"));
        
        assert!(!sink.is_empty());
        assert_eq!(sink.len(), 3);
        assert!(sink.has_errors());
        assert!(sink.has_warnings());
        assert_eq!(sink.error_count(), 2);
        assert_eq!(sink.warning_count(), 1);
    }

    #[test]
    fn test_diagnostic_sink_emit_methods() {
        let mut sink = DiagnosticSink::new();
        
        sink.error("test error");
        sink.warning("test warning");
        
        assert_eq!(sink.error_count(), 1);
        assert_eq!(sink.warning_count(), 1);
    }

    #[test]
    fn test_diagnostic_sink_take() {
        let mut sink = DiagnosticSink::new();
        sink.emit(Diagnostic::error("error"));
        sink.emit(Diagnostic::warning("warning"));
        
        let diagnostics = sink.take();
        
        assert_eq!(diagnostics.len(), 2);
        assert!(sink.is_empty());
        assert_eq!(sink.error_count(), 0);
        assert_eq!(sink.warning_count(), 0);
    }

    #[test]
    fn test_diagnostic_sink_clear() {
        let mut sink = DiagnosticSink::new();
        sink.emit(Diagnostic::error("error"));
        
        sink.clear();
        
        assert!(sink.is_empty());
        assert_eq!(sink.error_count(), 0);
    }

    #[test]
    fn test_diagnostic_sink_extend() {
        let mut sink1 = DiagnosticSink::new();
        sink1.emit(Diagnostic::error("error 1"));
        
        let mut sink2 = DiagnosticSink::new();
        sink2.emit(Diagnostic::error("error 2"));
        sink2.emit(Diagnostic::warning("warning"));
        
        sink1.extend(sink2);
        
        assert_eq!(sink1.len(), 3);
        assert_eq!(sink1.error_count(), 2);
        assert_eq!(sink1.warning_count(), 1);
    }

    #[test]
    fn test_diagnostic_sink_iter() {
        let mut sink = DiagnosticSink::new();
        sink.emit(Diagnostic::error("error 1"));
        sink.emit(Diagnostic::error("error 2"));
        
        let messages: Vec<_> = sink.iter().map(|d| d.message.as_str()).collect();
        assert_eq!(messages, vec!["error 1", "error 2"]);
    }

    #[test]
    fn test_diagnostic_emitter() {
        let mut source_map = SourceMap::new();
        source_map.add_file("test.vo", "func main() {}");
        
        let emitter = DiagnosticEmitter::new(&source_map);
        
        let diagnostic = Diagnostic::error("undefined variable")
            .with_label(Label::primary(5u32..9u32).with_message("not found"));
        
        let output = emitter.emit_to_string(&diagnostic);
        
        assert!(output.contains("error"));
        assert!(output.contains("undefined variable"));
    }

    #[test]
    fn test_diagnostic_emitter_all() {
        let mut source_map = SourceMap::new();
        source_map.add_file("test.vo", "var x = 1\nvar y = 2");
        
        let mut sink = DiagnosticSink::new();
        sink.emit(Diagnostic::error("error 1").with_label(Label::primary(0u32..3u32)));
        sink.emit(Diagnostic::warning("warning 1").with_label(Label::primary(10u32..13u32)));
        
        let emitter = DiagnosticEmitter::new(&source_map);
        let output = emitter.emit_all_to_string(&sink);
        
        assert!(output.contains("error 1"));
        assert!(output.contains("warning 1"));
    }

    #[test]
    fn test_notes_only_diagnostic() {
        let diag = Diagnostic::note("this is informational")
            .with_note("additional context");
        
        assert_eq!(diag.severity, Severity::Note);
        assert_eq!(diag.notes.len(), 1);
    }

    #[test]
    fn test_diagnostic_with_suggestion() {
        let diag = Diagnostic::error("missing semicolon")
            .with_suggestion(Suggestion::new(
                10u32..10u32,
                ";",
                "add a semicolon",
            ));
        
        assert_eq!(diag.suggestions.len(), 1);
        assert_eq!(diag.suggestions[0].replacement, ";");
    }
}
