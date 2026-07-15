//! Centralized diagnostic definitions for the Vo parser.
//!
//! All parser error codes are defined here for easy reference and testing.
//!
//! With global position space, all error reporting methods use `Span` directly
//! without requiring a separate `FileId`.

use std::ops::Deref;

use vo_common::diagnostics::DiagnosticSink;
use vo_common::{Diagnostic, Label, Span};

/// Maximum number of concrete lexer/parser diagnostics retained for one file.
///
/// Parsing still advances to EOF after this boundary so recovery remains
/// deterministic, while malformed generated input cannot grow an unbounded
/// diagnostic vector. One final diagnostic records that suppression occurred.
pub(crate) const MAX_SYNTAX_DIAGNOSTICS: usize = 256;

#[derive(Default)]
pub(crate) struct SyntaxDiagnosticSink {
    inner: DiagnosticSink,
    suppressed: bool,
}

impl SyntaxDiagnosticSink {
    pub(crate) fn emit(&mut self, diagnostic: Diagnostic) {
        if self.inner.len() < MAX_SYNTAX_DIAGNOSTICS {
            self.inner.emit(diagnostic);
            return;
        }
        if self.suppressed {
            return;
        }
        self.suppressed = true;
        let span = diagnostic
            .labels
            .first()
            .map(|label| label.span)
            .unwrap_or_else(Span::dummy);
        self.inner.emit(
            SyntaxError::DiagnosticLimitExceeded.at_with_message(
                span,
                format!(
                    "syntax diagnostic limit of {MAX_SYNTAX_DIAGNOSTICS} reached; further diagnostics suppressed"
                ),
            ),
        );
    }

    pub(crate) fn take(&mut self) -> DiagnosticSink {
        self.suppressed = false;
        std::mem::take(&mut self.inner)
    }

    pub(crate) fn into_inner(self) -> DiagnosticSink {
        self.inner
    }
}

impl From<DiagnosticSink> for SyntaxDiagnosticSink {
    fn from(mut diagnostics: DiagnosticSink) -> Self {
        let mut bounded = Self::default();
        for diagnostic in diagnostics.take() {
            bounded.emit(diagnostic);
        }
        bounded
    }
}

impl Deref for SyntaxDiagnosticSink {
    type Target = DiagnosticSink;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Parser error codes (1xxx range).
///
/// Error code ranges:
/// - 1000-1099: Lexer errors (literals, comments)
/// - 1100-1199: Parser errors (syntax)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum SyntaxError {
    // === Lexer: Comments (1000-1009) ===
    /// Unterminated block comment.
    UnterminatedBlockComment = 1000,

    // === Lexer: String/Rune Literals (1010-1029) ===
    /// Unterminated string literal.
    UnterminatedString = 1010,
    /// Unterminated raw string literal.
    UnterminatedRawString = 1011,
    /// Unterminated rune literal.
    UnterminatedRune = 1012,
    /// Empty rune literal.
    EmptyRune = 1013,
    /// Rune literal has more than one character.
    MultiCharRune = 1014,

    // === Lexer: Escape Sequences (1030-1039) ===
    /// Unknown escape sequence.
    UnknownEscape = 1030,
    /// Escape sequence not terminated.
    UnterminatedEscape = 1031,
    /// Escape sequence requires N hex digits.
    EscapeHexDigits = 1032,
    /// Octal escape requires 3 digits.
    EscapeOctalDigits = 1033,
    /// Unicode escape does not encode a Unicode scalar value.
    EscapeInvalidUnicodeScalar = 1034,
    /// Octal escape does not fit in one byte.
    EscapeOctalValue = 1035,

    // === Lexer: Number Literals (1040-1059) ===
    /// Hexadecimal literal has no digits.
    HexNoDigits = 1040,
    /// Hexadecimal float literal has no digits.
    HexFloatNoDigits = 1041,
    /// Hexadecimal float requires 'p' exponent.
    HexFloatNoExponent = 1042,
    /// Octal literal has no digits.
    OctalNoDigits = 1043,
    /// Invalid digit in octal literal.
    OctalInvalidDigit = 1044,
    /// Binary literal has no digits.
    BinaryNoDigits = 1045,
    /// Invalid digit in binary literal.
    BinaryInvalidDigit = 1046,
    /// Exponent has no digits.
    ExponentNoDigits = 1047,
    /// Numeric separator is not between two digits.
    InvalidNumericSeparator = 1048,

    // === Lexer: Other (1090-1099) ===
    /// Unexpected character.
    UnexpectedChar = 1090,

    // === Parser: General (1100-1119) ===
    /// Expected token.
    ExpectedToken = 1100,
    /// Unexpected token.
    UnexpectedToken = 1101,
    /// Expected expression.
    ExpectedExpr = 1102,
    /// Expected statement.
    ExpectedStmt = 1103,
    /// Expected type.
    ExpectedType = 1104,
    /// Expected identifier.
    ExpectedIdent = 1105,
    /// Parser recursion budget was exhausted.
    NestingTooDeep = 1106,
    /// Source size or positions exceed the frontend's bounded span domain.
    SourceTooLarge = 1107,
    /// An expression exceeds a non-recursive structural resource limit.
    ExpressionTooComplex = 1108,
    /// Too many syntax diagnostics were produced for one source file.
    DiagnosticLimitExceeded = 1109,

    // === Parser: Declarations (1120-1139) ===
    /// Expected package declaration.
    ExpectedPackage = 1120,
    /// Expected function body.
    ExpectedFuncBody = 1121,
    /// Expected struct field.
    ExpectedStructField = 1122,
    /// Expected interface method.
    ExpectedInterfaceMethod = 1123,
    /// Keyword used as identifier.
    KeywordAsIdent = 1124,
    InvalidInlineMod = 1125,
}

impl SyntaxError {
    /// Returns the numeric error code.
    pub fn code(self) -> u16 {
        self as u16
    }

    /// Returns the error message.
    pub fn message(self) -> &'static str {
        match self {
            // Lexer: Comments
            SyntaxError::UnterminatedBlockComment => "unterminated block comment",

            // Lexer: String/Rune
            SyntaxError::UnterminatedString => "unterminated string literal",
            SyntaxError::UnterminatedRawString => "unterminated raw string literal",
            SyntaxError::UnterminatedRune => "unterminated rune literal",
            SyntaxError::EmptyRune => "empty rune literal",
            SyntaxError::MultiCharRune => "rune literal has more than one character",

            // Lexer: Escape
            SyntaxError::UnknownEscape => "unknown escape sequence",
            SyntaxError::UnterminatedEscape => "escape sequence not terminated",
            SyntaxError::EscapeHexDigits => "escape sequence requires hex digits",
            SyntaxError::EscapeOctalDigits => "octal escape sequence requires 3 octal digits",
            SyntaxError::EscapeInvalidUnicodeScalar => {
                "escape sequence is not a valid Unicode scalar value"
            }
            SyntaxError::EscapeOctalValue => "octal escape sequence must fit in one byte",

            // Lexer: Numbers
            SyntaxError::HexNoDigits => "hexadecimal literal has no digits",
            SyntaxError::HexFloatNoDigits => "hexadecimal float literal has no digits",
            SyntaxError::HexFloatNoExponent => "hexadecimal float literal requires 'p' exponent",
            SyntaxError::OctalNoDigits => "octal literal has no digits",
            SyntaxError::OctalInvalidDigit => "invalid digit in octal literal",
            SyntaxError::BinaryNoDigits => "binary literal has no digits",
            SyntaxError::BinaryInvalidDigit => "invalid digit in binary literal",
            SyntaxError::ExponentNoDigits => "exponent has no digits",
            SyntaxError::InvalidNumericSeparator => "invalid '_' in numeric literal",

            // Lexer: Other
            SyntaxError::UnexpectedChar => "unexpected character",

            // Parser: General
            SyntaxError::ExpectedToken => "expected token",
            SyntaxError::UnexpectedToken => "unexpected token",
            SyntaxError::ExpectedExpr => "expected expression",
            SyntaxError::ExpectedStmt => "expected statement",
            SyntaxError::ExpectedType => "expected type",
            SyntaxError::ExpectedIdent => "expected identifier",
            SyntaxError::NestingTooDeep => "syntax nesting exceeds the parser limit",
            SyntaxError::SourceTooLarge => "source exceeds the frontend size or position limit",
            SyntaxError::ExpressionTooComplex => "expression exceeds the structural resource limit",
            SyntaxError::DiagnosticLimitExceeded => "too many syntax diagnostics",

            // Parser: Declarations
            SyntaxError::ExpectedPackage => "expected package declaration",
            SyntaxError::ExpectedFuncBody => "expected function body",
            SyntaxError::ExpectedStructField => "expected struct field",
            SyntaxError::ExpectedInterfaceMethod => "expected interface method",
            SyntaxError::KeywordAsIdent => "cannot use keyword as identifier",
            SyntaxError::InvalidInlineMod => "invalid inline mod",
        }
    }

    /// Creates a diagnostic with this error code (no location).
    pub fn diagnostic(self) -> Diagnostic {
        Diagnostic::error(self.message()).with_code(self.code())
    }

    /// Creates a diagnostic with this error code and a custom message.
    pub fn with_message(self, message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(message).with_code(self.code())
    }

    /// Creates a diagnostic with this error code and a span label.
    /// Uses global position - no FileId needed.
    pub fn at(self, span: impl Into<Span>) -> Diagnostic {
        Diagnostic::error(self.message())
            .with_code(self.code())
            .with_label(Label::primary(span))
    }

    /// Creates a diagnostic with this error code, custom message, and span.
    pub fn at_with_message(self, span: impl Into<Span>, message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(message)
            .with_code(self.code())
            .with_label(Label::primary(span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_codes() {
        assert_eq!(SyntaxError::UnterminatedBlockComment.code(), 1000);
        assert_eq!(SyntaxError::UnterminatedString.code(), 1010);
        assert_eq!(SyntaxError::HexNoDigits.code(), 1040);
        assert_eq!(SyntaxError::UnexpectedChar.code(), 1090);
        assert_eq!(SyntaxError::ExpectedToken.code(), 1100);
        assert_eq!(SyntaxError::NestingTooDeep.code(), 1106);
        assert_eq!(SyntaxError::SourceTooLarge.code(), 1107);
        assert_eq!(SyntaxError::ExpressionTooComplex.code(), 1108);
        assert_eq!(SyntaxError::DiagnosticLimitExceeded.code(), 1109);
    }

    #[test]
    fn syntax_diagnostic_sink_retains_a_bounded_prefix_and_sentinel() {
        let mut diagnostics = SyntaxDiagnosticSink::default();
        for index in 0..(MAX_SYNTAX_DIAGNOSTICS + 32) {
            diagnostics.emit(
                SyntaxError::UnexpectedChar
                    .at_with_message(index as u32..index as u32 + 1, format!("error-{index}")),
            );
        }

        assert_eq!(diagnostics.len(), MAX_SYNTAX_DIAGNOSTICS + 1);
        assert_eq!(diagnostics.error_count(), MAX_SYNTAX_DIAGNOSTICS + 1);
        let final_diagnostic = diagnostics.iter().last().expect("limit sentinel");
        assert_eq!(
            final_diagnostic.code,
            Some(SyntaxError::DiagnosticLimitExceeded.code())
        );
        assert!(final_diagnostic
            .message
            .contains("further diagnostics suppressed"));
    }

    #[test]
    fn test_diagnostic_creation() {
        let diag = SyntaxError::UnterminatedString.diagnostic();
        assert_eq!(diag.code, Some(1010));
        assert_eq!(diag.message, "unterminated string literal");
    }

    #[test]
    fn test_at_with_span() {
        let diag = SyntaxError::UnterminatedString.at(10u32..20u32);
        assert_eq!(diag.code, Some(1010));
        assert_eq!(diag.labels.len(), 1);
        assert_eq!(diag.labels[0].span.start.0, 10);
        assert_eq!(diag.labels[0].span.end.0, 20);
    }
}
