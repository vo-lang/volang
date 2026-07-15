//! Lexer for the Vo programming language.
//!
//! This module provides tokenization of Vo source code, including:
//! - Identifier and keyword recognition
//! - Numeric literals (decimal, hex, octal, binary, float)
//! - String and rune literals with escape sequences
//! - Automatic semicolon insertion
//!
//! The lexer uses global position space - positions are offset by a base value
//! so that spans uniquely identify both file and location.

use std::str::Chars;

use vo_common::diagnostics::DiagnosticSink;
use vo_common::span::Span;
use vo_common::vfs::MAX_TEXT_FILE_BYTES;

use crate::errors::{SyntaxDiagnosticSink, SyntaxError};
use crate::identifier::{is_identifier_continue, is_identifier_start};
use crate::token::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SourcePositionError {
    LengthTooLarge { length: usize, limit: usize },
    EndOverflow { base: u32, length: u32 },
}

impl SourcePositionError {
    pub(crate) fn diagnostic(self, base: u32) -> vo_common::Diagnostic {
        let message = match self {
            Self::LengthTooLarge { length, limit } => format!(
                "source length {length} bytes exceeds the {limit}-byte source-file limit"
            ),
            Self::EndOverflow { base, length } => format!(
                "source range base {base} + length {length} exceeds the 32-bit source position limit"
            ),
        };
        SyntaxError::SourceTooLarge.at_with_message(Span::from_u32(base, base), message)
    }
}

pub(crate) fn source_position_error_for_len(
    length: usize,
    base: u32,
) -> Option<SourcePositionError> {
    if length > MAX_TEXT_FILE_BYTES {
        return Some(SourcePositionError::LengthTooLarge {
            length,
            limit: MAX_TEXT_FILE_BYTES,
        });
    }
    let length = match u32::try_from(length) {
        Ok(length) => length,
        Err(_) => {
            return Some(SourcePositionError::LengthTooLarge {
                length,
                limit: u32::MAX as usize,
            });
        }
    };
    base.checked_add(length)
        .is_none()
        .then_some(SourcePositionError::EndOverflow { base, length })
}

pub(crate) fn source_position_error(source: &str, base: u32) -> Option<SourcePositionError> {
    source_position_error_for_len(source.len(), base)
}

/// The lexer for Vo source code.
pub struct Lexer<'src> {
    /// The source text being lexed.
    source: &'src str,
    /// Character iterator.
    chars: Chars<'src>,
    /// Base offset in global position space.
    base: u32,
    /// Current byte position in the source (local, 0-indexed).
    local_pos: u32,
    /// The previous token kind (for semicolon insertion).
    prev_kind: Option<TokenKind>,
    /// Whether we need to insert a semicolon before the next token.
    pending_semicolon: bool,
    /// Diagnostics sink for lexer errors.
    diagnostics: SyntaxDiagnosticSink,
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer for the given source with a base offset.
    ///
    /// The base offset should come from `SourceMap::file_base()` to ensure
    /// all positions are globally unique.
    pub fn new(source: &'src str, base: u32) -> Self {
        let position_error = source_position_error(source, base);
        let lex_source: &'src str = if position_error.is_none() { source } else { "" };
        let mut diagnostics = SyntaxDiagnosticSink::default();
        if let Some(error) = position_error {
            diagnostics.emit(error.diagnostic(base));
        }
        Self {
            source,
            chars: lex_source.chars(),
            base,
            local_pos: 0,
            prev_kind: None,
            pending_semicolon: false,
            diagnostics,
        }
    }

    /// Returns the current global position.
    #[inline]
    fn pos(&self) -> u32 {
        self.base + self.local_pos
    }

    /// Returns the diagnostics collected during lexing.
    pub fn diagnostics(&self) -> &DiagnosticSink {
        &self.diagnostics
    }

    /// Takes the diagnostics, leaving an empty sink.
    pub fn take_diagnostics(&mut self) -> DiagnosticSink {
        self.diagnostics.take()
    }

    /// Collects all tokens into a vector.
    pub fn collect_tokens(mut self) -> (Vec<Token>, DiagnosticSink) {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        (tokens, self.diagnostics.into_inner())
    }

    /// Returns the next token.
    pub fn next_token(&mut self) -> Token {
        // Handle pending semicolon insertion
        if self.pending_semicolon {
            self.pending_semicolon = false;
            let pos = self.pos();
            let span = Span::from_u32(pos, pos);
            self.prev_kind = Some(TokenKind::Semicolon);
            return Token::new(TokenKind::Semicolon, span);
        }

        self.skip_whitespace_and_comments();

        // Check again after skipping whitespace (semicolon may have been triggered by newline)
        if self.pending_semicolon {
            self.pending_semicolon = false;
            let pos = self.pos();
            let span = Span::from_u32(pos, pos);
            self.prev_kind = Some(TokenKind::Semicolon);
            return Token::new(TokenKind::Semicolon, span);
        }

        let start = self.pos();

        let kind = match self.peek() {
            None => TokenKind::Eof,
            Some(c) => self.scan_token(c),
        };

        let span = Span::from_u32(start, self.pos());
        let token = Token::new(kind, span);

        // Track for semicolon insertion
        self.prev_kind = Some(kind);

        token
    }

    /// Peeks at the current character without consuming it.
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Peeks at the next character (one ahead of current).
    fn peek_next(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }

    /// Advances to the next character and returns it.
    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.local_pos += c.len_utf8() as u32;
        Some(c)
    }

    /// Advances if the current character matches the expected one.
    fn advance_if(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Skips whitespace and comments, handling semicolon insertion.
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') | Some('\r') => {
                    self.advance();
                }
                Some('\n') => {
                    // Check for semicolon insertion
                    if let Some(prev) = self.prev_kind {
                        if prev.can_end_statement() {
                            self.pending_semicolon = true;
                            self.advance();
                            return;
                        }
                    }
                    self.advance();
                }
                Some('/') => {
                    if self.peek_next() == Some('/') {
                        let has_newline = self.skip_line_comment();
                        // After line comment, check for semicolon insertion
                        if has_newline {
                            if let Some(prev) = self.prev_kind {
                                if prev.can_end_statement() {
                                    self.pending_semicolon = true;
                                    self.advance(); // consume the newline
                                    return;
                                }
                            }
                            self.advance(); // consume the newline
                        }
                    } else if self.peek_next() == Some('*') {
                        let has_newline = self.skip_block_comment();
                        // A newline inside a block comment has the same
                        // semicolon-insertion effect as an ordinary newline.
                        if has_newline && self.prev_kind.is_some_and(TokenKind::can_end_statement) {
                            self.pending_semicolon = true;
                            return;
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    /// Skips a line comment (// ...).
    /// Returns true if a newline was encountered (for semicolon insertion).
    fn skip_line_comment(&mut self) -> bool {
        self.advance(); // /
        self.advance(); // /
        while let Some(c) = self.peek() {
            if c == '\n' {
                return true;
            }
            self.advance();
        }
        false
    }

    /// Skips a block comment (`/* ... */`).
    /// Returns true when the comment contains at least one newline.
    fn skip_block_comment(&mut self) -> bool {
        let start = self.pos();
        self.advance(); // /
        self.advance(); // *

        let mut depth = 1;
        let mut has_newline = false;
        while depth > 0 {
            match self.peek() {
                None => {
                    self.diagnostics
                        .emit(SyntaxError::UnterminatedBlockComment.at(start..self.pos()));
                    return has_newline;
                }
                Some('*') if self.peek_next() == Some('/') => {
                    self.advance();
                    self.advance();
                    depth -= 1;
                }
                Some('/') if self.peek_next() == Some('*') => {
                    self.advance();
                    self.advance();
                    depth += 1;
                }
                Some('\n') => {
                    has_newline = true;
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }
        has_newline
    }

    /// Scans a single token.
    fn scan_token(&mut self, c: char) -> TokenKind {
        match c {
            // Single-character tokens
            '(' => {
                self.advance();
                TokenKind::LParen
            }
            ')' => {
                self.advance();
                TokenKind::RParen
            }
            '[' => {
                self.advance();
                TokenKind::LBracket
            }
            ']' => {
                self.advance();
                TokenKind::RBracket
            }
            '{' => {
                self.advance();
                TokenKind::LBrace
            }
            '}' => {
                self.advance();
                TokenKind::RBrace
            }
            ',' => {
                self.advance();
                TokenKind::Comma
            }
            ';' => {
                self.advance();
                TokenKind::Semicolon
            }
            '@' => {
                self.advance();
                TokenKind::At
            }

            // Dot or ellipsis
            '.' => {
                self.advance();
                if self.peek() == Some('.') && self.peek_next() == Some('.') {
                    self.advance();
                    self.advance();
                    TokenKind::Ellipsis
                } else if self.peek().is_some_and(|c| c.is_ascii_digit()) {
                    // Float starting with .
                    let start = self.local_pos.saturating_sub(1) as usize;
                    let kind = self.scan_float_after_dot();
                    if self.validate_numeric_separators(start) {
                        kind
                    } else {
                        TokenKind::Invalid
                    }
                } else {
                    TokenKind::Dot
                }
            }

            // Colon or :=
            ':' => {
                self.advance();
                if self.advance_if('=') {
                    TokenKind::ColonEq
                } else {
                    TokenKind::Colon
                }
            }

            // Plus, ++, +=
            '+' => {
                self.advance();
                if self.advance_if('+') {
                    TokenKind::PlusPlus
                } else if self.advance_if('=') {
                    TokenKind::PlusEq
                } else {
                    TokenKind::Plus
                }
            }

            // Minus, --, -=
            '-' => {
                self.advance();
                if self.advance_if('-') {
                    TokenKind::MinusMinus
                } else if self.advance_if('=') {
                    TokenKind::MinusEq
                } else {
                    TokenKind::Minus
                }
            }

            // Star, *=
            '*' => {
                self.advance();
                if self.advance_if('=') {
                    TokenKind::StarEq
                } else {
                    TokenKind::Star
                }
            }

            // Slash, /=
            '/' => {
                self.advance();
                if self.advance_if('=') {
                    TokenKind::SlashEq
                } else {
                    TokenKind::Slash
                }
            }

            // Percent, %=
            '%' => {
                self.advance();
                if self.advance_if('=') {
                    TokenKind::PercentEq
                } else {
                    TokenKind::Percent
                }
            }

            // Amp, &=, &&, &^, &^=
            '&' => {
                self.advance();
                if self.advance_if('&') {
                    TokenKind::AmpAmp
                } else if self.advance_if('^') {
                    if self.advance_if('=') {
                        TokenKind::AmpCaretEq
                    } else {
                        TokenKind::AmpCaret
                    }
                } else if self.advance_if('=') {
                    TokenKind::AmpEq
                } else {
                    TokenKind::Amp
                }
            }

            // Pipe, |=, ||
            '|' => {
                self.advance();
                if self.advance_if('|') {
                    TokenKind::PipePipe
                } else if self.advance_if('=') {
                    TokenKind::PipeEq
                } else {
                    TokenKind::Pipe
                }
            }

            // Caret, ^=
            '^' => {
                self.advance();
                if self.advance_if('=') {
                    TokenKind::CaretEq
                } else {
                    TokenKind::Caret
                }
            }

            // Less, <=, <<, <<=, <-
            '<' => {
                self.advance();
                if self.advance_if('-') {
                    TokenKind::Arrow
                } else if self.advance_if('<') {
                    if self.advance_if('=') {
                        TokenKind::ShlEq
                    } else {
                        TokenKind::Shl
                    }
                } else if self.advance_if('=') {
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }

            // Greater, >=, >>, >>=
            '>' => {
                self.advance();
                if self.advance_if('>') {
                    if self.advance_if('=') {
                        TokenKind::ShrEq
                    } else {
                        TokenKind::Shr
                    }
                } else if self.advance_if('=') {
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }

            // Equal, ==
            '=' => {
                self.advance();
                if self.advance_if('=') {
                    TokenKind::EqEq
                } else {
                    TokenKind::Eq
                }
            }

            // Not, !=
            '!' => {
                self.advance();
                if self.advance_if('=') {
                    TokenKind::NotEq
                } else {
                    TokenKind::Not
                }
            }

            // Question mark (error propagation)
            '?' => {
                self.advance();
                TokenKind::Question
            }

            // Tilde arrow (dynamic access)
            '~' => {
                self.advance();
                if self.advance_if('>') {
                    TokenKind::TildeArrow
                } else {
                    let start = self.pos() - 1;
                    self.diagnostics.emit(
                        SyntaxError::UnexpectedChar.at_with_message(
                            start..self.pos(),
                            "expected '>' after '~'".to_string(),
                        ),
                    );
                    TokenKind::Invalid
                }
            }

            // String literals
            '"' => self.scan_string(),
            '`' => self.scan_raw_string(),

            // Rune literals
            '\'' => self.scan_rune(),

            // Numbers
            '0'..='9' => self.scan_number(),

            // Identifiers and keywords
            c if is_identifier_start(c) => self.scan_ident(),

            // Invalid character
            _ => {
                let start = self.pos();
                self.advance();
                self.diagnostics.emit(
                    SyntaxError::UnexpectedChar.at_with_message(
                        start..self.pos(),
                        format!("unexpected character: {:?}", c),
                    ),
                );
                TokenKind::Invalid
            }
        }
    }

    /// Scans an identifier or keyword.
    fn scan_ident(&mut self) -> TokenKind {
        let start = self.local_pos as usize;

        while let Some(c) = self.peek() {
            if is_identifier_continue(c) {
                self.advance();
            } else {
                break;
            }
        }

        let text = &self.source[start..self.local_pos as usize];
        TokenKind::keyword(text).unwrap_or(TokenKind::Ident)
    }

    /// Scans a number literal (integer or float).
    fn scan_number(&mut self) -> TokenKind {
        let start = self.local_pos as usize;
        let kind = self.scan_number_inner();
        if self.validate_numeric_separators(start) {
            kind
        } else {
            TokenKind::Invalid
        }
    }

    fn scan_number_inner(&mut self) -> TokenKind {
        let first = self.advance().unwrap();

        if first == '0' {
            match self.peek() {
                Some('x') | Some('X') => return self.scan_hex_number(),
                Some('o') | Some('O') => return self.scan_octal_number(),
                Some('b') | Some('B') => return self.scan_binary_number(),
                Some('.') => {
                    self.advance();
                    return self.scan_float_after_dot();
                }
                Some('e') | Some('E') => {
                    return self.scan_float_exponent();
                }
                Some(c) if c.is_ascii_digit() => {
                    // Go-style octal: 0644
                    return self.scan_go_style_octal_number();
                }
                Some('_') => return self.scan_go_style_octal_number(),
                _ => return TokenKind::IntLit,
            }
        }

        self.scan_decimal_number()
    }

    /// Scans a decimal number (integer or float).
    fn scan_decimal_number(&mut self) -> TokenKind {
        self.scan_digits(10);

        match self.peek() {
            Some('.')
                if self
                    .peek_next()
                    .is_some_and(|c| c.is_ascii_digit() || c == 'e' || c == 'E') =>
            {
                self.advance();
                self.scan_float_after_dot()
            }
            Some('.') if self.peek_next() != Some('.') => {
                // Could be a float like "1." but not "1.."
                self.advance();
                self.scan_float_after_dot()
            }
            Some('e') | Some('E') => self.scan_float_exponent(),
            _ => TokenKind::IntLit,
        }
    }

    /// Scans a hexadecimal number.
    fn scan_hex_number(&mut self) -> TokenKind {
        self.advance(); // x or X

        // Check for hex float starting with dot: 0x.8p0
        if self.peek() == Some('.') {
            self.advance();
            if !self.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                let start = self.pos().saturating_sub(3);
                self.diagnostics
                    .emit(SyntaxError::HexFloatNoDigits.at(start..self.pos()));
                return TokenKind::Invalid;
            }
            self.scan_hex_digits();
            return self.scan_hex_exponent();
        }

        let has_digits = self.peek().is_some_and(|c| c.is_ascii_hexdigit())
            || self.peek() == Some('_') && self.peek_next().is_some_and(|c| c.is_ascii_hexdigit());
        if !has_digits {
            let start = self.pos().saturating_sub(2);
            self.diagnostics
                .emit(SyntaxError::HexNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.scan_hex_digits();

        // Check for hex float
        match self.peek() {
            Some('.') => {
                self.advance();
                self.scan_hex_digits();
                self.scan_hex_exponent()
            }
            Some('p') | Some('P') => self.scan_hex_exponent(),
            _ => TokenKind::IntLit,
        }
    }

    /// Scans an octal number (0o prefix style).
    fn scan_octal_number(&mut self) -> TokenKind {
        self.advance(); // o or O

        let has_digits = self.peek().is_some_and(|c| matches!(c, '0'..='7'))
            || self.peek() == Some('_') && self.peek_next().is_some_and(|c| matches!(c, '0'..='7'));
        if !has_digits {
            let start = self.pos().saturating_sub(2);
            self.diagnostics
                .emit(SyntaxError::OctalNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.scan_octal_digits();
        TokenKind::IntLit
    }

    /// Scans a Go-style octal number (0644 style, Go compatible).
    fn scan_go_style_octal_number(&mut self) -> TokenKind {
        self.scan_octal_digits();
        TokenKind::IntLit
    }

    /// Scans octal digits (0-7), emitting errors for invalid digits 8-9.
    fn scan_octal_digits(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '0'..='7' | '_' => {
                    self.advance();
                }
                '8' | '9' => {
                    let digit_start = self.pos();
                    self.advance();
                    self.diagnostics
                        .emit(SyntaxError::OctalInvalidDigit.at_with_message(
                            digit_start..self.pos(),
                            format!("invalid digit '{}' in octal literal", c),
                        ));
                }
                _ => break,
            }
        }
    }

    /// Scans a binary number.
    fn scan_binary_number(&mut self) -> TokenKind {
        self.advance(); // b or B

        let has_digits = self.peek().is_some_and(|c| matches!(c, '0' | '1'))
            || self.peek() == Some('_') && self.peek_next().is_some_and(|c| matches!(c, '0' | '1'));
        if !has_digits {
            let start = self.pos().saturating_sub(2);
            self.diagnostics
                .emit(SyntaxError::BinaryNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        while let Some(c) = self.peek() {
            match c {
                '0' | '1' | '_' => {
                    self.advance();
                }
                '2'..='9' => {
                    let digit_start = self.pos();
                    self.advance();
                    self.diagnostics
                        .emit(SyntaxError::BinaryInvalidDigit.at_with_message(
                            digit_start..self.pos(),
                            format!("invalid digit '{}' in binary literal", c),
                        ));
                }
                _ => break,
            }
        }

        TokenKind::IntLit
    }

    /// Scans decimal digits.
    fn scan_digits(&mut self, radix: u32) {
        while let Some(c) = self.peek() {
            if c == '_' || c.is_digit(radix) {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Scans hexadecimal digits.
    fn scan_hex_digits(&mut self) {
        while let Some(c) = self.peek() {
            if c == '_' || c.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn validate_numeric_separators(&mut self, start: usize) -> bool {
        let end = self.local_pos as usize;
        let literal = &self.source[start..end];
        let bytes = literal.as_bytes();
        let main_radix = if literal.starts_with("0x") || literal.starts_with("0X") {
            16
        } else if literal.starts_with("0b") || literal.starts_with("0B") {
            2
        } else if literal.starts_with("0o")
            || literal.starts_with("0O")
            || literal.starts_with('0') && literal.len() > 1 && !literal.contains(['.', 'e', 'E'])
        {
            8
        } else {
            10
        };
        let exponent = if main_radix == 16 {
            bytes.iter().position(|b| matches!(b, b'p' | b'P'))
        } else {
            bytes.iter().position(|b| matches!(b, b'e' | b'E'))
        };

        let mut valid = true;
        for (index, byte) in bytes.iter().enumerate() {
            if *byte != b'_' {
                continue;
            }

            let radix = if exponent.is_some_and(|position| index > position) {
                10
            } else {
                main_radix
            };
            let previous_is_digit = index
                .checked_sub(1)
                .and_then(|i| bytes.get(i))
                .is_some_and(|b| is_ascii_digit_for_radix(*b, radix));
            let next_is_digit = bytes
                .get(index + 1)
                .is_some_and(|b| is_ascii_digit_for_radix(*b, radix));

            if !(previous_is_digit && next_is_digit) {
                valid = false;
                let separator_start = self.base + start as u32 + index as u32;
                self.diagnostics.emit(
                    SyntaxError::InvalidNumericSeparator.at(separator_start..separator_start + 1),
                );
            }
        }
        valid
    }

    /// Scans the fractional part and optional exponent of a float.
    fn scan_float_after_dot(&mut self) -> TokenKind {
        self.scan_digits(10);

        if self.peek() == Some('e') || self.peek() == Some('E') {
            self.scan_float_exponent()
        } else {
            TokenKind::FloatLit
        }
    }

    /// Scans a decimal float exponent.
    fn scan_float_exponent(&mut self) -> TokenKind {
        self.advance(); // e or E

        if self.peek() == Some('+') || self.peek() == Some('-') {
            self.advance();
        }

        if !self.peek().is_some_and(|c| c.is_ascii_digit()) {
            let start = self.pos().saturating_sub(1);
            self.diagnostics
                .emit(SyntaxError::ExponentNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.scan_digits(10);
        TokenKind::FloatLit
    }

    /// Scans a hex float exponent (required for hex floats).
    fn scan_hex_exponent(&mut self) -> TokenKind {
        if self.peek() != Some('p') && self.peek() != Some('P') {
            let start = self.pos().saturating_sub(1);
            self.diagnostics
                .emit(SyntaxError::HexFloatNoExponent.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.advance(); // p or P

        if self.peek().is_some_and(|c| matches!(c, '+' | '-')) {
            self.advance();
        }

        if !self.peek().is_some_and(|c| c.is_ascii_digit()) {
            let start = self.pos().saturating_sub(1);
            self.diagnostics
                .emit(SyntaxError::ExponentNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.scan_digits(10);
        TokenKind::FloatLit
    }

    /// Scans a string literal.
    fn scan_string(&mut self) -> TokenKind {
        let start = self.pos();
        self.advance(); // opening "

        loop {
            match self.peek() {
                None | Some('\n') => {
                    self.diagnostics
                        .emit(SyntaxError::UnterminatedString.at(start..self.pos()));
                    return TokenKind::Invalid;
                }
                Some('"') => {
                    self.advance();
                    return TokenKind::StringLit;
                }
                Some('\\') => {
                    self.advance();
                    self.scan_escape_sequence();
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Scans a raw string literal.
    fn scan_raw_string(&mut self) -> TokenKind {
        let start = self.pos();
        self.advance(); // opening `

        loop {
            match self.peek() {
                None => {
                    self.diagnostics
                        .emit(SyntaxError::UnterminatedRawString.at(start..self.pos()));
                    return TokenKind::Invalid;
                }
                Some('`') => {
                    self.advance();
                    return TokenKind::RawStringLit;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Scans a rune literal.
    fn scan_rune(&mut self) -> TokenKind {
        let start = self.pos();
        self.advance(); // opening '

        match self.peek() {
            None | Some('\n') => {
                self.diagnostics
                    .emit(SyntaxError::UnterminatedRune.at(start..self.pos()));
                return TokenKind::Invalid;
            }
            Some('\'') => {
                self.advance();
                self.diagnostics
                    .emit(SyntaxError::EmptyRune.at(start..self.pos()));
                return TokenKind::Invalid;
            }
            Some('\\') => {
                self.advance();
                self.scan_escape_sequence();
            }
            _ => {
                self.advance();
            }
        }

        if self.peek() != Some('\'') {
            // Multi-character rune
            while self.peek() != Some('\'') && self.peek() != Some('\n') && self.peek().is_some() {
                if self.peek() == Some('\\') {
                    self.advance();
                    self.scan_escape_sequence();
                } else {
                    self.advance();
                }
            }

            if self.peek() == Some('\'') {
                self.advance();
            }

            self.diagnostics
                .emit(SyntaxError::MultiCharRune.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.advance(); // closing '
        TokenKind::RuneLit
    }

    /// Scans an escape sequence.
    fn scan_escape_sequence(&mut self) {
        let start = self.pos().saturating_sub(1);

        match self.peek() {
            Some('a') | Some('b') | Some('f') | Some('n') | Some('r') | Some('t') | Some('v')
            | Some('\\') | Some('\'') | Some('"') => {
                self.advance();
            }
            Some('x') => {
                self.advance();
                self.scan_hex_escape(2, start);
            }
            Some('u') => {
                self.advance();
                self.scan_hex_escape(4, start);
            }
            Some('U') => {
                self.advance();
                self.scan_hex_escape(8, start);
            }
            Some(c) if c.is_ascii_digit() => {
                // Octal escape
                self.scan_octal_escape(start);
            }
            Some(c) => {
                self.advance();
                self.diagnostics
                    .emit(SyntaxError::UnknownEscape.at_with_message(
                        start..self.pos(),
                        format!("unknown escape sequence: \\{}", c),
                    ));
            }
            None => {
                self.diagnostics
                    .emit(SyntaxError::UnterminatedEscape.at(start..self.pos()));
            }
        }
    }

    /// Scans a hex escape sequence with the given number of digits.
    fn scan_hex_escape(&mut self, digits: usize, start: u32) {
        let mut value = 0u32;
        for _ in 0..digits {
            match self.peek() {
                Some(c) if c.is_ascii_hexdigit() => {
                    value = value * 16 + c.to_digit(16).unwrap();
                    self.advance();
                }
                _ => {
                    self.diagnostics
                        .emit(SyntaxError::EscapeHexDigits.at_with_message(
                            start..self.pos(),
                            format!("escape sequence requires {} hex digits", digits),
                        ));
                    return;
                }
            }
        }

        if digits > 2 && char::from_u32(value).is_none() {
            self.diagnostics
                .emit(SyntaxError::EscapeInvalidUnicodeScalar.at(start..self.pos()));
        }
    }

    /// Scans an octal escape sequence (exactly 3 digits).
    fn scan_octal_escape(&mut self, start: u32) {
        let mut value = 0u32;
        for _ in 0..3 {
            match self.peek() {
                Some(c @ '0'..='7') => {
                    value = value * 8 + c.to_digit(8).unwrap();
                    self.advance();
                }
                _ => {
                    self.diagnostics
                        .emit(SyntaxError::EscapeOctalDigits.at(start..self.pos()));
                    return;
                }
            }
        }

        if value > u8::MAX as u32 {
            self.diagnostics
                .emit(SyntaxError::EscapeOctalValue.at(start..self.pos()));
        }
    }
}

fn is_ascii_digit_for_radix(byte: u8, radix: u32) -> bool {
    match radix {
        2 => matches!(byte, b'0' | b'1'),
        8 => matches!(byte, b'0'..=b'7'),
        10 => byte.is_ascii_digit(),
        16 => byte.is_ascii_hexdigit(),
        _ => false,
    }
}

#[cfg(test)]
mod tests;
