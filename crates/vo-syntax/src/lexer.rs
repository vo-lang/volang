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

use vo_common::span::Span;
use vo_common::diagnostics::DiagnosticSink;

use crate::errors::SyntaxError;
use crate::token::{Token, TokenKind};

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
    diagnostics: DiagnosticSink,
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer for the given source with a base offset.
    /// 
    /// The base offset should come from `SourceMap::file_base()` to ensure
    /// all positions are globally unique.
    pub fn new(source: &'src str, base: u32) -> Self {
        Self {
            source,
            chars: source.chars(),
            base,
            local_pos: 0,
            prev_kind: None,
            pending_semicolon: false,
            diagnostics: DiagnosticSink::new(),
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
        std::mem::take(&mut self.diagnostics)
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
        (tokens, self.diagnostics)
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
                        self.skip_block_comment();
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

    /// Skips a block comment (/* ... */).
    fn skip_block_comment(&mut self) {
        let start = self.pos();
        self.advance(); // /
        self.advance(); // *
        
        let mut depth = 1;
        while depth > 0 {
            match self.peek() {
                None => {
                    self.diagnostics.emit(SyntaxError::UnterminatedBlockComment.at(start..self.pos()));
                    return;
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
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Scans a single token.
    fn scan_token(&mut self, c: char) -> TokenKind {
        match c {
            // Single-character tokens
            '(' => { self.advance(); TokenKind::LParen }
            ')' => { self.advance(); TokenKind::RParen }
            '[' => { self.advance(); TokenKind::LBracket }
            ']' => { self.advance(); TokenKind::RBracket }
            '{' => { self.advance(); TokenKind::LBrace }
            '}' => { self.advance(); TokenKind::RBrace }
            ',' => { self.advance(); TokenKind::Comma }
            ';' => { self.advance(); TokenKind::Semicolon }
            '@' => { self.advance(); TokenKind::At }

            // Dot or ellipsis
            '.' => {
                self.advance();
                if self.peek() == Some('.') && self.peek_next() == Some('.') {
                    self.advance();
                    self.advance();
                    TokenKind::Ellipsis
                } else if self.peek().map_or(false, |c| c.is_ascii_digit()) {
                    // Float starting with .
                    self.scan_float_after_dot()
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
                    self.diagnostics.emit(SyntaxError::UnexpectedChar.at_with_message(start..self.pos(), "expected '>' after '~'".to_string()));
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
            c if is_ident_start(c) => self.scan_ident(),

            // Invalid character
            _ => {
                let start = self.pos();
                self.advance();
                self.diagnostics.emit(SyntaxError::UnexpectedChar.at_with_message(start..self.pos(), format!("unexpected character: {:?}", c)));
                TokenKind::Invalid
            }
        }
    }

    /// Scans an identifier or keyword.
    fn scan_ident(&mut self) -> TokenKind {
        let start = self.local_pos as usize;
        
        while let Some(c) = self.peek() {
            if is_ident_continue(c) {
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
                    // Legacy octal or decimal with leading zero
                    return self.scan_decimal_number();
                }
                _ => return TokenKind::IntLit,
            }
        }

        self.scan_decimal_number()
    }

    /// Scans a decimal number (integer or float).
    fn scan_decimal_number(&mut self) -> TokenKind {
        self.scan_digits(10);

        match self.peek() {
            Some('.') if self.peek_next().map_or(false, |c| c.is_ascii_digit() || c == 'e' || c == 'E') => {
                self.advance();
                self.scan_float_after_dot()
            }
            Some('.') if self.peek_next() != Some('.') => {
                // Could be a float like "1." but not "1.."
                self.advance();
                self.scan_float_after_dot()
            }
            Some('e') | Some('E') => {
                self.scan_float_exponent()
            }
            _ => TokenKind::IntLit,
        }
    }

    /// Scans a hexadecimal number.
    fn scan_hex_number(&mut self) -> TokenKind {
        self.advance(); // x or X
        
        // Check for hex float starting with dot: 0x.8p0
        if self.peek() == Some('.') {
            self.advance();
            if !self.peek().map_or(false, |c| c.is_ascii_hexdigit()) {
                let start = self.pos().saturating_sub(3);
                self.diagnostics.emit(SyntaxError::HexFloatNoDigits.at(start..self.pos()));
                return TokenKind::Invalid;
            }
            self.scan_hex_digits();
            return self.scan_hex_exponent();
        }
        
        if !self.peek().map_or(false, |c| c.is_ascii_hexdigit()) {
            let start = self.pos().saturating_sub(2);
            self.diagnostics.emit(SyntaxError::HexNoDigits.at(start..self.pos()));
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
            Some('p') | Some('P') => {
                self.scan_hex_exponent()
            }
            _ => TokenKind::IntLit,
        }
    }

    /// Scans an octal number.
    fn scan_octal_number(&mut self) -> TokenKind {
        self.advance(); // o or O
        
        if !self.peek().map_or(false, |c| matches!(c, '0'..='7')) {
            let start = self.pos().saturating_sub(2);
            self.diagnostics.emit(SyntaxError::OctalNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        while let Some(c) = self.peek() {
            match c {
                '0'..='7' | '_' => { self.advance(); }
                '8' | '9' => {
                    let digit_start = self.pos();
                    self.advance();
                    self.diagnostics.emit(SyntaxError::OctalInvalidDigit.at_with_message(digit_start..self.pos(), format!("invalid digit '{}' in octal literal", c)));
                }
                _ => break,
            }
        }

        TokenKind::IntLit
    }

    /// Scans a binary number.
    fn scan_binary_number(&mut self) -> TokenKind {
        self.advance(); // b or B
        
        if !self.peek().map_or(false, |c| matches!(c, '0' | '1')) {
            let start = self.pos().saturating_sub(2);
            self.diagnostics.emit(SyntaxError::BinaryNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        while let Some(c) = self.peek() {
            match c {
                '0' | '1' | '_' => { self.advance(); }
                '2'..='9' => {
                    let digit_start = self.pos();
                    self.advance();
                    self.diagnostics.emit(SyntaxError::BinaryInvalidDigit.at_with_message(digit_start..self.pos(), format!("invalid digit '{}' in binary literal", c)));
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

        if !self.peek().map_or(false, |c| c.is_ascii_digit()) {
            let start = self.pos().saturating_sub(1);
            self.diagnostics.emit(SyntaxError::ExponentNoDigits.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.scan_digits(10);
        TokenKind::FloatLit
    }

    /// Scans a hex float exponent (required for hex floats).
    fn scan_hex_exponent(&mut self) -> TokenKind {
        if self.peek() != Some('p') && self.peek() != Some('P') {
            let start = self.pos().saturating_sub(1);
            self.diagnostics.emit(SyntaxError::HexFloatNoExponent.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.advance(); // p or P

        if self.peek() == Some('+') || self.peek() == Some('-') {
            self.advance();
        }

        if !self.peek().map_or(false, |c| c.is_ascii_digit()) {
            let start = self.pos().saturating_sub(1);
            self.diagnostics.emit(SyntaxError::ExponentNoDigits.at(start..self.pos()));
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
                    self.diagnostics.emit(SyntaxError::UnterminatedString.at(start..self.pos()));
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
                    self.diagnostics.emit(SyntaxError::UnterminatedRawString.at(start..self.pos()));
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
                self.diagnostics.emit(SyntaxError::UnterminatedRune.at(start..self.pos()));
                return TokenKind::Invalid;
            }
            Some('\'') => {
                self.advance();
                self.diagnostics.emit(SyntaxError::EmptyRune.at(start..self.pos()));
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
            
            self.diagnostics.emit(SyntaxError::MultiCharRune.at(start..self.pos()));
            return TokenKind::Invalid;
        }

        self.advance(); // closing '
        TokenKind::RuneLit
    }

    /// Scans an escape sequence.
    fn scan_escape_sequence(&mut self) {
        let start = self.pos().saturating_sub(1);
        
        match self.peek() {
            Some('a') | Some('b') | Some('f') | Some('n') | Some('r') | 
            Some('t') | Some('v') | Some('\\') | Some('\'') | Some('"') => {
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
                self.diagnostics.emit(SyntaxError::UnknownEscape.at_with_message(start..self.pos(), format!("unknown escape sequence: \\{}", c)));
            }
            None => {
                self.diagnostics.emit(SyntaxError::UnterminatedEscape.at(start..self.pos()));
            }
        }
    }

    /// Scans a hex escape sequence with the given number of digits.
    fn scan_hex_escape(&mut self, digits: usize, start: u32) {
        for _ in 0..digits {
            match self.peek() {
                Some(c) if c.is_ascii_hexdigit() => {
                    self.advance();
                }
                _ => {
                    self.diagnostics.emit(SyntaxError::EscapeHexDigits.at_with_message(start..self.pos(), format!("escape sequence requires {} hex digits", digits)));
                    return;
                }
            }
        }
    }

    /// Scans an octal escape sequence (exactly 3 digits).
    fn scan_octal_escape(&mut self, start: u32) {
        for _ in 0..3 {
            match self.peek() {
                Some(c) if matches!(c, '0'..='7') => {
                    self.advance();
                }
                _ => {
                    self.diagnostics.emit(SyntaxError::EscapeOctalDigits.at(start..self.pos()));
                    return;
                }
            }
        }
    }
}

/// Returns true if the character can start an identifier.
fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

/// Returns true if the character can continue an identifier.
fn is_ident_continue(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<TokenKind> {
        let lexer = Lexer::new(source, 0);
        let (tokens, _) = lexer.collect_tokens();
        tokens.into_iter().map(|t| t.kind).collect()
    }

    fn lex_with_errors(source: &str) -> (Vec<TokenKind>, DiagnosticSink) {
        let lexer = Lexer::new(source, 0);
        let (tokens, diags) = lexer.collect_tokens();
        (tokens.into_iter().map(|t| t.kind).collect(), diags)
    }

    #[test]
    fn test_empty() {
        assert_eq!(lex(""), vec![TokenKind::Eof]);
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(lex("   \t\r  "), vec![TokenKind::Eof]);
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(lex("foo"), vec![TokenKind::Ident, TokenKind::Eof]);
        assert_eq!(lex("_bar"), vec![TokenKind::Ident, TokenKind::Eof]);
        assert_eq!(lex("baz123"), vec![TokenKind::Ident, TokenKind::Eof]);
        assert_eq!(lex("_"), vec![TokenKind::Ident, TokenKind::Eof]);
    }

    #[test]
    fn test_keywords() {
        assert_eq!(lex("func"), vec![TokenKind::Func, TokenKind::Eof]);
        assert_eq!(lex("struct"), vec![TokenKind::Struct, TokenKind::Eof]);
        assert_eq!(lex("object"), vec![TokenKind::Ident, TokenKind::Eof]);  // object is no longer a keyword
        assert_eq!(lex("return"), vec![TokenKind::Return, TokenKind::Eof]);
    }

    #[test]
    fn test_integers() {
        assert_eq!(lex("0"), vec![TokenKind::IntLit, TokenKind::Eof]);
        assert_eq!(lex("42"), vec![TokenKind::IntLit, TokenKind::Eof]);
        assert_eq!(lex("1_000_000"), vec![TokenKind::IntLit, TokenKind::Eof]);
    }

    #[test]
    fn test_hex_integers() {
        assert_eq!(lex("0x1F"), vec![TokenKind::IntLit, TokenKind::Eof]);
        assert_eq!(lex("0XFF"), vec![TokenKind::IntLit, TokenKind::Eof]);
        assert_eq!(lex("0x1_2_3"), vec![TokenKind::IntLit, TokenKind::Eof]);
    }

    #[test]
    fn test_octal_integers() {
        assert_eq!(lex("0o17"), vec![TokenKind::IntLit, TokenKind::Eof]);
        assert_eq!(lex("0O77"), vec![TokenKind::IntLit, TokenKind::Eof]);
    }

    #[test]
    fn test_binary_integers() {
        assert_eq!(lex("0b101"), vec![TokenKind::IntLit, TokenKind::Eof]);
        assert_eq!(lex("0B1111"), vec![TokenKind::IntLit, TokenKind::Eof]);
    }

    #[test]
    fn test_floats() {
        assert_eq!(lex("3.14"), vec![TokenKind::FloatLit, TokenKind::Eof]);
        assert_eq!(lex("1."), vec![TokenKind::FloatLit, TokenKind::Eof]);
        assert_eq!(lex(".5"), vec![TokenKind::FloatLit, TokenKind::Eof]);
        assert_eq!(lex("1e10"), vec![TokenKind::FloatLit, TokenKind::Eof]);
        assert_eq!(lex("1E-5"), vec![TokenKind::FloatLit, TokenKind::Eof]);
        assert_eq!(lex("1.5e+3"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    }

    #[test]
    fn test_hex_floats() {
        assert_eq!(lex("0x1p0"), vec![TokenKind::FloatLit, TokenKind::Eof]);
        assert_eq!(lex("0x1.2p3"), vec![TokenKind::FloatLit, TokenKind::Eof]);
        assert_eq!(lex("0x.8p0"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    }

    #[test]
    fn test_strings() {
        assert_eq!(lex(r#""hello""#), vec![TokenKind::StringLit, TokenKind::Eof]);
        assert_eq!(lex(r#""hello\nworld""#), vec![TokenKind::StringLit, TokenKind::Eof]);
        assert_eq!(lex(r#""""#), vec![TokenKind::StringLit, TokenKind::Eof]);
    }

    #[test]
    fn test_raw_strings() {
        assert_eq!(lex("`hello`"), vec![TokenKind::RawStringLit, TokenKind::Eof]);
        assert_eq!(lex("`hello\nworld`"), vec![TokenKind::RawStringLit, TokenKind::Eof]);
    }

    #[test]
    fn test_runes() {
        assert_eq!(lex("'a'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
        assert_eq!(lex("'\\n'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
        assert_eq!(lex("'\\x41'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
        assert_eq!(lex("'\\u0041'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
    }

    #[test]
    fn test_operators() {
        assert_eq!(lex("+"), vec![TokenKind::Plus, TokenKind::Eof]);
        assert_eq!(lex("-"), vec![TokenKind::Minus, TokenKind::Eof]);
        assert_eq!(lex("*"), vec![TokenKind::Star, TokenKind::Eof]);
        assert_eq!(lex("/"), vec![TokenKind::Slash, TokenKind::Eof]);
        assert_eq!(lex("=="), vec![TokenKind::EqEq, TokenKind::Eof]);
        assert_eq!(lex("!="), vec![TokenKind::NotEq, TokenKind::Eof]);
        assert_eq!(lex("<="), vec![TokenKind::LtEq, TokenKind::Eof]);
        assert_eq!(lex(">="), vec![TokenKind::GtEq, TokenKind::Eof]);
        assert_eq!(lex("&&"), vec![TokenKind::AmpAmp, TokenKind::Eof]);
        assert_eq!(lex("||"), vec![TokenKind::PipePipe, TokenKind::Eof]);
        assert_eq!(lex("<-"), vec![TokenKind::Arrow, TokenKind::Eof]);
        assert_eq!(lex("++"), vec![TokenKind::PlusPlus, TokenKind::Eof]);
        assert_eq!(lex("--"), vec![TokenKind::MinusMinus, TokenKind::Eof]);
        assert_eq!(lex(":="), vec![TokenKind::ColonEq, TokenKind::Eof]);
        assert_eq!(lex("..."), vec![TokenKind::Ellipsis, TokenKind::Eof]);
        assert_eq!(lex("&^"), vec![TokenKind::AmpCaret, TokenKind::Eof]);
    }

    #[test]
    fn test_assignment_operators() {
        assert_eq!(lex("="), vec![TokenKind::Eq, TokenKind::Eof]);
        assert_eq!(lex("+="), vec![TokenKind::PlusEq, TokenKind::Eof]);
        assert_eq!(lex("-="), vec![TokenKind::MinusEq, TokenKind::Eof]);
        assert_eq!(lex("*="), vec![TokenKind::StarEq, TokenKind::Eof]);
        assert_eq!(lex("/="), vec![TokenKind::SlashEq, TokenKind::Eof]);
        assert_eq!(lex("%="), vec![TokenKind::PercentEq, TokenKind::Eof]);
        assert_eq!(lex("<<="), vec![TokenKind::ShlEq, TokenKind::Eof]);
        assert_eq!(lex(">>="), vec![TokenKind::ShrEq, TokenKind::Eof]);
        assert_eq!(lex("&="), vec![TokenKind::AmpEq, TokenKind::Eof]);
        assert_eq!(lex("|="), vec![TokenKind::PipeEq, TokenKind::Eof]);
        assert_eq!(lex("^="), vec![TokenKind::CaretEq, TokenKind::Eof]);
        assert_eq!(lex("&^="), vec![TokenKind::AmpCaretEq, TokenKind::Eof]);
    }

    #[test]
    fn test_delimiters() {
        assert_eq!(lex("()"), vec![TokenKind::LParen, TokenKind::RParen, TokenKind::Eof]);
        assert_eq!(lex("[]"), vec![TokenKind::LBracket, TokenKind::RBracket, TokenKind::Eof]);
        assert_eq!(lex("{}"), vec![TokenKind::LBrace, TokenKind::RBrace, TokenKind::Eof]);
        assert_eq!(lex(","), vec![TokenKind::Comma, TokenKind::Eof]);
        assert_eq!(lex(":"), vec![TokenKind::Colon, TokenKind::Eof]);
        assert_eq!(lex("."), vec![TokenKind::Dot, TokenKind::Eof]);
        assert_eq!(lex(";"), vec![TokenKind::Semicolon, TokenKind::Eof]);
    }

    #[test]
    fn test_semicolon_insertion() {
        // After identifier
        assert_eq!(lex("foo\nbar"), vec![
            TokenKind::Ident, TokenKind::Semicolon,
            TokenKind::Ident, TokenKind::Eof
        ]);
        
        // After literal
        assert_eq!(lex("42\n"), vec![
            TokenKind::IntLit, TokenKind::Semicolon, TokenKind::Eof
        ]);
        
        // After return
        assert_eq!(lex("return\n"), vec![
            TokenKind::Return, TokenKind::Semicolon, TokenKind::Eof
        ]);
        
        // After )
        assert_eq!(lex(")\n"), vec![
            TokenKind::RParen, TokenKind::Semicolon, TokenKind::Eof
        ]);
        
        // After }
        assert_eq!(lex("}\n"), vec![
            TokenKind::RBrace, TokenKind::Semicolon, TokenKind::Eof
        ]);
        
        // No insertion after +
        assert_eq!(lex("+\n"), vec![TokenKind::Plus, TokenKind::Eof]);
        
        // No insertion after {
        assert_eq!(lex("{\n"), vec![TokenKind::LBrace, TokenKind::Eof]);
    }

    #[test]
    fn test_line_comments() {
        assert_eq!(lex("// comment"), vec![TokenKind::Eof]);
        assert_eq!(lex("foo // comment"), vec![TokenKind::Ident, TokenKind::Eof]);
        assert_eq!(lex("foo // comment\nbar"), vec![
            TokenKind::Ident, TokenKind::Semicolon,
            TokenKind::Ident, TokenKind::Eof
        ]);
    }

    #[test]
    fn test_block_comments() {
        assert_eq!(lex("/* comment */"), vec![TokenKind::Eof]);
        assert_eq!(lex("foo /* comment */ bar"), vec![
            TokenKind::Ident, TokenKind::Ident, TokenKind::Eof
        ]);
        assert_eq!(lex("/* nested /* comment */ */"), vec![TokenKind::Eof]);
    }

    #[test]
    fn test_complex_expression() {
        let tokens = lex("x := 1 + 2 * 3");
        assert_eq!(tokens, vec![
            TokenKind::Ident,
            TokenKind::ColonEq,
            TokenKind::IntLit,
            TokenKind::Plus,
            TokenKind::IntLit,
            TokenKind::Star,
            TokenKind::IntLit,
            TokenKind::Eof,
        ]);
    }

    #[test]
    fn test_function_declaration() {
        let tokens = lex("func main() int { return 0 }");
        assert_eq!(tokens, vec![
            TokenKind::Func,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Ident,
            TokenKind::LBrace,
            TokenKind::Return,
            TokenKind::IntLit,
            TokenKind::RBrace,
            TokenKind::Eof,
        ]);
    }

    #[test]
    fn test_error_unterminated_string() {
        let (tokens, diags) = lex_with_errors("\"hello");
        assert!(tokens.contains(&TokenKind::Invalid));
        assert!(diags.has_errors());
    }

    #[test]
    fn test_error_unterminated_rune() {
        let (tokens, diags) = lex_with_errors("'a");
        assert!(tokens.contains(&TokenKind::Invalid));
        assert!(diags.has_errors());
    }

    #[test]
    fn test_error_empty_rune() {
        let (tokens, diags) = lex_with_errors("''");
        assert!(tokens.contains(&TokenKind::Invalid));
        assert!(diags.has_errors());
    }

    #[test]
    fn test_error_invalid_hex() {
        let (tokens, diags) = lex_with_errors("0x");
        assert!(tokens.contains(&TokenKind::Invalid));
        assert!(diags.has_errors());
    }

    #[test]
    fn test_error_invalid_binary() {
        let (tokens, diags) = lex_with_errors("0b");
        assert!(tokens.contains(&TokenKind::Invalid));
        assert!(diags.has_errors());
    }

    #[test]
    fn test_error_invalid_octal() {
        let (tokens, diags) = lex_with_errors("0o");
        assert!(tokens.contains(&TokenKind::Invalid));
        assert!(diags.has_errors());
    }

    #[test]
    fn test_shift_operators() {
        assert_eq!(lex("<<"), vec![TokenKind::Shl, TokenKind::Eof]);
        assert_eq!(lex(">>"), vec![TokenKind::Shr, TokenKind::Eof]);
    }

    #[test]
    fn test_channel_type() {
        // chan<- should be parsed as chan, <-
        let tokens = lex("chan<-");
        assert_eq!(tokens, vec![TokenKind::Chan, TokenKind::Arrow, TokenKind::Eof]);
        
        // <-chan should be parsed as <-, chan
        let tokens = lex("<-chan");
        assert_eq!(tokens, vec![TokenKind::Arrow, TokenKind::Chan, TokenKind::Eof]);
    }

    #[test]
    fn test_global_positions() {
        // Test that positions are offset by base
        let lexer = Lexer::new("foo", 100);
        let (tokens, _) = lexer.collect_tokens();
        
        // First token "foo" should be at positions 100-103
        assert_eq!(tokens[0].span.start.0, 100);
        assert_eq!(tokens[0].span.end.0, 103);
    }

    // ========== Error Handling Tokens ==========

    #[test]
    fn test_fail_keyword() {
        assert_eq!(lex("fail"), vec![TokenKind::Fail, TokenKind::Eof]);
        // fail is a keyword, not an identifier
        assert_ne!(lex("fail"), vec![TokenKind::Ident, TokenKind::Eof]);
    }

    #[test]
    fn test_errdefer_keyword() {
        assert_eq!(lex("errdefer"), vec![TokenKind::Errdefer, TokenKind::Eof]);
        // errdefer is a keyword, not an identifier
        assert_ne!(lex("errdefer"), vec![TokenKind::Ident, TokenKind::Eof]);
    }

    #[test]
    fn test_question_operator() {
        assert_eq!(lex("?"), vec![TokenKind::Question, TokenKind::Eof]);
    }

    #[test]
    fn test_fail_statement() {
        // fail expr
        assert_eq!(
            lex("fail err"),
            vec![TokenKind::Fail, TokenKind::Ident, TokenKind::Eof]
        );
    }

    #[test]
    fn test_errdefer_statement() {
        // errdefer cleanup()
        assert_eq!(
            lex("errdefer cleanup()"),
            vec![
                TokenKind::Errdefer,
                TokenKind::Ident,
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_try_unwrap_expression() {
        // getValue()?
        assert_eq!(
            lex("getValue()?"),
            vec![
                TokenKind::Ident,
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::Question,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_question_after_call() {
        // result := foo()?
        assert_eq!(
            lex("result := foo()?"),
            vec![
                TokenKind::Ident,
                TokenKind::ColonEq,
                TokenKind::Ident,
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::Question,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_chained_try_unwrap() {
        // a()?.b()?
        assert_eq!(
            lex("a()?.b()?"),
            vec![
                TokenKind::Ident,
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::Question,
                TokenKind::Dot,
                TokenKind::Ident,
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::Question,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_fail_with_error_literal() {
        // fail errors.New("failed")
        assert_eq!(
            lex(r#"fail errors.New("failed")"#),
            vec![
                TokenKind::Fail,
                TokenKind::Ident,
                TokenKind::Dot,
                TokenKind::Ident,
                TokenKind::LParen,
                TokenKind::StringLit,
                TokenKind::RParen,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_error_handling_in_function() {
        // Complete error handling pattern
        let code = r#"func foo() error {
    errdefer cleanup()
    result := bar()?
    if result == nil {
        fail ErrNotFound
    }
    return nil
}"#;
        let tokens = lex(code);
        
        // Check key tokens are present
        assert!(tokens.contains(&TokenKind::Errdefer));
        assert!(tokens.contains(&TokenKind::Question));
        assert!(tokens.contains(&TokenKind::Fail));
    }

    #[test]
    fn test_fail_not_identifier_prefix() {
        // "failure" should be an identifier, not fail + ure
        assert_eq!(lex("failure"), vec![TokenKind::Ident, TokenKind::Eof]);
        assert_eq!(lex("failing"), vec![TokenKind::Ident, TokenKind::Eof]);
    }

    #[test]
    fn test_errdefer_not_identifier_prefix() {
        // "errdeferred" should be an identifier
        assert_eq!(lex("errdeferred"), vec![TokenKind::Ident, TokenKind::Eof]);
    }
}
