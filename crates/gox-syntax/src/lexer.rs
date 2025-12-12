//! Lexer for the GoX language.
//!
//! Converts source text into a stream of tokens according to GoX Language Specification §3.
//!
//! # Features
//! - Single-line (`//`) and multi-line (`/* */`) comments
//! - Automatic semicolon insertion (§3.6)
//! - All operators and keywords from the specification
//! - String escape sequences: `\n`, `\t`, `\\`, `\"`
//!
//! # Example
//! ```
//! use gox_syntax::lexer::Lexer;
//! use gox_syntax::token::TokenKind;
//!
//! let mut lexer = Lexer::new("var x = 42;");
//! let token = lexer.next_token();
//! assert!(matches!(token.kind, TokenKind::Var));
//! ```

use crate::token::{Token, TokenKind};
use gox_common::Span;

/// The GoX lexer.
pub struct Lexer<'a> {
    /// The source code being lexed.
    input: &'a str,
    /// Current byte position in the input.
    pos: usize,
    /// Next byte position to read.
    read_pos: usize,
    /// Current character (None if at EOF).
    ch: Option<char>,
    /// Previous token kind (for semicolon insertion).
    prev_kind: Option<TokenKind>,
    /// Whether we just passed a newline (for semicolon insertion).
    at_newline: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given input.
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
            pos: 0,
            read_pos: 0,
            ch: None,
            prev_kind: None,
            at_newline: false,
        };
        lexer.read_char();
        lexer
    }

    /// Read the next character, advancing positions.
    fn read_char(&mut self) {
        self.ch = self.input[self.read_pos..].chars().next();
        self.pos = self.read_pos;
        if let Some(c) = self.ch {
            self.read_pos += c.len_utf8();
        }
    }

    /// Peek at the next character without consuming.
    fn peek_char(&self) -> Option<char> {
        self.input[self.read_pos..].chars().next()
    }

    /// Skip whitespace, tracking newlines for semicolon insertion.
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if c == '\n' {
                self.at_newline = true;
                self.read_char();
            } else if c.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    /// Skip a single-line comment (`// ...`).
    fn skip_line_comment(&mut self) {
        while let Some(c) = self.ch {
            self.read_char();
            if c == '\n' {
                self.at_newline = true;
                break;
            }
        }
    }

    /// Skip a multi-line comment (`/* ... */`).
    fn skip_block_comment(&mut self) {
        self.read_char(); // consume '*' after '/'
        loop {
            match self.ch {
                None => break, // EOF inside comment
                Some('*') if self.peek_char() == Some('/') => {
                    self.read_char(); // consume '*'
                    self.read_char(); // consume '/'
                    break;
                }
                Some('\n') => {
                    self.at_newline = true;
                    self.read_char();
                }
                _ => self.read_char(),
            }
        }
    }

    /// Check if the previous token should trigger automatic semicolon insertion.
    /// Per GoX spec §3.6: semicolon inserted after identifiers, literals,
    /// keywords (break, continue, return, true, false, nil), and closing delimiters.
    fn should_insert_semi(&self) -> bool {
        match &self.prev_kind {
            Some(kind) => matches!(
                kind,
                TokenKind::Ident(_)
                    | TokenKind::Int(_)
                    | TokenKind::Float(_)
                    | TokenKind::String(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Nil
                    | TokenKind::Break
                    | TokenKind::Continue
                    | TokenKind::Return
                    | TokenKind::RParen
                    | TokenKind::RBracket
                    | TokenKind::RBrace
            ),
            None => false,
        }
    }

    /// Read an identifier or keyword.
    fn read_identifier(&mut self) -> String {
        let start = self.pos;
        while let Some(c) = self.ch {
            if c.is_alphanumeric() || c == '_' {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[start..self.pos].to_string()
    }

    /// Read a number (integer or float).
    fn read_number(&mut self) -> TokenKind {
        let start = self.pos;
        let mut is_float = false;

        // Read integer part
        while let Some(c) = self.ch {
            if c.is_ascii_digit() {
                self.read_char();
            } else {
                break;
            }
        }

        // Check for decimal point followed by digit
        if self.ch == Some('.') {
            if let Some(next) = self.peek_char() {
                if next.is_ascii_digit() {
                    is_float = true;
                    self.read_char(); // consume '.'

                    // Read fractional part
                    while let Some(c) = self.ch {
                        if c.is_ascii_digit() {
                            self.read_char();
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        let text = &self.input[start..self.pos];
        if is_float {
            TokenKind::Float(text.parse().unwrap_or(0.0))
        } else {
            TokenKind::Int(text.parse().unwrap_or(0))
        }
    }

    /// Read a string literal with escape sequence support.
    fn read_string(&mut self) -> TokenKind {
        self.read_char(); // consume opening '"'
        let mut result = String::new();

        loop {
            match self.ch {
                None => return TokenKind::UnterminatedString,
                Some('"') => {
                    self.read_char(); // consume closing '"'
                    return TokenKind::String(result);
                }
                Some('\\') => {
                    self.read_char(); // consume '\'
                    match self.ch {
                        Some('n') => {
                            result.push('\n');
                            self.read_char();
                        }
                        Some('t') => {
                            result.push('\t');
                            self.read_char();
                        }
                        Some('\\') => {
                            result.push('\\');
                            self.read_char();
                        }
                        Some('"') => {
                            result.push('"');
                            self.read_char();
                        }
                        Some(c) => {
                            result.push(c);
                            self.read_char();
                        }
                        None => return TokenKind::UnterminatedString,
                    }
                }
                Some('\n') => {
                    // Newline in string literal is an error
                    return TokenKind::UnterminatedString;
                }
                Some(c) => {
                    result.push(c);
                    self.read_char();
                }
            }
        }
    }

    /// Look up keyword or return identifier.
    fn lookup_ident(ident: &str) -> TokenKind {
        match ident {
            // Declaration keywords
            "package" => TokenKind::Package,
            "import" => TokenKind::Import,
            "var" => TokenKind::Var,
            "const" => TokenKind::Const,
            "type" => TokenKind::Type,
            "func" => TokenKind::Func,
            "interface" => TokenKind::Interface,
            "implements" => TokenKind::Implements,
            "struct" => TokenKind::Struct,
            "map" => TokenKind::Map,
            // Control flow
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "return" => TokenKind::Return,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            // Literals
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,
            // Identifier
            _ => TokenKind::Ident(ident.to_string()),
        }
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Token {
        // Check for automatic semicolon insertion
        if self.at_newline && self.should_insert_semi() {
            self.at_newline = false;
            self.prev_kind = Some(TokenKind::Semi);
            // Return a synthetic semicolon at current position
            return Token::new(TokenKind::Semi, Span::point(self.pos));
        }

        self.at_newline = false;
        self.skip_whitespace();

        // Check for semicolon insertion again after skipping whitespace
        if self.at_newline && self.should_insert_semi() {
            self.at_newline = false;
            self.prev_kind = Some(TokenKind::Semi);
            return Token::new(TokenKind::Semi, Span::point(self.pos));
        }

        // Clear the flag - we've passed the semi insertion point
        // Any newline encountered was either used for semi insertion or should be ignored
        self.at_newline = false;

        // Handle comments
        if self.ch == Some('/') {
            match self.peek_char() {
                Some('/') => {
                    self.read_char(); // consume first '/'
                    self.skip_line_comment();
                    return self.next_token();
                }
                Some('*') => {
                    self.read_char(); // consume '/'
                    self.skip_block_comment();
                    return self.next_token();
                }
                _ => {}
            }
        }

        let start = self.pos;

        let kind = match self.ch {
            None => TokenKind::Eof,

            Some(c) => match c {
                // ═══════════════════════════════════════════════════════════
                // Identifiers and keywords
                // ═══════════════════════════════════════════════════════════
                'a'..='z' | 'A'..='Z' | '_' => {
                    let ident = self.read_identifier();
                    Self::lookup_ident(&ident)
                }

                // ═══════════════════════════════════════════════════════════
                // Numbers
                // ═══════════════════════════════════════════════════════════
                '0'..='9' => self.read_number(),

                // ═══════════════════════════════════════════════════════════
                // Strings
                // ═══════════════════════════════════════════════════════════
                '"' => self.read_string(),

                // ═══════════════════════════════════════════════════════════
                // Two-character operators (check first)
                // ═══════════════════════════════════════════════════════════
                '+' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::PlusAssign
                    } else {
                        TokenKind::Plus
                    }
                }
                '-' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::MinusAssign
                    } else {
                        TokenKind::Minus
                    }
                }
                '*' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::StarAssign
                    } else {
                        TokenKind::Star
                    }
                }
                '/' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::SlashAssign
                    } else {
                        TokenKind::Slash
                    }
                }
                '%' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::PercentAssign
                    } else {
                        TokenKind::Percent
                    }
                }
                '=' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::Eq
                    } else {
                        TokenKind::Assign
                    }
                }
                '!' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::NotEq
                    } else {
                        TokenKind::Not
                    }
                }
                '<' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::LtEq
                    } else {
                        TokenKind::Lt
                    }
                }
                '>' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::GtEq
                    } else {
                        TokenKind::Gt
                    }
                }
                '&' => {
                    self.read_char();
                    if self.ch == Some('&') {
                        self.read_char();
                        TokenKind::And
                    } else {
                        TokenKind::Invalid('&')
                    }
                }
                '|' => {
                    self.read_char();
                    if self.ch == Some('|') {
                        self.read_char();
                        TokenKind::Or
                    } else {
                        TokenKind::Invalid('|')
                    }
                }
                ':' => {
                    self.read_char();
                    if self.ch == Some('=') {
                        self.read_char();
                        TokenKind::ColonAssign
                    } else {
                        TokenKind::Colon
                    }
                }

                // ═══════════════════════════════════════════════════════════
                // Single-character tokens
                // ═══════════════════════════════════════════════════════════
                '(' => {
                    self.read_char();
                    TokenKind::LParen
                }
                ')' => {
                    self.read_char();
                    TokenKind::RParen
                }
                '[' => {
                    self.read_char();
                    TokenKind::LBracket
                }
                ']' => {
                    self.read_char();
                    TokenKind::RBracket
                }
                '{' => {
                    self.read_char();
                    TokenKind::LBrace
                }
                '}' => {
                    self.read_char();
                    TokenKind::RBrace
                }
                ',' => {
                    self.read_char();
                    TokenKind::Comma
                }
                ';' => {
                    self.read_char();
                    TokenKind::Semi
                }
                '.' => {
                    self.read_char();
                    TokenKind::Dot
                }

                // ═══════════════════════════════════════════════════════════
                // Invalid character
                // ═══════════════════════════════════════════════════════════
                c => {
                    self.read_char();
                    TokenKind::Invalid(c)
                }
            },
        };

        let end = self.pos;
        self.prev_kind = Some(kind.clone());
        Token::new(kind, Span::new(start, end))
    }

    /// Collect all tokens into a vector (excluding EOF).
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token.is_eof() {
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.is_eof() {
            None
        } else {
            Some(token)
        }
    }
}
