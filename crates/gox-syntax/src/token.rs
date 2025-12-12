//! Token types for the GoX lexer.
//!
//! Based on GoX Language Specification §3 (Lexical Structure).

use gox_common::Span;
use std::fmt;

/// A token produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token.
    pub kind: TokenKind,
    /// The source span of this token.
    pub span: Span,
}

impl Token {
    /// Create a new token.
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Check if this is an EOF token.
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::Eof)
    }
}

/// Token kinds based on GoX specification.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // ═══════════════════════════════════════════════════════════════════════
    // Literals (§3.5)
    // ═══════════════════════════════════════════════════════════════════════
    /// Identifier: `[a-zA-Z_][a-zA-Z0-9_]*`
    Ident(String),
    /// Integer literal: `[0-9]+`
    Int(i64),
    /// Float literal: `[0-9]+.[0-9]+`
    Float(f64),
    /// String literal: `"..."`
    String(String),

    // ═══════════════════════════════════════════════════════════════════════
    // Keywords (§3.2)
    // ═══════════════════════════════════════════════════════════════════════

    // Declaration keywords
    Package,
    Import,
    Var,
    Const,
    Type,
    Func,
    Interface,
    Implements,
    Struct,
    Map,

    // Control flow keywords
    If,
    Else,
    For,
    Switch,
    Case,
    Default,
    Return,
    Break,
    Continue,

    // Literal keywords
    True,
    False,
    Nil,

    // ═══════════════════════════════════════════════════════════════════════
    // Operators (§3.4)
    // ═══════════════════════════════════════════════════════════════════════

    // Arithmetic
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Comparison
    Eq,    // ==
    NotEq, // !=
    Lt,    // <
    LtEq,  // <=
    Gt,    // >
    GtEq,  // >=

    // Logical
    And, // &&
    Or,  // ||
    Not, // !

    // Assignment
    Assign,        // =
    ColonAssign,   // :=
    PlusAssign,    // +=
    MinusAssign,   // -=
    StarAssign,    // *=
    SlashAssign,   // /=
    PercentAssign, // %=

    // ═══════════════════════════════════════════════════════════════════════
    // Delimiters (§3.4)
    // ═══════════════════════════════════════════════════════════════════════
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]
    LBrace,   // {
    RBrace,   // }
    Comma,    // ,
    Colon,    // :
    Semi,     // ;
    Dot,      // .

    // ═══════════════════════════════════════════════════════════════════════
    // Special
    // ═══════════════════════════════════════════════════════════════════════
    /// End of file.
    Eof,
    /// Invalid character.
    Invalid(char),
    /// Unterminated string literal.
    UnterminatedString,
}

impl TokenKind {
    /// Get a human-readable name for error messages.
    pub fn name(&self) -> &'static str {
        match self {
            TokenKind::Ident(_) => "identifier",
            TokenKind::Int(_) => "integer",
            TokenKind::Float(_) => "float",
            TokenKind::String(_) => "string",
            TokenKind::Package => "package",
            TokenKind::Import => "import",
            TokenKind::Var => "var",
            TokenKind::Const => "const",
            TokenKind::Type => "type",
            TokenKind::Func => "func",
            TokenKind::Interface => "interface",
            TokenKind::Implements => "implements",
            TokenKind::Struct => "struct",
            TokenKind::Map => "map",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::For => "for",
            TokenKind::Switch => "switch",
            TokenKind::Case => "case",
            TokenKind::Default => "default",
            TokenKind::Return => "return",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Nil => "nil",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Eq => "==",
            TokenKind::NotEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::And => "&&",
            TokenKind::Or => "||",
            TokenKind::Not => "!",
            TokenKind::Assign => "=",
            TokenKind::ColonAssign => ":=",
            TokenKind::PlusAssign => "+=",
            TokenKind::MinusAssign => "-=",
            TokenKind::StarAssign => "*=",
            TokenKind::SlashAssign => "/=",
            TokenKind::PercentAssign => "%=",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Semi => ";",
            TokenKind::Dot => ".",
            TokenKind::Eof => "end of file",
            TokenKind::Invalid(_) => "invalid character",
            TokenKind::UnterminatedString => "unterminated string",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}
