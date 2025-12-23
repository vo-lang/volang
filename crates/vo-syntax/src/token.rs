//! Token definitions for the Vo lexer.
//!
//! This module defines all token kinds recognized by the Vo lexer,
//! including keywords, operators, literals, and delimiters.

use std::fmt;

use vo_common::span::Span;

/// A token produced by the lexer.
#[derive(Clone, Debug)]
pub struct Token {
    /// The kind of token.
    pub kind: TokenKind,
    /// The source span of this token.
    pub span: Span,
}

impl Token {
    /// Creates a new token.
    #[inline]
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Returns true if this token is a keyword.
    #[inline]
    pub fn is_keyword(&self) -> bool {
        self.kind.is_keyword()
    }

    /// Returns true if this token is a literal.
    #[inline]
    pub fn is_literal(&self) -> bool {
        self.kind.is_literal()
    }

    /// Returns true if this token is an operator.
    #[inline]
    pub fn is_operator(&self) -> bool {
        self.kind.is_operator()
    }

    /// Returns true if this token can end a statement (triggers semicolon insertion).
    #[inline]
    pub fn can_end_statement(&self) -> bool {
        self.kind.can_end_statement()
    }
}

/// All possible token kinds in Vo.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum TokenKind {
    // Special tokens
    /// End of file
    Eof,
    /// Invalid token (lexer error)
    Invalid,
    /// Automatically inserted semicolon
    Semicolon,
    /// Newline (used for semicolon insertion, not emitted)
    Newline,

    // Identifiers and literals
    /// Identifier
    Ident,
    /// Integer literal
    IntLit,
    /// Float literal
    FloatLit,
    /// Rune literal
    RuneLit,
    /// String literal
    StringLit,
    /// Raw string literal
    RawStringLit,

    // Keywords
    /// `break`
    Break,
    /// `case`
    Case,
    /// `chan`
    Chan,
    /// `const`
    Const,
    /// `continue`
    Continue,
    /// `default`
    Default,
    /// `defer`
    Defer,
    /// `else`
    Else,
    /// `fallthrough`
    Fallthrough,
    /// `for`
    For,
    /// `func`
    Func,
    /// `go`
    Go,
    /// `goto`
    Goto,
    /// `if`
    If,
    /// `import`
    Import,
    /// `interface`
    Interface,
    /// `map`
    Map,
    /// `package`
    Package,
    /// `range`
    Range,
    /// `return`
    Return,
    /// `select`
    Select,
    /// `struct`
    Struct,
    /// `switch`
    Switch,
    /// `type`
    Type,
    /// `var`
    Var,
    /// `fail` (error handling)
    Fail,
    /// `errdefer` (error handling)
    Errdefer,

    // Operators and punctuation
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `&`
    Amp,
    /// `|`
    Pipe,
    /// `^`
    Caret,
    /// `&^`
    AmpCaret,
    /// `<<`
    Shl,
    /// `>>`
    Shr,

    /// `==`
    EqEq,
    /// `!=`
    NotEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,

    /// `&&`
    AmpAmp,
    /// `||`
    PipePipe,
    /// `!`
    Not,

    /// `<-`
    Arrow,

    /// `++`
    PlusPlus,
    /// `--`
    MinusMinus,

    /// `=`
    Eq,
    /// `:=`
    ColonEq,
    /// `+=`
    PlusEq,
    /// `-=`
    MinusEq,
    /// `*=`
    StarEq,
    /// `/=`
    SlashEq,
    /// `%=`
    PercentEq,
    /// `<<=`
    ShlEq,
    /// `>>=`
    ShrEq,
    /// `&=`
    AmpEq,
    /// `|=`
    PipeEq,
    /// `^=`
    CaretEq,
    /// `&^=`
    AmpCaretEq,

    // Delimiters
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `{`
    LBrace,
    /// `}`
    RBrace,

    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `.`
    Dot,
    /// `...`
    Ellipsis,
    /// `@` (used for external imports: @"alias")
    At,
    /// `?` (error propagation operator)
    Question,
}

impl TokenKind {
    /// Returns true if this is a keyword token.
    pub const fn is_keyword(self) -> bool {
        matches!(
            self,
            TokenKind::Break
                | TokenKind::Case
                | TokenKind::Chan
                | TokenKind::Const
                | TokenKind::Continue
                | TokenKind::Default
                | TokenKind::Defer
                | TokenKind::Else
                | TokenKind::Fallthrough
                | TokenKind::For
                | TokenKind::Func
                | TokenKind::Go
                | TokenKind::Goto
                | TokenKind::If
                | TokenKind::Import
                | TokenKind::Interface
                | TokenKind::Map
                | TokenKind::Package
                | TokenKind::Range
                | TokenKind::Return
                | TokenKind::Select
                | TokenKind::Struct
                | TokenKind::Switch
                | TokenKind::Type
                | TokenKind::Var
                | TokenKind::Fail
                | TokenKind::Errdefer
        )
    }

    /// Returns true if this is a literal token.
    pub const fn is_literal(self) -> bool {
        matches!(
            self,
            TokenKind::IntLit
                | TokenKind::FloatLit
                | TokenKind::RuneLit
                | TokenKind::StringLit
                | TokenKind::RawStringLit
        )
    }

    /// Returns true if this is an operator token.
    pub const fn is_operator(self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Amp
                | TokenKind::Pipe
                | TokenKind::Caret
                | TokenKind::AmpCaret
                | TokenKind::Shl
                | TokenKind::Shr
                | TokenKind::EqEq
                | TokenKind::NotEq
                | TokenKind::Lt
                | TokenKind::LtEq
                | TokenKind::Gt
                | TokenKind::GtEq
                | TokenKind::AmpAmp
                | TokenKind::PipePipe
                | TokenKind::Not
                | TokenKind::Arrow
                | TokenKind::PlusPlus
                | TokenKind::MinusMinus
        )
    }

    /// Returns true if this is an assignment operator.
    pub const fn is_assign_op(self) -> bool {
        matches!(
            self,
            TokenKind::Eq
                | TokenKind::PlusEq
                | TokenKind::MinusEq
                | TokenKind::StarEq
                | TokenKind::SlashEq
                | TokenKind::PercentEq
                | TokenKind::ShlEq
                | TokenKind::ShrEq
                | TokenKind::AmpEq
                | TokenKind::PipeEq
                | TokenKind::CaretEq
                | TokenKind::AmpCaretEq
        )
    }

    /// Returns true if this token can end a statement (triggers semicolon insertion).
    ///
    /// Per the Vo spec, semicolons are inserted after:
    /// - Identifiers and literals
    /// - Keywords: break, continue, fallthrough, return, fail
    /// - Operators: ++, --, ?
    /// - Closing delimiters: ), ], }
    pub const fn can_end_statement(self) -> bool {
        matches!(
            self,
            TokenKind::Ident
                | TokenKind::IntLit
                | TokenKind::FloatLit
                | TokenKind::RuneLit
                | TokenKind::StringLit
                | TokenKind::RawStringLit
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Fallthrough
                | TokenKind::Return
                | TokenKind::Fail
                | TokenKind::PlusPlus
                | TokenKind::MinusMinus
                | TokenKind::RParen
                | TokenKind::RBracket
                | TokenKind::RBrace
                | TokenKind::Question
        )
    }

    /// Returns the keyword for a given string, if any.
    pub fn keyword(s: &str) -> Option<TokenKind> {
        match s {
            "break" => Some(TokenKind::Break),
            "case" => Some(TokenKind::Case),
            "chan" => Some(TokenKind::Chan),
            "const" => Some(TokenKind::Const),
            "continue" => Some(TokenKind::Continue),
            "default" => Some(TokenKind::Default),
            "defer" => Some(TokenKind::Defer),
            "else" => Some(TokenKind::Else),
            "fallthrough" => Some(TokenKind::Fallthrough),
            "for" => Some(TokenKind::For),
            "func" => Some(TokenKind::Func),
            "go" => Some(TokenKind::Go),
            "goto" => Some(TokenKind::Goto),
            "if" => Some(TokenKind::If),
            "import" => Some(TokenKind::Import),
            "interface" => Some(TokenKind::Interface),
            "map" => Some(TokenKind::Map),
            "package" => Some(TokenKind::Package),
            "range" => Some(TokenKind::Range),
            "return" => Some(TokenKind::Return),
            "select" => Some(TokenKind::Select),
            "struct" => Some(TokenKind::Struct),
            "switch" => Some(TokenKind::Switch),
            "type" => Some(TokenKind::Type),
            "var" => Some(TokenKind::Var),
            "fail" => Some(TokenKind::Fail),
            "errdefer" => Some(TokenKind::Errdefer),
            _ => None,
        }
    }

    /// Returns the string representation of this token kind.
    pub const fn as_str(self) -> &'static str {
        match self {
            TokenKind::Eof => "EOF",
            TokenKind::Invalid => "INVALID",
            TokenKind::Semicolon => ";",
            TokenKind::Newline => "NEWLINE",
            TokenKind::Ident => "IDENT",
            TokenKind::IntLit => "INT",
            TokenKind::FloatLit => "FLOAT",
            TokenKind::RuneLit => "RUNE",
            TokenKind::StringLit => "STRING",
            TokenKind::RawStringLit => "RAW_STRING",
            TokenKind::Break => "break",
            TokenKind::Case => "case",
            TokenKind::Chan => "chan",
            TokenKind::Const => "const",
            TokenKind::Continue => "continue",
            TokenKind::Default => "default",
            TokenKind::Defer => "defer",
            TokenKind::Else => "else",
            TokenKind::Fallthrough => "fallthrough",
            TokenKind::For => "for",
            TokenKind::Func => "func",
            TokenKind::Go => "go",
            TokenKind::Goto => "goto",
            TokenKind::If => "if",
            TokenKind::Import => "import",
            TokenKind::Interface => "interface",
            TokenKind::Map => "map",
            TokenKind::Package => "package",
            TokenKind::Range => "range",
            TokenKind::Return => "return",
            TokenKind::Select => "select",
            TokenKind::Struct => "struct",
            TokenKind::Switch => "switch",
            TokenKind::Type => "type",
            TokenKind::Var => "var",
            TokenKind::Fail => "fail",
            TokenKind::Errdefer => "errdefer",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Amp => "&",
            TokenKind::Pipe => "|",
            TokenKind::Caret => "^",
            TokenKind::AmpCaret => "&^",
            TokenKind::Shl => "<<",
            TokenKind::Shr => ">>",
            TokenKind::EqEq => "==",
            TokenKind::NotEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::AmpAmp => "&&",
            TokenKind::PipePipe => "||",
            TokenKind::Not => "!",
            TokenKind::Arrow => "<-",
            TokenKind::PlusPlus => "++",
            TokenKind::MinusMinus => "--",
            TokenKind::Eq => "=",
            TokenKind::ColonEq => ":=",
            TokenKind::PlusEq => "+=",
            TokenKind::MinusEq => "-=",
            TokenKind::StarEq => "*=",
            TokenKind::SlashEq => "/=",
            TokenKind::PercentEq => "%=",
            TokenKind::ShlEq => "<<=",
            TokenKind::ShrEq => ">>=",
            TokenKind::AmpEq => "&=",
            TokenKind::PipeEq => "|=",
            TokenKind::CaretEq => "^=",
            TokenKind::AmpCaretEq => "&^=",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Dot => ".",
            TokenKind::Ellipsis => "...",
            TokenKind::At => "@",
            TokenKind::Question => "?",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_kind_keyword() {
        assert_eq!(TokenKind::keyword("func"), Some(TokenKind::Func));
        assert_eq!(TokenKind::keyword("struct"), Some(TokenKind::Struct));
        assert_eq!(TokenKind::keyword("object"), None);  // object is no longer a keyword
        assert_eq!(TokenKind::keyword("notakeyword"), None);
    }

    #[test]
    fn test_token_kind_is_keyword() {
        assert!(TokenKind::Func.is_keyword());
        assert!(TokenKind::Struct.is_keyword());
        assert!(!TokenKind::Plus.is_keyword());
        assert!(!TokenKind::Ident.is_keyword());
    }

    #[test]
    fn test_token_kind_is_literal() {
        assert!(TokenKind::IntLit.is_literal());
        assert!(TokenKind::FloatLit.is_literal());
        assert!(TokenKind::StringLit.is_literal());
        assert!(!TokenKind::Ident.is_literal());
        assert!(!TokenKind::Plus.is_literal());
    }

    #[test]
    fn test_token_kind_is_operator() {
        assert!(TokenKind::Plus.is_operator());
        assert!(TokenKind::Minus.is_operator());
        assert!(TokenKind::EqEq.is_operator());
        assert!(!TokenKind::Eq.is_operator());
        assert!(!TokenKind::Ident.is_operator());
    }

    #[test]
    fn test_token_kind_is_assign_op() {
        assert!(TokenKind::Eq.is_assign_op());
        assert!(TokenKind::PlusEq.is_assign_op());
        assert!(TokenKind::MinusEq.is_assign_op());
        assert!(!TokenKind::Plus.is_assign_op());
        assert!(!TokenKind::ColonEq.is_assign_op());
    }

    #[test]
    fn test_token_kind_can_end_statement() {
        assert!(TokenKind::Ident.can_end_statement());
        assert!(TokenKind::IntLit.can_end_statement());
        assert!(TokenKind::Return.can_end_statement());
        assert!(TokenKind::Break.can_end_statement());
        assert!(TokenKind::RParen.can_end_statement());
        assert!(TokenKind::RBrace.can_end_statement());
        assert!(!TokenKind::Plus.can_end_statement());
        assert!(!TokenKind::LParen.can_end_statement());
        assert!(!TokenKind::If.can_end_statement());
    }

    #[test]
    fn test_token_kind_as_str() {
        assert_eq!(TokenKind::Plus.as_str(), "+");
        assert_eq!(TokenKind::Func.as_str(), "func");
        assert_eq!(TokenKind::Ellipsis.as_str(), "...");
        assert_eq!(TokenKind::ColonEq.as_str(), ":=");
    }

    #[test]
    fn test_all_keywords() {
        let keywords = [
            "break", "case", "chan", "const", "continue", "default", "defer",
            "else", "fallthrough", "for", "func", "go", "goto", "if", "import",
            "interface", "map", "package", "range", "return", "select",
            "struct", "switch", "type", "var",
        ];
        
        for kw in keywords {
            assert!(TokenKind::keyword(kw).is_some(), "Missing keyword: {}", kw);
            assert!(TokenKind::keyword(kw).unwrap().is_keyword());
        }
    }
}
