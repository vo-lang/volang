//! Detra Lexer - tokenizes source into tokens.

use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Type,
    Struct,
    State,
    Const,
    External,
    Action,
    Require,
    Set,
    Emit,
    Rule,
    Derive,
    Check,
    View,
    Command,
    If,
    Else,
    For,
    In,
    Sort,
    Asc,
    Desc,
    True,
    False,
    Bool,
    Int,
    Float,
    String,
    Map,

    // Identifiers and literals
    Ident(String),
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),

    // Event variables
    EventVar(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Not,
    Assign,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Dot,

    // Special
    Eof,
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<SpannedToken>, String> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token()?;
            let is_eof = tok.token == Token::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<SpannedToken, String> {
        self.skip_whitespace_and_comments();

        let start = self.pos;
        let line = self.line;
        let col = self.col;

        let token = match self.peek() {
            None => Token::Eof,
            Some(c) => match c {
                'a'..='z' | 'A'..='Z' | '_' => self.ident_or_keyword(),
                '0'..='9' => self.number()?,
                '"' => self.string_lit()?,
                '$' => self.event_var()?,
                '+' => { self.advance(); Token::Plus }
                '-' => { self.advance(); Token::Minus }
                '*' => { self.advance(); Token::Star }
                '/' => { self.advance(); Token::Slash }
                '%' => { self.advance(); Token::Percent }
                '=' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::Eq
                    } else {
                        Token::Assign
                    }
                }
                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::Ne
                    } else {
                        Token::Not
                    }
                }
                '<' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::Le
                    } else {
                        Token::Lt
                    }
                }
                '>' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::Ge
                    } else {
                        Token::Gt
                    }
                }
                '&' => {
                    self.advance();
                    if self.peek() == Some('&') {
                        self.advance();
                        Token::And
                    } else {
                        return Err(format!("Expected '&&' at line {}", line));
                    }
                }
                '|' => {
                    self.advance();
                    if self.peek() == Some('|') {
                        self.advance();
                        Token::Or
                    } else {
                        return Err(format!("Expected '||' at line {}", line));
                    }
                }
                '(' => { self.advance(); Token::LParen }
                ')' => { self.advance(); Token::RParen }
                '{' => { self.advance(); Token::LBrace }
                '}' => { self.advance(); Token::RBrace }
                '[' => { self.advance(); Token::LBracket }
                ']' => { self.advance(); Token::RBracket }
                ',' => { self.advance(); Token::Comma }
                ':' => { self.advance(); Token::Colon }
                '.' => { self.advance(); Token::Dot }
                _ => return Err(format!("Unexpected character '{}' at line {}", c, line)),
            }
        };

        Ok(SpannedToken {
            token,
            span: Span {
                start,
                end: self.pos,
                line,
                col,
            },
        })
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(' ' | '\t' | '\r' | '\n') => { self.advance(); }
                Some('/') => {
                    let pos = self.pos;
                    self.advance();
                    if self.peek() == Some('/') {
                        self.advance();
                        while let Some(c) = self.peek() {
                            if c == '\n' { break; }
                            self.advance();
                        }
                    } else {
                        self.pos = pos;
                        self.chars = self.source[pos..].chars().peekable();
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn ident_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let ident = &self.source[start..self.pos];
        match ident {
            "type" => Token::Type,
            "struct" => Token::Struct,
            "state" => Token::State,
            "const" => Token::Const,
            "external" => Token::External,
            "action" => Token::Action,
            "require" => Token::Require,
            "set" => Token::Set,
            "emit" => Token::Emit,
            "rule" => Token::Rule,
            "derive" => Token::Derive,
            "check" => Token::Check,
            "view" => Token::View,
            "command" => Token::Command,
            "if" => Token::If,
            "else" => Token::Else,
            "for" => Token::For,
            "in" => Token::In,
            "sort" => Token::Sort,
            "asc" => Token::Asc,
            "desc" => Token::Desc,
            "true" => Token::True,
            "false" => Token::False,
            "bool" => Token::Bool,
            "int" => Token::Int,
            "float" => Token::Float,
            "string" => Token::String,
            "map" => Token::Map,
            _ => Token::Ident(ident.to_string()),
        }
    }

    fn number(&mut self) -> Result<Token, String> {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        if self.peek() == Some('.') {
            self.advance();
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            let s = &self.source[start..self.pos];
            let f: f64 = s.parse().map_err(|_| format!("Invalid float: {}", s))?;
            Ok(Token::FloatLit(f))
        } else {
            let s = &self.source[start..self.pos];
            let i: i64 = s.parse().map_err(|_| format!("Invalid int: {}", s))?;
            Ok(Token::IntLit(i))
        }
    }

    fn string_lit(&mut self) -> Result<Token, String> {
        self.advance(); // consume opening "
        let mut s = String::new();
        loop {
            match self.advance() {
                None => return Err("Unterminated string".to_string()),
                Some('"') => break,
                Some('\\') => {
                    match self.advance() {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('r') => s.push('\r'),
                        Some('\\') => s.push('\\'),
                        Some('"') => s.push('"'),
                        Some(c) => return Err(format!("Invalid escape: \\{}", c)),
                        None => return Err("Unterminated string".to_string()),
                    }
                }
                Some(c) => s.push(c),
            }
        }
        Ok(Token::StringLit(s))
    }

    fn event_var(&mut self) -> Result<Token, String> {
        self.advance(); // consume $
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let name = &self.source[start..self.pos];
        if name.is_empty() {
            return Err("Expected event variable name after $".to_string());
        }
        Ok(Token::EventVar(name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("state App { count int = 0 }");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].token, Token::State);
        assert_eq!(tokens[1].token, Token::Ident("App".to_string()));
        assert_eq!(tokens[2].token, Token::LBrace);
    }
}
