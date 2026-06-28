#![allow(clippy::result_unit_err)]
//! Parser for the Vo programming language.
//!
//! This module provides a recursive descent parser that converts a token stream
//! into an Abstract Syntax Tree (AST). Expression parsing uses Pratt parsing
//! for correct precedence handling.
//!
//! With global position space, the parser accepts a `base` offset. All span
//! positions in the resulting AST are global (base + local offset).

mod decl;
mod expr;
mod stmt;
mod types;

use crate::ast::InlineModMetadata;
use crate::ast::{ExprId, Ident, IdentId, TypeExprId};
use crate::inline_mod::parse_leading_inline_mod;
use vo_common::diagnostics::DiagnosticSink;
use vo_common::span::{BytePos, Span};
use vo_common::symbol::SymbolInterner;

use crate::ast::*;
use crate::errors::SyntaxError;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

/// Parse result type.
pub type ParseResult<T> = Result<T, ()>;

/// ID counters state for multi-file parsing.
#[derive(Debug, Clone, Default)]
pub struct IdState {
    pub expr_id: u32,
    pub type_expr_id: u32,
    pub ident_id: u32,
}

/// The parser for Vo source code.
pub struct Parser<'a> {
    /// Base offset for global positions.
    base: u32,
    /// The source text.
    source: &'a str,
    inline_mod: Option<InlineModMetadata>,
    /// The current token.
    current: Token,
    /// The peek token (one ahead).
    peek: Token,
    /// The lexer.
    lexer: Lexer<'a>,
    /// Symbol interner for identifiers.
    interner: SymbolInterner,
    /// Diagnostics sink.
    diagnostics: DiagnosticSink,
    /// Whether composite literals are allowed (disabled in if/for/switch conditions).
    allow_composite_lit: bool,
    /// Next ID counters for expressions, type expressions, and identifiers.
    next_ids: IdState,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from source code with a base offset.
    ///
    /// The base offset should come from `SourceMap::file_base()` to ensure
    /// all positions are globally unique.
    pub fn new(source: &'a str, base: u32) -> Self {
        let (inline_mod_output, diagnostics) = parse_leading_inline_mod(source, base);
        let mut lexer = Lexer::new(source, base);
        let current = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            base,
            source,
            inline_mod: inline_mod_output.inline_mod,
            current,
            peek,
            lexer,
            interner: SymbolInterner::new(),
            diagnostics,
            allow_composite_lit: true,
            next_ids: IdState::default(),
        }
    }

    /// Creates a new parser with a shared symbol interner.
    pub fn with_interner(source: &'a str, base: u32, interner: SymbolInterner) -> Self {
        Self::with_interner_and_ids(source, base, interner, IdState::default())
    }

    /// Creates a new parser with shared interner and ID state (for multi-file packages).
    pub fn with_interner_and_ids(
        source: &'a str,
        base: u32,
        interner: SymbolInterner,
        ids: IdState,
    ) -> Self {
        let (inline_mod_output, diagnostics) = parse_leading_inline_mod(source, base);
        let mut lexer = Lexer::new(source, base);
        let current = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            base,
            source,
            inline_mod: inline_mod_output.inline_mod,
            current,
            peek,
            lexer,
            interner,
            diagnostics,
            allow_composite_lit: true,
            next_ids: ids,
        }
    }

    /// Returns the current ID state.
    pub fn id_state(&self) -> IdState {
        self.next_ids.clone()
    }

    /// Returns the diagnostics collected during parsing.
    pub fn diagnostics(&self) -> &DiagnosticSink {
        &self.diagnostics
    }

    /// Takes the diagnostics, leaving an empty sink.
    pub fn take_diagnostics(&mut self) -> DiagnosticSink {
        let mut diagnostics = self.lexer.take_diagnostics();
        diagnostics.extend(std::mem::take(&mut self.diagnostics));
        diagnostics
    }

    /// Returns the symbol interner.
    pub fn interner(&self) -> &SymbolInterner {
        &self.interner
    }

    /// Takes the symbol interner.
    pub fn take_interner(self) -> SymbolInterner {
        self.interner
    }

    /// Allocates a new ExprId.
    pub(crate) fn alloc_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_ids.expr_id);
        self.next_ids.expr_id += 1;
        id
    }

    /// Creates an Expr with an auto-allocated ID.
    pub(crate) fn make_expr(&mut self, kind: ExprKind, span: Span) -> Expr {
        Expr {
            id: self.alloc_expr_id(),
            kind,
            span,
        }
    }

    /// Allocates a new TypeExprId.
    pub(crate) fn alloc_type_expr_id(&mut self) -> TypeExprId {
        let id = TypeExprId(self.next_ids.type_expr_id);
        self.next_ids.type_expr_id += 1;
        id
    }

    /// Creates a TypeExpr with an auto-allocated ID.
    pub(crate) fn make_type_expr(&mut self, kind: TypeExprKind, span: Span) -> TypeExpr {
        TypeExpr {
            id: self.alloc_type_expr_id(),
            kind,
            span,
        }
    }

    /// Allocates a new IdentId.
    fn alloc_ident_id(&mut self) -> IdentId {
        let id = IdentId(self.next_ids.ident_id);
        self.next_ids.ident_id += 1;
        id
    }

    /// Parses a complete source file.
    pub fn parse_file(&mut self) -> ParseResult<File> {
        let start = self.current.span.start;

        // Parse package clause
        let package = if self.at(TokenKind::Package) {
            self.advance();
            let name = self.parse_ident()?;
            self.expect_semi();
            Some(name)
        } else {
            None
        };

        // Parse imports
        let mut imports = Vec::new();
        while self.at(TokenKind::Import) {
            let parsed = self.parse_import_or_group()?;
            imports.extend(parsed);
        }

        // Parse top-level declarations
        let mut decls = Vec::new();
        while !self.at_eof() {
            if self.eat(TokenKind::Semicolon) {
                continue;
            }
            match self.parse_top_level_decl() {
                Ok(decl) => decls.push(decl),
                Err(()) => self.synchronize_to_decl(),
            }
        }

        let end = self.current.span.end;
        Ok(File {
            package,
            inline_mod: self.inline_mod.clone(),
            imports,
            decls,
            span: Span::new(start, end),
        })
    }

    /// Parse import or grouped imports: `import "path"` or `import ( ... )`
    fn parse_import_or_group(&mut self) -> ParseResult<Vec<ImportDecl>> {
        let start = self.current.span.start;
        self.expect(TokenKind::Import)?;

        // Check for grouped imports: import ( ... )
        if self.eat(TokenKind::LParen) {
            let mut imports = Vec::new();
            while !self.at(TokenKind::RParen) && !self.at_eof() {
                imports.push(self.parse_import_spec(start, false)?);
            }
            self.expect(TokenKind::RParen)?;
            self.expect_semi();
            Ok(imports)
        } else {
            // Single import - requires semicolon
            let import = self.parse_import_spec(start, true)?;
            Ok(vec![import])
        }
    }

    /// Parse a single import spec (path with optional alias)
    /// If require_semi is true, expect a semicolon after the import path
    fn parse_import_spec(&mut self, start: BytePos, require_semi: bool) -> ParseResult<ImportDecl> {
        // Check for optional alias (identifier before path)
        // Supported forms:
        //   import "path"      - standard import
        //   import name "path" - standard import with alias
        //   import . "path"         - dot import
        //   import _ "path"         - blank import
        let alias = if self.at(TokenKind::Ident)
            && (self.peek_is(TokenKind::StringLit) || self.peek_is(TokenKind::RawStringLit))
        {
            Some(self.parse_ident()?)
        } else if self.at(TokenKind::Dot)
            && (self.peek_is(TokenKind::StringLit) || self.peek_is(TokenKind::RawStringLit))
        {
            // Dot import: import . "path"
            let dot_span = self.current.span;
            self.advance();
            let dot_sym = self.interner.intern(".");
            Some(Ident {
                id: self.alloc_ident_id(),
                symbol: dot_sym,
                span: dot_span,
            })
        } else {
            None
        };

        if self.eat(TokenKind::At) {
            self.error(
                "`import @\"...\"` is no longer supported; use the canonical import path directly",
            );
        }

        let path = self.parse_string_lit()?;

        // Handle semicolon based on context
        if require_semi {
            self.expect_semi();
        } else {
            // In grouped imports, semicolons are inserted by lexer or optional
            self.eat(TokenKind::Semicolon);
        }

        Ok(ImportDecl {
            path,
            alias,
            span: Span::new(start, self.current.span.start),
        })
    }

    // =========================================================================
    // Token manipulation
    // =========================================================================

    fn advance(&mut self) -> Token {
        std::mem::replace(
            &mut self.current,
            std::mem::replace(&mut self.peek, self.lexer.next_token()),
        )
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    fn at_any(&self, kinds: &[TokenKind]) -> bool {
        kinds.contains(&self.current.kind)
    }

    fn peek_is(&self, kind: TokenKind) -> bool {
        self.peek.kind == kind
    }

    fn at_eof(&self) -> bool {
        self.current.kind == TokenKind::Eof
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if self.at(kind) {
            Ok(self.advance())
        } else {
            self.error_expected(kind.as_str());
            Err(())
        }
    }

    fn expect_semi(&mut self) {
        if !self.eat(TokenKind::Semicolon) && !self.at(TokenKind::RBrace) && !self.at_eof() {
            self.error_expected("';'");
        }
    }

    /// Converts a global span to a local range for indexing into source.
    fn span_to_local_range(&self, span: Span) -> std::ops::Range<usize> {
        let start = (span.start.0 - self.base) as usize;
        let end = (span.end.0 - self.base) as usize;
        start..end
    }

    fn span_text(&self, span: Span) -> &str {
        &self.source[self.span_to_local_range(span)]
    }

    fn make_ident(&mut self, token: &Token) -> Ident {
        let text = &self.source[self.span_to_local_range(token.span)];
        let symbol = self.interner.intern(text);
        Ident {
            id: self.alloc_ident_id(),
            symbol,
            span: token.span,
        }
    }

    // =========================================================================
    // Error handling
    // =========================================================================

    fn error(&mut self, message: impl Into<String>) {
        let span = self.current.span;
        self.diagnostics
            .emit(SyntaxError::UnexpectedToken.at_with_message(span, message));
    }

    fn error_at(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics
            .emit(SyntaxError::UnexpectedToken.at_with_message(span, message));
    }

    fn error_expected(&mut self, expected: &str) {
        self.error(format!(
            "expected {}, found {}",
            expected,
            self.current.kind.as_str()
        ));
    }

    fn synchronize_to_decl(&mut self) {
        while !self.at_eof() {
            if self.at_any(&[
                TokenKind::Var,
                TokenKind::Const,
                TokenKind::Type,
                TokenKind::Func,
            ]) {
                return;
            }
            self.advance();
        }
    }

    fn synchronize_to_stmt(&mut self) {
        while !self.at_eof() {
            if self.at_any(&[
                TokenKind::Var,
                TokenKind::Const,
                TokenKind::If,
                TokenKind::For,
                TokenKind::Switch,
                TokenKind::Select,
                TokenKind::Return,
                TokenKind::Break,
                TokenKind::Continue,
                TokenKind::Goto,
                TokenKind::Go,
                TokenKind::Defer,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::Semicolon,
            ]) {
                return;
            }
            self.advance();
        }
    }

    // =========================================================================
    // Helper parsers
    // =========================================================================

    pub(crate) fn parse_ident(&mut self) -> ParseResult<Ident> {
        if self.at(TokenKind::Ident) {
            let token = self.advance();
            Ok(self.make_ident(&token))
        } else if self.current.kind.is_keyword() {
            // Give a more helpful error when a keyword is used as an identifier
            let keyword = self.current.kind.as_str();
            let span = self.current.span;
            self.diagnostics
                .emit(crate::errors::SyntaxError::KeywordAsIdent.at_with_message(
                    span,
                    format!("cannot use keyword '{}' as identifier", keyword),
                ));
            Err(())
        } else {
            self.error_expected("identifier");
            Err(())
        }
    }

    pub(crate) fn parse_ident_list(&mut self) -> ParseResult<Vec<Ident>> {
        let mut idents = vec![self.parse_ident()?];
        while self.eat(TokenKind::Comma) {
            idents.push(self.parse_ident()?);
        }
        Ok(idents)
    }

    pub(crate) fn parse_string_lit(&mut self) -> ParseResult<StringLit> {
        if self.at(TokenKind::StringLit) {
            let token = self.advance();
            let text = &self.source[self.span_to_local_range(token.span)];
            let raw = self.interner.intern(text);
            // Parse the string value (strip quotes and process escapes)
            let value = parse_string_value(text);
            Ok(StringLit {
                raw,
                value,
                is_raw: false,
            })
        } else if self.at(TokenKind::RawStringLit) {
            let token = self.advance();
            let text = &self.source[self.span_to_local_range(token.span)];
            let raw = self.interner.intern(text);
            // Raw strings: just strip the backticks
            let value = text[1..text.len() - 1].to_string();
            Ok(StringLit {
                raw,
                value,
                is_raw: true,
            })
        } else {
            self.error_expected("string literal");
            Err(())
        }
    }

    /// Parse a rune literal and return the character value.
    pub(crate) fn parse_rune_value(&self, text: &str) -> char {
        parse_rune_value(text)
    }

    /// Parses a block: `{ stmt* }`
    pub(crate) fn parse_block(&mut self) -> ParseResult<Block> {
        let start = self.current.span.start;
        self.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(()) => self.synchronize_to_stmt(),
            }
        }

        let end_token = self.expect(TokenKind::RBrace)?;
        Ok(Block {
            stmts,
            span: Span::new(start, end_token.span.end),
        })
    }
}

/// Parse a string literal value, processing escape sequences.
/// The input includes the surrounding quotes.
fn parse_string_value(text: &str) -> String {
    let inner = &text[1..text.len() - 1]; // strip quotes
    let mut result = String::with_capacity(inner.len());
    let mut chars = inner.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(escaped) = parse_escape_char(&mut chars) {
                result.push(escaped);
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Parse a rune literal value.
/// The input includes the surrounding single quotes.
fn parse_rune_value(text: &str) -> char {
    let inner = &text[1..text.len() - 1]; // strip quotes
    let mut chars = inner.chars().peekable();

    if let Some(c) = chars.next() {
        if c == '\\' {
            parse_escape_char(&mut chars).unwrap_or('\0')
        } else {
            c
        }
    } else {
        '\0'
    }
}

/// Parse a single escape sequence and return the resulting character.
fn parse_escape_char(chars: &mut std::iter::Peekable<std::str::Chars>) -> Option<char> {
    match chars.next()? {
        'a' => Some('\x07'), // alert/bell
        'b' => Some('\x08'), // backspace
        'f' => Some('\x0C'), // form feed
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        'v' => Some('\x0B'), // vertical tab
        '\\' => Some('\\'),
        '\'' => Some('\''),
        '"' => Some('"'),
        'x' => {
            // \xNN - 2 hex digits
            let mut val = 0u32;
            for _ in 0..2 {
                if let Some(c) = chars.next() {
                    val = val * 16 + c.to_digit(16)?;
                }
            }
            char::from_u32(val)
        }
        'u' => {
            // \uNNNN - 4 hex digits
            let mut val = 0u32;
            for _ in 0..4 {
                if let Some(c) = chars.next() {
                    val = val * 16 + c.to_digit(16)?;
                }
            }
            char::from_u32(val)
        }
        'U' => {
            // \UNNNNNNNN - 8 hex digits
            let mut val = 0u32;
            for _ in 0..8 {
                if let Some(c) = chars.next() {
                    val = val * 16 + c.to_digit(16)?;
                }
            }
            char::from_u32(val)
        }
        c @ '0'..='7' => {
            // Octal escape \NNN - 3 octal digits
            let mut val = c.to_digit(8)?;
            for _ in 0..2 {
                if let Some(&next) = chars.peek() {
                    if let Some(d) = next.to_digit(8) {
                        chars.next();
                        val = val * 8 + d;
                    } else {
                        break;
                    }
                }
            }
            char::from_u32(val)
        }
        _ => None, // Unknown escape
    }
}

/// Parses source code and returns the AST.
///
/// # Arguments
/// * `source` - The source code text
/// * `base` - Base offset for global positions (from SourceMap::file_base)
pub fn parse(source: &str, base: u32) -> (File, DiagnosticSink, SymbolInterner) {
    let mut parser = Parser::new(source, base);
    let file = parser.parse_file().unwrap_or_else(|()| File {
        package: None,
        inline_mod: parser.inline_mod.clone(),
        imports: Vec::new(),
        decls: Vec::new(),
        span: Span::dummy(),
    });
    let diagnostics = parser.take_diagnostics();
    let interner = parser.take_interner();
    (file, diagnostics, interner)
}

/// Parses source code with a shared interner (for multi-file packages).
pub fn parse_with_interner(
    source: &str,
    base: u32,
    interner: SymbolInterner,
) -> (File, DiagnosticSink, SymbolInterner) {
    let mut parser = Parser::with_interner(source, base, interner);
    let file = parser.parse_file().unwrap_or_else(|()| File {
        package: None,
        inline_mod: parser.inline_mod.clone(),
        imports: Vec::new(),
        decls: Vec::new(),
        span: Span::dummy(),
    });
    let diagnostics = parser.take_diagnostics();
    let interner = parser.take_interner();
    (file, diagnostics, interner)
}

/// Parses source code with shared interner and ID state (for same-package multi-file).
pub fn parse_with_state(
    source: &str,
    base: u32,
    interner: SymbolInterner,
    ids: IdState,
) -> (File, DiagnosticSink, SymbolInterner, IdState) {
    let mut parser = Parser::with_interner_and_ids(source, base, interner, ids);
    let file = parser.parse_file().unwrap_or_else(|()| File {
        package: None,
        inline_mod: parser.inline_mod.clone(),
        imports: Vec::new(),
        decls: Vec::new(),
        span: Span::dummy(),
    });
    let diagnostics = parser.take_diagnostics();
    let ids = parser.id_state();
    let interner = parser.take_interner();
    (file, diagnostics, interner, ids)
}

#[cfg(test)]
mod tests;
