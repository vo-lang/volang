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

use vo_common::diagnostics::DiagnosticSink;
use vo_common::span::{BytePos, Span};
use vo_common::symbol::SymbolInterner;
use crate::ast::{Ident, ExprId, IdentId, TypeExprId};

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
        let mut lexer = Lexer::new(source, base);
        let current = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            base,
            source,
            current,
            peek,
            lexer,
            interner: SymbolInterner::new(),
            diagnostics: DiagnosticSink::new(),
            allow_composite_lit: true,
            next_ids: IdState::default(),
        }
    }

    /// Creates a new parser with a shared symbol interner.
    pub fn with_interner(source: &'a str, base: u32, interner: SymbolInterner) -> Self {
        Self::with_interner_and_ids(source, base, interner, IdState::default())
    }

    /// Creates a new parser with shared interner and ID state (for multi-file packages).
    pub fn with_interner_and_ids(source: &'a str, base: u32, interner: SymbolInterner, ids: IdState) -> Self {
        let mut lexer = Lexer::new(source, base);
        let current = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            base,
            source,
            current,
            peek,
            lexer,
            interner,
            diagnostics: DiagnosticSink::new(),
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
        std::mem::take(&mut self.diagnostics)
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
        use crate::ast::ImportKind;

        // Check for optional alias (identifier before path)
        // Supported forms:
        //   import "path"           - standard import
        //   import @"alias"         - external import
        //   import name "path"      - standard import with alias
        //   import name @"alias"    - external import with alias
        //   import . "path"         - dot import
        //   import _ "path"         - blank import
        let alias = if self.at(TokenKind::Ident)
            && (self.peek_is(TokenKind::StringLit)
                || self.peek_is(TokenKind::RawStringLit)
                || self.peek_is(TokenKind::At))
        {
            Some(self.parse_ident()?)
        } else if self.at(TokenKind::Dot)
            && (self.peek_is(TokenKind::StringLit)
                || self.peek_is(TokenKind::RawStringLit)
                || self.peek_is(TokenKind::At))
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

        // Check for @ (external import marker)
        let kind = if self.eat(TokenKind::At) {
            ImportKind::External
        } else {
            ImportKind::Standard
        };

        let path = self.parse_string_lit()?;

        // Handle semicolon based on context
        if require_semi {
            self.expect_semi();
        } else {
            // In grouped imports, semicolons are inserted by lexer or optional
            self.eat(TokenKind::Semicolon);
        }

        Ok(ImportDecl {
            kind,
            path,
            alias,
            span: Span::new(start, self.current.span.start),
        })
    }

    // =========================================================================
    // Token manipulation
    // =========================================================================

    fn advance(&mut self) -> Token {
        let token = std::mem::replace(
            &mut self.current,
            std::mem::replace(&mut self.peek, self.lexer.next_token()),
        );
        token
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
        if !self.eat(TokenKind::Semicolon) {
            if !self.at(TokenKind::RBrace) && !self.at_eof() {
                self.error_expected("';'");
            }
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
            self.diagnostics.emit(
                crate::errors::SyntaxError::KeywordAsIdent
                    .at_with_message(span, format!("cannot use keyword '{}' as identifier", keyword))
            );
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
            Ok(StringLit { raw, value, is_raw: false })
        } else if self.at(TokenKind::RawStringLit) {
            let token = self.advance();
            let text = &self.source[self.span_to_local_range(token.span)];
            let raw = self.interner.intern(text);
            // Raw strings: just strip the backticks
            let value = text[1..text.len()-1].to_string();
            Ok(StringLit { raw, value, is_raw: true })
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
    let inner = &text[1..text.len()-1]; // strip quotes
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
    let inner = &text[1..text.len()-1]; // strip quotes
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
        'a' => Some('\x07'),  // alert/bell
        'b' => Some('\x08'),  // backspace
        'f' => Some('\x0C'),  // form feed
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        'v' => Some('\x0B'),  // vertical tab
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
mod tests {
    use super::*;

    fn parse_str(source: &str) -> (File, DiagnosticSink) {
        let (file, diagnostics, _) = parse(source, 0);
        (file, diagnostics)
    }

    fn parse_ok(source: &str) -> File {
        let (file, diags) = parse_str(source);
        if diags.has_errors() {
            panic!("parse errors: {:?}", diags.iter().collect::<Vec<_>>());
        }
        file
    }

    // =========================================================================
    // File structure tests
    // =========================================================================

    #[test]
    fn test_empty_file() {
        let (file, diags) = parse_str("");
        assert!(diags.is_empty());
        assert!(file.package.is_none());
        assert!(file.imports.is_empty());
        assert!(file.decls.is_empty());
    }

    #[test]
    fn test_package_only() {
        let file = parse_ok("package main");
        assert!(file.package.is_some());
    }

    #[test]
    fn test_package_with_import() {
        let file = parse_ok(
            r#"
            package main
            import "fmt"
        "#,
        );
        assert!(file.package.is_some());
        assert_eq!(file.imports.len(), 1);
    }

    #[test]
    fn test_multiple_imports() {
        let file = parse_ok(
            r#"
            package main
            import "fmt"
            import "os"
            import "strings"
        "#,
        );
        assert_eq!(file.imports.len(), 3);
    }

    // =========================================================================
    // Variable declaration tests
    // =========================================================================

    #[test]
    fn test_var_decl_with_type() {
        let file = parse_ok("var x int");
        assert_eq!(file.decls.len(), 1);
        match &file.decls[0] {
            Decl::Var(v) => {
                assert_eq!(v.specs.len(), 1);
                assert_eq!(v.specs[0].names.len(), 1);
                assert!(v.specs[0].ty.is_some());
                assert!(v.specs[0].values.is_empty());
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_var_decl_with_init() {
        let file = parse_ok("var x = 42");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(v.specs[0].ty.is_none());
                assert_eq!(v.specs[0].values.len(), 1);
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_var_decl_multiple_names() {
        let file = parse_ok("var x, y, z int");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert_eq!(v.specs[0].names.len(), 3);
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_var_decl_grouped() {
        let file = parse_ok(
            r#"
            var (
                a int
                b = 1
                c, d string
            )
        "#,
        );
        match &file.decls[0] {
            Decl::Var(v) => {
                assert_eq!(v.specs.len(), 3);
            }
            _ => panic!("expected var decl"),
        }
    }

    // =========================================================================
    // Const declaration tests
    // =========================================================================

    #[test]
    fn test_const_decl() {
        let file = parse_ok("const x = 1");
        assert_eq!(file.decls.len(), 1);
        match &file.decls[0] {
            Decl::Const(_) => {}
            _ => panic!("expected const decl"),
        }
    }

    #[test]
    fn test_const_decl_grouped() {
        let file = parse_ok(
            r#"
            const (
                a = 1
                b
                c = 3
            )
        "#,
        );
        match &file.decls[0] {
            Decl::Const(c) => {
                assert_eq!(c.specs.len(), 3);
            }
            _ => panic!("expected const decl"),
        }
    }

    // =========================================================================
    // Type declaration tests
    // =========================================================================

    #[test]
    fn test_type_decl_alias() {
        let file = parse_ok("type MyInt int");
        match &file.decls[0] {
            Decl::Type(t) => {
                assert!(matches!(t.ty.kind, TypeExprKind::Ident(_)));
            }
            _ => panic!("expected type decl"),
        }
    }

    #[test]
    fn test_type_decl_struct() {
        let file = parse_ok(
            r#"
            type Person struct {
                name string
                age int
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Type(t) => match &t.ty.kind {
                TypeExprKind::Struct(s) => {
                    assert_eq!(s.fields.len(), 2);
                }
                _ => panic!("expected struct type"),
            },
            _ => panic!("expected type decl"),
        }
    }

    #[test]
    fn test_type_decl_pointer() {
        let file = parse_ok(
            r#"
            type CounterRef *Counter
        "#,
        );
        match &file.decls[0] {
            Decl::Type(t) => {
                assert!(matches!(t.ty.kind, TypeExprKind::Pointer(_)));
            }
            _ => panic!("expected type decl"),
        }
    }

    // =========================================================================
    // Function declaration tests
    // =========================================================================

    #[test]
    fn test_func_decl_simple() {
        let file = parse_ok("func main() {}");
        match &file.decls[0] {
            Decl::Func(f) => {
                assert!(f.receiver.is_none());
                assert!(f.sig.params.is_empty());
                assert!(f.sig.results.is_empty());
                assert!(f.body.is_some());
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_func_decl_with_params() {
        let file = parse_ok("func add(a, b int) int { return a + b }");
        match &file.decls[0] {
            Decl::Func(f) => {
                assert_eq!(f.sig.params.len(), 1);
                assert_eq!(f.sig.params[0].names.len(), 2);
                assert_eq!(f.sig.results.len(), 1);
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_func_decl_with_receiver() {
        let file = parse_ok("func (c Counter) Inc() { c.count++ }");
        match &file.decls[0] {
            Decl::Func(f) => {
                assert!(f.receiver.is_some());
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_func_decl_variadic() {
        let file = parse_ok("func printf(format string, args ...int) {}");
        match &file.decls[0] {
            Decl::Func(f) => {
                assert!(f.sig.variadic);
            }
            _ => panic!("expected func decl"),
        }
    }

    // =========================================================================
    // Interface type tests (via type declaration)
    // =========================================================================

    #[test]
    fn test_interface_type() {
        let file = parse_ok(
            r#"
            type Reader interface {
                Read(p []byte) int
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Type(t) => {
                match &t.ty.kind {
                    TypeExprKind::Interface(i) => {
                        assert_eq!(i.elems.len(), 1);
                    }
                    _ => panic!("expected interface type"),
                }
            }
            _ => panic!("expected type decl"),
        }
    }

    // =========================================================================
    // Expression tests
    // =========================================================================

    #[test]
    fn test_expr_binary() {
        let file = parse_ok("var x = 1 + 2 * 3");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(v.specs[0].values[0].kind, ExprKind::Binary(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_expr_call() {
        let file = parse_ok("var x = foo(1, 2, 3)");
        match &file.decls[0] {
            Decl::Var(v) => match &v.specs[0].values[0].kind {
                ExprKind::Call(c) => {
                    assert_eq!(c.args.len(), 3);
                }
                _ => panic!("expected call expr"),
            },
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_expr_index() {
        let file = parse_ok("var x = arr[0]");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(v.specs[0].values[0].kind, ExprKind::Index(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_expr_slice() {
        let file = parse_ok("var x = arr[1:3]");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(v.specs[0].values[0].kind, ExprKind::Slice(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_expr_selector() {
        let file = parse_ok("var x = obj.field");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(v.specs[0].values[0].kind, ExprKind::Selector(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_expr_composite_lit() {
        let file = parse_ok("var x = Point{x: 1, y: 2}");
        match &file.decls[0] {
            Decl::Var(v) => match &v.specs[0].values[0].kind {
                ExprKind::CompositeLit(c) => {
                    assert_eq!(c.elems.len(), 2);
                }
                _ => panic!("expected composite lit"),
            },
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_expr_func_lit() {
        let file = parse_ok("var f = func(x int) int { return x * 2 }");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(v.specs[0].values[0].kind, ExprKind::FuncLit(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    // =========================================================================
    // Type expression tests
    // =========================================================================

    #[test]
    fn test_type_array() {
        let file = parse_ok("var x [10]int");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(
                    v.specs[0].ty.as_ref().unwrap().kind,
                    TypeExprKind::Array(_)
                ));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_slice() {
        let file = parse_ok("var x []int");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(
                    v.specs[0].ty.as_ref().unwrap().kind,
                    TypeExprKind::Slice(_)
                ));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_map() {
        let file = parse_ok("var x map[string]int");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(
                    v.specs[0].ty.as_ref().unwrap().kind,
                    TypeExprKind::Map(_)
                ));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_chan() {
        let file = parse_ok("var x chan int");
        match &file.decls[0] {
            Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
                TypeExprKind::Chan(c) => {
                    assert_eq!(c.dir, ChanDir::Both);
                }
                _ => panic!("expected chan type"),
            },
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_send_chan() {
        let file = parse_ok("var x chan<- int");
        match &file.decls[0] {
            Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
                TypeExprKind::Chan(c) => {
                    assert_eq!(c.dir, ChanDir::Send);
                }
                _ => panic!("expected chan type"),
            },
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_recv_chan() {
        let file = parse_ok("var x <-chan int");
        match &file.decls[0] {
            Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
                TypeExprKind::Chan(c) => {
                    assert_eq!(c.dir, ChanDir::Recv);
                }
                _ => panic!("expected chan type"),
            },
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_func() {
        let file = parse_ok("var x func(int) string");
        match &file.decls[0] {
            Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
                TypeExprKind::Func(f) => {
                    assert_eq!(f.params.len(), 1);
                    assert_eq!(f.results.len(), 1);
                }
                _ => panic!("expected func type"),
            },
            _ => panic!("expected var decl"),
        }
    }

    // =========================================================================
    // Statement tests
    // =========================================================================

    #[test]
    fn test_stmt_if() {
        let file = parse_ok(
            r#"
            func main() {
                if x > 0 {
                    return 1
                }
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                // Find the if statement (there may be empty statements from semicolons)
                let if_stmts: Vec<_> = body
                    .stmts
                    .iter()
                    .filter(|s| matches!(s.kind, StmtKind::If(_)))
                    .collect();
                assert_eq!(if_stmts.len(), 1);
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_if_else() {
        let file = parse_ok(
            r#"
            func main() {
                if x > 0 {
                    return 1
                } else {
                    return 0
                }
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                match &body.stmts[0].kind {
                    StmtKind::If(i) => {
                        assert!(i.else_.is_some());
                    }
                    _ => panic!("expected if stmt"),
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_for_cond() {
        let file = parse_ok(
            r#"
            func main() {
                for x < 10 {
                    x++
                }
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                match &body.stmts[0].kind {
                    StmtKind::For(f) => {
                        assert!(matches!(f.clause, ForClause::Cond(_)));
                    }
                    _ => panic!("expected for stmt"),
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_for_three() {
        let file = parse_ok(
            r#"
            func main() {
                for i := 0; i < 10; i++ {
                    x++
                }
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                match &body.stmts[0].kind {
                    StmtKind::For(f) => {
                        assert!(matches!(f.clause, ForClause::Three { .. }));
                    }
                    _ => panic!("expected for stmt"),
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_switch() {
        let file = parse_ok(
            r#"
            func main() {
                switch x {
                case 1:
                    return 1
                case 2:
                    return 2
                default:
                    return 0
                }
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                match &body.stmts[0].kind {
                    StmtKind::Switch(s) => {
                        assert_eq!(s.cases.len(), 3);
                    }
                    _ => panic!("expected switch stmt"),
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_short_var_decl() {
        let file = parse_ok(
            r#"
            func main() {
                x := 1
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert!(matches!(body.stmts[0].kind, StmtKind::ShortVar(_)));
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_assign() {
        let file = parse_ok(
            r#"
            func main() {
                x = 1
                y += 2
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert!(matches!(body.stmts[0].kind, StmtKind::Assign(_)));
                assert!(matches!(body.stmts[1].kind, StmtKind::Assign(_)));
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_inc_dec() {
        let file = parse_ok(
            r#"
            func main() {
                x++
                y--
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                match &body.stmts[0].kind {
                    StmtKind::IncDec(i) => assert!(i.is_inc),
                    _ => panic!("expected inc/dec stmt"),
                }
                match &body.stmts[1].kind {
                    StmtKind::IncDec(i) => assert!(!i.is_inc),
                    _ => panic!("expected inc/dec stmt"),
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_return() {
        let file = parse_ok(
            r#"
            func main() {
                return 1, 2
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                match &body.stmts[0].kind {
                    StmtKind::Return(r) => {
                        assert_eq!(r.values.len(), 2);
                    }
                    _ => panic!("expected return stmt"),
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_go_defer() {
        let file = parse_ok(
            r#"
            func main() {
                go foo()
                defer bar()
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert!(matches!(body.stmts[0].kind, StmtKind::Go(_)));
                assert!(matches!(body.stmts[1].kind, StmtKind::Defer(_)));
            }
            _ => panic!("expected func decl"),
        }
    }

    // =========================================================================
    // Error handling tests
    // =========================================================================

    #[test]
    fn test_error_missing_semicolon() {
        let (_, diags) = parse_str("var x int var y int");
        assert!(diags.has_errors());
    }

    #[test]
    fn test_error_unexpected_token() {
        let (_, diags) = parse_str("func main() { + }");
        assert!(diags.has_errors());
    }

    // =========================================================================
    // Global position tests
    // =========================================================================

    #[test]
    fn test_global_positions() {
        // Parse with a base offset of 100
        let (file, _, _) = parse("var x int", 100);
        
        // Check that spans have been offset
        assert!(file.span.start.0 >= 100);
    }

    // =========================================================================
    // Error handling statement tests
    // =========================================================================

    #[test]
    fn test_fail_statement_simple() {
        let file = parse_ok(
            r#"
            func foo() error {
                fail err
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert!(matches!(body.stmts[0].kind, StmtKind::Fail(_)));
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_fail_statement_with_call() {
        let file = parse_ok(
            r#"
            func foo() error {
                fail errors.New("failed")
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert!(matches!(body.stmts[0].kind, StmtKind::Fail(_)));
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_errdefer_statement() {
        let file = parse_ok(
            r#"
            func foo() error {
                errdefer cleanup()
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert!(matches!(body.stmts[0].kind, StmtKind::ErrDefer(_)));
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_errdefer_with_method_call() {
        let file = parse_ok(
            r#"
            func foo() error {
                errdefer tx.Rollback()
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert!(matches!(body.stmts[0].kind, StmtKind::ErrDefer(_)));
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_try_unwrap_expression() {
        let file = parse_ok(
            r#"
            func foo() error {
                x := bar()?
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                if let StmtKind::ShortVar(sv) = &body.stmts[0].kind {
                    assert!(matches!(sv.values[0].kind, ExprKind::TryUnwrap(_)));
                } else {
                    panic!("expected short var decl");
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_try_unwrap_chained() {
        let file = parse_ok(
            r#"
            func foo() error {
                x := a()?.b()?
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                if let StmtKind::ShortVar(sv) = &body.stmts[0].kind {
                    // The outer expression should be TryUnwrap
                    assert!(matches!(sv.values[0].kind, ExprKind::TryUnwrap(_)));
                } else {
                    panic!("expected short var decl");
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_try_unwrap_as_statement() {
        let file = parse_ok(
            r#"
            func foo() error {
                doSomething()?
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                if let StmtKind::Expr(expr) = &body.stmts[0].kind {
                    assert!(matches!(expr.kind, ExprKind::TryUnwrap(_)));
                } else {
                    panic!("expected expr stmt");
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_combined_error_handling() {
        // Test a complete error handling pattern
        let file = parse_ok(
            r#"
            func processFile(path string) error {
                f := open(path)?
                defer f.Close()
                errdefer cleanup(path)
                
                data := read(f)?
                result := process(data)?
                
                if result == nil {
                    fail ErrNotFound
                }
                
                return nil
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                // Should have multiple statements including TryUnwrap, Defer, ErrDefer, Fail
                let has_errdefer = body.stmts.iter().any(|s| matches!(s.kind, StmtKind::ErrDefer(_)));
                let has_defer = body.stmts.iter().any(|s| matches!(s.kind, StmtKind::Defer(_)));
                let has_fail = body.stmts.iter().any(|s| {
                    if let StmtKind::If(if_stmt) = &s.kind {
                        if_stmt.then.stmts.iter().any(|s| matches!(s.kind, StmtKind::Fail(_)))
                    } else {
                        false
                    }
                });
                assert!(has_errdefer, "should have errdefer");
                assert!(has_defer, "should have defer");
                assert!(has_fail, "should have fail");
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_fail_in_if_block() {
        let file = parse_ok(
            r#"
            func foo() error {
                if x == nil {
                    fail ErrNil
                }
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                if let StmtKind::If(if_stmt) = &body.stmts[0].kind {
                    assert!(matches!(if_stmt.then.stmts[0].kind, StmtKind::Fail(_)));
                } else {
                    panic!("expected if stmt");
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_multiple_errdefer() {
        let file = parse_ok(
            r#"
            func foo() error {
                errdefer cleanup1()
                errdefer cleanup2()
                errdefer cleanup3()
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                assert_eq!(body.stmts.len(), 3);
                for stmt in &body.stmts {
                    assert!(matches!(stmt.kind, StmtKind::ErrDefer(_)));
                }
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_struct_field_tag() {
        let file = parse_ok(
            r#"
            package main
            type Person struct {
                Name string `json:"name"`
                Age  int    `json:"age"`
            }
        "#,
        );
        match &file.decls[0] {
            Decl::Type(t) => {
                if let crate::ast::TypeExprKind::Struct(s) = &t.ty.kind {
                    assert_eq!(s.fields.len(), 2);
                    assert!(s.fields[0].tag.is_some(), "Name field should have tag");
                    assert_eq!(s.fields[0].tag.as_ref().unwrap().value, r#"json:"name""#);
                    assert!(s.fields[1].tag.is_some(), "Age field should have tag");
                    assert_eq!(s.fields[1].tag.as_ref().unwrap().value, r#"json:"age""#);
                } else {
                    panic!("expected struct type");
                }
            }
            _ => panic!("expected type decl"),
        }
    }
}
