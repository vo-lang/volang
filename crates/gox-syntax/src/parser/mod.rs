//! Parser for the GoX programming language.
//!
//! This module provides a recursive descent parser that converts a token stream
//! into an Abstract Syntax Tree (AST). Expression parsing uses Pratt parsing
//! for correct precedence handling.

mod decl;
mod stmt;
mod expr;
mod types;

use gox_common::source::FileId;
use gox_common::span::Span;
use gox_common::symbol::{Ident, Symbol, SymbolInterner};
use gox_common::diagnostics::DiagnosticSink;

use crate::errors::SyntaxError;
use crate::token::{Token, TokenKind};
use crate::lexer::Lexer;
use crate::ast::*;

/// Parse result type.
pub type ParseResult<T> = Result<T, ()>;

/// The parser for GoX source code.
pub struct Parser<'a> {
    /// The file ID for diagnostics.
    file_id: FileId,
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
}

impl<'a> Parser<'a> {
    /// Creates a new parser from source code.
    pub fn new(file_id: FileId, source: &'a str) -> Self {
        let mut lexer = Lexer::new(file_id, source);
        let current = lexer.next_token();
        let peek = lexer.next_token();
        
        Self {
            file_id,
            source,
            current,
            peek,
            lexer,
            interner: SymbolInterner::new(),
            diagnostics: DiagnosticSink::new(),
            allow_composite_lit: true,
        }
    }

    /// Creates a new parser with a shared symbol interner.
    pub fn with_interner(file_id: FileId, source: &'a str, interner: SymbolInterner) -> Self {
        let mut lexer = Lexer::new(file_id, source);
        let current = lexer.next_token();
        let peek = lexer.next_token();
        
        Self {
            file_id,
            source,
            current,
            peek,
            lexer,
            interner,
            diagnostics: DiagnosticSink::new(),
            allow_composite_lit: true,
        }
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
            imports.push(self.parse_import_decl()?);
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

    fn parse_import_decl(&mut self) -> ParseResult<ImportDecl> {
        let start = self.current.span.start;
        self.expect(TokenKind::Import)?;
        let path = self.parse_string_lit()?;
        self.expect_semi();
        Ok(ImportDecl {
            path,
            span: Span::new(start, self.current.span.start),
        })
    }

    // =========================================================================
    // Token manipulation
    // =========================================================================

    fn advance(&mut self) -> Token {
        let token = std::mem::replace(&mut self.current, std::mem::replace(&mut self.peek, self.lexer.next_token()));
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

    fn span_text(&self, span: Span) -> &str {
        &self.source[span.to_range()]
    }

    fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    fn make_ident(&mut self, token: &Token) -> Ident {
        let text = &self.source[token.span.to_range()];
        let symbol = self.interner.intern(text);
        Ident::new(symbol, token.span)
    }

    // =========================================================================
    // Error handling
    // =========================================================================

    fn error(&mut self, message: impl Into<String>) {
        let span = self.current.span;
        self.diagnostics.emit(SyntaxError::UnexpectedToken.at_with_message(self.file_id, span, message));
    }

    fn error_at(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics.emit(SyntaxError::UnexpectedToken.at_with_message(self.file_id, span, message));
    }

    fn error_expected(&mut self, expected: &str) {
        self.error(format!("expected {}, found {}", expected, self.current.kind.as_str()));
    }

    fn synchronize_to_decl(&mut self) {
        while !self.at_eof() {
            if self.at_any(&[
                TokenKind::Var,
                TokenKind::Const,
                TokenKind::Type,
                TokenKind::Func,
                TokenKind::Interface,
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
            let text = &self.source[token.span.to_range()];
            let raw = self.interner.intern(text);
            Ok(StringLit { raw, is_raw: false })
        } else if self.at(TokenKind::RawStringLit) {
            let token = self.advance();
            let text = &self.source[token.span.to_range()];
            let raw = self.interner.intern(text);
            Ok(StringLit { raw, is_raw: true })
        } else {
            self.error_expected("string literal");
            Err(())
        }
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

/// Parses source code and returns the AST.
pub fn parse(file_id: FileId, source: &str) -> (File, DiagnosticSink, SymbolInterner) {
    let mut parser = Parser::new(file_id, source);
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

#[cfg(test)]
mod tests {
    use super::*;
    use gox_common::source::FileId;

    fn parse_str(source: &str) -> (File, DiagnosticSink) {
        let (file, diagnostics, _) = parse(FileId::new(0), source);
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
        let file = parse_ok(r#"
            package main
            import "fmt"
        "#);
        assert!(file.package.is_some());
        assert_eq!(file.imports.len(), 1);
    }

    #[test]
    fn test_multiple_imports() {
        let file = parse_ok(r#"
            package main
            import "fmt"
            import "os"
            import "strings"
        "#);
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
        let file = parse_ok(r#"
            var (
                a int
                b = 1
                c, d string
            )
        "#);
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
        let file = parse_ok(r#"
            const (
                a = 1
                b
                c = 3
            )
        "#);
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
        let file = parse_ok(r#"
            type Person struct {
                name string
                age int
            }
        "#);
        match &file.decls[0] {
            Decl::Type(t) => {
                match &t.ty.kind {
                    TypeExprKind::Struct(s) => {
                        assert_eq!(s.fields.len(), 2);
                    }
                    _ => panic!("expected struct type"),
                }
            }
            _ => panic!("expected type decl"),
        }
    }

    #[test]
    fn test_type_decl_object() {
        let file = parse_ok(r#"
            type Counter object {
                count int
            }
        "#);
        match &file.decls[0] {
            Decl::Type(t) => {
                assert!(matches!(t.ty.kind, TypeExprKind::Obx(_)));
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
    // Interface declaration tests
    // =========================================================================

    #[test]
    fn test_interface_decl() {
        let file = parse_ok(r#"
            interface Reader {
                Read(p []byte) int
            }
        "#);
        match &file.decls[0] {
            Decl::Interface(i) => {
                assert_eq!(i.elems.len(), 1);
            }
            _ => panic!("expected interface decl"),
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
            Decl::Var(v) => {
                match &v.specs[0].values[0].kind {
                    ExprKind::Call(c) => {
                        assert_eq!(c.args.len(), 3);
                    }
                    _ => panic!("expected call expr"),
                }
            }
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
            Decl::Var(v) => {
                match &v.specs[0].values[0].kind {
                    ExprKind::CompositeLit(c) => {
                        assert_eq!(c.elems.len(), 2);
                    }
                    _ => panic!("expected composite lit"),
                }
            }
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
                assert!(matches!(v.specs[0].ty.as_ref().unwrap().kind, TypeExprKind::Array(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_slice() {
        let file = parse_ok("var x []int");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(v.specs[0].ty.as_ref().unwrap().kind, TypeExprKind::Slice(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_map() {
        let file = parse_ok("var x map[string]int");
        match &file.decls[0] {
            Decl::Var(v) => {
                assert!(matches!(v.specs[0].ty.as_ref().unwrap().kind, TypeExprKind::Map(_)));
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_chan() {
        let file = parse_ok("var x chan int");
        match &file.decls[0] {
            Decl::Var(v) => {
                match &v.specs[0].ty.as_ref().unwrap().kind {
                    TypeExprKind::Chan(c) => {
                        assert_eq!(c.dir, ChanDir::Both);
                    }
                    _ => panic!("expected chan type"),
                }
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_send_chan() {
        let file = parse_ok("var x chan<- int");
        match &file.decls[0] {
            Decl::Var(v) => {
                match &v.specs[0].ty.as_ref().unwrap().kind {
                    TypeExprKind::Chan(c) => {
                        assert_eq!(c.dir, ChanDir::Send);
                    }
                    _ => panic!("expected chan type"),
                }
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_recv_chan() {
        let file = parse_ok("var x <-chan int");
        match &file.decls[0] {
            Decl::Var(v) => {
                match &v.specs[0].ty.as_ref().unwrap().kind {
                    TypeExprKind::Chan(c) => {
                        assert_eq!(c.dir, ChanDir::Recv);
                    }
                    _ => panic!("expected chan type"),
                }
            }
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_type_func() {
        let file = parse_ok("var x func(int) string");
        match &file.decls[0] {
            Decl::Var(v) => {
                match &v.specs[0].ty.as_ref().unwrap().kind {
                    TypeExprKind::Func(f) => {
                        assert_eq!(f.params.len(), 1);
                        assert_eq!(f.results.len(), 1);
                    }
                    _ => panic!("expected func type"),
                }
            }
            _ => panic!("expected var decl"),
        }
    }

    // =========================================================================
    // Statement tests
    // =========================================================================

    #[test]
    fn test_stmt_if() {
        let file = parse_ok(r#"
            func main() {
                if x > 0 {
                    return 1
                }
            }
        "#);
        match &file.decls[0] {
            Decl::Func(f) => {
                let body = f.body.as_ref().unwrap();
                // Find the if statement (there may be empty statements from semicolons)
                let if_stmts: Vec<_> = body.stmts.iter().filter(|s| matches!(s.kind, StmtKind::If(_))).collect();
                assert_eq!(if_stmts.len(), 1);
            }
            _ => panic!("expected func decl"),
        }
    }

    #[test]
    fn test_stmt_if_else() {
        let file = parse_ok(r#"
            func main() {
                if x > 0 {
                    return 1
                } else {
                    return 0
                }
            }
        "#);
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
        let file = parse_ok(r#"
            func main() {
                for x < 10 {
                    x++
                }
            }
        "#);
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
        let file = parse_ok(r#"
            func main() {
                for i := 0; i < 10; i++ {
                    x++
                }
            }
        "#);
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
        let file = parse_ok(r#"
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
        "#);
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
        let file = parse_ok(r#"
            func main() {
                x := 1
            }
        "#);
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
        let file = parse_ok(r#"
            func main() {
                x = 1
                y += 2
            }
        "#);
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
        let file = parse_ok(r#"
            func main() {
                x++
                y--
            }
        "#);
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
        let file = parse_ok(r#"
            func main() {
                return 1, 2
            }
        "#);
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
        let file = parse_ok(r#"
            func main() {
                go foo()
                defer bar()
            }
        "#);
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
}
