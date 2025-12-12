//! GoX Parser - Recursive Descent with Pratt Expression Parsing.
//!
//! Based on the parser implementation guide (`english/parser.md`) and
//! GoX Language Specification v2.2.

mod decl;
mod expr;
mod stmt;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};
use gox_common::{GoxError, GoxResult, Span};

// ═══════════════════════════════════════════════════════════════════════════
// Parser State
// ═══════════════════════════════════════════════════════════════════════════

/// The GoX parser.
pub struct Parser<'a> {
    /// Lexer producing tokens.
    lexer: Lexer<'a>,
    /// Current token.
    current: Token,
    /// Next token (peek).
    peek: Token,
    /// Flag to disambiguate composite literals vs blocks.
    allow_composite_lit: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser for the given source.
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let current = lexer.next_token();
        let peek = lexer.next_token();
        Self {
            lexer,
            current,
            peek,
            allow_composite_lit: true,
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Token Management
    // ═══════════════════════════════════════════════════════════════════════

    /// Advance to the next token.
    fn next_token(&mut self) {
        self.current = std::mem::replace(&mut self.peek, self.lexer.next_token());
    }

    /// Check if current token matches the given kind.
    fn cur_is(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.current.kind) == std::mem::discriminant(kind)
    }

    /// Check if peek token matches the given kind.
    #[allow(dead_code)]
    fn peek_is(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.peek.kind) == std::mem::discriminant(kind)
    }

    /// Check if current token is EOF.
    fn at_eof(&self) -> bool {
        matches!(self.current.kind, TokenKind::Eof)
    }

    /// Consume current token if it matches, returning true.
    fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.cur_is(kind) {
            self.next_token();
            true
        } else {
            false
        }
    }

    /// Expect current token to match, consuming it.
    fn expect(&mut self, kind: &TokenKind) -> GoxResult<Span> {
        if self.cur_is(kind) {
            let span = self.current.span;
            self.next_token();
            Ok(span)
        } else {
            Err(self.error_expected(kind))
        }
    }

    /// Expect a semicolon (with helpful error message).
    fn expect_semi(&mut self) -> GoxResult<()> {
        if self.eat(&TokenKind::Semi) {
            Ok(())
        } else {
            Err(self.error_msg("expected ';'"))
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Error Helpers
    // ═══════════════════════════════════════════════════════════════════════

    fn error_expected(&self, expected: &TokenKind) -> GoxError {
        GoxError::with_span(
            format!(
                "expected {}, found {}",
                expected.name(),
                self.current.kind.name()
            ),
            self.current.span,
            0, // file_id placeholder
        )
    }

    fn error_msg(&self, msg: &str) -> GoxError {
        GoxError::with_span(msg.to_string(), self.current.span, 0)
    }

    fn error_at(&self, msg: &str, span: Span) -> GoxError {
        GoxError::with_span(msg.to_string(), span, 0)
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Identifier Helper
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse an identifier.
    fn parse_ident(&mut self) -> GoxResult<Ident> {
        match &self.current.kind {
            TokenKind::Ident(name) => {
                let ident = Ident::new(name.clone(), self.current.span);
                self.next_token();
                Ok(ident)
            }
            _ => Err(self.error_msg("expected identifier")),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Entry Point
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse a complete file.
    pub fn parse_file(&mut self) -> GoxResult<File> {
        let start = self.current.span;

        // Package clause (optional)
        let package = if self.cur_is(&TokenKind::Package) {
            Some(self.parse_package_clause()?)
        } else {
            None
        };

        // Import declarations
        let mut imports = Vec::new();
        while self.cur_is(&TokenKind::Import) {
            imports.push(self.parse_import_decl()?);
        }

        // Top-level declarations
        let mut decls = Vec::new();
        while !self.at_eof() {
            // Skip any stray semicolons (from ASI after } or empty statements)
            while self.eat(&TokenKind::Semi) {}
            if self.at_eof() {
                break;
            }
            decls.push(self.parse_top_decl()?);
        }

        let end = if decls.is_empty() {
            if imports.is_empty() {
                package.as_ref().map(|p| p.span).unwrap_or(start)
            } else {
                imports.last().unwrap().span
            }
        } else {
            decls.last().unwrap().span()
        };

        Ok(File {
            package,
            imports,
            decls,
            span: start.to(&end),
        })
    }

    /// Parse package clause: `package name;`
    fn parse_package_clause(&mut self) -> GoxResult<PackageClause> {
        let start = self.expect(&TokenKind::Package)?;
        let name = self.parse_ident()?;
        self.expect_semi()?;
        Ok(PackageClause {
            span: start.to(&name.span),
            name,
        })
    }

    /// Parse import declaration: `import "path";`
    fn parse_import_decl(&mut self) -> GoxResult<ImportDecl> {
        let start = self.expect(&TokenKind::Import)?;

        let path = match &self.current.kind {
            TokenKind::String(s) => {
                let path = s.clone();
                let span = self.current.span;
                self.next_token();
                (path, span)
            }
            _ => return Err(self.error_msg("expected import path string")),
        };

        self.expect_semi()?;

        Ok(ImportDecl {
            path: path.0,
            span: start.to(&path.1),
        })
    }
}

/// Parse source code into an AST.
pub fn parse(source: &str) -> GoxResult<File> {
    let mut parser = Parser::new(source);
    parser.parse_file()
}
