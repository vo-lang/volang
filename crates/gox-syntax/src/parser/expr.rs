//! Expression parsing using Pratt parsing.

use super::{Parser, ParseResult};
use crate::ast::*;
use crate::token::TokenKind;
use gox_common::span::Span;

/// Operator precedence levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    Lowest = 0,
    Or,       // ||
    And,      // &&
    Equals,   // == !=
    Compare,  // < <= > >=
    Sum,      // + - | ^
    Shift,    // << >>
    Product,  // * / % & &^
    Prefix,   // -x !x ^x +x
    Postfix,  // f(x) a[i] a.b x.(T)
}

impl Precedence {
    fn from_token(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::PipePipe => Precedence::Or,
            TokenKind::AmpAmp => Precedence::And,
            TokenKind::EqEq | TokenKind::NotEq => Precedence::Equals,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => Precedence::Compare,
            TokenKind::Plus | TokenKind::Minus | TokenKind::Pipe | TokenKind::Caret => Precedence::Sum,
            TokenKind::Shl | TokenKind::Shr => Precedence::Shift,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent | TokenKind::Amp | TokenKind::AmpCaret => Precedence::Product,
            TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot | TokenKind::Question => Precedence::Postfix,
            TokenKind::LBrace => Precedence::Postfix,
            _ => Precedence::Lowest,
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an expression.
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_expr_prec(Precedence::Lowest)
    }

    /// Parses an expression list.
    pub fn parse_expr_list(&mut self) -> ParseResult<Vec<Expr>> {
        let mut exprs = vec![self.parse_expr()?];
        while self.eat(TokenKind::Comma) {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr_prec(&mut self, min_prec: Precedence) -> ParseResult<Expr> {
        let mut left = self.parse_prefix_expr()?;
        
        loop {
            let prec = Precedence::from_token(&self.current.kind);
            if prec <= min_prec {
                break;
            }
            
            // Check for composite literal - only if allowed
            if self.current.kind == TokenKind::LBrace && !self.allow_composite_lit {
                break;
            }
            
            left = self.parse_infix_expr(left, prec)?;
        }
        
        Ok(left)
    }

    fn parse_prefix_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current.span.start;
        
        match self.current.kind {
            TokenKind::Ident => self.parse_ident_or_composite_lit(),
            TokenKind::IntLit => {
                let token = self.advance();
                let text = &self.source[self.span_to_local_range(token.span)];
                let raw = self.interner.intern(text);
                Ok(self.make_expr(ExprKind::IntLit(IntLit { raw }), token.span))
            }
            TokenKind::FloatLit => {
                let token = self.advance();
                let text = &self.source[self.span_to_local_range(token.span)];
                let raw = self.interner.intern(text);
                Ok(self.make_expr(ExprKind::FloatLit(FloatLit { raw }), token.span))
            }
            TokenKind::RuneLit => {
                let token = self.advance();
                let text = &self.source[self.span_to_local_range(token.span)];
                let raw = self.interner.intern(text);
                Ok(self.make_expr(ExprKind::RuneLit(RuneLit { raw }), token.span))
            }
            TokenKind::StringLit => {
                let token = self.advance();
                let text = &self.source[self.span_to_local_range(token.span)];
                let raw = self.interner.intern(text);
                Ok(self.make_expr(ExprKind::StringLit(StringLit { raw, is_raw: false }), token.span))
            }
            TokenKind::RawStringLit => {
                let token = self.advance();
                let text = &self.source[self.span_to_local_range(token.span)];
                let raw = self.interner.intern(text);
                Ok(self.make_expr(ExprKind::StringLit(StringLit { raw, is_raw: true }), token.span))
            }
            TokenKind::LParen => {
                self.advance();
                let inner = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                let span = Span::new(start, self.current.span.start);
                Ok(self.make_expr(ExprKind::Paren(Box::new(inner)), span))
            }
            TokenKind::Minus | TokenKind::Plus | TokenKind::Not | TokenKind::Caret | TokenKind::Amp | TokenKind::Star => {
                let op_token = self.advance();
                let op = match op_token.kind {
                    TokenKind::Minus => UnaryOp::Neg,
                    TokenKind::Plus => UnaryOp::Pos,
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::Caret => UnaryOp::BitNot,
                    TokenKind::Amp => UnaryOp::Addr,
                    TokenKind::Star => UnaryOp::Deref,
                    _ => unreachable!(),
                };
                let operand = self.parse_expr_prec(Precedence::Prefix)?;
                let span = Span::new(start, self.current.span.start);
                Ok(self.make_expr(ExprKind::Unary(Box::new(UnaryExpr { op, operand })), span))
            }
            TokenKind::Arrow => {
                // Receive expression: <-ch
                self.advance();
                let operand = self.parse_expr_prec(Precedence::Prefix)?;
                let span = Span::new(start, self.current.span.start);
                Ok(self.make_expr(ExprKind::Receive(Box::new(operand)), span))
            }
            TokenKind::Func => {
                self.advance();
                let sig = self.parse_func_sig()?;
                let body = self.parse_block()?;
                let span = Span::new(start, self.current.span.start);
                Ok(self.make_expr(ExprKind::FuncLit(Box::new(FuncLit { sig, body })), span))
            }
            // Type-starting tokens for composite literals
            TokenKind::LBracket | TokenKind::Map | TokenKind::Struct => {
                self.parse_composite_lit_with_type()
            }
            _ => {
                self.error_expected("expression");
                Err(())
            }
        }
    }

    fn parse_ident_or_composite_lit(&mut self) -> ParseResult<Expr> {
        let ident = self.parse_ident()?;
        let span = ident.span;
        
        // Check for composite literal: Type{...}
        if self.allow_composite_lit && self.at(TokenKind::LBrace) {
            let ty = self.make_type_expr(TypeExprKind::Ident(ident), span);
            self.parse_composite_lit_body(ty)
        } else {
            Ok(self.make_expr(ExprKind::Ident(ident), span))
        }
    }

    fn parse_composite_lit_with_type(&mut self) -> ParseResult<Expr> {
        let ty = self.parse_type()?;
        if self.at(TokenKind::LBrace) {
            self.parse_composite_lit_body(ty)
        } else {
            // Type conversion: Type(expr)
            self.expect(TokenKind::LParen)?;
            let expr = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            let span = ty.span.to(self.current.span);
            Ok(self.make_expr(ExprKind::Conversion(Box::new(ConversionExpr { ty, expr })), span))
        }
    }

    fn parse_composite_lit_body(&mut self, ty: TypeExpr) -> ParseResult<Expr> {
        let start = ty.span.start;
        self.expect(TokenKind::LBrace)?;
        
        let mut elems = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            elems.push(self.parse_composite_lit_elem()?);
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        
        self.expect(TokenKind::RBrace)?;
        let span = Span::new(start, self.current.span.start);
        Ok(self.make_expr(ExprKind::CompositeLit(Box::new(CompositeLit { ty, elems })), span))
    }

    fn parse_composite_lit_elem(&mut self) -> ParseResult<CompositeLitElem> {
        let start = self.current.span.start;
        
        // Try to parse key: value
        let first = self.parse_expr()?;
        
        if self.eat(TokenKind::Colon) {
            // Has key
            let key = if let ExprKind::Ident(ident) = first.kind {
                CompositeLitKey::Ident(ident)
            } else {
                CompositeLitKey::Expr(first)
            };
            let value = self.parse_expr()?;
            Ok(CompositeLitElem {
                key: Some(key),
                value,
                span: Span::new(start, self.current.span.start),
            })
        } else {
            // No key, just value
            Ok(CompositeLitElem {
                key: None,
                value: first,
                span: Span::new(start, self.current.span.start),
            })
        }
    }

    fn parse_infix_expr(&mut self, left: Expr, prec: Precedence) -> ParseResult<Expr> {
        let start = left.span.start;
        
        match self.current.kind {
            // Binary operators
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash |
            TokenKind::Percent | TokenKind::Amp | TokenKind::Pipe | TokenKind::Caret |
            TokenKind::AmpCaret | TokenKind::Shl | TokenKind::Shr |
            TokenKind::EqEq | TokenKind::NotEq | TokenKind::Lt | TokenKind::LtEq |
            TokenKind::Gt | TokenKind::GtEq | TokenKind::AmpAmp | TokenKind::PipePipe => {
                let op_token = self.advance();
                let op = self.token_to_binary_op(&op_token.kind);
                let right = self.parse_expr_prec(prec)?;
                let span = Span::new(start, self.current.span.start);
                Ok(self.make_expr(ExprKind::Binary(Box::new(BinaryExpr { left, op, right })), span))
            }
            // Call expression
            TokenKind::LParen => {
                self.advance();
                // Check if this is a make/new call which takes a type as first argument
                let is_make_or_new = if let ExprKind::Ident(ref ident) = left.kind {
                    let name = self.interner.resolve(ident.symbol);
                    name == Some("make") || name == Some("new")
                } else {
                    false
                };
                let (args, spread) = if is_make_or_new {
                    self.parse_make_args()?
                } else {
                    self.parse_call_args()?
                };
                self.expect(TokenKind::RParen)?;
                let span = Span::new(start, self.current.span.start);
                Ok(self.make_expr(ExprKind::Call(Box::new(CallExpr { func: left, args, spread })), span))
            }
            // Index or slice expression
            TokenKind::LBracket => {
                self.advance();
                self.parse_index_or_slice(left, start)
            }
            // Selector or type assertion
            TokenKind::Dot => {
                self.advance();
                if self.at(TokenKind::LParen) {
                    // Type assertion: x.(T) or x.(type)
                    self.advance();
                    if self.at(TokenKind::Type) {
                        // Type switch guard: x.(type)
                        self.advance();
                        self.expect(TokenKind::RParen)?;
                        let span = Span::new(start, self.current.span.start);
                        Ok(self.make_expr(ExprKind::TypeAssert(Box::new(TypeAssertExpr { expr: left, ty: None })), span))
                    } else {
                        let ty = self.parse_type()?;
                        self.expect(TokenKind::RParen)?;
                        let span = Span::new(start, self.current.span.start);
                        Ok(self.make_expr(ExprKind::TypeAssert(Box::new(TypeAssertExpr { expr: left, ty: Some(ty) })), span))
                    }
                } else {
                    // Selector: x.field
                    let sel = self.parse_ident()?;
                    let span = Span::new(start, self.current.span.start);
                    Ok(self.make_expr(ExprKind::Selector(Box::new(SelectorExpr { expr: left, sel })), span))
                }
            }
            // Composite literal continuation
            TokenKind::LBrace if self.allow_composite_lit => {
                // This handles cases like: Type{...} where Type was parsed as an expression
                // We need to convert the left expression to a type
                if let ExprKind::Ident(ident) = left.kind {
                    let ty = self.make_type_expr(TypeExprKind::Ident(ident), left.span);
                    self.parse_composite_lit_body(ty)
                } else {
                    self.error("expected type for composite literal");
                    Err(())
                }
            }
            // Try-unwrap operator (error propagation)
            TokenKind::Question => {
                self.advance();
                let span = Span::new(start, self.current.span.start);
                Ok(self.make_expr(ExprKind::TryUnwrap(Box::new(left)), span))
            }
            _ => Ok(left),
        }
    }

    fn parse_call_args(&mut self) -> ParseResult<(Vec<Expr>, bool)> {
        let mut args = Vec::new();
        let mut spread = false;
        
        if self.at(TokenKind::RParen) {
            return Ok((args, spread));
        }
        
        loop {
            args.push(self.parse_expr()?);
            
            if self.eat(TokenKind::Ellipsis) {
                spread = true;
                break;
            }
            
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        
        Ok((args, spread))
    }
    
    /// Parse arguments for make/new calls where first argument is a type
    fn parse_make_args(&mut self) -> ParseResult<(Vec<Expr>, bool)> {
        let mut args = Vec::new();
        
        if self.at(TokenKind::RParen) {
            return Ok((args, false));
        }
        
        // First argument is a type - wrap it in a special expression
        let ty_start = self.current.span.start;
        let ty = self.parse_type()?;
        let ty_span = Span::new(ty_start, self.current.span.start);
        
        // Wrap the type in a TypeExpr expression (using Ident for named types, or we need a new variant)
        // For simplicity, convert the type to an expression representation
        let type_expr = self.type_to_expr(ty, ty_span)?;
        args.push(type_expr);
        
        // Parse remaining arguments as expressions
        while self.eat(TokenKind::Comma) {
            if self.at(TokenKind::RParen) {
                break;
            }
            args.push(self.parse_expr()?);
        }
        
        Ok((args, false))
    }
    
    /// Convert a type expression to an expression (for make/new first argument)
    fn type_to_expr(&mut self, ty: TypeExpr, span: Span) -> ParseResult<Expr> {
        // For identifier types, keep as Ident for backward compatibility
        if let TypeExprKind::Ident(ident) = ty.kind {
            return Ok(self.make_expr(ExprKind::Ident(ident), span));
        }
        // Wrap other types in TypeAsExpr
        Ok(self.make_expr(ExprKind::TypeAsExpr(Box::new(ty)), span))
    }

    fn parse_index_or_slice(&mut self, expr: Expr, start: gox_common::span::BytePos) -> ParseResult<Expr> {
        // Could be: a[i], a[low:high], a[low:], a[:high], a[:]
        
        let low = if self.at(TokenKind::Colon) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        
        if self.eat(TokenKind::Colon) {
            // Slice expression
            let high = if self.at(TokenKind::RBracket) {
                None
            } else {
                Some(self.parse_expr()?)
            };
            self.expect(TokenKind::RBracket)?;
            let span = Span::new(start, self.current.span.start);
            Ok(self.make_expr(ExprKind::Slice(Box::new(SliceExpr { expr, low, high })), span))
        } else {
            // Index expression
            self.expect(TokenKind::RBracket)?;
            let index = low.ok_or_else(|| {
                self.error("missing index in index expression");
            })?;
            let span = Span::new(start, self.current.span.start);
            Ok(self.make_expr(ExprKind::Index(Box::new(IndexExpr { expr, index })), span))
        }
    }

    fn token_to_binary_op(&self, kind: &TokenKind) -> BinaryOp {
        match kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            TokenKind::Percent => BinaryOp::Rem,
            TokenKind::Amp => BinaryOp::And,
            TokenKind::Pipe => BinaryOp::Or,
            TokenKind::Caret => BinaryOp::Xor,
            TokenKind::AmpCaret => BinaryOp::AndNot,
            TokenKind::Shl => BinaryOp::Shl,
            TokenKind::Shr => BinaryOp::Shr,
            TokenKind::EqEq => BinaryOp::Eq,
            TokenKind::NotEq => BinaryOp::NotEq,
            TokenKind::Lt => BinaryOp::Lt,
            TokenKind::LtEq => BinaryOp::LtEq,
            TokenKind::Gt => BinaryOp::Gt,
            TokenKind::GtEq => BinaryOp::GtEq,
            TokenKind::AmpAmp => BinaryOp::LogAnd,
            TokenKind::PipePipe => BinaryOp::LogOr,
            _ => unreachable!("not a binary operator: {:?}", kind),
        }
    }
}
