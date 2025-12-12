//! Expression parsing for GoX using Pratt parsing.
//!
//! Handles all expression forms including binary, unary, calls, indexing,
//! selectors, and composite literals.

use super::Parser;
use crate::ast::*;
use crate::token::TokenKind;
use gox_common::GoxResult;

// ═══════════════════════════════════════════════════════════════════════════
// Precedence Levels
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    Lowest = 0,
    Or,          // ||
    And,         // &&
    Equals,      // == !=
    LessGreater, // < <= > >=
    Sum,         // + -
    Product,     // * / %
    Prefix,      // -x !x
    Call,        // f(x) a[i] a.b
}

impl Precedence {
    fn from_token(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::Or => Precedence::Or,
            TokenKind::And => Precedence::And,
            TokenKind::Eq | TokenKind::NotEq => Precedence::Equals,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => {
                Precedence::LessGreater
            }
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Product,
            TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot => Precedence::Call,
            // Note: LBrace is NOT included here - composite literals are handled
            // specially in parse_ident_or_composite, not as infix operators
            _ => Precedence::Lowest,
        }
    }
}

impl<'a> Parser<'a> {
    // ═══════════════════════════════════════════════════════════════════════
    // Main Expression Parser
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse an expression with the given minimum precedence.
    pub(super) fn parse_expr(&mut self) -> GoxResult<Expr> {
        self.parse_expr_prec(Precedence::Lowest)
    }

    fn parse_expr_prec(&mut self, min_prec: Precedence) -> GoxResult<Expr> {
        // Parse prefix (left side)
        let mut left = self.parse_prefix()?;

        // Loop: while peek has higher precedence, parse infix
        while !self.cur_is(&TokenKind::Semi) && !self.cur_is(&TokenKind::Eof) {
            let prec = Precedence::from_token(&self.current.kind);
            if prec <= min_prec {
                break;
            }

            left = self.parse_infix(left, prec)?;
        }

        Ok(left)
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Prefix Expressions
    // ═══════════════════════════════════════════════════════════════════════

    fn parse_prefix(&mut self) -> GoxResult<Expr> {
        match &self.current.kind {
            // Identifiers
            TokenKind::Ident(_) => self.parse_ident_or_composite(),

            // Literals
            TokenKind::Int(n) => {
                let n = *n;
                let span = self.current.span;
                self.next_token();
                Ok(Expr::Literal(Literal::Int(n, span)))
            }
            TokenKind::Float(f) => {
                let f = *f;
                let span = self.current.span;
                self.next_token();
                Ok(Expr::Literal(Literal::Float(f, span)))
            }
            TokenKind::String(s) => {
                let s = s.clone();
                let span = self.current.span;
                self.next_token();
                Ok(Expr::Literal(Literal::String(s, span)))
            }
            TokenKind::True => {
                let span = self.current.span;
                self.next_token();
                Ok(Expr::Literal(Literal::Bool(true, span)))
            }
            TokenKind::False => {
                let span = self.current.span;
                self.next_token();
                Ok(Expr::Literal(Literal::Bool(false, span)))
            }
            TokenKind::Nil => {
                let span = self.current.span;
                self.next_token();
                Ok(Expr::Literal(Literal::Nil(span)))
            }

            // Grouped expression
            TokenKind::LParen => {
                let start = self.current.span;
                self.next_token();

                // Enable composite literals inside parens
                let saved = self.allow_composite_lit;
                self.allow_composite_lit = true;
                let expr = self.parse_expr()?;
                self.allow_composite_lit = saved;

                let end = self.expect(&TokenKind::RParen)?;
                Ok(Expr::Grouped(Box::new(expr), start.to(&end)))
            }

            // Unary operators
            TokenKind::Minus => {
                let start = self.current.span;
                self.next_token();
                let expr = self.parse_expr_prec(Precedence::Prefix)?;
                Ok(Expr::Unary(Box::new(UnaryExpr {
                    op: UnaryOp::Neg,
                    span: start.to(&expr.span()),
                    expr,
                })))
            }
            TokenKind::Not => {
                let start = self.current.span;
                self.next_token();
                let expr = self.parse_expr_prec(Precedence::Prefix)?;
                Ok(Expr::Unary(Box::new(UnaryExpr {
                    op: UnaryOp::Not,
                    span: start.to(&expr.span()),
                    expr,
                })))
            }
            TokenKind::Plus => {
                let start = self.current.span;
                self.next_token();
                let expr = self.parse_expr_prec(Precedence::Prefix)?;
                Ok(Expr::Unary(Box::new(UnaryExpr {
                    op: UnaryOp::Pos,
                    span: start.to(&expr.span()),
                    expr,
                })))
            }

            // Type-starting composite literals: []T{}, [N]T{}, map[K]V{}, struct{}{}
            TokenKind::LBracket | TokenKind::Map | TokenKind::Struct => {
                if self.allow_composite_lit {
                    self.parse_composite_lit_with_type()
                } else {
                    Err(self.error_msg("unexpected token in expression"))
                }
            }

            _ => Err(self.error_msg("unexpected token in expression")),
        }
    }

    /// Parse identifier or identifier followed by composite literal.
    fn parse_ident_or_composite(&mut self) -> GoxResult<Expr> {
        let id = self.parse_ident()?;

        // Check for composite literal: Type{...}
        if self.allow_composite_lit && self.cur_is(&TokenKind::LBrace) {
            return self.parse_composite_lit(Type::Named(id));
        }

        Ok(Expr::Ident(id))
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Infix Expressions
    // ═══════════════════════════════════════════════════════════════════════

    fn parse_infix(&mut self, left: Expr, prec: Precedence) -> GoxResult<Expr> {
        match &self.current.kind {
            // Binary operators
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq
            | TokenKind::And
            | TokenKind::Or => {
                let op = self.parse_binary_op()?;
                let right = self.parse_expr_prec(prec)?;
                let span = left.span().to(&right.span());
                Ok(Expr::Binary(Box::new(BinaryExpr {
                    left,
                    op,
                    right,
                    span,
                })))
            }

            // Function call
            TokenKind::LParen => {
                self.next_token();
                let args = self.parse_call_args()?;
                let end = self.expect(&TokenKind::RParen)?;
                Ok(Expr::Call(Box::new(CallExpr {
                    span: left.span().to(&end),
                    func: left,
                    args,
                })))
            }

            // Index
            TokenKind::LBracket => {
                self.next_token();
                let index = self.parse_expr()?;
                let end = self.expect(&TokenKind::RBracket)?;
                Ok(Expr::Index(Box::new(IndexExpr {
                    span: left.span().to(&end),
                    expr: left,
                    index,
                })))
            }

            // Selector
            TokenKind::Dot => {
                self.next_token();
                let field = self.parse_ident()?;
                Ok(Expr::Selector(Box::new(SelectorExpr {
                    span: left.span().to(&field.span),
                    expr: left,
                    field,
                })))
            }

            // Composite literal (only if allowed)
            TokenKind::LBrace if self.allow_composite_lit => {
                // Convert left expression to type
                match left {
                    Expr::Ident(id) => self.parse_composite_lit(Type::Named(id)),
                    _ => Err(self.error_msg("expected type for composite literal")),
                }
            }

            _ => Err(self.error_msg("unexpected infix operator")),
        }
    }

    fn parse_binary_op(&mut self) -> GoxResult<BinaryOp> {
        let op = match &self.current.kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            TokenKind::Percent => BinaryOp::Mod,
            TokenKind::Eq => BinaryOp::Eq,
            TokenKind::NotEq => BinaryOp::NotEq,
            TokenKind::Lt => BinaryOp::Lt,
            TokenKind::LtEq => BinaryOp::LtEq,
            TokenKind::Gt => BinaryOp::Gt,
            TokenKind::GtEq => BinaryOp::GtEq,
            TokenKind::And => BinaryOp::And,
            TokenKind::Or => BinaryOp::Or,
            _ => return Err(self.error_msg("expected binary operator")),
        };
        self.next_token();
        Ok(op)
    }

    fn parse_call_args(&mut self) -> GoxResult<Vec<Expr>> {
        if self.cur_is(&TokenKind::RParen) {
            return Ok(Vec::new());
        }

        let mut args = vec![self.parse_expr()?];
        while self.eat(&TokenKind::Comma) {
            args.push(self.parse_expr()?);
        }
        Ok(args)
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Composite Literals (§9.4)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse composite literal with type already parsed.
    fn parse_composite_lit(&mut self, ty: Type) -> GoxResult<Expr> {
        let start = ty.span();
        self.expect(&TokenKind::LBrace)?;

        let elements = self.parse_element_list()?;

        let end = self.expect(&TokenKind::RBrace)?;

        Ok(Expr::CompositeLit(Box::new(CompositeLit {
            ty,
            elements,
            span: start.to(&end),
        })))
    }

    /// Parse composite literal where we need to parse the type first.
    fn parse_composite_lit_with_type(&mut self) -> GoxResult<Expr> {
        let ty = self.parse_type()?;
        self.parse_composite_lit(ty)
    }

    fn parse_element_list(&mut self) -> GoxResult<Vec<Element>> {
        let mut elements = Vec::new();

        while !self.cur_is(&TokenKind::RBrace) && !self.at_eof() {
            elements.push(self.parse_element()?);

            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }

        Ok(elements)
    }

    fn parse_element(&mut self) -> GoxResult<Element> {
        let start = self.current.span;

        // Try to parse key: value or just value
        let first = self.parse_expr()?;

        if self.eat(&TokenKind::Colon) {
            // Key: Value form
            let key = match first {
                Expr::Ident(id) => ElementKey::Name(id),
                other => ElementKey::Expr(other),
            };
            let value = self.parse_expr()?;
            Ok(Element {
                key: Some(key),
                span: start.to(&value.span()),
                value,
            })
        } else {
            // Value only
            Ok(Element {
                key: None,
                span: first.span(),
                value: first,
            })
        }
    }
}
