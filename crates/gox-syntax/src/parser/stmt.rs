//! Statement parsing for GoX.
//!
//! Handles statements: block, if, for, switch, return, assignment, etc.

use super::Parser;
use crate::ast::*;
use crate::token::TokenKind;
use gox_common::GoxResult;

impl<'a> Parser<'a> {
    // ═══════════════════════════════════════════════════════════════════════
    // Block
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse a block: `{ stmts }`
    pub(super) fn parse_block(&mut self) -> GoxResult<Block> {
        let start = self.expect(&TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        while !self.cur_is(&TokenKind::RBrace) && !self.at_eof() {
            stmts.push(self.parse_stmt()?);
        }

        let end = self.expect(&TokenKind::RBrace)?;

        Ok(Block {
            stmts,
            span: start.to(&end),
        })
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Statement Dispatch
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse a statement.
    pub(super) fn parse_stmt(&mut self) -> GoxResult<Stmt> {
        match &self.current.kind {
            TokenKind::LBrace => Ok(Stmt::Block(self.parse_block()?)),
            TokenKind::Var => Ok(Stmt::Var(self.parse_var_decl()?)),
            TokenKind::Const => Ok(Stmt::Const(self.parse_const_decl()?)),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Switch => self.parse_switch_stmt(),
            TokenKind::Break => {
                let span = self.current.span;
                self.next_token();
                self.expect_semi()?;
                Ok(Stmt::Break(span))
            }
            TokenKind::Continue => {
                let span = self.current.span;
                self.next_token();
                self.expect_semi()?;
                Ok(Stmt::Continue(span))
            }
            TokenKind::Semi => {
                let span = self.current.span;
                self.next_token();
                Ok(Stmt::Empty(span))
            }
            _ => self.parse_simple_stmt(),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Simple Statement (§8.3)
    // ═══════════════════════════════════════════════════════════════════════

    /// Parse simple statement: assignment, short var decl, or expression.
    fn parse_simple_stmt(&mut self) -> GoxResult<Stmt> {
        // Parse the left side as expression(s)
        let exprs = self.parse_expr_list()?;

        match &self.current.kind {
            // Short variable declaration: x, y := a, b
            TokenKind::ColonAssign => {
                self.next_token();
                let values = self.parse_expr_list()?;
                self.expect_semi()?;

                // Convert expressions to identifiers
                let mut names = Vec::new();
                for expr in exprs {
                    match expr {
                        Expr::Ident(id) => names.push(id),
                        _ => {
                            return Err(self.error_at(
                                "expected identifier in short variable declaration",
                                expr.span(),
                            ))
                        }
                    }
                }

                let span = names
                    .first()
                    .unwrap()
                    .span
                    .to(&values.last().unwrap().span());
                Ok(Stmt::ShortVar(ShortVarDecl {
                    names,
                    values,
                    span,
                }))
            }

            // Assignment: x, y = a, b or x += a
            TokenKind::Assign
            | TokenKind::PlusAssign
            | TokenKind::MinusAssign
            | TokenKind::StarAssign
            | TokenKind::SlashAssign
            | TokenKind::PercentAssign => {
                let op = match &self.current.kind {
                    TokenKind::Assign => AssignOp::Assign,
                    TokenKind::PlusAssign => AssignOp::PlusAssign,
                    TokenKind::MinusAssign => AssignOp::MinusAssign,
                    TokenKind::StarAssign => AssignOp::StarAssign,
                    TokenKind::SlashAssign => AssignOp::SlashAssign,
                    TokenKind::PercentAssign => AssignOp::PercentAssign,
                    _ => unreachable!(),
                };
                self.next_token();

                let right = self.parse_expr_list()?;
                self.expect_semi()?;

                let span = exprs
                    .first()
                    .unwrap()
                    .span()
                    .to(&right.last().unwrap().span());
                Ok(Stmt::Assign(Assignment {
                    left: exprs,
                    op,
                    right,
                    span,
                }))
            }

            // Expression statement
            _ => {
                if exprs.len() != 1 {
                    return Err(self.error_msg("expected assignment or short variable declaration"));
                }
                let expr = exprs.into_iter().next().unwrap();
                let span = expr.span();
                self.expect_semi()?;
                Ok(Stmt::Expr(ExprStmt { expr, span }))
            }
        }
    }

    /// Parse comma-separated expression list.
    fn parse_expr_list(&mut self) -> GoxResult<Vec<Expr>> {
        let mut exprs = vec![self.parse_expr()?];
        while self.eat(&TokenKind::Comma) {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Return Statement (§8.4)
    // ═══════════════════════════════════════════════════════════════════════

    fn parse_return_stmt(&mut self) -> GoxResult<Stmt> {
        let start = self.expect(&TokenKind::Return)?;

        let values = if self.cur_is(&TokenKind::Semi) {
            Vec::new()
        } else {
            self.parse_expr_list()?
        };

        self.expect_semi()?;

        let end = values.last().map(|e| e.span()).unwrap_or(start);
        Ok(Stmt::Return(ReturnStmt {
            values,
            span: start.to(&end),
        }))
    }

    // ═══════════════════════════════════════════════════════════════════════
    // If Statement (§8.5)
    // ═══════════════════════════════════════════════════════════════════════

    fn parse_if_stmt(&mut self) -> GoxResult<Stmt> {
        let start = self.expect(&TokenKind::If)?;

        // Disable composite literals in condition
        let saved = self.allow_composite_lit;
        self.allow_composite_lit = false;
        let cond = self.parse_expr()?;
        self.allow_composite_lit = saved;

        let then_block = self.parse_block()?;

        let else_clause = if self.eat(&TokenKind::Else) {
            if self.cur_is(&TokenKind::If) {
                // else if
                let if_stmt = self.parse_if_stmt()?;
                match if_stmt {
                    Stmt::If(inner) => Some(ElseClause::If(inner)),
                    _ => unreachable!(),
                }
            } else {
                // else block
                Some(ElseClause::Block(self.parse_block()?))
            }
        } else {
            None
        };

        let end = else_clause
            .as_ref()
            .map(|e| match e {
                ElseClause::Block(b) => b.span,
                ElseClause::If(i) => i.span,
            })
            .unwrap_or(then_block.span);

        Ok(Stmt::If(Box::new(IfStmt {
            cond,
            then_block,
            else_clause,
            span: start.to(&end),
        })))
    }

    // ═══════════════════════════════════════════════════════════════════════
    // For Statement (§8.6)
    // ═══════════════════════════════════════════════════════════════════════

    fn parse_for_stmt(&mut self) -> GoxResult<Stmt> {
        let start = self.expect(&TokenKind::For)?;

        // Disable composite literals
        let saved = self.allow_composite_lit;
        self.allow_composite_lit = false;

        // Infinite loop: for { }
        if self.cur_is(&TokenKind::LBrace) {
            self.allow_composite_lit = saved;
            let body = self.parse_block()?;
            let body_span = body.span;
            return Ok(Stmt::For(Box::new(ForStmt {
                init: None,
                cond: None,
                post: None,
                body,
                span: start.to(&body_span),
            })));
        }

        // Try to parse first part and determine form
        let first = self.parse_for_clause_first()?;

        // Check if semicolon follows (three-clause form)
        if self.eat(&TokenKind::Semi) {
            // Three-clause: for init; cond; post { }
            let cond = if !self.cur_is(&TokenKind::Semi) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            self.expect(&TokenKind::Semi)?;

            let post = if !self.cur_is(&TokenKind::LBrace) {
                Some(Box::new(self.parse_for_post_stmt()?))
            } else {
                None
            };

            self.allow_composite_lit = saved;
            let body = self.parse_block()?;
            let body_span = body.span;

            Ok(Stmt::For(Box::new(ForStmt {
                init: first.map(Box::new),
                cond,
                post,
                body,
                span: start.to(&body_span),
            })))
        } else {
            // While-style: for cond { }
            self.allow_composite_lit = saved;
            let cond = match first {
                Some(Stmt::Expr(e)) => Some(e.expr),
                None => None,
                _ => return Err(self.error_msg("expected expression as for condition")),
            };

            let body = self.parse_block()?;
            let body_span = body.span;
            Ok(Stmt::For(Box::new(ForStmt {
                init: None,
                cond,
                post: None,
                body,
                span: start.to(&body_span),
            })))
        }
    }

    /// Parse the first part of a for clause (could be init or condition).
    fn parse_for_clause_first(&mut self) -> GoxResult<Option<Stmt>> {
        if self.cur_is(&TokenKind::Semi) || self.cur_is(&TokenKind::LBrace) {
            return Ok(None);
        }

        let exprs = self.parse_expr_list()?;

        // Check for short var decl
        if self.cur_is(&TokenKind::ColonAssign) {
            self.next_token();
            let values = self.parse_expr_list()?;

            let mut names = Vec::new();
            for expr in exprs {
                match expr {
                    Expr::Ident(id) => names.push(id),
                    _ => return Err(self.error_at("expected identifier", expr.span())),
                }
            }

            let span = names
                .first()
                .unwrap()
                .span
                .to(&values.last().unwrap().span());
            return Ok(Some(Stmt::ShortVar(ShortVarDecl {
                names,
                values,
                span,
            })));
        }

        // Check for assignment
        if let Some(op) = self.try_assign_op() {
            self.next_token();
            let right = self.parse_expr_list()?;
            let span = exprs
                .first()
                .unwrap()
                .span()
                .to(&right.last().unwrap().span());
            return Ok(Some(Stmt::Assign(Assignment {
                left: exprs,
                op,
                right,
                span,
            })));
        }

        // Just an expression
        if exprs.len() == 1 {
            let expr = exprs.into_iter().next().unwrap();
            let span = expr.span();
            return Ok(Some(Stmt::Expr(ExprStmt { expr, span })));
        }

        Err(self.error_msg("invalid for clause"))
    }

    /// Parse post statement in for (no semicolon).
    fn parse_for_post_stmt(&mut self) -> GoxResult<Stmt> {
        let exprs = self.parse_expr_list()?;

        // Check for short var decl (rare but allowed)
        if self.cur_is(&TokenKind::ColonAssign) {
            self.next_token();
            let values = self.parse_expr_list()?;

            let mut names = Vec::new();
            for expr in exprs {
                match expr {
                    Expr::Ident(id) => names.push(id),
                    _ => return Err(self.error_at("expected identifier", expr.span())),
                }
            }

            let span = names
                .first()
                .unwrap()
                .span
                .to(&values.last().unwrap().span());
            return Ok(Stmt::ShortVar(ShortVarDecl {
                names,
                values,
                span,
            }));
        }

        // Check for assignment
        if let Some(op) = self.try_assign_op() {
            self.next_token();
            let right = self.parse_expr_list()?;
            let span = exprs
                .first()
                .unwrap()
                .span()
                .to(&right.last().unwrap().span());
            return Ok(Stmt::Assign(Assignment {
                left: exprs,
                op,
                right,
                span,
            }));
        }

        // Expression statement
        if exprs.len() == 1 {
            let expr = exprs.into_iter().next().unwrap();
            let span = expr.span();
            return Ok(Stmt::Expr(ExprStmt { expr, span }));
        }

        Err(self.error_msg("invalid for post statement"))
    }

    fn try_assign_op(&self) -> Option<AssignOp> {
        match &self.current.kind {
            TokenKind::Assign => Some(AssignOp::Assign),
            TokenKind::PlusAssign => Some(AssignOp::PlusAssign),
            TokenKind::MinusAssign => Some(AssignOp::MinusAssign),
            TokenKind::StarAssign => Some(AssignOp::StarAssign),
            TokenKind::SlashAssign => Some(AssignOp::SlashAssign),
            TokenKind::PercentAssign => Some(AssignOp::PercentAssign),
            _ => None,
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Switch Statement (§8.7)
    // ═══════════════════════════════════════════════════════════════════════

    fn parse_switch_stmt(&mut self) -> GoxResult<Stmt> {
        let start = self.expect(&TokenKind::Switch)?;

        // Disable composite literals
        let saved = self.allow_composite_lit;
        self.allow_composite_lit = false;
        let expr = self.parse_expr()?;
        self.allow_composite_lit = saved;

        self.expect(&TokenKind::LBrace)?;

        let mut cases = Vec::new();
        let mut default = None;

        while !self.cur_is(&TokenKind::RBrace) && !self.at_eof() {
            if self.cur_is(&TokenKind::Case) {
                cases.push(self.parse_case_clause()?);
            } else if self.cur_is(&TokenKind::Default) {
                if default.is_some() {
                    return Err(self.error_msg("multiple default clauses"));
                }
                default = Some(self.parse_default_clause()?);
            } else {
                return Err(self.error_msg("expected 'case' or 'default'"));
            }
        }

        let end = self.expect(&TokenKind::RBrace)?;

        Ok(Stmt::Switch(Box::new(SwitchStmt {
            expr,
            cases,
            default,
            span: start.to(&end),
        })))
    }

    fn parse_case_clause(&mut self) -> GoxResult<CaseClause> {
        let start = self.expect(&TokenKind::Case)?;
        let exprs = self.parse_expr_list()?;
        self.expect(&TokenKind::Colon)?;

        let mut stmts = Vec::new();
        while !self.cur_is(&TokenKind::Case)
            && !self.cur_is(&TokenKind::Default)
            && !self.cur_is(&TokenKind::RBrace)
            && !self.at_eof()
        {
            stmts.push(self.parse_stmt()?);
        }

        let end = stmts.last().map(|s| s.span()).unwrap_or(start);

        Ok(CaseClause {
            exprs,
            stmts,
            span: start.to(&end),
        })
    }

    fn parse_default_clause(&mut self) -> GoxResult<DefaultClause> {
        let start = self.expect(&TokenKind::Default)?;
        self.expect(&TokenKind::Colon)?;

        let mut stmts = Vec::new();
        while !self.cur_is(&TokenKind::Case)
            && !self.cur_is(&TokenKind::Default)
            && !self.cur_is(&TokenKind::RBrace)
            && !self.at_eof()
        {
            stmts.push(self.parse_stmt()?);
        }

        let end = stmts.last().map(|s| s.span()).unwrap_or(start);

        Ok(DefaultClause {
            stmts,
            span: start.to(&end),
        })
    }
}
