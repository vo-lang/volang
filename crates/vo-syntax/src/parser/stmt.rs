//! Statement parsing.

use super::{Parser, ParseResult};
use crate::ast::*;
use crate::token::TokenKind;
use vo_common::span::Span;
use vo_common::symbol::Ident;

impl<'a> Parser<'a> {
    /// Parses a statement.
    pub fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        let start = self.current.span.start;
        
        let kind = match self.current.kind {
            TokenKind::Semicolon => {
                self.advance();
                StmtKind::Empty
            }
            TokenKind::LBrace => {
                let block = self.parse_block()?;
                StmtKind::Block(block)
            }
            TokenKind::Var => {
                let decl = self.parse_var_decl()?;
                StmtKind::Var(decl)
            }
            TokenKind::Const => {
                let decl = self.parse_const_decl()?;
                StmtKind::Const(decl)
            }
            TokenKind::Type => {
                let decl = self.parse_type_decl()?;
                StmtKind::Type(decl)
            }
            TokenKind::Return => {
                self.advance();
                let values = if self.at(TokenKind::Semicolon) || self.at(TokenKind::RBrace) {
                    Vec::new()
                } else {
                    self.parse_expr_list()?
                };
                self.expect_semi();
                StmtKind::Return(ReturnStmt { values })
            }
            TokenKind::If => self.parse_if_stmt()?,
            TokenKind::For => self.parse_for_stmt()?,
            TokenKind::Switch => self.parse_switch_stmt()?,
            TokenKind::Select => self.parse_select_stmt()?,
            TokenKind::Go => {
                self.advance();
                let call = self.parse_expr()?;
                self.expect_semi();
                StmtKind::Go(GoStmt { call })
            }
            TokenKind::Defer => {
                self.advance();
                let call = self.parse_expr()?;
                self.expect_semi();
                StmtKind::Defer(DeferStmt { call })
            }
            TokenKind::Errdefer => {
                self.advance();
                let call = self.parse_expr()?;
                self.expect_semi();
                StmtKind::ErrDefer(ErrDeferStmt { call })
            }
            TokenKind::Fail => {
                self.advance();
                let error = self.parse_expr()?;
                self.expect_semi();
                StmtKind::Fail(FailStmt { error })
            }
            TokenKind::Break => {
                self.advance();
                let label = if self.at(TokenKind::Ident) {
                    Some(self.parse_ident()?)
                } else {
                    None
                };
                self.expect_semi();
                StmtKind::Break(BreakStmt { label })
            }
            TokenKind::Continue => {
                self.advance();
                let label = if self.at(TokenKind::Ident) {
                    Some(self.parse_ident()?)
                } else {
                    None
                };
                self.expect_semi();
                StmtKind::Continue(ContinueStmt { label })
            }
            TokenKind::Goto => {
                self.advance();
                let label = self.parse_ident()?;
                self.expect_semi();
                StmtKind::Goto(GotoStmt { label })
            }
            TokenKind::Fallthrough => {
                self.advance();
                self.expect_semi();
                StmtKind::Fallthrough
            }
            _ => {
                // Could be: expression, assignment, short var decl, inc/dec, send, or labeled stmt
                return self.parse_simple_stmt();
            }
        };
        
        Ok(Stmt {
            kind,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_simple_stmt(&mut self) -> ParseResult<Stmt> {
        let start = self.current.span.start;
        
        // Check for labeled statement: label: stmt
        if self.at(TokenKind::Ident) && self.peek_is(TokenKind::Colon) {
            let label = self.parse_ident()?;
            self.expect(TokenKind::Colon)?;
            let stmt = self.parse_stmt()?;
            return Ok(Stmt {
                kind: StmtKind::Labeled(LabeledStmt {
                    label,
                    stmt: Box::new(stmt),
                }),
                span: Span::new(start, self.current.span.start),
            });
        }
        
        // Parse left-hand side expression(s)
        let left = self.parse_expr_list()?;
        
        let kind = match self.current.kind {
            // Short variable declaration: x := expr
            TokenKind::ColonEq => {
                self.advance();
                let names = self.exprs_to_idents(left)?;
                let values = self.parse_expr_list()?;
                self.expect_semi();
                StmtKind::ShortVar(ShortVarDecl { names, values })
            }
            // Assignment: x = expr, x += expr, etc.
            TokenKind::Eq | TokenKind::PlusEq | TokenKind::MinusEq |
            TokenKind::StarEq | TokenKind::SlashEq | TokenKind::PercentEq |
            TokenKind::AmpEq | TokenKind::PipeEq | TokenKind::CaretEq |
            TokenKind::AmpCaretEq | TokenKind::ShlEq | TokenKind::ShrEq => {
                let op = self.token_to_assign_op(&self.current.kind);
                self.advance();
                let rhs = self.parse_expr_list()?;
                self.expect_semi();
                StmtKind::Assign(AssignStmt { lhs: left, op, rhs })
            }
            // Increment/decrement: x++, x--
            TokenKind::PlusPlus => {
                self.advance();
                self.expect_semi();
                if left.len() != 1 {
                    self.error("increment requires single expression");
                    return Err(());
                }
                StmtKind::IncDec(IncDecStmt { expr: left.into_iter().next().unwrap(), is_inc: true })
            }
            TokenKind::MinusMinus => {
                self.advance();
                self.expect_semi();
                if left.len() != 1 {
                    self.error("decrement requires single expression");
                    return Err(());
                }
                StmtKind::IncDec(IncDecStmt { expr: left.into_iter().next().unwrap(), is_inc: false })
            }
            // Send statement: ch <- value
            TokenKind::Arrow => {
                self.advance();
                if left.len() != 1 {
                    self.error("send requires single channel expression");
                    return Err(());
                }
                let value = self.parse_expr()?;
                self.expect_semi();
                StmtKind::Send(SendStmt { chan: left.into_iter().next().unwrap(), value })
            }
            // Expression statement
            _ => {
                self.expect_semi();
                if left.len() != 1 {
                    self.error("expression statement requires single expression");
                    return Err(());
                }
                StmtKind::Expr(left.into_iter().next().unwrap())
            }
        };
        
        Ok(Stmt {
            kind,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_if_stmt(&mut self) -> ParseResult<StmtKind> {
        self.expect(TokenKind::If)?;
        
        // Disable composite literals in condition
        let saved = self.allow_composite_lit;
        self.allow_composite_lit = false;
        
        // Parse init or condition
        let first = self.parse_simple_stmt_or_expr()?;
        
        let (init, cond) = if self.at(TokenKind::Semicolon) {
            // Has init statement
            self.advance();
            let cond = self.parse_expr()?;
            (Some(Box::new(first)), cond)
        } else {
            // No init, first is the condition
            match first.kind {
                StmtKind::Expr(expr) => (None, expr),
                _ => {
                    self.error("expected expression in if condition");
                    return Err(());
                }
            }
        };
        
        self.allow_composite_lit = saved;
        
        let then = self.parse_block()?;
        
        let else_ = if self.eat(TokenKind::Else) {
            if self.at(TokenKind::If) {
                let else_if = self.parse_stmt()?;
                Some(Box::new(else_if))
            } else {
                let else_block = self.parse_block()?;
                Some(Box::new(Stmt {
                    kind: StmtKind::Block(else_block.clone()),
                    span: else_block.span,
                }))
            }
        } else {
            None
        };
        
        Ok(StmtKind::If(IfStmt { init, cond, then, else_ }))
    }

    fn parse_for_stmt(&mut self) -> ParseResult<StmtKind> {
        self.expect(TokenKind::For)?;
        
        // Disable composite literals in for header
        let saved = self.allow_composite_lit;
        self.allow_composite_lit = false;
        
        let clause = if self.at(TokenKind::LBrace) {
            // Infinite loop: for { }
            ForClause::Cond(None)
        } else if self.at(TokenKind::Range) {
            // for range expr { }
            self.advance();
            let expr = self.parse_expr()?;
            ForClause::Range { key: None, value: None, define: false, expr }
        } else {
            // Try to determine the form
            let first = self.parse_simple_stmt_or_expr()?;
            
            if self.at(TokenKind::LBrace) {
                // Condition only: for cond { }
                match first.kind {
                    StmtKind::Expr(expr) => ForClause::Cond(Some(expr)),
                    _ => {
                        self.error("expected expression in for condition");
                        return Err(());
                    }
                }
            } else if self.at(TokenKind::Semicolon) {
                // Three-clause form: for init; cond; post { }
                self.advance();
                let cond = if self.at(TokenKind::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(TokenKind::Semicolon)?;
                let post = if self.at(TokenKind::LBrace) {
                    None
                } else {
                    Some(Box::new(self.parse_simple_stmt_or_expr()?))
                };
                ForClause::Three { init: Some(Box::new(first)), cond, post }
            } else {
                // Check for range clause after short var decl or assignment
                match &first.kind {
                    StmtKind::ShortVar(svd) if self.at(TokenKind::Range) => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        let exprs: Vec<Expr> = svd.names.iter().map(|ident| {
                            self.make_expr(ExprKind::Ident(ident.clone()), ident.span)
                        }).collect();
                        let (key, value) = self.exprs_to_key_value(&exprs);
                        ForClause::Range { key, value, define: true, expr }
                    }
                    StmtKind::Assign(assign) if self.at(TokenKind::Range) => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        let (key, value) = self.exprs_to_key_value(&assign.lhs);
                        ForClause::Range { key, value, define: false, expr }
                    }
                    _ => {
                        self.error("invalid for clause");
                        return Err(());
                    }
                }
            }
        };
        
        self.allow_composite_lit = saved;
        let body = self.parse_block()?;
        
        Ok(StmtKind::For(ForStmt { clause, body }))
    }

    fn parse_switch_stmt(&mut self) -> ParseResult<StmtKind> {
        self.expect(TokenKind::Switch)?;
        
        let saved = self.allow_composite_lit;
        self.allow_composite_lit = false;
        
        // Parse optional init and tag
        let (init, tag) = if self.at(TokenKind::LBrace) {
            (None, None)
        } else {
            let first = self.parse_simple_stmt_or_expr()?;
            
            if self.at(TokenKind::Semicolon) {
                self.advance();
                let tag = if self.at(TokenKind::LBrace) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                (Some(Box::new(first)), tag)
            } else {
                match first.kind {
                    StmtKind::Expr(expr) => {
                        // Check for type switch: x.(type)
                        if let ExprKind::TypeAssert(ref ta) = expr.kind {
                            if ta.ty.is_none() {
                                // Type switch
                                self.allow_composite_lit = saved;
                                return self.parse_type_switch_body(None, None, expr);
                            }
                        }
                        (None, Some(expr))
                    }
                    _ => {
                        self.error("expected expression in switch");
                        return Err(());
                    }
                }
            }
        };
        
        self.allow_composite_lit = saved;
        
        // Check if this is a type switch
        if let Some(ref tag_expr) = tag {
            if let ExprKind::TypeAssert(ref ta) = tag_expr.kind {
                if ta.ty.is_none() {
                    return self.parse_type_switch_body(init, None, tag_expr.clone());
                }
            }
        }
        
        // Regular switch
        self.expect(TokenKind::LBrace)?;
        let mut cases = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            cases.push(self.parse_case_clause()?);
        }
        self.expect(TokenKind::RBrace)?;
        
        Ok(StmtKind::Switch(SwitchStmt { init, tag, cases }))
    }

    fn parse_type_switch_body(&mut self, init: Option<Box<Stmt>>, assign: Option<Ident>, expr: Expr) -> ParseResult<StmtKind> {
        self.expect(TokenKind::LBrace)?;
        let mut cases = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            cases.push(self.parse_type_case_clause()?);
        }
        self.expect(TokenKind::RBrace)?;
        
        Ok(StmtKind::TypeSwitch(TypeSwitchStmt { init, assign, expr, cases }))
    }

    fn parse_case_clause(&mut self) -> ParseResult<CaseClause> {
        let start = self.current.span.start;
        
        let exprs = if self.eat(TokenKind::Case) {
            self.parse_expr_list()?
        } else if self.eat(TokenKind::Default) {
            Vec::new()
        } else {
            self.error_expected("'case' or 'default'");
            return Err(());
        };
        
        self.expect(TokenKind::Colon)?;
        
        let mut body = Vec::new();
        while !self.at(TokenKind::Case) && !self.at(TokenKind::Default) && !self.at(TokenKind::RBrace) && !self.at_eof() {
            body.push(self.parse_stmt()?);
        }
        
        Ok(CaseClause {
            exprs,
            body,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_type_case_clause(&mut self) -> ParseResult<TypeCaseClause> {
        let start = self.current.span.start;
        
        let types = if self.eat(TokenKind::Case) {
            self.parse_type_list()?
        } else if self.eat(TokenKind::Default) {
            Vec::new()
        } else {
            self.error_expected("'case' or 'default'");
            return Err(());
        };
        
        self.expect(TokenKind::Colon)?;
        
        let mut body = Vec::new();
        while !self.at(TokenKind::Case) && !self.at(TokenKind::Default) && !self.at(TokenKind::RBrace) && !self.at_eof() {
            body.push(self.parse_stmt()?);
        }
        
        Ok(TypeCaseClause {
            types,
            body,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_type_list(&mut self) -> ParseResult<Vec<Option<TypeExpr>>> {
        let mut types = Vec::new();
        loop {
            if self.at(TokenKind::Ident) && self.span_text(self.current.span) == "nil" {
                self.advance();
                types.push(None);
            } else {
                types.push(Some(self.parse_type()?));
            }
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        Ok(types)
    }

    fn parse_select_stmt(&mut self) -> ParseResult<StmtKind> {
        self.expect(TokenKind::Select)?;
        self.expect(TokenKind::LBrace)?;
        
        let mut cases = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            cases.push(self.parse_select_case()?);
        }
        
        self.expect(TokenKind::RBrace)?;
        Ok(StmtKind::Select(SelectStmt { cases }))
    }

    fn parse_select_case(&mut self) -> ParseResult<SelectCase> {
        let start = self.current.span.start;
        
        let comm = if self.eat(TokenKind::Case) {
            Some(self.parse_comm_clause()?)
        } else if self.eat(TokenKind::Default) {
            None
        } else {
            self.error_expected("'case' or 'default'");
            return Err(());
        };
        
        self.expect(TokenKind::Colon)?;
        
        let mut body = Vec::new();
        while !self.at(TokenKind::Case) && !self.at(TokenKind::Default) && !self.at(TokenKind::RBrace) && !self.at_eof() {
            body.push(self.parse_stmt()?);
        }
        
        Ok(SelectCase {
            comm,
            body,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn parse_comm_clause(&mut self) -> ParseResult<CommClause> {
        // Could be: send (ch <- v) or recv (v = <-ch, v := <-ch, <-ch)
        
        // Check for receive expression starting with <-
        if self.at(TokenKind::Arrow) {
            self.advance();
            let expr = self.parse_expr()?;
            return Ok(CommClause::Recv(RecvStmt {
                lhs: Vec::new(),
                define: false,
                expr,
            }));
        }
        
        // Parse left side
        let left = self.parse_expr_list()?;
        
        match self.current.kind {
            TokenKind::Arrow => {
                // Send: ch <- value
                self.advance();
                if left.len() != 1 {
                    self.error("send requires single channel expression");
                    return Err(());
                }
                let value = self.parse_expr()?;
                Ok(CommClause::Send(SendStmt {
                    chan: left.into_iter().next().unwrap(),
                    value,
                }))
            }
            TokenKind::ColonEq => {
                // Receive with short var decl: v := <-ch
                self.advance();
                self.expect(TokenKind::Arrow)?;
                let expr = self.parse_expr()?;
                let lhs = self.exprs_to_idents(left)?;
                Ok(CommClause::Recv(RecvStmt { lhs, define: true, expr }))
            }
            TokenKind::Eq => {
                // Receive with assignment: v = <-ch
                self.advance();
                self.expect(TokenKind::Arrow)?;
                let expr = self.parse_expr()?;
                let lhs = self.exprs_to_idents(left)?;
                Ok(CommClause::Recv(RecvStmt { lhs, define: false, expr }))
            }
            _ => {
                self.error("expected send or receive in select case");
                Err(())
            }
        }
    }

    fn parse_simple_stmt_or_expr(&mut self) -> ParseResult<Stmt> {
        let start = self.current.span.start;
        
        // Check for labeled statement
        if self.at(TokenKind::Ident) && self.peek_is(TokenKind::Colon) {
            let label = self.parse_ident()?;
            self.expect(TokenKind::Colon)?;
            let stmt = self.parse_stmt()?;
            return Ok(Stmt {
                kind: StmtKind::Labeled(LabeledStmt {
                    label,
                    stmt: Box::new(stmt),
                }),
                span: Span::new(start, self.current.span.start),
            });
        }
        
        let left = self.parse_expr_list()?;
        
        let kind = match self.current.kind {
            TokenKind::ColonEq => {
                self.advance();
                let names = self.exprs_to_idents(left)?;
                // Check for range keyword - will be handled by for loop parser
                if self.at(TokenKind::Range) {
                    return Ok(Stmt {
                        kind: StmtKind::ShortVar(ShortVarDecl { names, values: vec![] }),
                        span: Span::new(start, self.current.span.start),
                    });
                }
                let values = self.parse_expr_list()?;
                StmtKind::ShortVar(ShortVarDecl { names, values })
            }
            TokenKind::Eq | TokenKind::PlusEq | TokenKind::MinusEq |
            TokenKind::StarEq | TokenKind::SlashEq | TokenKind::PercentEq |
            TokenKind::AmpEq | TokenKind::PipeEq | TokenKind::CaretEq |
            TokenKind::AmpCaretEq | TokenKind::ShlEq | TokenKind::ShrEq => {
                let op = self.token_to_assign_op(&self.current.kind);
                self.advance();
                let rhs = self.parse_expr_list()?;
                StmtKind::Assign(AssignStmt { lhs: left, op, rhs })
            }
            TokenKind::PlusPlus => {
                self.advance();
                if left.len() != 1 {
                    self.error("increment requires single expression");
                    return Err(());
                }
                StmtKind::IncDec(IncDecStmt { expr: left.into_iter().next().unwrap(), is_inc: true })
            }
            TokenKind::MinusMinus => {
                self.advance();
                if left.len() != 1 {
                    self.error("decrement requires single expression");
                    return Err(());
                }
                StmtKind::IncDec(IncDecStmt { expr: left.into_iter().next().unwrap(), is_inc: false })
            }
            TokenKind::Arrow => {
                self.advance();
                if left.len() != 1 {
                    self.error("send requires single channel expression");
                    return Err(());
                }
                let value = self.parse_expr()?;
                StmtKind::Send(SendStmt { chan: left.into_iter().next().unwrap(), value })
            }
            _ => {
                if left.len() != 1 {
                    self.error("expression statement requires single expression");
                    return Err(());
                }
                StmtKind::Expr(left.into_iter().next().unwrap())
            }
        };
        
        Ok(Stmt {
            kind,
            span: Span::new(start, self.current.span.start),
        })
    }

    fn exprs_to_idents(&mut self, exprs: Vec<Expr>) -> ParseResult<Vec<Ident>> {
        let mut idents = Vec::new();
        for expr in exprs {
            match expr.kind {
                ExprKind::Ident(ident) => idents.push(ident),
                _ => {
                    self.error_at(expr.span, "expected identifier");
                    return Err(());
                }
            }
        }
        Ok(idents)
    }

    fn exprs_to_key_value(&self, exprs: &[Expr]) -> (Option<Expr>, Option<Expr>) {
        match exprs.len() {
            0 => (None, None),
            1 => (Some(exprs[0].clone()), None),
            _ => (Some(exprs[0].clone()), Some(exprs[1].clone())),
        }
    }

    fn token_to_assign_op(&self, kind: &TokenKind) -> AssignOp {
        match kind {
            TokenKind::Eq => AssignOp::Assign,
            TokenKind::PlusEq => AssignOp::Add,
            TokenKind::MinusEq => AssignOp::Sub,
            TokenKind::StarEq => AssignOp::Mul,
            TokenKind::SlashEq => AssignOp::Div,
            TokenKind::PercentEq => AssignOp::Rem,
            TokenKind::AmpEq => AssignOp::And,
            TokenKind::PipeEq => AssignOp::Or,
            TokenKind::CaretEq => AssignOp::Xor,
            TokenKind::AmpCaretEq => AssignOp::AndNot,
            TokenKind::ShlEq => AssignOp::Shl,
            TokenKind::ShrEq => AssignOp::Shr,
            _ => unreachable!("not an assignment operator: {:?}", kind),
        }
    }
}
