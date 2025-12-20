//! Statement type checking.
//!
//! This module type-checks statements, handling control flow,
//! scope management, and statement-specific semantics.

#![allow(dead_code)]

use gox_common::span::Span;
use gox_common::vfs::FileSystem;
use gox_syntax::ast::{Block, FuncSig, Stmt, StmtKind};

use crate::objects::{ScopeKey, TypeKey};

use super::checker::Checker;

// =============================================================================
// StmtContext - control flow context for statement checking
// =============================================================================

/// Context for statement checking, tracking control flow validity.
#[derive(Clone, Copy, Debug, Default)]
pub struct StmtContext {
    /// Whether `break` is allowed.
    pub break_ok: bool,
    /// Whether `continue` is allowed.
    pub continue_ok: bool,
    /// Whether `fallthrough` is allowed.
    pub fallthrough_ok: bool,
    /// Whether this is the final case in a switch.
    pub final_switch_case: bool,
}

impl StmtContext {
    pub fn new() -> StmtContext {
        StmtContext::default()
    }

    /// Create a context for loop body.
    pub fn for_loop() -> StmtContext {
        StmtContext {
            break_ok: true,
            continue_ok: true,
            ..Default::default()
        }
    }

    /// Create a context for switch case.
    pub fn for_switch(is_final: bool) -> StmtContext {
        StmtContext {
            break_ok: true,
            fallthrough_ok: !is_final,
            final_switch_case: is_final,
            ..Default::default()
        }
    }

    /// Create a context for select case.
    pub fn for_select() -> StmtContext {
        StmtContext {
            break_ok: true,
            ..Default::default()
        }
    }
}

// =============================================================================
// Checker statement methods
// =============================================================================

impl<F: FileSystem> Checker<F> {
    /// Type-checks a function body.
    pub fn check_func_body(
        &mut self,
        _name: &str,
        _sig: crate::objects::TypeKey,
        body: &Block,
        _fctx: &mut super::checker::FilesContext<F>,
    ) {
        self.check_block(body);
    }

    /// Type-checks a block of statements with a new scope.
    pub fn check_block(&mut self, block: &Block) {
        self.check_block_with_context(block, &StmtContext::new());
    }

    /// Type-checks a block with specific statement context.
    pub fn check_block_with_context(&mut self, block: &Block, sctx: &StmtContext) {
        for stmt in &block.stmts {
            self.check_stmt_with_context(stmt, sctx);
        }
    }

    /// Type-checks a statement with default context.
    pub fn check_stmt(&mut self, stmt: &Stmt) {
        self.check_stmt_with_context(stmt, &StmtContext::new());
    }

    /// Type-checks a statement with specific context.
    pub fn check_stmt_with_context(&mut self, stmt: &Stmt, sctx: &StmtContext) {
        match &stmt.kind {
            StmtKind::Empty => {}
            StmtKind::Block(block) => self.check_block(block),
            StmtKind::Var(var) => self.check_var_decl_stmt(var),
            StmtKind::Const(cons) => self.check_const_decl_stmt(cons),
            StmtKind::Type(ty) => self.check_type_decl_stmt(ty),
            StmtKind::ShortVar(sv) => self.check_short_var_decl(sv),
            StmtKind::Expr(expr) => {
                self.check_expr(expr);
            }
            StmtKind::Assign(assign) => self.check_assign_stmt(assign),
            StmtKind::IncDec(incdec) => self.check_incdec_stmt(incdec),
            StmtKind::Return(ret) => self.check_return_stmt(ret),
            StmtKind::If(if_stmt) => self.check_if_stmt(if_stmt),
            StmtKind::For(for_stmt) => self.check_for_stmt(for_stmt),
            StmtKind::Switch(sw) => self.check_switch_stmt(sw),
            StmtKind::TypeSwitch(ts) => self.check_type_switch_stmt(ts),
            StmtKind::Select(sel) => self.check_select_stmt(sel),
            StmtKind::Go(go) => self.check_go_stmt(go),
            StmtKind::Defer(defer) => self.check_defer_stmt(defer),
            StmtKind::ErrDefer(errdefer) => self.check_errdefer_stmt(errdefer),
            StmtKind::Fail(fail) => self.check_fail_stmt(fail),
            StmtKind::Send(send) => self.check_send_stmt(send),
            StmtKind::Break(_) | StmtKind::Continue(_) | StmtKind::Goto(_) => {
                // Label validation done elsewhere
            }
            StmtKind::Fallthrough => {}
            StmtKind::Labeled(labeled) => self.check_labeled_stmt(labeled),
        }
    }

    fn check_var_decl_stmt(&mut self, var: &gox_syntax::ast::VarDecl) {
        for spec in &var.specs {
            if let Some(ty) = &spec.ty {
                let _typ = self.invalid_type(); // TODO: use type_expr with fctx
            }
            for val in &spec.values {
                self.check_expr(val);
            }
        }
    }

    fn check_const_decl_stmt(&mut self, cons: &gox_syntax::ast::ConstDecl) {
        for spec in &cons.specs {
            if let Some(ty) = &spec.ty {
                let _typ = self.invalid_type(); // TODO: use type_expr with fctx
            }
            for val in &spec.values {
                self.check_expr(val);
            }
        }
    }

    fn check_type_decl_stmt(&mut self, ty: &gox_syntax::ast::TypeDecl) {
        let _typ = self.invalid_type(); // TODO: use type_expr with fctx
    }

    fn check_short_var_decl(&mut self, sv: &gox_syntax::ast::ShortVarDecl) {
        for val in &sv.values {
            self.check_expr(val);
        }
    }

    fn check_assign_stmt(&mut self, assign: &gox_syntax::ast::AssignStmt) {
        for lhs in &assign.lhs {
            self.check_expr(lhs);
        }
        for rhs in &assign.rhs {
            self.check_expr(rhs);
        }
    }

    fn check_incdec_stmt(&mut self, incdec: &gox_syntax::ast::IncDecStmt) {
        self.check_expr(&incdec.expr);
    }

    fn check_return_stmt(&mut self, ret: &gox_syntax::ast::ReturnStmt) {
        for val in &ret.values {
            self.check_expr(val);
        }
    }

    fn check_if_stmt(&mut self, if_stmt: &gox_syntax::ast::IfStmt) {
        if let Some(init) = &if_stmt.init {
            self.check_stmt(init);
        }
        self.check_expr(&if_stmt.cond);
        self.check_block(&if_stmt.then);
        if let Some(else_) = &if_stmt.else_ {
            self.check_stmt(else_);
        }
    }

    fn check_for_stmt(&mut self, for_stmt: &gox_syntax::ast::ForStmt) {
        match &for_stmt.clause {
            gox_syntax::ast::ForClause::Cond(cond) => {
                if let Some(c) = cond {
                    self.check_expr(c);
                }
            }
            gox_syntax::ast::ForClause::Three { init, cond, post } => {
                if let Some(i) = init {
                    self.check_stmt(i);
                }
                if let Some(c) = cond {
                    self.check_expr(c);
                }
                if let Some(p) = post {
                    self.check_stmt(p);
                }
            }
            gox_syntax::ast::ForClause::Range { expr, .. } => {
                self.check_expr(expr);
            }
        }
        self.check_block(&for_stmt.body);
    }

    fn check_switch_stmt(&mut self, sw: &gox_syntax::ast::SwitchStmt) {
        if let Some(init) = &sw.init {
            self.check_stmt(init);
        }
        if let Some(tag) = &sw.tag {
            self.check_expr(tag);
        }
        for case in &sw.cases {
            for expr in &case.exprs {
                self.check_expr(expr);
            }
            for stmt in &case.body {
                self.check_stmt(stmt);
            }
        }
    }

    fn check_type_switch_stmt(&mut self, ts: &gox_syntax::ast::TypeSwitchStmt) {
        if let Some(init) = &ts.init {
            self.check_stmt(init);
        }
        self.check_expr(&ts.expr);
        for case in &ts.cases {
            for ty in &case.types {
                if let Some(t) = ty {
                    let _typ = self.invalid_type(); // TODO: use type_expr with fctx
                }
            }
            for stmt in &case.body {
                self.check_stmt(stmt);
            }
        }
    }

    fn check_select_stmt(&mut self, sel: &gox_syntax::ast::SelectStmt) {
        for case in &sel.cases {
            if let Some(comm) = &case.comm {
                match comm {
                    gox_syntax::ast::CommClause::Send(send) => {
                        self.check_expr(&send.chan);
                        self.check_expr(&send.value);
                    }
                    gox_syntax::ast::CommClause::Recv(recv) => {
                        self.check_expr(&recv.expr);
                    }
                }
            }
            for stmt in &case.body {
                self.check_stmt(stmt);
            }
        }
    }

    fn check_go_stmt(&mut self, go: &gox_syntax::ast::GoStmt) {
        self.check_expr(&go.call);
    }

    fn check_defer_stmt(&mut self, defer: &gox_syntax::ast::DeferStmt) {
        self.check_expr(&defer.call);
    }

    fn check_errdefer_stmt(&mut self, errdefer: &gox_syntax::ast::ErrDeferStmt) {
        self.check_expr(&errdefer.call);
    }

    fn check_fail_stmt(&mut self, fail: &gox_syntax::ast::FailStmt) {
        self.check_expr(&fail.error);
    }

    fn check_send_stmt(&mut self, send: &gox_syntax::ast::SendStmt) {
        self.check_expr(&send.chan);
        self.check_expr(&send.value);
    }

    fn check_labeled_stmt(&mut self, labeled: &gox_syntax::ast::LabeledStmt) {
        self.check_stmt(&labeled.stmt);
    }
}
