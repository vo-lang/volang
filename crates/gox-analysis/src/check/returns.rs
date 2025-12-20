//! Return statement checking.
//!
//! This module provides `is_terminating` logic to determine if a statement
//! always terminates (e.g., returns, panics, infinite loops, etc.).

#![allow(dead_code)]
use gox_syntax::ast::{CaseClause, ExprKind, SelectCase, Stmt, StmtKind, TypeCaseClause};

use super::checker::Checker;

impl Checker {
    /// Returns true if the statement is a terminating statement.
    /// A terminating statement prevents execution from reaching the end of a function.
    pub fn is_terminating(&self, stmt: &Stmt) -> bool {
        self.is_terminating_impl(stmt, None)
    }

    /// Implementation of is_terminating with optional label for break checking.
    fn is_terminating_impl(&self, stmt: &Stmt, label: Option<&str>) -> bool {
        match &stmt.kind {
            // Return always terminates
            StmtKind::Return(_) => true,

            // Goto and fallthrough always terminate their block
            StmtKind::Goto(_) | StmtKind::Fallthrough => true,

            // Block terminates if its last non-empty statement terminates
            StmtKind::Block(block) => self.is_terminating_list(&block.stmts, label),

            // Labeled statement - use the label for break checking
            StmtKind::Labeled(ls) => {
                let l = self.resolve_ident(&ls.label);
                self.is_terminating_impl(&ls.stmt, Some(l))
            }

            // If terminates if both branches terminate
            StmtKind::If(if_stmt) => {
                if_stmt.else_.is_some()
                    && self.is_terminating_list(&if_stmt.then.stmts, label)
                    && self.is_terminating_impl(if_stmt.else_.as_ref().unwrap(), label)
            }

            // Switch terminates if all cases terminate and there's a default
            StmtKind::Switch(sw) => self.is_terminating_switch(&sw.cases, label),

            // TypeSwitch terminates if all cases terminate and there's a default
            StmtKind::TypeSwitch(ts) => self.is_terminating_type_switch(&ts.cases, label),

            // Select terminates if all cases terminate
            StmtKind::Select(sel) => self.is_terminating_select(&sel.cases, label),

            // Infinite for loop (no condition) terminates if no break
            StmtKind::For(for_stmt) => {
                match &for_stmt.clause {
                    gox_syntax::ast::ForClause::Cond(None) => {
                        // Infinite loop - terminates unless there's a break
                        !self.has_break_list(&for_stmt.body.stmts, label, true)
                    }
                    _ => false,
                }
            }

            // Expression statement terminates if it's a panic call
            StmtKind::Expr(expr) => {
                if let ExprKind::Call(_call) = &expr.kind {
                    if let Some(ref panics) = self.octx.panics {
                        panics.contains(&expr.id)
                    } else {
                        false
                    }
                } else {
                    false
                }
            }

            // Fail statement terminates (GoX extension)
            StmtKind::Fail(_) => true,

            // Other statements don't terminate
            StmtKind::Empty
            | StmtKind::Var(_)
            | StmtKind::Const(_)
            | StmtKind::Type(_)
            | StmtKind::ShortVar(_)
            | StmtKind::Assign(_)
            | StmtKind::IncDec(_)
            | StmtKind::Send(_)
            | StmtKind::Go(_)
            | StmtKind::Defer(_)
            | StmtKind::ErrDefer(_)
            | StmtKind::Break(_)
            | StmtKind::Continue(_) => false,
        }
    }

    /// Returns true if the last non-empty statement in the list terminates.
    pub fn is_terminating_list(&self, stmts: &[Stmt], label: Option<&str>) -> bool {
        // Find the last non-empty statement
        let last_non_empty = stmts.iter().rev().find(|s| !matches!(s.kind, StmtKind::Empty));

        if let Some(stmt) = last_non_empty {
            self.is_terminating_impl(stmt, label)
        } else {
            false
        }
    }

    /// Returns true if a switch statement terminates.
    fn is_terminating_switch(
        &self,
        cases: &[CaseClause],
        label: Option<&str>,
    ) -> bool {
        let mut has_default = false;

        for case in cases {
            if case.exprs.is_empty() {
                has_default = true;
            }
            // Each case must terminate and not have a break
            if !self.is_terminating_list(&case.body, None)
                || self.has_break_list(&case.body, label, true)
            {
                return false;
            }
        }

        has_default
    }

    /// Returns true if a type switch statement terminates.
    fn is_terminating_type_switch(
        &self,
        cases: &[TypeCaseClause],
        label: Option<&str>,
    ) -> bool {
        let mut has_default = false;

        for case in cases {
            if case.types.is_empty() || case.types.iter().all(|t| t.is_none()) {
                has_default = true;
            }
            if !self.is_terminating_list(&case.body, None)
                || self.has_break_list(&case.body, label, true)
            {
                return false;
            }
        }

        has_default
    }

    /// Returns true if a select statement terminates.
    fn is_terminating_select(
        &self,
        cases: &[SelectCase],
        label: Option<&str>,
    ) -> bool {
        for case in cases {
            if !self.is_terminating_list(&case.body, None)
                || self.has_break_list(&case.body, label, true)
            {
                return false;
            }
        }
        // Select terminates if no case has a break path out
        true
    }

    /// Returns true if the statement contains a break referring to the given label
    /// or (if implicit is true) the closest outer breakable statement.
    fn has_break(&self, stmt: &Stmt, label: Option<&str>, implicit: bool) -> bool {
        match &stmt.kind {
            StmtKind::Break(brk) => {
                // Break with no label breaks implicitly
                // Break with label breaks the labeled statement
                if let Some(brk_label) = &brk.label {
                    let brk_name = self.resolve_ident(brk_label);
                    label.map_or(false, |l| l == brk_name)
                } else {
                    implicit
                }
            }
            StmtKind::Block(block) => self.has_break_list(&block.stmts, label, implicit),
            StmtKind::If(if_stmt) => {
                self.has_break_list(&if_stmt.then.stmts, label, implicit)
                    || if_stmt
                        .else_
                        .as_ref()
                        .map_or(false, |e| self.has_break(e, label, implicit))
            }
            StmtKind::Labeled(ls) => self.has_break(&ls.stmt, label, implicit),
            StmtKind::Switch(sw) => {
                // Only check for labeled breaks inside switch
                label.is_some()
                    && sw
                        .cases
                        .iter()
                        .any(|c| self.has_break_list(&c.body, label, implicit))
            }
            StmtKind::TypeSwitch(ts) => {
                label.is_some()
                    && ts
                        .cases
                        .iter()
                        .any(|c| self.has_break_list(&c.body, label, implicit))
            }
            StmtKind::Select(sel) => {
                label.is_some()
                    && sel
                        .cases
                        .iter()
                        .any(|c| self.has_break_list(&c.body, label, implicit))
            }
            StmtKind::For(for_stmt) => {
                // Only check for labeled breaks inside for
                label.is_some() && self.has_break_list(&for_stmt.body.stmts, label, implicit)
            }
            // Other statements don't contain breaks
            _ => false,
        }
    }

    /// Returns true if any statement in the list contains a break.
    fn has_break_list(&self, stmts: &[Stmt], label: Option<&str>, implicit: bool) -> bool {
        stmts.iter().any(|s| self.has_break(s, label, implicit))
    }
}
