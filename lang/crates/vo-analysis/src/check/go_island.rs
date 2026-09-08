//! Post-pass: sendability checks for `go @(island)` statements.
//!
//! This runs after escape analysis so that `closure_captures` is populated.
//! For every cross-island call, it checks the complete value transferred to the
//! target island:
//! - Every explicit argument
//! - Every function-literal capture
//! - The implicit receiver captured by a method value
//! - The callee itself, rejecting function values whose captures cannot be
//!   recovered statically

use vo_syntax::ast::{Expr, ExprKind, File, Ident, Stmt, StmtKind, Visitor};

use crate::check::sendable::check_sendable;
use crate::check::type_info::TypeInfo;
use crate::objects::TCObjects;
use crate::selection::SelectionKind;

use super::errors::TypeError;

/// Collected sendability violations from go @(island) statements.
pub struct GoIslandDiag {
    pub code: TypeError,
    pub span: vo_common::Span,
    pub message: String,
}

/// Run the go @(island) sendability post-pass over all files.
pub fn check_go_island_sendability(
    files: &[File],
    result: &TypeInfo,
    tc_objs: &TCObjects,
) -> Vec<GoIslandDiag> {
    let mut collector = GoIslandCollector {
        result,
        tc_objs,
        diagnostics: Vec::new(),
    };
    for file in files {
        collector.visit_file(file);
    }
    collector.diagnostics
}

struct GoIslandCollector<'a> {
    result: &'a TypeInfo,
    tc_objs: &'a TCObjects,
    diagnostics: Vec<GoIslandDiag>,
}

impl<'a> Visitor for GoIslandCollector<'a> {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if let StmtKind::Go(gs) = &stmt.kind {
            if gs.target_island.is_some() {
                self.check_go_island_call(&gs.call);
            }
        }
        // Continue walking into nested statements
        vo_syntax::ast::walk_stmt(self, stmt);
    }
}

impl<'a> GoIslandCollector<'a> {
    fn check_go_island_call(&mut self, call_expr: &Expr) {
        let call_expr = Self::strip_parens(call_expr);
        let ExprKind::Call(call) = &call_expr.kind else {
            return; // Not a call — stmt.rs already reports this
        };

        // Explicit arguments are serialized using their post-assignment parameter
        // shape. This matters for capability attenuation: a local `port T` may be
        // passed to a `port<- T` parameter without transferring its receive side.
        self.check_call_arguments(call_expr, call);

        self.check_callee(&call.func);
    }

    fn check_call_arguments(&mut self, expression: &Expr, call: &vo_syntax::ast::CallExpr) {
        let Some(checked) = self.result.call(expression) else {
            return;
        };
        for argument in &checked.arguments {
            self.check_type_sendability(
                argument.parameter_type,
                &call.args[argument.source_index],
                "argument",
            );
        }
    }

    fn check_callee(&mut self, callee: &Expr) {
        let callee = Self::strip_parens(callee);

        // Invalid callees are diagnosed by ordinary call checking. Avoid a second,
        // misleading cross-island diagnostic when no callable type was recorded.
        if !self.result.types.contains_key(&callee.id) {
            return;
        }

        match &callee.kind {
            ExprKind::FuncLit(_) => self.check_func_lit_captures(callee),
            ExprKind::Ident(ident) if self.is_declared_function(ident) => {
                // A declared function is represented as a zero-capture closure.
            }
            ExprKind::Selector(selector) => {
                if let Some(selection) = self.result.selections.get(&callee.id) {
                    match selection.kind() {
                        SelectionKind::MethodVal => {
                            // Evaluating x.M creates a closure that captures x.
                            self.check_method_receiver(&selector.expr);
                        }
                        SelectionKind::MethodExpr => {
                            // T.M has no hidden receiver. Its receiver is the first
                            // explicit argument and was checked above.
                        }
                        SelectionKind::FieldVal => self.reject_unknown_callee(callee),
                    }
                } else if self.is_declared_function(&selector.sel) {
                    // Package-qualified declared function.
                } else {
                    self.reject_unknown_callee(callee);
                }
            }
            _ => self.reject_unknown_callee(callee),
        }
    }

    fn check_func_lit_captures(&mut self, func_lit: &Expr) {
        let Some(captures) = self.result.closure_captures.get(&func_lit.id) else {
            return;
        };

        for &obj_key in captures {
            let lobj = &self.tc_objs.lobjs[obj_key];
            let Some(typ) = lobj.typ() else {
                continue;
            };
            let sendability = check_sendable(typ, self.tc_objs);
            if let crate::check::sendable::Sendability::NotSendable(reason) = sendability {
                self.diagnostics.push(GoIslandDiag {
                    code: TypeError::GoIslandNotSendable,
                    // A captured object can make several cross-island closures
                    // invalid. Anchor each diagnostic at the closure transfer so
                    // distinct violations do not collapse onto the declaration.
                    span: func_lit.span,
                    message: format!(
                        "cannot capture '{}' across island boundary: {}",
                        lobj.name(),
                        reason,
                    ),
                });
            }
        }
    }

    fn check_type_sendability(&mut self, typ: crate::objects::TypeKey, expr: &Expr, role: &str) {
        if let crate::check::sendable::Sendability::NotSendable(reason) =
            check_sendable(typ, self.tc_objs)
        {
            self.diagnostics.push(GoIslandDiag {
                code: TypeError::GoIslandNotSendable,
                span: expr.span,
                message: format!("cannot send {role} across island boundary: {reason}"),
            });
        }
    }

    fn check_method_receiver(&mut self, receiver: &Expr) {
        let Some(tv) = self.result.types.get(&receiver.id) else {
            return;
        };
        match check_sendable(tv.typ, self.tc_objs) {
            crate::check::sendable::Sendability::Static => {}
            crate::check::sendable::Sendability::NotSendable(reason) => {
                self.diagnostics.push(GoIslandDiag {
                    code: TypeError::GoIslandNotSendable,
                    span: receiver.span,
                    message: format!(
                        "cannot send method receiver across island boundary: {reason}"
                    ),
                });
            }
        }
    }

    fn is_declared_function(&self, ident: &Ident) -> bool {
        self.result
            .uses
            .get(&ident.id)
            .is_some_and(|&obj_key| self.tc_objs.lobjs[obj_key].entity_type().is_func())
    }

    fn reject_unknown_callee(&mut self, callee: &Expr) {
        self.diagnostics.push(GoIslandDiag {
            code: TypeError::GoIslandNotSendable,
            span: callee.span,
            message: "cannot send callee across island boundary: function value captures cannot be proven sendable"
                .to_string(),
        });
    }

    fn strip_parens(mut expr: &Expr) -> &Expr {
        while let ExprKind::Paren(inner) = &expr.kind {
            expr = inner;
        }
        expr
    }
}
