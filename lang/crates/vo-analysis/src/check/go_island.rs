//! Post-pass: sendability checks for `go @(island)` statements.
//!
//! This runs after escape analysis so that `closure_captures` is populated.
//! For each `go @(island) func(...){...}(args...)`, it checks:
//! - Every captured variable's type is sendable
//! - Every argument's type is sendable

use vo_syntax::ast::{ExprKind, File, StmtKind, Stmt, Visitor};

use crate::check::sendable::check_sendable;
use crate::check::type_info::TypeInfo;
use crate::objects::TCObjects;

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
    fn check_go_island_call(&mut self, call_expr: &vo_syntax::ast::Expr) {
        let ExprKind::Call(call) = &call_expr.kind else {
            return; // Not a call — stmt.rs already reports this
        };

        // Check argument sendability (types recorded during type checking)
        for arg in &call.args {
            if let Some(tv) = self.result.types.get(&arg.id) {
                let sendability = check_sendable(tv.typ, self.tc_objs);
                if let crate::check::sendable::Sendability::NotSendable(reason) = sendability {
                    self.diagnostics.push(GoIslandDiag {
                        code: TypeError::GoIslandNotSendable,
                        span: arg.span,
                        message: format!("cannot send argument across island boundary: {}", reason),
                    });
                }
            }
        }

        // Check closure capture sendability
        let ExprKind::FuncLit(_) = &call.func.kind else {
            return; // Not a func literal — captures only exist for func literals
        };

        let func_lit_id = call.func.id;
        let Some(captures) = self.result.closure_captures.get(&func_lit_id) else {
            return; // No captures
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
                    span: lobj.span(),
                    message: format!(
                        "cannot capture '{}' across island boundary: {}",
                        lobj.name(),
                        reason,
                    ),
                });
            }
        }
    }
}
