//! Proofs for hoisting a loop's slice/string length. Only the descriptor's
//! local storage identity matters: writes to a slice's elements keep its len.

use vo_analysis::objects::ObjKey;
use vo_analysis::Builtin;
use vo_syntax::ast::{self, Block, CommClause, Expr, ExprKind, Ident, Stmt, StmtKind, Visitor};

use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

fn identifier(mut expression: &Expr) -> Option<&Ident> {
    while let ExprKind::Paren(inner) = &expression.kind {
        expression = inner;
    }
    if let ExprKind::Ident(ident) = &expression.kind {
        Some(ident)
    } else {
        None
    }
}

pub(super) fn stable_length(
    expression: &Expr,
    body: &Block,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> bool {
    let ExprKind::Call(call) = &expression.kind else {
        return false;
    };
    if info.expr_builtin(call.func.id) != Some(Builtin::Len) || call.args.len() != 1 {
        return false;
    }
    let argument = &call.args[0];
    let Some(ident) = identifier(argument) else {
        return false;
    };
    let object = info.get_use(ident);
    let ty = info.expr_type(argument.id);
    if !(info.is_slice(ty) || info.is_string(ty))
        || info.is_escaped(object)
        || info.is_captured_by_closure(object)
        || !matches!(
            func.lookup_local_object(object),
            Some(StorageKind::Reference { .. } | StorageKind::StackValue { slots: 1, .. })
        )
    {
        return false;
    }
    let mut writes = Writes {
        info,
        object,
        found: false,
        depth: 0,
    };
    for statement in &body.stmts {
        writes.visit_stmt(statement);
    }
    !writes.found
}

struct Writes<'a, 'project> {
    info: &'a TypeInfoWrapper<'project>,
    object: ObjKey,
    found: bool,
    depth: usize,
}

impl Writes<'_, '_> {
    fn assigned(&mut self, ident: &Ident) {
        if !self.info.is_def(ident) && self.info.project.interner.resolve(ident.symbol) != Some("_")
        {
            self.found |= self.info.get_use(ident) == self.object;
        }
    }

    fn destination(&mut self, expression: &Expr) {
        if let Some(ident) = identifier(expression) {
            self.assigned(ident);
        }
    }
}

impl Visitor for Writes<'_, '_> {
    // Calls cannot replace an uncaptured, unescaped local descriptor. Skipping
    // RHS expression trees also avoids recursive walks of long arithmetic.
    fn visit_expr(&mut self, _: &Expr) {}

    fn visit_stmt(&mut self, statement: &Stmt) {
        if self.found {
            return;
        }
        if self.depth == 128 {
            self.found = true; // Budget exhaustion keeps the ordinary loop.
            return;
        }
        match &statement.kind {
            StmtKind::Assign(assignment) => {
                for lhs in &assignment.lhs {
                    self.destination(lhs);
                }
            }
            StmtKind::ShortVar(declaration) => {
                for name in &declaration.names {
                    self.assigned(name);
                }
            }
            StmtKind::IncDec(update) => self.destination(&update.expr),
            StmtKind::For(loop_) => {
                if let ast::ForClause::Range {
                    key,
                    value,
                    define: false,
                    ..
                } = &loop_.clause
                {
                    for destination in key.iter().chain(value.iter()) {
                        self.destination(destination);
                    }
                }
            }
            StmtKind::Select(select) => {
                for case in &select.cases {
                    if let Some(CommClause::Recv(recv)) = &case.comm {
                        for name in &recv.lhs {
                            self.assigned(name);
                        }
                    }
                }
            }
            _ => {}
        }
        self.depth += 1;
        ast::walk_stmt(self, statement);
        self.depth -= 1;
    }
}
