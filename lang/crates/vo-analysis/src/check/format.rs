//! AST formatting for trace output.

use vo_common::symbol::SymbolInterner;
use vo_syntax::ast::{Expr, ExprKind, Stmt, StmtKind, BinaryOp, UnaryOp};
use std::fmt::Write;

/// Format an expression for trace output.
pub fn format_expr(expr: &Expr, interner: &SymbolInterner) -> String {
    let mut buf = String::new();
    write_expr(&mut buf, expr, interner);
    buf
}

/// Format a statement for trace output.
pub fn format_stmt(stmt: &Stmt, interner: &SymbolInterner) -> String {
    let mut buf = String::new();
    write_stmt(&mut buf, stmt, interner);
    buf
}

fn write_expr(buf: &mut String, expr: &Expr, interner: &SymbolInterner) {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(name) = interner.resolve(ident.symbol) {
                buf.push_str(name);
            } else {
                buf.push_str("<ident>");
            }
        }
        ExprKind::IntLit(lit) => {
            if let Some(s) = interner.resolve(lit.raw) {
                buf.push_str(s);
            }
        }
        ExprKind::FloatLit(lit) => {
            if let Some(s) = interner.resolve(lit.raw) {
                buf.push_str(s);
            }
        }
        ExprKind::RuneLit(lit) => {
            if let Some(s) = interner.resolve(lit.raw) {
                buf.push_str(s);
            }
        }
        ExprKind::StringLit(lit) => {
            // Truncate long strings
            let s = &lit.value;
            if s.len() > 20 {
                let _ = write!(buf, "\"{}...\"", &s[..17]);
            } else {
                let _ = write!(buf, "\"{}\"", s);
            }
        }
        ExprKind::Binary(bin) => {
            write_expr(buf, &bin.left, interner);
            let _ = write!(buf, " {} ", format_binop(&bin.op));
            write_expr(buf, &bin.right, interner);
        }
        ExprKind::Unary(un) => {
            buf.push_str(format_unop(&un.op));
            write_expr(buf, &un.operand, interner);
        }
        ExprKind::Call(call) => {
            write_expr(buf, &call.func, interner);
            buf.push('(');
            for (i, arg) in call.args.iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }
                write_expr(buf, arg, interner);
            }
            if call.spread {
                buf.push_str("...");
            }
            buf.push(')');
        }
        ExprKind::Index(idx) => {
            write_expr(buf, &idx.expr, interner);
            buf.push('[');
            write_expr(buf, &idx.index, interner);
            buf.push(']');
        }
        ExprKind::Slice(sl) => {
            write_expr(buf, &sl.expr, interner);
            buf.push('[');
            if let Some(low) = &sl.low {
                write_expr(buf, low, interner);
            }
            buf.push(':');
            if let Some(high) = &sl.high {
                write_expr(buf, high, interner);
            }
            if let Some(max) = &sl.max {
                buf.push(':');
                write_expr(buf, max, interner);
            }
            buf.push(']');
        }
        ExprKind::Selector(sel) => {
            write_expr(buf, &sel.expr, interner);
            buf.push('.');
            if let Some(name) = interner.resolve(sel.sel.symbol) {
                buf.push_str(name);
            }
        }
        ExprKind::TypeAssert(ta) => {
            write_expr(buf, &ta.expr, interner);
            buf.push_str(".(type)");
        }
        ExprKind::CompositeLit(_) => {
            buf.push_str("(composite lit)");
        }
        ExprKind::FuncLit(_) => {
            buf.push_str("(func literal)");
        }
        ExprKind::Conversion(conv) => {
            buf.push_str("(conversion)");
            buf.push('(');
            write_expr(buf, &conv.expr, interner);
            buf.push(')');
        }
        ExprKind::Receive(expr) => {
            buf.push_str("<-");
            write_expr(buf, expr, interner);
        }
        ExprKind::Paren(expr) => {
            buf.push('(');
            write_expr(buf, expr, interner);
            buf.push(')');
        }
        ExprKind::TypeAsExpr(_) => {
            buf.push_str("(type)");
        }
        ExprKind::TryUnwrap(expr) => {
            write_expr(buf, expr, interner);
            buf.push('?');
        }
        ExprKind::DynAccess(d) => {
            write_expr(buf, &d.base, interner);
            buf.push_str("~>");
            match &d.op {
                vo_syntax::ast::DynAccessOp::Field(ident) => {
                    if let Some(name) = interner.resolve(ident.symbol) {
                        buf.push_str(name);
                    }
                }
                vo_syntax::ast::DynAccessOp::Index(idx) => {
                    buf.push('[');
                    write_expr(buf, idx, interner);
                    buf.push(']');
                }
                vo_syntax::ast::DynAccessOp::Call { args, .. } => {
                    write_args(buf, args, interner);
                }
                vo_syntax::ast::DynAccessOp::MethodCall { method, args, .. } => {
                    if let Some(name) = interner.resolve(method.symbol) {
                        buf.push_str(name);
                    }
                    write_args(buf, args, interner);
                }
            }
        }
        ExprKind::Ellipsis => {
            buf.push_str("...");
        }
    }
}

fn write_stmt(buf: &mut String, stmt: &Stmt, interner: &SymbolInterner) {
    match &stmt.kind {
        StmtKind::Empty => buf.push_str("(empty)"),
        StmtKind::Block(_) => buf.push_str("{ ... }"),
        StmtKind::Labeled(l) => {
            if let Some(name) = interner.resolve(l.label.symbol) {
                let _ = write!(buf, "{}: ...", name);
            }
        }
        StmtKind::Expr(e) => write_expr(buf, e, interner),
        StmtKind::Send(s) => {
            write_expr(buf, &s.chan, interner);
            buf.push_str(" <- ");
            write_expr(buf, &s.value, interner);
        }
        StmtKind::IncDec(id) => {
            write_expr(buf, &id.expr, interner);
            buf.push_str(if id.is_inc { "++" } else { "--" });
        }
        StmtKind::Assign(a) => {
            for (i, lhs) in a.lhs.iter().enumerate() {
                if i > 0 { buf.push_str(", "); }
                write_expr(buf, lhs, interner);
            }
            buf.push_str(" = ");
            for (i, rhs) in a.rhs.iter().enumerate() {
                if i > 0 { buf.push_str(", "); }
                write_expr(buf, rhs, interner);
            }
        }
        StmtKind::ShortVar(sd) => {
            for (i, name) in sd.names.iter().enumerate() {
                if i > 0 { buf.push_str(", "); }
                if let Some(n) = interner.resolve(name.symbol) {
                    buf.push_str(n);
                }
            }
            buf.push_str(" := ");
            for (i, rhs) in sd.values.iter().enumerate() {
                if i > 0 { buf.push_str(", "); }
                write_expr(buf, rhs, interner);
            }
        }
        StmtKind::Go(g) => {
            buf.push_str("go");
            if let Some(island) = &g.target_island {
                buf.push_str(" @(");
                write_expr(buf, island, interner);
                buf.push_str(") ");
            } else {
                buf.push(' ');
            }
            write_expr(buf, &g.call, interner);
        }
        StmtKind::Defer(d) => {
            buf.push_str("defer ");
            write_expr(buf, &d.call, interner);
        }
        StmtKind::Return(r) => {
            buf.push_str("return");
            if !r.values.is_empty() {
                buf.push(' ');
                for (i, e) in r.values.iter().enumerate() {
                    if i > 0 { buf.push_str(", "); }
                    write_expr(buf, e, interner);
                }
            }
        }
        StmtKind::Break(_) => buf.push_str("break"),
        StmtKind::Continue(_) => buf.push_str("continue"),
        StmtKind::Goto(_) => buf.push_str("goto"),
        StmtKind::Fallthrough => buf.push_str("fallthrough"),
        StmtKind::If(_) => buf.push_str("if ..."),
        StmtKind::Switch(_) => buf.push_str("switch ..."),
        StmtKind::TypeSwitch(_) => buf.push_str("switch type ..."),
        StmtKind::Select(_) => buf.push_str("select ..."),
        StmtKind::For(_) => buf.push_str("for ..."),
        StmtKind::Var(_) => buf.push_str("var ..."),
        StmtKind::Const(_) => buf.push_str("const ..."),
        StmtKind::Type(_) => buf.push_str("type ..."),
        StmtKind::Fail(f) => {
            buf.push_str("fail ");
            write_expr(buf, &f.error, interner);
        }
        StmtKind::ErrDefer(_) => buf.push_str("errdefer ..."),
    }
}

fn format_binop(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::And => "&",
        BinaryOp::Or => "|",
        BinaryOp::Xor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::AndNot => "&^",
        BinaryOp::LogAnd => "&&",
        BinaryOp::LogOr => "||",
        BinaryOp::Eq => "==",
        BinaryOp::NotEq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::LtEq => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::GtEq => ">=",
    }
}

fn format_unop(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Pos => "+",
        UnaryOp::Neg => "-",
        UnaryOp::Not => "!",
        UnaryOp::BitNot => "^",
        UnaryOp::Addr => "&",
        UnaryOp::Deref => "*",
    }
}

fn write_args(buf: &mut String, args: &[Expr], interner: &SymbolInterner) {
    buf.push('(');
    for (i, arg) in args.iter().enumerate() {
        if i > 0 { buf.push_str(", ") }
        write_expr(buf, arg, interner);
    }
    buf.push(')');
}
