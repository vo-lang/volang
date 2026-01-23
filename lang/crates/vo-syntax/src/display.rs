//! Display utilities for AST expressions.
//!
//! Provides formatting functions to convert AST expressions back to source-like strings.

use std::fmt::{self, Write};

use vo_common::symbol::SymbolInterner;

use crate::ast::{
    BinaryOp, CompositeLitKey, DynAccessOp, Expr, ExprKind, TypeExpr, TypeExprKind, UnaryOp,
};

/// Formats an expression to a string representation.
pub fn fmt_expr(expr: &Expr, f: &mut fmt::Formatter<'_>, interner: &SymbolInterner) -> fmt::Result {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            write!(f, "{}", interner.resolve(ident.symbol).unwrap_or("?"))
        }
        ExprKind::IntLit(lit) => {
            write!(f, "{}", interner.resolve(lit.raw).unwrap_or("?"))
        }
        ExprKind::FloatLit(lit) => {
            write!(f, "{}", interner.resolve(lit.raw).unwrap_or("?"))
        }
        ExprKind::RuneLit(lit) => {
            write!(f, "{}", interner.resolve(lit.raw).unwrap_or("?"))
        }
        ExprKind::StringLit(lit) => {
            write!(f, "{}", interner.resolve(lit.raw).unwrap_or("?"))
        }
        ExprKind::Binary(b) => {
            fmt_expr(&b.left, f, interner)?;
            write!(f, " {} ", b.op)?;
            fmt_expr(&b.right, f, interner)
        }
        ExprKind::Unary(u) => {
            write!(f, "{}", u.op)?;
            fmt_expr(&u.operand, f, interner)
        }
        ExprKind::Call(c) => {
            fmt_expr(&c.func, f, interner)?;
            f.write_char('(')?;
            for (i, arg) in c.args.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                fmt_expr(arg, f, interner)?;
            }
            if c.spread {
                f.write_str("...")?;
            }
            f.write_char(')')
        }
        ExprKind::Index(idx) => {
            fmt_expr(&idx.expr, f, interner)?;
            f.write_char('[')?;
            fmt_expr(&idx.index, f, interner)?;
            f.write_char(']')
        }
        ExprKind::Slice(s) => {
            fmt_expr(&s.expr, f, interner)?;
            f.write_char('[')?;
            if let Some(low) = &s.low {
                fmt_expr(low, f, interner)?;
            }
            f.write_char(':')?;
            if let Some(high) = &s.high {
                fmt_expr(high, f, interner)?;
            }
            f.write_char(']')
        }
        ExprKind::Selector(sel) => {
            fmt_expr(&sel.expr, f, interner)?;
            f.write_char('.')?;
            write!(f, "{}", interner.resolve(sel.sel.symbol).unwrap_or("?"))
        }
        ExprKind::TypeAssert(ta) => {
            fmt_expr(&ta.expr, f, interner)?;
            f.write_str(".(")?;
            match &ta.ty {
                Some(ty) => fmt_type_expr(ty, f, interner)?,
                None => f.write_str("type")?,
            }
            f.write_char(')')
        }
        ExprKind::CompositeLit(cl) => {
            if let Some(ty) = &cl.ty {
                fmt_type_expr(ty, f, interner)?;
            }
            f.write_char('{')?;
            for (i, elem) in cl.elems.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                if let Some(key) = &elem.key {
                    match key {
                        CompositeLitKey::Ident(ident) => {
                            write!(f, "{}", interner.resolve(ident.symbol).unwrap_or("?"))?;
                        }
                        CompositeLitKey::Expr(e) => {
                            fmt_expr(e, f, interner)?;
                        }
                    }
                    f.write_str(": ")?;
                }
                fmt_expr(&elem.value, f, interner)?;
            }
            f.write_char('}')
        }
        ExprKind::FuncLit(_) => {
            f.write_str("(func literal)")
        }
        ExprKind::Conversion(conv) => {
            fmt_type_expr(&conv.ty, f, interner)?;
            f.write_char('(')?;
            fmt_expr(&conv.expr, f, interner)?;
            f.write_char(')')
        }
        ExprKind::Receive(expr) => {
            f.write_str("<-")?;
            fmt_expr(expr, f, interner)
        }
        ExprKind::Paren(expr) => {
            f.write_char('(')?;
            fmt_expr(expr, f, interner)?;
            f.write_char(')')
        }
        ExprKind::TypeAsExpr(ty) => {
            fmt_type_expr(ty, f, interner)
        }
        ExprKind::TryUnwrap(expr) => {
            fmt_expr(expr, f, interner)?;
            f.write_char('?')
        }
        ExprKind::DynAccess(d) => {
            fmt_expr(&d.base, f, interner)?;
            f.write_str("~>")?;
            match &d.op {
                DynAccessOp::Field(ident) => {
                    write!(f, "{}", interner.resolve(ident.symbol).unwrap_or("?"))
                }
                DynAccessOp::Index(index) => {
                    f.write_char('[')?;
                    fmt_expr(index, f, interner)?;
                    f.write_char(']')
                }
                DynAccessOp::Call { args, spread } => {
                    fmt_call_args(args, *spread, f, interner)
                }
                DynAccessOp::MethodCall { method, args, spread } => {
                    write!(f, "{}", interner.resolve(method.symbol).unwrap_or("?"))?;
                    fmt_call_args(args, *spread, f, interner)
                }
            }
        }
        ExprKind::Ellipsis => {
            f.write_str("...")
        }
    }
}

/// Formats a type expression to a string representation.
pub fn fmt_type_expr(ty: &TypeExpr, f: &mut fmt::Formatter<'_>, interner: &SymbolInterner) -> fmt::Result {
    match &ty.kind {
        TypeExprKind::Ident(ident) => {
            write!(f, "{}", interner.resolve(ident.symbol).unwrap_or("?"))
        }
        TypeExprKind::Selector(sel) => {
            write!(f, "{}.{}", 
                interner.resolve(sel.pkg.symbol).unwrap_or("?"), 
                interner.resolve(sel.sel.symbol).unwrap_or("?"))
        }
        TypeExprKind::Pointer(inner) => {
            f.write_char('*')?;
            fmt_type_expr(inner, f, interner)
        }
        TypeExprKind::Array(arr) => {
            f.write_char('[')?;
            fmt_expr(&arr.len, f, interner)?;
            f.write_char(']')?;
            fmt_type_expr(&arr.elem, f, interner)
        }
        TypeExprKind::Slice(elem) => {
            f.write_str("[]")?;
            fmt_type_expr(elem, f, interner)
        }
        TypeExprKind::Map(m) => {
            f.write_str("map[")?;
            fmt_type_expr(&m.key, f, interner)?;
            f.write_char(']')?;
            fmt_type_expr(&m.value, f, interner)
        }
        TypeExprKind::Chan(c) => {
            use crate::ast::ChanDir;
            match c.dir {
                ChanDir::Send => f.write_str("chan<- ")?,
                ChanDir::Recv => f.write_str("<-chan ")?,
                ChanDir::Both => f.write_str("chan ")?,
            }
            fmt_type_expr(&c.elem, f, interner)
        }
        TypeExprKind::Func(_) => {
            f.write_str("func(...)")
        }
        TypeExprKind::Struct(_) => {
            f.write_str("struct{...}")
        }
        TypeExprKind::Interface(_) => {
            f.write_str("interface{...}")
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::And => "&",
            BinaryOp::Or => "|",
            BinaryOp::Xor => "^",
            BinaryOp::AndNot => "&^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::LtEq => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::GtEq => ">=",
            BinaryOp::LogAnd => "&&",
            BinaryOp::LogOr => "||",
        };
        f.write_str(s)
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOp::Pos => "+",
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "^",
            UnaryOp::Addr => "&",
            UnaryOp::Deref => "*",
        };
        f.write_str(s)
    }
}

fn fmt_call_args(args: &[Expr], spread: bool, f: &mut fmt::Formatter<'_>, interner: &SymbolInterner) -> fmt::Result {
    f.write_char('(')?;
    for (i, arg) in args.iter().enumerate() {
        if i > 0 { f.write_str(", ")? }
        fmt_expr(arg, f, interner)?;
    }
    if spread { f.write_str("...")? }
    f.write_char(')')
}
