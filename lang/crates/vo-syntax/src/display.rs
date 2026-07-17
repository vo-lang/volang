//! Display utilities for AST expressions.
//!
//! Provides formatting functions to convert AST expressions back to source-like strings.

use std::fmt::{self, Write};

use vo_common::symbol::{Symbol, SymbolInterner};

use crate::ast::{
    AssignOp, BinaryExpr, BinaryOp, Block, CaseClause, ChanDir, CommClause, CompositeLit,
    CompositeLitElem, CompositeLitKey, ConstDecl, ConstSpec, Decl, DynAccessOp, Expr, ExprKind,
    File, ForClause, FuncDecl, FuncSig, FuncType, Ident, IfStmt, ImportDecl, InlineModMetadata,
    InterfaceElem, InterfaceType, Param, PortType, Receiver, ResultParam, SelectCase, SelectStmt,
    Stmt, StmtKind, StructType, SwitchStmt, TypeCaseClause, TypeDecl, TypeExpr, TypeExprKind,
    TypeSwitchStmt, UnaryOp, VarDecl, VarSpec,
};
use crate::inline_mod::{INLINE_MOD_CLOSE, INLINE_MOD_OPEN};

const MAX_LINE_WIDTH: usize = 100;

pub fn format_file(file: &File, interner: &SymbolInterner) -> String {
    let mut printer = SourcePrinter::new(interner);
    printer.write_file(file);
    printer.finish()
}

/// Formats an expression to a string representation.
pub fn fmt_expr(expr: &Expr, f: &mut fmt::Formatter<'_>, interner: &SymbolInterner) -> fmt::Result {
    let mut printer = SourcePrinter::new(interner);
    printer.write_expr(expr);
    f.write_str(&printer.finish())
}

/// Formats a type expression to a string representation.
pub fn fmt_type_expr(
    ty: &TypeExpr,
    f: &mut fmt::Formatter<'_>,
    interner: &SymbolInterner,
) -> fmt::Result {
    let mut printer = SourcePrinter::new(interner);
    printer.write_type_expr(ty);
    f.write_str(&printer.finish())
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

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            AssignOp::Assign => "=",
            AssignOp::Add => "+=",
            AssignOp::Sub => "-=",
            AssignOp::Mul => "*=",
            AssignOp::Div => "/=",
            AssignOp::Rem => "%=",
            AssignOp::Shl => "<<=",
            AssignOp::Shr => ">>=",
            AssignOp::And => "&=",
            AssignOp::Or => "|=",
            AssignOp::Xor => "^=",
            AssignOp::AndNot => "&^=",
        };
        f.write_str(s)
    }
}

struct SourcePrinter<'a> {
    out: String,
    indent: usize,
    interner: &'a SymbolInterner,
    force_inline: bool,
}

impl<'a> SourcePrinter<'a> {
    fn new(interner: &'a SymbolInterner) -> Self {
        Self {
            out: String::new(),
            indent: 0,
            interner,
            force_inline: false,
        }
    }

    fn new_inline(interner: &'a SymbolInterner) -> Self {
        Self {
            out: String::new(),
            indent: 0,
            interner,
            force_inline: true,
        }
    }

    fn finish(self) -> String {
        self.out
    }

    fn write_file(&mut self, file: &File) {
        if let Some(inline_mod) = &file.inline_mod {
            self.write_inline_mod(inline_mod);
        }

        if let Some(pkg) = &file.package {
            self.write_str("package ");
            self.write_ident(pkg);
            self.newline();
        }

        if !file.imports.is_empty() {
            self.newline();
            self.write_import_block(&file.imports);
        }

        if !file.decls.is_empty() {
            if !file.imports.is_empty() {
                self.newline();
                self.newline();
            } else if file.package.is_some() {
                self.newline();
            }
            for (i, decl) in file.decls.iter().enumerate() {
                if i > 0 {
                    self.newline();
                    self.newline();
                }
                self.write_decl(decl);
            }
        }

        self.newline();
    }

    fn write_inline_mod(&mut self, inline_mod: &InlineModMetadata) {
        self.write_str(INLINE_MOD_OPEN);
        self.write_str(&inline_mod.body);
        self.write_str(INLINE_MOD_CLOSE);
        self.newline();
    }

    fn write_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Var(d) => self.write_var_decl(d),
            Decl::Const(d) => self.write_const_decl(d),
            Decl::Type(d) => self.write_type_decl(d),
            Decl::Func(d) => self.write_func_decl(d),
        }
    }

    fn write_var_decl(&mut self, decl: &VarDecl) {
        if decl.specs.len() == 1 {
            self.write_str("var ");
            self.write_var_spec(&decl.specs[0]);
            return;
        }

        self.write_str("var (");
        self.newline();
        self.indent += 1;
        for spec in &decl.specs {
            self.write_indent();
            self.write_var_spec(spec);
            self.newline();
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char(')');
    }

    fn write_var_spec(&mut self, spec: &VarSpec) {
        self.write_ident_list(&spec.names);
        if let Some(ty) = &spec.ty {
            self.write_char(' ');
            self.write_type_expr(ty);
        }
        if !spec.values.is_empty() {
            self.write_str(" = ");
            self.write_expr_list(&spec.values);
        }
    }

    fn write_const_decl(&mut self, decl: &ConstDecl) {
        if decl.specs.len() == 1 {
            self.write_str("const ");
            self.write_const_spec(&decl.specs[0]);
            return;
        }

        self.write_str("const (");
        self.newline();
        self.indent += 1;
        for spec in &decl.specs {
            self.write_indent();
            self.write_const_spec(spec);
            self.newline();
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char(')');
    }

    fn write_const_spec(&mut self, spec: &ConstSpec) {
        self.write_ident_list(&spec.names);
        if let Some(ty) = &spec.ty {
            self.write_char(' ');
            self.write_type_expr(ty);
        }
        if !spec.values.is_empty() {
            self.write_str(" = ");
            self.write_expr_list(&spec.values);
        }
    }

    fn write_type_decl(&mut self, decl: &TypeDecl) {
        self.write_str("type ");
        self.write_ident(&decl.name);
        if decl.is_alias {
            self.write_str(" = ");
        } else {
            self.write_char(' ');
        }
        self.write_type_expr(&decl.ty);
    }

    fn write_func_decl(&mut self, decl: &FuncDecl) {
        self.write_str("func ");
        if let Some(receiver) = &decl.receiver {
            self.write_receiver(receiver);
            self.write_char(' ');
        }
        self.write_ident(&decl.name);
        self.write_sig_tail(&decl.sig);
        if let Some(body) = &decl.body {
            self.write_char(' ');
            self.write_block(body);
        }
    }

    fn write_receiver(&mut self, receiver: &Receiver) {
        self.write_char('(');
        if let Some(name) = &receiver.name {
            self.write_ident(name);
            self.write_char(' ');
        }
        if receiver.is_pointer {
            self.write_char('*');
        }
        self.write_ident(&receiver.ty);
        self.write_char(')');
    }

    fn write_import_block(&mut self, imports: &[ImportDecl]) {
        self.write_str("import (");
        self.newline();
        self.indent += 1;
        for import in imports {
            self.write_indent();
            if let Some(alias) = &import.alias {
                self.write_ident(alias);
                self.write_char(' ');
            }
            self.write_raw(import.path.raw);
            self.newline();
        }
        self.indent -= 1;
        self.write_char(')');
    }

    fn write_stmt(&mut self, stmt: &Stmt) {
        if matches!(stmt.kind, StmtKind::Empty) {
            return;
        }
        self.write_indent();
        self.write_stmt_inline(stmt);
        self.newline();
    }

    fn write_stmt_inline(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Empty => {}
            StmtKind::Block(block) => self.write_block(block),
            StmtKind::Var(d) => self.write_var_decl(d),
            StmtKind::Const(d) => self.write_const_decl(d),
            StmtKind::Type(d) => self.write_type_decl(d),
            StmtKind::ShortVar(d) => {
                self.write_ident_list(&d.names);
                self.write_str(" := ");
                self.write_expr_list(&d.values);
            }
            StmtKind::Expr(expr) => self.write_expr(expr),
            StmtKind::Assign(assign) => {
                self.write_expr_list(&assign.lhs);
                self.write_char(' ');
                write!(self.out, "{}", assign.op).unwrap();
                self.write_char(' ');
                self.write_expr_list(&assign.rhs);
            }
            StmtKind::IncDec(stmt) => {
                self.write_expr(&stmt.expr);
                self.write_str(if stmt.is_inc { "++" } else { "--" });
            }
            StmtKind::Return(stmt) => {
                self.write_str("return");
                if !stmt.values.is_empty() {
                    self.write_char(' ');
                    self.write_expr_list(&stmt.values);
                }
            }
            StmtKind::If(stmt) => self.write_if_stmt(stmt),
            StmtKind::For(stmt) => self.write_for_stmt(stmt),
            StmtKind::Switch(stmt) => self.write_switch_stmt(stmt),
            StmtKind::TypeSwitch(stmt) => self.write_type_switch_stmt(stmt),
            StmtKind::Select(stmt) => self.write_select_stmt(stmt),
            StmtKind::Go(stmt) => {
                self.write_str("go ");
                if let Some(target) = &stmt.target_island {
                    self.write_str("@(");
                    self.write_expr(target);
                    self.write_str(") ");
                }
                self.write_expr(&stmt.call);
            }
            StmtKind::Defer(stmt) => {
                self.write_str("defer ");
                self.write_expr(&stmt.call);
            }
            StmtKind::ErrDefer(stmt) => {
                self.write_str("errdefer ");
                self.write_expr(&stmt.call);
            }
            StmtKind::Fail(stmt) => {
                self.write_str("fail ");
                self.write_expr(&stmt.error);
            }
            StmtKind::Send(stmt) => {
                self.write_expr(&stmt.chan);
                self.write_str(" <- ");
                self.write_expr(&stmt.value);
            }
            StmtKind::Break(stmt) => {
                self.write_str("break");
                if let Some(label) = &stmt.label {
                    self.write_char(' ');
                    self.write_ident(label);
                }
            }
            StmtKind::Continue(stmt) => {
                self.write_str("continue");
                if let Some(label) = &stmt.label {
                    self.write_char(' ');
                    self.write_ident(label);
                }
            }
            StmtKind::Goto(stmt) => {
                self.write_str("goto ");
                self.write_ident(&stmt.label);
            }
            StmtKind::Fallthrough => self.write_str("fallthrough"),
            StmtKind::Labeled(stmt) => {
                self.write_ident(&stmt.label);
                self.write_str(": ");
                self.write_stmt_inline(&stmt.stmt);
            }
        }
    }

    fn write_if_stmt(&mut self, stmt: &IfStmt) {
        self.write_str("if ");
        if let Some(init) = &stmt.init {
            self.write_stmt_inline(init);
            self.write_str("; ");
        }
        self.write_expr(&stmt.cond);
        self.write_char(' ');
        self.write_block(&stmt.then);
        if let Some(else_) = &stmt.else_ {
            self.write_str(" else ");
            self.write_stmt_inline(else_);
        }
    }

    fn write_for_stmt(&mut self, stmt: &crate::ast::ForStmt) {
        self.write_str("for");
        match &stmt.clause {
            ForClause::Cond(None) => {}
            ForClause::Cond(Some(cond)) => {
                self.write_char(' ');
                self.write_expr(cond);
            }
            ForClause::Three { init, cond, post } => {
                self.write_char(' ');
                if let Some(init) = init {
                    self.write_stmt_inline(init);
                }
                self.write_str("; ");
                if let Some(cond) = cond {
                    self.write_expr(cond);
                }
                self.write_str("; ");
                if let Some(post) = post {
                    self.write_stmt_inline(post);
                }
            }
            ForClause::Range {
                key,
                value,
                define,
                expr,
            } => {
                self.write_char(' ');
                if let Some(key) = key {
                    self.write_expr(key);
                    if let Some(value) = value {
                        self.write_str(", ");
                        self.write_expr(value);
                    }
                    self.write_str(if *define { " := range " } else { " = range " });
                } else {
                    self.write_str("range ");
                }
                self.write_expr(expr);
            }
        }
        self.write_char(' ');
        self.write_block(&stmt.body);
    }

    fn write_switch_stmt(&mut self, stmt: &SwitchStmt) {
        self.write_str("switch");
        if stmt.init.is_some() || stmt.tag.is_some() {
            self.write_char(' ');
            if let Some(init) = &stmt.init {
                self.write_stmt_inline(init);
                self.write_str("; ");
            }
            if let Some(tag) = &stmt.tag {
                self.write_expr(tag);
            }
        }
        self.write_str(" {");
        self.newline();
        self.indent += 1;
        for case in &stmt.cases {
            self.write_case_clause(case);
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char('}');
    }

    fn write_case_clause(&mut self, case: &CaseClause) {
        self.write_indent();
        if case.exprs.is_empty() {
            self.write_str("default:");
        } else {
            self.write_str("case ");
            self.write_expr_list(&case.exprs);
            self.write_char(':');
        }
        self.newline();
        self.indent += 1;
        for stmt in &case.body {
            self.write_stmt(stmt);
        }
        self.indent -= 1;
    }

    fn write_type_switch_stmt(&mut self, stmt: &TypeSwitchStmt) {
        self.write_str("switch ");
        if let Some(init) = &stmt.init {
            self.write_stmt_inline(init);
            self.write_str("; ");
        }
        if let Some(assign) = &stmt.assign {
            self.write_ident(assign);
            self.write_str(" := ");
        }
        self.write_expr(&stmt.expr);
        self.write_str(" {");
        self.newline();
        self.indent += 1;
        for case in &stmt.cases {
            self.write_type_case_clause(case);
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char('}');
    }

    fn write_type_case_clause(&mut self, case: &TypeCaseClause) {
        self.write_indent();
        if case.types.is_empty() {
            self.write_str("default:");
        } else {
            self.write_str("case ");
            for (i, ty) in case.types.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                if let Some(ty) = ty {
                    self.write_type_expr(ty);
                } else {
                    self.write_str("nil");
                }
            }
            self.write_char(':');
        }
        self.newline();
        self.indent += 1;
        for stmt in &case.body {
            self.write_stmt(stmt);
        }
        self.indent -= 1;
    }

    fn write_select_stmt(&mut self, stmt: &SelectStmt) {
        self.write_str("select {");
        self.newline();
        self.indent += 1;
        for case in &stmt.cases {
            self.write_select_case(case);
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char('}');
    }

    fn write_select_case(&mut self, case: &SelectCase) {
        self.write_indent();
        match &case.comm {
            None => self.write_str("default:"),
            Some(CommClause::Send(send)) => {
                self.write_str("case ");
                self.write_expr(&send.chan);
                self.write_str(" <- ");
                self.write_expr(&send.value);
                self.write_char(':');
            }
            Some(CommClause::Recv(recv)) => {
                self.write_str("case ");
                if !recv.lhs.is_empty() {
                    self.write_ident_list(&recv.lhs);
                    self.write_str(if recv.define { " := " } else { " = " });
                }
                self.write_str("<-");
                self.write_expr(&recv.expr);
                self.write_char(':');
            }
        }
        self.newline();
        self.indent += 1;
        for stmt in &case.body {
            self.write_stmt(stmt);
        }
        self.indent -= 1;
    }

    fn write_block(&mut self, block: &Block) {
        let has_non_empty = block
            .stmts
            .iter()
            .any(|stmt| !matches!(stmt.kind, StmtKind::Empty));
        if !has_non_empty {
            self.write_str("{}");
            return;
        }

        self.write_char('{');
        self.newline();
        self.indent += 1;
        for stmt in &block.stmts {
            self.write_stmt(stmt);
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char('}');
    }

    fn write_sig_tail(&mut self, sig: &FuncSig) {
        self.write_char('(');
        self.write_params(&sig.params, sig.variadic);
        self.write_char(')');
        self.write_results(&sig.results);
    }

    fn write_func_type(&mut self, ty: &FuncType) {
        self.write_str("func(");
        self.write_params(&ty.params, ty.variadic);
        self.write_char(')');
        self.write_func_type_results(&ty.results);
    }

    fn write_params(&mut self, params: &[Param], variadic: bool) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            if !param.names.is_empty() {
                self.write_ident_list(&param.names);
                self.write_char(' ');
            }
            if variadic && i + 1 == params.len() {
                self.write_str("...");
            }
            self.write_type_expr(&param.ty);
        }
    }

    fn write_plain_params(&mut self, params: &[Param]) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            if !param.names.is_empty() {
                self.write_ident_list(&param.names);
                self.write_char(' ');
            }
            self.write_type_expr(&param.ty);
        }
    }

    fn write_results(&mut self, results: &[ResultParam]) {
        if results.is_empty() {
            return;
        }
        self.write_char(' ');
        if results.len() == 1 && results[0].name.is_none() {
            self.write_type_expr(&results[0].ty);
            return;
        }
        self.write_char('(');
        for (i, result) in results.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            if let Some(name) = &result.name {
                self.write_ident(name);
                self.write_char(' ');
            }
            self.write_type_expr(&result.ty);
        }
        self.write_char(')');
    }

    fn write_func_type_results(&mut self, results: &[Param]) {
        if results.is_empty() {
            return;
        }
        self.write_char(' ');
        if results.len() == 1 && results[0].names.is_empty() {
            self.write_type_expr(&results[0].ty);
            return;
        }
        self.write_char('(');
        self.write_plain_params(results);
        self.write_char(')');
    }

    fn write_type_expr(&mut self, ty: &TypeExpr) {
        match &ty.kind {
            TypeExprKind::Ident(ident) => self.write_ident(ident),
            TypeExprKind::Selector(sel) => {
                self.write_ident(&sel.pkg);
                self.write_char('.');
                self.write_ident(&sel.sel);
            }
            TypeExprKind::Array(arr) => {
                self.write_char('[');
                self.write_expr(&arr.len);
                self.write_char(']');
                self.write_type_expr(&arr.elem);
            }
            TypeExprKind::Slice(elem) => {
                self.write_str("[]");
                self.write_type_expr(elem);
            }
            TypeExprKind::Map(map) => {
                self.write_str("map[");
                self.write_type_expr(&map.key);
                self.write_char(']');
                self.write_type_expr(&map.value);
            }
            TypeExprKind::Chan(chan) => self.write_chan_like("chan", chan.dir, &chan.elem),
            TypeExprKind::Port(port) => self.write_port_type(port),
            TypeExprKind::Island => self.write_str("island"),
            TypeExprKind::Func(func) => self.write_func_type(func),
            TypeExprKind::Struct(strukt) => self.write_struct_type(strukt),
            TypeExprKind::Pointer(inner) => {
                self.write_char('*');
                self.write_type_expr(inner);
            }
            TypeExprKind::Interface(interface) => self.write_interface_type(interface),
        }
    }

    fn write_port_type(&mut self, port: &PortType) {
        self.write_chan_like("port", port.dir, &port.elem);
    }

    fn write_chan_like(&mut self, prefix: &str, dir: ChanDir, elem: &TypeExpr) {
        match dir {
            ChanDir::Both => {
                self.write_str(prefix);
                self.write_char(' ');
            }
            ChanDir::Send => {
                self.write_str(prefix);
                self.write_str("<- ");
            }
            ChanDir::Recv => {
                self.write_str("<-");
                self.write_str(prefix);
                self.write_char(' ');
            }
        }
        self.write_type_expr(elem);
    }

    fn write_struct_type(&mut self, strukt: &StructType) {
        if strukt.fields.is_empty() {
            self.write_str("struct{}");
            return;
        }

        self.write_str("struct {");
        self.newline();
        self.indent += 1;
        for field in &strukt.fields {
            self.write_indent();
            if !field.names.is_empty() {
                self.write_ident_list(&field.names);
                self.write_char(' ');
            }
            self.write_type_expr(&field.ty);
            if let Some(tag) = &field.tag {
                self.write_char(' ');
                self.write_raw(tag.raw);
            }
            self.newline();
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char('}');
    }

    fn write_interface_type(&mut self, interface: &InterfaceType) {
        if interface.elems.is_empty() {
            self.write_str("interface{}");
            return;
        }

        self.write_str("interface {");
        self.newline();
        self.indent += 1;
        for elem in &interface.elems {
            self.write_indent();
            match elem {
                InterfaceElem::Method(method) => self.write_method_spec(method),
                InterfaceElem::Embedded(ident) => self.write_ident(ident),
                InterfaceElem::EmbeddedQualified { pkg, name, .. } => {
                    self.write_ident(pkg);
                    self.write_char('.');
                    self.write_ident(name);
                }
            }
            self.newline();
        }
        self.indent -= 1;
        self.write_indent();
        self.write_char('}');
    }

    fn write_method_spec(&mut self, method: &crate::ast::MethodSpec) {
        self.write_ident(&method.name);
        self.write_sig_tail(&method.sig);
    }

    fn write_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(ident) => self.write_ident(ident),
            ExprKind::IntLit(lit) => self.write_raw(lit.raw),
            ExprKind::FloatLit(lit) => self.write_raw(lit.raw),
            ExprKind::RuneLit(lit) => self.write_raw(lit.raw),
            ExprKind::StringLit(lit) => self.write_raw(lit.raw),
            ExprKind::Binary(binary) => self.write_binary_expr(binary),
            ExprKind::Unary(unary) => {
                write!(self.out, "{}", unary.op).unwrap();
                if matches!(&unary.operand.kind, ExprKind::Unary(_)) {
                    self.out.push(' ');
                }
                self.write_expr(&unary.operand);
            }
            ExprKind::Call(call) => {
                self.write_expr(&call.func);
                self.write_call_args(&call.args, call.spread);
            }
            ExprKind::Index(index) => {
                self.write_expr(&index.expr);
                self.write_char('[');
                self.write_expr(&index.index);
                self.write_char(']');
            }
            ExprKind::Slice(slice) => {
                self.write_expr(&slice.expr);
                self.write_char('[');
                if let Some(low) = &slice.low {
                    self.write_expr(low);
                }
                self.write_char(':');
                if let Some(high) = &slice.high {
                    self.write_expr(high);
                }
                if let Some(max) = &slice.max {
                    self.write_char(':');
                    self.write_expr(max);
                }
                self.write_char(']');
            }
            ExprKind::Selector(selector) => {
                self.write_expr(&selector.expr);
                self.write_char('.');
                self.write_ident(&selector.sel);
            }
            ExprKind::TypeAssert(assert) => {
                self.write_expr(&assert.expr);
                self.write_str(".(");
                match &assert.ty {
                    Some(ty) => self.write_type_expr(ty),
                    None => self.write_str("type"),
                }
                self.write_char(')');
            }
            ExprKind::CompositeLit(lit) => self.write_composite_lit(expr, lit),
            ExprKind::FuncLit(func) => {
                self.write_str("func");
                self.write_sig_tail(&func.sig);
                self.write_char(' ');
                self.write_block(&func.body);
            }
            ExprKind::Conversion(conv) => {
                if matches!(conv.ty.kind, TypeExprKind::Pointer(_)) {
                    self.write_char('(');
                    self.write_type_expr(&conv.ty);
                    self.write_char(')');
                } else {
                    self.write_type_expr(&conv.ty);
                }
                self.write_char('(');
                self.write_expr(&conv.expr);
                self.write_char(')');
            }
            ExprKind::Receive(expr) => {
                self.write_str("<-");
                self.write_expr(expr);
            }
            ExprKind::Paren(expr) => {
                self.write_char('(');
                self.write_expr(expr);
                self.write_char(')');
            }
            ExprKind::TypeAsExpr(ty) => self.write_type_expr(ty),
            ExprKind::TryUnwrap(expr) => {
                self.write_expr(expr);
                self.write_char('?');
            }
            ExprKind::DynAccess(access) => {
                self.write_expr(&access.base);
                self.write_str("~>");
                match &access.op {
                    DynAccessOp::Field(field) => self.write_ident(field),
                    DynAccessOp::Index(index) => {
                        self.write_char('[');
                        self.write_expr(index);
                        self.write_char(']');
                    }
                    DynAccessOp::Call { args, spread } => self.write_call_args(args, *spread),
                    DynAccessOp::MethodCall {
                        method,
                        args,
                        spread,
                    } => {
                        self.write_ident(method);
                        self.write_call_args(args, *spread);
                    }
                }
            }
            ExprKind::Ellipsis => self.write_str("..."),
        }
    }

    fn write_binary_expr(&mut self, binary: &BinaryExpr) {
        // Pratt parsing represents a flat left-associative expression as a
        // left-deep tree.  Render that spine in one pass.  Recursing through
        // it made formatting quadratic because every node first rendered its
        // complete subtree again in `expression_fits`.
        let mut leftmost = &binary.left;
        let mut chain = vec![binary];
        while let ExprKind::Binary(inner) = &leftmost.kind {
            chain.push(inner);
            leftmost = &inner.left;
        }
        chain.reverse();

        if self.force_inline {
            self.write_expr(leftmost);
            for binary in chain {
                self.write_char(' ');
                write!(self.out, "{}", binary.op).unwrap();
                self.write_char(' ');
                self.write_expr(&binary.right);
            }
            return;
        }

        let available = MAX_LINE_WIDTH.saturating_sub(self.current_column());
        let mut prefix_width = self.inline_expr(leftmost).chars().count();
        let mut break_after = chain.len();
        for (index, binary) in chain.iter().enumerate() {
            let right_width = self.inline_expr(&binary.right).chars().count();
            let operator_width = binary.op.to_string().chars().count();
            prefix_width = prefix_width
                .saturating_add(operator_width)
                .saturating_add(right_width)
                .saturating_add(2);
            if prefix_width > available && break_after == chain.len() {
                break_after = index;
            }
        }

        self.write_expr(leftmost);
        for (index, binary) in chain.into_iter().enumerate() {
            self.write_char(' ');
            write!(self.out, "{}", binary.op).unwrap();
            if index < break_after {
                self.write_char(' ');
                self.write_expr(&binary.right);
            } else {
                self.newline();
                self.indent += 1;
                self.write_indent();
                self.write_expr(&binary.right);
                self.indent -= 1;
            }
        }
    }

    fn write_composite_lit(&mut self, expr: &Expr, lit: &CompositeLit) {
        if self.force_inline || self.expression_fits(expr) {
            if let Some(ty) = &lit.ty {
                self.write_type_expr(ty);
            }
            self.write_char('{');
            for (index, elem) in lit.elems.iter().enumerate() {
                if index > 0 {
                    self.write_str(", ");
                }
                self.write_composite_elem(elem);
            }
            self.write_char('}');
            return;
        }

        if let Some(ty) = &lit.ty {
            self.write_type_expr(ty);
        }
        self.write_char('{');
        self.newline();
        self.indent += 1;

        let mut line_has_elements = false;
        for elem in &lit.elems {
            let inline = self.inline_composite_elem(elem);
            let inline_width = inline.chars().count();
            let fits_on_fresh_line = self.indent + inline_width < MAX_LINE_WIDTH;

            if !fits_on_fresh_line {
                if line_has_elements {
                    self.newline();
                }
                self.write_indent();
                self.write_composite_elem(elem);
                self.write_char(',');
                self.newline();
                line_has_elements = false;
                continue;
            }

            if !line_has_elements {
                self.write_indent();
            } else if self.current_column() + inline_width + 2 > MAX_LINE_WIDTH {
                self.newline();
                self.write_indent();
            } else {
                self.write_char(' ');
            }
            self.write_str(&inline);
            self.write_char(',');
            line_has_elements = true;
        }
        if line_has_elements {
            self.newline();
        }

        self.indent -= 1;
        self.write_indent();
        self.write_char('}');
    }

    fn write_composite_elem(&mut self, elem: &CompositeLitElem) {
        if let Some(key) = &elem.key {
            match key {
                CompositeLitKey::Ident(ident) => self.write_ident(ident),
                CompositeLitKey::Expr(expr) => self.write_expr(expr),
            }
            self.write_str(": ");
        }
        self.write_expr(&elem.value);
    }

    fn inline_expr(&self, expr: &Expr) -> String {
        let mut printer = Self::new_inline(self.interner);
        printer.write_expr(expr);
        printer.finish()
    }

    fn inline_composite_elem(&self, elem: &CompositeLitElem) -> String {
        let mut printer = Self::new_inline(self.interner);
        printer.write_composite_elem(elem);
        printer.finish()
    }

    fn expression_fits(&self, expr: &Expr) -> bool {
        self.current_column() + self.inline_expr(expr).chars().count() <= MAX_LINE_WIDTH
    }

    fn write_call_args(&mut self, args: &[Expr], spread: bool) {
        self.write_char('(');
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            self.write_expr(arg);
        }
        if spread {
            self.write_str("...");
        }
        self.write_char(')');
    }

    fn write_ident_list(&mut self, idents: &[Ident]) {
        for (i, ident) in idents.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            self.write_ident(ident);
        }
    }

    fn write_expr_list(&mut self, exprs: &[Expr]) {
        for (i, expr) in exprs.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            self.write_expr(expr);
        }
    }

    fn write_ident(&mut self, ident: &Ident) {
        let value = self.resolve_ident(ident).to_string();
        self.write_str(&value);
    }

    fn write_raw(&mut self, symbol: Symbol) {
        let value = self.resolve_symbol(symbol).to_string();
        self.write_str(&value);
    }

    fn resolve_ident(&self, ident: &Ident) -> &str {
        self.interner.resolve(ident.symbol).unwrap_or("?")
    }

    fn resolve_symbol(&self, symbol: Symbol) -> &str {
        self.interner.resolve(symbol).unwrap_or("?")
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.out.push('\t');
        }
    }

    fn write_char(&mut self, ch: char) {
        self.out.push(ch);
    }

    fn write_str(&mut self, s: &str) {
        self.out.push_str(s);
    }

    fn current_column(&self) -> usize {
        self.out.rsplit_once('\n').map_or_else(
            || self.out.chars().count(),
            |(_, line)| line.chars().count(),
        )
    }

    fn newline(&mut self) {
        self.out.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use super::{format_file, MAX_LINE_WIDTH};

    fn assert_reformats_identically(source: &str) -> String {
        let (file, diagnostics, interner) = crate::parser::parse(source, 0);
        assert!(
            !diagnostics.has_errors(),
            "source failed to parse: {source}"
        );
        let formatted = format_file(&file, &interner);
        let (reparsed, diagnostics, interner) = crate::parser::parse(&formatted, 0);
        assert!(!diagnostics.has_errors(), "formatted source:\n{formatted}");
        assert_eq!(format_file(&reparsed, &interner), formatted);
        formatted
    }

    #[test]
    fn format_simple_file() {
        let source = "package main\nimport \"fmt\"\nfunc main(){fmt.Println(\"hello\")}";
        let (file, diags, interner) = crate::parser::parse(source, 0);
        assert!(!diags.has_errors());
        let formatted = format_file(&file, &interner);
        assert_eq!(
            formatted,
            "package main\n\nimport (\n\t\"fmt\"\n)\n\nfunc main() {\n\tfmt.Println(\"hello\")\n}\n"
        );
    }

    #[test]
    fn format_struct_and_interface_types() {
        let source = "package main\ntype User struct{name string\nage int}\ntype Reader interface{Read(p []byte) (n int, err error)}";
        let (file, diags, interner) = crate::parser::parse(source, 0);
        assert!(!diags.has_errors());
        let formatted = format_file(&file, &interner);
        assert_eq!(
            formatted,
            "package main\n\ntype User struct {\n\tname string\n\tage int\n}\n\ntype Reader interface {\n\tRead(p []byte) (n int, err error)\n}\n"
        );
    }

    #[test]
    fn format_type_switch_guard_once() {
        let source = "package main\nfunc main(){var x any\nswitch v:=x.(type){case int:_=v}}";
        let (file, diags, interner) = crate::parser::parse(source, 0);
        assert!(!diags.has_errors());
        let formatted = format_file(&file, &interner);
        assert_eq!(formatted.matches(".(type)").count(), 1);
        assert!(formatted.contains("switch v := x.(type) {"));
        let (_, reparsed_diags, _) = crate::parser::parse(&formatted, 0);
        assert!(!reparsed_diags.has_errors());

        let source = "package main\nfunc main(){var x any\nswitch x.(type){case int:}}";
        let (file, diags, interner) = crate::parser::parse(source, 0);
        assert!(!diags.has_errors());
        let formatted = format_file(&file, &interner);
        assert!(formatted.contains("switch x.(type) {"));
        let (_, reparsed_diags, _) = crate::parser::parse(&formatted, 0);
        assert!(!reparsed_diags.has_errors());
    }

    #[test]
    fn format_preserves_original_inline_mod_toml_body() {
        let body = "\r\n# Keep authored TOML intact.\r\nmodule = \"local/demo\"\r\nvo = \"^0.1.0\"\r\n\r\n[dependencies]\r\n\"github.com/acme/lib\" = \"^1.2.0\"\r\n";
        let source = format!("/*vo:mod{body}*/\npackage main\n");
        let (file, diags, interner) = crate::parser::parse(&source, 0);
        assert!(!diags.has_errors());
        let formatted = format_file(&file, &interner);
        assert!(formatted.starts_with(&format!("/*vo:mod{body}*/\npackage main")));
    }

    #[test]
    fn format_preserves_pointer_conversion_parentheses() {
        let source =
            "package main\ntype Node struct{}\nfunc main(){var value any=(*Node)(nil)\n_=value}";
        let (file, diags, interner) = crate::parser::parse(source, 0);
        assert!(!diags.has_errors());
        let formatted = format_file(&file, &interner);
        assert!(formatted.contains("var value any = (*Node)(nil)"));
        let (_, reparsed_diags, _) = crate::parser::parse(&formatted, 0);
        assert!(!reparsed_diags.has_errors());
    }

    #[test]
    fn format_separates_nested_unary_operators() {
        let source =
            "package main\nfunc main(){x:=1\nb:=true\n_=- -x\n_=+ +x\n_=& &x\n_=& ^x\n_=! !b}";
        let (file, diags, interner) = crate::parser::parse(source, 0);
        assert!(!diags.has_errors());
        let formatted = format_file(&file, &interner);
        for expression in ["- -x", "+ +x", "& &x", "& ^x", "! !b"] {
            assert!(formatted.contains(expression), "{formatted}");
        }
        let (_, reparsed_diags, _) = crate::parser::parse(&formatted, 0);
        assert!(!reparsed_diags.has_errors(), "{formatted}");
    }

    #[test]
    fn format_wraps_long_composite_literals_and_is_idempotent() {
        let values = (0..64)
            .map(|value| value.to_string())
            .collect::<Vec<_>>()
            .join(",");
        let source = format!("package main\nvar values=[]int{{{values}}}\n");
        let formatted = assert_reformats_identically(&source);

        assert!(formatted.contains("[]int{\n"), "{formatted}");
        assert!(
            formatted
                .lines()
                .all(|line| line.chars().count() <= MAX_LINE_WIDTH),
            "{formatted}"
        );
    }

    #[test]
    fn format_wraps_long_binary_expressions_after_operators() {
        let terms = (1..24)
            .map(|shift| format!("x >> {shift}"))
            .collect::<Vec<_>>()
            .join(" | ");
        let source = format!("package main\nfunc reverse(x uint64) uint64{{return {terms}}}\n");
        let formatted = assert_reformats_identically(&source);

        assert!(formatted.contains("|\n"), "{formatted}");
        assert!(
            formatted
                .lines()
                .all(|line| line.chars().count() <= MAX_LINE_WIDTH),
            "{formatted}"
        );
    }

    #[test]
    fn format_handles_binary_expression_resource_boundary_linearly() {
        let operators = crate::parser::MAX_BINARY_EXPRESSION_PATH;
        let terms = vec!["value"; operators + 1].join(" + ");
        let source = format!("package main\nfunc sum() int{{return {terms}}}\n");
        let formatted = assert_reformats_identically(&source);

        assert_eq!(formatted.matches(" +").count(), operators);
        assert!(
            formatted
                .lines()
                .all(|line| line.chars().count() <= MAX_LINE_WIDTH),
            "formatter produced an overlong line"
        );
    }
}
