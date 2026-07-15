//! Escape analysis pass.
//!
//! This module implements static escape analysis to determine which local variables
//! need heap allocation. It runs as a separate pass after type checking completes.

use super::type_info::TypeInfo;
use crate::objects::{ObjKey, ScopeKey, TCObjects};
use crate::selection::SelectionKind;
use crate::typ;
use std::collections::{HashMap, HashSet};
use vo_syntax::ast::ExprId;
use vo_syntax::ast::{
    AssignOp, Block, CompositeLitKey, Decl, Expr, ExprKind, File, FuncDecl, Ident, Stmt, StmtKind,
    UnaryOp,
};

/// Escape analysis result.
pub struct EscapeResult {
    /// Variables that escape to heap.
    pub escaped: HashSet<ObjKey>,
    /// Closure captures: FuncLit ExprId -> captured variables.
    pub closure_captures: HashMap<ExprId, Vec<ObjKey>>,
    /// Variables defined inside loops (for Go 1.22 per-iteration semantics).
    pub loop_defined_vars: HashSet<ObjKey>,
}

/// Analyze escape for all files and return escaped variables and closure captures.
pub fn analyze(files: &[File], type_info: &TypeInfo, tc_objs: &TCObjects) -> EscapeResult {
    let mut analyzer = EscapeAnalyzer::new(type_info, tc_objs);
    for file in files {
        analyzer.visit_file(file);
    }
    EscapeResult {
        escaped: analyzer.escaped,
        closure_captures: analyzer.closure_captures,
        loop_defined_vars: analyzer.loop_defined_vars,
    }
}

/// Entry in closure stack during escape analysis.
struct ClosureEntry {
    id: ExprId,
    scope: Option<ScopeKey>,
}

/// Context for tracking named returns in the current function.
/// When a defer is encountered, all named returns must escape.
struct FuncContext {
    named_returns: Vec<ObjKey>,
}

/// The escape analyzer.
struct EscapeAnalyzer<'a> {
    type_info: &'a TypeInfo,
    tc_objs: &'a TCObjects,
    escaped: HashSet<ObjKey>,
    /// Closure captures: FuncLit ExprId -> captured variables.
    closure_captures: HashMap<ExprId, Vec<ObjKey>>,
    /// Variables defined inside loops (Go 1.22 per-iteration semantics).
    loop_defined_vars: HashSet<ObjKey>,
    /// Current function scope (None for package-level code)
    func_scope: Option<ScopeKey>,
    /// Stack of closures being analyzed (for nested closure captures)
    closure_stack: Vec<ClosureEntry>,
    /// Loop nesting depth (>0 means inside a loop)
    loop_depth: u32,
    /// Stack of function contexts for tracking named returns
    func_context_stack: Vec<FuncContext>,
}

impl<'a> EscapeAnalyzer<'a> {
    fn new(type_info: &'a TypeInfo, tc_objs: &'a TCObjects) -> Self {
        Self {
            type_info,
            tc_objs,
            escaped: HashSet::new(),
            closure_captures: HashMap::new(),
            loop_defined_vars: HashSet::new(),
            func_scope: None,
            closure_stack: Vec::new(),
            loop_depth: 0,
            func_context_stack: Vec::new(),
        }
    }

    /// Mark all named returns in the current function as escaped.
    /// Called when a defer/errdefer is encountered.
    fn mark_named_returns_escaped(&mut self) {
        if let Some(ctx) = self.func_context_stack.last() {
            for obj in &ctx.named_returns {
                self.escaped.insert(*obj);
            }
        }
    }

    /// Mark a variable as loop-defined if we're inside a loop.
    fn mark_loop_var(&mut self, obj: ObjKey) {
        if self.loop_depth > 0 {
            self.loop_defined_vars.insert(obj);
        }
    }

    /// Mark loop variable from an identifier expression (if define=true).
    fn mark_loop_var_from_ident(&mut self, expr: &Expr) {
        if let ExprKind::Ident(ident) = &expr.kind {
            if let Some(Some(obj)) = self.type_info.defs.get(&ident.id) {
                self.mark_loop_var(*obj);
            }
        }
    }

    /// Visit an identifier use and record closure captures/escapes when needed.
    fn visit_ident_use(&mut self, ident: &Ident) {
        let Some(func_scope) = self.func_scope else {
            return;
        };
        let Some(&obj) = self.type_info.uses.get(&ident.id) else {
            return;
        };
        if !self.is_captured(obj, func_scope) {
            return;
        }

        self.escaped.insert(obj);
        // Record capture for closures that actually need to capture this var.
        for entry in &self.closure_stack {
            if let Some(scope) = entry.scope {
                if self.is_captured(obj, scope) {
                    if let Some(captures) = self.closure_captures.get_mut(&entry.id) {
                        if !captures.contains(&obj) {
                            captures.push(obj);
                        }
                    }
                }
            }
        }
    }

    fn visit_file(&mut self, file: &File) {
        for decl in &file.decls {
            self.visit_decl(decl);
        }
    }

    fn visit_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Func(fdecl) => self.visit_func_decl(fdecl),
            Decl::Var(vdecl) => {
                for spec in &vdecl.specs {
                    for value in &spec.values {
                        self.visit_expr(value);
                    }
                }
            }
            Decl::Const(cdecl) => {
                for spec in &cdecl.specs {
                    for value in &spec.values {
                        self.visit_expr(value);
                    }
                }
            }
            Decl::Type(_) => {}
        }
    }

    fn visit_func_decl(&mut self, fdecl: &FuncDecl) {
        if let Some(body) = &fdecl.body {
            let func_scope = self.type_info.scopes.get(&fdecl.sig.span).copied();
            let saved = self.func_scope;
            self.func_scope = func_scope;

            // Collect named returns - they'll be marked as escaped when we encounter defer/errdefer
            let named_returns: Vec<ObjKey> = fdecl
                .sig
                .results
                .iter()
                .filter_map(|r| r.name.as_ref())
                .filter_map(|name| self.type_info.defs.get(&name.id).and_then(|o| *o))
                .collect();
            self.func_context_stack.push(FuncContext { named_returns });

            self.visit_block(body);

            self.func_context_stack.pop();
            self.func_scope = saved;
        }
    }

    fn visit_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Empty => {}
            StmtKind::Block(block) => self.visit_block(block),
            StmtKind::Var(vdecl) => {
                for spec in &vdecl.specs {
                    // Check if var type is interface
                    let is_interface_type = spec
                        .ty
                        .as_ref()
                        .and_then(|ty| self.type_info.type_exprs.get(&ty.id))
                        .map(|&type_key| typ::is_interface(type_key, self.tc_objs))
                        .unwrap_or(false);

                    for value in &spec.values {
                        self.visit_expr(value);
                        // If var type is interface and value is not basic type, value escapes
                        // But if value is also interface, it doesn't escape (interface-to-interface copy)
                        if is_interface_type {
                            let value_is_interface = self
                                .type_info
                                .types
                                .get(&value.id)
                                .map(|t| typ::is_interface(t.typ, self.tc_objs))
                                .unwrap_or(false);
                            if !value_is_interface {
                                if let Some(root) = self.find_root_var(value) {
                                    if !self.is_basic_type(root) {
                                        self.escaped.insert(root);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            StmtKind::Const(cdecl) => {
                for spec in &cdecl.specs {
                    for value in &spec.values {
                        self.visit_expr(value);
                    }
                }
            }
            StmtKind::Type(_) => {}
            StmtKind::ShortVar(svd) => {
                for value in &svd.values {
                    self.visit_expr(value);
                }
            }
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Assign(assign) => {
                for l in &assign.lhs {
                    self.visit_expr(l);
                }
                for r in &assign.rhs {
                    self.visit_expr(r);
                }

                // Check interface assignment
                for (l, r) in assign.lhs.iter().zip(assign.rhs.iter()) {
                    // If LHS is interface type and RHS is struct/array, RHS escapes
                    // But if RHS is also interface, it doesn't escape (interface-to-interface copy)
                    if assign.op == AssignOp::Assign {
                        if let Some(tv) = self.type_info.types.get(&l.id) {
                            if typ::is_interface(tv.typ, self.tc_objs) {
                                // Check if RHS is also interface - if so, don't escape
                                let rhs_is_interface = self
                                    .type_info
                                    .types
                                    .get(&r.id)
                                    .map(|t| typ::is_interface(t.typ, self.tc_objs))
                                    .unwrap_or(false);
                                if !rhs_is_interface {
                                    if let Some(root) = self.find_root_var(r) {
                                        if !self.is_basic_type(root) {
                                            self.escaped.insert(root);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            StmtKind::IncDec(id) => self.visit_expr(&id.expr),
            StmtKind::Return(ret) => {
                for expr in &ret.values {
                    self.visit_expr(expr);
                }
            }
            StmtKind::If(ifs) => {
                if let Some(init) = &ifs.init {
                    self.visit_stmt(init);
                }
                self.visit_expr(&ifs.cond);
                self.visit_block(&ifs.then);
                if let Some(else_) = &ifs.else_ {
                    self.visit_stmt(else_);
                }
            }
            StmtKind::For(fs) => {
                use vo_syntax::ast::ForClause;

                // Enter loop - variables defined here are loop variables
                self.loop_depth += 1;

                match &fs.clause {
                    ForClause::Cond(Some(e)) => self.visit_expr(e),
                    ForClause::Three { init, cond, post } => {
                        // Mark variables defined in init as loop variables
                        if let Some(init) = init {
                            if let StmtKind::ShortVar(svd) = &init.kind {
                                for name in &svd.names {
                                    if let Some(Some(obj)) = self.type_info.defs.get(&name.id) {
                                        self.mark_loop_var(*obj);
                                    }
                                }
                            }
                            self.visit_stmt(init);
                        }
                        if let Some(cond) = cond {
                            self.visit_expr(cond);
                        }
                        if let Some(post) = post {
                            self.visit_stmt(post);
                        }
                    }
                    ForClause::Range {
                        key,
                        value,
                        define,
                        expr,
                    } => {
                        if *define {
                            if let Some(k) = key {
                                self.mark_loop_var_from_ident(k);
                            }
                            if let Some(v) = value {
                                self.mark_loop_var_from_ident(v);
                            }
                        } else {
                            if let Some(k) = key {
                                self.visit_expr(k);
                            }
                            if let Some(v) = value {
                                self.visit_expr(v);
                            }
                        }
                        self.visit_expr(expr);
                    }
                    _ => {}
                }
                self.visit_block(&fs.body);

                self.loop_depth -= 1;
            }
            StmtKind::Switch(ss) => {
                if let Some(init) = &ss.init {
                    self.visit_stmt(init);
                }
                if let Some(tag) = &ss.tag {
                    self.visit_expr(tag);
                }
                for case in &ss.cases {
                    for e in &case.exprs {
                        self.visit_expr(e);
                    }
                    for s in &case.body {
                        self.visit_stmt(s);
                    }
                }
            }
            StmtKind::TypeSwitch(tss) => {
                if let Some(init) = &tss.init {
                    self.visit_stmt(init);
                }
                self.visit_expr(&tss.expr);
                for case in &tss.cases {
                    for s in &case.body {
                        self.visit_stmt(s);
                    }
                }
            }
            StmtKind::Select(ss) => {
                for case in &ss.cases {
                    if let Some(comm) = &case.comm {
                        match comm {
                            vo_syntax::ast::CommClause::Send(send) => {
                                self.visit_expr(&send.chan);
                                self.visit_expr(&send.value);
                            }
                            vo_syntax::ast::CommClause::Recv(recv) => {
                                self.visit_expr(&recv.expr);
                                for ident in &recv.lhs {
                                    self.visit_ident_use(ident);
                                }
                            }
                        }
                    }
                    for s in &case.body {
                        self.visit_stmt(s);
                    }
                }
            }
            StmtKind::Go(g) => {
                if let Some(island) = &g.target_island {
                    self.visit_expr(island);
                }
                self.visit_expr(&g.call);
            }
            StmtKind::Defer(d) => {
                // Named returns must escape for correct panic/recover semantics
                self.mark_named_returns_escaped();
                self.visit_expr(&d.call);
            }
            StmtKind::ErrDefer(d) => {
                self.mark_named_returns_escaped();
                self.visit_expr(&d.call);
            }
            StmtKind::Fail(f) => self.visit_expr(&f.error),
            StmtKind::Send(s) => {
                self.visit_expr(&s.chan);
                self.visit_expr(&s.value);
            }
            StmtKind::Break(_)
            | StmtKind::Continue(_)
            | StmtKind::Goto(_)
            | StmtKind::Fallthrough => {}
            StmtKind::Labeled(l) => self.visit_stmt(&l.stmt),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            // 1. Address taken → root variable escapes
            ExprKind::Unary(unary) if unary.op == UnaryOp::Addr => {
                if let Some(root) = self.find_root_var(&unary.operand) {
                    self.escaped.insert(root);
                }
                self.visit_expr(&unary.operand);
            }

            // 2. Slice operation → array escapes
            ExprKind::Slice(sl) => {
                let slices_array_storage =
                    self.type_info.types.get(&sl.expr.id).is_some_and(|tv| {
                        let underlying = typ::underlying_type(tv.typ, self.tc_objs);
                        self.tc_objs.types[underlying].try_as_array().is_some()
                    });
                if slices_array_storage {
                    if let Some(root) = self.find_root_var(&sl.expr) {
                        // The root can be a struct whose selected field is the
                        // array. Keep the complete aggregate alive so the slice
                        // may safely alias that inline field.
                        self.escaped.insert(root);
                    }
                }
                self.visit_expr(&sl.expr);
                if let Some(low) = &sl.low {
                    self.visit_expr(low);
                }
                if let Some(high) = &sl.high {
                    self.visit_expr(high);
                }
                if let Some(max) = &sl.max {
                    self.visit_expr(max);
                }
            }

            // 3. FuncLit → enter new func_scope, track captures
            ExprKind::FuncLit(func) => {
                let closure_scope = self.type_info.scopes.get(&func.sig.span).copied();
                let saved_scope = self.func_scope;

                self.func_scope = closure_scope;
                self.closure_stack.push(ClosureEntry {
                    id: expr.id,
                    scope: closure_scope,
                });
                self.closure_captures.insert(expr.id, Vec::new());

                // Collect named returns - they'll be marked as escaped when we encounter defer/errdefer
                let named_returns: Vec<ObjKey> = func
                    .sig
                    .results
                    .iter()
                    .filter_map(|r| r.name.as_ref())
                    .filter_map(|name| self.type_info.defs.get(&name.id).and_then(|o| *o))
                    .collect();
                self.func_context_stack.push(FuncContext { named_returns });

                self.visit_block(&func.body);

                self.func_context_stack.pop();
                self.func_scope = saved_scope;
                self.closure_stack.pop();
            }

            // 4. Variable reference → check if captured by closure
            ExprKind::Ident(ident) => {
                self.visit_ident_use(ident);
            }

            // Recurse into other expressions
            ExprKind::Binary(b) => {
                self.visit_expr(&b.left);
                self.visit_expr(&b.right);
            }
            ExprKind::Unary(u) => self.visit_expr(&u.operand),
            ExprKind::Call(c) => {
                // Method call with pointer receiver on value type → receiver escapes
                if let ExprKind::Selector(sel) = &c.func.kind {
                    self.check_ptr_recv_method_escape(c.func.id, &sel.expr);
                }
                self.visit_expr(&c.func);
                for arg in &c.args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Index(i) => {
                self.visit_expr(&i.expr);
                self.visit_expr(&i.index);
            }
            ExprKind::Selector(s) => {
                // Method value with pointer receiver → receiver escapes
                self.check_ptr_recv_method_escape(expr.id, &s.expr);
                self.visit_expr(&s.expr);
            }
            ExprKind::TypeAssert(t) => self.visit_expr(&t.expr),
            ExprKind::CompositeLit(c) => {
                for elem in &c.elems {
                    if let Some(key) = &elem.key {
                        match key {
                            CompositeLitKey::Expr(expr) => self.visit_expr(expr),
                            CompositeLitKey::Ident(ident)
                                if self.type_info.uses.contains_key(&ident.id) =>
                            {
                                self.visit_ident_use(ident);
                            }
                            CompositeLitKey::Ident(_) => {}
                        }
                    }
                    self.visit_expr(&elem.value);
                }
            }
            ExprKind::Conversion(c) => self.visit_expr(&c.expr),
            ExprKind::Receive(e) => self.visit_expr(e),
            ExprKind::Paren(e) => self.visit_expr(e),
            ExprKind::TryUnwrap(e) => self.visit_expr(e),
            ExprKind::DynAccess(d) => {
                self.visit_expr(&d.base);
                match &d.op {
                    vo_syntax::ast::DynAccessOp::Field(_) => {}
                    vo_syntax::ast::DynAccessOp::Index(idx) => self.visit_expr(idx),
                    vo_syntax::ast::DynAccessOp::Call { args, .. }
                    | vo_syntax::ast::DynAccessOp::MethodCall { args, .. } => {
                        for arg in args {
                            self.visit_expr(arg)
                        }
                    }
                }
            }
            ExprKind::IntLit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::RuneLit(_)
            | ExprKind::StringLit(_)
            | ExprKind::TypeAsExpr(_)
            | ExprKind::Ellipsis => {}
        }
    }

    /// Check if variable is captured by closure.
    /// A variable is captured if it's declared outside the current function scope.
    fn is_captured(&self, obj: ObjKey, func_scope: ScopeKey) -> bool {
        let var_scope = match self.tc_objs.lobjs[obj].parent() {
            Some(s) => s,
            None => return false, // package-level var, not captured
        };

        // Check if var_scope is a function scope (not package-level)
        if !self.is_within_any_func(var_scope) {
            return false; // package-level variable
        }

        // var_scope not within func_scope → captured
        !self.scope_contains(func_scope, var_scope)
    }

    /// Check if scope is within any function (not package-level).
    fn is_within_any_func(&self, scope: ScopeKey) -> bool {
        let mut current = Some(scope);
        while let Some(s) = current {
            if self.tc_objs.scopes[s].is_func() {
                return true;
            }
            current = self.tc_objs.scopes[s].parent();
        }
        false
    }

    /// Check if `inner` scope is contained within `outer` scope (or same).
    fn scope_contains(&self, outer: ScopeKey, inner: ScopeKey) -> bool {
        let mut current = Some(inner);
        while let Some(s) = current {
            if s == outer {
                return true;
            }
            current = self.tc_objs.scopes[s].parent();
        }
        false
    }

    /// Traverse Selector chain to find the root variable.
    fn find_root_var(&self, expr: &Expr) -> Option<ObjKey> {
        match &expr.kind {
            ExprKind::Ident(ident) => self.type_info.uses.get(&ident.id).copied(),
            ExprKind::Selector(sel) => self.find_root_var(&sel.expr),
            ExprKind::Paren(inner) => self.find_root_var(inner),
            ExprKind::Index(idx) => self.find_root_var(&idx.expr),
            _ => None,
        }
    }

    /// Check if the variable is a basic type (int, float, bool) that can be inlined into interface's data slot.
    /// Reference types (slice, map, chan, closure, pointer) are already GcRef, no escape concept.
    /// Only value types (struct, array) need to escape when assigned to interface.
    fn is_basic_type(&self, obj: ObjKey) -> bool {
        if let Some(typ) = self.tc_objs.lobjs[obj].typ() {
            let underlying = typ::underlying_type(typ, self.tc_objs);
            matches!(&self.tc_objs.types[underlying], typ::Type::Basic(_))
        } else {
            false
        }
    }

    /// Check if a method has pointer receiver.
    fn method_has_pointer_receiver(&self, method_obj: ObjKey) -> bool {
        if let Some(typ) = self.tc_objs.lobjs[method_obj].typ() {
            if let Some(sig) = self.tc_objs.types[typ].try_as_signature() {
                if let Some(recv_obj) = sig.recv() {
                    if let Some(recv_typ) = self.tc_objs.lobjs[*recv_obj].typ() {
                        return self.tc_objs.types[recv_typ].try_as_pointer().is_some();
                    }
                }
            }
        }
        false
    }

    /// Check if a type is a pointer type.
    fn is_pointer_type(&self, type_key: crate::objects::TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs);
        self.tc_objs.types[underlying].try_as_pointer().is_some()
    }

    /// Check if selector expr is a pointer-receiver method on value type → mark receiver as escaped.
    /// Used for both method calls (c.Method()) and method values (c.Method).
    fn check_ptr_recv_method_escape(
        &mut self,
        selector_id: vo_syntax::ast::ExprId,
        recv_expr: &Expr,
    ) {
        let Some(selection) = self.type_info.selections.get(&selector_id) else {
            return;
        };
        if !matches!(selection.kind(), SelectionKind::MethodVal) {
            return;
        }
        if !self.method_has_pointer_receiver(selection.obj()) {
            return;
        }
        let Some(recv_type) = self.type_info.types.get(&recv_expr.id) else {
            return;
        };
        if self.is_pointer_type(recv_type.typ) {
            return;
        }
        // Value type with pointer receiver method → escapes
        if let Some(root) = self.find_root_var(recv_expr) {
            self.escaped.insert(root);
        }
    }
}

#[cfg(test)]
mod tests;
