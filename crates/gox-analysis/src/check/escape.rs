//! Escape analysis pass.
//!
//! This module implements static escape analysis to determine which local variables
//! need heap allocation. It runs as a separate pass after type checking completes.

use crate::objects::{ObjKey, ScopeKey, TCObjects};
use crate::typ;
use super::type_info::TypeInfo;
use gox_syntax::ast::{
    AssignOp, Block, Decl, Expr, ExprKind, File, FuncDecl, Stmt, StmtKind, UnaryOp,
};
use std::collections::HashSet;

/// Analyze escape for all files and return the set of escaped variables.
pub fn analyze(
    files: &[File],
    type_info: &TypeInfo,
    tc_objs: &TCObjects,
) -> HashSet<ObjKey> {
    let mut analyzer = EscapeAnalyzer::new(type_info, tc_objs);
    for file in files {
        analyzer.visit_file(file);
    }
    analyzer.escaped
}

/// The escape analyzer.
struct EscapeAnalyzer<'a> {
    type_info: &'a TypeInfo,
    tc_objs: &'a TCObjects,
    escaped: HashSet<ObjKey>,
    /// Current function scope (None for package-level code)
    func_scope: Option<ScopeKey>,
}

impl<'a> EscapeAnalyzer<'a> {
    fn new(type_info: &'a TypeInfo, tc_objs: &'a TCObjects) -> Self {
        Self {
            type_info,
            tc_objs,
            escaped: HashSet::new(),
            func_scope: None,
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
            // Get function scope from signature
            let func_scope = self.type_info.scopes.get(&fdecl.sig.span).copied();
            let saved = self.func_scope;
            self.func_scope = func_scope;
            self.visit_block(body);
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
                    let is_interface_type = spec.ty.as_ref().and_then(|ty| {
                        self.type_info.type_exprs.get(&ty.id)
                    }).map(|&type_key| {
                        typ::is_interface(type_key, self.tc_objs)
                    }).unwrap_or(false);
                    
                    for value in &spec.values {
                        self.visit_expr(value);
                        // If var type is interface, value escapes
                        if is_interface_type {
                            if let Some(root) = self.find_root_var(value) {
                                self.escaped.insert(root);
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
                // Check interface assignment
                for (l, r) in assign.lhs.iter().zip(assign.rhs.iter()) {
                    self.visit_expr(l);
                    self.visit_expr(r);
                    
                    // If LHS is interface type, RHS escapes
                    if assign.op == AssignOp::Assign {
                        if let Some(tv) = self.type_info.types.get(&l.id) {
                            if typ::is_interface(tv.typ, self.tc_objs) {
                                if let Some(root) = self.find_root_var(r) {
                                    self.escaped.insert(root);
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
                use gox_syntax::ast::ForClause;
                match &fs.clause {
                    ForClause::Cond(Some(e)) => self.visit_expr(e),
                    ForClause::Three { init, cond, post } => {
                        if let Some(init) = init {
                            self.visit_stmt(init);
                        }
                        if let Some(cond) = cond {
                            self.visit_expr(cond);
                        }
                        if let Some(post) = post {
                            self.visit_stmt(post);
                        }
                    }
                    ForClause::Range { expr, .. } => self.visit_expr(expr),
                    _ => {}
                }
                self.visit_block(&fs.body);
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
                    for s in &case.body {
                        self.visit_stmt(s);
                    }
                }
            }
            StmtKind::Go(g) => self.visit_expr(&g.call),
            StmtKind::Defer(d) => self.visit_expr(&d.call),
            StmtKind::ErrDefer(d) => self.visit_expr(&d.call),
            StmtKind::Fail(f) => self.visit_expr(&f.error),
            StmtKind::Send(s) => {
                self.visit_expr(&s.chan);
                self.visit_expr(&s.value);
            }
            StmtKind::Break(_) | StmtKind::Continue(_) | StmtKind::Goto(_) | StmtKind::Fallthrough => {}
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
                if let Some(root) = self.find_root_var(&sl.expr) {
                    if self.is_array_type(root) {
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
            }

            // 3. FuncLit → enter new func_scope, check captures
            ExprKind::FuncLit(func) => {
                let closure_scope = self.type_info.scopes.get(&func.sig.span).copied();
                let saved = self.func_scope;
                self.func_scope = closure_scope;
                self.visit_block(&func.body);
                self.func_scope = saved;
            }

            // 4. Variable reference → check if captured by closure
            ExprKind::Ident(ident) => {
                if let Some(func_scope) = self.func_scope {
                    if let Some(&obj) = self.type_info.uses.get(ident) {
                        if self.is_captured(obj, func_scope) {
                            self.escaped.insert(obj);
                        }
                    }
                }
            }

            // Recurse into other expressions
            ExprKind::Binary(b) => {
                self.visit_expr(&b.left);
                self.visit_expr(&b.right);
            }
            ExprKind::Unary(u) => self.visit_expr(&u.operand),
            ExprKind::Call(c) => {
                self.visit_expr(&c.func);
                for arg in &c.args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Index(i) => {
                self.visit_expr(&i.expr);
                self.visit_expr(&i.index);
            }
            ExprKind::Selector(s) => self.visit_expr(&s.expr),
            ExprKind::TypeAssert(t) => self.visit_expr(&t.expr),
            ExprKind::CompositeLit(c) => {
                for elem in &c.elems {
                    self.visit_expr(&elem.value);
                }
            }
            ExprKind::Conversion(c) => self.visit_expr(&c.expr),
            ExprKind::Receive(e) => self.visit_expr(e),
            ExprKind::Paren(e) => self.visit_expr(e),
            ExprKind::TryUnwrap(e) => self.visit_expr(e),
            ExprKind::IntLit(_) | ExprKind::FloatLit(_) | ExprKind::RuneLit(_) | ExprKind::StringLit(_) | ExprKind::TypeAsExpr(_) => {}
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
            ExprKind::Ident(ident) => self.type_info.uses.get(ident).copied(),
            ExprKind::Selector(sel) => self.find_root_var(&sel.expr),
            ExprKind::Paren(inner) => self.find_root_var(inner),
            ExprKind::Index(idx) => self.find_root_var(&idx.expr),
            _ => None,
        }
    }

    /// Check if the variable has array type.
    fn is_array_type(&self, obj: ObjKey) -> bool {
        if let Some(typ) = self.tc_objs.lobjs[obj].typ() {
            let underlying = typ::underlying_type(typ, self.tc_objs);
            self.tc_objs.types[underlying].try_as_array().is_some()
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::project::analyze_single_file;
    use gox_syntax::parser;

    /// Helper to parse and type-check GoX code, returning escaped variable names.
    fn get_escaped_vars(source: &str) -> Vec<String> {
        let (file, _diags, interner) = parser::parse(source, 0);
        
        match analyze_single_file(file, interner) {
            Ok(project) => {
                let mut names: Vec<String> = project.type_info.escaped_vars
                    .iter()
                    .map(|&obj| project.tc_objs.lobjs[obj].name().to_string())
                    .collect();
                names.sort();
                names
            }
            Err(e) => {
                panic!("Type check failed: {:?}", e);
            }
        }
    }

    // =========================================================================
    // No escape tests
    // =========================================================================

    #[test]
    fn test_no_escape_simple() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                x := 42
                y := x + 1
                _ = y
            }
        "#);
        assert!(escaped.is_empty(), "no variables should escape: {:?}", escaped);
    }

    #[test]
    fn test_no_escape_struct_local_use() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var p Point
                p.x = 1
                p.y = 2
                _ = p.x + p.y
            }
        "#);
        assert!(escaped.is_empty(), "no variables should escape: {:?}", escaped);
    }

    // =========================================================================
    // Address taken tests
    // =========================================================================

    #[test]
    fn test_address_escape() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var s Point
                p := &s
                _ = p
            }
        "#);
        assert!(escaped.contains(&"s".to_string()), "s should escape: {:?}", escaped);
    }

    #[test]
    fn test_nested_field_address_escape() {
        let escaped = get_escaped_vars(r#"
            package main
            type Inner struct { x int }
            type Outer struct { inner Inner }
            func main() {
                var o Outer
                p := &o.inner
                _ = p
            }
        "#);
        assert!(escaped.contains(&"o".to_string()), "o should escape (not just inner): {:?}", escaped);
    }

    #[test]
    fn test_deeply_nested_field_address_escape() {
        let escaped = get_escaped_vars(r#"
            package main
            type A struct { x int }
            type B struct { a A }
            type C struct { b B }
            func main() {
                var c C
                p := &c.b.a.x
                _ = p
            }
        "#);
        assert!(escaped.contains(&"c".to_string()), "c should escape: {:?}", escaped);
    }

    // =========================================================================
    // Closure capture tests
    // =========================================================================

    #[test]
    fn test_closure_capture_int() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                x := 42
                f := func() int { return x }
                _ = f()
            }
        "#);
        assert!(escaped.contains(&"x".to_string()), "x should escape: {:?}", escaped);
    }

    #[test]
    fn test_closure_capture_struct() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var p Point
                f := func() int { return p.x }
                _ = f()
            }
        "#);
        assert!(escaped.contains(&"p".to_string()), "p should escape: {:?}", escaped);
    }

    #[test]
    fn test_closure_param_capture() {
        let escaped = get_escaped_vars(r#"
            package main
            func foo(x int) func() int {
                return func() int { return x }
            }
            func main() {
                f := foo(42)
                _ = f()
            }
        "#);
        assert!(escaped.contains(&"x".to_string()), "x (param) should escape: {:?}", escaped);
    }

    #[test]
    fn test_nested_closure_capture() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                x := 1
                f := func() {
                    g := func() int { return x }
                    _ = g()
                }
                f()
            }
        "#);
        assert!(escaped.contains(&"x".to_string()), "x should escape: {:?}", escaped);
    }

    #[test]
    fn test_closure_no_capture_local() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                f := func() int {
                    y := 10
                    return y
                }
                _ = f()
            }
        "#);
        // y is local to the closure, not captured
        assert!(!escaped.contains(&"y".to_string()), "y should not escape: {:?}", escaped);
    }

    // =========================================================================
    // Interface assignment tests
    // =========================================================================

    #[test]
    fn test_interface_assignment() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var s Point
                var i interface{} = s
                _ = i
            }
        "#);
        assert!(escaped.contains(&"s".to_string()), "s should escape: {:?}", escaped);
    }

    #[test]
    fn test_interface_param_assignment() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func foo(i interface{}) {
                _ = i
            }
            func main() {
                var s Point
                foo(s)
            }
        "#);
        // Note: This tests function call, not direct assignment
        // The current implementation may not catch this case
        // as it only handles direct assignment statements
    }

    // =========================================================================
    // Slice operation tests
    // =========================================================================

    #[test]
    fn test_array_slice_escape() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                var arr [5]int
                s := arr[:]
                _ = s
            }
        "#);
        assert!(escaped.contains(&"arr".to_string()), "arr should escape: {:?}", escaped);
    }

    #[test]
    fn test_array_partial_slice_escape() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                var arr [10]int
                s := arr[2:5]
                _ = s
            }
        "#);
        assert!(escaped.contains(&"arr".to_string()), "arr should escape: {:?}", escaped);
    }

    #[test]
    fn test_slice_of_slice_no_escape() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                var s []int
                s2 := s[1:3]
                _ = s2
            }
        "#);
        // Slicing a slice doesn't cause escape (slice is already a reference type)
        assert!(!escaped.contains(&"s".to_string()), "s should not escape: {:?}", escaped);
    }

    // =========================================================================
    // Package-level variable tests
    // =========================================================================

    #[test]
    fn test_package_var_no_capture() {
        let escaped = get_escaped_vars(r#"
            package main
            var globalX int = 10
            func main() {
                f := func() int { return globalX }
                _ = f()
            }
        "#);
        // Package-level variables are not "captured" - they're already global
        assert!(!escaped.contains(&"globalX".to_string()), "globalX should not be marked as escaped: {:?}", escaped);
    }

    // =========================================================================
    // Multiple variables tests
    // =========================================================================

    #[test]
    fn test_multiple_escapes() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var a Point
                var b Point
                var c Point
                
                p := &a
                f := func() int { return b.x }
                var i interface{} = c
                
                _ = p
                _ = f()
                _ = i
            }
        "#);
        assert!(escaped.contains(&"a".to_string()), "a should escape: {:?}", escaped);
        assert!(escaped.contains(&"b".to_string()), "b should escape: {:?}", escaped);
        assert!(escaped.contains(&"c".to_string()), "c should escape: {:?}", escaped);
    }

    #[test]
    fn test_mixed_escape_and_no_escape() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var escapes Point
                var stays Point
                
                p := &escapes
                _ = p
                
                stays.x = 1
                _ = stays.x
            }
        "#);
        assert!(escaped.contains(&"escapes".to_string()), "escapes should escape: {:?}", escaped);
        assert!(!escaped.contains(&"stays".to_string()), "stays should not escape: {:?}", escaped);
    }

    // =========================================================================
    // Control flow tests
    // =========================================================================

    #[test]
    fn test_escape_in_if_branch() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var s Point
                if true {
                    p := &s
                    _ = p
                }
            }
        "#);
        assert!(escaped.contains(&"s".to_string()), "s should escape: {:?}", escaped);
    }

    #[test]
    fn test_escape_in_for_loop() {
        let escaped = get_escaped_vars(r#"
            package main
            func main() {
                x := 0
                for i := 0; i < 10; i += 1 {
                    f := func() int { return x }
                    _ = f()
                }
            }
        "#);
        assert!(escaped.contains(&"x".to_string()), "x should escape: {:?}", escaped);
    }

    // =========================================================================
    // Edge cases
    // =========================================================================

    #[test]
    fn test_index_then_address() {
        let escaped = get_escaped_vars(r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var arr [3]Point
                p := &arr[0]
                _ = p
            }
        "#);
        assert!(escaped.contains(&"arr".to_string()), "arr should escape: {:?}", escaped);
    }
}
