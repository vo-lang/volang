//! Statement type checking.
//!
//! This module handles type checking for all statement forms:
//! - Block statements
//! - Variable declarations  
//! - Assignment statements
//! - Short variable declarations
//! - Return statements
//! - Control flow (if, for, switch, select)
//! - Go and defer statements
//! - Channel operations

use gox_common::{Span, Symbol};
use gox_syntax::ast::{self, Expr, ExprKind, Stmt, StmtKind};

use crate::errors::TypeError;
use crate::scope::{Entity, ScopeKind};
use crate::types::{BasicType, NamedTypeId, NamedTypeInfo, Type};

use super::{TypeChecker, LOCAL_TYPE_ID_OFFSET};

impl<'a> TypeChecker<'a> {
    /// Type-checks a statement.
    pub fn check_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Empty => {}
            StmtKind::Block(block) => self.check_block(block),
            StmtKind::Expr(expr) => {
                self.check_expr(expr);
            }
            StmtKind::Assign(assign) => self.check_assign(assign),
            StmtKind::ShortVar(sv) => self.check_short_var(sv),
            StmtKind::Return(ret) => self.check_return(ret),
            StmtKind::If(if_stmt) => self.check_if(if_stmt),
            StmtKind::For(for_stmt) => self.check_for(for_stmt),
            StmtKind::Switch(switch) => self.check_switch(switch),
            StmtKind::IncDec(inc_dec) => self.check_inc_dec(inc_dec),
            StmtKind::Send(send) => self.check_send(send),
            StmtKind::Go(go) => self.check_go(go),
            StmtKind::Defer(defer) => self.check_defer(defer),
            StmtKind::ErrDefer(errdefer) => self.check_errdefer(errdefer),
            StmtKind::Fail(fail) => self.check_fail(fail),
            StmtKind::Select(sel) => self.check_select(sel),
            StmtKind::Labeled(labeled) => self.check_stmt(&labeled.stmt),
            StmtKind::Var(var_decl) => self.check_var_decl(var_decl),
            StmtKind::Const(_) => {
                // Constants already handled in Phase 1
            }
            StmtKind::Type(type_decl) => self.check_type_decl_stmt(type_decl),
            StmtKind::Break(_) | StmtKind::Continue(_) | StmtKind::Goto(_) | StmtKind::Fallthrough => {
                // Control flow - no type checking needed
            }
            StmtKind::TypeSwitch(ts) => self.check_type_switch(ts),
        }
    }

    /// Checks a block of statements.
    pub(crate) fn check_block(&mut self, block: &ast::Block) {
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
    }

    // ========== Variable declarations ==========

    /// Checks a variable declaration statement.
    fn check_var_decl(&mut self, decl: &ast::VarDecl) {
        for spec in &decl.specs {
            // Determine the declared type (if any)
            let declared_ty = spec.ty.as_ref().map(|t| self.resolve_type_expr(t));

            // Check initializer values
            let value_types: Vec<Type> = spec.values.iter()
                .map(|v| self.check_expr(v))
                .collect();

            // Validate and define variables
            for (i, name) in spec.names.iter().enumerate() {
                let var_ty = if let Some(ref ty) = declared_ty {
                    // Type is declared - check initializer compatibility
                    if i < value_types.len()
                        && !self.is_assignable(&value_types[i], ty) {
                            self.error(TypeError::VarInitTypeMismatch, name.span);
                        }
                    ty.clone()
                } else if i < value_types.len() {
                    // No declared type - infer from initializer
                    self.default_type(&value_types[i])
                } else {
                    // No type and no initializer - error
                    self.error(TypeError::VarInitTypeMismatch, name.span);
                    Type::Invalid
                };

                self.define_var(name.symbol, var_ty, name.span);
            }
        }
    }

    /// Checks a local type declaration statement.
    fn check_type_decl_stmt(&mut self, decl: &ast::TypeDecl) {
        // Resolve the underlying type
        let underlying = self.resolve_type_expr(&decl.ty);
        
        // Create a local type ID (offset from package-level types)
        let local_idx = self.local_types.len() as u32;
        let id = NamedTypeId(LOCAL_TYPE_ID_OFFSET + local_idx);
        
        // Store the local type info
        self.local_types.push(NamedTypeInfo {
            name: decl.name.symbol,
            underlying: underlying.clone(),
            methods: Vec::new(),
        });
        
        // Add to local scope
        if let Some(ref mut scope) = self.local_scope {
            scope.insert(
                decl.name.symbol,
                Entity::Type(crate::scope::TypeEntity {
                    id,
                    span: decl.name.span,
                }),
            );
        }
    }

    // ========== Assignment statements ==========

    /// Checks if an expression is the blank identifier.
    fn is_blank_ident(&self, expr: &Expr) -> bool {
        if let ExprKind::Ident(ident) = &expr.kind {
            let name = self.interner.resolve(ident.symbol).unwrap_or("");
            return name == "_";
        }
        false
    }

    /// Checks an assignment statement.
    fn check_assign(&mut self, assign: &ast::AssignStmt) {
        let lhs_types: Vec<Type> = assign.lhs.iter().map(|e| self.check_expr(e)).collect();
        let rhs_types: Vec<Type> = assign.rhs.iter().map(|e| self.check_expr(e)).collect();

        // Check counts match
        if lhs_types.len() != rhs_types.len() {
            // Special case: multi-value function call
            if rhs_types.len() == 1 {
                if let Type::Tuple(results) = &rhs_types[0] {
                    if results.len() == lhs_types.len() {
                        // Check each result type
                        for (i, (lhs, rhs)) in lhs_types.iter().zip(results.iter()).enumerate() {
                            if !self.is_assignable(rhs, lhs) {
                                self.error_msg(TypeError::AssignTypeMismatch, assign.lhs[i].span, format!(
                                    "cannot assign value {} to variable",
                                    i + 1
                                ));
                            }
                        }
                        return;
                    }
                }
            }
            self.error_msg(TypeError::AssignCountMismatch, assign.lhs[0].span, format!(
                "assignment mismatch: {} variables but {} values",
                lhs_types.len(),
                rhs_types.len()
            ));
            return;
        }

        // Check type compatibility (skip blank identifiers)
        for (i, ((lhs_expr, lhs_ty), rhs)) in assign.lhs.iter().zip(lhs_types.iter()).zip(rhs_types.iter()).enumerate() {
            // Skip blank identifier - it accepts any value
            if self.is_blank_ident(lhs_expr) {
                continue;
            }
            if !self.is_assignable(rhs, lhs_ty) {
                self.error_msg(TypeError::AssignTypeMismatch, lhs_expr.span, format!(
                    "cannot assign to variable {}",
                    i + 1
                ));
            }
        }

        // For compound assignment, check operator is valid
        if assign.op != ast::AssignOp::Assign
            && lhs_types.len() != 1 {
                self.error(TypeError::AssignCountMismatch, assign.lhs[0].span);
            }
    }

    /// Checks a short variable declaration.
    fn check_short_var(&mut self, sv: &ast::ShortVarDecl) {
        let rhs_types: Vec<Type> = sv.values.iter().map(|e| self.check_expr(e)).collect();

        // Handle multi-value function call
        let var_types: Vec<Type> = if sv.names.len() != rhs_types.len() && rhs_types.len() == 1 {
            if let Type::Tuple(results) = &rhs_types[0] {
                if results.len() == sv.names.len() {
                    results.clone()
                } else {
                    self.error_msg(TypeError::AssignCountMismatch, sv.names[0].span, format!(
                        "assignment mismatch: {} variables but {} values",
                        sv.names.len(),
                        results.len()
                    ));
                    return;
                }
            } else {
                self.error_msg(TypeError::AssignCountMismatch, sv.names[0].span, format!(
                    "assignment mismatch: {} variables but {} values",
                    sv.names.len(),
                    rhs_types.len()
                ));
                return;
            }
        } else if sv.names.len() != rhs_types.len() {
            self.error_msg(TypeError::AssignCountMismatch, sv.names[0].span, format!(
                "assignment mismatch: {} variables but {} values",
                sv.names.len(),
                rhs_types.len()
            ));
            return;
        } else {
            rhs_types
        };

        // Check for duplicate names in the same := statement (excluding blank identifier).
        let mut seen: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        for name in &sv.names {
            let name_str = self.interner.resolve(name.symbol).unwrap_or("");
            if name_str == "_" {
                continue;
            }
            if !seen.insert(name.symbol) {
                self.error_msg(
                    TypeError::Redeclared,
                    name.span,
                    format!("{} repeated on left side of :=", name_str),
                );
                return;
            }
        }

        // First pass: determine if there is at least one new variable (blank identifier doesn't count).
        let mut has_new = false;
        for name in &sv.names {
            let name_str = self.interner.resolve(name.symbol).unwrap_or("");
            if name_str == "_" {
                continue;
            }
            let is_new = if let Some(ref scope) = self.local_scope {
                !scope.contains_local(name.symbol)
            } else {
                true
            };
            if is_new {
                has_new = true;
                break;
            }
        }

        // If there are no new variables, report and stop (avoid cascading extra errors).
        if !has_new {
            self.error(TypeError::NoNewVarsInShortDecl, sv.names[0].span);
            return;
        }

        // Second pass: define new vars, and type-check redeclared vars as assignments.
        for (name, ty) in sv.names.iter().zip(var_types.iter()) {
            let name_str = self.interner.resolve(name.symbol).unwrap_or("");
            if name_str == "_" {
                continue;
            }

            let is_new = if let Some(ref scope) = self.local_scope {
                !scope.contains_local(name.symbol)
            } else {
                true
            };

            if is_new {
                let var_ty = self.default_type(ty);
                self.define_var(name.symbol, var_ty, name.span);
            } else {
                let Some(ref local) = self.local_scope else {
                    continue;
                };
                let Some(entity) = local.lookup_local(name.symbol) else {
                    continue;
                };
                match entity {
                    Entity::Var(v) => {
                        if !self.is_assignable(ty, &v.ty) {
                            self.error_msg(
                                TypeError::AssignTypeMismatch,
                                name.span,
                                format!("cannot assign to {}", name_str),
                            );
                        }
                    }
                    _ => {
                        self.error_msg(
                            TypeError::Redeclared,
                            name.span,
                            TypeError::Redeclared.with_name(name_str),
                        );
                    }
                }
            }
        }
    }

    // ========== Return statement ==========

    /// Checks a return statement.
    fn check_return(&mut self, ret: &ast::ReturnStmt) {
        let actual_types: Vec<Type> = ret.values.iter().map(|e| self.check_expr(e)).collect();
        let return_types = self.return_types.clone();
        let value_spans: Vec<Span> = ret.values.iter().map(|v| v.span).collect();

        // Check return count matches expected
        if actual_types.len() != return_types.len() {
            // Special case: single multi-value expression
            if actual_types.len() == 1 {
                if let Type::Tuple(results) = &actual_types[0] {
                    if results.len() == return_types.len() {
                        // Check each result type
                        for (i, (actual, expected)) in results.iter().zip(return_types.iter()).enumerate() {
                            if !self.is_assignable(actual, expected) {
                                self.error_msg(TypeError::ReturnTypeMismatch, value_spans[0], format!(
                                    "cannot use value as return value {} in return statement",
                                    i + 1
                                ));
                            }
                        }
                        return;
                    }
                }
            }
            // Bare return is allowed if function has named returns
            if actual_types.is_empty() && !return_types.is_empty() && self.has_named_returns {
                return;
            }
            let span = value_spans.first().copied().unwrap_or(Span::dummy());
            self.error_msg(TypeError::WrongReturnCount, span, format!(
                "wrong number of return values: have {}, want {}",
                actual_types.len(),
                return_types.len()
            ));
            return;
        }

        // Check each return value type
        for (i, (actual, expected)) in actual_types.iter().zip(return_types.iter()).enumerate() {
            if !self.is_assignable(actual, expected) {
                self.error_msg(TypeError::ReturnTypeMismatch, value_spans[i], format!(
                    "cannot use type as return value {} in return statement",
                    i + 1
                ));
            }
        }
    }

    // ========== Control flow statements ==========

    /// Checks an if statement.
    fn check_if(&mut self, if_stmt: &ast::IfStmt) {
        // Push block scope for if statement (init vars are scoped to if)
        self.push_scope(ScopeKind::Block);

        // Check init statement if present
        if let Some(ref init) = if_stmt.init {
            self.check_stmt(init);
        }

        // Check condition is boolean
        let cond_ty = self.check_expr(&if_stmt.cond);
        if !self.is_bool_type(&cond_ty) {
            self.error(TypeError::NonBoolCondition, if_stmt.cond.span);
        }

        // Check then block
        self.check_block(&if_stmt.then);

        // Check else branch
        if let Some(ref else_) = if_stmt.else_ {
            self.check_stmt(else_);
        }

        self.pop_scope();
    }

    /// Checks a for statement.
    fn check_for(&mut self, for_stmt: &ast::ForStmt) {
        // Push block scope for for statement
        self.push_scope(ScopeKind::Block);

        match &for_stmt.clause {
            ast::ForClause::Cond(Some(cond)) => {
                let cond_ty = self.check_expr(cond);
                if !self.is_bool_type(&cond_ty) {
                    self.error(TypeError::NonBoolCondition, cond.span);
                }
            }
            ast::ForClause::Cond(None) => {
                // Infinite loop - OK
            }
            ast::ForClause::Three { init, cond, post } => {
                if let Some(ref init) = init {
                    self.check_stmt(init);
                }
                if let Some(ref cond) = cond {
                    let cond_ty = self.check_expr(cond);
                    if !self.is_bool_type(&cond_ty) {
                        self.error(TypeError::NonBoolCondition, cond.span);
                    }
                }
                if let Some(ref post) = post {
                    self.check_stmt(post);
                }
            }
            ast::ForClause::Range { key, value, expr, define } => {
                let range_ty = self.check_expr(expr);
                // Validate range expression type and define loop variables
                let (key_ty, value_ty) = match self.underlying_type(&range_ty) {
                    Type::Array(arr) => (Type::Basic(BasicType::Int), (*arr.elem).clone()),
                    Type::Slice(sl) => (Type::Basic(BasicType::Int), (*sl.elem).clone()),
                    Type::Map(m) => ((*m.key).clone(), (*m.value).clone()),
                    Type::Chan(c) => ((*c.elem).clone(), Type::Invalid),
                    Type::Basic(BasicType::String) => (Type::Basic(BasicType::Int), Type::Basic(BasicType::Int32)),
                    _ => {
                        self.error(TypeError::NotIterable, expr.span);
                        (Type::Invalid, Type::Invalid)
                    }
                };

                // Define loop variables if this is a := declaration
                if *define {
                    if let Some(ref k) = key {
                        let name = self.interner.resolve(k.symbol).unwrap_or("");
                        if name != "_" {
                            self.define_var(k.symbol, key_ty, k.span);
                        }
                    }
                    if let Some(ref v) = value {
                        let name = self.interner.resolve(v.symbol).unwrap_or("");
                        if name != "_" {
                            self.define_var(v.symbol, value_ty, v.span);
                        }
                    }
                }
            }
        }

        // Check body
        self.check_block(&for_stmt.body);

        self.pop_scope();
    }

    /// Checks a switch statement.
    fn check_switch(&mut self, switch: &ast::SwitchStmt) {
        // Check init statement
        if let Some(ref init) = switch.init {
            self.check_stmt(init);
        }

        // Check tag expression
        let tag_ty = if let Some(ref tag) = switch.tag {
            self.check_expr(tag)
        } else {
            Type::Basic(BasicType::Bool)
        };

        // Check case clauses
        for case in &switch.cases {
            for expr in &case.exprs {
                let case_ty = self.check_expr(expr);
                if !self.are_comparable(&tag_ty, &case_ty) {
                    self.error(TypeError::NotComparable, expr.span);
                }
            }
            for stmt in &case.body {
                self.check_stmt(stmt);
            }
        }
    }

    /// Checks a type switch statement.
    fn check_type_switch(&mut self, ts: &ast::TypeSwitchStmt) {
        // Push scope for type switch
        self.push_scope(ScopeKind::Block);

        if let Some(ref init) = ts.init {
            self.check_stmt(init);
        }

        let expr_ty = self.check_expr(&ts.expr);
        if !self.is_interface_type(&expr_ty) {
            self.error(TypeError::TypeAssertNonInterface, ts.expr.span);
        }

        for case in &ts.cases {
            // Push scope for each case
            self.push_scope(ScopeKind::Block);

            // If there's an assigned variable and a single type, bind it
            if let Some(ref assign) = ts.assign {
                if case.types.len() == 1 {
                    if let Some(Some(ref ty_expr)) = case.types.first() {
                        let case_ty = self.resolve_type_expr(ty_expr);
                        self.define_var(assign.symbol, case_ty, assign.span);
                    }
                } else if case.types.is_empty() {
                    // Default case - variable has the interface type
                    self.define_var(assign.symbol, expr_ty.clone(), assign.span);
                } else {
                    // Multiple types - variable has the interface type
                    self.define_var(assign.symbol, expr_ty.clone(), assign.span);
                }
            }

            for stmt in &case.body {
                self.check_stmt(stmt);
            }

            self.pop_scope();
        }

        self.pop_scope();
    }

    // ========== Increment/Decrement ==========

    /// Checks an increment/decrement statement.
    fn check_inc_dec(&mut self, inc_dec: &ast::IncDecStmt) {
        let ty = self.check_expr(&inc_dec.expr);
        if !self.is_numeric_type(&ty) {
            self.error(TypeError::NumericOperandRequired, inc_dec.expr.span);
        }
    }

    // ========== Channel operations ==========

    /// Checks a send statement.
    fn check_send(&mut self, send: &ast::SendStmt) {
        let chan_ty = self.check_expr(&send.chan);
        let value_ty = self.check_expr(&send.value);

        match self.underlying_type(&chan_ty) {
            Type::Chan(c) => {
                if c.dir == crate::types::ChanDir::RecvOnly {
                    self.error(TypeError::SendOnReceiveOnly, send.chan.span);
                }
                if !self.is_assignable(&value_ty, &c.elem) {
                    self.error(TypeError::SendTypeMismatch, send.value.span);
                }
            }
            _ => {
                self.error(TypeError::ReceiveNonChannel, send.chan.span);
            }
        }
    }

    // ========== Go and defer ==========

    /// Checks a go statement.
    fn check_go(&mut self, go: &ast::GoStmt) {
        let _ty = self.check_expr(&go.call);
        // go statement requires a function call
        if !matches!(&go.call.kind, ExprKind::Call(_)) {
            self.error(TypeError::NotCallable, go.call.span);
        }
    }

    /// Checks a defer statement.
    fn check_defer(&mut self, defer: &ast::DeferStmt) {
        let _ty = self.check_expr(&defer.call);
        // defer statement requires a function call
        if !matches!(&defer.call.kind, ExprKind::Call(_)) {
            self.error(TypeError::NotCallable, defer.call.span);
        }
    }

    /// Checks an errdefer statement.
    fn check_errdefer(&mut self, errdefer: &ast::ErrDeferStmt) {
        // errdefer is only valid in functions that return error
        if !self.is_fallible_function() {
            self.error(TypeError::ErrDeferOutsideFallible, errdefer.call.span);
        }
        let _ty = self.check_expr(&errdefer.call);
        // errdefer statement requires a function call
        if !matches!(&errdefer.call.kind, ExprKind::Call(_)) {
            self.error(TypeError::NotCallable, errdefer.call.span);
        }
    }

    /// Checks a fail statement.
    fn check_fail(&mut self, fail: &ast::FailStmt) {
        // fail is only valid in functions that return error
        if !self.is_fallible_function() {
            self.error(TypeError::FailOutsideFallible, fail.error.span);
            return;
        }
        let error_ty = self.check_expr(&fail.error);
        // The error expression must be assignable to error type
        if !self.is_error_type(&error_ty) {
            self.error(TypeError::FailNonError, fail.error.span);
        }
    }

    // ========== Select statement ==========

    /// Checks a select statement.
    fn check_select(&mut self, sel: &ast::SelectStmt) {
        for case in &sel.cases {
            // Each case has its own scope for variables declared in comm clause
            self.push_scope(ScopeKind::Block);
            
            if let Some(ref comm) = case.comm {
                self.check_comm_clause(comm);
            }
            for stmt in &case.body {
                self.check_stmt(stmt);
            }
            
            self.pop_scope();
        }
    }

    /// Checks a communication clause in a select case.
    fn check_comm_clause(&mut self, comm: &ast::CommClause) {
        match comm {
            ast::CommClause::Send(send) => self.check_send(send),
            ast::CommClause::Recv(recv) => {
                let chan_ty = self.check_expr(&recv.expr);
                let elem_ty = match self.underlying_type(&chan_ty) {
                    Type::Chan(c) => {
                        if c.dir == crate::types::ChanDir::SendOnly {
                            self.error(TypeError::ReceiveFromSendOnly, recv.expr.span);
                        }
                        (*c.elem).clone()
                    }
                    _ => {
                        self.error(TypeError::ReceiveNonChannel, recv.expr.span);
                        Type::Invalid
                    }
                };
                
                // Handle variable declarations: v := <-ch or v, ok := <-ch
                if !recv.lhs.is_empty() {
                    if recv.define {
                        // Short variable declaration
                        for (i, ident) in recv.lhs.iter().enumerate() {
                            let ty = if i == 0 {
                                elem_ty.clone()
                            } else {
                                // Second variable is the 'ok' bool
                                Type::Basic(BasicType::Bool)
                            };
                            self.define_var(ident.symbol, ty, ident.span);
                        }
                    } else {
                        // Assignment - check that variables exist and types match
                        for (i, ident) in recv.lhs.iter().enumerate() {
                            let expected_ty = if i == 0 {
                                elem_ty.clone()
                            } else {
                                Type::Basic(BasicType::Bool)
                            };
                            if let Some(Entity::Var(var)) = self.lookup(ident.symbol) {
                                if !self.is_assignable(&expected_ty, &var.ty) {
                                    self.error(TypeError::ArgTypeMismatch, ident.span);
                                }
                            } else {
                                self.error(TypeError::Undefined, ident.span);
                            }
                        }
                    }
                }
            }
        }
    }
}

