//! Statement type checking.
//!
//! This module type-checks statements, handling control flow,
//! scope management, and statement-specific semantics.


use std::collections::HashMap;

use gox_common::span::Span;
use gox_common::symbol::Ident;
use gox_syntax::ast::{AssignOp, Block, Expr, ForClause, Stmt, StmtKind};
use ordered_float::OrderedFloat;

use crate::constant::Value;
use crate::objects::{ScopeKey, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::typ::{self, BasicType, ChanDir};

use super::checker::{Checker, ObjContext};

// =============================================================================
// GoVal - for switch case duplicate detection
// =============================================================================

type F64 = OrderedFloat<f64>;

/// Value type for switch case duplicate detection.
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
enum GoVal {
    Int64(i64),
    Uint64(u64),
    Float64(F64),
    Str(String),
    Invalid,
}

impl GoVal {
    fn from_const(v: &Value) -> GoVal {
        match v {
            Value::Int(i) => {
                if let Some(val) = i.to_i64() {
                    GoVal::Int64(val)
                } else if let Some(val) = i.to_u64() {
                    GoVal::Uint64(val)
                } else {
                    GoVal::Invalid
                }
            }
            Value::Float(f) => GoVal::Float64(OrderedFloat(*f)),
            Value::Rat(r) => {
                if let Some(f) = r.to_f64() {
                    GoVal::Float64(OrderedFloat(f))
                } else {
                    GoVal::Invalid
                }
            }
            Value::Str(s) => GoVal::Str(s.clone()),
            _ => GoVal::Invalid,
        }
    }
}

/// Position and type for duplicate case detection.
struct PosType {
    span: Span,
    typ: TypeKey,
}

/// Map from case value to positions where it appears.
type ValueMap = HashMap<GoVal, Vec<PosType>>;

use num_traits::ToPrimitive;

// =============================================================================
// StmtContext - control flow context for statement checking
// =============================================================================

/// Context for statement checking, tracking control flow validity.
#[derive(Clone, Copy, Debug, Default)]
pub struct StmtContext {
    /// Whether `break` is allowed.
    pub break_ok: bool,
    /// Whether `continue` is allowed.
    pub continue_ok: bool,
    /// Whether `fallthrough` is allowed.
    pub fallthrough_ok: bool,
    /// Whether this is the final case in a switch.
    pub final_switch_case: bool,
}

impl StmtContext {
    pub fn new() -> StmtContext {
        StmtContext::default()
    }
}

// =============================================================================
// Checker statement methods
// =============================================================================

impl Checker {
    // =========================================================================
    // Scope management
    // =========================================================================

    /// Opens a new scope for a statement.
    fn open_scope(&mut self, span: Span, comment: &str) {
        let pos = span.start.to_usize();
        let scope = self.new_scope(
            self.octx.scope,
            pos,
            span.end.to_usize(),
            comment,
            false,
        );
        self.result.record_stmt_scope(pos, scope);
        self.octx.scope = Some(scope);
    }

    /// Closes the current scope, restoring the parent scope.
    fn close_scope(&mut self) {
        if let Some(current) = self.octx.scope {
            self.octx.scope = self.scope(current).parent();
        }
    }

    // =========================================================================
    // Function body checking
    // =========================================================================

    /// Type-checks a function body.
    pub fn func_body(
        &mut self,
        di: Option<crate::objects::DeclInfoKey>,
        _name: &str,
        sig: TypeKey,
        body: &Block,
        iota: Option<Value>,
    ) {
        let (pos, end) = (body.span.start.to_usize(), body.span.end.to_usize());

        // Set function scope extent
        let scope_key = self.otype(sig).try_as_signature().unwrap().scope().unwrap();
        {
            let scope = self.scope_mut(scope_key);
            scope.set_pos(pos);
            scope.set_end(end);
        }

        // Save and set up new object context
        let mut octx = ObjContext::new();
        octx.decl = di;
        octx.scope = Some(scope_key);
        octx.iota = iota;
        octx.sig = Some(sig);
        std::mem::swap(&mut self.octx, &mut octx);

        // Check the function body
        let sctx = StmtContext::new();
        self.stmt_list(&body.stmts, &sctx);

        // Check labels if any
        if self.octx.has_label {
            self.labels(body);
        }

        // Check for missing return
        let sig_val = self.otype(sig).try_as_signature().unwrap();
        if sig_val.results_count(self.objs()) > 0 && !self.is_terminating_block(body) {
            self.error_str(end, "missing return");
        }

        // Check for unused variables
        self.usage(scope_key);

        // Restore object context
        std::mem::swap(&mut self.octx, &mut octx);
    }

    /// Checks for unused variables in the given scope and its children.
    fn usage(&self, skey: ScopeKey) {
        use crate::obj::EntityType;

        let scope = &self.scope(skey);
        let mut unused: Vec<(usize, String)> = scope
            .elems()
            .iter()
            .filter_map(|(_, &okey)| {
                let lobj = &self.lobj(okey);
                match lobj.entity_type() {
                    EntityType::Var(var) => {
                        if !var.used {
                            Some((lobj.pos(), lobj.name().to_string()))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            })
            .collect();
        unused.sort_by(|a, b| a.0.cmp(&b.0));

        for (pos, name) in unused {
            self.error_str(pos, &format!("{} declared but not used", name));
        }

        // Recursively check children scopes (but not function literal scopes)
        for &child_key in scope.children() {
            if !self.scope(child_key).is_func() {
                self.usage(child_key);
            }
        }
    }

    /// Check labels in a block.
    fn labels(&mut self, body: &Block) {
        self.check_labels(body);
    }

    /// Check if a block is terminating.
    fn is_terminating_block(&self, block: &Block) -> bool {
        self.is_terminating_list(&block.stmts, None)
    }

    // =========================================================================
    // Switch/Select case helpers
    // =========================================================================

    /// Checks for multiple default cases in a switch/select.
    fn multiple_defaults(&self, cases: &[gox_syntax::ast::CaseClause]) {
        let mut first_default: Option<Span> = None;
        for case in cases {
            if case.exprs.is_empty() {
                // This is a default case
                if let Some(first_span) = first_default {
                    self.error(
                        case.span,
                        format!("multiple defaults (first at {})", first_span.start.to_usize()),
                    );
                } else {
                    first_default = Some(case.span);
                }
            }
        }
    }

    /// Checks for multiple default cases in a type switch.
    fn multiple_defaults_type(&self, cases: &[gox_syntax::ast::TypeCaseClause]) {
        let mut first_default: Option<Span> = None;
        for case in cases {
            if case.types.is_empty() {
                // This is a default case
                if let Some(first_span) = first_default {
                    self.error(
                        case.span,
                        format!("multiple defaults (first at {})", first_span.start.to_usize()),
                    );
                } else {
                    first_default = Some(case.span);
                }
            }
        }
    }

    /// Checks for multiple default cases in a select.
    fn multiple_defaults_select(&self, cases: &[gox_syntax::ast::SelectCase]) {
        let mut first_default: Option<Span> = None;
        for case in cases {
            if case.comm.is_none() {
                // This is a default case
                if let Some(first_span) = first_default {
                    self.error(
                        case.span,
                        format!("multiple defaults (first at {})", first_span.start.to_usize()),
                    );
                } else {
                    first_default = Some(case.span);
                }
            }
        }
    }

    /// Checks case values in an expression switch.
    fn case_values(
        &mut self,
        x: &mut Operand,
        exprs: &[Expr],
        seen: &mut ValueMap,
    ) {
        for e in exprs {
            let v = &mut Operand::new();
            self.expr(v, e);
            if x.invalid() || v.invalid() {
                continue;
            }
            self.convert_untyped(v, x.typ.unwrap());
            if v.invalid() {
                continue;
            }
            // Order matters: By comparing v against x, error positions are at the case values.
            let res = &mut v.clone();
            self.comparison(res, x, gox_syntax::ast::BinaryOp::Eq);
            if res.invalid() {
                continue;
            }
            
            // Check for duplicate constant values
            if let OperandMode::Constant(val) = &v.mode {
                match GoVal::from_const(val) {
                    GoVal::Invalid => {}
                    gov => {
                        let entry = seen.entry(gov).or_insert_with(Vec::new);
                        if let Some(pt) = entry
                            .iter()
                            .find(|pt| typ::identical(v.typ.unwrap(), pt.typ, self.objs()))
                        {
                            self.error(
                                e.span,
                                format!("duplicate case {} in expression switch", val),
                            );
                            self.error_str(pt.span.start.to_usize(), "\tprevious case");
                            continue;
                        }
                        entry.push(PosType {
                            span: e.span,
                            typ: v.typ.unwrap(),
                        });
                    }
                }
            }
        }
    }

    /// Checks case types in a type switch.
    fn case_types(
        &mut self,
        _x: &mut Operand,
        _xtype: TypeKey,
        types: &[Option<gox_syntax::ast::TypeExpr>],
        seen: &mut HashMap<Option<TypeKey>, Span>,
    ) -> Option<TypeKey> {
        let mut last_type: Option<TypeKey> = None;
        for ty_opt in types {
            let t = match ty_opt {
                Some(ty) => {
                    let t = self.type_expr(ty);
                    if t == self.invalid_type() {
                        continue;
                    }
                    Some(t)
                }
                None => None, // nil case
            };

            // Check for duplicate types
            if let Some((&_prev_t, &prev_span)) = seen
                .iter()
                .find(|(&t2, _)| typ::identical_o(t, t2, self.objs()))
            {
                let ts = t.map_or("nil".to_owned(), |tk| format!("{:?}", tk));
                let span = ty_opt.as_ref().map_or(Span::default(), |ty| ty.span);
                self.error(span, format!("duplicate case {} in type switch", ts));
                self.error_str(prev_span.start.to_usize(), "\tprevious case");
                continue;
            }

            let span = ty_opt.as_ref().map_or(Span::default(), |ty| ty.span);
            seen.insert(t, span);

            if let Some(t) = t {
                // Type assertion check - verify t is a valid type for type switch
                // (interface implementation check would go here)
                last_type = Some(t);
            }
        }
        last_type
    }

    // =========================================================================
    // Statement list and wrapper (with fctx)
    // =========================================================================

    /// Checks a list of statements.
    fn stmt_list(&mut self, list: &[Stmt], sctx: &StmtContext) {
        // Trailing empty statements are "invisible" to fallthrough analysis
        let index = list
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, x)| match &x.kind {
                StmtKind::Empty => None,
                _ => Some(i + 1),
            })
            .unwrap_or(list.len());

        for (i, s) in list[0..index].iter().enumerate() {
            let mut inner = *sctx;
            inner.fallthrough_ok = sctx.fallthrough_ok && i + 1 == index;
            self.stmt(s, &inner);
        }
    }

    /// Main statement checking wrapper - handles delayed actions.
    fn stmt(&mut self, stmt: &Stmt, ctx: &StmtContext) {
        let begin_scope = self.octx.scope;
        let begin_delayed_count = self.delayed_count();

        self.stmt_impl(stmt, ctx);

        self.process_delayed(begin_delayed_count);
        debug_assert_eq!(begin_scope, self.octx.scope);
    }

    /// Internal statement implementation.
    fn stmt_impl(&mut self, stmt: &Stmt, ctx: &StmtContext) {
        let mut inner_ctx = *ctx;
        inner_ctx.fallthrough_ok = false;
        inner_ctx.final_switch_case = false;

        match &stmt.kind {
            StmtKind::Empty => {}

            StmtKind::Block(block) => {
                self.open_scope(block.span, "block");
                self.stmt_list(&block.stmts, &inner_ctx);
                self.close_scope();
            }

            StmtKind::Labeled(labeled) => {
                self.octx.has_label = true;
                self.stmt(&labeled.stmt, ctx);
            }

            StmtKind::Expr(e) => {
                // spec: "With the exception of specific built-in functions,
                // function and method calls and receive operations can appear
                // in statement context."
                let x = &mut Operand::new();
                self.raw_expr(x, e, None);
                // Check that the expression is a valid statement expression
                match &x.mode {
                    OperandMode::Invalid | OperandMode::NoValue => {}
                    OperandMode::Builtin(_) => {
                        self.error(e.span, "builtin must be called".to_string());
                    }
                    OperandMode::TypeExpr => {
                        self.error(e.span, "type is not an expression".to_string());
                    }
                    _ => {
                        // Valid expression statement: call result is discarded
                        // In Go, only calls and receive operations are valid as statements
                        // For now, allow any expression (could warn about unused values)
                    }
                }
            }

            StmtKind::Send(ss) => {
                let (ch, x) = (&mut Operand::new(), &mut Operand::new());
                self.expr(ch, &ss.chan);
                self.expr(x, &ss.value);
                if ch.invalid() || x.invalid() {
                    return;
                }
                let chtype = ch.typ.unwrap();
                let under_chtype = typ::underlying_type(chtype, self.objs());
                if let Some(chan) = self.otype(under_chtype).try_as_chan() {
                    if chan.dir() == ChanDir::RecvOnly {
                        self.error(ss.chan.span, "cannot send to receive-only channel".to_string());
                    } else {
                        let elem_ty = Some(chan.elem());
                        self.assignment(x, elem_ty, "send");
                    }
                } else {
                    self.error(ss.chan.span, "cannot send to non-chan type".to_string());
                }
            }

            StmtKind::IncDec(ids) => {
                let x = &mut Operand::new();
                self.expr(x, &ids.expr);
                if x.invalid() {
                    return;
                }
                if !typ::is_numeric(x.typ.unwrap(), self.objs()) {
                    self.error(ids.expr.span, "non-numeric operand for inc/dec".to_string());
                    return;
                }
                // x++ is like x = x + 1, x-- is like x = x - 1
                // For now, just check assignability (binary op result type is same as x)
                self.assign_var(&ids.expr, x);
            }

            StmtKind::Assign(astmt) => {
                match astmt.op {
                    AssignOp::Assign => {
                        if astmt.lhs.is_empty() {
                            self.error(stmt.span, "missing lhs in assignment".to_string());
                            return;
                        }
                        self.assign_vars(&astmt.lhs, &astmt.rhs);
                    }
                    _ => {
                        // Compound assignment: +=, -=, etc.
                        if astmt.lhs.len() != 1 || astmt.rhs.len() != 1 {
                            self.error(
                                stmt.span,
                                "assignment operation requires single-valued expressions".to_string(),
                            );
                            return;
                        }
                        if let Some(bin_op) = Self::assign_op_to_binary(astmt.op) {
                            let x = &mut Operand::new();
                            self.binary(x, None, &astmt.lhs[0], &astmt.rhs[0], bin_op);
                            if !x.invalid() {
                                self.assign_var(&astmt.lhs[0], x);
                            }
                        }
                    }
                }
            }

            StmtKind::ShortVar(sv) => {
                self.short_var_decl_stmt(&sv.names, &sv.values, stmt.span);
            }

            StmtKind::Go(gs) => {
                self.suspended_call("go", &gs.call);
            }

            StmtKind::Defer(ds) => {
                self.suspended_call("defer", &ds.call);
            }

            StmtKind::ErrDefer(eds) => {
                // GoX extension: errdefer runs only on error return
                self.suspended_call("errdefer", &eds.call);
            }

            StmtKind::Fail(fs) => {
                // GoX extension: fail returns error from fallible function
                let x = &mut Operand::new();
                self.expr(x, &fs.error);
                // Check that the expression is assignable to error type
                if !x.invalid() {
                    let error_type = self.universe().error_type();
                    self.assignment(x, Some(error_type), "fail statement");
                }
            }

            StmtKind::Return(rs) => {
                let reskey = self
                    .otype(self.octx.sig.unwrap())
                    .try_as_signature()
                    .unwrap()
                    .results();
                let res = self.otype(reskey).try_as_tuple().unwrap();
                if res.vars().len() > 0 {
                    // function returns results
                    // (if one, say the first, result parameter is named, all of them are named)
                    if rs.values.is_empty() && self.lobj(res.vars()[0]).name() != "" {
                        // spec: "Implementation restriction: A compiler may disallow an empty expression
                        // list in a "return" statement if a different entity (constant, type, or variable)
                        // with the same name as a result parameter is in scope at the place of the return."
                        for &okey in res.vars().iter() {
                            let lobj = self.lobj(okey);
                            if let Some(alt) = self.lookup(lobj.name()) {
                                if alt == okey {
                                    continue;
                                }
                                self.error(
                                    stmt.span,
                                    format!(
                                        "result parameter {} not in scope at return",
                                        lobj.name()
                                    ),
                                );
                                let alt_pos = self.lobj(alt).pos();
                                self.error_str(alt_pos, &format!("\tinner declaration of {}", lobj.name()));
                                // ok to continue
                            }
                        }
                    } else {
                        // return has results or result parameters are unnamed
                        let vars = res.vars().clone();
                        self.init_vars(&vars, &rs.values, Some(stmt.span));
                    }
                } else if !rs.values.is_empty() {
                    self.error(rs.values[0].span, "no result values expected".to_string());
                    self.use_exprs(&rs.values);
                }
            }

            StmtKind::Break(bs) => {
                if bs.label.is_some() {
                    self.octx.has_label = true;
                    return; // checked in label pass
                }
                if !ctx.break_ok {
                    self.error(stmt.span, "break not in for, switch, or select statement".to_string());
                }
            }

            StmtKind::Continue(cs) => {
                if cs.label.is_some() {
                    self.octx.has_label = true;
                    return; // checked in label pass
                }
                if !ctx.continue_ok {
                    self.error(stmt.span, "continue not in for statement".to_string());
                }
            }

            StmtKind::Goto(_) => {
                self.octx.has_label = true;
                // checked in label pass
            }

            StmtKind::Fallthrough => {
                if !ctx.fallthrough_ok {
                    let msg = if ctx.final_switch_case {
                        "cannot fallthrough final case in switch"
                    } else {
                        "fallthrough statement out of place"
                    };
                    self.error(stmt.span, msg.to_string());
                }
            }

            StmtKind::If(ifs) => {
                self.open_scope(stmt.span, "if");
                if let Some(init) = &ifs.init {
                    self.stmt(init, &StmtContext::new());
                }
                let x = &mut Operand::new();
                self.expr(x, &ifs.cond);
                if !x.invalid() && !typ::is_boolean(x.typ.unwrap(), self.objs()) {
                    self.error(ifs.cond.span, "non-boolean condition in if statement".to_string());
                }
                self.open_scope(ifs.then.span, "then");
                self.stmt_list(&ifs.then.stmts, &inner_ctx);
                self.close_scope();
                if let Some(els) = &ifs.else_ {
                    self.stmt(els, &inner_ctx);
                }
                self.close_scope();
            }

            StmtKind::Switch(ss) => {
                inner_ctx.break_ok = true;
                self.open_scope(stmt.span, "switch");
                if let Some(init) = &ss.init {
                    self.stmt(init, &StmtContext::new());
                }
                let x = &mut Operand::new();
                if let Some(tag) = &ss.tag {
                    self.expr(x, tag);
                    self.assignment(x, None, "switch expression");
                } else {
                    // Missing switch expression is equivalent to true
                    x.mode = OperandMode::Constant(Value::with_bool(true));
                    x.typ = Some(self.basic_type(BasicType::Bool));
                }
                self.multiple_defaults(&ss.cases);
                let mut seen = ValueMap::new();
                let case_count = ss.cases.len();
                for (i, clause) in ss.cases.iter().enumerate() {
                    // Check case expressions for duplicates
                    self.case_values(x, &clause.exprs, &mut seen);
                    self.open_scope(clause.span, "case");
                    let mut inner2 = inner_ctx;
                    if i + 1 < case_count {
                        inner2.fallthrough_ok = true;
                    } else {
                        inner2.final_switch_case = true;
                    }
                    self.stmt_list(&clause.body, &inner2);
                    self.close_scope();
                }
                self.close_scope();
            }

            StmtKind::TypeSwitch(tss) => {
                inner_ctx.break_ok = true;
                self.open_scope(stmt.span, "type switch");
                if let Some(init) = &tss.init {
                    self.stmt(init, &StmtContext::new());
                }
                
                // Check if there's a lhs variable (x := expr.(type))
                // If name is "_", treat as None to avoid declared but not used error
                let lhs: Option<&Ident> = tss.assign.as_ref().and_then(|assign| {
                    let name = self.resolve_symbol(assign.symbol);
                    if name == "_" {
                        self.soft_error(assign.span, "no new variable on left side of :=".to_string());
                        None
                    } else {
                        self.result.record_def(assign.clone(), None);
                        Some(assign)
                    }
                });
                
                let x = &mut Operand::new();
                self.expr(x, &tss.expr);
                if x.invalid() {
                    self.close_scope();
                    return;
                }
                let xtype = typ::underlying_type(x.typ.unwrap(), self.objs());
                if self.otype(xtype).try_as_interface().is_none() {
                    self.error(tss.expr.span, "cannot type switch on non-interface type".to_string());
                    self.close_scope();
                    return;
                }
                
                self.multiple_defaults_type(&tss.cases);
                
                // Track seen types for duplicate detection
                let mut seen_types: HashMap<Option<TypeKey>, Span> = HashMap::new();
                let mut lhs_vars: Vec<crate::objects::ObjKey> = Vec::new();
                
                for clause in &tss.cases {
                    // Check each type in this type switch case.
                    let mut t = self.case_types(x, xtype, &clause.types, &mut seen_types);
                    self.open_scope(clause.span, "case");
                    
                    // If lhs exists, declare a corresponding variable in the case-local scope.
                    if let Some(lhs_ident) = lhs {
                        // spec: "The TypeSwitchGuard may include a short variable declaration.
                        // When that form is used, the variable is declared at the beginning of
                        // the implicit block in each clause. In clauses with a case listing
                        // exactly one type, the variable has that type; otherwise, the variable
                        // has the type of the expression in the TypeSwitchGuard."
                        if clause.types.len() != 1 || t.is_none() {
                            t = x.typ;
                        }
                        let name = self.resolve_symbol(lhs_ident.symbol).to_string();
                        let okey = self.new_var(lhs_ident.span.start.to_usize(), Some(self.pkg), name, t);
                        self.declare(self.octx.scope.unwrap(), okey);
                        self.result.record_implicit(clause.span, okey);
                        // For the "declared but not used" error, all lhs variables act as
                        // one; i.e., if any one of them is 'used', all of them are 'used'.
                        // Collect them for later analysis.
                        lhs_vars.push(okey);
                    }
                    
                    self.stmt_list(&clause.body, &inner_ctx);
                    self.close_scope();
                }
                
                // If lhs exists, we must have at least one lhs variable that was used.
                if lhs.is_some() {
                    let used = lhs_vars.iter().fold(false, |acc, &okey| {
                        let prop = self.lobj_mut(okey).entity_type_mut().var_property_mut();
                        let var_used = prop.used;
                        prop.used = true; // avoid usage error when checking entire function
                        acc || var_used
                    });
                    if !used {
                        let lhs_ident = lhs.unwrap();
                        let name = self.resolve_symbol(lhs_ident.symbol);
                        self.soft_error(lhs_ident.span, format!("{} declared but not used", name));
                    }
                }
                
                self.close_scope();
            }

            StmtKind::Select(ss) => {
                inner_ctx.break_ok = true;
                self.multiple_defaults_select(&ss.cases);
                for case in &ss.cases {
                    self.open_scope(case.span, "case");
                    if let Some(comm) = &case.comm {
                        match comm {
                            gox_syntax::ast::CommClause::Send(send) => {
                                let ch = &mut Operand::new();
                                let val = &mut Operand::new();
                                self.expr(ch, &send.chan);
                                self.expr(val, &send.value);
                                // Check channel type for send
                                if !ch.invalid() {
                                    let chtype = ch.typ.unwrap();
                                    let under = typ::underlying_type(chtype, self.objs());
                                    if let Some(chan) = self.otype(under).try_as_chan() {
                                        if chan.dir() == ChanDir::RecvOnly {
                                            self.error(send.chan.span, "cannot send to receive-only channel".to_string());
                                        } else if !val.invalid() {
                                            self.assignment(val, Some(chan.elem()), "send");
                                        }
                                    } else {
                                        self.error(send.chan.span, "cannot send to non-chan type".to_string());
                                    }
                                }
                            }
                            gox_syntax::ast::CommClause::Recv(recv) => {
                                let x = &mut Operand::new();
                                self.expr(x, &recv.expr);
                                // Check receive expression is a channel receive
                                if !x.invalid() {
                                    let xtype = x.typ.unwrap();
                                    let under = typ::underlying_type(xtype, self.objs());
                                    if let Some(chan) = self.otype(under).try_as_chan() {
                                        if chan.dir() == ChanDir::SendOnly {
                                            self.error(recv.expr.span, "cannot receive from send-only channel".to_string());
                                        }
                                        // Assign received value to lhs variables if any
                                        // (recv.lhs contains the identifiers, recv.define indicates :=)
                                    } else {
                                        self.error(recv.expr.span, "cannot receive from non-chan type".to_string());
                                    }
                                }
                            }
                        }
                    }
                    self.stmt_list(&case.body, &inner_ctx);
                    self.close_scope();
                }
            }

            StmtKind::For(fs) => {
                inner_ctx.break_ok = true;
                inner_ctx.continue_ok = true;
                self.open_scope(stmt.span, "for");
                match &fs.clause {
                    ForClause::Cond(cond) => {
                        if let Some(c) = cond {
                            let x = &mut Operand::new();
                            self.expr(x, c);
                            if !x.invalid() && !typ::is_boolean(x.typ.unwrap(), self.objs()) {
                                self.error(c.span, "non-boolean condition in for statement".to_string());
                            }
                        }
                    }
                    ForClause::Three { init, cond, post } => {
                        if let Some(i) = init {
                            self.stmt(i, &StmtContext::new());
                        }
                        if let Some(c) = cond {
                            let x = &mut Operand::new();
                            self.expr(x, c);
                            if !x.invalid() && !typ::is_boolean(x.typ.unwrap(), self.objs()) {
                                self.error(c.span, "non-boolean condition in for statement".to_string());
                            }
                        }
                        if let Some(p) = post {
                            // spec: post statement must not be a short variable declaration
                            if matches!(&p.kind, StmtKind::ShortVar(_)) {
                                self.error(p.span, "cannot declare in post statement".to_string());
                            }
                            self.stmt(p, &StmtContext::new());
                        }
                    }
                    ForClause::Range { key, value, define, expr } => {
                        let x = &mut Operand::new();
                        self.expr(x, expr);
                        
                        // Determine key/value types based on range expression type
                        let (key_type, val_type) = if x.invalid() {
                            (None, None)
                        } else {
                            let xtype = x.typ.unwrap();
                            let under = typ::underlying_type(xtype, self.objs());
                            match self.otype(under) {
                                typ::Type::Basic(_) if typ::is_string(under, self.objs()) => {
                                    (Some(self.basic_type(BasicType::Int)), 
                                     Some(self.basic_type(BasicType::Rune)))
                                }
                                typ::Type::Array(a) => {
                                    (Some(self.basic_type(BasicType::Int)), Some(a.elem()))
                                }
                                typ::Type::Slice(s) => {
                                    (Some(self.basic_type(BasicType::Int)), Some(s.elem()))
                                }
                                typ::Type::Map(m) => {
                                    (Some(m.key()), Some(m.elem()))
                                }
                                typ::Type::Chan(c) => {
                                    if c.dir() == ChanDir::SendOnly {
                                        self.error(expr.span, "cannot range over send-only channel".to_string());
                                    }
                                    (Some(c.elem()), None)
                                }
                                _ => {
                                    self.error(expr.span, format!("cannot range over {:?}", xtype));
                                    (None, None)
                                }
                            }
                        };
                        
                        // Declare or assign key/value variables
                        // Aligned with goscript/types/src/check/stmt.rs::Stmt::Range
                        let scope_key = self.octx.scope.unwrap();
                        let lhs: [Option<&Expr>; 2] = [key.as_ref(), value.as_ref()];
                        let rhs = [key_type, val_type];
                        
                        if *define {
                            // Short variable declaration
                            let mut vars = vec![];
                            for (i, lhs_expr) in lhs.iter().enumerate() {
                                if lhs_expr.is_none() {
                                    continue;
                                }
                                let lhs_e = lhs_expr.unwrap();
                                // For define, lhs must be an identifier
                                let ident = match self.expr_as_ident(lhs_e) {
                                    Some(id) => id,
                                    None => {
                                        self.error(lhs_e.span, "expected identifier on left side of :=".to_string());
                                        continue;
                                    }
                                };
                                let name = self.resolve_ident(&ident).to_string();
                                let has_name = name != "_";
                                // Create var with type None - init_var will set type
                                let okey = self.new_var(lhs_e.span.start.to_usize(), Some(self.pkg), name, None);
                                self.result.record_def(ident, Some(okey));
                                if has_name {
                                    vars.push(okey);
                                }
                                // Initialize lhs variable using init_var
                                if let Some(rhs_type) = rhs[i] {
                                    let mut x = Operand::new();
                                    x.mode = OperandMode::Value;
                                    x.typ = Some(rhs_type);
                                    self.init_var(okey, &mut x, "range clause");
                                } else {
                                    let invalid_type = self.invalid_type();
                                    self.lobj_mut(okey).set_type(Some(invalid_type));
                                }
                            }
                            // Declare variables
                            for okey in vars {
                                self.declare(scope_key, okey);
                            }
                        } else {
                            // ordinary assignment
                            for (i, lhs_expr) in lhs.iter().enumerate() {
                                if lhs_expr.is_some() && rhs[i].is_some() {
                                    x.mode = OperandMode::Value;
                                    x.typ = rhs[i];
                                    self.assign_var(lhs_expr.unwrap(), x);
                                }
                            }
                        }
                    }
                }
                self.open_scope(fs.body.span, "for body");
                self.stmt_list(&fs.body.stmts, &inner_ctx);
                self.close_scope();
                self.close_scope();
            }

            StmtKind::Var(var) => {
                // Variable declaration in statement context
                let scope_key = self.octx.scope.unwrap();
                for spec in &var.specs {
                    // Get type if specified
                    let declared_type = spec.ty.as_ref().map(|ty| self.type_expr(ty));
                    
                    // Evaluate initializer expressions
                    let mut rhs_types: Vec<Option<TypeKey>> = Vec::new();
                    for val in &spec.values {
                        let x = &mut Operand::new();
                        self.expr(x, val);
                        if !x.invalid() {
                            if let Some(dt) = declared_type {
                                self.assignment(x, Some(dt), "variable declaration");
                            }
                        }
                        rhs_types.push(x.typ);
                    }
                    
                    // Declare variables
                    for (i, name) in spec.names.iter().enumerate() {
                        let name_str = self.resolve_symbol(name.symbol).to_string();
                        let var_type = declared_type.or_else(|| rhs_types.get(i).copied().flatten());
                        let okey = self.new_var(name.span.start.to_usize(), Some(self.pkg), name_str.clone(), var_type);
                        if name_str != "_" {
                            self.result.record_def(name.clone(), Some(okey));
                            self.declare(scope_key, okey);
                        }
                    }
                }
            }

            StmtKind::Const(cons) => {
                // Constant declaration in statement context
                let scope_key = self.octx.scope.unwrap();
                for spec in &cons.specs {
                    // Get type if specified
                    let declared_type = spec.ty.as_ref().map(|ty| self.type_expr(ty));
                    
                    // Evaluate constant expressions
                    for (_i, (name, val)) in spec.names.iter().zip(spec.values.iter()).enumerate() {
                        let x = &mut Operand::new();
                        self.expr(x, val);
                        
                        let const_type = declared_type.or(x.typ);
                        let const_val = match &x.mode {
                            OperandMode::Constant(v) => v.clone(),
                            _ => {
                                if !x.invalid() {
                                    self.error(val.span, "const initializer is not a constant".to_string());
                                }
                                Value::Unknown
                            }
                        };
                        
                        let name_str = self.resolve_symbol(name.symbol).to_string();
                        let okey = self.new_const(name.span.start.to_usize(), Some(self.pkg), name_str.clone(), const_type, const_val);
                        if name_str != "_" {
                            self.result.record_def(name.clone(), Some(okey));
                            self.declare(scope_key, okey);
                        }
                    }
                }
            }

            StmtKind::Type(ty) => {
                // Type declaration in statement context
                let scope_key = self.octx.scope.unwrap();
                let name_str = self.resolve_symbol(ty.name.symbol).to_string();
                let rhs_type = self.type_expr(&ty.ty);
                let okey = self.new_type_name(ty.name.span.start.to_usize(), Some(self.pkg), name_str.clone(), Some(rhs_type));
                if name_str != "_" {
                    self.result.record_def(ty.name.clone(), Some(okey));
                    self.declare(scope_key, okey);
                }
            }
        }
    }

    /// Check a go/defer/errdefer call - must be a function call.
    fn suspended_call(&mut self, kw: &str, call: &Expr) {
        let x = &mut Operand::new();
        self.raw_expr(x, call, None);
        
        // Check that it's actually a function call
        match &call.kind {
            gox_syntax::ast::ExprKind::Call(_) => {
                // Valid: function call
            }
            gox_syntax::ast::ExprKind::Conversion(_) => {
                self.error(call.span, format!("{} requires function call, not conversion", kw));
            }
            _ => {
                if !x.invalid() {
                    self.error(call.span, format!("expression in {} must be function call", kw));
                }
            }
        }
    }

    /// Short variable declaration in statement context.
    /// Aligned with goscript/types/src/check/assignment.rs::short_var_decl
    fn short_var_decl_stmt(
        &mut self,
        names: &[Ident],
        values: &[Expr],
        span: Span,
    ) {
        let scope_key = match self.octx.scope {
            Some(s) => s,
            None => return,
        };

        let mut new_vars = Vec::new();
        let mut lhs_vars = Vec::new();

        for ident in names.iter() {
            let name = self.resolve_symbol(ident.symbol).to_string();

            // Check if variable already exists in current scope
            if let Some(okey) = self.scope(scope_key).lookup(&name) {
                self.result.record_use(ident.clone(), okey);
                if self.lobj(okey).entity_type().is_var() {
                    lhs_vars.push(okey);
                } else {
                    self.error(ident.span, format!("cannot assign to {}", name));
                    // dummy variable
                    let dummy = self.new_var(ident.span.start.to_usize(), Some(self.pkg), "_".to_string(), None);
                    lhs_vars.push(dummy);
                }
            } else {
                // Declare new variable with type=None (init_var will set type)
                let okey = self.new_var(ident.span.start.to_usize(), Some(self.pkg), name.clone(), None);
                if name != "_" {
                    new_vars.push(okey);
                }
                self.result.record_def(ident.clone(), Some(okey));
                lhs_vars.push(okey);
            }
        }

        // Use init_vars to properly handle type inference (converts untyped to default types)
        self.init_vars(&lhs_vars, values, None);

        // Declare new variables in scope
        if !new_vars.is_empty() {
            for okey in &new_vars {
                self.declare(scope_key, *okey);
            }
        } else {
            self.soft_error(span, "no new variables on left side of :=".to_string());
        }
    }

    // =========================================================================
    // Helper functions
    // =========================================================================

    /// Converts an AssignOp to the corresponding BinaryOp for compound assignments.
    fn assign_op_to_binary(op: AssignOp) -> Option<gox_syntax::ast::BinaryOp> {
        use gox_syntax::ast::BinaryOp;
        match op {
            AssignOp::Assign => None, // Not a compound assignment
            AssignOp::Add => Some(BinaryOp::Add),
            AssignOp::Sub => Some(BinaryOp::Sub),
            AssignOp::Mul => Some(BinaryOp::Mul),
            AssignOp::Div => Some(BinaryOp::Div),
            AssignOp::Rem => Some(BinaryOp::Rem),
            AssignOp::Shl => Some(BinaryOp::Shl),
            AssignOp::Shr => Some(BinaryOp::Shr),
            AssignOp::And => Some(BinaryOp::And),
            AssignOp::Or => Some(BinaryOp::Or),
            AssignOp::Xor => Some(BinaryOp::Xor),
            AssignOp::AndNot => Some(BinaryOp::AndNot),
        }
    }

}
