//! Type information produced by type checking.


use crate::obj;
use crate::objects::{ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::OperandMode;
use crate::selection::Selection;
use gox_common::symbol::Ident;
use gox_common_core::{ExprId, TypeExprId};
use gox_common::Span;
use gox_syntax::ast::Expr;
use std::collections::{HashMap, HashSet};

/// TypeAndValue reports the type and value (for constants) of an expression.
#[derive(Debug, Clone)]
pub struct TypeAndValue {
    pub mode: OperandMode,
    pub typ: TypeKey,
}

impl TypeAndValue {
    pub(crate) fn new(mode: OperandMode, typ: TypeKey) -> Self {
        TypeAndValue { mode, typ }
    }
}

/// An Initializer describes a package-level variable initialization.
#[derive(Debug, Clone)]
pub struct Initializer {
    pub lhs: Vec<ObjKey>,
    pub rhs: Expr,
}

impl Initializer {
    pub(crate) fn new(lhs: Vec<ObjKey>, rhs: Expr) -> Self {
        Initializer { lhs, rhs }
    }
}

/// TypeInfo holds the results of type checking.
#[derive(Debug, Default)]
pub struct TypeInfo {
    /// Maps expressions to their types (and values for constants).
    pub types: HashMap<ExprId, TypeAndValue>,

    /// Maps type expressions to their resolved types.
    pub type_exprs: HashMap<TypeExprId, TypeKey>,

    /// Maps identifiers to the objects they define.
    /// Key is the Ident (which contains span for uniqueness).
    pub defs: HashMap<Ident, Option<ObjKey>>,

    /// Maps identifiers to the objects they denote (use).
    pub uses: HashMap<Ident, ObjKey>,

    /// Maps AST node spans to their implicitly declared objects.
    pub implicits: HashMap<Span, ObjKey>,

    /// Maps selector expression IDs to their selections.
    pub selections: HashMap<ExprId, Selection>,

    /// Maps AST node spans to the scopes they define.
    pub scopes: HashMap<Span, ScopeKey>,

    /// Maps statement positions to the scopes they define.
    /// Used for block, if, for, switch, etc. statements.
    pub stmt_scopes: HashMap<usize, ScopeKey>,

    /// Package-level initializers in execution order.
    pub init_order: Vec<Initializer>,

    /// Variables that escape to heap (set by escape analysis pass).
    pub escaped_vars: HashSet<ObjKey>,
}

impl TypeInfo {
    pub(crate) fn new() -> TypeInfo {
        TypeInfo::default()
    }

    /// Records the type and mode for an expression.
    pub(crate) fn record_type(&mut self, expr_id: ExprId, mode: OperandMode, typ: TypeKey) {
        self.types.insert(expr_id, TypeAndValue::new(mode, typ));
    }

    /// Records a type and value for an expression (alias for record_type).
    pub(crate) fn record_type_and_value(&mut self, expr_id: ExprId, mode: OperandMode, typ: TypeKey) {
        self.record_type(expr_id, mode, typ);
    }

    /// Records the resolved type for a type expression.
    pub(crate) fn record_type_expr(&mut self, type_expr_id: TypeExprId, typ: TypeKey) {
        self.type_exprs.insert(type_expr_id, typ);
    }

    /// Records a definition.
    pub(crate) fn record_def(&mut self, ident: Ident, obj: Option<ObjKey>) {
        self.defs.insert(ident, obj);
    }

    /// Records a use.
    pub(crate) fn record_use(&mut self, ident: Ident, obj: ObjKey) {
        self.uses.insert(ident, obj);
    }

    /// Records an implicit object.
    pub(crate) fn record_implicit(&mut self, span: Span, obj: ObjKey) {
        self.implicits.insert(span, obj);
    }

    /// Records a selection.
    pub(crate) fn record_selection(&mut self, expr_id: ExprId, sel: Selection) {
        self.selections.insert(expr_id, sel);
    }

    /// Records a scope for an AST node.
    pub(crate) fn record_scope(&mut self, span: Span, scope: ScopeKey) {
        self.scopes.insert(span, scope);
    }

    /// Records a scope for a statement (by position).
    pub(crate) fn record_stmt_scope(&mut self, pos: usize, scope: ScopeKey) {
        self.stmt_scopes.insert(pos, scope);
    }

    /// Records init order.
    pub(crate) fn record_init_order(&mut self, init_order: Vec<Initializer>) {
        self.init_order = init_order;
    }

    /// Records builtin type signature for a builtin function expression.
    /// The expression must be a (possibly parenthesized) identifier denoting a built-in.
    pub(crate) fn record_builtin_type(
        &mut self,
        mode: &OperandMode,
        expr: &gox_syntax::ast::Expr,
        sig: TypeKey,
    ) {
        use gox_syntax::ast::ExprKind;
        
        let mut e = expr;
        loop {
            self.record_type_and_value(e.id, mode.clone(), sig);
            match &e.kind {
                ExprKind::Ident(_) => break,
                ExprKind::Paren(inner) => e = inner,
                _ => break, // Should not happen for builtin calls
            }
        }
    }

    /// Records comma-ok types for expressions like map index, type assertion, channel receive.
    /// Aligned with goscript/types/src/check/check.rs::record_comma_ok_types
    pub(crate) fn record_comma_ok_types(
        &mut self,
        expr: &gox_syntax::ast::Expr,
        t: &[TypeKey; 2],
        tc_objs: &mut TCObjects,
        pkg: PackageKey,
    ) {
        use gox_syntax::ast::ExprKind;
        
        let pos = expr.span.start.to_usize();
        let mut e = expr;
        loop {
            let tv = self.types.get_mut(&e.id).unwrap();
            let vars = vec![
                tc_objs.lobjs.insert(obj::LangObj::new_var(pos, Some(pkg), String::new(), Some(t[0]))),
                tc_objs.lobjs.insert(obj::LangObj::new_var(pos, Some(pkg), String::new(), Some(t[1]))),
            ];
            tv.typ = tc_objs.new_t_tuple(vars);
            match &e.kind {
                ExprKind::Paren(inner) => e = inner,
                _ => break,
            }
        }
    }

    /// Looks up the type of an expression.
    pub(crate) fn expr_type(&self, expr_id: ExprId) -> Option<TypeKey> {
        self.types.get(&expr_id).map(|tv| tv.typ)
    }

    /// Looks up the mode of an expression.
    pub(crate) fn expr_mode(&self, expr_id: ExprId) -> Option<&OperandMode> {
        self.types.get(&expr_id).map(|tv| &tv.mode)
    }

    /// Looks up the object for a definition.
    pub(crate) fn get_def(&self, ident: &Ident) -> Option<ObjKey> {
        self.defs.get(ident).and_then(|o| *o)
    }

    /// Looks up the object for a use.
    pub(crate) fn get_use(&self, ident: &Ident) -> Option<ObjKey> {
        self.uses.get(ident).copied()
    }

    /// Returns true if the identifier is a definition.
    pub(crate) fn is_def(&self, ident: &Ident) -> bool {
        self.defs.contains_key(ident)
    }

    /// Returns true if the variable escapes to heap.
    pub fn is_escaped(&self, obj: ObjKey) -> bool {
        self.escaped_vars.contains(&obj)
    }
}
