//! Type information produced by type checking.

#![allow(dead_code)]

use crate::objects::{ObjKey, ScopeKey, TypeKey};
use crate::operand::OperandMode;
use crate::selection::Selection;
use gox_common::symbol::Ident;
use gox_common_core::ExprId;
use std::collections::HashMap;

/// TypeAndValue reports the type and value (for constants) of an expression.
#[derive(Debug, Clone)]
pub struct TypeAndValue {
    pub mode: OperandMode,
    pub typ: TypeKey,
}

impl TypeAndValue {
    pub fn new(mode: OperandMode, typ: TypeKey) -> Self {
        TypeAndValue { mode, typ }
    }
}

/// An Initializer describes a package-level variable initialization.
#[derive(Debug)]
pub struct Initializer {
    pub lhs: Vec<ObjKey>,
    pub rhs_expr_id: ExprId,
}

impl Initializer {
    pub fn new(lhs: Vec<ObjKey>, rhs_expr_id: ExprId) -> Self {
        Initializer { lhs, rhs_expr_id }
    }
}

/// TypeInfo holds the results of type checking.
#[derive(Debug, Default)]
pub struct TypeInfo {
    /// Maps expressions to their types (and values for constants).
    pub types: HashMap<ExprId, TypeAndValue>,

    /// Maps identifiers to the objects they define.
    /// Key is the Ident (which contains span for uniqueness).
    pub defs: HashMap<Ident, Option<ObjKey>>,

    /// Maps identifiers to the objects they denote (use).
    pub uses: HashMap<Ident, ObjKey>,

    /// Maps expression IDs to their implicitly declared objects.
    pub implicits: HashMap<ExprId, ObjKey>,

    /// Maps selector expression IDs to their selections.
    pub selections: HashMap<ExprId, Selection>,

    /// Maps expression IDs to the scopes they define.
    pub scopes: HashMap<ExprId, ScopeKey>,

    /// Maps statement positions to the scopes they define.
    /// Used for block, if, for, switch, etc. statements.
    pub stmt_scopes: HashMap<usize, ScopeKey>,

    /// Package-level initializers in execution order.
    pub init_order: Vec<Initializer>,
}

impl TypeInfo {
    pub fn new() -> TypeInfo {
        TypeInfo::default()
    }

    /// Records the type and mode for an expression.
    pub fn record_type(&mut self, expr_id: ExprId, mode: OperandMode, typ: TypeKey) {
        self.types.insert(expr_id, TypeAndValue::new(mode, typ));
    }

    /// Records a type and value for an expression (alias for record_type).
    pub fn record_type_and_value(&mut self, expr_id: ExprId, mode: OperandMode, typ: TypeKey) {
        self.record_type(expr_id, mode, typ);
    }

    /// Records a definition.
    pub fn record_def(&mut self, ident: Ident, obj: Option<ObjKey>) {
        self.defs.insert(ident, obj);
    }

    /// Records a use.
    pub fn record_use(&mut self, ident: Ident, obj: ObjKey) {
        self.uses.insert(ident, obj);
    }

    /// Records an implicit object.
    pub fn record_implicit(&mut self, expr_id: ExprId, obj: ObjKey) {
        self.implicits.insert(expr_id, obj);
    }

    /// Records a selection.
    pub fn record_selection(&mut self, expr_id: ExprId, sel: Selection) {
        self.selections.insert(expr_id, sel);
    }

    /// Records a scope for an expression.
    pub fn record_scope(&mut self, expr_id: ExprId, scope: ScopeKey) {
        self.scopes.insert(expr_id, scope);
    }

    /// Records a scope for a statement (by position).
    pub fn record_stmt_scope(&mut self, pos: usize, scope: ScopeKey) {
        self.stmt_scopes.insert(pos, scope);
    }

    /// Records init order.
    pub fn record_init_order(&mut self, init_order: Vec<Initializer>) {
        self.init_order = init_order;
    }

    /// Records builtin type signature for a builtin function expression.
    /// The expression must be a (possibly parenthesized) identifier denoting a built-in.
    pub fn record_builtin_type(
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
    pub fn record_comma_ok_types(
        &mut self,
        expr: &gox_syntax::ast::Expr,
        types: [TypeKey; 2],
        tuple_type: TypeKey,
    ) {
        use gox_syntax::ast::ExprKind;
        
        let mut e = expr;
        loop {
            if let Some(tv) = self.types.get_mut(&e.id) {
                tv.typ = tuple_type;
            }
            match &e.kind {
                ExprKind::Paren(inner) => e = inner,
                _ => break,
            }
        }
    }

    /// Looks up the type of an expression.
    pub fn expr_type(&self, expr_id: ExprId) -> Option<TypeKey> {
        self.types.get(&expr_id).map(|tv| tv.typ)
    }

    /// Looks up the mode of an expression.
    pub fn expr_mode(&self, expr_id: ExprId) -> Option<&OperandMode> {
        self.types.get(&expr_id).map(|tv| &tv.mode)
    }

    /// Looks up the object for a definition.
    pub fn get_def(&self, ident: &Ident) -> Option<ObjKey> {
        self.defs.get(ident).and_then(|o| *o)
    }

    /// Looks up the object for a use.
    pub fn get_use(&self, ident: &Ident) -> Option<ObjKey> {
        self.uses.get(ident).copied()
    }

    /// Returns true if the identifier is a definition.
    pub fn is_def(&self, ident: &Ident) -> bool {
        self.defs.contains_key(ident)
    }
}
