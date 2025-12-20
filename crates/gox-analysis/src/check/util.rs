//! Utility macros and functions for the type checker.
//!
//! This module provides utility types, macros, and functions used throughout
//! the type checking process.

#![allow(dead_code)]

use std::cmp::Ordering;

use gox_common::span::Span;
use gox_common::vfs::FileSystem;
use gox_common_core::ExprId;
use gox_syntax::ast::{Expr, ExprKind};

use crate::obj::{self, Builtin, LangObj};
use crate::objects::{ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::package::Package;
use crate::scope::{self, Scope};
use crate::typ::{self, BasicType, Type};
use crate::universe::BuiltinInfo;

use super::checker::Checker;

// Re-export type utility functions from typ module
pub use crate::typ::{
    comparable, deep_underlying_type, has_nil, identical, identical_o, is_boolean, is_const_type,
    is_float, is_integer, is_interface, is_named, is_numeric, is_ordered, is_string, is_typed,
    is_unsigned, is_untyped, underlying_type,
};

// =============================================================================
// UnpackResult - for unpacking assignment RHS
// =============================================================================

/// Result of unpacking the right-hand side of an assignment.
#[derive(Debug)]
pub enum UnpackResult<'a> {
    /// RHS is a tuple expression.
    Tuple(Option<ExprId>, Vec<Option<TypeKey>>, Ordering),
    /// RHS returns comma-ok (map index, type assert, channel receive).
    CommaOk(Option<ExprId>, [TypeKey; 2]),
    /// Multiple expressions (N to N assignment).
    Multiple(&'a [Expr], Ordering),
    /// Single expression (1 to 1 assignment).
    Single(Operand, Ordering),
    /// Nothing to unpack.
    Nothing(Ordering),
    /// Error occurred during unpacking.
    Error,
}

impl<'a> UnpackResult<'a> {
    /// Get the i-th value from the unpacked result.
    pub fn get<F: FileSystem>(
        &self,
        checker: &mut Checker<F>,
        x: &mut Operand,
        i: usize,
    ) {
        match self {
            UnpackResult::Tuple(expr_id, types, _) => {
                x.mode = OperandMode::Value;
                x.expr_id = *expr_id;
                x.typ = types[i];
            }
            UnpackResult::CommaOk(expr_id, types) => {
                x.mode = OperandMode::Value;
                x.expr_id = *expr_id;
                x.typ = Some(types[i]);
            }
            UnpackResult::Multiple(exprs, _) => {
                checker.multi_expr(x, &exprs[i]);
            }
            UnpackResult::Single(sx, _) => {
                x.mode = sx.mode.clone();
                x.expr_id = sx.expr_id;
                x.typ = sx.typ;
            }
            UnpackResult::Nothing(_) => unreachable!(),
            UnpackResult::Error => unreachable!(),
        }
    }

    /// Returns the count of RHS values and ordering relative to LHS count.
    pub fn rhs_count(&self) -> (usize, Ordering) {
        match self {
            UnpackResult::Tuple(_, types, ord) => (types.len(), *ord),
            UnpackResult::CommaOk(_, types) => (types.len(), Ordering::Equal),
            UnpackResult::Multiple(exprs, ord) => (exprs.len(), *ord),
            UnpackResult::Single(_, ord) => (1, *ord),
            UnpackResult::Nothing(ord) => (0, *ord),
            UnpackResult::Error => unreachable!(),
        }
    }

    /// Use (type-check) remaining expressions starting from index `from`.
    pub fn use_<F: FileSystem>(
        &self,
        checker: &mut Checker<F>,
        from: usize,
    ) {
        let exprs = match self {
            UnpackResult::Multiple(exprs, _) => exprs,
            _ => return,
        };

        let mut x = Operand::new();
        for i in from..exprs.len() {
            checker.multi_expr(&mut x, &exprs[i]);
        }
    }

    /// Returns true if this is an error result.
    pub fn is_err(&self) -> bool {
        matches!(self, UnpackResult::Error)
    }
}

/// Wrapper for UnpackResult with consumed operands.
#[derive(Debug)]
pub struct UnpackedResultLeftovers<'a> {
    pub leftovers: &'a UnpackResult<'a>,
    pub consumed: Option<&'a Vec<Operand>>,
}

impl<'a> UnpackedResultLeftovers<'a> {
    pub fn new(
        re: &'a UnpackResult<'a>,
        consumed: Option<&'a Vec<Operand>>,
    ) -> UnpackedResultLeftovers<'a> {
        UnpackedResultLeftovers {
            leftovers: re,
            consumed,
        }
    }

    /// Use all remaining values.
    pub fn use_all<F: FileSystem>(&self, checker: &mut Checker<F>) {
        let from = self.consumed.map_or(0, |c| c.len());
        self.leftovers.use_(checker, from);
    }

    /// Get the i-th value, considering already consumed operands.
    pub fn get<F: FileSystem>(
        &self,
        checker: &mut Checker<F>,
        x: &mut Operand,
        i: usize,
    ) {
        if let Some(consumed) = self.consumed {
            if i < consumed.len() {
                let c = &consumed[i];
                x.mode = c.mode.clone();
                x.expr_id = c.expr_id;
                x.typ = c.typ;
                return;
            }
        }
        self.leftovers.get(checker, x, i);
    }
}

// =============================================================================
// Checker utility methods
// =============================================================================

impl<F: FileSystem> Checker<F> {
    /// Remove parentheses from an expression.
    pub fn unparen(expr: &Expr) -> &Expr {
        match &expr.kind {
            ExprKind::Paren(inner) => Self::unparen(inner),
            _ => expr,
        }
    }

    /// Report an invalid AST error.
    pub fn invalid_ast(&self, span: Span, err: &str) {
        self.error(span, format!("invalid AST: {}", err));
    }

    /// Report an invalid argument error.
    pub fn invalid_arg(&self, span: Span, err: &str) {
        self.error(span, format!("invalid argument: {}", err));
    }

    /// Report an invalid operation error.
    pub fn invalid_op(&self, span: Span, err: &str) {
        self.error(span, format!("invalid operation: {}", err));
    }

    /// Format object path as string for error messages.
    pub fn obj_path_str(&self, path: &[ObjKey]) -> String {
        let names: Vec<&str> = path.iter().map(|p| self.lobj(*p).name()).collect();
        names.join("->")
    }

    /// Check if obj appears in path (cycle detection).
    /// If report is true, also reports a cycle error.
    pub fn has_cycle(&self, okey: ObjKey, path: &[ObjKey], report: bool) -> bool {
        if let Some((i, _)) = path.iter().enumerate().find(|(_, &x)| x == okey) {
            if report {
                let obj_val = self.lobj(okey);
                self.error(
                    Span::dummy(), // TODO: get span from obj
                    format!("illegal cycle in declaration of {}", obj_val.name()),
                );
                // Print cycle
                for o in path[i..].iter() {
                    let oval = self.lobj(*o);
                    self.error(Span::dummy(), format!("\t{} refers to", oval.name()));
                }
                self.error(Span::dummy(), format!("\t{}", obj_val.name()));
            }
            return true;
        }
        false
    }

    /// Create a comma-ok tuple type.
    pub fn comma_ok_type(
        tc_objs: &mut TCObjects,
        pkg: PackageKey,
        t: &[TypeKey; 2],
    ) -> TypeKey {
        let vars = vec![
            tc_objs.lobjs.insert(obj::LangObj::new_var(
                0,
                Some(pkg),
                String::new(),
                Some(t[0]),
            )),
            tc_objs.lobjs.insert(obj::LangObj::new_var(
                0,
                Some(pkg),
                String::new(),
                Some(t[1]),
            )),
        ];
        // TODO: Implement new_t_tuple in TCObjects
        tc_objs.types.insert(typ::Type::Tuple(typ::TupleDetail::new(vars)))
    }

    /// Unpack the RHS of an assignment.
    pub fn unpack<'b>(
        &mut self,
        rhs: &'b [Expr],
        lhs_len: usize,
        allow_comma_ok: bool,
        variadic: bool,
    ) -> UnpackResult<'b> {
        let do_match = |rhs_len: usize| {
            let order = rhs_len.cmp(&lhs_len);
            if variadic && order == Ordering::Greater {
                Ordering::Equal
            } else {
                order
            }
        };

        if rhs.len() != 1 {
            let matching = do_match(rhs.len());
            return if rhs.is_empty() {
                UnpackResult::Nothing(matching)
            } else {
                UnpackResult::Multiple(rhs, matching)
            };
        }

        let mut x = Operand::new();
        self.multi_expr(&mut x, &rhs[0]);
        if x.invalid() {
            return UnpackResult::Error;
        }

        if let Some(t) = self.otype(x.typ.unwrap()).try_as_tuple() {
            let types: Vec<Option<TypeKey>> =
                t.vars().iter().map(|x| self.lobj(*x).typ()).collect();
            let matching = do_match(types.len());
            return UnpackResult::Tuple(x.expr_id, types, matching);
        } else if x.mode == OperandMode::MapIndex || x.mode == OperandMode::CommaOk {
            if allow_comma_ok {
                let types = [x.typ.unwrap(), self.basic_type(BasicType::UntypedBool)];
                return UnpackResult::CommaOk(x.expr_id, types);
            }
            x.mode = OperandMode::Value;
        }

        UnpackResult::Single(x, do_match(1))
    }

    /// Type-check a list of expressions (for side effects).
    pub fn use_exprs(&mut self, exprs: &[Expr]) {
        let mut x = Operand::new();
        for e in exprs.iter() {
            self.raw_expr(&mut x, e, None);
        }
    }

    /// Like use_exprs, but doesn't "use" top-level identifiers.
    /// Used for LHS of assignments.
    pub fn use_lhs(&mut self, lhs: &[Expr]) {
        let mut x = Operand::new();
        for e in lhs.iter() {
            let v: Option<(ObjKey, bool)> = match Self::unparen(e) {
                Expr { kind: ExprKind::Ident(ident), .. } => {
                    if ident.symbol.is_dummy() {
                        continue;
                    }
                    // TODO: Check if identifier is "_"
                    // Look up in scope and track used state
                    None
                }
                _ => None,
            };

            self.raw_expr(&mut x, e, None);

            // Restore used state if needed
            if let Some((_okey, _used)) = v {
                // TODO: Restore used state
            }
        }
    }

    /// Look up an identifier in the current scope.
    pub fn lookup(&self, name: &str) -> Option<ObjKey> {
        let scope_key = self.octx.scope?;
        if let Some(pos) = self.octx.pos {
            scope::lookup_parent_at(scope_key, name, pos, &self.tc_objs)
        } else {
            scope::lookup_parent(scope_key, name, &self.tc_objs)
        }
        .map(|(_, okey)| okey)
    }

    /// Add a dependency from the current declaration to `to`.
    pub fn add_decl_dep(&mut self, to: ObjKey) {
        let decl_key = match self.octx.decl {
            Some(k) => k,
            None => return,
        };
        if !self.obj_map.contains_key(&to) {
            return;
        }
        // TODO: Implement dependency tracking in DeclInfo
        let _ = decl_key;
        let _ = to;
    }

    // =========================================================================
    // Accessor methods
    // =========================================================================

    /// Get a language object by key.
    pub fn lobj(&self, key: ObjKey) -> &LangObj {
        &self.tc_objs.lobjs[key]
    }

    /// Get a mutable language object by key.
    pub fn lobj_mut(&mut self, key: ObjKey) -> &mut LangObj {
        &mut self.tc_objs.lobjs[key]
    }

    /// Get a type by key.
    pub fn otype(&self, key: TypeKey) -> &Type {
        &self.tc_objs.types[key]
    }

    /// Get a mutable type by key.
    pub fn otype_mut(&mut self, key: TypeKey) -> &mut Type {
        &mut self.tc_objs.types[key]
    }

    /// Get an interface type by key.
    pub fn otype_interface(&self, key: TypeKey) -> &typ::InterfaceDetail {
        self.otype(key).try_as_interface().unwrap()
    }

    /// Get a signature type by key.
    pub fn otype_signature(&self, key: TypeKey) -> &typ::SignatureDetail {
        self.otype(key).try_as_signature().unwrap()
    }

    /// Get a mutable interface type by key.
    pub fn otype_interface_mut(&mut self, key: TypeKey) -> &mut typ::InterfaceDetail {
        self.otype_mut(key).try_as_interface_mut().unwrap()
    }

    /// Get a mutable signature type by key.
    pub fn otype_signature_mut(&mut self, key: TypeKey) -> &mut typ::SignatureDetail {
        self.otype_mut(key).try_as_signature_mut().unwrap()
    }

    /// Get a package by key.
    pub fn package(&self, key: PackageKey) -> &Package {
        &self.tc_objs.pkgs[key]
    }

    /// Get a mutable package by key.
    pub fn package_mut(&mut self, key: PackageKey) -> &mut Package {
        &mut self.tc_objs.pkgs[key]
    }

    /// Get a scope by key.
    pub fn scope(&self, key: ScopeKey) -> &Scope {
        &self.tc_objs.scopes[key]
    }

    // Note: decl_info removed - DeclInfo is in resolver module, not objects

    /// Get builtin function info.
    pub fn builtin_info(&self, id: Builtin) -> &BuiltinInfo {
        &self.universe().builtins()[&id]
    }

    /// Get a basic type by kind.
    pub fn basic_type(&self, t: BasicType) -> TypeKey {
        self.universe().types()[&t]
    }

    /// Get the invalid type.
    pub fn invalid_type(&self) -> TypeKey {
        self.basic_type(BasicType::Invalid)
    }
}

// =============================================================================
// Type utility functions
// =============================================================================

/// Returns true if the type is a basic type of the given kind.
pub fn is_basic(typ: TypeKey, kind: BasicType, objs: &TCObjects) -> bool {
    match &objs.types[typ] {
        Type::Basic(detail) => detail.typ() == kind,
        _ => false,
    }
}

/// Returns the element type of a slice, array, map, channel, or pointer.
pub fn elem_type(typ: TypeKey, objs: &TCObjects) -> Option<TypeKey> {
    match &objs.types[typ] {
        Type::Array(d) => Some(d.elem()),
        Type::Slice(d) => Some(d.elem()),
        Type::Map(d) => Some(d.elem()),
        Type::Chan(d) => Some(d.elem()),
        Type::Pointer(d) => Some(d.base()),
        _ => None,
    }
}
