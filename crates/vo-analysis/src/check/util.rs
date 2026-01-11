//! Utility macros and functions for the type checker.
//!
//! This module provides utility types, macros, and functions used throughout
//! the type checking process.


use std::cmp::Ordering;

use vo_common::span::Span;
use vo_syntax::ast::{Expr, ExprKind};

use crate::obj::{self, Builtin, LangObj};
use crate::objects::{DeclInfoKey, ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::scope;
use crate::typ::{self, BasicType, Type};
use crate::universe::BuiltinInfo;

use super::checker::Checker;
use super::errors::TypeError;

// =============================================================================
// UnpackResult - for unpacking assignment RHS
// =============================================================================

/// Result of unpacking the right-hand side of an assignment.
#[derive(Debug)]
pub enum UnpackResult<'a> {
    /// RHS is a tuple expression.
    Tuple(Option<&'a Expr>, Vec<Option<TypeKey>>, Ordering),
    /// RHS returns comma-ok (map index, type assert, channel receive).
    CommaOk(Option<&'a Expr>, [TypeKey; 2]),
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
    pub(crate) fn get(
        &self,
        checker: &mut Checker,
        x: &mut Operand,
        i: usize,
    ) {
        match self {
            UnpackResult::Tuple(expr, types, _) => {
                x.mode = OperandMode::Value;
                if let Some(e) = expr {
                    x.set_expr(e);
                }
                x.typ = types[i];
            }
            UnpackResult::CommaOk(expr, types) => {
                x.mode = OperandMode::Value;
                if let Some(e) = expr {
                    x.set_expr(e);
                }
                x.typ = Some(types[i]);
            }
            UnpackResult::Multiple(exprs, _) => {
                checker.multi_expr(x, &exprs[i]);
            }
            UnpackResult::Single(sx, _) => {
                x.mode = sx.mode.clone();
                x.expr = sx.expr;
                x.typ = sx.typ;
            }
            UnpackResult::Nothing(_) => unreachable!(),
            UnpackResult::Error => unreachable!(),
        }
    }

    /// Returns the count of RHS values and ordering relative to LHS count.
    pub(crate) fn rhs_count(&self) -> (usize, Ordering) {
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
    pub(crate) fn use_(
        &self,
        checker: &mut Checker,
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
    pub(crate) fn is_err(&self) -> bool {
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
    pub(crate) fn new(
        re: &'a UnpackResult<'a>,
        consumed: Option<&'a Vec<Operand>>,
    ) -> UnpackedResultLeftovers<'a> {
        UnpackedResultLeftovers {
            leftovers: re,
            consumed,
        }
    }

    /// Use all remaining values.
    pub(crate) fn use_all(&self, checker: &mut Checker) {
        let from = self.consumed.map_or(0, |c| c.len());
        self.leftovers.use_(checker, from);
    }

    /// Get the i-th value, considering already consumed operands.
    pub(crate) fn get(
        &self,
        checker: &mut Checker,
        x: &mut Operand,
        i: usize,
    ) {
        if let Some(consumed) = self.consumed {
            if i < consumed.len() {
                let c = &consumed[i];
                x.mode = c.mode.clone();
                x.expr = c.expr;
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

impl Checker {
    /// Remove parentheses from an expression.
    pub(crate) fn unparen(expr: &Expr) -> &Expr {
        match &expr.kind {
            ExprKind::Paren(inner) => Self::unparen(inner),
            _ => expr,
        }
    }

    /// Report an invalid AST error.
    pub(crate) fn invalid_ast(&self, span: Span, err: &str) {
        self.error_code_msg(TypeError::InvalidOp, span, format!("invalid AST: {}", err));
    }

    /// Report an invalid argument error.
    pub(crate) fn invalid_arg(&self, span: Span, err: &str) {
        self.error_code_msg(TypeError::InvalidOp, span, format!("invalid argument: {}", err));
    }

    /// Report an invalid operation error.
    pub(crate) fn invalid_op(&self, span: Span, err: &str) {
        self.error_code_msg(TypeError::InvalidOp, span, format!("invalid operation: {}", err));
    }

    /// Format object path as string for error messages.
    pub(crate) fn obj_path_str(&self, path: &[ObjKey]) -> String {
        let names: Vec<&str> = path.iter().map(|p| self.lobj(*p).name()).collect();
        names.join("->")
    }

    /// Check if obj appears in path (cycle detection).
    /// If report is true, also reports a cycle error.
    pub(crate) fn has_cycle(&self, okey: ObjKey, path: &[ObjKey], report: bool) -> bool {
        if let Some((i, _)) = path.iter().enumerate().find(|(_, &x)| x == okey) {
            if report {
                let obj_val = self.lobj(okey);
                let pos = obj_val.pos();
                let span = Span::new(vo_common::BytePos(pos as u32), vo_common::BytePos(pos as u32));
                self.error_code_msg(
                    TypeError::IllegalCycle,
                    span,
                    format!("illegal cycle in declaration of {}", obj_val.name()),
                );
                // Print cycle
                for o in path[i..].iter() {
                    let oval = self.lobj(*o);
                    let pos = oval.pos();
                    let span = Span::new(vo_common::BytePos(pos as u32), vo_common::BytePos(pos as u32));
                    self.error_code_msg(TypeError::RefersTo, span, format!("\t{} refers to", oval.name()));
                }
                let pos = obj_val.pos();
                let span = Span::new(vo_common::BytePos(pos as u32), vo_common::BytePos(pos as u32));
                self.error_code_msg(TypeError::RefersTo, span, format!("\t{}", obj_val.name()));
            }
            return true;
        }
        false
    }

    /// Unpack the RHS of an assignment.
    pub(crate) fn unpack<'b>(
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
            return UnpackResult::Tuple(Some(&rhs[0]), types, matching);
        } else if x.mode == OperandMode::MapIndex || x.mode == OperandMode::CommaOk {
            if allow_comma_ok {
                let types = [x.typ.unwrap(), self.basic_type(BasicType::UntypedBool)];
                return UnpackResult::CommaOk(Some(&rhs[0]), types);
            }
            x.mode = OperandMode::Value;
        }

        UnpackResult::Single(x, do_match(1))
    }

    /// Type-check a list of expressions (for side effects).
    pub(crate) fn use_exprs(&mut self, exprs: &[Expr]) {
        let mut x = Operand::new();
        for e in exprs.iter() {
            self.raw_expr(&mut x, e, None);
        }
    }

    /// Like use_exprs, but doesn't "use" top-level identifiers.
    /// Used for LHS of assignments.
    pub(crate) fn use_lhs(&mut self, lhs: &[Expr]) {
        let mut x = Operand::new();
        for e in lhs.iter() {
            let v: Option<(ObjKey, bool)> = match Self::unparen(e) {
                Expr { kind: ExprKind::Ident(ident), .. } => {
                    if ident.symbol.is_dummy() {
                        continue;
                    }
                    let name = self.resolve_ident(ident);
                    if name == "_" {
                        continue;
                    }
                    // Look up in scope and track used state
                    self.lookup(name).and_then(|okey| {
                        let lobj = self.lobj(okey);
                        if lobj.pkg() == Some(self.pkg) {
                            if let Some(used) = lobj.var_used() {
                                return Some((okey, used));
                            }
                        }
                        None
                    })
                }
                _ => None,
            };

            self.raw_expr(&mut x, e, None);

            // Restore used state if needed
            if let Some((okey, used)) = v {
                self.lobj_mut(okey).set_var_used(used);
            }
        }
    }

    /// Look up an identifier in the current scope.
    pub(crate) fn lookup(&self, name: &str) -> Option<ObjKey> {
        let scope_key = self.octx.scope?;
        if let Some(pos) = self.octx.pos {
            scope::lookup_parent_at(scope_key, name, pos, self.objs())
        } else {
            scope::lookup_parent(scope_key, name, self.objs())
        }
        .map(|(_, okey)| okey)
    }

    /// Add a dependency from the current declaration to `to`.
    pub(crate) fn add_decl_dep(&mut self, to: ObjKey) {
        let decl_key = match self.octx.decl {
            Some(k) => k,
            None => return,
        };
        if !self.obj_map.contains_key(&to) {
            return;
        }
        self.decl_info_mut(decl_key).add_dep(to);
    }

    // =========================================================================
    // Accessor methods
    // =========================================================================

    /// Get a language object by key.
    pub(crate) fn lobj(&self, key: ObjKey) -> &LangObj {
        &self.tc_objs.lobjs[key]
    }

    /// Get a mutable language object by key.
    pub(crate) fn lobj_mut(&mut self, key: ObjKey) -> &mut LangObj {
        &mut self.tc_objs.lobjs[key]
    }

    /// Get a type by key.
    pub(crate) fn otype(&self, key: TypeKey) -> &Type {
        &self.tc_objs.types[key]
    }

    /// Get a mutable type by key.
    pub(crate) fn otype_mut(&mut self, key: TypeKey) -> &mut Type {
        &mut self.tc_objs.types[key]
    }

    /// Get an interface type by key.
    pub(crate) fn otype_interface(&self, key: TypeKey) -> &typ::InterfaceDetail {
        self.otype(key).try_as_interface().unwrap()
    }

    /// Get a signature type by key.
    pub(crate) fn otype_signature(&self, key: TypeKey) -> &typ::SignatureDetail {
        self.otype(key).try_as_signature().unwrap()
    }

    /// Get a mutable interface type by key.
    pub(crate) fn otype_interface_mut(&mut self, key: TypeKey) -> &mut typ::InterfaceDetail {
        self.otype_mut(key).try_as_interface_mut().unwrap()
    }

    /// Get a mutable signature type by key.
    pub(crate) fn otype_signature_mut(&mut self, key: TypeKey) -> &mut typ::SignatureDetail {
        self.otype_mut(key).try_as_signature_mut().unwrap()
    }

    // Note: package, package_mut, scope are in checker.rs

    /// Get builtin function info.
    pub(crate) fn builtin_info(&self, id: Builtin) -> &BuiltinInfo {
        &self.universe().builtins()[&id]
    }

    /// Get a basic type by kind.
    pub(crate) fn basic_type(&self, t: BasicType) -> TypeKey {
        self.universe().types()[&t]
    }

    /// Get the invalid type.
    pub(crate) fn invalid_type(&self) -> TypeKey {
        self.basic_type(BasicType::Invalid)
    }
    
    // =========================================================================
    // TCObjects access helpers
    // =========================================================================
    
    /// Get a reference to the TCObjects.
    #[inline]
    pub(crate) fn objs(&self) -> &TCObjects {
        &self.tc_objs
    }
    
    /// Get a mutable reference to the TCObjects.
    #[inline]
    pub(crate) fn objs_mut(&mut self) -> &mut TCObjects {
        &mut self.tc_objs
    }
    
    /// Insert a declaration info and return its key.
    #[inline]
    pub(crate) fn insert_decl(&mut self, decl: super::resolver::DeclInfo) -> DeclInfoKey {
        self.tc_objs.decls.insert(decl)
    }
    
    // Factory method wrappers
    
    #[inline]
    pub(crate) fn new_scope(&mut self, parent: Option<ScopeKey>, pos: usize, end: usize, comment: &str, is_func: bool) -> ScopeKey {
        self.tc_objs.new_scope(parent, pos, end, comment, is_func)
    }
    
    #[inline]
    pub(crate) fn new_package(&mut self, path: String) -> PackageKey {
        self.tc_objs.new_package(path)
    }
    
    #[inline]
    pub(crate) fn new_pkg_name(&mut self, span: Span, pkg: Option<PackageKey>, name: String, imported: PackageKey) -> ObjKey {
        self.tc_objs.new_pkg_name(span, pkg, name, imported)
    }
    
    #[inline]
    pub(crate) fn new_const(&mut self, span: Span, pkg: Option<PackageKey>, name: String, typ: Option<TypeKey>, val: crate::constant::Value) -> ObjKey {
        self.tc_objs.new_const(span, pkg, name, typ, val)
    }
    
    #[inline]
    pub(crate) fn new_var(&mut self, span: Span, pkg: Option<PackageKey>, name: String, typ: Option<TypeKey>) -> ObjKey {
        self.tc_objs.new_var(span, pkg, name, typ)
    }
    
    #[inline]
    pub(crate) fn new_param_var(&mut self, span: Span, pkg: Option<PackageKey>, name: String, typ: Option<TypeKey>) -> ObjKey {
        self.tc_objs.new_param_var(span, pkg, name, typ)
    }
    
    #[inline]
    pub(crate) fn new_field(&mut self, span: Span, pkg: Option<PackageKey>, name: String, typ: Option<TypeKey>, embedded: bool) -> ObjKey {
        self.tc_objs.new_field(span, pkg, name, typ, embedded)
    }
    
    #[inline]
    pub(crate) fn new_func(&mut self, span: Span, pkg: Option<PackageKey>, name: String, typ: Option<TypeKey>, has_body: bool) -> ObjKey {
        self.tc_objs.new_func(span, pkg, name, typ, has_body)
    }
    
    #[inline]
    pub(crate) fn new_type_name(&mut self, span: Span, pkg: Option<PackageKey>, name: String, typ: Option<TypeKey>) -> ObjKey {
        self.tc_objs.new_type_name(span, pkg, name, typ)
    }
    
    #[inline]
    pub(crate) fn new_label(&mut self, span: Span, pkg: Option<PackageKey>, name: String) -> ObjKey {
        self.tc_objs.new_label(span, pkg, name)
    }
    
    // Type creation wrappers
    
    #[inline]
    pub(crate) fn new_t_array(&mut self, elem: TypeKey, len: Option<u64>) -> TypeKey {
        self.tc_objs.new_t_array(elem, len)
    }
    
    #[inline]
    pub(crate) fn new_t_slice(&mut self, elem: TypeKey) -> TypeKey {
        self.tc_objs.new_t_slice(elem)
    }
    
    #[inline]
    pub(crate) fn new_t_map(&mut self, key: TypeKey, value: TypeKey) -> TypeKey {
        self.tc_objs.new_t_map(key, value)
    }
    
    #[inline]
    pub(crate) fn new_t_chan(&mut self, dir: typ::ChanDir, elem: TypeKey) -> TypeKey {
        self.tc_objs.new_t_chan(dir, elem)
    }
    
    #[inline]
    pub(crate) fn new_t_pointer(&mut self, base: TypeKey) -> TypeKey {
        self.tc_objs.new_t_pointer(base)
    }
    
    /// Create pointer type with validation that base is struct.
    /// Returns invalid_type and reports error if base is not struct.
    pub(crate) fn new_t_pointer_checked(&mut self, base: TypeKey, span: vo_common::span::Span) -> TypeKey {
        let invalid_type = self.invalid_type();
        if base == invalid_type {
            return invalid_type;
        }
        let underlying = crate::typ::underlying_type(base, self.objs());
        if self.otype(underlying).try_as_struct().is_none() {
            self.error_code_msg(
                super::errors::TypeError::PointerToNonStruct,
                span,
                format!(
                    "invalid pointer type *{} (pointer base must be struct)",
                    self.type_str(base)
                ),
            );
            return invalid_type;
        }
        self.tc_objs.new_t_pointer(base)
    }
    
    #[inline]
    pub(crate) fn new_t_tuple(&mut self, vars: Vec<ObjKey>) -> TypeKey {
        self.tc_objs.new_t_tuple(vars)
    }
    
    #[inline]
    pub(crate) fn new_t_struct(&mut self, fields: Vec<ObjKey>, tags: Option<Vec<Option<String>>>) -> TypeKey {
        self.tc_objs.new_t_struct(fields, tags)
    }
    
    #[inline]
    pub(crate) fn new_t_signature(
        &mut self,
        scope: Option<ScopeKey>,
        recv: Option<ObjKey>,
        params: TypeKey,
        results: TypeKey,
        variadic: bool,
    ) -> TypeKey {
        self.tc_objs.new_t_signature(scope, recv, params, results, variadic)
    }
    
    #[inline]
    pub(crate) fn new_t_interface(&mut self, methods: Vec<ObjKey>, embeddeds: Vec<TypeKey>) -> TypeKey {
        self.tc_objs.new_t_interface(methods, embeddeds)
    }
    
    #[inline]
    pub(crate) fn new_t_empty_interface(&mut self) -> TypeKey {
        self.tc_objs.new_t_empty_interface()
    }
    
    #[inline]
    pub(crate) fn new_t_named(&mut self, obj: Option<ObjKey>, underlying: Option<TypeKey>, methods: Vec<ObjKey>) -> TypeKey {
        self.tc_objs.new_t_named(obj, underlying, methods)
    }
    
    /// Format a type as a string for error messages.
    pub(crate) fn type_str(&self, t: TypeKey) -> String {
        crate::display::type_string(t, &self.tc_objs)
    }
    
    /// Get span from an object.
    pub(crate) fn obj_span(&self, okey: ObjKey) -> Span {
        self.lobj(okey).span()
    }
}
