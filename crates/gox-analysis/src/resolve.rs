//! Phase 2: Type Resolution
//!
//! This module resolves all type expressions and computes derived type information.
//! It is the second phase of the three-phase type checking pipeline.
//!
//! # Responsibilities
//!
//! - **Type resolution**: Resolves all type expressions (e.g., `[]int`, `map[string]T`)
//!   to internal [`Type`] representations
//! - **Named type resolution**: Resolves named types to their underlying types while
//!   detecting illegal type cycles (e.g., `type A A`)
//! - **Function signature resolution**: Resolves parameter and return types for all functions
//! - **Variable type resolution**: Resolves declared types for package-level variables
//! - **Method association**: Associates method declarations with their receiver types
//!   and builds method sets
//! - **Interface expansion**: Expands embedded interfaces and computes full method sets
//! - **Validation**: Validates map key comparability and array length constants
//!
//! # Cycle Detection
//!
//! The resolver detects illegal type cycles using a `resolving` set to track types
//! currently being resolved. Cycles through slices, maps, and channels are
//! allowed (they break the cycle), but direct cycles like `type A A` are errors.
//!
//! # Output
//!
//! Returns a [`ResolveResult`] containing:
//! - The updated package-level [`Scope`] with resolved types
//! - A vector of [`NamedTypeInfo`] with fully resolved underlying types and methods
//!
//! # Example
//!
//! ```ignore
//! use gox_analysis::resolve::TypeResolver;
//!
//! let resolver = TypeResolver::new(collect_result, &interner, &mut diag);
//! let result = resolver.resolve();
//! // result.named_types[id] contains the resolved type info
//! ```

use std::collections::HashSet;

use gox_common::{DiagnosticSink, Span, Symbol, SymbolInterner};
use gox_syntax::ast::{self, TypeExprKind};

use crate::collect::{
    CollectResult, FuncSigPlaceholder, MethodPlaceholder, NamedTypePlaceholder, VarTypePlaceholder,
};
use crate::constant::Constant;
use crate::errors::TypeError;
use crate::scope::{Entity, Scope};
use crate::types::BasicType;
use crate::types::{
    ArrayType, ChanDir, ChanType, Field, FuncType, InterfaceType, MapType, Method, NamedTypeId,
    NamedTypeInfo, SliceType, StructType, Type,
};

/// The result of Phase 2 type resolution.
pub struct ResolveResult {
    /// The package-level scope (updated with resolved types).
    pub scope: Scope,
    /// Resolved named types (id -> full info).
    pub named_types: Vec<NamedTypeInfo>,
}

/// Type resolver for Phase 2.
pub struct TypeResolver<'a> {
    /// Symbol interner for resolving names.
    interner: &'a SymbolInterner,
    /// Diagnostics sink.
    diagnostics: &'a mut DiagnosticSink,
    /// Package-level scope from Phase 1.
    scope: Scope,
    /// Named type placeholders from Phase 1.
    placeholders: Vec<NamedTypePlaceholder>,
    /// Method placeholders from Phase 1.
    methods: Vec<MethodPlaceholder>,
    /// Variable type placeholders from Phase 1.
    var_types: Vec<VarTypePlaceholder>,
    /// Function signature placeholders from Phase 1.
    func_sigs: Vec<FuncSigPlaceholder>,
    /// Resolved named types.
    named_types: Vec<NamedTypeInfo>,
    /// Types currently being resolved (for cycle detection).
    resolving: HashSet<NamedTypeId>,
    /// Whether we're inside an object/slice/map/interface (breaks cycles).
    in_indirect: bool,
}

impl<'a> TypeResolver<'a> {
    /// Creates a new type resolver from Phase 1 results.
    pub fn new(
        collect_result: CollectResult,
        interner: &'a SymbolInterner,
        diagnostics: &'a mut DiagnosticSink,
    ) -> Self {
        let placeholder_count = collect_result.named_types.len();
        Self {
            interner,
            diagnostics,
            scope: collect_result.scope,
            placeholders: collect_result.named_types,
            methods: collect_result.methods,
            var_types: collect_result.var_types,
            func_sigs: collect_result.func_sigs,
            named_types: Vec::with_capacity(placeholder_count),
            resolving: HashSet::new(),
            in_indirect: false,
        }
    }

    /// Resolves all types and returns the result.
    pub fn resolve(mut self) -> ResolveResult {
        // Initialize named_types with placeholders
        for placeholder in &self.placeholders {
            self.named_types.push(NamedTypeInfo {
                name: placeholder.name,
                underlying: Type::Invalid,
                methods: Vec::new(),
            });
        }

        // Resolve each named type
        for i in 0..self.placeholders.len() {
            self.resolve_named_type(NamedTypeId(i as u32));
        }

        // Resolve function signatures BEFORE variable types
        // (so function return types are known when inferring var types)
        self.resolve_func_sigs();

        // Resolve variable types (may depend on function return types)
        self.resolve_var_types();

        // Resolve methods and associate them with their receiver types
        self.resolve_methods();

        ResolveResult {
            scope: self.scope,
            named_types: self.named_types,
        }
    }

    /// Resolves variable types and updates the scope.
    fn resolve_var_types(&mut self) {
        let var_types = std::mem::take(&mut self.var_types);

        for var in var_types {
            if let Some(ref ty_expr) = var.ty {
                let resolved_ty = self.resolve_type_expr(ty_expr);
                // Update the variable in scope
                if let Some(Entity::Var(v)) = self.scope.lookup_local_mut(var.name) {
                    v.ty = resolved_ty;
                }
            } else if let Some(ref init_expr) = var.init_expr {
                // Try to infer type from initializer (now that function sigs are resolved)
                let current_ty = if let Some(Entity::Var(v)) = self.scope.lookup(var.name) {
                    v.ty.clone()
                } else {
                    continue;
                };

                if current_ty == Type::Invalid {
                    let inferred_ty = self.infer_type_from_expr(init_expr);
                    if inferred_ty != Type::Invalid {
                        if let Some(Entity::Var(v)) = self.scope.lookup_local_mut(var.name) {
                            v.ty = inferred_ty;
                        }
                    }
                }
            }
        }
    }

    /// Infer type from expression (for var initialization after function signatures are resolved)
    fn infer_type_from_expr(&self, expr: &ast::Expr) -> Type {
        use gox_syntax::ast::ExprKind;
        match &expr.kind {
            ExprKind::IntLit(_) => Type::Basic(BasicType::Int),
            ExprKind::FloatLit(_) => Type::Basic(BasicType::Float64),
            ExprKind::StringLit(_) => Type::Basic(BasicType::String),
            ExprKind::Ident(ident) => {
                if let Some(Entity::Var(var)) = self.scope.lookup(ident.symbol) {
                    return var.ty.clone();
                }
                Type::Invalid
            }
            ExprKind::Binary(bin) => {
                let left_ty = self.infer_type_from_expr(&bin.left);
                if left_ty != Type::Invalid {
                    left_ty
                } else {
                    self.infer_type_from_expr(&bin.right)
                }
            }
            ExprKind::Unary(un) => self.infer_type_from_expr(&un.operand),
            ExprKind::Paren(inner) => self.infer_type_from_expr(inner),
            ExprKind::Call(call) => {
                if let ExprKind::Ident(func_ident) = &call.func.kind {
                    // Look up function return type
                    if let Some(Entity::Func(func)) = self.scope.lookup(func_ident.symbol) {
                        if !func.sig.results.is_empty() {
                            return func.sig.results[0].clone();
                        }
                    }
                }
                Type::Invalid
            }
            _ => Type::Invalid,
        }
    }

    /// Resolves function signatures and updates the scope.
    fn resolve_func_sigs(&mut self) {
        let func_sigs = std::mem::take(&mut self.func_sigs);

        for func in func_sigs {
            let resolved_sig = self.resolve_func_sig(&func.sig);
            // Update the function in scope
            if let Some(Entity::Func(f)) = self.scope.lookup_local_mut(func.name) {
                f.sig = resolved_sig;
            }
        }
    }

    /// Resolves methods and associates them with their receiver types.
    fn resolve_methods(&mut self) {
        // Clone methods to avoid borrow issues
        let methods = std::mem::take(&mut self.methods);

        for method in methods {
            // Find the receiver type
            let receiver_type_id = self.find_named_type_by_name(method.receiver_type);

            if let Some(type_id) = receiver_type_id {
                // Resolve the method signature
                let sig = self.resolve_func_sig(&method.decl.sig);

                let method_info = Method {
                    name: method.name,
                    sig,
                };

                // Add method to the type's method set
                let idx = type_id.0 as usize;
                if idx < self.named_types.len() {
                    self.named_types[idx].methods.push(method_info);
                }
            } else {
                // Receiver type not found
                let name = self
                    .interner
                    .resolve(method.receiver_type)
                    .unwrap_or("<unknown>");
                let span = method.decl.receiver.as_ref().map(|r| r.ty.span).unwrap_or(method.decl.span);
                self.diagnostics.emit(
                    TypeError::UndefinedReceiverType
                        .at_with_message(span, TypeError::UndefinedReceiverType.with_name(name)),
                );
            }
        }
    }

    /// Finds a named type by its name symbol.
    fn find_named_type_by_name(&self, name: Symbol) -> Option<NamedTypeId> {
        for (i, info) in self.named_types.iter().enumerate() {
            if info.name == name {
                return Some(NamedTypeId(i as u32));
            }
        }
        None
    }

    /// Resolves a function signature.
    fn resolve_func_sig(&mut self, sig: &ast::FuncSig) -> FuncType {
        let params: Vec<Type> = sig
            .params
            .iter()
            .flat_map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                std::iter::repeat_n(ty, p.names.len().max(1))
            })
            .collect();

        let results: Vec<Type> = sig
            .results
            .iter()
            .map(|r| self.resolve_type_expr(&r.ty))
            .collect();

        FuncType {
            params,
            results,
            variadic: sig.variadic,
        }
    }

    /// Resolves a named type by its ID.
    fn resolve_named_type(&mut self, id: NamedTypeId) -> Type {
        let idx = id.0 as usize;

        // Already resolved?
        if self.named_types[idx].underlying != Type::Invalid {
            return Type::Named(id);
        }

        // Cycle detection
        if self.resolving.contains(&id) {
            if !self.in_indirect {
                let name = self.placeholders[idx].name;
                let span = self.placeholders[idx].span;
                let name_str = self.interner.resolve(name).unwrap_or("<unknown>");
                self.diagnostics.emit(
                    TypeError::InvalidRecursiveType
                        .at_with_message(span, TypeError::InvalidRecursiveType.with_name(name_str)),
                );
            }
            return Type::Invalid;
        }

        self.resolving.insert(id);

        // Clone the AST type to avoid borrow issues
        let ast_type = self.placeholders[idx].ast_type.clone();
        let underlying = self.resolve_type_expr(&ast_type);

        self.resolving.remove(&id);

        // Store the resolved type
        self.named_types[idx].underlying = underlying.clone();

        Type::Named(id)
    }

    /// Resolves an AST type expression to an internal Type.
    fn resolve_type_expr(&mut self, expr: &ast::TypeExpr) -> Type {
        match &expr.kind {
            TypeExprKind::Ident(ident) => self.resolve_type_name(ident.symbol, ident.span),

            TypeExprKind::Selector(sel) => {
                // Qualified type: pkg.Type - resolve as external type reference
                // For now, treat as Invalid since cross-package types need special handling
                let _pkg_name = self.interner.resolve(sel.pkg.symbol).unwrap_or("");
                let _type_name = self.interner.resolve(sel.sel.symbol).unwrap_or("");
                // TODO: Proper cross-package type resolution
                Type::Invalid
            }

            TypeExprKind::Array(arr) => {
                let len = self.eval_array_length(&arr.len);
                let elem = self.resolve_type_expr(&arr.elem);
                Type::Array(ArrayType {
                    len,
                    elem: Box::new(elem),
                })
            }

            TypeExprKind::Slice(elem) => {
                let was_indirect = self.in_indirect;
                self.in_indirect = true;
                let elem_type = self.resolve_type_expr(elem);
                self.in_indirect = was_indirect;
                Type::Slice(SliceType {
                    elem: Box::new(elem_type),
                })
            }

            TypeExprKind::Map(map) => {
                let was_indirect = self.in_indirect;
                self.in_indirect = true;
                let key = self.resolve_type_expr(&map.key);
                let value = self.resolve_type_expr(&map.value);
                self.in_indirect = was_indirect;

                // Validate: key must be comparable
                if !self.is_comparable(&key) {
                    self.diagnostics.emit(TypeError::InvalidMapKey.at(map.key.span));
                }

                Type::Map(MapType {
                    key: Box::new(key),
                    value: Box::new(value),
                })
            }

            TypeExprKind::Chan(chan) => {
                let was_indirect = self.in_indirect;
                self.in_indirect = true;
                let elem = self.resolve_type_expr(&chan.elem);
                self.in_indirect = was_indirect;

                let dir = match chan.dir {
                    ast::ChanDir::Both => ChanDir::Both,
                    ast::ChanDir::Send => ChanDir::SendOnly,
                    ast::ChanDir::Recv => ChanDir::RecvOnly,
                };

                Type::Chan(ChanType {
                    dir,
                    elem: Box::new(elem),
                })
            }

            TypeExprKind::Func(func) => {
                let was_indirect = self.in_indirect;
                self.in_indirect = true;

                // AST FuncType has params as Vec<TypeExpr> directly
                let params: Vec<Type> = func
                    .params
                    .iter()
                    .map(|p| self.resolve_type_expr(p))
                    .collect();

                // AST FuncType results are Vec<TypeExpr> (not ResultParam)
                let results: Vec<Type> = func
                    .results
                    .iter()
                    .map(|r| self.resolve_type_expr(r))
                    .collect();

                self.in_indirect = was_indirect;

                Type::Func(FuncType {
                    params,
                    results,
                    variadic: false, // AST FuncType doesn't track variadic
                })
            }

            TypeExprKind::Struct(s) => {
                let fields = self.resolve_fields(&s.fields);
                Type::Struct(StructType { fields })
            }

            TypeExprKind::Pointer(inner) => {
                let was_indirect = self.in_indirect;
                self.in_indirect = true;
                let elem = self.resolve_type_expr(inner);
                self.in_indirect = was_indirect;
                Type::Pointer(Box::new(elem))
            }

            TypeExprKind::Interface(iface) => {
                let was_indirect = self.in_indirect;
                self.in_indirect = true;
                let (methods, embeds) = self.resolve_interface_elems(&iface.elems);
                self.in_indirect = was_indirect;
                Type::Interface(InterfaceType { methods, embeds })
            }
        }
    }

    /// Resolves a type name to a Type.
    fn resolve_type_name(&mut self, name: Symbol, span: Span) -> Type {
        // Look up in scope
        match self.scope.lookup(name) {
            Some(Entity::Var(v)) => {
                // Could be a built-in type stored as Var
                if let Type::Basic(b) = &v.ty {
                    return Type::Basic(*b);
                }
                let name_str = self.interner.resolve(name).unwrap_or("<unknown>");
                self.diagnostics.emit(
                    TypeError::NotAType.at_with_message(span, TypeError::NotAType.with_name(name_str)),
                );
                Type::Invalid
            }
            Some(Entity::Type(t)) => {
                // Resolve the named type
                self.resolve_named_type(t.id)
            }
            Some(_) => {
                let name_str = self.interner.resolve(name).unwrap_or("<unknown>");
                self.diagnostics.emit(
                    TypeError::NotAType.at_with_message(span, TypeError::NotAType.with_name(name_str)),
                );
                Type::Invalid
            }
            None => {
                let name_str = self.interner.resolve(name).unwrap_or("<unknown>");
                self.diagnostics.emit(
                    TypeError::Undefined.at_with_message(span, TypeError::Undefined.with_name(name_str)),
                );
                Type::Invalid
            }
        }
    }

    /// Resolves struct/object fields.
    fn resolve_fields(&mut self, ast_fields: &[ast::Field]) -> Vec<Field> {
        let mut fields = Vec::new();

        for ast_field in ast_fields {
            let ty = self.resolve_type_expr(&ast_field.ty);
            let tag = ast_field
                .tag
                .as_ref()
                .map(|t| self.interner.resolve(t.raw).unwrap_or("").to_string());

            if ast_field.names.is_empty() {
                // Embedded field
                let embedded_name = self.extract_type_name(&ast_field.ty);
                fields.push(Field {
                    name: embedded_name,
                    ty,
                    embedded: true,
                    tag,
                });
            } else {
                // Named fields
                for name in &ast_field.names {
                    fields.push(Field {
                        name: Some(name.symbol),
                        ty: ty.clone(),
                        embedded: false,
                        tag: tag.clone(),
                    });
                }
            }
        }

        fields
    }

    /// Extracts the type name from a type expression (for embedded fields).
    fn extract_type_name(&self, expr: &ast::TypeExpr) -> Option<Symbol> {
        match &expr.kind {
            TypeExprKind::Ident(ident) => Some(ident.symbol),
            _ => None,
        }
    }

    /// Resolves interface elements (methods and embedded interfaces).
    fn resolve_interface_elems(
        &mut self,
        elems: &[ast::InterfaceElem],
    ) -> (Vec<Method>, Vec<Symbol>) {
        let mut methods = Vec::new();
        let mut embeds = Vec::new();

        for elem in elems {
            match elem {
                ast::InterfaceElem::Method(m) => {
                    let params: Vec<Type> = m
                        .sig
                        .params
                        .iter()
                        .flat_map(|p| {
                            let ty = self.resolve_type_expr(&p.ty);
                            if p.names.is_empty() {
                                vec![ty]
                            } else {
                                vec![ty; p.names.len()]
                            }
                        })
                        .collect();

                    let results: Vec<Type> = m
                        .sig
                        .results
                        .iter()
                        .map(|r| self.resolve_type_expr(&r.ty))
                        .collect();

                    methods.push(Method {
                        name: m.name.symbol,
                        sig: FuncType {
                            params,
                            results,
                            variadic: m.sig.variadic,
                        },
                    });
                }
                ast::InterfaceElem::Embedded(ident) => {
                    embeds.push(ident.symbol);
                }
            }
        }

        (methods, embeds)
    }

    /// Evaluates an array length expression to a constant.
    fn eval_array_length(&mut self, expr: &ast::Expr) -> u64 {
        match self.eval_const_expr(expr) {
            Some(c) => {
                if let Some(n) = c.to_i64() {
                    if n >= 0 {
                        n as u64
                    } else {
                        self.diagnostics
                            .emit(TypeError::NegativeArrayLength.at(expr.span));
                        0
                    }
                } else {
                    self.diagnostics
                        .emit(TypeError::NonConstantArrayLength.at(expr.span));
                    0
                }
            }
            _ => {
                self.diagnostics
                    .emit(TypeError::NonConstantArrayLength.at(expr.span));
                0
            }
        }
    }

    /// Evaluates a constant expression (simplified).
    fn eval_const_expr(&self, expr: &ast::Expr) -> Option<Constant> {
        match &expr.kind {
            ast::ExprKind::IntLit(lit) => {
                let s = self.interner.resolve(lit.raw)?;
                parse_int_literal(s).ok().map(Constant::int)
            }
            ast::ExprKind::Ident(ident) => {
                if let Some(Entity::Var(v)) = self.scope.lookup(ident.symbol) {
                    v.constant.clone()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Checks if a type is comparable.
    fn is_comparable(&self, ty: &Type) -> bool {
        match ty {
            Type::Basic(_) => true,
            Type::Array(a) => self.is_comparable(&a.elem),
            Type::Struct(s) => s.fields.iter().all(|f| self.is_comparable(&f.ty)),
            Type::Pointer(_) => true, // reference comparison
            Type::Interface(_) => true,
            Type::Named(id) => {
                let idx = id.0 as usize;
                if idx < self.named_types.len() {
                    self.is_comparable(&self.named_types[idx].underlying)
                } else {
                    false
                }
            }
            // Slice, Map, Func, Chan are not comparable
            Type::Slice(_) | Type::Map(_) | Type::Func(_) | Type::Chan(_) => false,
            Type::Tuple(_) => false,
            Type::Untyped(_) => true,
            Type::Nil => true,
            Type::Invalid => false,
        }
    }
}

/// Parses an integer literal string to i64.
fn parse_int_literal(s: &str) -> Result<i64, ()> {
    let s = s.replace('_', "");
    if s.starts_with("0x") || s.starts_with("0X") {
        i64::from_str_radix(&s[2..], 16).map_err(|_| ())
    } else if s.starts_with("0o") || s.starts_with("0O") {
        i64::from_str_radix(&s[2..], 8).map_err(|_| ())
    } else if s.starts_with("0b") || s.starts_with("0B") {
        i64::from_str_radix(&s[2..], 2).map_err(|_| ())
    } else if s.starts_with('0')
        && s.len() > 1
        && s.chars().nth(1).is_some_and(|c| c.is_ascii_digit())
    {
        i64::from_str_radix(&s[1..], 8).map_err(|_| ())
    } else {
        s.parse().map_err(|_| ())
    }
}

/// Convenience function to run Phase 2 resolution.
pub fn resolve_types(
    collect_result: CollectResult,
    interner: &SymbolInterner,
    diagnostics: &mut DiagnosticSink,
) -> ResolveResult {
    let resolver = TypeResolver::new(collect_result, interner, diagnostics);
    resolver.resolve()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::collect::collect_types;
    use crate::types::BasicType;
    use gox_syntax::parse;

    fn resolve_source(source: &str) -> (ResolveResult, DiagnosticSink, SymbolInterner) {
        let (file, _parse_diag, interner) = parse(source, 0);

        let mut collect_diag = DiagnosticSink::new();
        let collect_result = collect_types(&file, &interner, &mut collect_diag);

        let mut resolve_diag = DiagnosticSink::new();
        let result = resolve_types(collect_result, &interner, &mut resolve_diag);

        (result, resolve_diag, interner)
    }

    #[test]
    fn test_resolve_basic_type_alias() {
        let (result, diag, _) = resolve_source("package main\ntype MyInt int");

        assert!(!diag.has_errors());
        assert_eq!(result.named_types.len(), 1);
        assert_eq!(
            result.named_types[0].underlying,
            Type::Basic(BasicType::Int)
        );
    }

    #[test]
    fn test_resolve_chained_type_alias() {
        let (result, diag, _) = resolve_source("package main\ntype A int\ntype B A");

        assert!(!diag.has_errors());
        assert_eq!(result.named_types.len(), 2);
        // A -> int
        assert_eq!(
            result.named_types[0].underlying,
            Type::Basic(BasicType::Int)
        );
        // B -> Named(A)
        assert!(matches!(result.named_types[1].underlying, Type::Named(_)));
    }

    #[test]
    fn test_resolve_struct_type() {
        let (result, diag, interner) =
            resolve_source("package main\ntype Point struct { x, y int }");

        assert!(!diag.has_errors());
        assert_eq!(result.named_types.len(), 1);

        if let Type::Struct(s) = &result.named_types[0].underlying {
            assert_eq!(s.fields.len(), 2);
            let x_sym = interner.get("x").unwrap();
            let y_sym = interner.get("y").unwrap();
            assert_eq!(s.fields[0].name, Some(x_sym));
            assert_eq!(s.fields[1].name, Some(y_sym));
        } else {
            panic!("expected struct type");
        }
    }

    #[test]
    fn test_resolve_pointer_type() {
        let (result, diag, _) = resolve_source("package main\ntype Counter struct { value int }\ntype CounterRef *Counter");

        assert!(!diag.has_errors());
        assert!(matches!(result.named_types[1].underlying, Type::Pointer(_)));
    }

    #[test]
    fn test_resolve_slice_type() {
        let (result, diag, _) = resolve_source("package main\ntype IntSlice []int");

        assert!(!diag.has_errors());
        if let Type::Slice(s) = &result.named_types[0].underlying {
            assert_eq!(*s.elem, Type::Basic(BasicType::Int));
        } else {
            panic!("expected slice type");
        }
    }

    #[test]
    fn test_resolve_map_type() {
        let (result, diag, _) = resolve_source("package main\ntype StringIntMap map[string]int");

        assert!(!diag.has_errors());
        if let Type::Map(m) = &result.named_types[0].underlying {
            assert_eq!(*m.key, Type::Basic(BasicType::String));
            assert_eq!(*m.value, Type::Basic(BasicType::Int));
        } else {
            panic!("expected map type");
        }
    }

    #[test]
    fn test_resolve_array_type() {
        let (result, diag, _) = resolve_source("package main\ntype IntArray [10]int");

        assert!(!diag.has_errors());
        if let Type::Array(a) = &result.named_types[0].underlying {
            assert_eq!(a.len, 10);
            assert_eq!(*a.elem, Type::Basic(BasicType::Int));
        } else {
            panic!("expected array type");
        }
    }

    #[test]
    fn test_resolve_chan_type() {
        let (result, diag, _) = resolve_source("package main\ntype IntChan chan int");

        assert!(!diag.has_errors());
        if let Type::Chan(c) = &result.named_types[0].underlying {
            assert_eq!(c.dir, ChanDir::Both);
            assert_eq!(*c.elem, Type::Basic(BasicType::Int));
        } else {
            panic!("expected chan type");
        }
    }

    #[test]
    fn test_resolve_func_type() {
        let (result, diag, _) = resolve_source("package main\ntype BinaryOp func(int, int) int");

        assert!(!diag.has_errors());
        if let Type::Func(f) = &result.named_types[0].underlying {
            assert_eq!(f.params.len(), 2);
            assert_eq!(f.results.len(), 1);
            assert!(!f.variadic);
        } else {
            panic!("expected func type");
        }
    }

    #[test]
    fn test_resolve_interface_type() {
        let (result, diag, interner) =
            resolve_source("package main\ninterface Reader { Read(buf []byte) int }");

        assert!(!diag.has_errors());
        if let Type::Interface(i) = &result.named_types[0].underlying {
            assert_eq!(i.methods.len(), 1);
            let read_sym = interner.get("Read").unwrap();
            assert_eq!(i.methods[0].name, read_sym);
        } else {
            panic!("expected interface type");
        }
    }

    #[test]
    fn test_resolve_direct_cycle_error() {
        let (_, diag, _) = resolve_source("package main\ntype A B\ntype B A");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_resolve_struct_cycle_error() {
        let (_, diag, _) = resolve_source("package main\ntype Node struct { next Node }");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_resolve_pointer_self_reference_ok() {
        let (result, diag, _) = resolve_source("package main\ntype Node struct { next *Node }");

        // Pointer types allow self-reference (reference semantics)
        assert!(!diag.has_errors());
        assert!(matches!(result.named_types[0].underlying, Type::Struct(_)));
    }

    #[test]
    fn test_resolve_slice_self_reference_ok() {
        let (result, diag, _) =
            resolve_source("package main\ntype Tree struct { children []Tree }");

        // Slice breaks the cycle
        assert!(!diag.has_errors());
        assert!(matches!(result.named_types[0].underlying, Type::Struct(_)));
    }

    #[test]
    fn test_resolve_map_key_not_comparable_error() {
        let (_, diag, _) = resolve_source("package main\ntype BadMap map[[]int]string");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_resolve_undefined_type_error() {
        let (_, diag, _) = resolve_source("package main\ntype X UndefinedType");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_resolve_array_const_length() {
        let (result, diag, _) = resolve_source("package main\nconst N = 5\ntype Arr [N]int");

        assert!(!diag.has_errors());
        if let Type::Array(a) = &result.named_types[0].underlying {
            assert_eq!(a.len, 5);
        } else {
            panic!("expected array type");
        }
    }

    #[test]
    fn test_resolve_forward_reference() {
        let (result, diag, _) = resolve_source("package main\ntype A B\ntype B int");

        assert!(!diag.has_errors());
        // A -> Named(B)
        assert!(matches!(result.named_types[0].underlying, Type::Named(_)));
        // B -> int
        assert_eq!(
            result.named_types[1].underlying,
            Type::Basic(BasicType::Int)
        );
    }

    #[test]
    fn test_resolve_nested_struct() {
        let (result, diag, _) = resolve_source(
            "package main\ntype Inner struct { value int }\ntype Outer struct { inner Inner }",
        );

        assert!(!diag.has_errors());
        assert_eq!(result.named_types.len(), 2);
    }

    #[test]
    fn test_resolve_interface_with_embedded() {
        let (result, diag, interner) = resolve_source(
            "package main\ninterface Reader { Read() int }\ninterface ReadWriter { Reader; Write() int }"
        );

        assert!(!diag.has_errors());
        if let Type::Interface(i) = &result.named_types[1].underlying {
            assert_eq!(i.methods.len(), 1); // Write
            assert_eq!(i.embeds.len(), 1); // Reader
            let reader_sym = interner.get("Reader").unwrap();
            assert_eq!(i.embeds[0], reader_sym);
        } else {
            panic!("expected interface type");
        }
    }

    #[test]
    fn test_resolve_send_chan() {
        let (result, diag, _) = resolve_source("package main\ntype SendChan chan<- int");

        assert!(!diag.has_errors());
        if let Type::Chan(c) = &result.named_types[0].underlying {
            assert_eq!(c.dir, ChanDir::SendOnly);
        } else {
            panic!("expected chan type");
        }
    }

    #[test]
    fn test_resolve_recv_chan() {
        let (result, diag, _) = resolve_source("package main\ntype RecvChan <-chan int");

        assert!(!diag.has_errors());
        if let Type::Chan(c) = &result.named_types[0].underlying {
            assert_eq!(c.dir, ChanDir::RecvOnly);
        } else {
            panic!("expected chan type");
        }
    }

    #[test]
    fn test_resolve_func_type_multiple_params() {
        let (result, diag, _) = resolve_source("package main\ntype BinaryFunc func(int, int) int");

        assert!(!diag.has_errors());
        if let Type::Func(f) = &result.named_types[0].underlying {
            assert_eq!(f.params.len(), 2);
            assert_eq!(f.results.len(), 1);
        } else {
            panic!("expected func type");
        }
    }

    #[test]
    fn test_resolve_map_with_struct_key() {
        let (result, diag, _) =
            resolve_source("package main\ntype Key struct { id int }\ntype M map[Key]string");

        // Struct with comparable fields is valid as map key
        assert!(!diag.has_errors());
        assert!(matches!(result.named_types[1].underlying, Type::Map(_)));
    }

    #[test]
    fn test_resolve_array_of_arrays() {
        let (result, diag, _) = resolve_source("package main\ntype Matrix [3][3]int");

        assert!(!diag.has_errors());
        if let Type::Array(a) = &result.named_types[0].underlying {
            assert_eq!(a.len, 3);
            assert!(matches!(*a.elem, Type::Array(_)));
        } else {
            panic!("expected array type");
        }
    }

    #[test]
    fn test_resolve_pointer_like_cycle_via_slice() {
        let (result, diag, _) = resolve_source(
            "package main\ntype List struct { head Node }\ntype Node struct { next []Node; value int }"
        );

        // Slice breaks the cycle
        assert!(!diag.has_errors());
        assert_eq!(result.named_types.len(), 2);
    }

    #[test]
    fn test_resolve_mutual_recursion_via_pointer() {
        let (result, diag, _) =
            resolve_source("package main\ntype A struct { b *B }\ntype B struct { a *A }");

        // Pointer types allow mutual recursion
        assert!(!diag.has_errors());
        assert_eq!(result.named_types.len(), 2);
    }
}
